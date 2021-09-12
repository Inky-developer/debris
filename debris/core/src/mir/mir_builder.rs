use itertools::Itertools;
use rustc_hash::FxHashMap;

use debris_common::{Ident, Span};

use crate::{
    error::Result,
    hir::{
        hir_nodes::{
            HirBlock, HirConditionalBranch, HirConstValue, HirExpression, HirFormatStringMember,
            HirFunction, HirFunctionCall, HirObject, HirStatement, HirTypePattern,
            HirVariableInitialization, HirVariablePattern, HirVariableUpdate,
        },
        Hir, IdentifierPath, SpannedIdentifier,
    },
    mir::{
        mir_context::{MirContext, MirContextId},
        mir_nodes::{FunctionCall, MirNode, PrimitiveDeclaration, VariableUpdate},
        mir_object::{MirObject, MirObjectId},
        mir_primitives::{MirFormatString, MirFormatStringComponent, MirFunction, MirPrimitive},
        namespace::MirNamespace,
        Mir,
    },
    CompileContext,
};

use super::{
    mir_context::MirContextKind,
    mir_nodes::{Branch, Goto},
};

pub struct MirBuilder<'ctx, 'hir> {
    compile_context: &'ctx CompileContext,
    hir: &'hir Hir,
    current_context: MirContext,
    singletons: MirSingletons,
    entry_context: MirContextId,
    contexts: FxHashMap<MirContextId, MirContext>,
    namespace: MirNamespace,
    extern_items: FxHashMap<Ident, MirObjectId>,
    next_context_id: u32,
}

impl<'ctx, 'hir> MirBuilder<'ctx, 'hir> {
    pub fn new(ctx: &'ctx CompileContext, hir: &'hir Hir) -> Self {
        let entry_context = MirContextId {
            compilation_id: ctx.compilation_id,
            id: 0,
        };

        let mut namespace = MirNamespace::new(ctx);

        let null = namespace.insert_object().id;
        let never = namespace.insert_object().id;
        let singletons = MirSingletons { null, never };

        let mut current_context =
            MirContext::new(entry_context, None, MirContextKind::Block, &singletons);
        current_context.nodes.push(
            PrimitiveDeclaration {
                span: Span::empty(),
                target: null,
                value: MirPrimitive::Null,
            }
            .into(),
        );
        current_context.nodes.push(
            PrimitiveDeclaration {
                span: Span::empty(),
                target: never,
                value: MirPrimitive::Never,
            }
            .into(),
        );

        MirBuilder {
            compile_context: ctx,
            hir,
            current_context,
            singletons,
            entry_context,
            contexts: Default::default(),
            namespace,
            extern_items: Default::default(),
            next_context_id: 1,
        }
    }

    pub fn build(mut self) -> Result<Mir> {
        self.handle_block(&self.hir.main_function, MirContextKind::Block)?;

        Ok(Mir {
            namespace: self.namespace,
            extern_items: self.extern_items,
            entry_context: self.entry_context,
            contexts: self.contexts,
        })
    }

    fn emit(&mut self, node: impl Into<MirNode>) {
        self.current_context.nodes.push(node.into());
    }

    /// Traverses the namespaces stack until it finds `ident`.
    /// If `ident` cannot be found, it will be inserted at the lowest namespace
    fn variable_get_or_insert(&mut self, ident: Ident) -> MirObjectId {
        let mut current_context = &self.current_context;

        loop {
            if let Some(obj_id) = current_context.local_namespace.get_property(&ident) {
                break obj_id;
            }

            if let Some(prev_context) = current_context.super_context_id {
                current_context = &self.contexts[&prev_context];
            } else if let Some(id) = self.extern_items.get(&ident) {
                break *id;
            } else {
                let obj_id = self.namespace.insert_object().id;
                self.extern_items.insert(ident.clone(), obj_id);
                break obj_id;
            }
        }
    }

    fn get_ident(&self, spanned_ident: &SpannedIdentifier) -> Ident {
        self.compile_context
            .input_files
            .get_span_str(spanned_ident.span)
            .into()
    }

    fn resolve_path(&mut self, path: &IdentifierPath) -> MirObjectId {
        let (obj, ident) = self.resolve_path_without_last(path);
        match obj {
            Some(obj) => obj.id.property_get_or_insert(&mut self.namespace, ident),
            None => self.variable_get_or_insert(ident),
        }
    }

    /// Resolves the path up to the last ident, so that the attribute can be set manually
    fn resolve_path_without_last(
        &mut self,
        path: &IdentifierPath,
    ) -> (Option<&mut MirObject>, Ident) {
        match path.idents() {
            [single] => (None, self.get_ident(single)),
            [multiple @ .., last] => {
                let mut obj: Option<MirObjectId> = None;
                let ident = self.get_ident(last);

                for spanned_ident in multiple {
                    let ident = self.get_ident(spanned_ident);

                    obj = Some(match obj {
                        None => self.variable_get_or_insert(self.get_ident(spanned_ident)),
                        Some(obj) => obj.property_get_or_insert(&mut self.namespace, ident),
                    })
                }

                (
                    Some(
                        self.namespace
                            .get_obj_mut(obj.expect("Path cannot be empty")),
                    ),
                    ident,
                )
            }
            [] => unreachable!(),
        }
    }

    /// Creates a new context and returns the previous one
    #[must_use = "This method returns the previous context"]
    fn next_context(&mut self, kind: MirContextKind) -> MirContext {
        let next_id = self.next_context_id;
        self.next_context_id += 1;
        let super_ctx_id = self.current_context.super_context_id;
        std::mem::replace(
            &mut self.current_context,
            MirContext::new(
                MirContextId {
                    compilation_id: self.compile_context.compilation_id,
                    id: next_id,
                },
                super_ctx_id,
                kind,
                &self.singletons,
            ),
        )
    }
}

impl MirBuilder<'_, '_> {
    pub fn handle_block(&mut self, block: &HirBlock, kind: MirContextKind) -> Result<()> {
        self.handle_block_keep_context(block)?;

        let old_context = self.next_context(kind);
        self.contexts.insert(old_context.id, old_context);

        Ok(())
    }

    /// Evalutes the block in its own context and then puts the old context back.
    /// Then adds an instruction to go to this block and also inserts the context into the context map
    fn handle_nested_block(
        &mut self,
        block: &HirBlock,
        kind: MirContextKind,
    ) -> Result<(MirObjectId, MirContextId)> {
        let context = self._handle_nested_block(block, kind)?;
        let context_id = context.id;

        let return_value = context.return_values.return_value();
        self.emit(Goto {
            span: block.span,
            context_id,
        });

        self.contexts.insert(context.id, context);

        Ok((return_value, context_id))
    }

    /// Evaluates the block in its own context and then puts the old context back
    fn _handle_nested_block(
        &mut self,
        block: &HirBlock,
        kind: MirContextKind,
    ) -> Result<MirContext> {
        let old_context = self.next_context(kind);
        self.handle_block_keep_context(block)?;
        Ok(std::mem::replace(&mut self.current_context, old_context))
    }

    fn handle_block_keep_context(&mut self, block: &HirBlock) -> Result<()> {
        // ToDo: Handle objects and return value
        for object in &block.objects {
            self.handle_object(object)?;
        }

        for statement in &block.statements {
            self.handle_statement(statement)?;
        }

        if let Some(return_value) = &block.return_value {
            let value = self.handle_expression(return_value)?;
            assert_eq!(self.current_context.return_values.implicite_return, None);
            self.current_context.return_values.implicite_return = Some(value);
        }
        Ok(())
    }

    fn handle_object(&mut self, object: &HirObject) -> Result<()> {
        match object {
            HirObject::Function(function) => self.handle_function(function),
            _ => todo!(),
        }
    }

    fn handle_function(&mut self, function: &HirFunction) -> Result<()> {
        let prev_context = self.next_context(MirContextKind::Function);

        let target = self.namespace.insert_object().id;
        let ident = self.get_ident(&function.ident);
        self.current_context
            .local_namespace
            .insert(target, ident.clone());

        let parameters = function
            .parameters
            .iter()
            .map(|_| self.namespace.insert_object().id)
            .collect_vec();
        for (parameter, param_declaration) in parameters.iter().zip(function.parameters.iter()) {
            self.current_context
                .local_namespace
                .insert(*parameter, self.get_ident(&param_declaration.ident))
        }

        assert!(function.attributes.is_empty(), "TODO");
        self.handle_block_keep_context(&function.block)?;
        let function_ctx = std::mem::replace(&mut self.current_context, prev_context);

        let parameter_types = function
            .parameters
            .iter()
            .map(|param| self.handle_type_pattern(&param.typ))
            .try_collect()?;
        let return_type = function
            .return_type
            .as_ref()
            .map(|pat| self.handle_type_pattern(pat))
            .transpose()?;
        let function_primitive = MirPrimitive::Function(MirFunction {
            context_id: function_ctx.id,
            name: ident.clone(),
            parameter_types,
            parameters,
            return_type,
        });

        self.emit(PrimitiveDeclaration {
            span: function.span,
            target,
            value: function_primitive,
        });
        self.current_context.local_namespace.insert(target, ident);

        self.contexts.insert(function_ctx.id, function_ctx);

        Ok(())
    }

    fn handle_statement(&mut self, statement: &HirStatement) -> Result<()> {
        match statement {
            HirStatement::VariableDecl(variable_decl) => {
                self.handle_variable_declaration(variable_decl)
            }
            HirStatement::FunctionCall(function_call) => {
                self.handle_function_call(function_call)?;
                Ok(())
            }
            HirStatement::VariableUpdate(variable_update) => {
                self.handle_variable_update(variable_update)?;
                Ok(())
            }
            HirStatement::Block(block) => {
                self.handle_nested_block(block, MirContextKind::Block)?;
                Ok(())
            }
            HirStatement::ConditonalBranch(branch) => {
                self.handle_branch(branch)?;
                Ok(())
            }
            other => todo!("{:?}", other),
        }
    }

    fn handle_variable_declaration(
        &mut self,
        variable_decl: &HirVariableInitialization,
    ) -> Result<()> {
        fn handle_pattern(
            this: &mut MirBuilder,
            pattern: &HirVariablePattern,
            value: MirObjectId,
            span: Span,
        ) {
            match pattern {
                HirVariablePattern::Path(path) => {
                    let (obj_opt, last_ident) = this.resolve_path_without_last(path);
                    let local_namespace = match obj_opt {
                        None => &mut this.current_context.local_namespace,
                        Some(obj) => &mut obj.local_namespace,
                    };
                    local_namespace.insert(value, last_ident)
                }
                HirVariablePattern::Tuple(patterns) => {
                    for (index, pattern) in patterns.iter().enumerate() {
                        let value =
                            value.property_get_or_insert(&mut this.namespace, Ident::Index(index));
                        handle_pattern(this, pattern, value, span);
                    }
                }
            }
        }

        let _mode = variable_decl.mode; // ToDo: Not ignore mode
        let value = self.handle_expression(&variable_decl.value)?;
        handle_pattern(self, &variable_decl.pattern, value, variable_decl.span);

        Ok(())
    }

    fn handle_variable_update(&mut self, variable_update: &HirVariableUpdate) -> Result<()> {
        fn handle_pattern(
            this: &mut MirBuilder,
            pattern: &HirVariablePattern,
            value: MirObjectId,
            span: Span,
        ) {
            match pattern {
                HirVariablePattern::Tuple(patterns) => {
                    for (index, pattern) in patterns.iter().enumerate() {
                        let value =
                            value.property_get_or_insert(&mut this.namespace, Ident::Index(index));
                        handle_pattern(this, pattern, value, span);
                    }
                }
                HirVariablePattern::Path(path) => {
                    let (obj_opt, last_ident) = this.resolve_path_without_last(path);
                    let local_namespace = match obj_opt {
                        Some(obj) => &mut obj.local_namespace,
                        None => &mut this.current_context.local_namespace,
                    };
                    let prev_value = local_namespace.get_property(&last_ident);
                    this.emit(VariableUpdate {
                        span,
                        target: prev_value
                            .expect("TODO: Throw error message updated before assigned"),
                        value,
                    })
                }
            }
        }

        let new_value = self.handle_expression(&variable_update.value)?;
        handle_pattern(
            self,
            &variable_update.pattern,
            new_value,
            variable_update.span,
        );

        Ok(())
    }

    fn handle_expression(&mut self, expression: &HirExpression) -> Result<MirObjectId> {
        match expression {
            HirExpression::Value(literal) => self.handle_literal_value(literal),
            HirExpression::BinaryOperation {
                operation,
                rhs,
                lhs,
            } => {
                let lhs = self.handle_expression(lhs)?;
                let rhs = self.handle_expression(rhs)?;

                let function = lhs.property_get_or_insert(
                    &mut self.namespace,
                    operation.operator.get_special_ident().into(),
                );
                let return_value = self.namespace.insert_object().id;

                self.emit(FunctionCall {
                    span: expression.span(),
                    function,
                    parameters: vec![lhs, rhs],
                    return_value,
                });
                Ok(return_value)
            }
            HirExpression::FunctionCall(function_call) => self.handle_function_call(function_call),
            HirExpression::Variable(spanned_ident) => {
                let ident = self.get_ident(spanned_ident);
                Ok(self.variable_get_or_insert(ident))
            }
            HirExpression::Block(block) => {
                Ok(self.handle_nested_block(block, MirContextKind::Block)?.0)
            }
            HirExpression::ConditionalBranch(branch) => self.handle_branch(branch),
            other => todo!("{:?}", other),
        }
    }

    fn handle_literal_value(&mut self, value: &HirConstValue) -> Result<MirObjectId> {
        let primitive = match value {
            HirConstValue::Integer { value, .. } => MirPrimitive::Int(*value),
            HirConstValue::Bool { value, .. } => MirPrimitive::Bool(*value),
            HirConstValue::Fixed { .. } => {
                unimplemented!("Decimal literals are not yet implemented")
            }
            HirConstValue::String { value, .. } => MirPrimitive::String(value.clone()),
            HirConstValue::FormatString { value, .. } => {
                let parts = value
                    .iter()
                    .map(|member| match member {
                        HirFormatStringMember::String(val) => {
                            MirFormatStringComponent::String(val.clone())
                        }
                        HirFormatStringMember::Variable(spanned_ident) => {
                            let ident = self.get_ident(spanned_ident);
                            MirFormatStringComponent::Value(
                                self.current_context
                                    .local_namespace
                                    .property_get_or_insert(&mut self.namespace, ident),
                            )
                        }
                    })
                    .collect::<Vec<_>>();
                MirPrimitive::FormatString(MirFormatString::from(parts))
            }
        };

        let obj = self.namespace.insert_object().id;
        self.emit(PrimitiveDeclaration {
            value: primitive,
            target: obj,
            span: value.span(),
        });
        Ok(obj)
    }

    fn handle_function_call(&mut self, function_call: &HirFunctionCall) -> Result<MirObjectId> {
        let function = if let Some(accessor) = &function_call.accessor {
            let obj = self.handle_expression(accessor)?;
            let ident = self.get_ident(&function_call.ident);
            obj.get_property(&self.namespace, &ident)
                .expect("TODO: Throw error for undefined function")
        } else {
            self.variable_get_or_insert(self.get_ident(&function_call.ident))
        };

        let parameters = function_call
            .parameters
            .iter()
            .map(|param| self.handle_expression(param))
            .try_collect()?;
        let return_value = self.namespace.insert_object().id;

        self.emit(FunctionCall {
            span: function_call.span,
            parameters,
            return_value,
            function,
        });
        Ok(return_value)
    }

    fn handle_type_pattern(&mut self, param: &HirTypePattern) -> Result<MirObjectId> {
        match param {
            HirTypePattern::Path(path) => Ok(self.resolve_path(path)),
            _ => todo!(),
        }
    }

    fn handle_branch(&mut self, branch: &HirConditionalBranch) -> Result<MirObjectId> {
        let condition = self.handle_expression(&branch.condition)?;

        let pos_context =
            self._handle_nested_block(&branch.block_positive, MirContextKind::Block)?;
        let pos_return_value = pos_context.return_values.return_value();
        let pos_branch = pos_context.id;
        self.contexts.insert(pos_branch, pos_context);

        let neg_branch;
        if let Some(neg_block) = &branch.block_negative {
            let mut neg_context = self._handle_nested_block(neg_block, MirContextKind::Block)?;

            let neg_return_value = neg_context.return_values.return_value();

            // This condition is not strictly required, but it can reduce the amount of mir, because it can omit many `null := null` assignments
            if pos_return_value != neg_return_value {
                neg_context.nodes.push(
                    VariableUpdate {
                        span: neg_block.last_item_span(),
                        target: pos_return_value,
                        value: neg_return_value,
                    }
                    .into(),
                );
            }

            let neg_context_id = neg_context.id;
            self.contexts.insert(neg_context_id, neg_context);
            neg_branch = Some(neg_context_id);
        } else {
            neg_branch = None;
        }

        self.emit(Branch {
            span: branch.span,
            condition_span: branch.condition.span(),
            return_value: pos_return_value,
            condition,
            pos_branch,
            neg_branch,
        });
        Ok(pos_return_value)
    }
}

/// Holds some singletons objects for easier access
#[derive(Debug)]
pub struct MirSingletons {
    pub null: MirObjectId,
    pub never: MirObjectId,
}
