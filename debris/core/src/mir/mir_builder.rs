use itertools::Itertools;
use rustc_hash::FxHashMap;

use debris_common::{Ident, Span};

use crate::{
    error::{LangError, LangErrorKind, Result},
    hir::{
        hir_nodes::{
            HirBlock, HirConditionalBranch, HirConstValue, HirControlFlow, HirControlKind,
            HirExpression, HirFormatStringMember, HirFunction, HirFunctionCall, HirObject,
            HirStatement, HirTypePattern, HirVariableInitialization, HirVariablePattern,
            HirVariableUpdate,
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
    mir_context::{
        MirContextKind, ReturnContext, ReturnValuesArena, ReturnValuesData, ReturnValuesDataId,
    },
    mir_nodes::{Branch, Goto},
};

pub struct MirBuilder<'ctx, 'hir> {
    compile_context: &'ctx CompileContext,
    hir: &'hir Hir,
    current_context: MirContext,
    singletons: MirSingletons,
    entry_context: MirContextId,
    contexts: FxHashMap<MirContextId, MirContext>,
    return_values_arena: ReturnValuesArena,
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

        let mut return_values_arena = ReturnValuesArena::default();

        let mut namespace = MirNamespace::new(ctx);

        let null = namespace.insert_object().id;
        let never = namespace.insert_object().id;
        let singletons = MirSingletons { null, never };

        let root_context_kind = MirContextKind::Block;
        let return_values =
            ReturnValuesData::new(root_context_kind.default_return_value(&singletons));
        let return_values_id = return_values_arena.add(return_values);

        let mut current_context =
            MirContext::new(entry_context, None, root_context_kind, return_values_id);
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
            return_values_arena,
            namespace,
            extern_items: Default::default(),
            next_context_id: 1,
        }
    }

    pub fn build(mut self) -> Result<Mir> {
        self.handle_block(&self.hir.main_function, MirContextKind::Block)?;

        Ok(Mir {
            namespace: self.namespace,
            return_values_arena: self.return_values_arena,
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

    fn next_context_with_return_data(
        &mut self,
        kind: MirContextKind,
        super_ctx_id: Option<MirContextId>,
    ) -> MirContextId {
        let return_values_id = self.return_values_arena.add(ReturnValuesData::new(
            kind.default_return_value(&self.singletons),
        ));
        self.next_context(kind, super_ctx_id, return_values_id)
    }

    /// Creates a new context and returns the previous one
    fn next_context(
        &mut self,
        kind: MirContextKind,
        super_ctx_id: Option<MirContextId>,
        return_values_data_id: ReturnValuesDataId,
    ) -> MirContextId {
        let next_id = self.next_context_id;
        self.next_context_id += 1;

        let old_context = std::mem::replace(
            &mut self.current_context,
            MirContext::new(
                MirContextId {
                    compilation_id: self.compile_context.compilation_id,
                    id: next_id,
                },
                super_ctx_id,
                kind,
                return_values_data_id,
            ),
        );
        let old_context_id = old_context.id;
        self.contexts.insert(old_context_id, old_context);
        old_context_id
    }

    fn return_value(&mut self, context_id: MirContextId, value: MirObjectId, span: Span) {
        let context = context_mut_hack(&mut self.contexts, &mut self.current_context, &context_id);

        if let Some(return_value) = context
            .return_values(&self.return_values_arena)
            .explicite_return
        {
            self.emit(VariableUpdate {
                span,
                target: return_value,
                value,
                must_exist: true,
            });
        } else {
            context
                .return_values_mut(&mut self.return_values_arena)
                .explicite_return = Some(value);
            if context_id == self.current_context.id {
                self.current_context
                    .return_values_mut(&mut self.return_values_arena)
                    .unconditionally_returned = true;
            }
        }
    }

    fn get_return_context(&self, kind: HirControlKind) -> ReturnContext {
        match kind {
            HirControlKind::Break | HirControlKind::Continue => todo!(),
            HirControlKind::Return => ReturnContext::ExitFunction,
        }
    }

    /// Finds the context to return to using `control_flow`
    fn target_context_for(&self, control_flow: HirControlKind) -> Option<&MirContext> {
        let mut current_context = &self.current_context;
        loop {
            match (current_context.kind, control_flow) {
                (MirContextKind::Function, HirControlKind::Return) => break,
                (_, HirControlKind::Break | HirControlKind::Continue) => {
                    todo!("Loops are not yet implemented")
                }
                _ => {}
            }

            current_context = self
                .contexts
                .get(&current_context.super_context_id?)
                .unwrap();
        }

        Some(current_context)
    }
}

impl MirBuilder<'_, '_> {
    pub fn handle_block(&mut self, block: &HirBlock, kind: MirContextKind) -> Result<()> {
        self.handle_block_keep_context(block)?;

        let return_values_id = self.return_values_arena.add(ReturnValuesData::new(
            MirContextKind::Block.default_return_value(&self.singletons),
        ));
        self.next_context(kind, Some(self.current_context.id), return_values_id);

        Ok(())
    }

    /// Handles a nested block and returns its id
    fn handle_nested_block(
        &mut self,
        block: &HirBlock,
        kind: MirContextKind,
    ) -> Result<MirContextId> {
        let return_values_id = self.return_values_arena.add(ReturnValuesData::new(
            kind.default_return_value(&self.singletons),
        ));
        let old_context_id =
            self.next_context(kind, Some(self.current_context.id), return_values_id);
        self.handle_block_keep_context(block)?;
        let old_context = self.contexts.remove(&old_context_id).unwrap();
        let context = std::mem::replace(&mut self.current_context, old_context);

        let context_id = context.id;
        self.contexts.insert(context.id, context);
        Ok(context_id)
    }

    fn handle_block_keep_context(&mut self, block: &HirBlock) -> Result<()> {
        let current_context_id = self.current_context.id;

        // ToDo: Handle objects and return value
        for object in &block.objects {
            self.handle_object(object)?;
        }

        for statement in &block.statements {
            self.handle_statement(statement)?;
        }

        if let Some(return_value) = &block.return_value {
            let value = self.handle_expression(return_value)?;
            self.return_value(self.current_context.id, value, return_value.span());
        }

        
        // Some statement could have modified the current context (like control flow),
        // so set the correct context in case it got changed
        if self.current_context.id != current_context_id {
            let actual_context = self.contexts.remove(&current_context_id).unwrap();
            let context = std::mem::replace(&mut self.current_context, actual_context);
            
            let other_context_id = context.id;
            self.contexts.insert(other_context_id, context);
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
        let prev_context_id = self
            .next_context_with_return_data(MirContextKind::Function, Some(self.current_context.id));

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

        let prev_context = self.contexts.remove(&prev_context_id).unwrap();
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
        if self
            .current_context
            .return_values(&self.return_values_arena)
            .unconditionally_returned
        {
            return Err(LangError::new(LangErrorKind::UnreachableCode, statement.span()).into());
        }

        match statement {
            HirStatement::VariableDecl(variable_decl) => {
                self.handle_variable_declaration(variable_decl)
            }
            HirStatement::FunctionCall(function_call) => {
                self.handle_function_call(function_call)?;
                Ok(())
            }
            HirStatement::VariableUpdate(variable_update) => {
                self.handle_variable_update(variable_update)
            }
            HirStatement::Block(block) => {
                self.handle_nested_block(block, MirContextKind::Block)?;
                Ok(())
            }
            HirStatement::ConditonalBranch(branch) => {
                self.handle_branch(branch)?;
                Ok(())
            }
            HirStatement::ControlFlow(control_flow) => self.handle_control_flow(control_flow),
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
                    let prev_value = match obj_opt {
                        Some(obj) => obj
                            .id
                            .property_get_or_insert(&mut this.namespace, last_ident),
                        None => this.variable_get_or_insert(last_ident),
                    };

                    this.emit(VariableUpdate {
                        span,
                        target: prev_value,
                        value,
                        must_exist: true,
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
                let context_id = self.handle_nested_block(block, MirContextKind::Block)?;
                Ok(self
                    .contexts
                    .get(&context_id)
                    .unwrap()
                    .return_values(&self.return_values_arena)
                    .return_value())
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

        let pos_context_id =
            self.handle_nested_block(&branch.block_positive, MirContextKind::Block)?;
        let pos_return_value = self
            .contexts
            .get(&pos_context_id)
            .unwrap()
            .return_values(&self.return_values_arena)
            .return_value();

        let return_value = self.namespace.insert_object().id;

        self.contexts.get_mut(&pos_context_id).unwrap().nodes.push(
            VariableUpdate {
                span: branch.block_positive.last_item_span(),
                target: return_value,
                value: pos_return_value,
                must_exist: false,
            }
            .into(),
        );

        let neg_context_id = if let Some(neg_block) = &branch.block_negative {
            let neg_context_id = self.handle_nested_block(neg_block, MirContextKind::Block)?;

            let neg_return_value = self
                .contexts
                .get(&neg_context_id)
                .unwrap()
                .return_values(&self.return_values_arena)
                .return_value();

            self.contexts.get_mut(&neg_context_id).unwrap().nodes.push(
                VariableUpdate {
                    span: neg_block.last_item_span(),
                    target: return_value,
                    value: neg_return_value,
                    must_exist: false,
                }
                .into(),
            );

            neg_context_id
        } else {
            let neg_context_id = {
                let old_context_id = self.next_context_with_return_data(
                    MirContextKind::Block,
                    Some(self.current_context.id),
                );

                let old_context = self.contexts.remove(&old_context_id).unwrap();
                let context = std::mem::replace(&mut self.current_context, old_context);

                let context_id = context.id;
                self.contexts.insert(context.id, context);
                context_id
            };

            let neg_return_value = self.singletons.null;

            self.contexts.get_mut(&neg_context_id).unwrap().nodes.push(
                VariableUpdate {
                    span: branch.span,
                    target: return_value,
                    value: neg_return_value,
                    must_exist: false,
                }
                .into(),
            );

            neg_context_id
        };

        self.emit(Branch {
            span: branch.span,
            condition_span: branch.condition.span(),
            return_value,
            condition,
            pos_branch: pos_context_id,
            neg_branch: neg_context_id,
        });

        self.next_context(
            self.current_context.kind,
            Some(self.current_context.id),
            self.current_context.return_values_id,
        );

        for id in [pos_context_id, neg_context_id] {
            let context = self.contexts.get_mut(&id).unwrap();

            let target_context = match &context.return_context {
                ReturnContext::Next => Some(self.current_context.id),
                ReturnContext::ExitFunction => None,
                ReturnContext::Specific(id) => Some(*id),
            };

            if let Some(target_context) = target_context {
                context.nodes.push(MirNode::Goto(Goto {
                    span: branch.span,
                    context_id: target_context,
                }));
            }
        }

        Ok(return_value)
    }

    fn handle_control_flow(&mut self, control_flow: &HirControlFlow) -> Result<()> {
        let expression = match &control_flow.expression {
            Some(expression) => self.handle_expression(expression)?,
            None => self.singletons.null,
        };

        let context_id = self
            .target_context_for(control_flow.kind)
            .ok_or_else(|| {
                LangError::new(
                    LangErrorKind::InvalidControlFlow { mode: () },
                    control_flow.span,
                )
            })?
            .id;

        let return_context = self.get_return_context(control_flow.kind);
        self.current_context.return_context = return_context;

        self.return_value(context_id, expression, control_flow.span);

        // The block which contains the control flow itself should return the never type
        if context_id != self.current_context.id {
            self.return_value(
                self.current_context.id,
                self.singletons.never,
                control_flow.span,
            );
        }

        Ok(())
    }
}

fn context_mut_hack<'a>(
    contexts: &'a mut FxHashMap<MirContextId, MirContext>,
    current_context: &'a mut MirContext,
    context_id: &MirContextId,
) -> &'a mut MirContext {
    match contexts.get_mut(context_id) {
        Some(context) => context,
        None => {
            assert_eq!(current_context.id, *context_id);
            current_context
        }
    }
}

/// Holds some singletons objects for easier access
#[derive(Debug)]
pub struct MirSingletons {
    pub null: MirObjectId,
    pub never: MirObjectId,
}
