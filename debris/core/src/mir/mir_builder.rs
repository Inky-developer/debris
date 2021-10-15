use itertools::Itertools;
use rustc_hash::FxHashMap;

use debris_common::{Ident, Span};

use crate::{
    error::{LangError, LangErrorKind, Result},
    hir::{
        hir_nodes::{
            HirBlock, HirConditionalBranch, HirConstValue, HirControlFlow, HirControlKind,
            HirExpression, HirFormatStringMember, HirFunction, HirFunctionCall, HirImport,
            HirInfiniteLoop, HirModule, HirObject, HirStatement, HirTypePattern,
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
    mir_context::{
        MirContextKind, ReturnContext, ReturnValuesArena, ReturnValuesData, ReturnValuesDataId,
    },
    mir_nodes::{Branch, Goto},
    mir_primitives::MirModule,
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

        let mut current_context = MirContext::new(
            entry_context,
            None,
            false,
            root_context_kind,
            return_values_id,
            ReturnContext::Pass,
        );
        current_context.nodes.push(
            PrimitiveDeclaration {
                span: Span::EMPTY,
                target: null,
                value: MirPrimitive::Null,
            }
            .into(),
        );
        current_context.nodes.push(
            PrimitiveDeclaration {
                span: Span::EMPTY,
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

    fn get_context(&self, id: MirContextId) -> &MirContext {
        if id == self.current_context.id {
            &self.current_context
        } else {
            &self.contexts[&id]
        }
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

    /// Returns the object specified by the path or returns an error if the object does not exist
    fn resolve_path(&mut self, path: &IdentifierPath) -> Result<MirObjectId> {
        let (obj, ident) = self.resolve_path_without_last(path);
        match obj {
            Some(obj) => obj.local_namespace.get_property(&ident).ok_or_else(|| {
                LangError::new(
                    LangErrorKind::MissingProperty {
                        similar: vec![],
                        parent: self
                            .compile_context
                            .input_files
                            .get_span_str(path.span_without_last().expect("Must be valid"))
                            .to_string(),
                        property: ident.clone(),
                    },
                    path.span(),
                )
                .into()
            }),
            None => Ok(self.variable_get_or_insert(ident)),
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
        is_chained: bool,
        return_context: ReturnContext,
    ) -> MirContextId {
        let return_values_id = self.return_values_arena.add(ReturnValuesData::new(
            kind.default_return_value(&self.singletons),
        ));
        self.next_context(
            kind,
            super_ctx_id,
            is_chained,
            return_values_id,
            return_context.into(),
        )
    }

    /// Creates a new context and returns the previous one
    fn next_context(
        &mut self,
        kind: MirContextKind,
        super_ctx_id: Option<MirContextId>,
        is_chained: bool,
        return_values_data_id: ReturnValuesDataId,
        return_context_behavior: ReturnContextBehaviour,
    ) -> MirContextId {
        let context = self.create_context(
            kind,
            super_ctx_id,
            is_chained,
            return_values_data_id,
            return_context_behavior,
        );

        let old_context = std::mem::replace(&mut self.current_context, context);
        let old_context_id = old_context.id;
        self.contexts.insert(old_context_id, old_context);
        old_context_id
    }

    fn create_context(
        &mut self,
        kind: MirContextKind,
        super_ctx_id: Option<MirContextId>,
        is_chained: bool,
        return_values_data_id: ReturnValuesDataId,
        return_context_behavior: ReturnContextBehaviour,
    ) -> MirContext {
        let next_id = self.next_context_id;
        self.next_context_id += 1;

        let context_id = MirContextId {
            compilation_id: self.compile_context.compilation_id,
            id: next_id,
        };

        let return_context = match return_context_behavior {
            ReturnContextBehaviour::Normal(return_context) => return_context,
            ReturnContextBehaviour::Loop => ReturnContext::Specific(context_id),
        };

        MirContext::new(
            context_id,
            super_ctx_id,
            is_chained,
            kind,
            return_values_data_id,
            return_context,
        )
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

    /// Calculates the ReturnContext to use for a context with control flow,
    /// where `kind` is the kind of control flow and
    /// `context_id` is the context that is targeted by the control flow
    fn get_return_context(&self, kind: HirControlKind, context_id: MirContextId) -> ReturnContext {
        match kind {
            HirControlKind::Break => {
                let context = self.get_context(context_id);
                assert_eq!(context.kind, MirContextKind::Loop);

                context
                    .super_context_id
                    .map(ReturnContext::Specific)
                    .unwrap_or(ReturnContext::Pass)
            }
            HirControlKind::Continue => match self.get_context(context_id).return_context {
                ReturnContext::Pass => ReturnContext::Pass,
                ReturnContext::Specific(id) => ReturnContext::Specific(id),
                ReturnContext::ManuallyHandled(id) => ReturnContext::Specific(id),
            },
            HirControlKind::Return => ReturnContext::Pass,
        }
    }

    /// Finds the context to return to using `control_flow`
    fn target_context_for(&self, control_flow: HirControlKind) -> Option<&MirContext> {
        let mut current_context = &self.current_context;
        loop {
            match (current_context.kind, control_flow) {
                (MirContextKind::Function, HirControlKind::Return) => break,
                (MirContextKind::Loop, HirControlKind::Break | HirControlKind::Continue) => break,
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
        self.next_context(
            kind,
            self.current_context.super_context_id,
            false,
            return_values_id,
            ReturnContext::Pass.into(),
        );

        Ok(())
    }

    /// Handles a nested block and returns its id
    fn handle_nested_block(
        &mut self,
        block: &HirBlock,
        kind: MirContextKind,
        return_context_behavior: ReturnContextBehaviour,
        return_value_id: Option<MirObjectId>,
    ) -> Result<MirContextId> {
        let mut return_values_data =
            ReturnValuesData::new(kind.default_return_value(&self.singletons));

        // This makes sure that the return value will be copied to the given id, if required.
        if let Some(return_value_id) = return_value_id {
            return_values_data.explicite_return = Some(return_value_id);
        }

        let return_values_id = self.return_values_arena.add(return_values_data);

        let old_context_id = self.next_context(
            kind,
            Some(self.current_context.id),
            false,
            return_values_id,
            return_context_behavior,
        );
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

    fn handle_module(&mut self, module: &HirModule) -> Result<()> {
        let ident = self.get_ident(&module.ident);
        let context_id = self.handle_nested_block(
            &module.block,
            MirContextKind::Module,
            ReturnContextBehaviour::Normal(ReturnContext::Pass),
            None,
        )?;

        let obj_id = self.namespace.insert_object().id;
        self.current_context
            .local_namespace
            .insert(obj_id, ident.clone());

        let ctx = self.get_context(context_id);
        self.namespace.get_obj_mut(obj_id).local_namespace = ctx.local_namespace.clone();

        self.emit(PrimitiveDeclaration {
            span: module.span,
            target: obj_id,
            value: MirPrimitive::Module(MirModule { context_id, ident }),
        });

        Ok(())
    }

    fn handle_object(&mut self, object: &HirObject) -> Result<()> {
        match object {
            HirObject::Function(function) => self.handle_function(function),
            _ => todo!(),
        }
    }

    fn handle_function(&mut self, function: &HirFunction) -> Result<()> {
        let prev_context_id = self.next_context_with_return_data(
            MirContextKind::Function,
            None,
            false,
            ReturnContext::Pass,
        );

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
            HirStatement::VariableUpdate(variable_update) => {
                self.handle_variable_update(variable_update)
            }
            HirStatement::FunctionCall(function_call) => {
                self.handle_function_call(function_call)?;
                Ok(())
            }
            HirStatement::Block(block) => {
                self.handle_nested_block(
                    block,
                    MirContextKind::Block,
                    ReturnContext::Pass.into(),
                    None,
                )?;
                Ok(())
            }
            HirStatement::ConditonalBranch(branch) => {
                self.handle_branch(branch)?;
                Ok(())
            }
            HirStatement::ControlFlow(control_flow) => self.handle_control_flow(control_flow),
            HirStatement::InfiniteLoop(infinite_loop) => self.handle_infinite_loop(infinite_loop),
            HirStatement::Import(import) => self.handle_import(import),
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
                    ident_span: expression.span(),
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
                let context_id = self.handle_nested_block(
                    block,
                    MirContextKind::Block,
                    ReturnContext::Pass.into(),
                    None,
                )?;
                Ok(self
                    .contexts
                    .get(&context_id)
                    .unwrap()
                    .return_values(&self.return_values_arena)
                    .return_value())
            }
            HirExpression::ConditionalBranch(branch) => self.handle_branch(branch),
            HirExpression::Path(path) => self.resolve_path(&path),
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
            obj.get_property(&self.namespace, &ident).ok_or_else(|| {
                LangError::new(
                    LangErrorKind::MissingProperty {
                        similar: vec![],
                        parent: self
                            .compile_context
                            .input_files
                            .get_span_str(accessor.span())
                            .to_string(),
                        property: ident.clone(),
                    },
                    function_call.ident.span,
                )
            })?
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
            ident_span: function_call.ident.span,
            parameters,
            return_value,
            function,
        });
        Ok(return_value)
    }

    fn handle_type_pattern(&mut self, param: &HirTypePattern) -> Result<MirObjectId> {
        match param {
            HirTypePattern::Path(path) => self.resolve_path(path),
            _ => todo!(),
        }
    }

    fn handle_branch(&mut self, branch: &HirConditionalBranch) -> Result<MirObjectId> {
        let next_context = self.create_context(
            self.current_context.kind,
            Some(self.current_context.id),
            true,
            self.current_context.return_values_id,
            self.current_context.return_context.into(),
        );

        // The return context of this context should be disabled, because both branches go to `next_context` by default,
        // which inherits the return context of this context.
        self.current_context.return_context.set_handled_manually();

        let condition = self.handle_expression(&branch.condition)?;

        let pos_context_id = self.handle_nested_block(
            &branch.block_positive,
            MirContextKind::Block,
            ReturnContext::Specific(next_context.id).into(),
            None,
        )?;
        let return_value = self
            .contexts
            .get(&pos_context_id)
            .unwrap()
            .return_values(&self.return_values_arena)
            .return_value();

        let neg_context_id = if let Some(neg_block) = &branch.block_negative {
            self.handle_nested_block(
                neg_block,
                MirContextKind::Block,
                ReturnContext::Specific(next_context.id).into(),
                Some(return_value),
            )?
        } else {
            let old_context_id = self.next_context_with_return_data(
                MirContextKind::Block,
                Some(self.current_context.id),
                false,
                ReturnContext::Specific(next_context.id),
            );

            self.emit(VariableUpdate {
                span: branch.span,
                target: return_value,
                value: self.singletons.null,
            });

            let old_context = self.contexts.remove(&old_context_id).unwrap();
            let context = std::mem::replace(&mut self.current_context, old_context);

            let context_id = context.id;
            self.contexts.insert(context.id, context);

            context_id
        };

        self.emit(Branch {
            span: branch.span,
            condition_span: branch.condition.span(),
            return_value,
            condition,
            pos_branch: pos_context_id,
            neg_branch: neg_context_id,
        });

        let old_context = std::mem::replace(&mut self.current_context, next_context);
        self.contexts.insert(old_context.id, old_context);

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
                    LangErrorKind::InvalidControlFlow {
                        control_flow: control_flow.kind,
                    },
                    control_flow.span,
                )
            })?
            .id;

        let return_context = self.get_return_context(control_flow.kind, context_id);
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

    fn handle_import(&mut self, import: &HirImport) -> Result<()> {
        let ident = self.get_ident(&import.ident);
        let hir_module = self
            .hir
            .imported_modules
            .iter()
            .find(|module| {
                self.compile_context
                    .input_files
                    .get_span_str(module.ident.span)
                    == ident
            })
            .expect("Must already be imported");

        self.handle_module(hir_module)?;
        Ok(())
    }

    fn handle_infinite_loop(&mut self, infinite_loop: &HirInfiniteLoop) -> Result<()> {
        // Create the next context so the loop knows where to break to
        let old_context_id = self.next_context(
            self.current_context.kind,
            Some(self.current_context.id),
            true,
            self.current_context.return_values_id,
            self.current_context.return_context.into(),
        );

        let loop_ctx_id = self.handle_nested_block(
            &infinite_loop.block,
            MirContextKind::Loop,
            ReturnContextBehaviour::Loop,
            None,
        )?;

        self.contexts.get_mut(&old_context_id).unwrap().nodes.push(
            Goto {
                context_id: loop_ctx_id,
                span: infinite_loop.span,
            }
            .into(),
        );

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

/// Simple enum to specify how to calculate the return context
enum ReturnContextBehaviour {
    /// Simple uses the specified return context
    Normal(ReturnContext),
    /// Creates a `ReturnContext::Specific(..)` with the current context as parameter
    Loop,
}

impl From<ReturnContext> for ReturnContextBehaviour {
    fn from(return_context: ReturnContext) -> Self {
        ReturnContextBehaviour::Normal(return_context)
    }
}
