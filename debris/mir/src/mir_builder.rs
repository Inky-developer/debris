use std::{collections::HashSet, iter::zip};

use itertools::Itertools;
use rustc_hash::FxHashMap;

use debris_common::{CompileContext, FxIndexMap, Ident, Span};
use debris_error::{ControlFlowRequires, LangError, LangErrorKind, Result};
use debris_hir::{
    hir_nodes::{
        self, HirBlock, HirConditionalBranch, HirConstValue, HirControlFlow, HirControlKind,
        HirDeclarationMode, HirExpression, HirFormatStringMember, HirFunction, HirFunctionCall,
        HirImport, HirInfiniteLoop, HirModule, HirObject, HirStatement, HirStruct,
        HirStructInitialization, HirTupleInitialization, HirTypePattern, HirVariableInitialization,
        HirVariablePattern, HirVariableUpdate,
    },
    Hir, IdentifierPath, SpannedIdentifier,
};

use crate::{
    mir_context::{
        MirContext, MirContextId, MirContextKind, ReturnContext, ReturnValuesArena,
        ReturnValuesData, ReturnValuesDataId,
    },
    mir_nodes::{
        Branch, FunctionCall, Goto, MirNode, PrimitiveDeclaration, PropertyUpdate, RuntimeCopy,
        RuntimePromotion, VariableUpdate, VerifyPropertyExists,
    },
    mir_object::MirObjectId,
    mir_primitives::{
        MirFormatString, MirFormatStringComponent, MirFunction, MirFunctionParameter, MirModule,
        MirPrimitive, MirStruct,
    },
    namespace::{MirLocalNamespace, MirLocalNamespaceId, MirNamespace},
    Mir, MirExternItem,
};
use crate::{
    mir_nodes::{VerifyTupleLength, VerifyValueComptime},
    mir_primitives::MirStructType,
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
    extern_items: FxHashMap<Ident, MirExternItem>,
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

        let null = namespace.insert_object(entry_context).id;
        let never = namespace.insert_object(entry_context).id;
        let singletons = MirSingletons { null, never };

        let root_context_kind = MirContextKind::Block;
        let return_values =
            ReturnValuesData::new(root_context_kind.default_return_value(&singletons));
        let return_values_id = return_values_arena.add(return_values);

        let mut current_context = MirContext::new(
            entry_context,
            None,
            namespace.insert_local_namespace(),
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

    fn insert_object(&mut self) -> MirObjectId {
        self.namespace.insert_object(self.current_context.id).id
    }

    fn get_context(&self, id: MirContextId) -> &MirContext {
        if id == self.current_context.id {
            &self.current_context
        } else {
            &self.contexts[&id]
        }
    }

    fn get_local_namespace(&mut self, id: MirContextId) -> &mut MirLocalNamespace {
        let context = if id == self.current_context.id {
            &mut self.current_context
        } else {
            self.contexts.get_mut(&id).unwrap()
        };

        self.namespace
            .get_local_namespace_mut(context.local_namespace_id)
    }

    fn get_variable_opt(&mut self, ident: &Ident) -> Option<MirObjectId> {
        let mut current_context = &self.current_context;

        loop {
            if let Some(obj_id) = current_context
                .local_namespace(&mut self.namespace)
                .get_property(ident)
            {
                break Some(obj_id);
            }

            if let Some(prev_context) = current_context.super_context_id {
                current_context = &self.contexts[&prev_context];
            } else if let Some(item) = self.extern_items.get(ident) {
                break Some(item.object_id);
            } else {
                break None;
            }
        }
    }

    /// Traverses the namespaces stack until it finds `ident`.
    /// If `ident` cannot be found, it will be inserted at the lowest namespace
    fn variable_get_or_insert(&mut self, ident: Ident, definition_span: Span) -> MirObjectId {
        self.get_variable_opt(&ident).unwrap_or_else(|| {
            let object_id = self.insert_object();
            self.extern_items.insert(
                ident,
                MirExternItem {
                    definition_span,
                    object_id,
                },
            );
            object_id
        })
    }

    fn get_ident(&self, spanned_ident: &SpannedIdentifier) -> Ident {
        self.compile_context
            .input_files
            .get_span_str(spanned_ident.span)
            .into()
    }

    fn get_object_property(
        &mut self,
        obj_id: Option<MirObjectId>,
        ident: Ident,
        span: Span,
    ) -> MirObjectId {
        match obj_id {
            Some(obj_id) => {
                let obj = self.namespace.get_obj_mut(obj_id);
                let defining_context = obj.defining_context;
                let obj_id = obj.id;
                obj_id.property_get_or_insert(
                    &mut self.namespace,
                    &mut self.current_context.nodes,
                    ident,
                    span,
                    defining_context,
                )
            }
            None => self.variable_get_or_insert(ident, span),
        }
    }

    /// Returns the object specified by the path
    fn resolve_path(&mut self, path: &IdentifierPath) -> MirObjectId {
        let (obj, ident) = self.resolve_path_without_last(path);
        self.get_object_property(obj, ident, path.last().span)
    }

    /// Resolves the path up to the last ident, so that the attribute can be set manually
    fn resolve_path_without_last(&mut self, path: &IdentifierPath) -> (Option<MirObjectId>, Ident) {
        match path.idents() {
            [single] => (None, self.get_ident(single)),
            [multiple @ .., last] => {
                let mut obj: Option<MirObjectId> = None;
                let ident = self.get_ident(last);

                for spanned_ident in multiple {
                    let ident = self.get_ident(spanned_ident);

                    obj = Some(match obj {
                        None => self.variable_get_or_insert(
                            self.get_ident(spanned_ident),
                            spanned_ident.span,
                        ),
                        Some(obj) => obj.property_get_or_insert(
                            &mut self.namespace,
                            &mut self.current_context.nodes,
                            ident,
                            spanned_ident.span,
                            self.current_context.id,
                        ),
                    });
                }

                (obj, ident)
            }
            [] => unreachable!(),
        }
    }

    /// Creates a new context with default return new return values data.
    /// returns the previous context id
    fn next_context_with_return_data(
        &mut self,
        kind: MirContextKind,
        super_ctx_id: Option<MirContextId>,
        local_namespace_id: MirLocalNamespaceId,
        return_context: ReturnContext,
    ) -> MirContextId {
        let return_values_id = self.return_values_arena.add(ReturnValuesData::new(
            kind.default_return_value(&self.singletons),
        ));
        self.next_context(
            kind,
            super_ctx_id,
            local_namespace_id,
            return_values_id,
            return_context.into(),
        )
    }

    /// Creates a new context and returns the previous one
    fn next_context(
        &mut self,
        kind: MirContextKind,
        super_ctx_id: Option<MirContextId>,
        local_namespace_id: MirLocalNamespaceId,
        return_values_data_id: ReturnValuesDataId,
        return_context_behavior: ReturnContextBehavior,
    ) -> MirContextId {
        let context = self.create_context(
            kind,
            super_ctx_id,
            local_namespace_id,
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
        local_namespace_id: MirLocalNamespaceId,
        return_values_data_id: ReturnValuesDataId,
        return_context_behavior: ReturnContextBehavior,
    ) -> MirContext {
        let next_id = self.next_context_id;
        self.next_context_id += 1;

        let context_id = MirContextId {
            compilation_id: self.compile_context.compilation_id,
            id: next_id,
        };

        let return_context = match return_context_behavior {
            ReturnContextBehavior::Normal(return_context) => return_context,
            ReturnContextBehavior::Loop => ReturnContext::Specific(context_id),
        };

        MirContext::new(
            context_id,
            super_ctx_id,
            local_namespace_id,
            kind,
            return_values_data_id,
            return_context,
        )
    }

    fn return_value(&mut self, context_id: MirContextId, value: MirObjectId, span: Span) {
        // Promote the value if returned from a dynamic context
        let value = if self.current_context.kind.is_runtime() {
            self.promote(value, span)
        } else {
            value
        };

        if let Some((return_value, _)) =
            get_context(&self.contexts, &self.current_context, &context_id)
                .return_values(&self.return_values_arena)
                .explicit_return
        {
            self.emit(VariableUpdate {
                span,
                target: return_value,
                value,
                comptime_update_allowed: false,
            });
        } else {
            let context =
                get_context_mut(&mut self.contexts, &mut self.current_context, &context_id);
            context
                .return_values_mut(&mut self.return_values_arena)
                .explicit_return = Some((value, span));
            if context_id == self.current_context.id {
                self.current_context
                    .return_values_mut(&mut self.return_values_arena)
                    .unconditionally_returned = true;
            }
        }
    }

    // Promotes a value to its runtime variant
    fn promote(&mut self, value: MirObjectId, span: Span) -> MirObjectId {
        let promoted = self.insert_object();
        self.emit(RuntimePromotion {
            span,
            target: promoted,
            value,
        });
        promoted
    }

    // Copies a value if it is a runtime value
    fn copy(&mut self, value: MirObjectId, span: Span) -> MirObjectId {
        let copied = self.insert_object();
        self.emit(RuntimeCopy {
            span,
            target: copied,
            value,
        });
        copied
    }

    /// Calculates the [`ReturnContext`] to use for a context with control flow,
    /// where `kind` is the kind of control flow and
    /// `context_id` is the context that is targeted by the control flow
    fn get_return_context(&self, kind: HirControlKind, context_id: MirContextId) -> ReturnContext {
        match kind {
            HirControlKind::Break => {
                let context = self.get_context(context_id);
                assert_eq!(context.kind, MirContextKind::Loop);

                context
                    .super_context_id
                    .map_or(ReturnContext::Pass, ReturnContext::Specific)
            }
            HirControlKind::Continue => match self.get_context(context_id).return_context {
                ReturnContext::Pass => ReturnContext::Pass,
                ReturnContext::Specific(id) | ReturnContext::ManuallyHandled(id) => {
                    ReturnContext::Specific(id)
                }
            },
            HirControlKind::Return => ReturnContext::Pass,
        }
    }

    /// Finds the context to return to using `control_flow`
    fn target_context_for(&self, control_flow: HirControlKind) -> Option<&MirContext> {
        let mut current_context = &self.current_context;
        loop {
            match (current_context.kind, control_flow) {
                (MirContextKind::FunctionRuntime, HirControlKind::Return)
                | (MirContextKind::Loop, HirControlKind::Break | HirControlKind::Continue) => break,
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
        let local_namespace_id = self.namespace.insert_local_namespace();
        self.next_context(
            kind,
            self.current_context.super_context_id,
            local_namespace_id,
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
        return_context_behavior: ReturnContextBehavior,
        return_value_id: Option<(MirObjectId, Span)>,
    ) -> Result<MirContextId> {
        let mut return_values_data =
            ReturnValuesData::new(kind.default_return_value(&self.singletons));

        // This makes sure that the return value will be copied to the given id, if required.
        if let Some(return_value_id) = return_value_id {
            return_values_data.explicit_return = Some(return_value_id);
        }

        let return_values_data_id = self.return_values_arena.add(return_values_data);
        let local_namespace_id = self.namespace.insert_local_namespace();
        let old_context_id = self.next_context(
            kind,
            Some(self.current_context.id),
            local_namespace_id,
            return_values_data_id,
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

        self.handle_context_objects(&block.objects)?;

        for statement in &block.statements {
            // TODO: Throw warning if this context has early returned
            self.handle_statement(statement)?;
        }

        if let Some(return_value) = &block.return_value {
            let return_span = return_value.span();
            let return_value = self.handle_expression(return_value)?;
            self.return_value(self.current_context.id, return_value, return_span);
        } else if !self.current_context.has_early_returned {
            let return_value = self
                .current_context
                .kind
                .default_return_value(&self.singletons);
            let return_span = block.last_item_span();
            self.return_value(self.current_context.id, return_value, return_span);
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

    fn handle_hir_block(&mut self, block: &HirBlock) -> Result<MirObjectId> {
        let and_then_context = self.create_context(
            self.current_context.kind,
            self.current_context.super_context_id,
            self.current_context.local_namespace_id,
            self.current_context.return_values_id,
            self.current_context.return_context.into(),
        );

        let context_id = self.handle_nested_block(
            block,
            MirContextKind::Block,
            ReturnContext::Specific(and_then_context.id).into(),
            None,
        )?;
        self.current_context.return_context = ReturnContext::Specific(context_id);

        let old_context = std::mem::replace(&mut self.current_context, and_then_context);
        self.contexts.insert(old_context.id, old_context);

        Ok(self
            .contexts
            .get(&context_id)
            .unwrap()
            .return_values(&self.return_values_arena)
            .return_value())
    }

    fn register_object_name(&mut self, spanned_ident: SpannedIdentifier) -> MirObjectId {
        let obj_id = self.namespace.insert_object(self.current_context.id).id;
        let ident = self.get_ident(&spanned_ident);
        self.namespace
            .get_local_namespace_mut(self.current_context.local_namespace_id)
            .insert(obj_id, ident, spanned_ident.span);
        obj_id
    }

    fn handle_context_objects(&mut self, objects: &[HirObject]) -> Result<()> {
        // Already register all the function names, so that functions can call functions which are later declared
        for object in objects {
            if let HirObject::Function(function) = object {
                if let Some(spanned_ident) = function.ident.spanned_ident() {
                    self.register_object_name(spanned_ident);
                }
            }
        }

        for object in objects {
            match object {
                HirObject::Function(function) => {
                    self.handle_function(function)?;
                }
                HirObject::Module(module) => self.handle_module(module)?,
                HirObject::Import(import) => self.handle_import(import)?,
                HirObject::Struct(strukt) => self.handle_struct(strukt)?,
            };
        }

        Ok(())
    }

    fn handle_module(&mut self, module: &HirModule) -> Result<()> {
        let context_id = self.handle_nested_block(
            &module.block,
            MirContextKind::Module,
            ReturnContextBehavior::Normal(ReturnContext::Pass),
            None,
        )?;

        let ident = self.get_ident(&module.ident);
        let obj_id = self.insert_object();
        self.namespace
            .get_local_namespace_mut(self.current_context.local_namespace_id)
            .insert(obj_id, ident.clone(), module.ident.span);

        let ctx_local_namespace = self.get_local_namespace(context_id);
        *self.namespace.get_obj_namespace_mut(obj_id) = ctx_local_namespace.clone();

        self.emit(PrimitiveDeclaration {
            span: module.span,
            target: obj_id,
            value: MirPrimitive::Module(MirModule {
                context_id,
                ident,
                span: module.span,
                last_item_span: module.block.last_item_span(),
            }),
        });

        Ok(())
    }

    fn handle_function(&mut self, function: &HirFunction) -> Result<MirObjectId> {
        let ident = function.ident.into_ident(&self.compile_context.input_files);
        let function_obj_id = self
            .namespace
            .get_local_namespace(self.current_context.local_namespace_id)
            .get_property(&ident)
            .unwrap_or_else(|| {
                if let Some(ident) = function.ident.spanned_ident() {
                    self.register_object_name(ident)
                } else {
                    self.namespace.insert_object(self.current_context.id).id
                }
            });

        let next_context_id = MirContextId {
            compilation_id: self.compile_context.compilation_id,
            id: self.next_context_id,
        };
        let parameters = function
            .parameters
            .iter()
            .map(|param| {
                let value = self.namespace.insert_object(next_context_id).id;
                let typ = self.handle_type_pattern(&param.typ)?;
                let span = param.span;
                Ok(MirFunctionParameter { span, typ, value })
            })
            .collect::<Result<Vec<_>>>()?;

        let local_namespace_id = self.namespace.insert_local_namespace();
        let prev_context_id = self.next_context_with_return_data(
            MirContextKind::FunctionRuntime,
            Some(self.current_context.id),
            local_namespace_id,
            ReturnContext::Pass,
        );

        for (parameter, param_declaration) in zip(&parameters, &function.parameters) {
            let ident = self.get_ident(&param_declaration.ident);
            self.current_context
                .local_namespace(&mut self.namespace)
                .insert(parameter.value, ident, param_declaration.ident.span);
        }

        self.handle_block_keep_context(&function.block)?;

        let prev_context = self.contexts.remove(&prev_context_id).unwrap();
        let function_ctx = std::mem::replace(&mut self.current_context, prev_context);

        let return_type = function
            .return_type
            .as_ref()
            .map(|pat| self.handle_type_pattern(pat))
            .transpose()?;
        let return_type_span = function.return_type_span();
        let return_span = function_ctx
            .return_values(&self.return_values_arena)
            .return_span()
            .unwrap_or_else(|| function.block.last_item_span());
        let function_primitive = MirPrimitive::Function(MirFunction {
            signature_span: function.signature_span,
            context_id: function_ctx.id,
            name: ident.clone(),
            parameters,
            return_type,
            return_span,
            return_type_span,
        });

        self.emit(PrimitiveDeclaration {
            span: function.span,
            target: function_obj_id,
            value: function_primitive,
        });
        self.current_context
            .local_namespace(&mut self.namespace)
            .insert(function_obj_id, ident, function.ident.span());

        self.contexts.insert(function_ctx.id, function_ctx);

        self.apply_function_attributes(function_obj_id, &function.attributes)?;

        Ok(function_obj_id)
    }

    /// Treats the function attributes as functions and calls these with this function as parameter
    fn apply_function_attributes(
        &mut self,
        function_id: MirObjectId,
        attributes: &[hir_nodes::Attribute],
    ) -> Result<()> {
        for attribute in attributes {
            let attribute_obj = self.handle_expression(&attribute.expression)?;
            self.emit(FunctionCall {
                function: attribute_obj,
                value_span: attribute.span(),
                parameters: vec![function_id],
                return_value: self.singletons.null,
                self_obj: None,
                span: attribute.span(),
            });
        }

        Ok(())
    }

    fn handle_struct(&mut self, strukt: &HirStruct) -> Result<()> {
        let struct_ident = self.get_ident(&strukt.ident);
        let struct_obj_id = self.insert_object();
        self.get_local_namespace(self.current_context.id).insert(
            struct_obj_id,
            struct_ident.clone(),
            strukt.ident.span,
        );

        let mut map =
            FxIndexMap::with_capacity_and_hasher(strukt.properties.len(), Default::default());
        for property in &strukt.properties {
            let typ = self.handle_type_pattern(&property.datatype)?;
            let ident = self.get_ident(&property.ident);
            map.insert(ident, (typ, property.span));
        }

        let struct_namespace_id = self.namespace.get_obj(struct_obj_id).local_namespace_id;
        let old_context_id = self.next_context_with_return_data(
            MirContextKind::Struct,
            Some(self.current_context.id),
            struct_namespace_id,
            ReturnContext::Pass,
        );
        self.handle_context_objects(&strukt.objects)?;

        let struct_context = std::mem::replace(
            &mut self.current_context,
            self.contexts.remove(&old_context_id).unwrap(),
        );
        let struct_context_id = struct_context.id;
        self.contexts.insert(struct_context.id, struct_context);

        let mir_struct = MirStructType {
            name: struct_ident,
            properties: map,
            context_id: struct_context_id,
        };
        self.emit(PrimitiveDeclaration {
            span: strukt.ident.span,
            target: struct_obj_id,
            value: MirPrimitive::StructType(mir_struct),
        });

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
            HirStatement::Block(block) => {
                self.handle_hir_block(block)?;
                Ok(())
            }
            HirStatement::ConditionalBranch(branch) => {
                self.handle_branch(branch)?;
                Ok(())
            }
            HirStatement::ControlFlow(control_flow) => {
                self.handle_control_flow(control_flow)?;
                Ok(())
            }
            HirStatement::InfiniteLoop(infinite_loop) => {
                self.handle_infinite_loop(infinite_loop)?;
                Ok(())
            }
            HirStatement::Expression(expr) => {
                self.handle_expression(expr)?;
                Ok(())
            }
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
            mode: HirDeclarationMode,
            span: Span,
        ) {
            match pattern {
                HirVariablePattern::Path(path) => {
                    let value = match mode {
                        HirDeclarationMode::Comptime => value,
                        HirDeclarationMode::Let => {
                            let runtime_value = this.promote(value, span);
                            let old_namespace = this.namespace.get_obj(value).local_namespace_id;
                            this.namespace.get_obj_mut(runtime_value).local_namespace_id =
                                old_namespace;
                            runtime_value
                        }
                    };

                    // if this declaration used comptime, verify that the value is actually known at compile time
                    if matches!(mode, HirDeclarationMode::Comptime) {
                        this.emit(VerifyValueComptime {
                            value,
                            span: path.span(),
                        });
                    }

                    let (obj_id_opt, last_ident) = this.resolve_path_without_last(path);

                    if let Some(obj_id) = obj_id_opt {
                        this.emit(VerifyPropertyExists {
                            span: path.last().span,
                            ident: last_ident.clone(),
                            obj_id,
                        });
                    }

                    let local_namespace = match obj_id_opt {
                        None => this.current_context.local_namespace(&mut this.namespace),
                        Some(obj_id) => this.namespace.get_obj_namespace_mut(obj_id),
                    };
                    local_namespace.insert(value, last_ident, path.last().span);
                }
                HirVariablePattern::Tuple(patterns) => {
                    let span = match patterns.as_slice() {
                        [] => span,
                        [single] => single.span(),
                        [first, .., last] => first.span().until(last.span()),
                    };
                    this.emit(VerifyTupleLength {
                        length: patterns.len(),
                        value,
                        span,
                    });
                    for (index, pattern) in patterns.iter().enumerate() {
                        let value = value.property_get_or_insert(
                            &mut this.namespace,
                            &mut this.current_context.nodes,
                            Ident::Index(index),
                            pattern.span(),
                            this.current_context.id,
                        );
                        handle_pattern(this, pattern, value, mode, span);
                    }
                }
            }
        }

        let value = self.handle_expression(&variable_decl.value)?;
        handle_pattern(
            self,
            &variable_decl.pattern,
            value,
            variable_decl.mode,
            variable_decl.span,
        );

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
                        let value = value.property_get_or_insert(
                            &mut this.namespace,
                            &mut this.current_context.nodes,
                            Ident::Index(index),
                            pattern.span(),
                            this.current_context.id,
                        );
                        handle_pattern(this, pattern, value, span);
                    }
                }
                HirVariablePattern::Path(path) => {
                    let check_comptime_update_allowed =
                        |this: &mut MirBuilder, obj: MirObjectId| {
                            let prev_val_def_ctx = this.namespace.get_obj(obj).defining_context;
                            !exists_runtime_context(
                                prev_val_def_ctx,
                                &this.contexts,
                                &this.current_context,
                            )
                        };

                    let (obj_opt, last_ident) = this.resolve_path_without_last(path);
                    if let Some(parent) = obj_opt {
                        let comptime_update_allowed = check_comptime_update_allowed(this, parent);
                        this.emit(PropertyUpdate {
                            span,
                            parent,
                            ident: last_ident,
                            value,
                            comptime_update_allowed,
                        });
                    } else {
                        let target =
                            this.get_object_property(obj_opt, last_ident, path.last().span);
                        let comptime_update_allowed = check_comptime_update_allowed(this, target);
                        this.emit(VariableUpdate {
                            span,
                            target,
                            value,
                            comptime_update_allowed,
                        });
                    }
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
                    &mut self.current_context.nodes,
                    operation.operator.get_special_ident().into(),
                    operation.span,
                    self.current_context.id,
                );
                let return_value = self.insert_object();

                self.emit(FunctionCall {
                    span: expression.span(),
                    value_span: expression.span(),
                    function,
                    parameters: vec![lhs, rhs],
                    self_obj: None,
                    return_value,
                });
                Ok(return_value)
            }
            HirExpression::UnaryOperation { operation, value } => {
                let value = self.handle_expression(value)?;

                let function = value.property_get_or_insert(
                    &mut self.namespace,
                    &mut self.current_context.nodes,
                    operation.operator.get_ident(),
                    operation.span,
                    self.current_context.id,
                );
                let return_value = self.insert_object();

                self.emit(FunctionCall {
                    span: expression.span(),
                    value_span: expression.span(),
                    function,
                    parameters: vec![value],
                    self_obj: None,
                    return_value,
                });
                Ok(return_value)
            }
            HirExpression::Function(function) => self.handle_function(function),
            HirExpression::FunctionCall(function_call) => self.handle_function_call(function_call),
            HirExpression::Variable(spanned_ident) => {
                let ident = self.get_ident(spanned_ident);
                Ok(self.variable_get_or_insert(ident, spanned_ident.span))
            }
            HirExpression::Path(path) => Ok(self.resolve_path(path)),
            HirExpression::PropertyAccess { lhs, rhs } => {
                let lhs = self.handle_expression(lhs)?;
                let ident = self.get_ident(rhs);
                Ok(self.get_object_property(Some(lhs), ident, rhs.span))
            }
            HirExpression::Block(block) => self.handle_hir_block(block),
            HirExpression::InfiniteLoop(infinite_loop) => self.handle_infinite_loop(infinite_loop),
            HirExpression::ConditionalBranch(branch) => self.handle_branch(branch),
            HirExpression::TupleInitialization(tuple_initialization) => {
                self.handle_tuple_initialization(tuple_initialization)
            }
            HirExpression::StructInitialization(struct_initialization) => {
                self.handle_struct_initialization(struct_initialization)
            }
            HirExpression::ControlFlow(control_flow) => self.handle_control_flow(control_flow),
        }
    }

    /// If the expression accesses a value on an object, this method returns both the parent and the
    /// accessed value
    fn handle_expression_and_base(
        &mut self,
        expr: &HirExpression,
    ) -> Result<(MirObjectId, Option<MirObjectId>)> {
        match expr {
            HirExpression::PropertyAccess { lhs, rhs } => {
                let parent = self.handle_expression(lhs)?;
                let result = self.get_object_property(Some(parent), self.get_ident(rhs), rhs.span);
                Ok((result, Some(parent)))
            }
            HirExpression::Path(path) => {
                let (obj, last) = self.resolve_path_without_last(path);
                let result = self.get_object_property(obj, last, path.last().span);
                Ok((result, obj))
            }
            _ => self.handle_expression(expr).map(|expr| (expr, None)),
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
                    .map(|member| {
                        Ok(match member {
                            HirFormatStringMember::String(val) => {
                                MirFormatStringComponent::String(val.clone())
                            }
                            HirFormatStringMember::Variable(hir_expression) => {
                                let expr = self.handle_expression(hir_expression)?;
                                MirFormatStringComponent::Value(expr)
                            }
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                MirPrimitive::FormatString(MirFormatString::from(parts))
            }
        };

        let obj = self.insert_object();
        self.emit(PrimitiveDeclaration {
            value: primitive,
            target: obj,
            span: value.span(),
        });
        Ok(obj)
    }

    fn handle_function_call(&mut self, function_call: &HirFunctionCall) -> Result<MirObjectId> {
        let (function, base) = self.handle_expression_and_base(&function_call.value)?;

        let parameters = function_call
            .parameters
            .iter()
            .map(|param| self.handle_expression(param))
            .try_collect()?;
        let return_value = self.insert_object();

        self.emit(FunctionCall {
            span: function_call.span,
            value_span: function_call.value.span(),
            parameters,
            self_obj: base,
            return_value,
            function,
        });
        Ok(return_value)
    }

    fn handle_type_pattern(&mut self, param: &HirTypePattern) -> Result<MirObjectId> {
        match param {
            HirTypePattern::Path(path) => Ok(self.resolve_path(path)),
            HirTypePattern::Tuple { span, values } => {
                let obj_ids = values
                    .iter()
                    .map(|value| self.handle_type_pattern(value).map(|id| (id, value.span())))
                    .try_collect()?;
                let tuple_id = self.insert_object();
                self.emit(PrimitiveDeclaration {
                    span: *span,
                    target: tuple_id,
                    value: MirPrimitive::TupleClass(obj_ids),
                });
                Ok(tuple_id)
            }
            HirTypePattern::Function {
                span,
                parameters,
                return_type,
            } => {
                let parameters = parameters
                    .iter()
                    .map(|param| self.handle_type_pattern(param))
                    .try_collect()?;
                let return_type = return_type
                    .as_ref()
                    .map(|return_type| self.handle_type_pattern(return_type))
                    .transpose()?;

                let function_type_id = self.insert_object();
                self.emit(PrimitiveDeclaration {
                    span: *span,
                    target: function_type_id,
                    value: MirPrimitive::FunctionClass(parameters, return_type),
                });
                Ok(function_type_id)
            }
        }
    }

    fn handle_branch(&mut self, branch: &HirConditionalBranch) -> Result<MirObjectId> {
        let branch_kind = if branch.is_comptime {
            MirContextKind::BlockConditionalComptime
        } else {
            MirContextKind::BlockConditionalRuntime
        };

        let next_context = self.create_context(
            self.current_context.kind,
            self.current_context.super_context_id,
            self.current_context.local_namespace_id,
            self.current_context.return_values_id,
            self.current_context.return_context.into(),
        );

        // The return context of this context should be disabled, because both branches go to `next_context` by default,
        // which inherits the return context of this context.
        self.current_context.return_context.set_handled_manually();

        let condition = self.handle_expression(&branch.condition)?;

        let pos_context_id = self.handle_nested_block(
            &branch.block_positive,
            branch_kind,
            ReturnContext::Specific(next_context.id).into(),
            None,
        )?;
        let return_value = self
            .contexts
            .get(&pos_context_id)
            .unwrap()
            .return_values(&self.return_values_arena)
            .return_value();
        // set the defining context of the return value to the current context,
        // so it can be found by both branches
        self.namespace.get_obj_mut(return_value).defining_context = self.current_context.id;
        let return_value_span = self
            .contexts
            .get(&pos_context_id)
            .unwrap()
            .return_values(&self.return_values_arena)
            .explicit_return
            .map_or_else(|| branch.block_positive.last_item_span(), |(_, span)| span);

        let neg_context_id = if let Some(neg_block) = &branch.block_negative {
            self.handle_nested_block(
                neg_block,
                branch_kind,
                ReturnContext::Specific(next_context.id).into(),
                Some((return_value, return_value_span)),
            )?
        } else {
            let local_namespace_id = self.namespace.insert_local_namespace();
            let old_context_id = self.next_context_with_return_data(
                branch_kind,
                Some(self.current_context.id),
                local_namespace_id,
                ReturnContext::Specific(next_context.id),
            );

            // Don't update singleton values, these should be constant.
            if !self.singletons.contains(return_value) {
                let ret_val_def_ctx = self.namespace.get_obj(return_value).defining_context;
                let comptime_update_allowed =
                    !exists_runtime_context(ret_val_def_ctx, &self.contexts, &self.current_context);
                self.emit(VariableUpdate {
                    span: branch.span,
                    target: return_value,
                    value: self.singletons.null,
                    comptime_update_allowed,
                });
            }

            let old_context = self.contexts.remove(&old_context_id).unwrap();
            let context = std::mem::replace(&mut self.current_context, old_context);

            let context_id = context.id;
            self.contexts.insert(context.id, context);

            context_id
        };

        self.emit(Branch {
            span: branch.span,
            is_comptime: branch.is_comptime,
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

    fn handle_control_flow(&mut self, control_flow: &HirControlFlow) -> Result<MirObjectId> {
        if !control_flow.kind.takes_value() && control_flow.expression.is_some() {
            return Err(LangError::new(
                LangErrorKind::ContinueWithValue,
                control_flow.expression.as_ref().unwrap().span(),
            )
            .into());
        }

        let raw_expression = match &control_flow.expression {
            Some(expression) => self.handle_expression(expression)?,
            None => self.singletons.null,
        };

        let context_id = self
            .target_context_for(control_flow.kind)
            .ok_or_else(|| {
                LangError::new(
                    LangErrorKind::InvalidControlFlow {
                        control_flow: control_flow.kind.to_string(),
                        requires: match control_flow.kind {
                            HirControlKind::Return => ControlFlowRequires::Function,
                            HirControlKind::Break | HirControlKind::Continue => {
                                ControlFlowRequires::Loop
                            }
                        },
                    },
                    control_flow.span,
                )
            })?
            .id;

        let return_context = self.get_return_context(control_flow.kind, context_id);
        self.current_context.return_context = return_context;

        if control_flow.kind.returns() {
            // For now always promote the return value. In the future this should only happen
            // if the affected context is not labelled comptime.
            let expression = self.promote(raw_expression, control_flow.span);
            self.return_value(context_id, expression, control_flow.span);
        }

        // The block which contains the control flow itself should return the never type
        if context_id != self.current_context.id {
            self.return_value(
                self.current_context.id,
                self.singletons.never,
                control_flow.span,
            );
        }

        self.current_context.has_early_returned = true;

        Ok(self.singletons.never)
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

    fn handle_tuple_initialization(
        &mut self,
        tuple_initialization: &HirTupleInitialization,
    ) -> Result<MirObjectId> {
        let target = self.insert_object();

        let values: Vec<(MirObjectId, Span)> = tuple_initialization
            .values
            .iter()
            .map(|value| {
                self.handle_expression(value)
                    .map(|expr| (self.copy(expr, value.span()), value.span()))
            })
            .try_collect()?;

        // also mark the values for the object
        let obj_namespace_id = self.namespace.get_obj_mut(target).local_namespace_id;
        for (index, (value, span)) in values.iter().enumerate() {
            let ident = Ident::Index(index);
            self.namespace
                .get_local_namespace_mut(obj_namespace_id)
                .insert(*value, ident, *span);
        }

        self.emit(PrimitiveDeclaration {
            span: tuple_initialization.span,
            target,
            value: MirPrimitive::Tuple(values.into_iter().map(|(value, _)| value).collect()),
        });

        Ok(target)
    }

    fn handle_struct_initialization(
        &mut self,
        struct_initialization: &HirStructInitialization,
    ) -> Result<MirObjectId> {
        let target = self.insert_object();

        let values: FxHashMap<Ident, (MirObjectId, Span)> = struct_initialization
            .values
            .iter()
            .map(|(ident, expr)| {
                let span = ident.span;
                let ident = self.get_ident(ident);
                let raw_value = self.handle_expression(expr)?;
                let value = self.copy(raw_value, span);
                Ok((ident, (value, span)))
            })
            .collect::<Result<_>>()?;

        // also mark the values for the object
        let obj_namespace_id = self.namespace.get_obj_mut(target).local_namespace_id;
        for (ident, (value, span)) in &values {
            self.namespace
                .get_local_namespace_mut(obj_namespace_id)
                .insert(*value, ident.clone(), *span);
        }

        let struct_type = self.handle_expression(&struct_initialization.base)?;

        self.emit(PrimitiveDeclaration {
            span: struct_initialization.span,
            target,
            value: MirPrimitive::Struct(MirStruct {
                struct_type,
                base_span: struct_initialization.base.span(),
                values,
            }),
        });

        Ok(target)
    }

    fn handle_infinite_loop(&mut self, infinite_loop: &HirInfiniteLoop) -> Result<MirObjectId> {
        // Create the next context so the loop knows where to break to
        let old_context_id = self.next_context(
            self.current_context.kind,
            Some(self.current_context.id), // TODO: Should this be `self.current_context.super_ctx_id`?
            self.current_context.local_namespace_id,
            self.current_context.return_values_id,
            self.current_context.return_context.into(),
        );

        // The return context of this context should be disabled, because no code can run after an infinite loop
        // If there is a break in the loop, the next context can still be entered.
        self.contexts
            .get_mut(&old_context_id)
            .unwrap()
            .return_context
            .set_handled_manually();

        let loop_ctx_id = self.handle_nested_block(
            &infinite_loop.block,
            MirContextKind::Loop,
            ReturnContextBehavior::Loop,
            None,
        )?;

        self.contexts.get_mut(&old_context_id).unwrap().nodes.push(
            Goto {
                context_id: loop_ctx_id,
                span: infinite_loop.span,
            }
            .into(),
        );

        Ok(self
            .contexts
            .get(&loop_ctx_id)
            .unwrap()
            .return_values(&self.return_values_arena)
            .return_value())
    }
}

fn get_context_mut<'a>(
    contexts: &'a mut FxHashMap<MirContextId, MirContext>,
    current_context: &'a mut MirContext,
    context_id: &MirContextId,
) -> &'a mut MirContext {
    if let Some(context) = contexts.get_mut(context_id) {
        context
    } else {
        assert_eq!(current_context.id, *context_id);
        current_context
    }
}

fn get_context<'a>(
    contexts: &'a FxHashMap<MirContextId, MirContext>,
    current_context: &'a MirContext,
    context_id: &MirContextId,
) -> &'a MirContext {
    contexts.get(context_id).map_or_else(
        || {
            assert_eq!(current_context.id, *context_id);
            current_context
        },
        |context| context,
    )
}

/// Returns true if any context, that lies 'between' `target_id` and `current_context`, is marked as runtime context.
/// This is used to check whether a variable update can be performed at compile time
fn exists_runtime_context(
    target_id: MirContextId,
    contexts: &FxHashMap<MirContextId, MirContext>,
    current_context: &MirContext,
) -> bool {
    let mut target_hierarchy = HashSet::new();
    let mut target_context = get_context(contexts, current_context, &target_id);
    loop {
        target_hierarchy.insert(target_context.id);
        if let Some(predecessor) = target_context.super_context_id {
            target_context = get_context(contexts, current_context, &predecessor);
        } else {
            break;
        }
    }

    let mut current_context = current_context;
    loop {
        if target_hierarchy.contains(&current_context.id) {
            return false;
        }

        if current_context.kind.is_runtime() {
            return true;
        }

        current_context = match current_context.super_context_id {
            Some(ctx_id) => &contexts[&ctx_id],
            None => break,
        };
    }
    // This can be reached if the current context got split into multiple contexts (due to control flow)
    // In this case the first half of this context will never be checked
    false
}

/// Holds some singletons objects for easier access
#[derive(Debug)]
pub struct MirSingletons {
    pub null: MirObjectId,
    pub never: MirObjectId,
}

impl MirSingletons {
    pub fn contains(&self, value: MirObjectId) -> bool {
        value == self.null || value == self.never
    }
}

/// Simple enum to specify how to calculate the return context
#[derive(Clone, Copy)]
enum ReturnContextBehavior {
    /// Simple uses the specified return context
    Normal(ReturnContext),
    /// Creates a `ReturnContext::Specific(..)` with the current context as parameter
    Loop,
}

impl From<ReturnContext> for ReturnContextBehavior {
    fn from(return_context: ReturnContext) -> Self {
        ReturnContextBehavior::Normal(return_context)
    }
}
