use std::{collections::HashMap, iter, rc::Rc, todo, unimplemented};

use debris_common::{Accessor, CodeRef, Span, SpecialIdent};

use crate::{
    debris_object::ValidPayload,
    error::{LangError, LangErrorKind, Result},
    hir::{
        hir_nodes::{
            HirBlock, HirConditionalBranch, HirConstValue, HirExpression, HirFunction,
            HirFunctionCall, HirImport, HirItem, HirModule, HirObject, HirPropertyDeclaration,
            HirStatement, HirStruct, HirTypePattern, HirVariableInitialization,
        },
        HirVisitor,
    },
    llir::utils::ItemId,
    objects::{
        obj_class::{GenericClass, GenericClassRef, HasClass},
        obj_function::{FunctionParameters, ObjFunction},
        obj_int_static::ObjStaticInt,
        obj_module::{ModuleFactory, ObjModule},
        obj_native_function::{
            match_parameters, FunctionParameterDefinition, ObjNativeFunction,
            ObjNativeFunctionSignature,
        },
        obj_null::ObjNull,
        obj_string::ObjString,
    },
    CompileContext, Namespace, TypePattern,
};

use super::{
    mir_context::AccessedProperty, mir_nodes::MirBranchIf, ContextId, Mir, MirContext,
    MirContextInfo, MirGotoContext, MirNode, MirValue, NamespaceArena,
};

#[derive(Debug)]
pub struct CachedFunctionSignature {
    pub parameters: Vec<FunctionParameterDefinition>,
    pub return_type: TypePattern,
    block_id: usize,
}

/// Visits the hir and creates a mir from it
#[derive(Debug)]
pub struct MirBuilder<'a, 'ctx> {
    mir: &'a mut Mir<'ctx>,
    compile_context: &'ctx CompileContext,
    code: CodeRef<'ctx>,
    context_stack: ContextStack,
    /// Cache for all function definitions that were already visited
    visited_functions: HashMap<Span, Rc<CachedFunctionSignature>>,
    function_blocks: Vec<&'a HirBlock>,
    /// The first visited function gets marked as the main function
    pub(super) main_context: Option<ContextId>,
    /// The global, empty context which only contains modules
    global_context: ContextId,
}

impl<'a> HirVisitor<'a> for MirBuilder<'a, '_> {
    type Output = Result<MirValue>;

    fn visit_item(&mut self, item: &'a HirItem) -> Self::Output {
        match item {
            HirItem::Object(object) => self.visit_object(object),
            HirItem::Statement(statement) => self.visit_statement(statement),
        }
    }

    fn visit_object(&mut self, object: &'a HirObject) -> Self::Output {
        match object {
            HirObject::Function(func) => self.visit_function(func),
            HirObject::Struct(val) => self.visit_struct(val),
            HirObject::Module(module) => self.visit_module(module),
        }
    }

    fn visit_struct(&mut self, _struct_: &'a HirStruct) -> Self::Output {
        unimplemented!("Hir level structs are not yet implemented!")
    }

    fn visit_module(&mut self, module: &'a HirModule) -> Self::Output {
        let context_id = self.add_context_after(self.global_context, module.block.span);
        self.visit_block_local(&module.block)?;
        self.pop_context();

        // Modules are mainly intended to provide objects,
        // but they *are* allowed to emit nodes.
        // If that is the case here, then call that context
        if !self.mir.contexts.get(context_id).nodes.is_empty() {
            self.call_context(context_id, module.span);
        }

        // ToDo: intern idents
        let object_map = self
            .mir
            .contexts
            .get(context_id)
            .namespace(&self.mir.namespaces)
            .into_iter()
            .filter_map(|(ident, value)| match value {
                MirValue::Concrete(obj) => Some((ident.clone(), obj.clone())),
                MirValue::Template { .. } => None,
            })
            .collect();

        let ident = self.context().get_ident(&module.ident);
        let module_obj = ObjModule::with_members(ident.clone(), object_map);
        let object = MirValue::Concrete(module_obj.into_object(self.compile_context));
        self.context_info()
            .add_value(ident, object, module.ident.span)?;

        Ok(MirValue::null(self.compile_context))
    }

    fn visit_block(&mut self, block: &'a HirBlock) -> Self::Output {
        self.add_context(block.span);
        let result = self.visit_block_local(block)?;
        let context = self.pop_context();

        let context_id = context.id;
        self.call_context(context_id, block.span);

        Ok(result)
    }

    fn visit_import(&mut self, import: &'a HirImport) -> Self::Output {
        let accessor = Accessor::Path(
            import
                .accessor
                .idents
                .iter()
                .map(|ident| self.context().get_ident(ident))
                .collect(),
        );
        println!("ToDo: import {:?}", accessor);
        Ok(MirValue::null(self.compile_context))
    }

    fn visit_function(&mut self, function: &'a HirFunction) -> Self::Output {
        fn tmp_type_pattern(ctx: &MirContext, path: &HirTypePattern) -> Result<TypePattern> {
            match path {
                HirTypePattern::Path(path) => {
                    let path = path.single_ident().ok_or_else(|| {
                        LangError::new(
                            LangErrorKind::NotYetImplemented {
                                msg: "Type paths are not yet supported. use a single identifier"
                                    .to_string(),
                            },
                            path.span(),
                        )
                    })?;
                    ctx.get_type_pattern(path)
                }
                HirTypePattern::Function {
                    parameters,
                    return_type,
                    ..
                } => {
                    let mut function_cls =
                        GenericClass::new(ObjFunction::class(ctx.compile_context));

                    let parameters = parameters
                        .iter()
                        .map(|t| tmp_type_pattern(ctx, t))
                        .collect::<Result<Vec<_>>>()?;
                    function_cls.set_generics("In".to_string(), parameters);
                    function_cls.set_generics(
                        "Out".to_string(),
                        vec![match return_type {
                            Some(pattern) => tmp_type_pattern(ctx, pattern.as_ref())?,
                            None => ObjNull::class(ctx.compile_context).as_generic_ref().into(),
                        }],
                    );
                    Ok(function_cls.into_class_ref().into())
                }
            }
        };

        // Get the data about this function
        let visited_function = match self.visited_functions.get(&function.span) {
            Some(visited_function) => visited_function,
            None => {
                // The pattern of values that are allowed to be returned from this
                let result_pattern = match &function.return_type {
                    // ToDo: enable actual paths instead of truncating to the first element
                    Some(return_type) => tmp_type_pattern(self.context(), return_type)?,
                    None => ObjNull::class(self.compile_context).as_generic_ref().into(),
                };

                // Converts the hir paramaeters into a vector of `FunctionParameterDefinition`
                let parameters = function
                    .parameters
                    .iter()
                    .map(|decl| {
                        Ok(FunctionParameterDefinition {
                            expected_type: tmp_type_pattern(self.context(), &decl.typ)?,
                            name: self.context().get_ident(&decl.ident),
                            span: decl.span,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                self.function_blocks.push(&function.block);
                self.visited_functions.insert(
                    function.span,
                    Rc::new(CachedFunctionSignature {
                        parameters,
                        return_type: result_pattern,
                        block_id: self.function_blocks.len() - 1,
                    }),
                );
                self.visited_functions.get(&function.span).unwrap()
            }
        };

        let attributes = function
            .attributes
            .iter()
            .map(|attr| {
                Accessor::Path(
                    attr.accessor
                        .idents
                        .iter()
                        .map(|ident| self.context().get_ident(ident))
                        .collect(),
                )
            })
            .collect();

        let function_signature = ObjNativeFunctionSignature::new(
            self.compile_context,
            self.compile_context.get_unique_id(),
            function.span,
            attributes,
            function.return_type_span(),
            self.context().id,
            visited_function.parameters.as_slice(),
            visited_function.return_type.clone(),
        )
        .into_object(self.compile_context);

        let ident = self.context().get_ident(&function.ident);

        self.context_info()
            .add_value(ident, function_signature.into(), function.span)?;

        Ok(MirValue::null(self.compile_context))
    }

    fn visit_conditional_branch(&mut self, branch: &'a HirConditionalBranch) -> Self::Output {
        let condition = self.visit_expression(&branch.condition)?;
        condition.assert_type(TypePattern::Bool, branch.condition.span(), None)?;

        let context_id = self.add_context(branch.block_positive.span);
        let mut result = self.visit_block_local(&branch.block_positive)?;

        // If the condition is not comptime, the return_value must not be comptime either
        if condition.class().typ().runtime_encodable() && !result.class().typ().runtime_encodable()
        {
            result = self.promote_runtime(result, branch.block_positive.last_item_span())?;
        }

        // Copy the value, since we don't want to alias another variable
        if let Some((class, id)) = result.template() {
            result = self.try_clone(class, id, branch.block_positive.last_item_span())?;
        };
        self.pop_context();

        let (neg_branch, neg_value) = if let Some(neg_branch) = branch.block_negative.as_deref() {
            let context_id = self.add_context(neg_branch.span);

            let mut else_result = self.visit_block_local(neg_branch)?;
            // If the condition is not comptime, the alternative return_value must not be comptime either
            if condition.class().typ().runtime_encodable()
                && !else_result.class().typ().runtime_encodable()
            {
                else_result =
                    self.promote_runtime(else_result, branch.block_positive.last_item_span())?;
            }
            self.pop_context();

            // Asserts that both blocks have the same type
            else_result.assert_type(
                TypePattern::Class(result.class().clone()),
                neg_branch.last_item_span(),
                Some(branch.block_positive.last_item_span()),
            )?;

            (Some(context_id), Some(else_result))
        } else {
            (None, None)
        };

        // If there is no negative branch, the positive branch must return null
        if branch.block_negative.is_none() {
            result.assert_type(
                TypePattern::Class(self.compile_context.type_ctx().null().class.clone()),
                branch.block_positive.last_item_span(),
                None,
            )?;
        }

        self.push(MirNode::BranchIf(MirBranchIf {
            condition,
            pos_branch: context_id,
            neg_branch,
            pos_value: result.clone(),
            neg_value,
            span: branch.span,
        }));

        // The result should now be equivalent to the result_else, because of the mem_copy
        Ok(result)
    }

    fn visit_statement(&mut self, statement: &'a HirStatement) -> Self::Output {
        match statement {
            HirStatement::VariableDecl(declaration) => self.visit_variable_declaration(declaration),
            HirStatement::FunctionCall(call) => self.visit_function_call(call),
            HirStatement::Import(import) => self.visit_import(import),
        }
    }

    fn visit_expression(&mut self, expression: &'a HirExpression) -> Self::Output {
        match expression {
            HirExpression::Value(const_value) => self.visit_const_value(const_value),
            HirExpression::Variable(spanned_ident) => self
                .context_info()
                .get_from_spanned_ident(spanned_ident)
                .map(|value| value.clone()),
            HirExpression::Path(path) => Ok(self.context_info().resolve_path(path)?.value),
            HirExpression::BinaryOperation {
                lhs,
                rhs,
                operation,
            } => {
                // Get the correct function object
                // The Type is solely determined by the lhs value
                let lhs = self.visit_expression(lhs)?;
                let rhs = self.visit_expression(rhs)?;

                let object = lhs
                    .get_property(&operation.operator.get_special_ident().into())
                    .ok_or_else(|| {
                        LangError::new(
                            LangErrorKind::UnexpectedOperator {
                                operator: operation.operator.get_special_ident(),
                                lhs: lhs.class().clone(),
                                rhs: rhs.class().clone(),
                            },
                            operation.span,
                        )
                    })?;

                let (return_value, node) = self.context_info().register_function_call(
                    object,
                    vec![lhs, rhs],
                    None,
                    operation.span,
                )?;

                self.push(node);
                Ok(return_value)
            }
            HirExpression::UnaryOperation {
                operation: _,
                value: _,
            } => {
                todo!("Unary operations are not yet implemented")
            }
            HirExpression::Block(block) => self.visit_block(block),
            HirExpression::FunctionCall(function_call) => self.visit_function_call(function_call),
            HirExpression::ConditionalBranch(branch) => self.visit_conditional_branch(branch),
        }
    }

    fn visit_function_call(&mut self, function_call: &'a HirFunctionCall) -> Self::Output {
        let AccessedProperty { value, parent } =
            self.context_info().resolve_path(&function_call.accessor)?;

        let function_object = match value {
            MirValue::Concrete(function) => function,
            MirValue::Template { id: _, class } => {
                return Err(LangError::new(
                    LangErrorKind::UnexpectedType {
                        declared: None,
                        expected: ObjFunction::class(self.compile_context)
                            .as_generic_ref()
                            .into(),
                        got: class,
                    },
                    function_call.parameters_span,
                )
                .into())
            }
        };

        let parameters = function_call
            .parameters
            .iter()
            .map(|expr| self.visit_expression(expr))
            .collect::<Result<Vec<_>>>()?;

        // Native functions are created per call
        // because its easier to track the parameter types and return types
        // So if this object is a native function signature, evaluate it now
        let mut exact_return_value = None;
        let function_object = if let Some(function_sig) =
            function_object.downcast_payload::<ObjNativeFunctionSignature>()
        {
            if self.context_stack.contains(&function_sig.function_span) {
                return Err(LangError::new(
                    LangErrorKind::NotYetImplemented {
                        msg: "Recursive function call".to_string(),
                    },
                    function_call.accessor.span(),
                )
                .into());
            }
            let context_id =
                self.add_context_after(function_sig.definition_scope, function_sig.function_span);

            let signature = self
                .visited_functions
                .get(&function_sig.function_span)
                .unwrap()
                .clone();

            // Check for actual paramaters that do not match the declared parameters
            if !match_parameters(
                &signature.parameters,
                parameters.iter().map(|param| param.class().as_ref()),
            ) {
                return Err(LangError::new(
                    LangErrorKind::UnexpectedOverload {
                        expected: vec![(
                            FunctionParameters::Specific(
                                signature
                                    .parameters
                                    .iter()
                                    .map(|param| param.expected_type.clone())
                                    .collect(),
                            ),
                            signature.return_type.clone(),
                        )],
                        parameters: parameters
                            .into_iter()
                            .map(|value| value.class().clone())
                            .collect(),
                    },
                    function_call.span,
                )
                .into());
            }

            let return_value = {
                for (parameter, sig) in parameters.iter().zip(signature.parameters.iter()) {
                    self.context_info()
                        .add_value(sig.name.clone(), parameter.clone(), sig.span)?;
                }

                let result = self.visit_block_local(self.function_blocks[signature.block_id])?;
                // println!("Call: {:?}\n\n", self.namespace_mut());
                self.pop_context();
                result
            };

            // Check for an actual return type that does not match the declared type
            if !signature.return_type.matches(return_value.class()) {
                return Err(LangError::new(
                    LangErrorKind::UnexpectedType {
                        declared: Some(function_sig.return_type_span),
                        expected: signature.return_type.clone(),
                        got: return_value.class().clone(),
                    },
                    self.function_blocks[signature.block_id].last_item_span(),
                )
                .into());
            }

            // Set the exact return value to the return value
            // Since we just execute the entire function, this is much better than just the class
            // For example, knowing this allows for functions as values
            exact_return_value = Some(return_value.clone());

            ObjNativeFunction::new(
                self.compile_context,
                context_id,
                signature,
                Rc::clone(return_value.class()),
            )
            .into_object(self.compile_context)
        } else {
            function_object
        };

        let (return_class_value, function_node) = self.context_info().register_function_call(
            function_object,
            parameters,
            parent,
            function_call.span,
        )?;

        let return_value = exact_return_value.unwrap_or(return_class_value);

        self.push(function_node);
        Ok(return_value)
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &'a HirVariableInitialization,
    ) -> Self::Output {
        let value = self.visit_expression(&variable_declaration.value)?;
        let value = self.try_clone_if_var(value, variable_declaration.span)?;

        let ident = self.context().get_ident(&variable_declaration.ident);
        self.context_info()
            .add_value(ident, value, variable_declaration.span)?;

        Ok(MirValue::null(&self.compile_context))
    }

    fn visit_property_declaration(
        &mut self,
        _property_declaration: &'a HirPropertyDeclaration,
    ) -> Self::Output {
        unimplemented!("No structs - no properties")
    }

    fn visit_const_value(&mut self, const_value: &'a HirConstValue) -> Self::Output {
        Ok(match const_value {
            HirConstValue::Integer { span: _, value } => ObjStaticInt::new(*value)
                .into_object(&self.compile_context)
                .into(),
            HirConstValue::Fixed { span: _, value: _ } => unimplemented!("Fixed point numbers"),
            HirConstValue::String { span: _, value } => ObjString::new(value.clone())
                .into_object(&self.compile_context)
                .into(),
        })
    }
}

impl<'a, 'ctx> MirBuilder<'a, 'ctx> {
    pub fn new(
        mir: &'a mut Mir<'ctx>,
        modules: &[ModuleFactory],
        compile_context: &'ctx CompileContext,
        code: CodeRef<'ctx>,
    ) -> Self {
        // The global context which contains the imported modules
        let mut global_context = MirContext::new(&mut mir.namespaces, None, compile_context, code);

        for module_factory in modules {
            let module = module_factory.call(&compile_context);
            if module_factory.import_members() {
                global_context.register_members(&mut mir.namespaces, module);
            } else {
                global_context.register(&mut mir.namespaces, module);
            }
        }

        let global_context_id = global_context.id;
        mir.add_context(global_context);
        let context_stack = ContextStack {
            current_context: global_context_id,
            current_span: Span::empty(),
            stack: Vec::new(),
        };

        MirBuilder {
            mir,
            compile_context,
            code,
            context_stack,
            visited_functions: HashMap::new(),
            function_blocks: Vec::new(),
            main_context: None,
            global_context: global_context_id,
        }
    }

    #[inline]
    fn current_context(&self) -> ContextId {
        self.context_stack.current_context
    }

    /// Visits a block without creating a new context
    fn visit_block_local(&mut self, block: &'a HirBlock) -> Result<MirValue> {
        for object in &block.objects {
            self.visit_object(object)?;
        }

        for statement in &block.statements {
            self.visit_statement(statement)?;
        }

        let return_value = match &block.return_value {
            Some(return_value) => self.visit_expression(&return_value)?,
            None => MirValue::null(self.compile_context),
        };

        Ok(return_value)
    }

    /// Creates a new context and pushes it to the top
    fn add_context(&mut self, span: Span) -> ContextId {
        self.add_context_after(self.context().id, span)
    }

    /// Creates a new context that is not successor of the current,
    /// but successor of a specific context
    fn add_context_after(&mut self, ancestor: ContextId, span: Span) -> ContextId {
        let context: MirContext<'ctx> = MirContext::new(
            &mut self.mir.namespaces,
            Some(ancestor),
            self.compile_context,
            self.code,
        );
        if self.main_context.is_none() {
            self.main_context = Some(context.id);
        }
        self.context_stack.push(context.id, span);
        self.mir.add_context(context);
        self.context_stack.current_context
    }

    fn pop_context(&mut self) -> &mut MirContext<'ctx> {
        let prev_context = self.current_context();
        self.context_stack.pop();

        self.mir.contexts.get_mut(prev_context)
    }

    /// Adds a mir node to the current context
    fn push(&mut self, mir_node: MirNode) {
        self.context_mut().nodes.push(mir_node)
    }

    /// Promotes a comptime value to its runtime variant
    fn promote_runtime(&mut self, value: MirValue, span: Span) -> Result<MirValue> {
        let function = value
            .class()
            .get_property(&SpecialIdent::PromoteRuntime.into())
            .ok_or_else(|| {
                LangError::new(
                    LangErrorKind::UnpromotableType {
                        got: value.class().clone(),
                    },
                    span,
                )
            })?;

        let (result, node) =
            self.context_info()
                .register_function_call(function, vec![value], None, span)?;
        self.push(node);
        Ok(result)
    }

    /// Generates a function call to that context.
    /// After the other context was executed, the current context
    /// will continue to run normally.
    fn call_context(&mut self, context_id: ContextId, span: Span) {
        let node = MirNode::GotoContext(MirGotoContext { context_id, span });
        self.push(node);
    }

    /// Tries to clone this value or returns the original value if it does not need to be cloned
    fn try_clone_if_var(&mut self, value: MirValue, span: Span) -> Result<MirValue> {
        if let Some((class, id)) = value.template() {
            if self.namespace_mut().has_item_key(id.id) {
                return self.try_clone(class, id, span);
            }
        }
        Ok(value)
    }

    /// Tries to clone the value, returns it unmodified if that value cannot be cloned
    fn try_clone(&mut self, class: GenericClassRef, id: ItemId, span: Span) -> Result<MirValue> {
        let function = class.get_property(&SpecialIdent::Clone.into());
        if let Some(function) = function {
            let (value, node) = self.context_info().register_function_call(
                function,
                vec![MirValue::Template { class, id }],
                None,
                span,
            )?;
            self.push(node);
            Ok(value)
        } else {
            Ok(MirValue::Template { class, id })
        }
    }
}

/// Implements functionality for working with the attributes mutably
impl<'code> MirBuilder<'_, 'code> {
    /// Returns a mutable reference to the current context
    pub fn context_mut(&mut self) -> &mut MirContext<'code> {
        self.mir.contexts.get_mut(self.current_context())
    }

    /// Returns a shared reference to the current context
    pub fn context(&self) -> &MirContext {
        self.mir.contexts.get(self.current_context())
    }

    /// Returns a helper struct that contains both a context and the arena
    pub fn context_info(&mut self) -> MirContextInfo<'_, 'code> {
        self.mir.context(self.current_context())
    }

    /// Returns a mutable reference to the current namespace
    pub fn namespace_mut(&mut self) -> &mut Namespace {
        let id = self.context_info().context.id;
        &mut self.mir.namespaces[id.as_inner()]
    }

    /// Returns a mutable reference to the global arena
    pub fn arena_mut(&mut self) -> &mut NamespaceArena {
        &mut self.mir.namespaces
    }

    /// Returns a shared reference to the global arena
    pub fn arena(&self) -> &NamespaceArena {
        &self.mir.namespaces
    }
}

/// Keeps track of the current compile context
#[derive(Debug)]
pub struct ContextStack {
    /// The current compile context
    current_context: ContextId,
    /// The span of the current context
    current_span: Span,
    /// The stack, consisting of (context, origin of this context)
    stack: Vec<(ContextId, Span)>,
}

impl ContextStack {
    fn push(&mut self, next_context: ContextId, next_span: Span) {
        self.stack.push((self.current_context, self.current_span));
        self.current_context = next_context;
        self.current_span = next_span;
    }

    fn pop(&mut self) {
        let (previous_context, previous_span) =
            self.stack.pop().expect("Tried to pop the global stack");
        self.current_context = previous_context;
        self.current_span = previous_span;
    }

    fn contains(&self, span: &Span) -> bool {
        iter::once(&self.current_span)
            .chain(self.stack.iter().map(|entry| &entry.1))
            .any(|val| val == span)
    }
}
