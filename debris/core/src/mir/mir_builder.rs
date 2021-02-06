use std::{collections::HashMap, iter, rc::Rc};

use debris_common::{Accessor, CodeRef, Span, SpecialIdent};

use crate::{
    debris_object::ValidPayload,
    error::{LangError, LangErrorKind, Result},
    hir::{
        hir_nodes::{
            HirBlock, HirConditionalBranch, HirConstValue, HirControlFlow, HirExpression,
            HirFunction, HirFunctionCall, HirImport, HirItem, HirModule, HirObject,
            HirPropertyDeclaration, HirStatement, HirStruct, HirTypePattern,
            HirVariableInitialization,
        },
        HirVisitor,
    },
    llir::utils::ItemId,
    objects::{
        obj_bool_static::ObjStaticBool,
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
    CompileContext, Namespace, ObjectRef, TypePattern,
};

use super::{
    mir_context::AccessedProperty,
    mir_nodes::{MirBranchIf, MirJumpLocation},
    ContextId, ContextKind, ControlFlowMode, Mir, MirContext, MirContextInfo, MirGotoContext,
    MirNode, MirValue, NamespaceArena,
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
    context_stack: ContextStack,
    /// A list of all modules that are imported
    hir_modules: &'a [HirModule],
    /// Cache for all function definitions that were already visited
    visited_functions: HashMap<Span, Rc<CachedFunctionSignature>>,
    function_blocks: Vec<&'a HirBlock>,
    /// The first visited function gets marked as the main function
    pub main_context: Option<ContextId>,
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
        let context_id =
            self.add_context_after(self.global_context, module.block.span, ContextKind::Block);
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
            .iter()
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
        let context_id = self.add_context(block.span, ContextKind::Block);
        let result = self.visit_block_local(block)?;
        self.pop_context();

        self.call_context(context_id, block.span);

        Ok(result)
    }

    fn visit_import(&mut self, import: &'a HirImport) -> Self::Output {
        let id = import.id;
        let module = &self.hir_modules[id];
        self.visit_module(module)
    }

    fn visit_control_flow(&mut self, control_flow: &'a HirControlFlow) -> Self::Output {
        let jump_location = self
            .context_stack
            .jump_location_for(control_flow.kind.into());
        if jump_location.is_none() {
            return Err(LangError::new(
                LangErrorKind::InvalidControlFlow {
                    mode: control_flow.kind.into(),
                },
                control_flow.span,
            )
            .into());
        }

        self.context_mut().control_flow = ControlFlowMode::from(control_flow.kind);
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
                        GenericClass::new(&ObjFunction::class(ctx.compile_context));

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
        // Get the condition, whose value is not yet known - just the type
        let condition = self.visit_expression(&branch.condition)?;
        condition.assert_type(TypePattern::Bool, branch.condition.span(), None)?;

        // Visit the positive block
        let pos_branch =
            self.add_context(branch.block_positive.span, ContextKind::ConditionalBlock);
        let mut pos_value = self.visit_block_local(&branch.block_positive)?;

        // If the condition is not comptime, the return_value must not be comptime either
        if condition.class().typ().runtime_encodable()
            && !pos_value.class().typ().runtime_encodable()
        {
            pos_value = self.promote_runtime(pos_value, branch.block_positive.last_item_span())?;
        }

        // Copy the value, since we don't want to alias another variable
        if let Some((class, id)) = pos_value.template() {
            pos_value = self.try_clone(class, id, branch.block_positive.last_item_span())?;
        };
        self.pop_context();

        // Visit the negative block
        let (neg_branch, neg_value) = if let Some(neg_branch) = branch.block_negative.as_deref() {
            let context_id = self.add_context(neg_branch.span, ContextKind::ConditionalBlock);

            let mut else_result = self.visit_block_local(neg_branch)?;
            // If the condition is not comptime, the alternative return_value must not be comptime either
            if condition.class().typ().runtime_encodable()
                && !else_result.class().typ().runtime_encodable()
            {
                else_result =
                    self.promote_runtime(else_result, branch.block_positive.last_item_span())?;
            }
            self.pop_context();

            (context_id, else_result)
        } else {
            let context_id = self.add_context(Span::empty(), ContextKind::ConditionalBlock);
            self.pop_context();
            (context_id, MirValue::null(self.compile_context))
        };

        // Asserts that both blocks have the same type
        neg_value.assert_type(
            TypePattern::Class(pos_value.class().clone()),
            branch.block_positive.last_item_span(),
            branch
                .block_negative
                .as_deref()
                .map(|block| block.last_item_span()),
        )?;

        // If either of the values is concrete, set up a new template.
        // Once the condition gets evaluated, the template gets replaced by the
        // correct result
        let mut result = pos_value.clone();
        if pos_value.concrete().is_some() || neg_value.concrete().is_some() {
            // Make the result a template.
            // Once the condition is evaluated, the template will be replaced
            // by either pos_value
            result = self
                .context_info()
                .add_anonymous_template(pos_value.class().clone());
        }

        let value_id = result.template().map(|(_, id)| id);
        self.push(MirNode::BranchIf(MirBranchIf {
            span: branch.span,
            pos_branch,
            neg_branch,
            pos_value,
            neg_value,
            value_id,
            condition,
        }));

        // Set a jump location here, don't automatically run since that
        // is done in both of the branches
        let jump_location = self.context_mut().next_jump_location();
        self.push(MirNode::JumpLocation(MirJumpLocation {
            index: jump_location,
            run: false,
        }));

        let default_next = (self.context().id, jump_location);

        // Set the successor contexts for both blocks. If none, then normal control flow
        // is used
        for branch_id in [pos_branch, neg_branch].iter() {
            let branch_next = self.get_jump_target(*branch_id, default_next).unwrap();

            let context = self.mir.context_info(*branch_id).context;
            context.nodes.push(MirNode::GotoContext(MirGotoContext {
                // ToDo: Error Message
                context_id: branch_next.0,
                block_id: branch_next.1,
                span: branch.span,
            }));
        }

        // The result should now be equivalent to the result_else, because of the mem_copy
        Ok(result)
    }

    fn visit_statement(&mut self, statement: &'a HirStatement) -> Self::Output {
        // If a return target is already set, then this statement
        // comes after a control-flow statement, which is illegal.
        if !self.context().control_flow.is_normal() {
            return Err(LangError::new(LangErrorKind::UnreachableCode, statement.span()).into());
        }

        match statement {
            HirStatement::VariableDecl(declaration) => self.visit_variable_declaration(declaration),
            HirStatement::FunctionCall(call) => self.visit_function_call(call),
            HirStatement::Import(import) => self.visit_import(import),
            HirStatement::ControlFlow(control_flow) => self.visit_control_flow(control_flow),
            HirStatement::Block(block) => {
                self.visit_block(block)?;
                Ok(MirValue::null(self.compile_context))
            }
            HirStatement::ConditonalBranch(branch) => {
                self.visit_conditional_branch(branch)?;
                Ok(MirValue::null(self.compile_context))
            }
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
                operation,
                value: _,
            } => {
                // Note: this feature is blocked by the precedence handler.
                // Right now, unary operators are parsed with the wrong precedence
                Err(LangError::new(
                    LangErrorKind::NotYetImplemented {
                        msg: "Unary operations are not yet implemented".to_string(),
                    },
                    operation.span,
                )
                .into())
            }
            HirExpression::Block(block) => self.visit_block(block),
            HirExpression::FunctionCall(function_call) => self.visit_function_call(function_call),
            HirExpression::ConditionalBranch(branch) => self.visit_conditional_branch(branch),
        }
    }

    fn visit_function_call(&mut self, function_call: &'a HirFunctionCall) -> Self::Output {
        let AccessedProperty { value, parent } =
            self.context_info().resolve_path(&function_call.accessor)?;

        // If the object is not yet known, there is no way to call it
        let object = match value {
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

        // Since the function might have to jump back to this point, set a jump location
        let next_jump_location = self.context_mut().next_jump_location();
        self.context_stack.push_jump_location(
            ControlFlowMode::Return,
            self.context().id,
            next_jump_location,
        );

        // Native functions are created per call
        // because its easier to track the parameter types and return types
        // So if this object is a native function signature, evaluate it now
        let (function_object, return_value) = if let Some(function_sig) =
            object.downcast_payload::<ObjNativeFunctionSignature>()
        {
            let (function, return_value) =
                self.instantiate_native_function(function_sig, &parameters, function_call.span)?;
            (function, Some(return_value))
        } else {
            (object, None)
        };

        // Register the function call in the previous context
        let (return_class_value, function_node) = self.context_info().register_function_call(
            function_object,
            parameters,
            parent,
            function_call.span,
        )?;

        // If the function is not a native function, use the normal `return_class_value`
        let return_value = return_value.unwrap_or(return_class_value);

        self.push(function_node);

        self.context_stack
            .pop_jump_location(ControlFlowMode::Return);

        self.push(MirNode::JumpLocation(MirJumpLocation {
            index: next_jump_location,
            run: true,
        }));

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
            HirConstValue::Bool { value, .. } => ObjStaticBool::from(*value)
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
        hir_modules: &'a [HirModule],
        modules: &[ModuleFactory],
        compile_context: &'ctx CompileContext,
        code: CodeRef<'ctx>,
    ) -> Self {
        // The global context which contains the imported modules
        let mut global_context = MirContext::new(
            &mut mir.namespaces,
            None,
            compile_context,
            code.get_span(),
            ContextKind::Block,
        );

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
            jump_locations: Vec::new(),
        };

        MirBuilder {
            mir,
            compile_context,
            context_stack,
            hir_modules,
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

    fn push_existing_context(&mut self, id: ContextId) {
        if self.main_context.is_none() {
            self.main_context = Some(id);
        }
        self.context_stack.push(id, self.mir.contexts.get(id).span);
    }

    /// Creates a new context and pushes it to the top
    fn add_context(&mut self, span: Span, kind: ContextKind) -> ContextId {
        self.add_context_after(self.context().id, span, kind)
    }

    /// Creates a new context that is not successor of the current,
    /// but successor of a specific context
    fn add_context_after(
        &mut self,
        ancestor: ContextId,
        span: Span,
        kind: ContextKind,
    ) -> ContextId {
        let context: MirContext<'ctx> = MirContext::new(
            &mut self.mir.namespaces,
            Some(ancestor),
            self.compile_context,
            span,
            kind,
        );
        let id = context.id;
        self.mir.add_context(context);
        self.push_existing_context(id);
        id
    }

    fn pop_context(&mut self) -> &mut MirContext<'ctx> {
        let prev_context = self.current_context();
        self.context_stack.pop();

        self.mir.contexts.get_mut(prev_context)
    }

    /// Calculates the next jump target for `context_id`
    fn get_jump_target(
        &self,
        context_id: ContextId,
        default: (ContextId, usize),
    ) -> Option<(ContextId, usize)> {
        let mode = self.mir.contexts.get(context_id).control_flow;
        if mode.is_normal() {
            Some(default)
        } else {
            self.context_stack.jump_location_for(mode)
        }
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
        let node = MirNode::GotoContext(MirGotoContext {
            context_id,
            block_id: 0,
            span,
        });
        self.push(node);
    }

    /// Instantiates a native function signature and returns
    /// a NativeFunction object and its exact return type
    fn instantiate_native_function(
        &mut self,
        function_sig: &ObjNativeFunctionSignature,
        parameters: &[MirValue],
        span: Span,
    ) -> Result<(ObjectRef, MirValue)> {
        if self.context_stack.contains(&function_sig.function_span) {
            return Err(LangError::new(
                LangErrorKind::NotYetImplemented {
                    msg: "Recursive function call".to_string(),
                },
                span,
            )
            .into());
        }

        let context_id = self.add_context_after(
            function_sig.definition_scope,
            function_sig.function_span,
            ContextKind::NativeFunction,
        );

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
                        .iter()
                        .map(|value| value.class().clone())
                        .collect(),
                },
                span,
            )
            .into());
        }

        let return_value = {
            for (parameter, sig) in parameters.iter().zip(signature.parameters.iter()) {
                self.context_info()
                    .add_value(sig.name.clone(), parameter.clone(), sig.span)?;
            }

            let result = self.visit_block_local(self.function_blocks[signature.block_id])?;

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

        Ok((
            ObjNativeFunction::new(
                self.compile_context,
                context_id,
                signature,
                Rc::clone(return_value.class()),
            )
            .into_object(self.compile_context),
            return_value,
        ))
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
        self.mir.context_info(self.current_context())
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
/// And the current points of interest in the stack frame.
#[derive(Debug)]
pub struct ContextStack {
    /// The current compile context
    current_context: ContextId,
    /// The span of the current context
    current_span: Span,
    /// The stack, consisting of (context, origin of this context)
    stack: Vec<(ContextId, Span)>,
    jump_locations: Vec<(ControlFlowMode, (ContextId, usize))>,
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

    pub fn push_jump_location(&mut self, mode: ControlFlowMode, id: ContextId, index: usize) {
        self.jump_locations.push((mode, (id, index)));
    }

    pub fn pop_jump_location(&mut self, mode: ControlFlowMode) {
        let (old_mode, _id) = self
            .jump_locations
            .pop()
            .expect("Tried to pop from an empty stack");
        assert_eq!(
            old_mode, mode,
            "Invalid stack element: Expected {:?} but got {:?}",
            mode, old_mode
        );
    }

    /// Selectes the current jump location for a give control flow
    pub fn jump_location_for(&self, mode: ControlFlowMode) -> Option<(ContextId, usize)> {
        self.jump_locations
            .iter()
            .rev()
            .find(|(other_mode, _id)| *other_mode == mode)
            .map(|(_, id)| *id)
    }

    fn contains(&self, span: &Span) -> bool {
        iter::once(&self.current_span)
            .chain(self.stack.iter().map(|entry| &entry.1))
            .any(|val| val == span)
    }
}
