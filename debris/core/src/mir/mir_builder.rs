use std::{rc::Rc, unimplemented};

use debris_common::{CodeRef, Span};
use generational_arena::Index;

use crate::{
    debris_object::ValidPayload,
    error::{LangError, LangErrorKind, Result},
    hir::{
        hir_nodes::{
            HirBlock, HirConstValue, HirExpression, HirFunction, HirFunctionCall, HirItem,
            HirObject, HirPropertyDeclaration, HirStatement, HirStruct, HirTypePattern,
            HirVariableInitialization,
        },
        HirVisitor,
    },
    objects::{
        FunctionParameterDefinition, HasClass, ModuleFactory, ObjClass, ObjFunction,
        ObjNativeFunction, ObjNativeFunctionSignature, ObjNull, ObjStaticInt, ObjString,
    },
    CompileContext, Namespace, TypePattern,
};

use super::{
    mir_context::AccessedProperty, Mir, MirContext, MirContextInfo, MirGotoContext,
    MirNamespaceEntry, MirNode, MirValue, NamespaceArena,
};

/// Visits the hir and creates a mir from it
#[derive(Debug)]
pub struct MirBuilder<'a, 'ctx> {
    mir: &'a mut Mir<'ctx>,
    compile_context: &'ctx CompileContext,
    code: CodeRef<'ctx>,
    current_context: usize,
    context_stack: Vec<usize>,
}

impl HirVisitor for MirBuilder<'_, '_> {
    type Output = Result<MirValue>;

    fn visit_item(&mut self, item: &HirItem) -> Self::Output {
        match item {
            HirItem::Object(object) => self.visit_object(object),
            HirItem::Statement(statement) => self.visit_statement(statement),
        }
    }

    fn visit_object(&mut self, object: &HirObject) -> Self::Output {
        match object {
            HirObject::Function(func) => self.visit_function(func),
            HirObject::Struct(val) => self.visit_struct(val),
        }
    }

    fn visit_struct(&mut self, _struct_: &HirStruct) -> Self::Output {
        unimplemented!("Hir level structs are not yet implemented!")
    }

    fn visit_function(&mut self, function: &HirFunction) -> Self::Output {
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
                    let function_cls: Rc<ObjClass> = ObjFunction::class(ctx.compile_context);

                    let parameters = parameters
                        .iter()
                        .map(|t| tmp_type_pattern(ctx, t))
                        .collect::<Result<Vec<_>>>()?;
                    function_cls.set_generics("In".to_string(), parameters);
                    function_cls.set_generics(
                        "Out".to_string(),
                        vec![match return_type {
                            Some(pattern) => tmp_type_pattern(ctx, pattern.as_ref())?,
                            None => ObjNull::class(ctx.compile_context).into(),
                        }],
                    );
                    Ok(function_cls.into())
                }
            }
        };

        // The pattern of values that are allowed to be returned from this
        let result_pattern = match &function.return_type {
            // ToDo: enable actual paths instead of truncating to the first element
            Some(return_type) => tmp_type_pattern(self.context(), return_type)?,
            None => ObjNull::class(self.compile_context).into(),
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

        let function_signature = ObjNativeFunctionSignature::new(
            self.compile_context.get_unique_id(),
            function.block.clone(),
            parameters,
            result_pattern,
            function.return_type_span(),
            self.context().namespace_idx,
        )
        .into_object(self.compile_context);

        let ident = self.context().get_ident(&function.ident);

        self.context_info()
            .add_value(ident, function_signature.into(), function.span)?;

        Ok(MirValue::null(self.compile_context))
    }

    fn visit_statement(&mut self, statement: &HirStatement) -> Self::Output {
        match statement {
            HirStatement::Block(block) => self.visit_block(block),
            HirStatement::FunctionCall(call) => self.visit_function_call(call),
            HirStatement::VariableDecl(declaration) => self.visit_variable_declaration(declaration),
        }
    }

    fn visit_block(&mut self, block: &HirBlock) -> Self::Output {
        self.add_context();
        let result = self.visit_block_local(block)?;
        let context = self.pop_context();

        let context_id = context.id;
        self.call_context(context_id, block.span);

        Ok(result)
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &HirVariableInitialization,
    ) -> Self::Output {
        let value = self.visit_expression(&variable_declaration.value)?;
        let ident = self.context().get_ident(&variable_declaration.ident);
        self.context_info()
            .add_value(ident, value, variable_declaration.span)?;

        Ok(MirValue::null(&self.compile_context))
    }

    fn visit_property_declaration(
        &mut self,
        _property_declaration: &HirPropertyDeclaration,
    ) -> Self::Output {
        unimplemented!("No structs - no properties")
    }

    fn visit_expression(&mut self, expression: &HirExpression) -> Self::Output {
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
            HirExpression::FunctionCall(function_call) => self.visit_function_call(function_call),
            HirExpression::Block(block) => self.visit_block(block),
        }
    }

    fn visit_function_call(&mut self, function_call: &HirFunctionCall) -> Self::Output {
        let AccessedProperty { value, parent } =
            self.context_info().resolve_path(&function_call.accessor)?;

        let function_object = match value {
            MirValue::Concrete(function) => function,
            MirValue::Template { id: _, class: _ } => {
                return Err(LangError::new(
                    LangErrorKind::NotYetImplemented {
                        msg: "Higher order functions".to_string(),
                    },
                    function_call.span,
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
            // Verify that the amount of parameters is correct
            if function_sig.parameter_signature.len() != parameters.len() {
                return Err(LangError::new(
                    LangErrorKind::UnexpectedOverload {
                        expected: vec![(
                            function_sig
                                .parameter_signature
                                .iter()
                                .map(|param| param.expected_type.clone())
                                .collect::<Vec<_>>()
                                .into(),
                            function_sig.return_type.clone(),
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

            let context_id = self.add_context_after(function_sig.definition_scope);

            let return_value = {
                for (parameter, sig) in parameters
                    .iter()
                    .zip(function_sig.parameter_signature.iter())
                {
                    self.context_info()
                        .add_value(sig.name.clone(), parameter.clone(), sig.span)?;
                }

                let result = self.visit_block_local(&function_sig.block)?;
                // println!("Call: {:?}\n\n", self.namespace_mut());
                self.pop_context();
                result
            };

            if !function_sig.return_type.matches(return_value.class()) {
                return Err(LangError::new(
                    LangErrorKind::UnexpectedType {
                        declared: Some(function_sig.return_type_span),
                        expected: function_sig.return_type.clone(),
                        got: return_value.class().clone(),
                    },
                    function_sig.block.last_item_span(),
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
                function_sig.parameter_signature.clone(),
                return_value.class().clone(),
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

    fn visit_const_value(&mut self, const_value: &HirConstValue) -> Self::Output {
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
        let mut global_context =
            MirContext::new(&mut mir.namespaces, None, 0, compile_context, code);

        for module_factory in modules {
            let module = module_factory.call(&compile_context);
            if module_factory.import_members() {
                global_context.register_members(&mut mir.namespaces, module);
            } else {
                global_context.register(&mut mir.namespaces, module);
            }
        }

        mir.contexts.push(global_context);

        MirBuilder {
            mir,
            compile_context,
            code,
            current_context: 0,
            context_stack: vec![0],
        }
    }

    /// Visits a block without creating a new context
    fn visit_block_local(&mut self, block: &HirBlock) -> Result<MirValue> {
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
    fn add_context(&mut self) -> u64 {
        self.add_context_after(self.context().namespace_idx)
    }

    /// Creates a new context that is not successor of the current,
    /// but successor of a specific context
    fn add_context_after(&mut self, ancestor: Index) -> u64 {
        self.current_context = self.mir.contexts.len();
        self.context_stack.push(self.current_context);

        let context: MirContext<'ctx> = MirContext::new(
            &mut self.mir.namespaces,
            Some(ancestor),
            self.current_context as u64,
            self.compile_context,
            self.code,
        );
        self.mir.add_context(context);
        self.current_context as u64
    }

    fn pop_context(&mut self) -> &mut MirContext<'ctx> {
        let prev_context = self.current_context;
        self.context_stack.pop().expect("Popped the global context");
        self.current_context = *self
            .context_stack
            .last()
            .expect("Popped the global context");

        &mut self.mir.contexts[prev_context]
    }

    /// Adds a mir node to the current context
    fn push(&mut self, mir_node: MirNode) {
        self.context_mut().nodes.push(mir_node)
    }

    /// Generates a function call to that context.
    /// After the other context was executed, the current context
    /// will continue to run normally.
    fn call_context(&mut self, context_id: u64, span: Span) {
        let node = MirNode::GotoContext(MirGotoContext { context_id, span });
        self.push(node);
    }
}

/// Implements functionality for working with the attributes mutably
impl<'code> MirBuilder<'_, 'code> {
    /// Returns a mutable reference to the current context
    pub fn context_mut(&mut self) -> &mut MirContext<'code> {
        &mut self.mir.contexts[self.current_context]
    }

    /// Returns a shared reference to the current context
    pub fn context(&self) -> &MirContext {
        &self.mir.contexts[self.current_context]
    }

    /// Returns a helper struct that contains both a context and the arena
    pub fn context_info(&mut self) -> MirContextInfo<'_, 'code> {
        self.mir.context(self.current_context)
    }

    /// Returns a mutable reference to the current namespace
    pub fn namespace_mut(&mut self) -> &mut Namespace<MirNamespaceEntry> {
        let index = self.context_info().context.namespace_idx;
        &mut self.arena_mut()[index]
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
