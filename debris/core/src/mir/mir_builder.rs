use std::{rc::Rc, unimplemented};

use debris_common::{CodeRef, Span};

use crate::{
    debris_object::ValidPayload,
    error::{LangError, LangErrorKind, Result},
    hir::{
        hir_nodes::{
            HirBlock, HirConstValue, HirExpression, HirFunction, HirFunctionCall, HirObject,
            HirPropertyDeclaration, HirStatement, HirStruct, HirVariableDeclaration,
        },
        HirVisitor,
    },
    llir::utils::ItemId,
    objects::{ModuleFactory, ObjInt, ObjStaticInt, ObjString},
    CompileContext, Namespace,
};

use super::{
    Mir, MirContext, MirContextInfo, MirNamespaceEntry, MirNode, MirValue, NamespaceArena,
};

/// Visits the hir and creates a mir from it
#[derive(Debug)]
pub struct MirBuilder<'a, 'code> {
    mir: &'a mut Mir<'code>,
    compile_context: Rc<CompileContext>,
    code: CodeRef<'code>,
    current_context: usize,
}

impl HirVisitor for MirBuilder<'_, '_> {
    type Output = Result<MirValue>;

    fn visit_object(&mut self, _object: &HirObject) -> Self::Output {
        unimplemented!("Hir level objects are not yet implemented!")
    }

    fn visit_struct(&mut self, _struct_: &HirStruct) -> Self::Output {
        unimplemented!("Hir level structs are not yet implemented!")
    }

    fn visit_function(&mut self, function: &HirFunction) -> Self::Output {
        self.add_context();

        for statement in &function.block.statements {
            self.visit_statement(statement)?;
        }

        self.pop_context();
        Ok(MirValue::null(&self.compile_context))
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

        for statement in &block.statements {
            self.visit_statement(statement)?;
        }

        let context = self.pop_context();
        let context_id = context.id;
        self.call_context(context_id, block.span);

        Ok(MirValue::null(&self.compile_context))
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &HirVariableDeclaration,
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
            HirExpression::Path(path) => self.context_info().resolve_path(path),
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
            HirExpression::Execute(expression) => {
                let execute_string = self.visit_expression(expression)?;

                let return_id = ItemId {
                    context_id: self.current_context as u64,
                    id: self.namespace_mut().next_id(),
                };

                let return_value =
                    MirValue::Concrete(ObjInt::new(return_id).into_object(&self.compile_context));
                self.push(MirNode::RawCommand {
                    value: execute_string,
                    var_id: return_id,
                });

                Ok(return_value)
            }
        }
    }

    fn visit_function_call(&mut self, function_call: &HirFunctionCall) -> Self::Output {
        let function_value = self.context_info().resolve_path(&function_call.accessor)?;
        let function_object = match function_value {
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
            .collect::<Result<_>>()?;

        let (return_value, function_node) = self.context_info().register_function_call(
            function_object,
            parameters,
            function_call.span,
        )?;

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

impl<'a, 'code> MirBuilder<'a, 'code> {
    pub fn new(
        mir: &'a mut Mir<'code>,
        modules: &[ModuleFactory],
        compile_context: Rc<CompileContext>,
        code: CodeRef<'code>,
    ) -> Self {
        // The global context which contains the imported modules
        let mut global_context =
            MirContext::new(&mut mir.namespaces, None, 0, compile_context.clone(), code);

        for module_factory in modules {
            let module = module_factory.call(&compile_context);
            global_context.register(&mut mir.namespaces, module);
        }
        mir.contexts.push(global_context);

        MirBuilder {
            mir,
            compile_context,
            code,
            current_context: 0,
        }
    }

    /// Creates a new context and pushes it to the top
    fn add_context(&mut self) {
        let ancestor = Some(self.context().namespace_idx);

        self.current_context += 1;

        let context: MirContext<'code> = MirContext::new(
            &mut self.mir.namespaces,
            ancestor,
            self.current_context as u64,
            self.compile_context.clone(),
            self.code,
        );
        self.mir.add_context(context);
    }

    fn pop_context(&mut self) -> &mut MirContext<'code> {
        let prev_context = self.current_context;
        if self.current_context > 0 {
            self.current_context -= 1;
        }

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
        let node = MirNode::GotoContext { context_id, span };
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
