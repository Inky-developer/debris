use super::{mir_context::NamespaceArena, MirBuilder, MirContext, MirContextInfo};

use crate::{
    error::Result,
    hir::{Hir, HirVisitor},
    objects::ModuleFactory,
    CompileContext,
};

/// A Mid-level intermediate representation
#[derive(Debug, Default)]
pub struct Mir<'ctx> {
    /// All contexts
    ///
    /// A context can be for example a function body
    pub contexts: Vec<MirContext<'ctx>>,
    pub namespaces: NamespaceArena,
}

impl<'ctx> Mir<'ctx> {
    pub fn context<'b>(&'b mut self, index: usize) -> MirContextInfo<'b, 'ctx> {
        MirContextInfo {
            context: &mut self.contexts[index],
            arena: &mut self.namespaces,
        }
    }

    pub fn add_context(&mut self, context: MirContext<'ctx>) {
        self.contexts.push(context)
    }

    /// Converts the hir into a mir
    ///
    /// extern_modules: A slice of [ModuleFactory], which when called return a module object
    pub fn from_hir(
        hir: &Hir<'ctx>,
        compile_context: &'ctx CompileContext,
        extern_modules: &[ModuleFactory],
    ) -> Result<Mir<'ctx>> {
        let mut mir = Mir::default();

        let mut builder = MirBuilder::new(&mut mir, extern_modules, compile_context, hir.code_ref);
        let main_function = &hir.main_function;
        builder.visit_function(main_function)?;

        Ok(mir)
    }
}

// fn handle_function(
//     mir: &mut Mir,
//     compile_context: Rc<CompileContext>,
//     ctx_id: u64,
//     function: &HirFunction,
//     code: Rc<Code>,
//     global_imports: &[ModuleFactory],
// ) -> Result<()> {
//     let context = MirContext::new(&mut mir.namespaces, ctx_id, compile_context, code);
//     mir.add_context(context);

//     let mut mir_info = MirInfo {
//         mir,
//         current_context: ctx_id,
//     };

//     // Load the extern modules
//     for module_factory in global_imports {
//         let module = module_factory.call(&mir_info.context().compile_context);
//         mir_info.context_info().register(module);
//     }

//     // And then evaluate the hir tree
//     for statement in &function.block.statements {
//         let mut nodes = handle_statement(&mut mir_info, statement)?;
//         mir_info.context_mut().nodes.append(&mut nodes);
//     }

//     Ok(())
// }

// fn handle_block(ctx: &mut MirInfo, block: &HirBlock) -> Result<(MirNode, MirValue)> {
//     let context_id = ctx.mir.contexts.len() as u64;
//     {
//         let compile_context = ctx.context().compile_context.clone();
//         let code = ctx.context().code.clone();
//         let ancestor = ctx.context().namespace_idx;
//         let context =
//             MirContext::with_ancestor(ctx.arena_mut(), ancestor, context_id, compile_context, code);
//         ctx.mir.add_context(context);
//     }

//     let mut mir_info = MirInfo {
//         current_context: context_id,
//         mir: ctx.mir,
//     };

//     for statement in &block.statements {
//         let mut nodes = handle_statement(&mut mir_info, statement)?;
//         mir_info.context_mut().nodes.append(&mut nodes);
//     }

//     // Just return 0 for now. Should return the last used expression
//     Ok((
//         MirNode::GotoContext {
//             span: block.span.clone(),
//             context_id: mir_info.current_context,
//         },
//         MirValue::Concrete(ObjStaticInt::from(0).into_object(&mir_info.context().compile_context)),
//     ))
// }

// fn handle_statement(ctx: &mut MirInfo, statement: &HirStatement) -> Result<Vec<MirNode>> {
//     Ok(match statement {
//         HirStatement::VariableDecl(HirVariableDeclaration { span, ident, value }) => {
//             handle_variable_decl(ctx, ctx.context().get_ident(ident), value, span)?
//         }
//         HirStatement::FunctionCall(call) => {
//             // The return value is discarded!
//             let (nodes, _value) = handle_function_call(ctx, call)?;
//             nodes
//         }
//         HirStatement::Block(block) => {
//             let (node, _value) = handle_block(ctx, block)?;
//             vec![node]
//         }
//     })
// }

// fn handle_variable_decl(
//     ctx: &mut MirInfo,
//     identifier: Ident,
//     value: &HirExpression,
//     span: &LocalSpan,
// ) -> Result<Vec<MirNode>> {
//     let (nodes, value) = handle_expression(ctx, value)?;

//     ctx.context_info()
//         .add_value(&identifier, value, span.clone())?;

//     Ok(nodes)
// }

// fn handle_expression(
//     ctx: &mut MirInfo,
//     expression: &HirExpression,
// ) -> Result<(Vec<MirNode>, MirValue)> {
//     let mut nodes = Vec::new();

//     let value = match expression {
//         HirExpression::Value(constant) => handle_constant(ctx, constant)?,
//         HirExpression::Variable(val) => ctx.context_info().get_from_spanned_ident(&val)?.clone(),
//         HirExpression::Path(path) => ctx.context().resolve_path(ctx.arena(), path)?,
//         HirExpression::BinaryOperation {
//             lhs,
//             operation,
//             rhs,
//         } => {
//             let (mut new_nodes, value) = handle_binary_operation(ctx, lhs, rhs, operation)?;
//             nodes.append(&mut new_nodes);
//             value
//         }
//         HirExpression::FunctionCall(call) => {
//             let (mut new_nodes, value) = handle_function_call(ctx, call)?;
//             nodes.append(&mut new_nodes);
//             value
//         }
//         HirExpression::Block(block) => {
//             let (node, value) = handle_block(ctx, block)?;
//             nodes.push(node);
//             value
//         }
//         HirExpression::UnaryOperation {
//             value: _value,
//             operation: _operation,
//         } => todo!(),
//         HirExpression::Execute(execute) => {
//             let (mut new_nodes, value) = handle_expression(ctx, execute)?;
//             nodes.append(&mut new_nodes);

//             // The id for the dynamic next int
//             let return_id = ItemId {
//                 context_id: ctx.context_mut().id,
//                 id: ctx.namespace_mut().next_id(),
//             };

//             let return_value =
//                 ObjInt::new(return_id).into_object(&ctx.context_mut().compile_context);

//             nodes.push(MirNode::RawCommand {
//                 value,
//                 var_id: return_id,
//             });
//             MirValue::Concrete(return_value)
//         }
//     };

//     Ok((nodes, value))
// }

// /// Operation between two expressions
// ///
// /// Works similar like [handle_function_call], since a binary operation *is* a llir function call.
// /// Does not check whether the parameters are valid.
// fn handle_binary_operation(
//     ctx: &mut MirInfo,
//     lhs: &HirExpression,
//     rhs: &HirExpression,
//     op: &HirInfix,
// ) -> Result<(Vec<MirNode>, MirValue)> {
//     let (mut lhs_nodes, lhs_value) = handle_expression(ctx, lhs)?;
//     let (mut rhs_nodes, rhs_value) = handle_expression(ctx, rhs)?;
//     lhs_nodes.append(&mut rhs_nodes);

//     // get the function that can perform the operation
//     let object = lhs_value
//         .get_property(&op.operator.get_special_ident().into())
//         .ok_or_else(|| {
//             LangError::new(
//                 LangErrorKind::UnexpectedOperator {
//                     operator: op.operator.get_special_ident(),
//                     lhs: lhs_value.class().typ(),
//                     rhs: rhs_value.class().typ(),
//                 },
//                 ctx.context_mut().as_span(op.span.clone()),
//             )
//         })?;

//     let (return_value, function_node) = ctx.context_info().register_function_call(
//         object,
//         vec![lhs_value, rhs_value],
//         op.span.clone(),
//     )?;
//     lhs_nodes.push(function_node);
//     Ok((lhs_nodes, return_value))
// }

// /// handles the parameters and emits a function node.
// /// Does not check whether the parameters are actually valid for this function.
// fn handle_function_call(
//     ctx: &mut MirInfo,
//     call: &HirFunctionCall,
// ) -> Result<(Vec<MirNode>, MirValue)> {
//     let mut nodes = Vec::new();
//     // Resolve the function object
//     let value = ctx.context().resolve_path(ctx.arena(), &call.accessor)?;
//     let object = match value {
//         MirValue::Template { id: _, class: _ } => {
//             return Err(LangError::new(
//                 LangErrorKind::NotYetImplemented {
//                     msg: "Higher order functions".to_string(),
//                 },
//                 ctx.context_mut().as_span(call.span.clone()),
//             )
//             .into())
//         }
//         MirValue::Concrete(function_obj) => function_obj,
//     };

//     let mut parameters = Vec::with_capacity(call.parameters.len());
//     for parameter in &call.parameters {
//         let (mut new_nodes, value) = handle_expression(ctx, parameter)?;
//         nodes.append(&mut new_nodes);
//         parameters.push(value);
//     }

//     let (return_value, function_node) =
//         ctx.context_info()
//             .register_function_call(object, parameters, call.span.clone())?;

//     nodes.push(function_node);

//     Ok((nodes, return_value))
// }

// fn handle_constant(ctx: &MirInfo, constant: &HirConstValue) -> Result<MirValue> {
//     Ok(match constant {
//         HirConstValue::Integer { span: _, value } => ObjStaticInt::new(*value)
//             .into_object(&ctx.context().compile_context)
//             .into(),
//         HirConstValue::Fixed { span: _, value: _ } => todo!(),
//         HirConstValue::String { span: _, value } => ObjString::from(value.clone())
//             .into_object(&ctx.context().compile_context)
//             .into(),
//     })
// }
