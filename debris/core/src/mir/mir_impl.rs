use std::rc::Rc;

use debris_common::{Code, Ident, LocalSpan};

use super::{mir_context::NamespaceArena, MirContext, MirContextInfo, MirInfo, MirNode, MirValue};

use crate::{
    error::{LangError, LangErrorKind, Result},
    hir::{
        hir_nodes::{
            HirConstValue, HirExpression, HirFunction, HirFunctionCall, HirInfix, HirStatement,
        },
        Hir,
    },
    llir::utils::ItemId,
    objects::{ModuleFactory, ObjInt, ObjStaticInt, ObjString},
    CompileContext, ValidPayload,
};

/// A Mid-level intermediate representation
#[derive(Debug, Default, Eq, PartialEq)]
pub struct Mir {
    /// All contexts
    ///
    /// A context can be for example a function body
    pub contexts: Vec<MirContext>,
    pub namespaces: NamespaceArena,
}

impl Mir {
    pub fn context(&mut self, index: usize) -> MirContextInfo {
        MirContextInfo {
            context: &mut self.contexts[index],
            arena: &mut self.namespaces,
        }
    }

    pub fn add_context(&mut self, context: MirContext) {
        self.contexts.push(context)
    }

    /// Converts the hir into a mir
    ///
    /// extern_modules: A slice of [ModuleFactory], which when called return a module object
    pub fn from_hir(
        hir: &Hir,
        compile_context: Rc<CompileContext>,
        extern_modules: &[ModuleFactory],
    ) -> Result<Mir> {
        let mut mir = Mir::default();

        handle_function(
            &mut mir,
            compile_context,
            0,
            &hir.main_function,
            hir.code.clone(),
            extern_modules,
        )?;

        Ok(mir)
    }
}

fn handle_function(
    mir: &mut Mir,
    compile_context: Rc<CompileContext>,
    ctx_id: u64,
    function: &HirFunction,
    code: Rc<Code>,
    global_imports: &[ModuleFactory],
) -> Result<()> {
    let context = MirContext::new(&mut mir.namespaces, ctx_id, compile_context, code);
    mir.add_context(context);

    let mut mir_info = MirInfo {
        mir,
        current_context: ctx_id,
    };

    // Load the extern modules
    for module_factory in global_imports {
        let module = module_factory.call(&mir_info.context().compile_context);
        mir_info.context_info().register(module);
    }

    // And then evaluate the hir tree
    for statement in &function.statements {
        let mut nodes = handle_statement(&mut mir_info, statement)?;
        mir_info.context_mut().nodes.append(&mut nodes);
    }

    Ok(())
}

fn handle_statement(ctx: &mut MirInfo, statement: &HirStatement) -> Result<Vec<MirNode>> {
    Ok(match statement {
        HirStatement::VariableDecl { span, ident, value } => {
            handle_variable_decl(ctx, ctx.context().get_ident(ident), value, span)?
        }
        HirStatement::FunctionCall(call) => {
            // The return value is discarded!
            let (nodes, _value) = handle_function_call(ctx, call)?;
            nodes
        }
    })
}

fn handle_variable_decl(
    ctx: &mut MirInfo,
    identifier: Ident,
    value: &HirExpression,
    span: &LocalSpan,
) -> Result<Vec<MirNode>> {
    let (nodes, value) = handle_expression(ctx, value)?;

    ctx.context_info()
        .add_value(&identifier, value, span.clone())?;

    Ok(nodes)
}

fn handle_expression(
    ctx: &mut MirInfo,
    expression: &HirExpression,
) -> Result<(Vec<MirNode>, MirValue)> {
    let mut nodes = Vec::new();

    let value = match expression {
        HirExpression::Value(constant) => handle_constant(ctx, constant)?,
        HirExpression::Variable(val) => ctx.context_info().get_from_spanned_ident(&val)?.clone(),
        HirExpression::Path(path) => ctx.context().resolve_path(ctx.arena(), path)?,
        HirExpression::BinaryOperation {
            lhs,
            operation,
            rhs,
        } => {
            let (mut new_nodes, value) = handle_binary_operation(ctx, lhs, rhs, operation)?;
            nodes.append(&mut new_nodes);
            value
        }
        HirExpression::FunctionCall(call) => {
            let (mut new_nodes, value) = handle_function_call(ctx, call)?;
            nodes.append(&mut new_nodes);
            value
        }
        HirExpression::UnaryOperation {
            value: _value,
            operation: _operation,
        } => todo!(),
        HirExpression::Execute(execute) => {
            let (mut new_nodes, value) = handle_expression(ctx, execute)?;
            nodes.append(&mut new_nodes);

            // The id for the dynamic next int
            let return_id = ItemId {
                context_id: ctx.context_mut().id,
                id: ctx.namespace_mut().next_id(),
            };

            let return_value =
                ObjInt::new(return_id).into_object(&ctx.context_mut().compile_context);

            nodes.push(MirNode::RawCommand {
                value,
                var_id: return_id,
            });
            MirValue::Concrete(return_value)
        }
    };

    Ok((nodes, value))
}

/// Operation between two expressions
///
/// Works similar like [handle_function_call], since a binary operation *is* a llir function call.
/// Does not check whether the parameters are valid.
fn handle_binary_operation(
    ctx: &mut MirInfo,
    lhs: &HirExpression,
    rhs: &HirExpression,
    op: &HirInfix,
) -> Result<(Vec<MirNode>, MirValue)> {
    let (mut lhs_nodes, lhs_value) = handle_expression(ctx, lhs)?;
    let (mut rhs_nodes, rhs_value) = handle_expression(ctx, rhs)?;
    lhs_nodes.append(&mut rhs_nodes);

    // get the function that can perform the operation
    let object = lhs_value
        .get_property(&op.operator.get_special_ident().into())
        .ok_or_else(|| {
            LangError::new(
                LangErrorKind::UnexpectedOperator {
                    operator: op.operator.get_special_ident(),
                    lhs: lhs_value.class().typ(),
                    rhs: rhs_value.class().typ(),
                },
                ctx.context_mut().as_span(op.span.clone()),
            )
        })?;

    let (return_value, function_node) = ctx.context_info().register_function_call(
        object,
        vec![lhs_value, rhs_value],
        op.span.clone(),
    )?;
    lhs_nodes.push(function_node);
    Ok((lhs_nodes, return_value))
}

/// handles the parameters and emits a function node.
/// Does not check, whether the parameters are actually valid for this function.
fn handle_function_call(
    ctx: &mut MirInfo,
    call: &HirFunctionCall,
) -> Result<(Vec<MirNode>, MirValue)> {
    let mut nodes = Vec::new();
    // Resolve the function object
    let value = ctx.context().resolve_path(ctx.arena(), &call.accessor)?;
    let object = match value {
        MirValue::Template { id: _, class: _ } => {
            return Err(LangError::new(
                LangErrorKind::NotYetImplemented {
                    msg: "Higher order functions".to_string(),
                },
                ctx.context_mut().as_span(call.span.clone()),
            )
            .into())
        }
        MirValue::Concrete(function_obj) => function_obj,
    };

    let mut parameters = Vec::with_capacity(call.parameters.len());
    for parameter in &call.parameters {
        let (mut new_nodes, value) = handle_expression(ctx, parameter)?;
        nodes.append(&mut new_nodes);
        parameters.push(value);
    }

    let (return_value, function_node) =
        ctx.context_info()
            .register_function_call(object, parameters, call.span.clone())?;

    nodes.push(function_node);

    Ok((nodes, return_value))
}

fn handle_constant(ctx: &MirInfo, constant: &HirConstValue) -> Result<MirValue> {
    Ok(match constant {
        HirConstValue::Integer { span: _, value } => ObjStaticInt::new(*value)
            .into_object(&ctx.context().compile_context)
            .into(),
        HirConstValue::Fixed { span: _, value: _ } => todo!(),
        HirConstValue::String { span: _, value } => ObjString::from(value.clone())
            .into_object(&ctx.context().compile_context)
            .into(),
    })
}
