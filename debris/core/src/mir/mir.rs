use debris_common::{Code, Ident, LocalSpan};
use debris_type::Type;
use std::rc::Rc;

use super::{MirContext, MirNode, MirValue};

use crate::objects::{ModuleFactory, ObjectFunction, ObjectStaticInteger};
use crate::CompileContext;
use crate::ObjectPayload;
use crate::{
    error::{LangError, LangErrorKind, Result},
    objects::ObjectString,
};
use crate::{
    hir::hir_nodes::{
        HirConstValue, HirExpression, HirFunction, HirFunctionCall, HirInfix, HirStatement,
    },
    objects::ObjectDynamicInteger,
};
use crate::{hir::Hir, llir::utils::ItemId};

#[derive(Debug, Eq, PartialEq)]
pub struct Mir {
    pub contexts: Vec<MirContext>,
}

impl Mir {
    pub fn from_hir(
        hir: &Hir,
        compile_context: Rc<CompileContext>,
        extern_modules: &[ModuleFactory],
    ) -> Result<Mir> {
        let mut contexts = Vec::new();
        contexts.push(handle_function(
            compile_context,
            0,
            &hir.main_function,
            hir.code.clone(),
            extern_modules,
        )?);
        Ok(Mir { contexts })
    }
}

fn handle_function(
    compile_context: Rc<CompileContext>,
    ctx_id: u64,
    function: &HirFunction,
    code: Rc<Code>,
    global_imports: &[ModuleFactory],
) -> Result<MirContext> {
    let mut ctx = MirContext::new(ctx_id, compile_context, code);

    // Load the extern modules
    for module_factory in global_imports {
        ctx.register(module_factory.call(&ctx.compile_context));
    }

    for statement in &function.statements {
        let mut nodes = handle_statement(&mut ctx, statement)?;
        ctx.nodes.append(&mut nodes);
    }

    Ok(ctx)
}

fn handle_statement(ctx: &mut MirContext, statement: &HirStatement) -> Result<Vec<MirNode>> {
    Ok(match statement {
        HirStatement::VariableDecl { span, ident, value } => {
            handle_variable_decl(ctx, ctx.get_ident(ident), value, span)?
        }
        HirStatement::FunctionCall(call) => {
            // The return value is discarded!
            let (nodes, _value) = handle_function_call(ctx, call)?;
            nodes
        }
    })
}

fn handle_variable_decl(
    ctx: &mut MirContext,
    identifier: Ident,
    value: &HirExpression,
    span: &LocalSpan,
) -> Result<Vec<MirNode>> {
    let (nodes, value) = handle_expression(ctx, value)?;

    ctx.add_value(&identifier, value, span.clone())?;

    Ok(nodes)
}

fn handle_expression(
    ctx: &mut MirContext,
    expression: &HirExpression,
) -> Result<(Vec<MirNode>, MirValue)> {
    let mut nodes = Vec::new();

    let value = match expression {
        HirExpression::Value(constant) => handle_constant(ctx, constant)?,
        HirExpression::Variable(val) => ctx.get_from_spanned_ident(&val)?.clone(),
        HirExpression::Path(path) => ctx.resolve_path(path)?,
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
                context_id: ctx.id,
                id: ctx.next_id(),
            };

            let return_value =
                ObjectDynamicInteger::new(return_id).into_object(&ctx.compile_context);

            nodes.push(MirNode::RawCommand {
                value,
                var_id: return_id,
            });
            MirValue::Concrete(return_value)
        }
    };

    Ok((nodes, value))
}

/// Operation between two expressions.
/// Only applicable to expressions of the same type
fn handle_binary_operation(
    ctx: &mut MirContext,
    lhs: &HirExpression,
    rhs: &HirExpression,
    op: &HirInfix,
) -> Result<(Vec<MirNode>, MirValue)> {
    let (mut lhs_nodes, lhs_value) = handle_expression(ctx, lhs)?;
    let (mut rhs_nodes, rhs_value) = handle_expression(ctx, rhs)?;
    lhs_nodes.append(&mut rhs_nodes);

    let object = lhs_value
        .get_property(&op.operator.get_special_ident().into())
        .ok_or_else(|| {
            LangError::new(
                LangErrorKind::UnexpectedOperator {
                    operator: op.operator.get_special_ident(),
                    lhs: lhs_value.typ().clone(),
                    rhs: rhs_value.typ().clone(),
                },
                ctx.as_span(op.span.clone()),
            )
        })?;

    // Only functions should be registered for properties of special idents
    let function = object
        .downcast_payload::<ObjectFunction>()
        .expect("Expected a function");
    let return_value = ctx
        .add_anonymous_template(
            ctx.compile_context
                .type_ctx
                .template_for_type(&function.return_type),
        )
        .clone();

    lhs_nodes.push(MirNode::Call {
        span: lhs.span().until(&rhs.span()),
        value: object,
        parameters: vec![lhs_value, rhs_value],
        return_value: return_value.clone(),
    });
    Ok((lhs_nodes, return_value))
}

fn handle_function_call(
    ctx: &mut MirContext,
    call: &HirFunctionCall,
) -> Result<(Vec<MirNode>, MirValue)> {
    let value = ctx.resolve_path(&call.accessor)?;
    let object = match value {
        MirValue::Template { id: _, template: _ } => panic!("Expected a concrete function"),
        MirValue::Concrete(function_obj) => function_obj,
    };
    let function = object.downcast_payload::<ObjectFunction>().ok_or_else(|| {
        LangError::new(
            LangErrorKind::UnexpectedType {
                expected: Type::Function,
                got: object.typ.clone(),
            },
            ctx.as_span(call.accessor.span()),
        )
    })?;
    let return_template = function.return_type.clone();

    let return_value = ctx
        .add_anonymous_template(
            ctx.compile_context
                .type_ctx
                .template_for_type(&return_template),
        )
        .clone();

    let mut mir_nodes = Vec::new();
    let mut parameters = Vec::new();
    for hir_value in &call.parameters {
        let (mut new_nodes, value) = handle_expression(ctx, &hir_value)?;
        mir_nodes.append(&mut new_nodes);
        parameters.push(value);
    }

    mir_nodes.push(MirNode::Call {
        span: call.span.clone(),
        value: object,
        parameters: parameters,
        return_value: return_value.clone(),
    });

    Ok((mir_nodes, return_value))
}

fn handle_constant(ctx: &MirContext, constant: &HirConstValue) -> Result<MirValue> {
    Ok(match constant {
        HirConstValue::Integer { span: _, value } => ObjectStaticInteger::new(*value)
            .into_object(&ctx.compile_context)
            .into(),
        HirConstValue::Fixed { span: _, value: _ } => todo!(),
        HirConstValue::String { span: _, value } => ObjectString::from(value.clone())
            .into_object(&ctx.compile_context)
            .into(),
    })
}
