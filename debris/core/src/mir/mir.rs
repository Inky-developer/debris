use debris_common::{Code, Ident, LocalSpan};
use debris_type::Type;
use std::rc::Rc;

use super::{ItemIdentifier, MirContext, MirNode, MirValue};

use crate::error::{LangError, LangErrorKind, Result};
use crate::hir::hir_nodes::{
    HirConstValue, HirExpression, HirFunction, HirFunctionCall, HirInfix, HirStatement,
};
use crate::hir::Hir;
use crate::objects::{ObjectFunction, ObjectInteger};
use crate::CompileContext;
use crate::ObjectPayload;

#[derive(Debug, Eq, PartialEq)]
pub struct Mir {
    pub contexts: Vec<MirContext>,
}

impl Mir {
    pub fn from_hir(hir: &Hir, compile_context: Rc<CompileContext>) -> Result<Mir> {
        let mut contexts = Vec::new();
        contexts.push(handle_function(
            compile_context,
            0,
            &hir.main_function,
            hir.code.clone(),
        )?);
        Ok(Mir { contexts })
    }
}

fn handle_function(
    compile_context: Rc<CompileContext>,
    ctx_id: u64,
    function: &HirFunction,
    code: Rc<Code>,
) -> Result<MirContext> {
    let mut ctx = MirContext::new(ctx_id, compile_context, code);

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
    let (mut nodes, value) = handle_expression(ctx, value)?;
    ctx.add_value(&identifier, value, span.clone())?;

    // Ad the definition. The identifier should already map to some id
    nodes.push(MirNode::Define {
        span: span.clone(),
        item: ItemIdentifier::new(
            ctx.id,
            ctx.get_id(&identifier)
                .expect(&format!("Unknown identifier {:?}", identifier)),
        ),
    });
    Ok(nodes)
}

fn handle_expression(
    ctx: &mut MirContext,
    expression: &HirExpression,
) -> Result<(Vec<MirNode>, MirValue)> {
    let mut nodes = Vec::new();

    let value = match expression {
        HirExpression::Number(constant) => handle_constant(ctx, constant)?,
        HirExpression::Variable(val) => ctx.get_from_spanned_ident(&val)?.clone(),
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
                    typ: lhs_value.typ().clone(),
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
    let return_template = ctx
        .compile_context
        .type_ctx
        .template_for_type(&function.return_type);

    let return_value = ctx.add_anonymous_template(return_template).clone();

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
        HirConstValue::Integer { span: _, value } => ObjectInteger::new(*value)
            .into_object(&ctx.compile_context)
            .into(),
        HirConstValue::Fixed { span: _, value: _ } => todo!(),
    })
}
