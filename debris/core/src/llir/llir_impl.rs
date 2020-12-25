use std::rc::Rc;

use debris_common::Span;

use super::{
    llir_nodes::{Call, Execute, FastStoreFromResult, Function, Node},
    utils::ItemId,
    utils::Scoreboard,
    LLIRContext,
};
use crate::{
    error::Result,
    mir::NamespaceArena,
    mir::{MirContext, MirNode, MirValue},
    objects::ObjFunction,
    objects::ObjString,
    Config, ObjectRef,
};

struct LLIRInfo<'a, 'b, 'code> {
    context: LLIRContext<'a, 'code>,
    arena: &'b mut NamespaceArena,
}

/// The low-level intermediate representation struct
///
/// Contains all generated functions and a compilation configuration
#[derive(Debug, Eq, PartialEq)]
pub struct Llir {
    /// The functions which were created
    pub functions: Vec<Function>,
    /// The compilation config
    pub config: Rc<Config>,
}

impl Llir {
    /// Compiles the mir into a llir
    pub fn from_mir(
        contexts: &[MirContext],
        namespaces: &mut NamespaceArena,
        config: Rc<Config>,
    ) -> Result<Llir> {
        let functions: Result<_> = contexts
            .iter()
            .skip(1) // The first context does not contain anything useful for the code generation
            .map(|context| parse_context(context, namespaces))
            .collect();

        Ok(Llir {
            functions: functions?,
            config,
        })
    }
}

fn parse_context(context: &MirContext, arena: &mut NamespaceArena) -> Result<Function> {
    let llir_context = LLIRContext {
        code: context.code.clone(),
        mir_nodes: &context.nodes,
        namespace_idx: context.namespace_idx,
        compile_context: context.compile_context.clone(),
        context_id: context.id,
    };

    let mut llir_info = LLIRInfo {
        context: llir_context,
        arena,
    };

    let nodes = llir_info
        .context
        .mir_nodes
        .iter()
        .map(|mir_node| parse_node(&mut llir_info, mir_node))
        .collect::<Result<Vec<_>>>();

    let nodes = nodes?.into_iter().flatten().collect();

    Ok(Function {
        id: context.id,
        nodes,
    })
}

fn parse_node(ctx: &mut LLIRInfo, node: &MirNode) -> Result<Vec<Node>> {
    match node {
        MirNode::Call {
            span,
            value,
            parameters,
            return_value,
        } => parse_call(ctx, span, value, parameters, return_value),
        MirNode::GotoContext { span, context_id } => parse_goto_context(ctx, *span, *context_id),
        MirNode::RawCommand { value, var_id } => Ok({
            let object = ctx.context.get_object(ctx.arena, value).unwrap();
            let value = object
                .downcast_payload::<ObjString>()
                .expect("Expected a string for execute");
            let string: &str = &value;

            vec![Node::FastStoreFromResult(FastStoreFromResult {
                command: Box::new(Node::Execute(Execute {
                    command: string.to_owned(),
                })),
                scoreboard: Scoreboard::Main,
                id: *var_id,
            })]
        }),
    }
}

fn parse_call(
    ctx: &mut LLIRInfo,
    span: &Span,
    value: &ObjectRef,
    parameters: &[MirValue],
    return_value: &MirValue,
) -> Result<Vec<Node>> {
    let parameters = parameters
        .iter()
        .map(|value| {
            ctx.context
                .get_object(ctx.arena, value)
                .expect("All parameters are evaluated before the call")
        })
        .collect::<Vec<_>>();

    let parameter_types = parameters.iter().map(|param| param.class.as_ref());

    let function_object = value.downcast_payload::<ObjFunction>().unwrap();
    // Parameters get validated in the mir
    let callback = function_object
        .signature(parameter_types)
        .expect("There must be an overload")
        .function();

    // Get the unique id of the value that should be returned
    let return_id = match return_value {
        MirValue::Concrete(_) => panic!("Expected a template"),
        MirValue::Template { id, class: _ } => *id,
    };

    let (result, nodes) = callback.call(
        &ctx.context.compile_context,
        span,
        &parameters,
        ItemId {
            context_id: ctx.context.context_id,
            id: return_id,
        },
    )?;

    // Replace the templated value with the computed actual value
    ctx.context.set_object(ctx.arena, result, return_id);

    Ok(nodes)
}

fn parse_goto_context(_ctx: &mut LLIRInfo, _span: Span, id: u64) -> Result<Vec<Node>> {
    Ok(vec![Node::Call(Call { id })])
}
