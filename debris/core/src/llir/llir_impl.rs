use std::rc::Rc;

use debris_common::Span;

use super::{
    llir_nodes::{Execute, FastStoreFromResult, Function, Node},
    utils::ItemId,
    utils::Scoreboard,
    LLIRContext,
};
use crate::{
    error::Result,
    mir::{Mir, MirContext, MirNode, MirValue},
    objects::ObjString,
};
use crate::{mir::NamespaceArena, ObjectRef};
use crate::{objects::ObjFunction, Config};

struct LLIRInfo<'a, 'b> {
    context: LLIRContext<'a>,
    arena: &'b mut NamespaceArena,
}

/// The low-level intermediate representation struct
///
/// Contains all generated functions and a compilation configuration
#[derive(Debug, Eq, PartialEq)]
pub struct LLIR {
    /// The functions which were created
    pub functions: Vec<Function>,
    /// The compilation config
    pub config: Rc<Config>,
}

impl LLIR {
    /// Compiles the mir into a llir
    pub fn from_mir(mir: &Mir, config: Rc<Config>) -> Result<LLIR> {
        // Copy the namespace
        // This operation should not be too expensive, because the arena contains mostly rc's
        // but it isn't nice anyways
        let mut namespaces = mir.namespaces.clone();

        let functions: Result<_> = mir
            .contexts
            .iter()
            .map(|context| parse_context(context, &mut namespaces))
            .collect();

        Ok(LLIR {
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
        } => parse_call(
            ctx,
            &ctx.context.as_span(span.clone()),
            value,
            parameters,
            return_value,
        ),
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
