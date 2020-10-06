use std::rc::Rc;

use debris_common::Span;

use super::{
    llir_nodes::{Execute, FastStoreFromResult, Function, Node},
    utils::ItemId,
    utils::Scoreboard,
    LLIRContext,
};
use crate::ObjectRef;
use crate::{
    error::Result,
    mir::{Mir, MirContext, MirNode, MirValue},
    objects::ObjectString,
};
use crate::{objects::ObjectFunction, Config};

#[derive(Debug, Eq, PartialEq)]
pub struct LLIR {
    pub functions: Vec<Function>,
    pub config: Rc<Config>,
}

impl LLIR {
    pub fn from_mir(mir: &Mir, config: Rc<Config>) -> Result<LLIR> {
        let functions: Result<_> = mir
            .contexts
            .iter()
            .map(|context| parse_context(context))
            .collect();

        Ok(LLIR {
            functions: functions?,
            config,
        })
    }
}

fn parse_context(context: &MirContext) -> Result<Function> {
    let mut llir_context = LLIRContext {
        code: context.code.clone(),
        mir_nodes: &context.nodes,
        objects: context.values.clone(),
        compile_context: context.compile_context.clone(),
        context_id: context.id,
    };

    let nodes = llir_context
        .mir_nodes
        .iter()
        .map(|mir_node| parse_node(&mut llir_context, mir_node))
        .collect::<Result<Vec<_>>>();

    let nodes = nodes?.into_iter().flatten().collect();

    Ok(Function {
        id: context.id,
        nodes,
    })
}

fn parse_node(context: &mut LLIRContext, node: &MirNode) -> Result<Vec<Node>> {
    match node {
        MirNode::Call {
            span,
            value,
            parameters,
            return_value,
        } => parse_call(
            context,
            &context.as_span(span.clone()),
            value,
            parameters,
            return_value,
        ),
        MirNode::RawCommand { value, var_id } => Ok({
            let object = context.get_object(value).unwrap();
            let value = object
                .downcast_payload::<ObjectString>()
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
    context: &mut LLIRContext,
    span: &Span,
    value: &ObjectRef,
    parameters: &Vec<MirValue>,
    return_value: &MirValue,
) -> Result<Vec<Node>> {
    let parameters = parameters
        .iter()
        .map(|value| context.get_object(value).unwrap())
        .collect();

    let function = value.downcast_payload::<ObjectFunction>().unwrap();
    let return_id = match return_value {
        MirValue::Concrete(_) => panic!("Expected a template"),
        MirValue::Template { id, template: _ } => *id,
    };
    let (result, nodes) = function.call(
        &context.compile_context,
        span,
        parameters,
        ItemId {
            context_id: context.context_id,
            id: return_id,
        },
    )?;

    let result_id = *match return_value {
        MirValue::Concrete(_) => panic!("Expected a template, got a concrete value"),
        MirValue::Template {
            id,
            template: _template,
        } => id,
    };

    context.set_object(result, result_id as usize);
    Ok(nodes)
}
