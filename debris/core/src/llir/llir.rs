use std::rc::Rc;

use super::{
    llir_nodes::{FastStore, Function, Node},
    utils::ItemId,
    utils::Scoreboard,
    utils::ScoreboardValue,
};
use crate::{
    error::Result,
    mir::{ItemIdentifier, Mir, MirContext, MirNode, MirValue},
    CompileContext,
};
use crate::{objects::ObjectFunction, Config};
use crate::{objects::ObjectInteger, ObjectRef};

struct LLIRContext<'a> {
    mir_nodes: &'a Vec<MirNode>,
    objects: Vec<MirValue>,
    id: u64,
    compile_context: Rc<CompileContext>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LLIR {
    pub functions: Vec<Function>,
    pub config: Rc<Config>,
}

impl<'a> LLIRContext<'a> {
    pub fn get_object(&self, identifier: &ItemIdentifier) -> Option<&MirValue> {
        if self.id != identifier.context_id {
            None
        } else {
            self.objects.get(identifier.item_id as usize)
        }
    }
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
        mir_nodes: &context.nodes,
        objects: context.values.clone(),
        id: context.id,
        compile_context: context.compile_context.clone(),
    };

    let nodes: Result<Vec<Vec<Node>>> = llir_context
        .mir_nodes
        .iter()
        .map(|mir_node| parse_node(&mut llir_context, mir_node))
        .collect();

    let nodes = nodes?.into_iter().flatten().collect();

    Ok(Function {
        id: context.id,
        nodes: nodes,
    })
}

fn parse_node(context: &mut LLIRContext, node: &MirNode) -> Result<Vec<Node>> {
    match node {
        MirNode::Call {
            span: _,
            value,
            parameters,
            return_value,
        } => parse_call(context, value, parameters, return_value),
        MirNode::Define { span: _, item } => {
            let value = context.get_object(item).expect("invalid item").clone();
            parse_define(context, item, value)
        }
    }
}

fn parse_define(
    context: &mut LLIRContext,
    item: &ItemIdentifier,
    value: MirValue,
) -> Result<Vec<Node>> {
    let object = get_object(context, &value).unwrap();

    let node = Node::FastStore(FastStore {
        scoreboard: Scoreboard::Main,
        id: ItemId {
            id: item.item_id,
            context_id: context.id,
        },
        value: ScoreboardValue::Static(object.downcast_payload::<ObjectInteger>().unwrap().value),
    });

    Ok(vec![node])
}

fn parse_call(
    context: &mut LLIRContext,
    value: &ObjectRef,
    parameters: &Vec<MirValue>,
    return_value: &MirValue,
) -> Result<Vec<Node>> {
    let parameters = parameters
        .iter()
        .map(|value| get_object(context, value).unwrap())
        .collect();

    let function = value.downcast_payload::<ObjectFunction>().unwrap().function;
    let (result, nodes) = (function)(context.compile_context.clone(), parameters)?;

    let result_id = *match return_value {
        MirValue::Concrete(_) => panic!("Expected a template, got a concrete value"),
        MirValue::Template {
            id,
            template: _template,
        } => id,
    };

    set_object(context, result, result_id as usize);
    Ok(nodes)
}

// Various utility functions
fn set_object(context: &mut LLIRContext, value: ObjectRef, index: usize) {
    if context.objects.len() <= index {
        panic!(
            "Could not replace object at index {}, vec has only {} element(s)",
            index,
            context.objects.len()
        );
    }

    context.objects[index] = MirValue::Concrete(value);
}

fn get_object(context: &LLIRContext, value: &MirValue) -> Option<ObjectRef> {
    match value {
        MirValue::Concrete(obj) => Some(obj.clone()),
        MirValue::Template {
            id,
            template: _template,
        } => match context.objects.get(*id as usize) {
            Some(MirValue::Concrete(obj)) => Some(obj.clone()),
            Some(MirValue::Template { id: _, template: _ }) | None => None,
        },
    }
}
