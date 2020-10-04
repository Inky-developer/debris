use std::ops::Deref;
use std::rc::Rc;

use super::{
    llir_nodes::{Execute, FastStore, Function, Node},
    utils::ItemId,
    utils::Scoreboard,
    utils::ScoreboardValue,
};
use crate::{
    error::Result,
    mir::{ItemIdentifier, Mir, MirContext, MirNode, MirValue},
    objects::ObjectString,
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
    pub fn get_object_by_id(&self, identifier: &ItemIdentifier) -> Option<&MirValue> {
        if self.id != identifier.context_id {
            None
        } else {
            self.objects.get(identifier.item_id as usize)
        }
    }

    fn get_object(&self, value: &MirValue) -> Option<ObjectRef> {
        match value {
            MirValue::Concrete(obj) => Some(obj.clone()),
            MirValue::Template {
                id,
                template: _template,
            } => match self.objects.get(*id as usize) {
                Some(MirValue::Concrete(obj)) => Some(obj.clone()),
                Some(MirValue::Template { id: _, template: _ }) | None => None,
            },
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
            let value = context
                .get_object_by_id(item)
                .expect("invalid item")
                .clone();
            parse_define(context, item, value)
        }
        MirNode::RawCommand(value) => Ok({
            let object = context.get_object(value).unwrap();
            let value = object
                .downcast_payload::<ObjectString>()
                .expect("Expected a string for execute");
            vec![Node::Execute(Execute {
                command: value.deref().to_owned(),
            })]
        }),
    }
}

fn parse_define(
    context: &mut LLIRContext,
    item: &ItemIdentifier,
    value: MirValue,
) -> Result<Vec<Node>> {
    let object = context.get_object(&value).unwrap();

    if object.is_instance::<ObjectInteger>() {
        let node = Node::FastStore(FastStore {
            scoreboard: Scoreboard::Main,
            id: ItemId {
                id: item.item_id,
                context_id: context.id,
            },
            value: ScoreboardValue::Static(
                object.downcast_payload::<ObjectInteger>().unwrap().value,
            ),
        });
        Ok(vec![node])
    } else {
        Ok(vec![])
    }
}

fn parse_call(
    context: &mut LLIRContext,
    value: &ObjectRef,
    parameters: &Vec<MirValue>,
    return_value: &MirValue,
) -> Result<Vec<Node>> {
    let parameters = parameters
        .iter()
        .map(|value| context.get_object(value).unwrap())
        .collect();

    let function = value.downcast_payload::<ObjectFunction>().unwrap();
    let (result, nodes) = function.call(&context.compile_context, parameters)?;

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
