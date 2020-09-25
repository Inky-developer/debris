use debris_type::Type;
use std::fmt::Debug;
use std::{any::Any, rc::Rc};

use crate::{error, llir::llir_nodes::Node};
use crate::{objects::ObjectTemplate, CompileContext, DebrisObject};
use crate::{ObjectPayload, ObjectProperties, ObjectRef};

type CallbackFunction =
    fn(Rc<CompileContext>, Vec<ObjectRef>) -> error::Result<(ObjectRef, Vec<Node>)>;

#[derive(Eq, PartialEq)]
pub struct ObjectFunction {
    pub parameters: Vec<Type>,
    pub return_type: Type,
    pub function: CallbackFunction,
}

pub fn function_template() -> ObjectTemplate {
    ObjectTemplate::new(Type::Function, ObjectProperties::default())
}

impl ObjectFunction {
    pub fn new(parameters: Vec<Type>, return_type: Type, function: CallbackFunction) -> Self {
        ObjectFunction {
            parameters,
            return_type,
            function,
        }
    }
}

impl ObjectPayload for ObjectFunction {
    fn as_any(&self) -> &dyn Any {
        self as &dyn Any
    }

    fn typ(&self) -> Type {
        Type::Function
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new(ctx.type_ctx.template_for_type(&self.typ()), self)
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
    }
}

impl Debug for ObjectFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "fn({:?}) -> {:?}",
            self.parameters, self.return_type
        ))
    }
}
