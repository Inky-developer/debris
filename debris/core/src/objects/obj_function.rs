use debris_type::Type;
use std::fmt::Debug;

use crate::{error::Result, llir::llir_nodes::Node};
use crate::{CompileContext, DebrisObject};
use crate::{ObjectPayload, ObjectProperties, ObjectRef};

use super::{ObjectType, TypeRef};

// type CallbackFunction = fn(FunctionContenxt, Vec<ObjectRef>) -> Result<(ObjectRef, Vec<Node>)>;

type CallbackType = fn(&mut FunctionContext, Vec<ObjectRef>) -> Result<ObjectRef>;
pub struct CallbackFunction(pub CallbackType);

pub struct FunctionContext<'a> {
    pub compile_context: &'a CompileContext,
    pub nodes: Vec<Node>,
}

#[derive(Eq, PartialEq)]
pub struct ObjectFunction {
    pub parameters: Vec<Type>,
    pub return_type: Type,
    pub function: CallbackFunction,
}

impl ObjectFunction {
    pub fn new(parameters: Vec<Type>, return_type: Type, function: CallbackFunction) -> Self {
        ObjectFunction {
            parameters,
            return_type,
            function,
        }
    }

    pub fn call(
        &self,
        ctx: &CompileContext,
        parameters: Vec<ObjectRef>,
    ) -> Result<(ObjectRef, Vec<Node>)> {
        let mut function_ctx = FunctionContext {
            compile_context: ctx,
            nodes: vec![],
        };
        (self.function.0)(&mut function_ctx, parameters).map(|value| (value, function_ctx.nodes))
    }

    pub fn template() -> TypeRef {
        ObjectType::new_ref(
            Type::Template(Box::new(Type::Function)),
            ObjectProperties::default(),
            None,
        )
    }

    pub fn init_template(_: &CompileContext, _: &TypeRef) {}
}

impl ObjectPayload for ObjectFunction {
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
            "Function({:?}) -> {:?}",
            self.parameters, self.return_type
        ))
    }
}

// I don't understand why this can't be derived
impl PartialEq for CallbackFunction {
    fn eq(&self, other: &CallbackFunction) -> bool {
        self.0 as usize == other.0 as usize
    }
}

impl Eq for CallbackFunction {}

impl From<CallbackType> for CallbackFunction {
    fn from(val: CallbackType) -> Self {
        CallbackFunction(val)
    }
}
