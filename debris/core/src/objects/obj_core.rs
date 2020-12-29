use crate::{
    llir::llir_nodes::{Execute, FastStoreFromResult, Node},
    ObjectPayload, ObjectProperties, ObjectRef,
};

use debris_common::Ident;
use debris_derive::object;

use crate::Type;

use super::{FunctionContext, ObjInt, ObjString};

/// Contains core functionality and often used functions
#[derive(Debug, PartialEq, Eq)]
pub struct ObjCore {
    properties: ObjectProperties,
}

#[object(Type::Module)]
impl ObjCore {
    /// Takes a String and directly inserts it into the generated code
    #[method]
    fn execute(ctx: &mut FunctionContext, string: &ObjString) -> ObjInt {
        let string_value = string.as_str();
        let return_value = ctx.item_id;

        let execute_command = Node::Execute(Execute {
            command: string_value.to_string(),
        });
        ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
            command: execute_command.into(),
            id: return_value,
            scoreboard: crate::llir::utils::Scoreboard::Main,
        }));

        return_value.into()
    }
}

impl ObjectPayload for ObjCore {
    fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.properties.get(ident).cloned()
    }
}
