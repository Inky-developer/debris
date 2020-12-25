use debris_derive::object;

use crate::{CompileContext, ObjectPayload, ObjectRef, Type};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ObjNull;

#[object(Type::Null)]
impl ObjNull {
    pub fn instance(ctx: &CompileContext) -> ObjectRef {
        ctx.type_ctx.null(ctx)
    }
}

impl ObjectPayload for ObjNull {}
