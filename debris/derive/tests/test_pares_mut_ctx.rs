use debris_core::{
    objects::FunctionContext, CompileContext, DebrisObject, ObjectPayload, ObjectRef,
};
use debris_derive::object;

#[derive(Debug)]
struct Something;

#[object(debris_core::Type::StaticInt)]
impl Something {
    #[special]
    fn add(_ctx: &mut FunctionContext) -> Something {
        Something
    }
}

impl ObjectPayload for Something {
    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new_ref(Self::class(ctx), self)
    }

    fn eq(&self, _: &ObjectRef) -> bool {
        false
    }
}
