use debris_derive::object;

use crate::{
    llir::{
        llir_nodes::{FastStore, Node},
        utils::{Scoreboard, ScoreboardValue},
    },
    memory::MemoryLayout,
    CompileContext, ObjectPayload, Type,
};

use super::{FunctionContext, ObjBool};

#[derive(Debug, Eq, PartialEq)]
pub struct ObjStaticBool {
    pub value: bool,
}

#[object(Type::StaticBool)]
impl ObjStaticBool {
    #[special]
    fn promote_runtime(ctx: &mut FunctionContext, this: &ObjStaticBool) -> ObjBool {
        ctx.emit(Node::FastStore(FastStore {
            id: ctx.item_id,
            scoreboard: Scoreboard::Main,
            value: ScoreboardValue::Static(this.value as i32),
        }));
        ObjBool::new(ctx.item_id)
    }
}

impl ObjectPayload for ObjStaticBool {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Zero
    }
}

impl From<bool> for ObjStaticBool {
    fn from(value: bool) -> Self {
        ObjStaticBool { value }
    }
}
