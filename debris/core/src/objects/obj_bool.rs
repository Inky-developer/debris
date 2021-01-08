use debris_derive::object;

use crate::{
    llir::utils::ItemId,
    llir::{
        llir_nodes::Node,
        utils::{Scoreboard, ScoreboardValue},
    },
    memory::{copy, MemoryCounter, MemoryLayout},
    CompileContext, ObjectCopy, ObjectPayload, ObjectRef, Type, ValidPayload,
};

/// A boolean value that is stored on a scoreboard
///
/// The bool is treated as true if the scoreboard value is equal to one
/// If the value is zero, the bool is treated as false.
/// Any other value is undefined behaviour and should not be possible to achieve.
#[derive(Debug, Eq, PartialEq)]
pub struct ObjBool {
    pub id: ItemId,
}

#[object(Type::DynamicBool)]
impl ObjBool {
    pub fn new(id: ItemId) -> Self {
        ObjBool { id }
    }

    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Scoreboard(Scoreboard::Main, self.id)
    }
}

impl ObjectPayload for ObjBool {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::One(self.id)
    }
}

impl ObjectCopy for ObjBool {
    fn object_copy(
        &self,
        ctx: &CompileContext,
        nodes: &mut Vec<Node>,
        memory: &mut MemoryCounter,
    ) -> ObjectRef {
        let id = memory.next();
        let obj_bool = ObjBool::new(id);
        nodes.push(copy(id, self.id));
        obj_bool.into_object(ctx)
    }
}

impl From<ItemId> for ObjBool {
    fn from(val: ItemId) -> Self {
        ObjBool::new(val)
    }
}
