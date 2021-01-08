use debris_derive::object;

use crate::{
    llir::utils::ItemId,
    llir::utils::{Scoreboard, ScoreboardValue},
    memory::{copy, MemoryLayout},
    CompileContext, ObjectPayload, Type,
};

use super::FunctionContext;

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

    #[special]
    fn clone(ctx: &mut FunctionContext, value: &ObjBool) -> ObjBool {
        ctx.emit(copy(ctx.item_id, value.id));
        ObjBool::new(ctx.item_id)
    }
}

impl ObjectPayload for ObjBool {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::One(self.id)
    }
}

impl From<ItemId> for ObjBool {
    fn from(val: ItemId) -> Self {
        ObjBool::new(val)
    }
}
