use debris_derive::object;

use crate::{
    llir::utils::ItemId,
    llir::utils::{Scoreboard, ScoreboardValue},
    ObjectPayload, Type,
};

/// A boolean value that is stored on a scoreboard
///
/// The bool is treated as true if the scoreboard value is not zero.
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

impl ObjectPayload for ObjBool {}

impl From<ItemId> for ObjBool {
    fn from(val: ItemId) -> Self {
        ObjBool::new(val)
    }
}
