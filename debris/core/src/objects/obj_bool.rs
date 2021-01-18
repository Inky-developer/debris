use debris_derive::object;

use crate::{
    llir::utils::ItemId,
    llir::{
        llir_nodes::{Condition, FastStore, FastStoreFromResult, Node},
        utils::{Scoreboard, ScoreboardComparison, ScoreboardValue},
    },
    memory::{copy, MemoryLayout},
    CompileContext, ObjectPayload, Type,
};

use super::{obj_bool_static::ObjStaticBool, obj_function::FunctionContext};

/// Returns the boolean or-ed with the static value
pub fn or_static(item_id: ItemId, bool: &ObjBool, value: bool) -> (Node, ObjBool) {
    (
        match value {
            false => copy(item_id, bool.id),
            true => Node::FastStore(FastStore {
                id: item_id,
                scoreboard: Scoreboard::Main,
                value: ScoreboardValue::Static(1),
            }),
        },
        ObjBool::new(item_id),
    )
}

pub fn and_static(item_id: ItemId, bool: &ObjBool, value: bool) -> (Node, ObjBool) {
    (
        match value {
            true => copy(item_id, bool.id),
            false => Node::FastStore(FastStore {
                id: item_id,
                scoreboard: Scoreboard::Main,
                value: ScoreboardValue::Static(0),
            }),
        },
        ObjBool::new(item_id),
    )
}

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

    #[special]
    fn and(ctx: &mut FunctionContext, lhs: &ObjBool, rhs: &ObjBool) -> ObjBool {
        ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
            id: ctx.item_id,
            scoreboard: Scoreboard::Main,
            command: Box::new(Node::Condition(Condition::And(vec![
                Condition::Compare {
                    lhs: lhs.as_scoreboard_value(),
                    rhs: ScoreboardValue::Static(1),
                    comparison: ScoreboardComparison::Equal,
                },
                Condition::Compare {
                    lhs: rhs.as_scoreboard_value(),
                    rhs: ScoreboardValue::Static(1),
                    comparison: ScoreboardComparison::Equal,
                },
            ]))),
        }));
        ObjBool::new(ctx.item_id)
    }

    #[special]
    fn and(ctx: &mut FunctionContext, lhs: &ObjBool, rhs: &ObjStaticBool) -> ObjBool {
        let (node, value) = and_static(ctx.item_id, lhs, rhs.value);
        ctx.emit(node);
        value
    }

    #[special]
    fn or(ctx: &mut FunctionContext, lhs: &ObjBool, rhs: &ObjBool) -> ObjBool {
        ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
            id: ctx.item_id,
            scoreboard: Scoreboard::Main,
            command: Box::new(Node::Condition(Condition::Or(vec![
                Condition::Compare {
                    lhs: lhs.as_scoreboard_value(),
                    rhs: ScoreboardValue::Static(1),
                    comparison: ScoreboardComparison::Equal,
                },
                Condition::Compare {
                    lhs: rhs.as_scoreboard_value(),
                    rhs: ScoreboardValue::Static(1),
                    comparison: ScoreboardComparison::Equal,
                },
            ]))),
        }));
        ObjBool::new(ctx.item_id)
    }

    #[special]
    fn or(ctx: &mut FunctionContext, lhs: &ObjBool, rhs: &ObjStaticBool) -> ObjBool {
        let (node, value) = or_static(ctx.item_id, lhs, rhs.value);
        ctx.emit(node);
        value
    }

    #[special]
    fn not(ctx: &mut FunctionContext, value: &ObjBool) -> ObjBool {
        ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
            id: ctx.item_id,
            scoreboard: Scoreboard::Main,
            command: Box::new(Node::Condition(Condition::Compare {
                lhs: value.as_scoreboard_value(),
                rhs: ScoreboardValue::Static(0),
                comparison: ScoreboardComparison::Equal,
            })),
        }));
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
