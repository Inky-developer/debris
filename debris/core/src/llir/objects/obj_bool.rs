use std::fmt;

use debris_derive::object;

use crate::llir::{
    llir_nodes::{Condition, FastStore, FastStoreFromResult, Node},
    memory::{copy, MemoryLayout},
    utils::{ItemId, Scoreboard, ScoreboardComparison, ScoreboardValue},
    ObjectPayload, Type,
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

pub fn cmp(
    item_id: ItemId,
    bool: &ObjBool,
    value: ScoreboardValue,
    cmp: ScoreboardComparison,
) -> (Node, ObjBool) {
    (
        Node::FastStoreFromResult(FastStoreFromResult {
            id: item_id,
            scoreboard: Scoreboard::Main,
            command: Box::new(Node::Condition(Condition::Compare {
                comparison: cmp,
                lhs: bool.as_scoreboard_value(),
                rhs: value,
            })),
        }),
        item_id.into(),
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
    pub memory_layout: MemoryLayout,
}

#[object(Type::DynamicBool)]
impl ObjBool {
    pub fn new(id: ItemId) -> Self {
        ObjBool {
            id,
            memory_layout: MemoryLayout::One(id),
        }
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

    #[special]
    fn cmp_eq(ctx: &mut FunctionContext, this: &ObjBool, other: &ObjStaticBool) -> ObjBool {
        let (node, ret) = cmp(
            ctx.item_id,
            this,
            other.as_scoreboard_value(),
            ScoreboardComparison::Equal,
        );
        ctx.emit(node);
        ret
    }

    #[special]
    fn cmp_eq(ctx: &mut FunctionContext, this: &ObjBool, other: &ObjBool) -> ObjBool {
        let (node, ret) = cmp(
            ctx.item_id,
            this,
            other.as_scoreboard_value(),
            ScoreboardComparison::Equal,
        );
        ctx.emit(node);
        ret
    }

    #[special]
    fn cmp_ne(ctx: &mut FunctionContext, this: &ObjBool, other: &ObjStaticBool) -> ObjBool {
        let (node, ret) = cmp(
            ctx.item_id,
            this,
            other.as_scoreboard_value(),
            ScoreboardComparison::NotEqual,
        );
        ctx.emit(node);
        ret
    }

    #[special]
    fn cmp_ne(ctx: &mut FunctionContext, this: &ObjBool, other: &ObjBool) -> ObjBool {
        let (node, ret) = cmp(
            ctx.item_id,
            this,
            other.as_scoreboard_value(),
            ScoreboardComparison::NotEqual,
        );
        ctx.emit(node);
        ret
    }
}

impl ObjectPayload for ObjBool {
    fn memory_layout(&self) -> &MemoryLayout {
        &self.memory_layout
    }
}

impl fmt::Display for ObjBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bool({})", self.id)
    }
}

impl From<ItemId> for ObjBool {
    fn from(val: ItemId) -> Self {
        ObjBool::new(val)
    }
}
