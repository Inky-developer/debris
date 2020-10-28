use debris_derive::object;

use crate::{
    llir::llir_nodes::BinaryOperation,
    llir::llir_nodes::Node,
    llir::utils::ItemId,
    llir::utils::Scoreboard,
    llir::utils::ScoreboardOperation,
    llir::{
        llir_nodes::{Condition, FastStoreFromResult},
        utils::{ScoreboardComparison, ScoreboardValue},
    },
    ObjectPayload, Type,
};

use super::{FunctionContext, ObjBool, ObjStaticInt};

/// Shorthand for adding a binary operation node
macro_rules! bin_op {
    ($operation:expr, $ctx:ident, $lhs:ident, $rhs:ident) => {
        $ctx.emit(Node::BinaryOperation(BinaryOperation {
            id: $ctx.item_id,
            scoreboard: Scoreboard::Main,
            operation: $operation,
            lhs: $lhs.as_scoreboard_value(),
            rhs: $rhs.as_scoreboard_value(),
        }));
    };
}

/// A dynamic Integer
///
/// Dynamic means that the value of this integer is know at runtime, but not at compile time.
/// These integers could for example be stored in a scoreboard.
///
/// This object defines binary operations for between itself and [static integers](debris_core::objects::StaticInt).
#[derive(Debug, Eq, PartialEq)]
pub struct ObjInt {
    /// The id of the item
    pub id: ItemId,
}

#[object(Type::DynamicInt)]
impl ObjInt {
    /// Creates a new dynamic integer with this id
    pub fn new(id: ItemId) -> Self {
        ObjInt { id }
    }

    /// Returns a `ScoreboardValue` which identifies a specific value on a scoreboard
    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Scoreboard(Scoreboard::Main, self.id)
    }

    // Operations between dynamic ints
    #[special]
    fn add(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn sub(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn mul(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn div(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn modu(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    // Operations between dynamic int and static int
    #[special]
    fn add(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn sub(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn mul(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn div(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn modu(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn cmp_eq(ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt) -> ObjBool {
        ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
            scoreboard: Scoreboard::Main,
            id: ctx.item_id,
            command: Box::new(Node::Condition(Condition::Compare {
                lhs: lhs.as_scoreboard_value(),
                rhs: rhs.as_scoreboard_value(),
                comparison: ScoreboardComparison::Equal,
            })),
        }));

        ctx.item_id.into()
    }
}

impl ObjectPayload for ObjInt {}

impl From<ItemId> for ObjInt {
    fn from(value: ItemId) -> Self {
        ObjInt { id: value }
    }
}
