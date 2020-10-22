use debris_derive::object;

use crate::{
    llir::llir_nodes::BinaryOperation, llir::llir_nodes::Node, llir::utils::ItemId,
    llir::utils::Scoreboard, llir::utils::ScoreboardOperation, llir::utils::ScoreboardValue,
    ObjectPayload, Type,
};

use super::{FunctionContext, StaticInt};

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
pub struct DynInt {
    /// The id of the item
    pub id: ItemId,
}

#[object(Type::DynamicInt)]
impl DynInt {
    /// Creates a new dynamic integer with this id
    pub fn new(id: ItemId) -> Self {
        DynInt { id }
    }

    /// Returns a `ScoreboardValue` which identifies a specific value on a scoreboard
    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Scoreboard(Scoreboard::Main, self.id)
    }

    // Operations between dynamic ints
    #[special]
    fn add(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> DynInt {
        bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn sub(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> DynInt {
        bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn mul(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> DynInt {
        bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn div(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> DynInt {
        bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn modu(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> DynInt {
        bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    // Operations between dynamic int and static int
    #[special]
    fn add(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> DynInt {
        bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn sub(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> DynInt {
        bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn mul(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> DynInt {
        bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn div(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> DynInt {
        bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
        ctx.item_id.into()
    }

    #[special]
    fn modu(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> DynInt {
        bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
        ctx.item_id.into()
    }
}

impl ObjectPayload for DynInt {}

impl From<ItemId> for DynInt {
    fn from(value: ItemId) -> Self {
        DynInt { id: value }
    }
}
