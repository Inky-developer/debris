use std::fmt;

use crate::{
    function_interface::make_overload,
    impl_class,
    item_id::ItemId,
    llir_nodes::{BinaryOperation, Condition, FastStore, FastStoreFromResult, Node},
    memory::MemoryLayout,
    minecraft_utils::{Scoreboard, ScoreboardComparison, ScoreboardOperation, ScoreboardValue},
    objects::obj_int_static::ObjStaticInt,
    ObjectPayload, Type,
};

use super::{obj_bool::ObjBool, obj_function::FunctionContext};

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

macro_rules! cmp {
    ($ctx:expr, $lhs:expr, $rhs:expr, $cmp:expr) => {{
        $ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
            scoreboard: Scoreboard::Main,
            id: $ctx.item_id,
            command: Box::new(Node::Condition(Condition::Compare {
                lhs: $lhs.as_scoreboard_value(),
                rhs: $rhs.as_scoreboard_value(),
                comparison: $cmp,
            })),
        }));

        $ctx.item_id.into()
    }};
}

/// A dynamic Integer
///
/// Dynamic means that the value of this integer is know at runtime, but not at compile time.
/// These integers could for example be stored in a scoreboard.
///
/// This object defines binary operations for between itself and [static integers](crate::objects::obj_int_static::ObjStaticInt).
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjInt {
    /// The id of the item
    pub id: ItemId,
    memory_layout: MemoryLayout,
}

impl_class! {ObjInt, Type::DynamicInt, {
    "min" => make_overload(vec![
        |ctx: &mut FunctionContext, a: &ObjInt, b: &ObjInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Min, ctx, a, b);
            ctx.item_id.into()
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, a: &ObjInt, b: &ObjStaticInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Min, ctx, a, b);
            ctx.item_id.into()
        }.to_normalized_function(),
    ]),

    "max" => make_overload(vec![
        |ctx: &mut FunctionContext, a: &ObjInt, b: &ObjInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Max, ctx, a, b);
            ctx.item_id.into()
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, a: &ObjInt, b: &ObjStaticInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Max, ctx, a, b);
            ctx.item_id.into()
        }.to_normalized_function(),
    ]),

    UnaryMinus => |ctx: &mut FunctionContext, value: &ObjInt| -> ObjInt {
        let factor = ObjStaticInt::new(-1);
        bin_op!(ScoreboardOperation::Times, ctx, value, factor);
        ctx.item_id.into()
    },

    Add => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
    ]),

    Sub => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
    ]),

    Mul => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
    ]),

    Div => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
    ]),

    Mod => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
            bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
            ctx.item_id.into()
        }.to_normalized_function(),
    ]),

    CmpEq => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
            cmp!(ctx, lhs, rhs, ScoreboardComparison::Equal)
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
            cmp!(ctx, lhs, rhs, ScoreboardComparison::Equal)
        }.to_normalized_function(),
    ]),

    CmpNe => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
            cmp!(ctx, lhs, rhs, ScoreboardComparison::NotEqual)
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
            cmp!(ctx, lhs, rhs, ScoreboardComparison::NotEqual)
        }.to_normalized_function(),
    ]),

    CmpGt => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
            // a value cannot be greater than the maximum value
            if rhs.value == i32::MAX {
                ctx.emit(Node::FastStore(FastStore {
                    id: ctx.item_id,
                    scoreboard: Scoreboard::Main,
                    value: ScoreboardValue::Static(0),
                }));
                return ObjBool::new(ctx.item_id);
            }
            cmp!(ctx, lhs, rhs, ScoreboardComparison::Greater)
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
            cmp!(ctx, lhs, rhs, ScoreboardComparison::Greater)
        }.to_normalized_function(),
    ]),

    CmpGe => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
            // a value is always greater than the minimum value
            if rhs.value == i32::MIN {
                ctx.emit(Node::FastStore(FastStore {
                    id: ctx.item_id,
                    scoreboard: Scoreboard::Main,
                    value: ScoreboardValue::Static(1),
                }));
                return ObjBool::new(ctx.item_id);
            }
            cmp!(ctx, lhs, rhs, ScoreboardComparison::GreaterOrEqual)
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
            cmp!(ctx, lhs, rhs, ScoreboardComparison::GreaterOrEqual)
        }.to_normalized_function(),
    ]),

    CmpLt => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
            // a value cannot be less than the minimum value
            if rhs.value == i32::MIN {
                ctx.emit(Node::FastStore(FastStore {
                    id: ctx.item_id,
                    scoreboard: Scoreboard::Main,
                    value: ScoreboardValue::Static(0),
                }));
                return ObjBool::new(ctx.item_id);
            }
            cmp!(ctx, lhs, rhs, ScoreboardComparison::Less)
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
            cmp!(ctx, lhs, rhs, ScoreboardComparison::Less)
        }.to_normalized_function(),
    ]),

    CmpLe => make_overload(vec![
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
            // a value is always greater than the maximum value
            if rhs.value == i32::MAX {
                ctx.emit(Node::FastStore(FastStore {
                    id: ctx.item_id,
                    scoreboard: Scoreboard::Main,
                    value: ScoreboardValue::Static(1),
                }));
                return ObjBool::new(ctx.item_id);
            }
            cmp!(ctx, lhs, rhs, ScoreboardComparison::LessOrEqual)
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
            cmp!(ctx, lhs, rhs, ScoreboardComparison::LessOrEqual)
        }.to_normalized_function()
    ])
}}

impl ObjInt {
    /// Creates a new dynamic integer with this id
    pub fn new(id: ItemId) -> Self {
        ObjInt {
            id,
            memory_layout: MemoryLayout::One(id),
        }
    }

    /// Returns a `ScoreboardValue` which identifies a specific value on a scoreboard
    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Scoreboard(Scoreboard::Main, self.id)
    }
}

impl ObjectPayload for ObjInt {
    fn memory_layout(&self) -> &MemoryLayout {
        &self.memory_layout
    }
}

impl fmt::Display for ObjInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Int({})", self.id)
    }
}

impl From<ItemId> for ObjInt {
    fn from(value: ItemId) -> Self {
        ObjInt::new(value)
    }
}
