use std::fmt;

use crate::{
    impl_class,
    llir_nodes::{BinaryOperation, Condition, FastStoreFromResult, Node},
    memory::{copy, MemoryLayout},
    utils::{ItemId, Scoreboard, ScoreboardComparison, ScoreboardOperation, ScoreboardValue},
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
/// This object defines binary operations for between itself and [static integers](debris_core::objects::obj_int::ObjStaticInt).
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjInt {
    /// The id of the item
    pub id: ItemId,
    memory_layout: MemoryLayout,
}

impl_class! {ObjInt, Type::DynamicInt, {
    "min" => |ctx: &mut FunctionContext, a: &ObjInt, b: &ObjInt| -> ObjInt {
        bin_op!(ScoreboardOperation::Min, ctx, a, b);
        ctx.item_id.into()
    },

    // "min" => |ctx: &mut FunctionContext, a: &ObjInt, b: &ObjStaticInt| -> ObjInt {
    //     bin_op!(ScoreboardOperation::Min, ctx, a, b);
    //     ctx.item_id.into()
    // }

    "max" => |ctx: &mut FunctionContext, a: &ObjInt, b: &ObjInt| -> ObjInt {
        bin_op!(ScoreboardOperation::Max, ctx, a, b);
        ctx.item_id.into()
    },

    // "max" => |ctx: &mut FunctionContext, a: &ObjInt, b: &ObjStaticInt| -> ObjInt {
    //     bin_op!(ScoreboardOperation::Max, ctx, a, b);
    //     ctx.item_id.into()
    // }

    Clone => |ctx: &mut FunctionContext, value: &ObjInt| -> ObjInt {
        ctx.emit(copy(ctx.item_id, value.id));
        ObjInt::new(ctx.item_id)
    },

    // Operations between dynamic ints
    Add => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
        bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
        ctx.item_id.into()
    },

    Sub => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
        bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
        ctx.item_id.into()
    },

    Mul => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
        bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
        ctx.item_id.into()
    },

    Div => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
        bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
        ctx.item_id.into()
    },

    Modu => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjInt {
        bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
        ctx.item_id.into()
    },

    // // Operations between dynamic int and static int
    // add => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
    //     bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
    //     ctx.item_id.into()
    // },

    // sub => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
    //     bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
    //     ctx.item_id.into()
    // },

    // mul => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
    //     bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
    //     ctx.item_id.into()
    // },

    // div => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
    //     bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
    //     ctx.item_id.into()
    // },

    // modu => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjInt {
    //     bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
    //     ctx.item_id.into()
    // },

    // Comparisons of Ints
    CmpEq => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
        cmp!(ctx, lhs, rhs, ScoreboardComparison::Equal)
    },

    CmpNe => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
        cmp!(ctx, lhs, rhs, ScoreboardComparison::NotEqual)
    },

    CmpGt => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
        cmp!(ctx, lhs, rhs, ScoreboardComparison::Greater)
    },

    CmpGe => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
        cmp!(ctx, lhs, rhs, ScoreboardComparison::GreaterOrEqual)
    },

    CmpLt => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
        cmp!(ctx, lhs, rhs, ScoreboardComparison::Less)
    },

    CmpLe => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjInt| -> ObjBool {
        cmp!(ctx, lhs, rhs, ScoreboardComparison::LessOrEqual)
    }

    // // Comparisons of Static ints
    // cmp_eq => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
    //     cmp!(ctx, lhs, rhs, ScoreboardComparison::Equal)
    // },

    // cmp_ne => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
    //     cmp!(ctx, lhs, rhs, ScoreboardComparison::NotEqual)
    // },

    // cmp_gt => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
    //     // a value cannot be greater than the maximum value
    //     if rhs.value == i32::MAX {
    //         ctx.emit(Node::FastStore(FastStore {
    //             id: ctx.item_id,
    //             scoreboard: Scoreboard::Main,
    //             value: ScoreboardValue::Static(0),
    //         }));
    //         return ObjBool::new(ctx.item_id);
    //     }
    //     cmp!(ctx, lhs, rhs, ScoreboardComparison::Greater)
    // },

    // cmp_ge => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
    //     // a value is always greater than the minimum value
    //     if rhs.value == i32::MIN {
    //         ctx.emit(Node::FastStore(FastStore {
    //             id: ctx.item_id,
    //             scoreboard: Scoreboard::Main,
    //             value: ScoreboardValue::Static(1),
    //         }));
    //         return ObjBool::new(ctx.item_id);
    //     }
    //     cmp!(ctx, lhs, rhs, ScoreboardComparison::GreaterOrEqual)
    // },

    // cmp_lt => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
    //     // a value cannot be less than the minimum value
    //     if rhs.value == i32::MIN {
    //         ctx.emit(Node::FastStore(FastStore {
    //             id: ctx.item_id,
    //             scoreboard: Scoreboard::Main,
    //             value: ScoreboardValue::Static(0),
    //         }));
    //         return ObjBool::new(ctx.item_id);
    //     }
    //     cmp!(ctx, lhs, rhs, ScoreboardComparison::Less)
    // },

    // cmp_le => |ctx: &mut FunctionContext, lhs: &ObjInt, rhs: &ObjStaticInt| -> ObjBool {
    //     // a value is always greater than the maximum value
    //     if rhs.value == i32::MAX {
    //         ctx.emit(Node::FastStore(FastStore {
    //             id: ctx.item_id,
    //             scoreboard: Scoreboard::Main,
    //             value: ScoreboardValue::Static(1),
    //         }));
    //         return ObjBool::new(ctx.item_id);
    //     }
    //     cmp!(ctx, lhs, rhs, ScoreboardComparison::LessOrEqual)
    // }
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
