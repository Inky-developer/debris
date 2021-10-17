use std::fmt;

use debris_derive::object;

use super::{obj_bool::ObjBool, obj_function::FunctionContext, obj_int::ObjInt};

use crate::{
    llir_nodes::{BinaryOperation, Condition, FastStore, FastStoreFromResult, Node},
    memory::MemoryLayout,
    utils::{Scoreboard, ScoreboardComparison, ScoreboardOperation, ScoreboardValue},
    ObjectPayload, Type,
};

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

/// A static integer object
///
/// Static integers are known at compile time and at runtime.
/// Binary operations are only supported betwen static integers.
/// To support operations between static and dynamic ints, static ints define PromoteTo<DynamicInteger> (toDo).
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjStaticInt {
    pub value: i32,
}

#[object(Type::ComptimeInt)]
impl ObjStaticInt {
    /// Creates a new static integers with this value
    pub fn new(value: impl Into<i32>) -> Self {
        ObjStaticInt {
            value: value.into(),
        }
    }

    /// Returns a `ScoreboardValue` which matches this int
    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Static(self.value)
    }

    #[method]
    fn abs(this: &ObjStaticInt) -> i32 {
        this.value.abs()
    }

    #[method]
    fn min(a: &ObjStaticInt, b: &ObjStaticInt) -> i32 {
        ScoreboardOperation::Min.evaluate(a.value, b.value)
    }

    #[method]
    fn min(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Min, ctx, a, b);
        ctx.item_id.into()
    }

    #[method]
    fn max(a: &ObjStaticInt, b: &ObjStaticInt) -> i32 {
        ScoreboardOperation::Max.evaluate(a.value, b.value)
    }

    #[method]
    fn max(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Max, ctx, a, b);
        ctx.item_id.into()
    }

    #[special]
    fn promote_runtime(ctx: &mut FunctionContext, this: &ObjStaticInt) -> ObjInt {
        ctx.emit(Node::FastStore(FastStore {
            id: ctx.item_id,
            scoreboard: Scoreboard::Main,
            value: ScoreboardValue::Static(this.value),
        }));
        ObjInt::new(ctx.item_id)
    }

    #[special]
    fn clone(this: &ObjStaticInt) -> i32 {
        this.value
    }

    // Operations between two static ints
    #[special]
    fn add(a: &ObjStaticInt, b: &ObjStaticInt) -> i32 {
        ScoreboardOperation::Plus.evaluate(a.value, b.value)
    }

    #[special]
    fn sub(a: &ObjStaticInt, b: &ObjStaticInt) -> i32 {
        ScoreboardOperation::Minus.evaluate(a.value, b.value)
    }

    #[special]
    fn mul(a: &ObjStaticInt, b: &ObjStaticInt) -> i32 {
        ScoreboardOperation::Times.evaluate(a.value, b.value)
    }

    #[special]
    fn div(a: &ObjStaticInt, b: &ObjStaticInt) -> i32 {
        ScoreboardOperation::Divide.evaluate(a.value, b.value)
    }

    #[special]
    fn modu(a: &ObjStaticInt, b: &ObjStaticInt) -> i32 {
        ScoreboardOperation::Modulo.evaluate(a.value, b.value)
    }

    // Operations between static and non-static int
    #[special]
    fn add(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Plus, ctx, a, b);
        ctx.item_id.into()
    }

    #[special]
    fn sub(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Minus, ctx, a, b);
        ctx.item_id.into()
    }

    #[special]
    fn mul(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Times, ctx, a, b);
        ctx.item_id.into()
    }

    #[special]
    fn div(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Divide, ctx, a, b);
        ctx.item_id.into()
    }

    #[special]
    fn modu(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
        bin_op!(ScoreboardOperation::Modulo, ctx, a, b);
        ctx.item_id.into()
    }

    // Comparisons between two static ints
    #[special]
    fn cmp_eq(a: &ObjStaticInt, b: &ObjStaticInt) -> bool {
        a.value == b.value
    }

    #[special]
    fn cmp_ne(a: &ObjStaticInt, b: &ObjStaticInt) -> bool {
        a.value != b.value
    }

    #[special]
    fn cmp_gt(a: &ObjStaticInt, b: &ObjStaticInt) -> bool {
        a.value > b.value
    }

    #[special]
    fn cmp_ge(a: &ObjStaticInt, b: &ObjStaticInt) -> bool {
        a.value >= b.value
    }

    #[special]
    fn cmp_lt(a: &ObjStaticInt, b: &ObjStaticInt) -> bool {
        a.value < b.value
    }

    #[special]
    fn cmp_le(a: &ObjStaticInt, b: &ObjStaticInt) -> bool {
        a.value <= b.value
    }

    // comparisons between static int and dynamic int
    #[special]
    fn cmp_eq(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
        cmp!(ctx, a, b, ScoreboardComparison::Equal)
    }

    #[special]
    fn cmp_ne(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
        cmp!(ctx, a, b, ScoreboardComparison::NotEqual)
    }

    #[special]
    fn cmp_gt(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
        // The minimum value is never greater than any other value
        if a.value == i32::MIN {
            ctx.emit(Node::FastStore(FastStore {
                id: ctx.item_id,
                scoreboard: Scoreboard::Main,
                value: ScoreboardValue::Static(0),
            }));
            return ObjBool::new(ctx.item_id);
        }
        cmp!(ctx, a, b, ScoreboardComparison::Greater)
    }

    #[special]
    fn cmp_ge(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
        // The maximum value is always greater or equal than any other value
        if a.value == i32::MAX {
            ctx.emit(Node::FastStore(FastStore {
                id: ctx.item_id,
                scoreboard: Scoreboard::Main,
                value: ScoreboardValue::Static(1),
            }));
            return ObjBool::new(ctx.item_id);
        }
        cmp!(ctx, a, b, ScoreboardComparison::GreaterOrEqual)
    }

    #[special]
    fn cmp_lt(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
        // The maximum value is never less than any other value
        if a.value == i32::MAX {
            ctx.emit(Node::FastStore(FastStore {
                id: ctx.item_id,
                scoreboard: Scoreboard::Main,
                value: ScoreboardValue::Static(0),
            }));
            return ObjBool::new(ctx.item_id);
        }
        cmp!(ctx, a, b, ScoreboardComparison::Less)
    }

    #[special]
    fn cmp_le(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
        // The minimum value is always less than or equal any other value
        if a.value == i32::MIN {
            ctx.emit(Node::FastStore(FastStore {
                id: ctx.item_id,
                scoreboard: Scoreboard::Main,
                value: ScoreboardValue::Static(1),
            }));
            return ObjBool::new(ctx.item_id);
        }
        cmp!(ctx, a, b, ScoreboardComparison::LessOrEqual)
    }
}

impl ObjectPayload for ObjStaticInt {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}

impl fmt::Display for ObjStaticInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// Implements From for all numeric types
macro_rules! impl_for {
    ($x:ty, $($xs:tt)*) => {
        impl_for!($x);
        impl_for!($($xs)*);
    };
    ($x:ty) => {
        impl From<$x> for ObjStaticInt {
            fn from(value: $x) -> Self {
                ObjStaticInt::new(value as i32)
            }
        }
    };
    () => {};
}

impl_for!(i8, i16, i32, i64, i128, isize);
impl_for!(u8, u16, u32, u64, u128, usize);
