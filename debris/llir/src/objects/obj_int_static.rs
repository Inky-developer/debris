use std::fmt;

use debris_error::{LangErrorKind, LangResult};

use super::{obj_class::HasClass, obj_function::FunctionContext, obj_int::ObjInt};

use crate::{
    class::ClassRef,
    impl_class,
    llir_nodes::{FastStore, Node},
    memory::MemoryLayout,
    objects::obj_class::ObjClass,
    type_context::TypeContext,
    utils::{Scoreboard, ScoreboardOperation, ScoreboardValue},
    ObjectPayload, ObjectRef, Type,
};

// /// Shorthand for adding a binary operation node
// macro_rules! bin_op {
//     ($operation:expr, $ctx:ident, $lhs:ident, $rhs:ident) => {
//         $ctx.emit(Node::BinaryOperation(BinaryOperation {
//             id: $ctx.item_id,
//             scoreboard: Scoreboard::Main,
//             operation: $operation,
//             lhs: $lhs.as_scoreboard_value(),
//             rhs: $rhs.as_scoreboard_value(),
//         }));
//     };
// }

// macro_rules! cmp {
//     ($ctx:expr, $lhs:expr, $rhs:expr, $cmp:expr) => {{
//         $ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
//             scoreboard: Scoreboard::Main,
//             id: $ctx.item_id,
//             command: Box::new(Node::Condition(Condition::Compare {
//                 lhs: $lhs.as_scoreboard_value(),
//                 rhs: $rhs.as_scoreboard_value(),
//                 comparison: $cmp,
//             })),
//         }));

//         $ctx.item_id.into()
//     }};
// }

/// A static integer object
///
/// Static integers are known at compile time and at runtime.
/// Binary operations are only supported betwen static integers.
/// To support operations between static and dynamic ints, static ints define PromoteTo<DynamicInteger> (toDo).
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjStaticInt {
    pub value: i32,
}

impl_class! {ObjStaticInt, Type::ComptimeInt, {
    "abs" => |this: &ObjStaticInt| -> i32 {
        this.value.abs()
    },

    "min" => |a: &ObjStaticInt, b: &ObjStaticInt| -> i32 {
        ScoreboardOperation::Min.evaluate(a.value, b.value)
    },

    // "min" => |ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt| -> ObjInt {
    //     bin_op!(ScoreboardOperation::Min, ctx, a, b);
    //     ctx.item_id.into()
    // }

    "max" => |a: &ObjStaticInt, b: &ObjStaticInt| -> i32 {
        ScoreboardOperation::Max.evaluate(a.value, b.value)
    },

    // fn max(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
    //     bin_op!(ScoreboardOperation::Max, ctx, a, b);
    //     ctx.item_id.into()
    // }

    Promote => |ctx: &mut FunctionContext, this: &ObjStaticInt, target: &ObjClass| -> LangResult<ObjectRef> {
        match target.class.kind.typ() {
            Type::DynamicInt => {
                ctx.emit(Node::FastStore(FastStore {
                    id: ctx.item_id,
                    scoreboard: Scoreboard::Main,
                    value: ScoreboardValue::Static(this.value),
                }));
                Ok(ObjInt::new(ctx.item_id).into_object(ctx.type_ctx))
            }
            _ => Err(LangErrorKind::InvalidConversion {
                this: this.get_class(ctx.type_ctx).to_string(),
                target: target.class.to_string(),
            }),
        }
    },

    Clone => |this: &ObjStaticInt| -> i32 {
        this.value
    },

    // Operations between two static ints
    Add => |a: &ObjStaticInt, b: &ObjStaticInt| -> i32 {
        ScoreboardOperation::Plus.evaluate(a.value, b.value)
    },

    Sub => |a: &ObjStaticInt, b: &ObjStaticInt| -> i32 {
        ScoreboardOperation::Minus.evaluate(a.value, b.value)
    },

    Mul => |a: &ObjStaticInt, b: &ObjStaticInt| -> i32 {
        ScoreboardOperation::Times.evaluate(a.value, b.value)
    },

    Div => |a: &ObjStaticInt, b: &ObjStaticInt| -> i32 {
        ScoreboardOperation::Divide.evaluate(a.value, b.value)
    },

    Modu => |a: &ObjStaticInt, b: &ObjStaticInt| -> i32 {
        ScoreboardOperation::Modulo.evaluate(a.value, b.value)
    },

    // // Operations between static and non-static int
    // fn add(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
    //     bin_op!(ScoreboardOperation::Plus, ctx, a, b);
    //     ctx.item_id.into()
    // }

    // fn sub(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
    //     bin_op!(ScoreboardOperation::Minus, ctx, a, b);
    //     ctx.item_id.into()
    // }

    // fn mul(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
    //     bin_op!(ScoreboardOperation::Times, ctx, a, b);
    //     ctx.item_id.into()
    // }

    // fn div(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
    //     bin_op!(ScoreboardOperation::Divide, ctx, a, b);
    //     ctx.item_id.into()
    // }

    // fn modu(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjInt {
    //     bin_op!(ScoreboardOperation::Modulo, ctx, a, b);
    //     ctx.item_id.into()
    // }

    // Comparisons between two static ints
    CmpEq => |a: &ObjStaticInt, b: &ObjStaticInt| -> bool {
        a.value == b.value
    },

    CmpNe => |a: &ObjStaticInt, b: &ObjStaticInt| -> bool {
        a.value != b.value
    },

    CmpGt => |a: &ObjStaticInt, b: &ObjStaticInt| -> bool {
        a.value > b.value
    },

    CmpGe => |a: &ObjStaticInt, b: &ObjStaticInt| -> bool {
        a.value >= b.value
    },

    CmpLt => |a: &ObjStaticInt, b: &ObjStaticInt| -> bool {
        a.value < b.value
    },

    CmpLe => |a: &ObjStaticInt, b: &ObjStaticInt| -> bool {
        a.value <= b.value
    }

    // // comparisons between static int and dynamic int
    // fn cmp_eq(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
    //     cmp!(ctx, a, b, ScoreboardComparison::Equal)
    // }

    // fn cmp_ne(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
    //     cmp!(ctx, a, b, ScoreboardComparison::NotEqual)
    // }

    // fn cmp_gt(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
    //     // The minimum value is never greater than any other value
    //     if a.value == i32::MIN {
    //         ctx.emit(Node::FastStore(FastStore {
    //             id: ctx.item_id,
    //             scoreboard: Scoreboard::Main,
    //             value: ScoreboardValue::Static(0),
    //         }));
    //         return ObjBool::new(ctx.item_id);
    //     }
    //     cmp!(ctx, a, b, ScoreboardComparison::Greater)
    // }

    // fn cmp_ge(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
    //     // The maximum value is always greater or equal than any other value
    //     if a.value == i32::MAX {
    //         ctx.emit(Node::FastStore(FastStore {
    //             id: ctx.item_id,
    //             scoreboard: Scoreboard::Main,
    //             value: ScoreboardValue::Static(1),
    //         }));
    //         return ObjBool::new(ctx.item_id);
    //     }
    //     cmp!(ctx, a, b, ScoreboardComparison::GreaterOrEqual)
    // }

    // fn cmp_lt(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
    //     // The maximum value is never less than any other value
    //     if a.value == i32::MAX {
    //         ctx.emit(Node::FastStore(FastStore {
    //             id: ctx.item_id,
    //             scoreboard: Scoreboard::Main,
    //             value: ScoreboardValue::Static(0),
    //         }));
    //         return ObjBool::new(ctx.item_id);
    //     }
    //     cmp!(ctx, a, b, ScoreboardComparison::Less)
    // }

    // fn cmp_le(ctx: &mut FunctionContext, a: &ObjStaticInt, b: &ObjInt) -> ObjBool {
    //     // The minimum value is always less than or equal any other value
    //     if a.value == i32::MIN {
    //         ctx.emit(Node::FastStore(FastStore {
    //             id: ctx.item_id,
    //             scoreboard: Scoreboard::Main,
    //             value: ScoreboardValue::Static(1),
    //         }));
    //         return ObjBool::new(ctx.item_id);
    //     }
    //     cmp!(ctx, a, b, ScoreboardComparison::LessOrEqual)
    // }
}}

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
}

impl ObjectPayload for ObjStaticInt {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn runtime_class(&self, ctx: &TypeContext) -> Option<ClassRef> {
        Some(ObjInt::class(ctx))
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
