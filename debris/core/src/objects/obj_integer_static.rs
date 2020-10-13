use debris_derive::{object, ObjectPayload};

use crate::{llir::utils::ScoreboardValue, Type};

use super::FunctionContext;

/// A static integer object
///
/// Static integers are known at compile time and at runtime.
/// Binary operations are only supported betwen static integers.
/// To support operations between static and dynamic ints, static ints define PromoteTo<DynamicInteger> (toDo).
#[derive(Debug, Eq, PartialEq, ObjectPayload)]
pub struct StaticInt {
    pub value: i32,
}

#[object(Type::StaticInt)]
impl StaticInt {
    /// Creates a new static integers with this value
    pub fn new(value: impl Into<i32>) -> Self {
        StaticInt {
            value: value.into(),
        }
    }

    /// Returns a `ScoreboardValue` which matches this int
    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Static(self.value)
    }

    #[special]
    fn add(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> StaticInt {
        StaticInt::new(a.value + b.value)
    }

    #[special]
    fn sub(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> StaticInt {
        StaticInt::new(a.value - b.value)
    }

    #[special]
    fn mul(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> StaticInt {
        StaticInt::new(a.value * b.value)
    }

    #[special]
    fn div(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> StaticInt {
        StaticInt::new(a.value / b.value)
    }

    #[special]
    fn modu(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> StaticInt {
        StaticInt::new(a.value % b.value)
    }
}

/// Implements From for all numeric types
macro_rules! impl_for {
    ($x:ty, $($xs:tt)*) => {
        impl_for!($x);
        impl_for!($($xs)*);
    };
    ($x:ty) => {
        impl From<$x> for StaticInt {
            fn from(value: $x) -> Self {
                StaticInt::new(value as i32)
            }
        }
    };
    () => {};
}

impl_for!(i8, i16, i32, i64, i128, isize);
impl_for!(u8, u16, u32, u64, u128, usize);
