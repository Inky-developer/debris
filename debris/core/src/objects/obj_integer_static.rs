use debris_derive::template;
use debris_type::Type;

use super::{FunctionContext, ObjectType, TypeRef};
use crate::{
    error::LangResult, llir::utils::ScoreboardValue, CompileContext, DebrisObject, ObjectPayload,
    ObjectProperties, ObjectRef,
};

#[derive(Debug, Eq, PartialEq)]
pub struct StaticInt {
    pub value: i32,
}

impl ObjectPayload for StaticInt {
    fn typ(&self) -> Type {
        Type::StaticInt
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new(ctx.type_ctx.template_for_type(&self.typ()), self).into()
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
    }
}

#[template]
impl StaticInt {
    pub fn new<T: Into<i32>>(value: T) -> Self {
        StaticInt {
            value: value.into(),
        }
    }

    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Static(self.value)
    }

    pub fn template() -> TypeRef {
        ObjectType::new_ref(
            Type::Template(Box::new(Type::StaticInt)),
            ObjectProperties::default(),
            None,
        )
    }

    #[special(fn(StaticInt, StaticInt) -> StaticInt)]
    fn add(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
        Ok(StaticInt::new(a.value + b.value))
    }

    #[special(fn(StaticInt, StaticInt) -> StaticInt)]
    fn sub(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
        Ok(StaticInt::new(a.value - b.value))
    }

    #[special(fn(StaticInt, StaticInt) -> StaticInt)]
    fn mul(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
        Ok(StaticInt::new(a.value * b.value))
    }

    #[special(fn(StaticInt, StaticInt) -> StaticInt)]
    fn div(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
        Ok(StaticInt::new(a.value / b.value))
    }

    #[special(fn(StaticInt, StaticInt) -> StaticInt)]
    fn modu(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
        Ok(StaticInt::new(a.value % b.value))
    }
}

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
