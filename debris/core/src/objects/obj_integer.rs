use debris_derive::template;
use debris_type::Type;

use super::{FunctionContext, ObjectType, TypeRef};
use crate::{
    error::Result, CompileContext, DebrisObject, ObjectPayload, ObjectProperties, ObjectRef,
};

#[derive(Debug, Eq, PartialEq)]
pub struct ObjectInteger {
    pub value: i32,
}

#[template]
impl ObjectInteger {
    pub fn new<T: Into<i32>>(value: T) -> Self {
        ObjectInteger {
            value: value.into(),
        }
    }

    pub fn template() -> TypeRef {
        ObjectType::new_ref(
            Type::Template(Box::new(Type::Int)),
            ObjectProperties::default(),
            None,
        )
    }

    #[special(fn(Int, Int) -> Int)]
    fn add(_: &mut FunctionContext, a: &ObjectInteger, b: &ObjectInteger) -> Result<ObjectInteger> {
        Ok(ObjectInteger::new(a.value + b.value))
    }

    #[special(fn(Int, Int) -> Int)]
    fn sub(_: &mut FunctionContext, a: &ObjectInteger, b: &ObjectInteger) -> Result<ObjectInteger> {
        Ok(ObjectInteger::new(a.value - b.value))
    }

    #[special(fn(Int, Int) -> Int)]
    fn mul(_: &mut FunctionContext, a: &ObjectInteger, b: &ObjectInteger) -> Result<ObjectInteger> {
        Ok(ObjectInteger::new(a.value * b.value))
    }

    #[special(fn(Int, Int) -> Int)]
    fn div(_: &mut FunctionContext, a: &ObjectInteger, b: &ObjectInteger) -> Result<ObjectInteger> {
        Ok(ObjectInteger::new(a.value / b.value))
    }
}

impl ObjectPayload for ObjectInteger {
    fn typ(&self) -> Type {
        Type::Int
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

macro_rules! impl_for {
    ($x:ty, $($xs:tt)*) => {
        impl_for!($x);
        impl_for!($($xs)*);
    };
    ($x:ty) => {
        impl From<$x> for ObjectInteger {
            fn from(value: $x) -> Self {
                ObjectInteger::new(value as i32)
            }
        }
    };
    () => {};
}

impl_for!(i8, i16, i32, i64, i128, isize);
impl_for!(u8, u16, u32, u64, u128, usize);
