use std::{any::TypeId, rc::Rc};

use debris_common::SpecialIdent;

use crate::{
    error::LangResult, llir::utils::ScoreboardValue, CompileContext, DebrisObject, ObjectPayload,
    ObjectRef, Type,
};

use super::{
    CallbackFunction, ClassRef, FunctionContext, FunctionSignature, FunctionSignatureMap,
    ObjectClass, ObjectFunction,
};

/// A static integer object
///
/// Static integers are known at compile time and at runtime.
/// Binary operations are only supported betwen static integers.
/// To support operations between static and dynamic ints, static ints define PromoteTo<DynamicInteger> (toDo).
#[derive(Debug, Eq, PartialEq)]
pub struct StaticInt {
    pub value: i32,
}

// #[object]
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

    pub fn class(ctx: &CompileContext) -> ClassRef {
        ctx.type_ctx.get_or_insert(TypeId::of::<Self>(), || {
            fn add(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
                Ok(StaticInt::new(a.value + b.value))
            }

            let class: Rc<_> = ObjectClass::new_empty(Type::StaticInt).into();
            class.set_property(
                SpecialIdent::Add.into(),
                ObjectFunction::new(FunctionSignatureMap::new(vec![(
                    FunctionSignature::new(vec![class.clone(), class.clone()], class.clone()),
                    CallbackFunction(|ctx, parameters| {
                        add(
                            ctx,
                            parameters[0].downcast_payload().unwrap(),
                            parameters[1].downcast_payload().unwrap(),
                        )
                        .map(|result| result.into_object(ctx.compile_context))
                    }),
                )]))
                .into_object(ctx),
            );
            class
        })
    }

    // #[special]
    // fn add(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
    //     Ok(StaticInt::new(a.value + b.value))
    // }

    // #[special(fn(StaticInt, StaticInt) -> StaticInt)]
    // fn sub(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
    //     Ok(StaticInt::new(a.value - b.value))
    // }

    // #[special(fn(StaticInt, StaticInt) -> StaticInt)]
    // fn mul(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
    //     Ok(StaticInt::new(a.value * b.value))
    // }

    // #[special(fn(StaticInt, StaticInt) -> StaticInt)]
    // fn div(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
    //     Ok(StaticInt::new(a.value / b.value))
    // }

    // #[special(fn(StaticInt, StaticInt) -> StaticInt)]
    // fn modu(_: &mut FunctionContext, a: &StaticInt, b: &StaticInt) -> LangResult<StaticInt> {
    //     Ok(StaticInt::new(a.value % b.value))
    // }
}

impl ObjectPayload for StaticInt {
    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new_ref(Self::class(ctx), self)
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
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
