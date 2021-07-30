//! This module contains a trait `ToFunctionInterface` which allows to write nice function interfaces
//! by using much less boilerplate to define functions.
//!
//! # Examples:
//! ```ignore
//! use debris_core::{objects::{ObjInt, FunctionContext}, ObjectRef, error::LangResult};
//!
//! /// The most general form of a function
//! fn foo(ctx: &mut FunctionContext, objects: &[ObjectRef]) -> LangResult<ObjectRef> {
//!     // [...]
//! }
//!
//! /// Return types can be anything that can be converted into a LangResult<ObjectRef>, so this is also valid
//! fn bar(ctx: &mut FunctionContext, objects: &[ObjectRef]) -> ObjectRef {
//!     // [...]
//! }
//!
//! /// Instead of taking an arbitrary amount of any object, it is possible to specify which objects are required
//! fn square(_ctx: &mut FunctionContext, value: &ObjInt) -> ObjInt {
//!     // [...]
//! }
//!
//! /// Since integers can be converted into `ObjStaticInt`, this is also possible
//! /// Note that `ctx` can be omitted if it is not needed
//! fn square_static(value: &ObjStaticInt) -> i32 {
//!     value.value * value.value
//! }
//! ```

use std::rc::Rc;

use crate::{
    error::{LangError, LangResult, Result},
    objects::{
        obj_bool_static::ObjStaticBool, obj_function::FunctionContext,
        obj_int_static::ObjStaticInt, obj_null::ObjNull, obj_string::ObjString,
    },
    CompileContext, ObjectPayload, ObjectRef, ValidPayload,
};

/// The common type for working with callbacks
pub struct DebrisFunctionInterface(Box<dyn NormalizedFunctionInterface>);

impl DebrisFunctionInterface {
    /// Calls this interface and returns the result and a vec of the generated nodes
    pub(crate) fn call(
        &self,
        function_ctx: &mut FunctionContext,
        parameters: &[ObjectRef],
    ) -> Result<ObjectRef> {
        let return_value = match self.0.call(function_ctx, parameters) {
            Ok(val) => val,
            Err(lang_err) => return Err(LangError::new(lang_err, function_ctx.span).into()),
        };

        Ok(return_value)
    }
}

impl From<Box<dyn NormalizedFunctionInterface>> for DebrisFunctionInterface {
    fn from(value: Box<dyn NormalizedFunctionInterface>) -> Self {
        DebrisFunctionInterface(value)
    }
}

impl<F> From<F> for DebrisFunctionInterface
where
    F: NormalizedFunctionInterface + 'static,
{
    fn from(value: F) -> Self {
        DebrisFunctionInterface(Box::new(value))
    }
}

/// Any function that can be called as a normal function interface
pub trait NormalizedFunctionInterface {
    fn call(&self, ctx: &mut FunctionContext, parameters: &[ObjectRef]) -> LangResult<ObjectRef>;
}

impl<F> NormalizedFunctionInterface for F
where
    F: Fn(&mut FunctionContext, &[ObjectRef]) -> LangResult<ObjectRef>,
{
    fn call(&self, ctx: &mut FunctionContext, parameters: &[ObjectRef]) -> LangResult<ObjectRef> {
        (self)(ctx, parameters)
    }
}

/// Trait used for converting any valid return value into a LangResult<ObjectRef>
pub trait ValidReturnType {
    fn into_result(self, ctx: &mut FunctionContext) -> LangResult<ObjectRef>;
}

impl<T> ValidReturnType for T
where
    T: ObjectPayload,
{
    fn into_result(self, ctx: &mut FunctionContext) -> LangResult<ObjectRef> {
        Ok(self.into_object(ctx.compile_context()))
    }
}

impl<T> ValidReturnType for LangResult<T>
where
    T: ObjectPayload,
{
    fn into_result(self, ctx: &mut FunctionContext) -> LangResult<ObjectRef> {
        self.map(|value| value.into_object(ctx.compile_context()))
    }
}

impl ValidReturnType for LangResult<ObjectRef> {
    fn into_result(self, _ctx: &mut FunctionContext) -> LangResult<ObjectRef> {
        self
    }
}

impl ValidReturnType for ObjectRef {
    fn into_result(self, _ctx: &mut FunctionContext) -> LangResult<ObjectRef> {
        Ok(self)
    }
}

/// Maps a valid return type from `$from` to `$to`
macro_rules! impl_map_valid_return_type {
    ($from:ty, $to:ty) => {
        impl ValidReturnType for $from {
            fn into_result(self, ctx: &mut FunctionContext) -> LangResult<ObjectRef> {
                Ok(<$to as From<$from>>::from(self).into_object(ctx.compile_context()))
            }
        }
    };
}

// Functions which return unit are mapped to ObjNull
impl_map_valid_return_type!((), ObjNull);
impl_map_valid_return_type!(i32, ObjStaticInt);
impl_map_valid_return_type!(bool, ObjStaticBool);
impl_map_valid_return_type!(Rc<str>, ObjString);

/// This trait can convert functions into compatible interface functions
pub trait ToFunctionInterface<Params, Return>
where
    Return: ValidReturnType,
    Self: 'static,
{
    fn to_function_interface(&'static self) -> Box<dyn NormalizedFunctionInterface>;
}

/// For functions of the format Fn(ctx, objects) -> ValidReturn
impl<F, R> ToFunctionInterface<(&mut FunctionContext, &[ObjectRef]), R> for F
where
    F: Fn(&mut FunctionContext, &[ObjectRef]) -> R + 'static,
    R: ValidReturnType,
{
    fn to_function_interface(&'static self) -> Box<dyn NormalizedFunctionInterface> {
        Box::new(move |ctx: &mut FunctionContext, objects: &[ObjectRef]| {
            (self)(ctx, objects).into_result(ctx)
        })
    }
}

/// Implements the `ToFunctionInterface` trait for functions with a variable amount of parameters
macro_rules! impl_to_function_interface {
    ($($xs:ident),*) => {
        /// With mut function context
        impl<Function, Return, $($xs),*> ToFunctionInterface<(&mut FunctionContext, $(&$xs),*), Return> for Function
        where
            Function: Fn(&mut FunctionContext, $(&$xs),*) -> Return + 'static,
            Return: ValidReturnType,
            $($xs: ObjectPayload),*
        {
            fn to_function_interface(&'static self) -> Box<dyn NormalizedFunctionInterface> {
                Box::new(move |ctx: &mut FunctionContext, objects: &[ObjectRef]| {
                    #[allow(unused_variables, unused_mut)]
                    let mut iter = objects.iter();
                    (self)(
                        ctx,
                        $(
                            iter.next().expect("Expected next parameter").downcast_payload::<$xs>().expect("Invalid type")
                        ),*
                    ).into_result(ctx)
                })
            }
        }

        /// With non-mut function context
        impl<Function, Return, $($xs),*> ToFunctionInterface<(&FunctionContext, $(&$xs),*), Return> for Function
        where
            Function: Fn(&FunctionContext, $(&$xs),*) -> Return + 'static,
            Return: ValidReturnType,
            $($xs: ObjectPayload),*
        {
            fn to_function_interface(&'static self) -> Box<dyn NormalizedFunctionInterface> {
                Box::new(move |ctx: &mut FunctionContext, objects: &[ObjectRef]| {
                    #[allow(unused_variables, unused_mut)]
                    let mut iter = objects.iter();
                    (self)(
                        ctx,
                        $(
                            iter.next().expect("Expected next parameter").downcast_payload::<$xs>().expect("Invalid type")
                        ),*
                    ).into_result(ctx)
                })
            }
        }

        /// Without function context
        #[allow(unused_parens)]
        impl<Function, Return, $($xs),*> ToFunctionInterface<($(&$xs),*), Return> for Function
        where
            Function: Fn($(&$xs),*) -> Return + 'static,
            Return: ValidReturnType,
            $($xs: ObjectPayload),*
        {
            fn to_function_interface(&'static self) -> Box<dyn NormalizedFunctionInterface> {
                Box::new(move |ctx: &mut FunctionContext, objects: &[ObjectRef]| {
                    #[allow(unused_variables, unused_mut)]
                    let mut iter = objects.iter();
                    (self)(
                        $(
                            iter.next().expect("Expected next parameter").downcast_payload::<$xs>().expect("Invalid type")
                        ),*
                    ).into_result(ctx)
                })
            }
        }

    };
}

// Function interface may take up to eight types
impl_to_function_interface!();
impl_to_function_interface!(A);
impl_to_function_interface!(A, B);
impl_to_function_interface!(A, B, C);
impl_to_function_interface!(A, B, C, D);
impl_to_function_interface!(A, B, C, D, E);
impl_to_function_interface!(A, B, C, D, E, F);
impl_to_function_interface!(A, B, C, D, E, F, G);
impl_to_function_interface!(A, B, C, D, E, F, G, H);
