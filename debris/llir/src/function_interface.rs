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

use debris_error::{LangError, LangErrorKind, LangResult, Result};

use crate::{
    class::ClassRef,
    objects::{
        obj_bool_static::ObjStaticBool, obj_function::FunctionContext,
        obj_int_static::ObjStaticInt, obj_null::ObjNull, obj_string::ObjString,
    },
    type_context::TypeContext,
    ObjectPayload, ObjectRef, ValidPayload,
};

/// The common type for working with callbacks
pub struct DebrisFunctionInterface(NormalizedFunction);

impl DebrisFunctionInterface {
    /// Calls this interface and returns the result and a vec of the generated nodes
    pub(crate) fn call(&self, function_ctx: &mut FunctionContext) -> Result<ObjectRef> {
        let raw_value = self.call_raw(function_ctx);
        self.handle_raw_result(function_ctx, raw_value)
            .map_err(|kind| LangError::new(kind, function_ctx.span).into())
    }

    pub(crate) fn call_raw(
        &self,
        function_ctx: &mut FunctionContext,
    ) -> Option<LangResult<ObjectRef>> {
        (self.0.inner_fn)(function_ctx)
    }

    pub fn handle_raw_result(
        &self,
        function_ctx: &FunctionContext,
        value: Option<LangResult<ObjectRef>>,
    ) -> LangResult<ObjectRef> {
        match value {
            Some(val) => val,
            None => Err(LangErrorKind::UnexpectedOverload {
                expected: (self.0.required_parameter_fn)(function_ctx.type_ctx).map_or_else(
                    || vec![vec!["<Any>".to_string()]],
                    |overloads| {
                        overloads
                            .into_iter()
                            .map(|params| {
                                params.into_iter().map(|param| param.to_string()).collect()
                            })
                            .collect()
                    },
                ),
                parameters: function_ctx
                    .parameters
                    .iter()
                    .map(|param| param.class.to_string())
                    .collect(),
            }),
        }
    }
}

impl From<NormalizedFunction> for DebrisFunctionInterface {
    fn from(value: NormalizedFunction) -> Self {
        DebrisFunctionInterface(value)
    }
}

type NormalizedFnSig = dyn Fn(&mut FunctionContext) -> Option<LangResult<ObjectRef>>;

pub struct NormalizedFunction {
    /// Calls this function if the parameters are valid and returns None otherwise
    inner_fn: Box<NormalizedFnSig>,

    /// Returns the parameters that this function wants. If None any parameters are assumed to be valid
    required_parameter_fn: Box<dyn Fn(&TypeContext) -> Option<Vec<Vec<ClassRef>>>>,
}

pub fn make_overload(functions: Vec<NormalizedFunction>) -> NormalizedFunction {
    let functions = Rc::new(functions);
    let functions_2 = Rc::clone(&functions);
    NormalizedFunction {
        inner_fn: Box::new(move |ctx| {
            for function in functions.as_ref() {
                if let Some(result) = (function.inner_fn)(ctx) {
                    return Some(result);
                }
            }
            None
        }),
        required_parameter_fn: Box::new(move |type_ctx| {
            let mut values = Vec::with_capacity(functions_2.len());
            for function in functions_2.as_ref() {
                values.extend(
                    (function.required_parameter_fn)(type_ctx).unwrap_or_else(|| vec![vec![]]),
                );
            }
            Some(values)
        }),
    }
}

/// Trait used for converting any valid return value into a LangResult<ObjectRef>
pub trait ValidReturnType {
    fn into_result(self, ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>>;
}

impl<T> ValidReturnType for T
where
    T: ObjectPayload,
{
    fn into_result(self, ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
        Some(Ok(self.into_object(ctx.type_ctx)))
    }
}

impl<T> ValidReturnType for LangResult<T>
where
    T: ObjectPayload,
{
    fn into_result(self, ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
        Some(self.map(|value| value.into_object(ctx.type_ctx)))
    }
}

impl<T> ValidReturnType for Option<LangResult<T>>
where
    T: ObjectPayload,
{
    fn into_result(self, ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
        self.map(|res| res.map(|val| val.into_object(ctx.type_ctx)))
    }
}

impl ValidReturnType for Option<LangResult<ObjectRef>> {
    fn into_result(self, _ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
        self
    }
}

impl ValidReturnType for LangResult<ObjectRef> {
    fn into_result(self, _ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
        Some(self)
    }
}

impl ValidReturnType for ObjectRef {
    fn into_result(self, _ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
        Some(Ok(self))
    }
}

/// Maps a valid return type from `$from` to `$to`
macro_rules! impl_map_valid_return_type {
    ($from:ty, $to:ty) => {
        impl ValidReturnType for $from {
            fn into_result(self, ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
                Some(Ok(
                    <$to as From<$from>>::from(self).into_object(&ctx.type_ctx)
                ))
            }
        }

        impl ValidReturnType for LangResult<$from> {
            fn into_result(self, ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
                Some(match self {
                    Ok(value) => Ok(<$to as From<$from>>::from(value).into_object(&ctx.type_ctx)),
                    Err(err) => Err(err),
                })
            }
        }

        impl ValidReturnType for Option<$from> {
            fn into_result(self, ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
                self.map(|value| Ok(<$to as From<$from>>::from(value).into_object(&ctx.type_ctx)))
            }
        }

        impl ValidReturnType for Option<LangResult<$from>> {
            fn into_result(self, ctx: &mut FunctionContext) -> Option<LangResult<ObjectRef>> {
                self.map(|res| match res {
                    Ok(value) => Ok(<$to as From<$from>>::from(value).into_object(&ctx.type_ctx)),
                    Err(err) => Err(err),
                })
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
pub trait ToFunctionInterface<Params, Return>: 'static {
    fn to_normalized_function(self) -> NormalizedFunction;
}

impl ToFunctionInterface<(), ()> for NormalizedFunction {
    fn to_normalized_function(self) -> NormalizedFunction {
        self
    }
}

macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + count!($($xs)*));
}

/// Implements the `ToFunctionInterface` trait for functions with a variable amount of parameters
macro_rules! impl_to_function_interface {
    ($($xs:ident),*) => {
        /// With mut function context
        impl<Function, Return, $($xs),*> ToFunctionInterface<(&mut FunctionContext<'_>, $(&$xs),*), Return> for Function
        where
            Function: Fn(&mut FunctionContext, $(&$xs),*) -> Return + 'static,
            Return: ValidReturnType,
            $($xs: ObjectPayload),*
        {
            impl_to_function_interface!(impl_inner, [$($xs),*], ,);
        }

        /// With non-mut function context
        impl<Function, Return, $($xs),*> ToFunctionInterface<(&FunctionContext<'_>, $(&$xs),*), Return> for Function
        where
            Function: Fn(&FunctionContext, $(&$xs),*) -> Return + 'static,
            Return: ValidReturnType,
            $($xs: ObjectPayload),*
        {
            impl_to_function_interface!(impl_inner, [$($xs),*], ,);
        }

        /// Without function context
        #[allow(unused_parens)]
        impl<Function, Return, $($xs),*> ToFunctionInterface<($(&$xs),*), Return> for Function
        where
            Function: Fn($(&$xs),*) -> Return + 'static,
            Return: ValidReturnType,
            $($xs: ObjectPayload),*
        {
            impl_to_function_interface!(impl_inner, [$($xs),*],);
        }

    };

    // TODO: Make overloading less inefficient
    // (Right now an error message is constructed for each not matching overload)
    (impl_inner, [$($xs:ident),*], $($use_ctx:tt)?) => {
        fn to_normalized_function(self) -> NormalizedFunction {
            let inner_fn = Box::new(move |ctx: &mut FunctionContext| {
                #[allow(unused_variables, unused_mut)]
                let mut iter = ctx.parameters.iter();

                let temp_slice: [ObjectRef; count!($($xs)*)] = [$(impl_to_function_interface!(maybe_promote, ctx, iter.next(), $xs)),*];
                #[allow(unused_variables, unused_mut)]
                let mut temp_values = temp_slice.iter();

                (self)($(ctx $use_ctx)? $(
                    impl_to_function_interface!(verify_type, temp_values.next(), $xs)?
                ),*).into_result(ctx)

            });

            NormalizedFunction {
                inner_fn,
                required_parameter_fn: Box::new(#[allow(unused_variables)] |type_ctx| Some(vec![vec![$($xs::class(type_ctx)),*]]))
            }
        }
    };

    (maybe_promote, $ctx:expr, $val:expr, $typ:ident) => {{
        let value = $val?;
        if value.downcast_payload::<$typ>().is_some() {
            value.clone()
        } else {
            match $ctx.promote_obj(value.clone(), $crate::objects::obj_class::ObjClass::new($typ::class($ctx.type_ctx)).into_object($ctx.type_ctx)) {
                Some(Ok(value)) => value,
                other => return other,
            }
        }
    }};

    (verify_type, $val:expr, $typ:ident) => {
        $val.and_then(|value| value.downcast_payload::<$typ>())
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

/// This trait allows downcasting an entire array of objects into a tuple of concrete payloads
pub trait DowncastArray<'a, T> {
    fn downcast_array(&'a self) -> Option<T>;
}

/// This macro works much like `impl_to_function_interface` by implementing the array downcast trait for tuples of variable lengths
macro_rules! impl_downcast_array {
    ($($xs:ident),*) => {
        impl<'a, $($xs),*> DowncastArray<'a, ($(&'a $xs),*,)> for [ObjectRef]
        where
            $(
                $xs: ObjectPayload
            ),*
        {
            #[allow(non_snake_case)]
            fn downcast_array(&'a self) -> Option<($(&'a $xs),*,)> {
                match &self {
                    [$($xs),*] => {
                        $(
                            let $xs = $xs.downcast_payload()?;
                        )*
                        Some(($($xs),*,))
                    }
                    _ => None,
                }
            }
        }
    };
}

impl_downcast_array!(A);
impl_downcast_array!(A, B);
impl_downcast_array!(A, B, C);
impl_downcast_array!(A, B, C, D);
impl_downcast_array!(A, B, C, D, E);
impl_downcast_array!(A, B, C, D, E, F);
impl_downcast_array!(A, B, C, D, E, F, G);
impl_downcast_array!(A, B, C, D, E, F, G, H);
impl_downcast_array!(A, B, C, D, E, F, G, H, I);
impl_downcast_array!(A, B, C, D, E, F, G, H, I, J);
