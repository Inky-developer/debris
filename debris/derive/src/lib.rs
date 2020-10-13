use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, DeriveInput, ItemImpl};

mod object;
mod utils;

/// Handles an impl block containing methods for debris objects
///
/// # Usage:
///
/// ```ignore
/// #[object(Type::Foo)]
/// impl FooStruct {
///     // mark functions for the object with #[method] or #[special]
///     #[special]
///     fn add(_ctx: &CompileContext, a: &FooStruct, b: &FooStruct) -> FooStruct {
///         (a.value + b.value).into()
///     }
///
///     // If the function can fail (at compile time), return a [LangResult]:
///     #[method]
///     fn might_fail(ctx: &CompileContext, value: &FooStuct) -> LangResult<FooStruct> {
///         Ok(0.into())
///     }
///
///     // The `CompileContext` can also be taken by &mut reference
///     #[method]
///     fn foo(ctx: &mut CompileContext) -> FooStruct {
///         ctx.emit(...);
///         Ok(0.into())
///     }
///     
///     // Overloads are also supported
///     #[special]
///     fn add(_ctx: &CompileContext, argument: &FooStruct) -> FooStruct {...}
/// }
/// ```
///
/// The user could now run this code (note that `a` is of type `Foo`):
/// `let b = a.might_fail(other_a);`
///
/// Note that overloads must have different arguments (which is not checked right now).
///
/// # Implementation
///
/// This proc-macro collects all functions marked as #\[special\] or #\[method\] and removes them from the impl body.
/// Then, this macro adds a method `pub fn class(ctx: &CompileContext) -> Rc<Class>` which returns the associated class
/// of this struct. This class contains all marked methods as `ObjectFunction` which can be called from debris code.
///
/// Since this operations is quite expensive, it is only run once and then cached.
///
/// A limitation of the current system is that it is not possible to have classes that depend on each other.
/// For example, lets say there are two classes: `Foo` and `Bar`.
/// Now, if `Foo` implements `fn foo_bar(_: &FunctionContext, bar: &Bar)` it is not possible for `Bar` to
/// have a method that requires `Foo` as a parameter or return type and would cause a stackoverflow error at runtime.
/// I don't think this is a major limitation since I cannot think of any core type where that would be necessarry,
/// but if that turns out to be a problem, classes could be identified by integer ids instead of `Rc<Class>` which would
/// additionally be cheaper.
#[proc_macro_attribute]
pub fn object(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let result = parse_macro_input!(input as ItemImpl);

    let result = object::handle_object(args, result);

    let output = match result {
        Ok(item_impl) => quote! {
            #item_impl
        },
        Err(e) => {
            let e = e.to_compile_error();
            quote! {
                #e
            }
        }
    };

    TokenStream::from(output)
}

/// Implements [ObjectPayload](debris_core::ObjectPayload) for that type
///
/// The implementation looks roughly like this:
///
/// ```ignore
/// impl ObjectPayload for {DerivedObject} {
///    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
///        DebrisObject::new_ref(Self::class(&ctx), self)
///    }
///
///    fn eq(&self, other: &ObjectRef) -> bool {
///        other
///            .downcast_payload::<Self>()
///            .map_or(false, |value| value == self)
///    }
///}
/// ```
///
/// Note that this derive macro requires `PartialEq` and
/// a `class` method. The `class` method should usually be implemented
/// via the #[object(type)] attribute macro.
#[proc_macro_derive(ObjectPayload)]
pub fn object_payload(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let item_ident = input.ident;

    let output = quote! {
        impl ::debris_core::ObjectPayload for #item_ident {
            fn into_object(self, ctx: &::debris_core::CompileContext) -> ::debris_core::ObjectRef {
                ::debris_core::DebrisObject::new_ref(Self::class(&ctx), self)
            }

            fn eq(&self, other: &::debris_core::ObjectRef) -> bool {
                other
                    .downcast_payload::<Self>()
                    .map_or(false, |value| value == self)
            }
        }
    };

    output.into()
}
