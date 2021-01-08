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
/// Then, this macro implementes the trait [HasClass](debris_core::objects::HasClass) which returns the associated class
/// of this struct. This class contains all marked methods as `ObjectFunction` which can be called from debris code.
///
/// Since this operation is quite expensive, it is only run once and then cached.
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

#[proc_macro_derive(ObjectCopy)]
pub fn object_copy(input: TokenStream) -> TokenStream {
    let value = parse_macro_input!(input as DeriveInput);
    let ident = &value.ident;
    let result = quote! {
        impl ::debris_core::ObjectCopy for #ident {
            fn object_copy(
            &self,
            ctx: &::debris_core::CompileContext,
            nodes: &mut ::std::vec::Vec<::debris_core::llir::llir_nodes::Node>,
            memory: &mut ::debris_core::memory::MemoryCounter,
        ) -> ::debris_core::ObjectRef {
            use ::debris_core::ValidPayload;
            <Self as std::clone::Clone>::clone(self).into_object(ctx)
        }
        }
    };
    TokenStream::from(result)
}
