use std::collections::HashMap;

use proc_macro2::Span;
use quote::quote;
use syn::{
    spanned::Spanned, AttributeArgs, Ident, ImplItem, ImplItemMethod, ItemImpl, NestedMeta, Path,
    Type,
};

use crate::utils::camelcase;

type Groups = HashMap<MethodIdent, Vec<MethodMetadata>>;

pub(super) fn handle_object(
    args: AttributeArgs,
    mut input: ItemImpl,
) -> syn::Result<proc_macro2::TokenStream> {
    let cfg = MacroConfig::from_args(args)?;
    let struct_type = input.self_ty.as_ref().clone();
    let groups = group_methods(&mut input)?;

    // for (ident, vec) in &groups {
    //     println!("{:?}: {}", ident, vec.len());
    // }

    let class_impl = creat_trait_impl(groups, &cfg, struct_type);

    let result = quote! {
        #input

        #class_impl
    };
    Ok(result)
}

/// Creates the `class` method which contains all the methods that were collected
fn creat_trait_impl(
    groups: Groups,
    MacroConfig { typ }: &MacroConfig,
    struct_type: Type,
) -> proc_macro2::TokenStream {
    let wrapped_methods = groups.values().flatten().map(|meta| &meta.method);

    let properties = groups.iter().map(|(method_ident, value)| {
        let properties_key = match method_ident {
            MethodIdent::Normal(key) => quote! {::debris_common::Ident::new(stringify!(#key))},
            MethodIdent::Special(key) => quote! {::debris_common::SpecialIdent::#key.into()},
        };

        let functions = value.iter().map(|method_meta| {
            let fn_name = &method_meta.function_name;

            quote! {
                (
                    get_function_overload(ctx, &#fn_name)
                )
            }
        });

        let fn_debug_name = method_ident.to_string();
        quote! {
            class.set_property(
                #properties_key,
                ::debris_core::objects::obj_function::ObjFunction::new(
                    ctx,
                    #fn_debug_name,
                    vec![
                        #(
                            #functions
                        ),*
                    ]
                ).into_object(ctx)
            )
        }
    });

    quote! {
        impl ::debris_core::objects::obj_class::HasClass for #struct_type {
            fn class(ctx: &debris_core::CompileContext) -> debris_core::class::ClassRef {
                use ::debris_core::ObjectPayload;
                use ::debris_core::ValidPayload;
                use ::debris_core::function_interface::ToFunctionInterface;
                use ::debris_core::function_interface::ValidReturnType;

                fn get_function_overload<F, Params, Return>(ctx: &::debris_core::CompileContext, function: &'static F) ->
                    ::debris_core::objects::obj_function::FunctionOverload
                where
                    F: ToFunctionInterface<Params, Return>,
                    Return: ValidReturnType
                {
                    ::debris_core::objects::obj_function::FunctionOverload::new(
                        ::debris_core::objects::obj_function::FunctionSignature::new(
                            F::query_parameters(ctx),
                            match F::query_return(ctx) {
                                Some(ty) => ty,
                                None => panic!("Cannot create function which does not specify the return type: {}", std::any::type_name::<F>())
                            }
                        ).into(),
                        function.to_function_interface().into()
                    )
                }

                ctx.type_ctx().get::<Self>().unwrap_or_else(|| {
                    #(
                        #wrapped_methods
                    )*

                    let class: ::std::rc::Rc<_> = ::debris_core::class::Class::new_empty(#typ.into()).into();
                    ctx.type_ctx().insert::<Self>(class.clone());

                    #(
                        #properties;
                    )*

                    class
                })
            }
        }
    }
}

/// Groups different methods by their identifier
///
/// Overwrites are put into the same group.
fn group_methods(item_impl: &mut ItemImpl) -> syn::Result<Groups> {
    let mut groups: Groups = HashMap::new();

    // Contains every item of the impl that should be removed by this macro
    let mut methods_to_remove = Vec::new();
    for (item_index, item) in item_impl.items.iter_mut().enumerate() {
        if let ImplItem::Method(method) = item {
            // If the method has an attribute (One of #[method], #[special]), add it to the groups
            // and change the name of the method to something else, so that the user
            // can declare multiple methods with the same name

            let interesting_attributes = method
                .attrs
                .iter()
                .enumerate()
                .filter_map(|(index, item)| {
                    item.parse_meta()
                        .ok()
                        .and_then(|meta| {
                            meta.path().get_ident().and_then(|ident| {
                                match ident.to_string().as_str() {
                                    "method" => Some(MethodIdent::Normal(Ident::new(
                                        &method.sig.ident.to_string(),
                                        method.sig.ident.span(),
                                    ))),
                                    "special" => Some(MethodIdent::Special(Ident::new(
                                        &camelcase(&method.sig.ident.to_string()),
                                        method.sig.ident.span(),
                                    ))),
                                    _ => None,
                                }
                            })
                        })
                        .map(|ident| (index, ident))
                })
                .collect::<Vec<_>>();

            let (index, method_ident) = match interesting_attributes.len() {
                // no attributes - normal rust method
                0 => continue,
                // one attribute - normal debris method
                1 => interesting_attributes.into_iter().next().unwrap(),
                // More than one attribute - error
                _ => {
                    return Err(syn::Error::new(
                        method.span(),
                        "Expected at most one of #[method], #[special] but got more",
                    ))
                }
            };

            methods_to_remove.push(item_index);
            method.attrs.remove(index);

            let function_idx = groups.get(&method_ident).map_or(0, |vec| vec.len());
            let function_new_name = Ident::new(
                &format!(
                    "__debris_derive_method_{}_{}_{}",
                    method_ident.prefix(),
                    method.sig.ident,
                    function_idx
                ),
                method.sig.ident.span(),
            );

            // rename the function
            method.sig.ident = function_new_name.clone();

            let metadata = MethodMetadata::from_method(method, method_ident.clone());
            groups.entry(method_ident).or_default().push(metadata);
        }
    }

    // Since these are indices, removing a single index shifts all higher indices.
    // To avoid trouble with that, sort the indices from highest to lowest
    methods_to_remove.reverse();
    // This operation should maybe sped up
    for index in methods_to_remove {
        item_impl.items.remove(index);
    }

    Ok(groups)
}

/// A identifier for a debris method
#[derive(Debug, Hash, Eq, PartialEq, Clone)]
enum MethodIdent {
    Normal(Ident),
    Special(Ident),
}

impl std::fmt::Display for MethodIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ident = match self {
            MethodIdent::Normal(ident) => ident,
            MethodIdent::Special(ident) => ident,
        };
        ident.fmt(f)
    }
}

/// Contains all neccessary metadata for a debris method
#[derive(Debug)]
struct MethodMetadata {
    /// The ident for the debris object
    method_ident: MethodIdent,
    /// The ident for the implementing function
    function_name: Ident,
    /// The method
    method: ImplItemMethod,
}

impl MethodMetadata {
    /// Returns metadata for this method or None if this is not a method for debris
    fn from_method(method: &ImplItemMethod, method_ident: MethodIdent) -> Self {
        MethodMetadata {
            function_name: method.sig.ident.clone(),
            method_ident,
            method: method.clone(),
        }
    }
}

impl MethodIdent {
    /// The prefix used to rename methods
    fn prefix(&self) -> &'static str {
        match self {
            MethodIdent::Normal(_) => "normal",
            MethodIdent::Special(_) => "special",
        }
    }
}

/// Various parameters for this macro
#[derive(Debug)]
struct MacroConfig {
    typ: Path,
}

impl MacroConfig {
    fn from_args(args: AttributeArgs) -> syn::Result<Self> {
        let path = match args.as_slice() {
            [_first, second, ..] => {
                return Err(syn::Error::new(
                    second.span(),
                    "Expected at most one parameter to this macro",
                ))
            }
            [] => {
                return Err(syn::Error::new(
                    Span::call_site(),
                    "Expected at least one parameter to this macro",
                ))
            }
            [first] => match first {
                NestedMeta::Lit(_val) => {
                    return Err(syn::Error::new(
                        first.span(),
                        "Expected a path, not a literal",
                    ))
                }
                NestedMeta::Meta(meta) => meta.path().clone(),
            },
        };

        Ok(MacroConfig { typ: path })
    }
}
