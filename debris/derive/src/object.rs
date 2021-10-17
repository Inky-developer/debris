use std::collections::HashMap;

use proc_macro2::Span;
use quote::{quote, ToTokens};
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
    let name = struct_type.to_token_stream().to_string();

    let properties = groups.iter().map(|(method_ident, value)| {
        let properties_key = match method_ident {
            MethodIdent::Normal(key) => quote! {::debris_common::Ident::new(stringify!(#key))},
            MethodIdent::Special(key) => quote! {::debris_common::SpecialIdent::#key.into()},
        };

        let fn_debug_name = format!("{}.{}", name, method_ident.to_string());
        let function = value.first().map(|method_meta| {
            let fn_name = &method_meta.function_name;

            quote! {
                {
                    let function = ::debris_llir::function_interface::DebrisFunctionInterface::from(#fn_name.to_function_interface());
                    ::debris_llir::objects::obj_function::ObjFunction::new(#fn_debug_name, ::std::rc::Rc::new(function))
                }
            }
        });

        quote! {
            class.set_property(
                #properties_key,
                #function.into_object(ctx)
            )
        }
    });

    quote! {
        impl ::debris_llir::objects::obj_class::HasClass for #struct_type {
            fn class(ctx: &::debris_llir::type_context::TypeContext) -> debris_llir::class::ClassRef {
                use ::debris_llir::ObjectPayload;
                use ::debris_llir::ValidPayload;
                use ::debris_llir::function_interface::ToFunctionInterface;
                use ::debris_llir::function_interface::ValidReturnType;

                ctx.get::<Self>().unwrap_or_else(|| {
                    #(
                        #wrapped_methods
                    )*

                    let class: ::std::rc::Rc<_> = ::debris_llir::class::Class::new_empty(#typ.into()).into();
                    ctx.insert::<Self>(class.clone());

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

            let metadata = MethodMetadata::from_method(method);
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
    /// The ident for the implementing function
    function_name: Ident,
    /// The method
    method: ImplItemMethod,
}

impl MethodMetadata {
    /// Returns metadata for this method or None if this is not a method for debris
    fn from_method(method: &ImplItemMethod) -> Self {
        MethodMetadata {
            function_name: method.sig.ident.clone(),
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
