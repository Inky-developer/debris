use std::collections::HashMap;

use proc_macro2::Span;
use quote::quote;
use syn::{
    spanned::Spanned, AngleBracketedGenericArguments, AttributeArgs, FnArg, GenericArgument, Ident,
    ImplItem, ImplItemMethod, ItemImpl, NestedMeta, Path, PathArguments, Type,
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
    /// Returns the class of the object
    fn get_debris_type(struct_type: &Type, typ: &Type) -> proc_macro2::TokenStream {
        let is_self = struct_type == typ;

        if is_self {
            quote! {class.clone()}
        } else {
            quote! {
                #typ::class(ctx)
            }
        }
    }

    let wrapped_methods = groups.values().flatten().map(|meta| &meta.method);

    let properties = groups.iter().map(|(method_ident, value)| {
        let properties_key = match method_ident {
            MethodIdent::Normal(key) => quote! {::debris_common::Ident::new(stringify!(#key))},
            MethodIdent::Special(key) => quote! {::debris_common::SpecialIdent::#key.into()},
        };

        let functions = value.iter().map(|method_meta| {
            let return_type = get_debris_type(&struct_type, match &method_meta.return_type {
                ReturnType::NoResult(typ) => typ,
                ReturnType::Result(typ) => typ
            });


            let fn_name = &method_meta.function_name;

            let params = method_meta.parameters.iter().enumerate().map(|(index, param)| {
                quote! {
                    params[#index].downcast_payload().expect(&format!("Expected type {}",  stringify!(#param)))
                }
            });

            let param_types = method_meta.parameters.iter().map(|param| get_debris_type(&struct_type, param));

            let fn_call = if method_meta.return_type.is_result() {
                quote! {
                    #fn_name(
                        ctx,
                        #(#params),*
                    ).map(|result| result.into_object(ctx.compile_context))
                }
            } else {
                quote! {
                    Ok(
                        #fn_name(
                            ctx,
                            #(#params),*
                        ).into_object(ctx.compile_context)
                    )
                }
            };

            quote! {
                (
                    ::debris_core::objects::FunctionSignature::new(
                        vec![#( #param_types ),*],
                        #return_type,
                        ::debris_core::objects::CallbackFunction(|ctx, params| {
                            #fn_call
                        })
                    )
                )
            }
        });

        quote! {
            class.set_property(
                #properties_key,
                ::debris_core::objects::ObjFunction::new(vec![
                    #(
                        #functions
                    ),*
                ]).into_object(ctx)
            )
        }
    });

    quote! {
        impl ::debris_core::objects::HasClass for #struct_type {
            fn class(ctx: &debris_core::CompileContext) -> debris_core::objects::ClassRef {
                use ::debris_core::ObjectPayload;
                use ::debris_core::ValidPayload;

                ctx.type_ctx.get::<Self>().unwrap_or_else(|| {
                    #(
                        #wrapped_methods
                    )*

                    let class: ::std::rc::Rc<_> = ::debris_core::objects::ObjClass::new_empty(#typ).into();
                    ctx.type_ctx.insert::<Self>(class.clone());

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

            let metadata_opt = MethodMetadata::from_method(method, method_ident.clone())?;
            if let Some(metadata) = metadata_opt {
                groups.entry(method_ident).or_default().push(metadata);
            }
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

/// Allowed modifiers for the function context
#[derive(Debug)]
enum FunctionContextType {
    Mutable,
    Immutable,
}

/// Allowed return types
#[derive(Debug)]
enum ReturnType {
    Result(Type),
    NoResult(Type),
}

/// Contains all neccessary metadata for a debris method
#[derive(Debug)]
struct MethodMetadata {
    function_context_type: FunctionContextType,
    parameters: Vec<Type>,
    return_type: ReturnType,
    /// The ident for the debris object
    method_ident: MethodIdent,
    /// The ident for the implementing function
    function_name: Ident,
    /// The method
    method: ImplItemMethod,
}

impl MethodMetadata {
    /// Returns metadata for this method or None if this is not a method for debris
    fn from_method(
        method: &ImplItemMethod,
        method_ident: MethodIdent,
    ) -> syn::Result<Option<Self>> {
        let function_context_type = {
            // The first function argument
            let function_context_param = method.sig.inputs.first().ok_or_else(|| {
                syn::Error::new(method.sig.inputs.span(), "Requieres at least one argument")
            })?;

            let function_context_ty = match function_context_param {
                FnArg::Receiver(_) => {
                    return Err(syn::Error::new(
                        function_context_param.span(),
                        "The first argument must not be self",
                    ))
                }
                FnArg::Typed(pat_typ) => pat_typ.ty.as_ref(),
            };

            match function_context_ty {
                Type::Reference(reference) => match reference.mutability {
                    Some(_) => FunctionContextType::Mutable,
                    None => FunctionContextType::Immutable,
                },
                _ => {
                    return Err(syn::Error::new(
                        function_context_param.span(),
                        "The first argument must be a reference",
                    ))
                }
            }
        };

        let debris_parameters = method
            .sig
            .inputs
            .iter()
            .skip(1)
            .map(|param| match param {
                FnArg::Receiver(_) => unreachable!("Self can only be the first argument"),
                FnArg::Typed(pat_type) => match pat_type.ty.as_ref() {
                    Type::Reference(ty) => {
                        if ty.mutability.is_some() {
                            Err(syn::Error::new(
                                ty.span(),
                                "Expected an immutable reference",
                            ))
                        } else {
                            Ok(ty.elem.as_ref().clone())
                        }
                    }
                    other => Err(syn::Error::new(other.span(), "Expected a reference")),
                },
            })
            .collect::<syn::Result<Vec<_>>>()?;

        // Return type is either in a `LangResult` or free-standing
        let return_type = match &method.sig.output {
            syn::ReturnType::Default => {
                return Err(syn::Error::new(
                    method.sig.output.span(),
                    "Implicite Null is not yet implemented",
                ))
            }
            syn::ReturnType::Type(_, ty) => match ty.as_ref() {
                Type::Path(t_path) => {
                    let path_segments = &t_path.path.segments;

                    if path_segments.len() == 1
                        && path_segments.first().unwrap().ident == "LangResult"
                    {
                        match &path_segments.first().unwrap().arguments {
                            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                colon2_token: _,
                                gt_token: _,
                                lt_token: _,
                                args,
                            }) => {
                                if args.len() != 1 {
                                    return Err(syn::Error::new(
                                        t_path.span(),
                                        "Invalid LangResult",
                                    ));
                                }
                                let arg = args.first().unwrap();
                                match arg {
                                    GenericArgument::Type(typ) => ReturnType::Result(typ.clone()),
                                    _ => {
                                        return Err(syn::Error::new(
                                            t_path.span(),
                                            "Invalid LangResult",
                                        ))
                                    }
                                }
                            }
                            _ => return Err(syn::Error::new(t_path.span(), "Invalid LangResult")),
                        }
                    } else {
                        ReturnType::NoResult(Type::Path(t_path.clone()))
                    }
                }
                other_type => ReturnType::NoResult(other_type.clone()),
            },
        };

        Ok(Some(MethodMetadata {
            function_name: method.sig.ident.clone(),
            method_ident,
            function_context_type,
            parameters: debris_parameters,
            return_type,
            method: method.clone(),
        }))
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

impl ReturnType {
    fn is_result(&self) -> bool {
        matches!(self, ReturnType::Result(_))
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
