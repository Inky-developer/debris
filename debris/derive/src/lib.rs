use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Result, parse_macro_input, parse_quote, spanned::Spanned, Attribute, Error, ExprCall,
    Ident, ImplItem, ImplItemMethod, ItemImpl, ReturnType, Type, TypeBareFn,
};

mod utils;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
enum MethodIdent {
    Normal(Ident),
    Special(Ident),
}

impl MethodIdent {
    fn ident(&self) -> &Ident {
        match self {
            MethodIdent::Normal(s) => &s,
            MethodIdent::Special(s) => &s,
        }
    }

    fn as_debris_ident(&self) -> ExprCall {
        match self {
            MethodIdent::Normal(s) => {
                parse_quote! {debris_common::Ident::new(stringify!(#s))}
            }
            MethodIdent::Special(s) => {
                let s = Ident::new(&utils::titlecase(&s.to_string()), s.span());
                parse_quote! {debris_common::Ident::Special(debris_common::SpecialIdent::#s)}
            }
        }
    }
}

/// parses something like #[method((Int, Int) -> Int)]
/// Or #[special(Add, (Int, Int) -> Int)]
#[derive(Debug)]
struct MethodMetadata {
    arguments: Vec<Ident>,
    return_type: Ident,
    method_kind: MethodIdent,
    renamed_method: Ident,
}

impl MethodMetadata {
    fn from_attribute(
        attribute: &Attribute,
        method_kind: MethodIdent,
        renamed_method: Ident,
    ) -> Result<Self> {
        let meta: TypeBareFn = attribute.parse_args().unwrap();
        Ok(MethodMetadata {
            arguments: meta
                .inputs
                .iter()
                .map(|arg| {
                    let ts = arg.ty.to_token_stream();
                    Ident::new(&ts.to_string(), ts.span())
                })
                .collect(),
            method_kind,
            renamed_method,
            return_type: match meta.output {
                ReturnType::Default => {
                    return Err(Error::new(attribute.span(), "No output type specified!"))
                }
                ReturnType::Type(_, typ) => {
                    let ty = typ.to_token_stream();
                    Ident::new(&ty.to_string(), ty.span())
                }
            },
        })
    }
}

#[proc_macro_attribute]
pub fn template(args: TokenStream, input: TokenStream) -> TokenStream {
    assert!(args.is_empty());

    let input = parse_macro_input!(input as ItemImpl);

    let result = handle_item_impl(input);

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

fn handle_item_impl(mut input: ItemImpl) -> Result<ItemImpl> {
    // A map of all the methods, including potential overloads
    let mut methods: HashMap<MethodIdent, Vec<MethodMetadata>> = HashMap::new();

    let impl_items = input
        .items
        .iter_mut()
        .map(|item| match item {
            ImplItem::Method(method) => handle_method(method, &methods).map(|(func, metadata)| {
                if let Some(val) = metadata {
                    methods
                        .entry(val.method_kind.clone())
                        .or_default()
                        .push(val);
                }
                ImplItem::Method(func)
            }),
            other => Ok(other.clone()),
        })
        .collect::<Result<Vec<_>>>()?;

    input.items = impl_items;

    let impl_type = &input.self_ty;
    let function_wrappers = methods
        .iter()
        .map(|(method_name, overloads)| quote_overloads(method_name, overloads, impl_type))
        .collect::<Vec<_>>();

    // for f in function_wrappers.iter() {
    //     println!("{}", f);
    // }

    let init_template_function = parse_quote! {
        pub fn init_template(ctx: &::debris_core::CompileContext, template: &::debris_core::objects::TypeRef) {
            use ::debris_core::ObjectPayload;
            #(#function_wrappers)*
        }
    };
    input.items.push(init_template_function);

    // let template_function = parse_quote! {
    //     pub fn template() -> TypeRef {
    //         ObjectType::new_ref(
    //             Type::Template(Box::new(Type::Int)),
    //             ObjectProperties::default(),
    //             None,
    //         )
    //     }
    // };
    // input.items.push(template_function);

    Ok(input)
}

/// Adds a new function, containing all overloads
fn quote_overloads(
    method_name: &MethodIdent,
    overloads: &[MethodMetadata],
    impl_type: &Type,
) -> proc_macro2::TokenStream {
    let debris_ident = method_name.as_debris_ident();

    let mut wrapper_functions = vec![];
    let quoted_overloads = overloads.iter().enumerate().map(|(index, meta)| {
        let index = syn::Index::from(index);
        let wrapper_name = format_ident!("wrap_{}_{}", method_name.ident(), index);
        wrapper_functions.push(wrapper_name.clone());
        let new_method_name = &meta.renamed_method;

        let quoted_parameters = meta.arguments.iter().enumerate().map(|(param_index, _typ)|{
            let index = syn::Index::from(param_index);
            quote! {
                parameters[#index].downcast_payload().expect("Overload called with invalid parameter type")
            }
        });
        // Here must not be an empty line, otherwise rustfmt crashes lol
        quote! {
            fn #wrapper_name(
                ctx: &mut ::debris_core::objects::FunctionContext,
                parameters: ::std::vec::Vec<::debris_core::ObjectRef>
            ) -> ::debris_core::error::LangResult<::debris_core::ObjectRef> {
                #impl_type::#new_method_name(
                    ctx,
                    #(#quoted_parameters),*
                ).map(|result| result.into_object(ctx.compile_context))
            }
        }
    });

    let function_signatures = overloads.iter().map(|overload| {
        let parameters = &overload.arguments;
        let return_type = &overload.return_type;
        quote! {
            debris_core::objects::FunctionSignature::new(
                vec![#(::debris_type::Type::#parameters),*],
                ::debris_type::Type::#return_type
            )
        }
    });

    quote! {
        #(
            #quoted_overloads
        )*

        template.set_property(
            #debris_ident,
            ::debris_core::objects::ObjectFunction::new(
                ::debris_core::objects::FunctionSignatureMap::new(vec![#(
                        (
                            #function_signatures,
                            ::debris_core::objects::CallbackFunction(#wrapper_functions)
                        )
                    ),*
                ]),
            ).into_object(ctx)
        );
    }
}

fn handle_method(
    method: &mut ImplItemMethod,
    methods: &HashMap<MethodIdent, Vec<MethodMetadata>>,
) -> Result<(ImplItemMethod, Option<MethodMetadata>)> {
    let (special, attrs): (Vec<Attribute>, Vec<Attribute>) = method
        .attrs
        .iter()
        .cloned()
        .partition(|attr| attr.path.is_ident("method") | attr.path.is_ident("special"));

    if special.len() > 1 {
        return Err(Error::new(
            special[1].span(),
            "Expected at most one implementation attribute",
        ));
    }

    let special = if special.is_empty() {
        None
    } else {
        let method_kind = if special[0].path.is_ident("method") {
            MethodIdent::Normal(method.sig.ident.clone())
        } else {
            MethodIdent::Special(method.sig.ident.clone())
        };

        let method_index = methods.get(&method_kind).map_or(0, |vector| vector.len());
        method.sig.ident = Ident::new(
            &format!("__{}_{}", method.sig.ident, method_index),
            method.sig.ident.span(),
        );
        let metadata =
            MethodMetadata::from_attribute(&special[0], method_kind, method.sig.ident.clone())?;

        // rename the method to allow multiple methods of the same name

        Some(metadata)
    };

    Ok((
        ImplItemMethod {
            attrs,
            vis: method.vis.clone(),
            defaultness: method.defaultness,
            sig: method.sig.clone(),
            block: method.block.clone(),
        },
        special,
    ))
}
