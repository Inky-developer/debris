use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Result, parse_macro_input, parse_quote, spanned::Spanned, Attribute, Error, ExprCall,
    Ident, ImplItem, ImplItemMethod, ItemImpl, ReturnType, TypeBareFn,
};

mod utils;

#[derive(Debug)]
enum MethodKind {
    Normal(Ident),
    Special(Ident),
}

impl MethodKind {
    fn ident(&self) -> &Ident {
        match self {
            MethodKind::Normal(s) => &s,
            MethodKind::Special(s) => &s,
        }
    }

    fn as_debris_ident(&self) -> ExprCall {
        match self {
            MethodKind::Normal(s) => {
                parse_quote! {debris_common::Ident::new(stringify!(#s))}
            }
            MethodKind::Special(s) => {
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
    method_kind: MethodKind,
}

impl MethodMetadata {
    fn from_attribute(attribute: &Attribute, method_kind: MethodKind) -> Result<Self> {
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
    let mut methods = vec![];
    let impl_items = input
        .items
        .iter()
        .map(|item| match item {
            ImplItem::Method(method) => handle_method(method).map(|(func, metadata)| {
                if let Some(val) = metadata {
                    methods.push(val);
                }

                ImplItem::Method(func)
            }),
            other @ _ => Ok(other.clone()),
        })
        .collect::<Result<Vec<_>>>()?;

    input.items = impl_items;

    // I hate this shit, but it seems like the best workaround
    let crate_name = env!("CARGO_PKG_NAME");
    let core_name = Ident::new(
        match crate_name {
            "debris-core" => "crate",
            _ => "debris_core",
        },
        Span::call_site(),
    );

    let impl_type = &input.self_ty;
    let function_wrappers: Vec<_> = methods
        .iter()
        .map(|metadata| {
            let MethodMetadata {
                arguments,
                method_kind,
                return_type,
            } = metadata;
            let method_name = method_kind.ident();
            let debris_ident = method_kind.as_debris_ident();
            // let parameter_count = (0..arguments.len()).map(syn::Index::from);
            let wrapper_name = format_ident!("wrap_{}", method_name);

            let quoted_parameters = arguments.iter().enumerate().map(|(index, args_ident)| {
                let index = syn::Index::from(index);
                quote! {
                    parameters[#index].downcast_payload().ok_or_else(|| #core_name::error::LangErrorKind::UnexpectedType{
                        expected: debris_type::Type::#args_ident,
                        got: parameters[#index].typ.clone()
                    })?
                }
            });

            quote! {
                fn #wrapper_name(
                    ctx: &mut #core_name::objects::FunctionContext,
                    parameters: ::std::vec::Vec<#core_name::ObjectRef>
                ) -> #core_name::error::LangResult<#core_name::ObjectRef> {
                    #impl_type::#method_name(
                        ctx,
                        #(#quoted_parameters),*
                    ).map(|result| result.into_object(ctx.compile_context))
                }

                template.set_property(
                    #debris_ident,
                    #core_name::objects::ObjectFunction::new(
                        vec![#(::debris_type::Type::#arguments),*],
                        ::debris_type::Type::#return_type,
                        #core_name::objects::CallbackFunction(#wrapper_name)
                    ).into_object(ctx)
                );
            }
        })
        .collect();

    let init_template_function = parse_quote! {
        pub fn init_template(ctx: &#core_name::CompileContext, template: &#core_name::objects::TypeRef) {
            use #core_name::ObjectPayload;
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

fn handle_method(method: &ImplItemMethod) -> Result<(ImplItemMethod, Option<MethodMetadata>)> {
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

    let special = if special.len() == 0 {
        None
    } else {
        let method_kind = if special[0].path.is_ident("method") {
            MethodKind::Normal(method.sig.ident.clone())
        } else {
            MethodKind::Special(method.sig.ident.clone())
        };
        let metadata = MethodMetadata::from_attribute(&special[0], method_kind)?;
        Some(metadata)
    };

    Ok((
        ImplItemMethod {
            attrs: attrs,
            block: method.block.clone(),
            defaultness: method.defaultness,
            vis: method.vis.clone(),
            sig: method.sig.clone(),
        },
        special,
    ))
}
