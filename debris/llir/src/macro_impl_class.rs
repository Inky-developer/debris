#[macro_export]
macro_rules! impl_class {
    ($ty:ty, $debris_ty:path, {$($ident:tt => $fn:expr),*}) => {
        impl $crate::objects::obj_class::HasClass for $ty {
            fn create_properties(#[allow(unused_variables)] ctx: &crate::type_context::TypeContext) -> $crate::ObjectProperties {
                #[allow(unused_mut)]
                let mut properties = $crate::ObjectProperties::default();
                $({
                    use $crate::ValidPayload;
                    use $crate::function_interface::ToFunctionInterface;

                    let function = $crate::function_interface::DebrisFunctionInterface::from(
                        ($fn).to_normalized_function(),
                    );
                    let obj_function = $crate::objects::obj_function::ObjFunction::new(
                        concat!(stringify!($ty), ".", impl_class!(get_fn_name $ident)),
                        ::std::rc::Rc::new(function)
                    ).into_object(ctx);
                    properties.insert(
                        impl_class!(get_ident $ident),
                        obj_function,
                    );
                })*

                properties
            }

            fn static_class(ctx: &$crate::type_context::TypeContext) -> $crate::class::ClassRef {
                if let Some(class) = ctx.get::<$ty>() {
                    return class;
                }

                let mut class = $crate::class::Class::new_empty($debris_ty.into());
                class.properties = Self::create_properties(ctx).into();
                let class_ref = ::std::rc::Rc::new(class);
                ctx.insert::<Self>(class_ref.clone());
                class_ref
            }
        }
    };
    (get_ident $ident:literal) => {$ident.into()};
    (get_ident $ident:tt) => {::debris_common::SpecialIdent::$ident.into()};

    (get_fn_name $value:literal) => {$value};
    (get_fn_name $value:tt) => {concat!("<", stringify!($value), ">")};
}
