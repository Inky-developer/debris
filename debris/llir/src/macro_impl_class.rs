#[macro_export]
macro_rules! impl_class {
    ($ty:ty, $debris_ty:path, {$($ident:tt => $fn:expr),*}) => {
        impl $crate::objects::obj_class::HasClass for $ty {
            fn class(ctx: &crate::type_context::TypeContext) -> $crate::class::ClassRef {
                if let Some(class) = ctx.get::<$ty>() {
                    return class;
                }

                let class = ::std::rc::Rc::new($crate::class::Class::new_empty($debris_ty.into()));
                ctx.insert::<Self>(class.clone());

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
                    class.set_property(
                        impl_class!(get_ident $ident),
                        obj_function,
                    );
                })*

                class
            }
        }
    };
    (get_ident $ident:literal) => {$ident.into()};
    (get_ident $ident:tt) => {::debris_common::SpecialIdent::$ident.into()};

    (get_fn_name $value:literal) => {$value};
    (get_fn_name $value:tt) => {concat!("<", stringify!($value), ">")};
}
