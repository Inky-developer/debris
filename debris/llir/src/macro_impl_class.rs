#[macro_export]
macro_rules! impl_class {
    ($ty:ty, $debris_ty:path, {$($ident:tt => $fn:expr),*}) => {
        impl $crate::objects::obj_class::HasClass for $ty {
            fn class(ctx: &crate::type_context::TypeContext) -> $crate::class::ClassRef {

                let class = ::std::rc::Rc::new($crate::class::Class::new_empty($debris_ty.into()));
                ctx.insert::<Self>(class.clone());

                $({
                    use $crate::ValidPayload;
                    use $crate::function_interface::ToFunctionInterface;

                    let function = $crate::function_interface::DebrisFunctionInterface::from(
                        ($fn).to_function_interface(),
                    );
                    let obj_function = $crate::objects::obj_function::ObjFunction::new(
                        stringify!($ident),
                        ::std::rc::Rc::new(function)
                    ).into_object(ctx);
                    class.set_property(
                        impl_class!(getident $ident),
                        obj_function,
                    );
                })*

                class
            }
        }
    };
    (getident $ident:literal) => {$ident.into()};
    (getident $ident:tt) => {::debris_common::SpecialIdent::$ident.into()};
}

// class.set_property(
//     ::debris_common::SpecialIdent::PromoteRuntime.into(),
//     {
//         let function = ::debris_llir::function_interface::DebrisFunctionInterface::from(
//             __debris_derive_method_special_promote_runtime_0.to_function_interface(),
//         );
//         ::debris_llir::objects::obj_function::ObjFunction::new(
//             "ObjStaticInt.PromoteRuntime",
//             ::std::rc::Rc::new(function),
//         )
//     }
//     .into_object(ctx),
// );
