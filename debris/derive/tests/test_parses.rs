use debris_derive::object;

#[allow(dead_code)]
struct Something;

#[object(debris_core::Type::StaticInt)]
impl Something {}
