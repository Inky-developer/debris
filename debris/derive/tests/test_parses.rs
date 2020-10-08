#![allow(dead_code, unused_variables)]
use debris_core::error::LangResult;
use debris_core::objects::FunctionContext;
use debris_core::objects::StaticInt;
use debris_derive::template;

struct MyObject;

#[template]
impl MyObject {
    #[method(fn() -> StaticInt)]
    fn get_zero(ctx: &mut FunctionContext) -> LangResult<StaticInt> {
        Ok(0.into())
    }
}

fn main() {}
