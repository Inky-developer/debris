#![allow(dead_code, unused_variables)]
use debris_core::error::LangResult;
use debris_core::objects::FunctionContext;
use debris_core::objects::StaticInt;
use debris_derive::template;

struct MyObject;

#[template]
impl MyObject {
    #[special(fn() -> StaticInt)]
    fn add(ctx: &mut FunctionContext) -> LangResult<StaticInt> {
        Ok(0.into())
    }
}

fn main() {}
