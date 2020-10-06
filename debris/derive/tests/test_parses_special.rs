#![allow(dead_code, unused_variables)]
use debris_core::error::LangResult;
use debris_core::objects::FunctionContext;
use debris_core::objects::ObjectStaticInteger;
use debris_derive::template;

struct MyObject;

#[template]
impl MyObject {
    #[special(fn() -> StaticInt)]
    fn add(ctx: &mut FunctionContext) -> LangResult<ObjectStaticInteger> {
        Ok(0.into())
    }
}

fn main() {}
