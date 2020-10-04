#![allow(dead_code, unused_variables)]
use debris_core::error::Result;
use debris_core::objects::FunctionContext;
use debris_core::objects::ObjectInteger;
use debris_derive::template;

struct MyObject;

#[template]
impl MyObject {
    #[special(fn() -> Int)]
    fn add(ctx: &mut FunctionContext) -> Result<ObjectInteger> {
        Ok(0.into())
    }
}

fn main() {}
