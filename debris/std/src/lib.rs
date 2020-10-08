//! The standard library implementation for the debris language
//!
//! Currently experimental.
//! There are not yet specific plans how it will look like.
//!
//! However, I plan to add at least a wrapper for every minecraft command.
use debris_core::{
    error::LangResult,
    llir::llir_nodes::Execute,
    llir::llir_nodes::Node,
    objects::{CallbackFunction, FunctionContext, ObjectFunction, ObjectModule, StaticInt},
    CompileContext, ObjectPayload, ObjectRef,
};
use debris_type::Type;

/// Loads the standard library module
pub fn load(ctx: &CompileContext) -> ObjectModule {
    let mut obj = ObjectModule::new("builtins");
    obj.register("hello_world", StaticInt::new(1).into_object(ctx));
    obj.register(
        "print",
        ObjectFunction::without_overload(
            vec![],
            Type::StaticInt,
            CallbackFunction(execute_something),
        )
        .into_object(ctx),
    );
    obj
}

fn execute_something(ctx: &mut FunctionContext, _: Vec<ObjectRef>) -> LangResult<ObjectRef> {
    ctx.emit(Node::Execute(Execute {
        command: r#"tellraw @a {"text":"Hello World from Debris!", "color": "gold"}"#.to_string(),
    }));
    Ok(StaticInt::new(0).into_object(ctx.compile_context))
}
