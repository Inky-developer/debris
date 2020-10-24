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
    objects::{CallbackFunction, FunctionContext, HasClass, ObjFunction, ObjModule, ObjStaticInt},
    CompileContext, ObjectRef, ValidPayload,
};

/// Loads the standard library module
pub fn load(ctx: &CompileContext) -> ObjModule {
    let mut module = ObjModule::new("builtins");
    module.register("hello_world", ObjStaticInt::new(1).into_object(ctx));
    module.register(
        "print",
        ObjFunction::without_overload(
            vec![],
            ObjStaticInt::class(ctx),
            CallbackFunction(execute_something),
        )
        .into_object(ctx),
    );
    module
}

fn execute_something(ctx: &mut FunctionContext, _: &[ObjectRef]) -> LangResult<ObjectRef> {
    ctx.emit(Node::Execute(Execute {
        command: r#"tellraw @a {"text":"Hello World from Debris!", "color": "gold"}"#.to_string(),
    }));
    Ok(ObjStaticInt::new(0).into_object(ctx.compile_context))
}
