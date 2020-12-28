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
    objects::{FunctionContext, ObjModule, ObjStaticInt},
    CompileContext, ObjectRef, ValidPayload,
};

/// Loads the standard library module
pub fn load(ctx: &CompileContext) -> ObjModule {
    let null_cls = ctx.type_ctx.null(ctx).class.clone();

    let mut module = ObjModule::new("builtins");
    module.register("hello_world", ObjStaticInt::new(1).into_object(ctx));
    module.register_function(ctx, "print", print_int, null_cls.clone());
    module.register_function(ctx, "dbg", dbg_any, null_cls);
    module
}

fn print_int(ctx: &mut FunctionContext, args: &[ObjectRef]) -> LangResult<ObjectRef> {
    let value = args[0].downcast_payload::<ObjStaticInt>().unwrap();

    ctx.emit(Node::Execute(Execute {
        command: format!("tellraw @a {{\"text\":\"Hello World from Debris! Your value is {}\", \"color\": \"gold\"}}", value.value),
    }));
    Ok(ctx.null())
}

fn dbg_any(ctx: &mut FunctionContext, args: &[ObjectRef]) -> LangResult<ObjectRef> {
    let value = &args[0];

    ctx.emit(Node::Execute(Execute {
        command: format!("say {:?}", value),
    }));
    Ok(ctx.null())
}
