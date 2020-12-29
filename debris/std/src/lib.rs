//! The standard library implementation for the debris language
//!
//! Currently experimental.
//! There are not yet specific plans how it will look like.
//!
//! However, I plan to add at least a wrapper for every minecraft command.
use debris_core::{
    llir::llir_nodes::Execute,
    llir::llir_nodes::Node,
    objects::{FunctionContext, ObjModule, ObjStaticInt},
    CompileContext, ObjectRef, ValidPayload,
};

/// Loads the standard library module
pub fn load(ctx: &CompileContext) -> ObjModule {
    let mut module = ObjModule::new("builtins");
    module.register("hello_world", ObjStaticInt::new(1).into_object(ctx));
    module.register_typed_function(ctx, "print", &print_int);
    module.register_typed_function(ctx, "dbg", &dbg_any);
    module.register_typed_function(ctx, "test", &test);
    module
}

fn print_int(ctx: &mut FunctionContext, value: &ObjStaticInt) {
    ctx.emit(Node::Execute(Execute {
        command: format!("tellraw @a {{\"text\":\"Hello World from Debris! Your value is {}\", \"color\": \"gold\"}}", value.value),
    }));
}

fn dbg_any(ctx: &mut FunctionContext, args: &[ObjectRef]) {
    let value = &args[0];

    ctx.emit(Node::Execute(Execute {
        command: format!("say {:?}", value),
    }));
}

fn test(a: &ObjStaticInt, b: &ObjStaticInt) -> i32 {
    a.value + b.value
}
