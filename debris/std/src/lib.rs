//! The standard library implementation for the debris language
//!
//! Currently experimental.
//! There are not yet specific plans how it will look like.
//!
//! However, I plan to add at least a wrapper for every minecraft command.
use debris_core::{
    llir::llir_nodes::Execute,
    llir::{
        llir_nodes::{FastStoreFromResult, Node},
        utils::Scoreboard,
    },
    objects::{FunctionContext, ObjInt, ObjModule, ObjStaticInt, ObjString},
    CompileContext, ObjectRef,
};

/// Loads the standard library module
pub fn load(ctx: &CompileContext) -> ObjModule {
    let mut module = ObjModule::new("builtins");
    module.register_typed_function(ctx, "execute", &execute);
    module.register_typed_function(ctx, "print", &print_int);
    module.register_typed_function(ctx, "dbg", &dbg_any);

    module
}

/// Executes a string as a command and returns the result
fn execute(ctx: &mut FunctionContext, string: &ObjString) -> ObjInt {
    let string_value = string.as_str();
    let return_value = ctx.item_id;

    let execute_command = Node::Execute(Execute {
        command: string_value.to_string(),
    });
    ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
        command: execute_command.into(),
        id: return_value,
        scoreboard: Scoreboard::Main,
    }));

    return_value.into()
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
