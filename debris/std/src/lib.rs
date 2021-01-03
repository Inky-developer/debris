//! The standard library implementation for the debris language
//!
//! Currently experimental.
//! There are not yet specific plans how it will look like.
//!
//! However, I plan to add at least a wrapper for every minecraft command.
use debris_core::{
    function_interface::ToFunctionInterface,
    llir::llir_nodes::Execute,
    llir::{
        llir_nodes::{FastStore, FastStoreFromResult, Node},
        utils::{Scoreboard, ScoreboardValue},
    },
    objects::{
        FunctionContext, FunctionSignature, ObjBool, ObjFunction, ObjInt, ObjModule, ObjStaticBool,
        ObjStaticInt, ObjString,
    },
    CompileContext, ObjectPayload, ObjectRef, ValidPayload,
};

fn signature_for<Params, Return, T>(ctx: &CompileContext, function: &'static T) -> FunctionSignature
where
    T: ToFunctionInterface<Params, Return> + 'static,
    Return: ObjectPayload,
{
    FunctionSignature::new(
        T::query_parameters(ctx),
        T::query_return(ctx).expect("Expected a return type"),
        function.to_function_interface().into(),
    )
}

/// Loads the standard library module
pub fn load(ctx: &CompileContext) -> ObjModule {
    let mut module = ObjModule::new("builtins");
    module.register_typed_function(ctx, "execute", &execute);
    module.register_typed_function(ctx, "print", &print_int);
    module.register_typed_function(ctx, "dbg", &dbg_any);
    // module.register_typed_function(ctx, "dyn_int", &static_int_to_int);
    module.register(
        "dyn_int",
        ObjFunction::new(
            ctx,
            vec![
                signature_for(ctx, &static_int_to_int),
                signature_for(ctx, &int_to_int),
                signature_for(ctx, &static_bool_to_int),
                signature_for(ctx, &bool_to_int),
            ],
        )
        .into_object(ctx),
    );
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

fn static_int_to_int(ctx: &mut FunctionContext, x: &ObjStaticInt) -> ObjInt {
    ctx.emit(Node::FastStore(FastStore {
        scoreboard: Scoreboard::Main,
        id: ctx.item_id,
        value: ScoreboardValue::Static(x.value),
    }));

    ObjInt::from(ctx.item_id)
}

fn int_to_int(x: &ObjInt) -> ObjInt {
    *x
}

fn static_bool_to_int(ctx: &mut FunctionContext, x: &ObjStaticBool) -> ObjInt {
    let value = x.value() as i32;
    static_int_to_int(ctx, &value.into())
}

fn bool_to_int(x: &ObjBool) -> ObjInt {
    ObjInt::new(x.id)
}
