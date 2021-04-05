//! The standard library implementation for the debris language
//!
//! Currently experimental.
//! There are not yet specific plans how it will look like.
//!
//! However, I plan to add at least a wrapper for every minecraft command.
use debris_core::{
    error::{LangErrorKind, LangResult},
    function_interface::{ToFunctionInterface, ValidReturnType},
    llir::{
        json_format::{FormattedText, JsonFormatComponent},
        llir_nodes::{
            ExecuteRaw, ExecuteRawComponent, FastStore, FastStoreFromResult, Node, WriteMessage,
            WriteTarget,
        },
        utils::{Scoreboard, ScoreboardValue},
    },
    memory::copy,
    objects::{
        obj_bool::ObjBool,
        obj_bool_static::ObjStaticBool,
        obj_format_string::{FormatStringComponent, ObjFormatString},
        obj_function::{
            CompilerFunction, FunctionContext, FunctionFlags, FunctionOverload, FunctionSignature,
            ObjFunction,
        },
        obj_int::ObjInt,
        obj_int_static::ObjStaticInt,
        obj_module::ObjModule,
        obj_null::ObjNull,
        obj_string::ObjString,
    },
    CompileContext, ObjectRef, ValidPayload,
};

fn signature_for<Params, Return, T>(ctx: &CompileContext, function: &'static T) -> FunctionOverload
where
    T: ToFunctionInterface<Params, Return> + 'static,
    Return: ValidReturnType,
{
    FunctionOverload::new(
        FunctionSignature::new(
            T::query_parameters(ctx),
            T::query_return(ctx).expect("Expected a return type"),
        )
        .into(),
        function.to_function_interface().into(),
    )
}

/// Loads the standard library module
pub fn load(ctx: &CompileContext) -> ObjModule {
    let mut module = ObjModule::new("builtins");
    module.register_typed_function(ctx, "execute", &execute);
    module.register_typed_function(ctx, "set_score", &set_score);
    module.register(
        "print",
        ObjFunction::new(
            ctx,
            vec![
                signature_for(ctx, &print_int_static),
                signature_for(ctx, &print_int),
                signature_for(ctx, &print_string),
                signature_for(ctx, &print_format_string),
            ],
        )
        .into_object(ctx),
    );
    module.register_typed_function(ctx, "dbg", &dbg_any);
    module.register(
        "register_ticking_function",
        ObjFunction::with_flags(
            ctx,
            vec![signature_for(ctx, &register_ticking_function)],
            FunctionFlags::CompilerImplemented(CompilerFunction::RegisterTickingFunction),
        )
        .into_object(ctx),
    );
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

    let execute_command = Node::Execute(ExecuteRaw(vec![ExecuteRawComponent::String(
        string_value.to_string(),
    )]));
    ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
        command: execute_command.into(),
        id: return_value,
        scoreboard: Scoreboard::Main,
    }));

    return_value.into()
}

/// Sets a value to a predefined score, for example to interact with apis.
/// ToDo: Make this function redundant by implementing formatted strings
fn set_score(
    ctx: &mut FunctionContext,
    scoreboard: &ObjString,
    player: &ObjString,
    value: &ObjInt,
) {
    let execute = ExecuteRaw(vec![
        ExecuteRawComponent::String(format!(
            "scoreboard players operation {} {} = ",
            scoreboard.as_str(),
            player.as_str()
        )),
        ExecuteRawComponent::ScoreboardValue(value.as_scoreboard_value()),
    ]);
    ctx.emit(Node::Execute(execute));
}

fn print_int_static(ctx: &mut FunctionContext, value: &ObjStaticInt) {
    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText {
            components: vec![JsonFormatComponent::RawText(value.value.to_string())],
        },
    }));
}

fn print_int(ctx: &mut FunctionContext, value: &ObjInt) {
    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText {
            components: vec![JsonFormatComponent::Score(ScoreboardValue::Scoreboard(
                Scoreboard::Main,
                value.id,
            ))],
        },
    }))
}

fn print_string(ctx: &mut FunctionContext, value: &ObjString) {
    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText {
            components: vec![JsonFormatComponent::RawText(value.as_str().to_owned())],
        },
    }))
}

fn print_format_string(ctx: &mut FunctionContext, value: &ObjFormatString) -> LangResult<ObjNull> {
    let components = value
        .components
        .iter()
        .map(|component| {
            Ok(match component {
                FormatStringComponent::String(string) => {
                    JsonFormatComponent::RawText(string.clone())
                }
                FormatStringComponent::Value(value) => {
                    let value = ctx.get_object(value);

                    if let Some(string) = value.downcast_payload::<ObjString>() {
                        JsonFormatComponent::RawText(string.as_str().to_string())
                    } else if let Some(int) = value.downcast_payload::<ObjInt>() {
                        JsonFormatComponent::Score(int.as_scoreboard_value())
                    } else if let Some(static_int) = value.downcast_payload::<ObjStaticInt>() {
                        JsonFormatComponent::Score(static_int.as_scoreboard_value())
                    } else if let Some(bool) = value.downcast_payload::<ObjBool>() {
                        JsonFormatComponent::Score(bool.as_scoreboard_value())
                    } else if let Some(static_bool) = value.downcast_payload::<ObjStaticBool>() {
                        JsonFormatComponent::Score(static_bool.as_scoreboard_value())
                    } else {
                        return Err(LangErrorKind::NotYetImplemented {
                            msg: format!(
                                "ToDO: Implement proper value formatting ({} not yet supported)",
                                value.class
                            ),
                        });
                    }
                }
            })
        })
        .collect::<Result<_, _>>()?;
    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText { components },
    }));
    Ok(ObjNull)
}

fn dbg_any(ctx: &mut FunctionContext, args: &[ObjectRef]) {
    let value = &args[0];

    ctx.emit(Node::Execute(ExecuteRaw(vec![
        ExecuteRawComponent::String(format!("say {}", value)),
    ])));
}

/// Empty stub, the function in implemented in the compiler
fn register_ticking_function() {}

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
    let value = x.value as i32;
    static_int_to_int(ctx, &value.into())
}

fn bool_to_int(ctx: &mut FunctionContext, x: &ObjBool) -> ObjInt {
    ctx.emit(copy(ctx.item_id, x.id));
    ObjInt::new(ctx.item_id)
}
