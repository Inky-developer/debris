//! The standard library implementation for the debris language
//!
//! Currently experimental.
//! There are not yet specific plans how it will look like.
//!
//! However, I plan to add at least a wrapper for every minecraft command.
use debris_core::{
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
        obj_class::{HasClass, ObjClass},
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
        obj_struct::ObjStruct,
        obj_struct_object::ObjStructObject,
    },
    CompileContext, ObjectRef, ValidPayload,
};
use std::rc::Rc;

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

    register_primitives(ctx, &mut module);

    module.register(
        "execute",
        ObjFunction::new(
            ctx,
            vec![
                signature_for(ctx, &execute_string),
                signature_for(ctx, &execute_format_string),
            ],
        )
        .into_object(ctx),
    );
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

macro_rules! register_primitives {
    ($ctx:ident, $module:ident, $($ident:literal => $class:ident),*) => {{
        $(
            $module.register($ident, ObjClass::from($class::class($ctx)).into_object($ctx));
        )*
    }}
}

/// Registers all primitive types
fn register_primitives(ctx: &CompileContext, module: &mut ObjModule) {
    register_primitives! {ctx, module,
        "Null" => ObjNull,
        "Int" => ObjInt,
        "ComptimeInt" => ObjStaticInt,
        "Bool" => ObjBool,
        "ComptimeBool" => ObjStaticBool,
        "String" => ObjString,
        "FormatString" => ObjFormatString,
        "Struct" => ObjStruct
    };
}

/// Executes a string as a command and returns the result
fn execute_string(ctx: &mut FunctionContext, string: &ObjString) -> ObjInt {
    let string_value = string.value();
    let return_value = ctx.item_id;

    let execute_command =
        Node::Execute(ExecuteRaw(vec![ExecuteRawComponent::String(string_value)]));
    ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
        command: execute_command.into(),
        id: return_value,
        scoreboard: Scoreboard::Main,
    }));

    return_value.into()
}

fn execute_format_string(ctx: &mut FunctionContext, format_string: &ObjFormatString) -> ObjInt {
    let return_value = ctx.item_id;

    let components = format_string
        .components
        .iter()
        .map(|component| match component {
            FormatStringComponent::String(string) => ExecuteRawComponent::String(string.clone()),
            FormatStringComponent::Value(value) => {
                let obj = ctx.get_object(value);
                if let Some(string) = obj.downcast_payload::<ObjString>() {
                    ExecuteRawComponent::String(string.value())
                } else if let Some(int) = obj.downcast_payload::<ObjInt>() {
                    ExecuteRawComponent::ScoreboardValue(int.as_scoreboard_value())
                } else if let Some(bool) = obj.downcast_payload::<ObjBool>() {
                    ExecuteRawComponent::ScoreboardValue(bool.as_scoreboard_value())
                } else if let Some(static_int) = obj.downcast_payload::<ObjStaticInt>() {
                    ExecuteRawComponent::ScoreboardValue(static_int.as_scoreboard_value())
                } else if let Some(static_bool) = obj.downcast_payload::<ObjStaticBool>() {
                    ExecuteRawComponent::ScoreboardValue(static_bool.as_scoreboard_value())
                } else {
                    ExecuteRawComponent::String(format!("{:?}", component).into())
                }
            }
        })
        .collect();

    let execute_command = Node::Execute(ExecuteRaw(components));
    ctx.emit(Node::FastStoreFromResult(FastStoreFromResult {
        command: execute_command.into(),
        id: return_value,
        scoreboard: Scoreboard::Main,
    }));

    return_value.into()
}

fn print_int_static(ctx: &mut FunctionContext, value: &ObjStaticInt) {
    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText {
            components: vec![JsonFormatComponent::RawText(value.value.to_string().into())],
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
            components: vec![JsonFormatComponent::RawText(value.value())],
        },
    }))
}

fn print_format_string(ctx: &mut FunctionContext, value: &ObjFormatString) {
    fn fmt_component(
        ctx: &FunctionContext,
        buf: &mut Vec<JsonFormatComponent>,
        value: ObjectRef,
        sep: Rc<str>,
    ) {
        if let Some(string) = value.downcast_payload::<ObjString>() {
            buf.push(JsonFormatComponent::RawText(string.value()))
        } else if let Some(int) = value.downcast_payload::<ObjInt>() {
            buf.push(JsonFormatComponent::Score(int.as_scoreboard_value()))
        } else if let Some(static_int) = value.downcast_payload::<ObjStaticInt>() {
            buf.push(JsonFormatComponent::Score(static_int.as_scoreboard_value()))
        } else if let Some(bool) = value.downcast_payload::<ObjBool>() {
            buf.push(JsonFormatComponent::Score(bool.as_scoreboard_value()))
        } else if let Some(static_bool) = value.downcast_payload::<ObjStaticBool>() {
            buf.push(JsonFormatComponent::Score(
                static_bool.as_scoreboard_value(),
            ))
        } else if let Some(struct_obj) = value.downcast_payload::<ObjStructObject>() {
            buf.push(JsonFormatComponent::RawText(
                format!("{} {{ ", struct_obj.struct_type.ident).into(),
            ));

            let namespace = ctx.llir_builder.arena.get(struct_obj.variables).unwrap();
            for (ident, value) in namespace {
                buf.push(JsonFormatComponent::RawText(format!("{}: ", ident).into()));
                fmt_component(ctx, buf, ctx.get_object(value), Rc::clone(&sep));
                buf.push(JsonFormatComponent::RawText(sep.clone()));
            }
            if !namespace.is_empty() {
                buf.pop();
            }

            buf.push(JsonFormatComponent::RawText(" }".into()));
        } else {
            buf.push(JsonFormatComponent::RawText(
                value.payload.to_string().into(),
            ))
        }
    }

    let mut buf = Vec::with_capacity(value.components.len());
    for component in value.components.iter() {
        match component {
            FormatStringComponent::String(str_rc) => {
                buf.push(JsonFormatComponent::RawText(str_rc.clone()))
            }
            FormatStringComponent::Value(value) => {
                let value = ctx.get_object(value);
                fmt_component(ctx, &mut buf, value, ", ".into());
            }
        }
    }

    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText { components: buf },
    }));
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
    x.clone()
}

fn static_bool_to_int(ctx: &mut FunctionContext, x: &ObjStaticBool) -> ObjInt {
    let value = x.value as i32;
    static_int_to_int(ctx, &value.into())
}

fn bool_to_int(ctx: &mut FunctionContext, x: &ObjBool) -> ObjInt {
    ctx.emit(copy(ctx.item_id, x.id));
    ObjInt::new(ctx.item_id)
}
