//! The standard library implementation for the debris language
//!
//! Currently experimental.
//! There are not yet specific plans how it will look like.
//!
//! However, I plan to add at least a wrapper for every minecraft command.
use std::{collections::HashMap, rc::Rc};

use debris_common::Ident;
use debris_error::{LangErrorKind, LangResult};
use debris_llir::{
    function_interface::{DowncastArray, ToFunctionInterface, ValidReturnType},
    json_format::{FormattedText, JsonFormatComponent},
    llir_nodes::{
        ExecuteRaw, ExecuteRawComponent, FastStore, FastStoreFromResult, Node, WriteMessage,
        WriteTarget,
    },
    match_object,
    minecraft_utils::{Scoreboard, ScoreboardValue},
    objects::{
        obj_bool::ObjBool,
        obj_bool_static::ObjStaticBool,
        obj_class::{HasClass, ObjClass},
        obj_format_string::{FormatStringComponent, ObjFormatString},
        obj_function::{FunctionContext, ObjFunction},
        obj_int::ObjInt,
        obj_int_static::ObjStaticInt,
        obj_module::ObjModule,
        obj_native_function::ObjNativeFunction,
        obj_null::ObjNull,
        obj_string::ObjString,
        obj_struct_object::ObjStructObject,
        obj_tuple_object::ObjTupleObject,
    },
    type_context::TypeContext,
    ObjectRef, ValidPayload,
};

// Helper macro that can be used to match on the type of function parameters
macro_rules! match_parameters {
    ($ctx:ident, $args:ident, ($($name:ident),+): ($($type:path),+) => $cmd:expr, $($rest:tt)*) => {{
        match_parameters! { impl, [], $ctx, $args, ($($name),+): ($($type),+) => $cmd, $($rest)* }
    }};
    (error, $ctx:ident, $data:ident, ) => {};
    (error, $ctx:ident, $data:ident, [$($expected:tt)*], $($rest:tt)*) => {
        $data.push(vec![$(<$expected>::class($ctx.type_ctx).to_string()),*]);
        match_parameters!(error, $ctx, $data, $($rest)*)
    };
    (impl, [$($expected:tt)*], $ctx:ident, $args:ident,) => {
        #[allow(clippy::vec_init_then_push)]
        {
            let mut data = Vec::new();
            match_parameters!{error, $ctx, data, $($expected)*}

            return Err(LangErrorKind::UnexpectedOverload {
                parameters: $args.iter().map(|obj| obj.class.to_string()).collect(),
                expected: data,
                function_definition_span: None,
            });
        }
    };
    (impl, [$($expected:tt)*], $ctx:ident, $args:ident, ($($name:ident),+): ($($type:path),+) => $cmd:expr, $($rest:tt)*) => {
        if let Some(($($name),*,)) = DowncastArray::<($(&$type),*,)>::downcast_array($args) {
            $cmd
        } else {
            match_parameters! { impl, [[$($type),+], $($expected)*], $ctx, $args, $($rest)* }
        }
    };
}

fn function_for<Params, Return, T>(name: &'static str, function: T) -> ObjFunction
where
    T: ToFunctionInterface<Params, Return> + 'static,
    Return: ValidReturnType,
{
    ObjFunction::new(name, Rc::new(function.to_normalized_function().into()))
}

pub fn load_all(ctx: &TypeContext) -> HashMap<Ident, ObjectRef> {
    load(ctx).members.into_iter().collect()
}

/// Loads the standard library module
pub fn load(ctx: &TypeContext) -> ObjModule {
    let mut module = ObjModule::new("builtins");

    register_primitives(ctx, &mut module);

    module.register_function(ctx, function_for("execute", &execute));
    module.register_function(ctx, function_for("print", &print));
    module.register_function(ctx, function_for("dyn_int", &dyn_int));
    module.register_function(ctx, function_for("on_tick", &on_tick));
    module.register_function(ctx, function_for("export", &export));

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
fn register_primitives(ctx: &TypeContext, module: &mut ObjModule) {
    register_primitives! {ctx, module,
        "Null" => ObjNull,
        "Int" => ObjInt,
        "ComptimeInt" => ObjStaticInt,
        "Bool" => ObjBool,
        "ComptimeBool" => ObjStaticBool,
        "String" => ObjString,
        "FormatString" => ObjFormatString,
        "Type" => ObjClass
    };
}

fn execute(ctx: &mut FunctionContext, args: &[ObjectRef]) -> LangResult<ObjInt> {
    match_parameters! {ctx, args,
        (value): (ObjString) => Ok(execute_string(ctx, value)),
        (value): (ObjFormatString) => Ok(execute_format_string(ctx, value)),
    }
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
            FormatStringComponent::Value(obj) => {
                match_object!{obj,
                    string: ObjString => ExecuteRawComponent::String(string.value()),
                    int: ObjInt => ExecuteRawComponent::ScoreboardValue(int.as_scoreboard_value()),
                    bool: ObjBool => ExecuteRawComponent::ScoreboardValue(bool.as_scoreboard_value()),
                    static_int: ObjStaticInt => ExecuteRawComponent::ScoreboardValue(static_int.as_scoreboard_value()),
                    static_bool: ObjStaticBool => ExecuteRawComponent::ScoreboardValue(static_bool.as_scoreboard_value()),
                    else => ExecuteRawComponent::String(format!("{}", obj).into())
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

fn print(ctx: &mut FunctionContext, parameters: &[ObjectRef]) -> LangResult<()> {
    match_parameters! {ctx, parameters,
        (value): (ObjInt) => print_int(ctx, value),
        (value): (ObjStaticInt) => print_int_static(ctx, value),
        (value): (ObjString) => print_string(ctx, value),
        (value): (ObjFormatString) => print_format_string(ctx, value),
    }
    Ok(())
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
    }));
}

fn print_int_static(ctx: &mut FunctionContext, value: &ObjStaticInt) {
    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText {
            components: vec![JsonFormatComponent::RawText(value.value.to_string().into())],
        },
    }));
}

fn print_string(ctx: &mut FunctionContext, value: &ObjString) {
    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText {
            components: vec![JsonFormatComponent::RawText(value.value())],
        },
    }));
}

fn print_format_string(ctx: &mut FunctionContext, value: &ObjFormatString) {
    fn fmt_component(buf: &mut Vec<JsonFormatComponent>, value: &ObjectRef, sep: &Rc<str>) {
        match_object! {value,
            string: ObjString => buf.push(JsonFormatComponent::RawText(string.value())),
            int: ObjInt => buf.push(JsonFormatComponent::Score(int.as_scoreboard_value())),
            static_int: ObjStaticInt => buf.push(JsonFormatComponent::Score(static_int.as_scoreboard_value())),
            bool: ObjBool => buf.push(JsonFormatComponent::Score(bool.as_scoreboard_value())),
            static_bool: ObjStaticBool => buf.push(JsonFormatComponent::Score(static_bool.as_scoreboard_value())),
            strukt: ObjStructObject => {
                buf.push(JsonFormatComponent::RawText(format!("{} {{ ", strukt.struct_type.ident).into()));

                let mut iter = strukt.properties.iter();
                if let Some((ident, value)) = iter.next() {
                    buf.push(JsonFormatComponent::RawText(format!("{}: ", ident).into()));
                    fmt_component(buf, value, sep);
                    for (ident, value) in iter {
                        buf.push(JsonFormatComponent::RawText(Rc::clone(sep)));
                        buf.push(JsonFormatComponent::RawText(format!("{}: ", ident).into()));
                        fmt_component(buf, value, sep);
                    }
                }

                buf.push(JsonFormatComponent::RawText(" }".into()));
            },
            tuple: ObjTupleObject => {
                buf.push(JsonFormatComponent::RawText("(".into()));

                let mut iter = tuple.values.iter();
                if let Some(value) = iter.next() {
                    fmt_component(buf, value, sep);
                    for value in iter {
                        buf.push(JsonFormatComponent::RawText(Rc::clone(sep)));
                        fmt_component(buf, value, sep);
                    }
                }

                buf.push(JsonFormatComponent::RawText(")".into()));
            },
            else => buf.push(JsonFormatComponent::RawText(value.payload.to_string().into()))
        };
    }

    let mut buf = Vec::with_capacity(value.components.len());
    for component in &value.components {
        match component {
            FormatStringComponent::String(str_rc) => {
                buf.push(JsonFormatComponent::RawText(str_rc.clone()));
            }
            FormatStringComponent::Value(value) => {
                fmt_component(&mut buf, value, &", ".into());
            }
        }
    }

    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText { components: buf },
    }));
}

fn dyn_int(ctx: &mut FunctionContext, args: &[ObjectRef]) -> LangResult<ObjInt> {
    match_parameters! {ctx, args,
        (value): (ObjStaticInt) => Ok(static_int_to_int(ctx, value)),
        (value): (ObjInt) => Ok(int_to_int(value)),
        (value): (ObjStaticBool) => Ok(static_bool_to_int(ctx, value)),
        (value): (ObjBool) => Ok(bool_to_int(value)),
    }
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
    x.clone()
}

fn static_bool_to_int(ctx: &mut FunctionContext, x: &ObjStaticBool) -> ObjInt {
    let value = x.value as i32;
    static_int_to_int(ctx, &value.into())
}

fn bool_to_int(x: &ObjBool) -> ObjInt {
    ObjInt::new(x.id)
}

/// Registers a function to run on load
fn on_tick(ctx: &mut FunctionContext, function: &ObjNativeFunction) {
    ctx.runtime
        .register_ticking_function(function.function_id, ctx.span);
}

/// Exports a function to a specific path
fn export(ctx: &mut FunctionContext, parameters: &[ObjectRef]) -> LangResult<ObjectRef> {
    match_parameters!(ctx, parameters,
        (value): (ObjString) => Ok(export_impl(ctx, value.value())),
    )
}

fn export_impl(ctx: &FunctionContext, exported_path: Rc<str>) -> ObjectRef {
    let export = move |ctx: &mut FunctionContext, function: &ObjNativeFunction| {
        ctx.runtime
            .export(function.function_id, exported_path.to_string(), ctx.span);
    };
    let export_function = function_for("export_inner", export);
    export_function.into_object(ctx.type_ctx)
}
