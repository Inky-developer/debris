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
    class::{Class, ClassKind},
    function_interface::{DowncastArray, ToFunctionInterface, ValidReturnType},
    json_format::{FormattedText, JsonFormatComponent},
    llir_nodes::{
        Call, ExecuteRaw, ExecuteRawComponent, FastStore, FastStoreFromResult, Node, WriteMessage,
        WriteTarget,
    },
    minecraft_utils::{Scoreboard, ScoreboardValue},
    objects::{
        obj_bool::ObjBool,
        obj_bool_static::ObjStaticBool,
        obj_class::{HasClass, ObjClass},
        obj_format_string::{FormatStringComponent, ObjFormatString},
        obj_function::{FunctionContext, ObjFunction},
        obj_function_ref::ObjFunctionRef,
        obj_int::ObjInt,
        obj_int_static::ObjStaticInt,
        obj_module::ObjModule,
        obj_native_function::ObjNativeFunction,
        obj_null::ObjNull,
        obj_string::ObjString,
    },
    type_context::TypeContext,
    ObjectRef, Type, ValidPayload,
};

// Helper macro that can be used to match on the type of function parameters
macro_rules! match_parameters {
    ($ctx:ident, $args:ident, ($($name:ident),+): ($($type:path),+) => $cmd:expr, $($rest:tt)*) => {{
        match_parameters! { impl, [], $ctx, $args, ($($name),+): ($($type),+) => $cmd, $($rest)* }
    }};
    (error, $ctx:ident, $data:ident, ) => {};
    (error, $ctx:ident, $data:ident, [$($expected:tt)*], $($rest:tt)*) => {
        $data.push(vec![$(<$expected>::class($ctx.type_ctx()).to_string()),*]);
        match_parameters!(error, $ctx, $data, $($rest)*)
    };
    (impl, [$($expected:tt)*], $ctx:ident, $args:ident,) => {
        #[allow(clippy::vec_init_then_push)]
        {
            let mut data = Vec::new();
            match_parameters!{error, $ctx, data, $($expected)*}

            Err(LangErrorKind::UnexpectedOverload {
                parameters: $args.iter().map(|obj| obj.class.to_string()).collect(),
                expected: data,
                function_definition_span: None,
            })
        }
    };
    (impl, [$($expected:tt)*], $ctx:ident, $args:ident, ($($name:ident),+): ($($type:path),+) => $cmd:expr, $($rest:tt)*) => {
        if let Some(($($name),*,)) = DowncastArray::<($(&$type),*,)>::downcast_array($args) {
            Ok($cmd)
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
        "Bool" => ObjBool,
        "ComptimeBool" => ObjStaticBool,
        "FormatString" => ObjFormatString,
        "FunctionRef" => ObjFunctionRef,
        "Int" => ObjInt,
        "ComptimeInt" => ObjStaticInt,
        "Null" => ObjNull,
        "String" => ObjString,
        "Type" => ObjClass
    };

    module.register(
        "Any",
        ObjClass::new(Class::new_empty(ClassKind::Type(Type::Any)).into()).into_object(ctx),
    );
}

fn execute(ctx: &mut FunctionContext, args: &[ObjectRef]) -> LangResult<ObjInt> {
    match_parameters! {ctx, args,
        (value): (ObjString) => execute_string(ctx, value),
        (value): (ObjFormatString) => execute_format_string(ctx, value),
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

    let mut components = Vec::with_capacity(format_string.components.len());
    for component in &format_string.components {
        match component {
            FormatStringComponent::String(string) => {
                components.push(JsonFormatComponent::RawText(string.clone()));
            }
            FormatStringComponent::Value(obj) => obj.payload.json_fmt(&mut components),
        }
    }

    // Just convert the (possibly styled) json text into simple execute compatible components
    let components = components
        .into_iter()
        .map(|json_component| match json_component {
            JsonFormatComponent::RawText(text) => ExecuteRawComponent::String(text),
            JsonFormatComponent::Score(score) => ExecuteRawComponent::ScoreboardValue(score),
            JsonFormatComponent::Function(function) => {
                ExecuteRawComponent::Node(Node::Call(Call { id: function }))
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

fn print(ctx: &mut FunctionContext, parameters: &[ObjectRef]) {
    let mut components = Vec::with_capacity(parameters.len());
    match parameters {
        [] => {}
        [single] => single.payload.json_fmt(&mut components),
        [first, rest @ ..] => {
            first.payload.json_fmt(&mut components);
            let sep = ", ".into();
            for obj in rest {
                components.push(JsonFormatComponent::RawText(Rc::clone(&sep)));
                obj.payload.json_fmt(&mut components);
            }
        }
    }
    let node = Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText { components },
    });
    ctx.emit(node);
}

fn dyn_int(ctx: &mut FunctionContext, args: &[ObjectRef]) -> LangResult<ObjInt> {
    match_parameters! {ctx, args,
        (value): (ObjStaticInt) => static_int_to_int(ctx, value),
        (value): (ObjInt) => int_to_int(value),
        (value): (ObjStaticBool) => static_bool_to_int(ctx, value),
        (value): (ObjBool) => bool_to_int(value),
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
    let value = i32::from(x.value);
    static_int_to_int(ctx, &value.into())
}

fn bool_to_int(x: &ObjBool) -> ObjInt {
    ObjInt::new(x.id)
}

/// Registers a function to run on load
fn on_tick(ctx: &mut FunctionContext, function: &ObjNativeFunction) {
    let span = ctx.span;
    ctx.runtime_mut()
        .register_ticking_function(function.function_id, span);
}

/// Exports a function to a specific path
fn export(ctx: &mut FunctionContext, parameters: &[ObjectRef]) -> LangResult<ObjectRef> {
    match_parameters!(ctx, parameters,
        (value): (ObjString) => export_impl(ctx, value.value()),
    )
}

fn export_impl(ctx: &FunctionContext, exported_path: Rc<str>) -> ObjectRef {
    let export = move |ctx: &mut FunctionContext, function: &ObjNativeFunction| {
        let span = ctx.span;
        ctx.runtime_mut()
            .export(function.function_id, exported_path.to_string(), span);
    };
    let export_function = function_for("export_inner", export);
    export_function.into_object(ctx.type_ctx())
}
