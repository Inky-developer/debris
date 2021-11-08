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
    memory::copy,
    objects::{
        obj_bool::ObjBool,
        obj_bool_static::ObjStaticBool,
        obj_class::{HasClass, ObjClass},
        obj_format_string::{FormatStringComponent, ObjFormatString},
        obj_function::{FunctionContext, ObjFunction},
        obj_int::ObjInt,
        obj_int_static::ObjStaticInt,
        obj_module::ObjModule,
        obj_null::ObjNull,
        obj_string::ObjString,
        obj_struct::ObjStruct,
        obj_struct_object::ObjStructObject,
        obj_tuple_object::ObjTupleObject,
    },
    type_context::TypeContext,
    utils::{Scoreboard, ScoreboardValue},
    ObjectRef, ValidPayload,
};

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
        "Struct" => ObjStruct
    };
}

fn execute(ctx: &mut FunctionContext) -> LangResult<ObjInt> {
    let args = ctx.parameters;
    if let Some((value,)) = args.downcast_array() {
        Ok(execute_string(ctx, value))
    } else if let Some((value,)) = args.downcast_array() {
        Ok(execute_format_string(ctx, value))
    } else {
        return Err(LangErrorKind::UnexpectedOverload {
            parameters: args.iter().map(|obj| obj.class.to_string()).collect(),
            expected: vec![
                vec![ObjString::class(ctx.type_ctx).to_string()],
                vec![ObjFormatString::class(ctx.type_ctx).to_string()],
            ],
        });
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

// TODO: Use a macro to automatically dispatch the correct overload and handle the error message
fn print(ctx: &mut FunctionContext) -> LangResult<()> {
    let args = ctx.parameters;
    if let Some((value,)) = args.downcast_array() {
        print_int(ctx, value)
    } else if let Some((value,)) = args.downcast_array() {
        print_int_static(ctx, value)
    } else if let Some((value,)) = args.downcast_array() {
        print_string(ctx, value)
    } else if let Some((value,)) = args.downcast_array() {
        print_format_string(ctx, value)
    } else {
        return Err(LangErrorKind::UnexpectedOverload {
            parameters: args.iter().map(|obj| obj.class.to_string()).collect(),
            expected: vec![
                vec![ObjInt::class(ctx.type_ctx).to_string()],
                vec![ObjStaticInt::class(ctx.type_ctx).to_string()],
                vec![ObjString::class(ctx.type_ctx).to_string()],
                vec![ObjFormatString::class(ctx.type_ctx).to_string()],
            ],
        });
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
    }))
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
    }))
}

fn print_format_string(ctx: &mut FunctionContext, value: &ObjFormatString) {
    fn fmt_component(buf: &mut Vec<JsonFormatComponent>, value: ObjectRef, sep: Rc<str>) {
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
        } else if let Some(_struct_obj) = value.downcast_payload::<ObjStructObject>() {
            // buf.push(JsonFormatComponent::RawText(
            //     format!("{} {{ ", struct_obj.struct_type.ident).into(),
            // ));
            //
            // let namespace = ctx.llir_builder.arena.get(struct_obj.variables);
            // for (ident, _) in &struct_obj.struct_type.fields {
            //     let value = namespace
            //         .get(ctx.llir_builder.arena, ident)
            //         .unwrap()
            //         .1
            //         .value();
            //     buf.push(JsonFormatComponent::RawText(format!("{}: ", ident).into()));
            //     fmt_component(ctx, buf, ctx.get_object(value), Rc::clone(&sep));
            //     buf.push(JsonFormatComponent::RawText(sep.clone()));
            // }
            // if !namespace.is_empty() {
            //     buf.pop();
            // }
            //
            // buf.push(JsonFormatComponent::RawText(" }".into()));
            todo!("Implement print for struct objects")
        } else if let Some(obj) = value.downcast_payload::<ObjTupleObject>() {
            buf.push(JsonFormatComponent::RawText("(".into()));

            let mut iter = obj.values.iter();
            if let Some(value) = iter.next() {
                fmt_component(buf, value.clone(), Rc::clone(&sep));
                for value in iter {
                    buf.push(JsonFormatComponent::RawText(Rc::clone(&sep)));
                    fmt_component(buf, value.clone(), Rc::clone(&sep));
                }
            }

            buf.push(JsonFormatComponent::RawText(")".into()));
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
                fmt_component(&mut buf, value.clone(), ", ".into());
            }
        }
    }

    ctx.emit(Node::Write(WriteMessage {
        target: WriteTarget::Chat,
        message: FormattedText { components: buf },
    }));
}

fn dyn_int(ctx: &mut FunctionContext) -> LangResult<ObjInt> {
    let args = ctx.parameters;
    if let Some((value,)) = args.downcast_array() {
        Ok(static_int_to_int(ctx, value))
    } else if let Some((value,)) = args.downcast_array() {
        Ok(int_to_int(value))
    } else if let Some((value,)) = args.downcast_array() {
        Ok(static_bool_to_int(ctx, value))
    } else if let Some((value,)) = args.downcast_array() {
        Ok(bool_to_int(ctx, value))
    } else {
        Err(LangErrorKind::UnexpectedOverload {
            parameters: args.iter().map(|obj| obj.class.to_string()).collect(),
            expected: vec![
                vec![ObjStaticInt::class(ctx.type_ctx).to_string()],
                vec![ObjInt::class(ctx.type_ctx).to_string()],
                vec![ObjStaticBool::class(ctx.type_ctx).to_string()],
                vec![ObjBool::class(ctx.type_ctx).to_string()],
            ],
        })
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

fn bool_to_int(ctx: &mut FunctionContext, x: &ObjBool) -> ObjInt {
    ctx.emit(copy(ctx.item_id, x.id));
    ObjInt::new(ctx.item_id)
}
