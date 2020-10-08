use debris_core::{
    error::LangResult,
    llir::llir_nodes::Execute,
    llir::llir_nodes::Node,
    objects::{
        CallbackFunction, FunctionContext, ObjectFunction, ObjectModule, ObjectStaticInteger,
    },
    CompileContext, ObjectPayload, ObjectRef,
};
use debris_type::Type;

pub fn load(ctx: &CompileContext) -> ObjectModule {
    let mut obj = ObjectModule::new("builtins");
    obj.register("hello_world", ObjectStaticInteger::new(1).into_object(ctx));
    obj.register(
        "print",
        ObjectFunction::without_overload(
            vec![],
            Type::StaticInt,
            CallbackFunction(execute_something),
        )
        .into_object(ctx),
    );
    obj
}

fn execute_something(ctx: &mut FunctionContext, _: Vec<ObjectRef>) -> LangResult<ObjectRef> {
    ctx.emit(Node::Execute(Execute {
        command: r#"tellraw @a {"text":"Hello World from Debris!", "color": "gold"}"#.to_string(),
    }));
    Ok(ObjectStaticInteger::new(0).into_object(ctx.compile_context))
}
