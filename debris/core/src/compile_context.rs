use debris_type::Type;
use std::default::Default;

use crate::objects::{
    ObjectDynamicInteger, ObjectFunction, ObjectModule, ObjectStaticInteger, ObjectType,
};
use crate::objects::{ObjectString, TypeRef};
use crate::Config;

#[derive(Debug, Eq, PartialEq)]
pub struct CompileContext {
    pub type_ctx: TypeContext,
    pub config: Config,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeContext {
    pub static_int_template: TypeRef,
    pub dynamic_int_template: TypeRef,
    pub function_template: TypeRef,
    pub module_template: TypeRef,
    pub string_template: TypeRef,
    pub type_template: TypeRef,
}

impl CompileContext {}

impl Default for CompileContext {
    fn default() -> Self {
        let this = CompileContext {
            config: Config::default(),
            type_ctx: TypeContext::default(),
        };

        init_types(&this);
        this
    }
}

impl TypeContext {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn template_for_type(&self, typ: &Type) -> TypeRef {
        match typ {
            Type::Bool => todo!(),
            Type::Fixed => todo!(),
            Type::Function => self.function_template.clone(),
            Type::StaticInt => self.static_int_template.clone(),
            Type::DynamicInt => self.dynamic_int_template.clone(),
            Type::Module => self.module_template.clone(),
            Type::String => self.string_template.clone(),
            Type::Template(_type) => panic!("No meta template"),
            Type::Type => self.type_template.clone(),
        }
    }
}

impl Default for TypeContext {
    fn default() -> Self {
        TypeContext {
            function_template: ObjectFunction::template(),
            static_int_template: ObjectStaticInteger::template(),
            dynamic_int_template: ObjectDynamicInteger::template(),
            module_template: ObjectModule::template(),
            string_template: ObjectString::template(),
            type_template: ObjectType::template(),
        }
    }
}

fn init_types(ctx: &CompileContext) {
    ObjectType::init_template(ctx, &ctx.type_ctx.type_template);
    ObjectFunction::init_template(ctx, &ctx.type_ctx.function_template);
    ObjectStaticInteger::init_template(ctx, &ctx.type_ctx.static_int_template);
    ObjectDynamicInteger::init_template(ctx, &ctx.type_ctx.dynamic_int_template);
    ObjectModule::init_template(ctx, &ctx.type_ctx.module_template);
    ObjectString::init_template(ctx, &ctx.type_ctx.string_template);
}
