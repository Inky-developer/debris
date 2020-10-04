use debris_type::Type;
use std::default::Default;

use crate::objects::{ObjectFunction, ObjectInteger, ObjectType};
use crate::objects::{ObjectString, TypeRef};
use crate::Config;

#[derive(Debug, Eq, PartialEq)]
pub struct CompileContext {
    pub type_ctx: TypeContext,
    pub config: Config,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeContext {
    pub int_template: TypeRef,
    pub function_template: TypeRef,
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
            Type::Int => self.int_template.clone(),
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
            int_template: ObjectInteger::template(),
            string_template: ObjectString::template(),
            type_template: ObjectType::template(),
        }
    }
}

fn init_types(ctx: &CompileContext) {
    ObjectType::init_template(ctx, &ctx.type_ctx.type_template);
    ObjectFunction::init_template(ctx, &ctx.type_ctx.function_template);
    ObjectInteger::init_template(ctx, &ctx.type_ctx.int_template);
}
