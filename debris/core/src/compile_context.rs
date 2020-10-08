use debris_type::Type;
use std::default::Default;

use crate::objects::{DynInt, ObjectFunction, ObjectModule, ObjectType, StaticInt};
use crate::objects::{ObjectString, TypeRef};
use crate::Config;

/// The Compilation context stores various information about the current compilation
#[derive(Debug, Eq, PartialEq)]
pub struct CompileContext {
    /// Contains all types
    pub type_ctx: TypeContext,
    /// The current config which specifies how to compile
    pub config: Config,
}

/// Holds every template for types
///
/// Unfortunately, the default implementation does not initialize the types.
/// ToDo: automatically initialize types in the default implementation
#[derive(Debug, Eq, PartialEq)]
pub struct TypeContext {
    pub static_int_template: TypeRef,
    pub dynamic_int_template: TypeRef,
    pub function_template: TypeRef,
    pub module_template: TypeRef,
    pub string_template: TypeRef,
    pub type_template: TypeRef,
}

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
    /// Creates a new type context with uninitialized types
    pub fn new() -> Self {
        Default::default()
    }

    /// Returns the template that matches the given type
    ///
    /// ToDo: Some types are context specific, like custom structs.
    /// These should not be handled by a global context.
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
            static_int_template: StaticInt::template(),
            dynamic_int_template: DynInt::template(),
            module_template: ObjectModule::template(),
            string_template: ObjectString::template(),
            type_template: ObjectType::template(),
        }
    }
}

/// Initializes all types for a compile context
pub fn init_types(ctx: &CompileContext) {
    ObjectType::init_template(ctx, &ctx.type_ctx.type_template);
    ObjectFunction::init_template(ctx, &ctx.type_ctx.function_template);
    StaticInt::init_template(ctx, &ctx.type_ctx.static_int_template);
    DynInt::init_template(ctx, &ctx.type_ctx.dynamic_int_template);
    ObjectModule::init_template(ctx, &ctx.type_ctx.module_template);
    ObjectString::init_template(ctx, &ctx.type_ctx.string_template);
}
