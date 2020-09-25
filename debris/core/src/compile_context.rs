use debris_type::Type;
use std::{default::Default, rc::Rc};

use crate::objects::ObjectTemplate;
use crate::objects::{function_template, integer_template, type_template};
use crate::Config;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct CompileContext {
    pub type_ctx: TypeContext,
    pub config: Config,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TypeContext {
    int_template: Rc<ObjectTemplate>,
    function_template: Rc<ObjectTemplate>,
    type_template: Rc<ObjectTemplate>,
}

impl CompileContext {}

impl TypeContext {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn template_for_type(&self, typ: &Type) -> Rc<ObjectTemplate> {
        match typ {
            Type::Type => self.type_template.clone(),
            Type::Bool => todo!(),
            Type::Fixed => todo!(),
            Type::Function => self.function_template.clone(),
            Type::Int => self.int_template.clone(),
            Type::Template(_type) => todo!(),
        }
    }
}

impl Default for TypeContext {
    fn default() -> Self {
        TypeContext {
            int_template: Rc::new(integer_template()),
            function_template: Rc::new(function_template()),
            type_template: Rc::new(type_template()),
        }
    }
}
