use debris_common::SpecialIdent;
use debris_type::Type;
use std::{any::Any, rc::Rc};

use super::{function_template, ObjectFunction, ObjectTemplate};
use crate::{
    error::Result, llir::llir_nodes::Node, CompileContext, DebrisObject, ObjectPayload,
    ObjectProperties, ObjectRef,
};

macro_rules! map {
    ($($k:expr => $v:expr),+) => {
        {
            let mut map = ObjectProperties::default();
            $(
                map.insert($k, $v);
            )+
            map
        }
    };
}

#[derive(Debug, Eq, PartialEq)]
pub struct ObjectInteger {
    pub value: i32,
}

pub fn integer_template() -> ObjectTemplate {
    fn add_wrapper(
        ctx: Rc<CompileContext>,
        values: Vec<ObjectRef>,
    ) -> Result<(ObjectRef, Vec<Node>)> {
        Ok(add(
            &ctx,
            values[0].downcast_payload().unwrap(),
            values[1].downcast_payload().unwrap(),
        ))
    }

    fn sub_wrapper(
        ctx: Rc<CompileContext>,
        values: Vec<ObjectRef>,
    ) -> Result<(ObjectRef, Vec<Node>)> {
        Ok(sub(
            &ctx,
            values[0].downcast_payload().unwrap(),
            values[1].downcast_payload().unwrap(),
        ))
    }

    fn mul_wrapper(
        ctx: Rc<CompileContext>,
        values: Vec<ObjectRef>,
    ) -> Result<(ObjectRef, Vec<Node>)> {
        Ok(mul(
            &ctx,
            values[0].downcast_payload().unwrap(),
            values[1].downcast_payload().unwrap(),
        ))
    }

    fn div_wrapper(
        ctx: Rc<CompileContext>,
        values: Vec<ObjectRef>,
    ) -> Result<(ObjectRef, Vec<Node>)> {
        Ok(div(
            &ctx,
            values[0].downcast_payload().unwrap(),
            values[1].downcast_payload().unwrap(),
        ))
    }

    let hacky_function_template = Rc::new(function_template());

    let add_object = DebrisObject::new(
        hacky_function_template.clone(),
        ObjectFunction::new(vec![Type::Int, Type::Int], Type::Int, add_wrapper),
    );

    let sub_object = DebrisObject::new(
        hacky_function_template.clone(),
        ObjectFunction::new(vec![Type::Int, Type::Int], Type::Int, sub_wrapper),
    );

    let mul_object = DebrisObject::new(
        hacky_function_template.clone(),
        ObjectFunction::new(vec![Type::Int, Type::Int], Type::Int, mul_wrapper),
    );

    let div_object = DebrisObject::new(
        hacky_function_template.clone(),
        ObjectFunction::new(vec![Type::Int, Type::Int], Type::Int, div_wrapper),
    );

    let properties_map = map![
        SpecialIdent::Add.into() => add_object,
        SpecialIdent::Sub.into() => sub_object,
        SpecialIdent::Mul.into() => mul_object,
        SpecialIdent::Div.into() => div_object
    ];

    ObjectTemplate::new(Type::Int, properties_map)
}

impl ObjectInteger {
    pub fn new<T: Into<i32>>(value: T) -> Self {
        ObjectInteger {
            value: value.into(),
        }
    }
}

impl ObjectPayload for ObjectInteger {
    fn as_any(&self) -> &dyn Any {
        self as &dyn Any
    }

    fn typ(&self) -> Type {
        Type::Int
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new(ctx.type_ctx.template_for_type(&self.typ()), self).into()
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
    }
}

fn add(ctx: &CompileContext, a: &ObjectInteger, b: &ObjectInteger) -> (ObjectRef, Vec<Node>) {
    (
        ObjectInteger {
            value: a.value + b.value,
        }
        .into_object(ctx),
        Vec::new(),
    )
}

fn sub(ctx: &CompileContext, a: &ObjectInteger, b: &ObjectInteger) -> (ObjectRef, Vec<Node>) {
    (
        ObjectInteger {
            value: a.value - b.value,
        }
        .into_object(ctx),
        Vec::new(),
    )
}

fn mul(ctx: &CompileContext, a: &ObjectInteger, b: &ObjectInteger) -> (ObjectRef, Vec<Node>) {
    (
        ObjectInteger {
            value: a.value * b.value,
        }
        .into_object(ctx),
        Vec::new(),
    )
}

fn div(ctx: &CompileContext, a: &ObjectInteger, b: &ObjectInteger) -> (ObjectRef, Vec<Node>) {
    (
        ObjectInteger {
            value: a.value / b.value,
        }
        .into_object(ctx),
        Vec::new(),
    )
}
