use debris_common::{Ident, LocalSpan};
use debris_type::Type;

use std::fmt::Debug;
use std::rc::Rc;

use super::ItemIdentifier;
use crate::objects::ObjectTemplate;
use crate::ObjectRef;

pub type TemplateRef = Rc<ObjectTemplate>;

#[derive(Eq, PartialEq, Clone)]
pub enum MirValue {
    Concrete(ObjectRef),
    Template { id: u64, template: TemplateRef },
}

#[derive(Debug, Eq, PartialEq)]
pub enum MirNode {
    Define {
        span: LocalSpan,
        item: ItemIdentifier,
    },
    Call {
        span: LocalSpan,
        value: ObjectRef,
        parameters: Vec<MirValue>,
        return_value: MirValue,
    },
}

impl MirValue {
    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        match self {
            MirValue::Concrete(object_ref) => object_ref.get_property(ident),
            MirValue::Template { id: _, template } => template.get_property(ident).cloned(),
        }
    }

    pub fn typ(&self) -> &Type {
        match self {
            MirValue::Concrete(object_ref) => &object_ref.typ,
            MirValue::Template { id: _, template } => match template.get_type() {
                Type::Template(correct_type) => &correct_type,
                other @ _ => other,
            },
        }
    }
}

impl From<ObjectRef> for MirValue {
    fn from(value: ObjectRef) -> Self {
        MirValue::Concrete(value)
    }
}

impl Debug for MirValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MirValue::Concrete(value) => f.write_fmt(format_args!("{:?}", value)),
            MirValue::Template { id, template } => f
                .debug_struct("TemplatedValue")
                .field("id", id)
                .field("template.type", template.get_type())
                .finish(),
        }
    }
}
