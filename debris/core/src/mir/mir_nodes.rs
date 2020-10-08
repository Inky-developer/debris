use debris_common::{Ident, LocalSpan};
use debris_type::Type;

use std::fmt::Debug;

use crate::{llir::utils::ItemId, objects::TypeRef, ObjectRef};

#[derive(Eq, PartialEq, Clone)]
pub enum MirValue {
    Concrete(ObjectRef),
    Template { id: u64, template: TypeRef },
}

#[derive(Debug, Eq, PartialEq)]
pub enum MirNode {
    Call {
        span: LocalSpan,
        value: ObjectRef,
        parameters: Vec<MirValue>,
        return_value: MirValue,
    },
    RawCommand {
        value: MirValue,
        var_id: ItemId,
    },
}

impl MirValue {
    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        match self {
            MirValue::Concrete(object_ref) => object_ref.get_property(ident),
            MirValue::Template { id: _, template } => template.get_property(ident),
        }
    }

    pub fn typ(&self) -> &Type {
        match self {
            MirValue::Concrete(object_ref) => &object_ref.typ,
            MirValue::Template { id: _, template } => match template.value_typ() {
                Type::Template(correct_type) => correct_type.as_ref(),
                other @ _ => other,
            },
        }
    }

    pub fn as_object(&self) -> Option<ObjectRef> {
        match self {
            MirValue::Concrete(obj) => Some(obj.clone()),
            MirValue::Template { id: _, template: _ } => None,
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
                .field("template.type", &template.value_typ())
                .finish(),
        }
    }
}
