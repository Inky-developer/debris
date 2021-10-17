use debris_derive::object;
use std::rc::Rc;

use crate::{
    class::{Class, ClassKind, ClassRef},
    memory::MemoryLayout,
    type_context::TypeContext,
    ObjectPayload, Type, TypePattern,
};

use super::obj_class::HasClass;

pub type TupleRef = Rc<Tuple>;

#[derive(Debug, PartialEq, Eq)]
pub struct Tuple {
    pub layout: Vec<TypePattern>,
}

impl Tuple {
    /// Returns whether the other tuple matches this tuple,
    /// where this tuple is a pattern
    pub fn matches(&self, other: &Tuple) -> bool {
        self.layout.len() == other.layout.len()
            && self
                .layout
                .iter()
                .zip(other.layout.iter())
                .all(|(pat, got)| {
                    let class = match got {
                        TypePattern::Any => unreachable!(),
                        TypePattern::Class(class) => &**class,
                    };
                    pat.matches(class)
                })
    }

    /// Returns whether every type contained in this tuple
    /// can be encoded at runtime.
    pub fn runtime_encodable(&self) -> bool {
        self.layout.iter().all(|field| match field {
            TypePattern::Any => false,
            TypePattern::Class(class) => class.kind.runtime_encodable(),
        })
    }

    /// Returns true if any of the contained fields of this tuple
    /// should be const
    pub fn comptime_encodable(&self) -> bool {
        self.layout.iter().any(|value| match value {
            TypePattern::Any => true,
            TypePattern::Class(class) => class.kind.comptime_encodable(),
        })
    }
}

impl From<Vec<TypePattern>> for Tuple {
    fn from(values: Vec<TypePattern>) -> Self {
        Tuple { layout: values }
    }
}

impl std::fmt::Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.layout.as_slice() {
            [] => write!(f, "()"),
            [single] => write!(f, "({})", single),
            [first, rest @ ..] => {
                write!(f, "({}", first)?;
                for value in rest {
                    write!(f, ", {}", value)?;
                }
                write!(f, ")")
            }
        }
    }
}

/// A heterogenous, compile-time known collection
/// of objects.
/// Todo: Consider making the construction of tuples which share all but
/// a few elements with another tuple cheaper.
#[derive(Debug, PartialEq, Eq)]
pub struct ObjTupleObject {
    pub tuple: TupleRef,
    memory_layout: MemoryLayout,
}

#[object(Type::TupleObject)]
impl ObjTupleObject {
    pub fn new(_tuple: TupleRef) -> Self {
        todo!()
    }
}

impl ObjectPayload for ObjTupleObject {
    fn memory_layout(&self) -> &MemoryLayout {
        &self.memory_layout
    }

    fn create_class(&self, ctx: &TypeContext) -> ClassRef {
        let class_kind = ClassKind::TupleObject {
            tuple: self.tuple.clone(),
        };
        let class = Class {
            kind: class_kind,
            properties: Self::class(ctx).properties.clone(),
        };
        ClassRef::from(class)
    }
}

impl std::fmt::Display for ObjTupleObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tuple({})", self.tuple)
    }
}
