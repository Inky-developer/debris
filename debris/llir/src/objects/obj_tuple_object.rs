use std::rc::Rc;

use debris_error::LangResult;

use crate::{
    class::{Class, ClassKind, ClassRef},
    impl_class,
    memory::MemoryLayout,
    objects::{obj_class::ObjClass, obj_function::FunctionContext},
    type_context::TypeContext,
    ObjectPayload, ObjectRef, Type, TypePattern,
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
    pub class: TupleRef,
    pub values: Vec<ObjectRef>,
    memory_layout: MemoryLayout,
}

impl_class! {ObjTupleObject, Type::TupleObject, {
    Promote => |ctx: &mut FunctionContext, this: &ObjTupleObject, target: &ObjClass| -> Option<LangResult<ObjectRef>> {
        match &target.class.kind {
            ClassKind::Tuple(tuple) | ClassKind::TupleObject { tuple } => {
                if tuple.layout.len() != this.values.len() {
                    return None;
                }

                let mut promoted_values = Vec::with_capacity(this.values.len());
                for (value, target) in this.values.iter().zip(tuple.layout.iter()) {
                    let target = target.expect_class("Must be a class");

                    // Check if the value must be promoted and if so, try to promote it
                    if value.class.matches_exact(target) {
                        promoted_values.push(value.clone());
                    } else {
                        let promoted = match ctx.promote_obj(value.clone(), ObjClass::new(target.clone()).into_object(ctx.type_ctx)) {
                            Some(Ok(value)) => value,
                            other => return other,
                        };
                        promoted_values.push(promoted);
                    }
                }

                let promoted_tuple = ObjTupleObject::new(promoted_values);
                Some(Ok(promoted_tuple.into_object(ctx.type_ctx)))
            }
            _ => None,
        }
    }
}}

impl ObjTupleObject {
    pub fn new(values: Vec<ObjectRef>) -> Self {
        let mut memory_ids = Vec::new();
        for value in values.iter() {
            match value.payload.memory_layout() {
                MemoryLayout::Unsized => {}
                MemoryLayout::One(id) => memory_ids.push(*id),
                MemoryLayout::Multiple(ids) => memory_ids.extend(ids.iter().copied()),
            }
        }
        let memory_layout = MemoryLayout::Multiple(memory_ids);

        let type_patterns = values
            .iter()
            .map(|value| TypePattern::Class(value.class.clone()))
            .collect();
        let class = Rc::new(Tuple {
            layout: type_patterns,
        });

        ObjTupleObject {
            class,
            values,
            memory_layout,
        }
    }
}

impl ObjectPayload for ObjTupleObject {
    fn memory_layout(&self) -> &MemoryLayout {
        &self.memory_layout
    }

    fn create_class(&self, ctx: &TypeContext) -> ClassRef {
        let class_kind = ClassKind::TupleObject {
            tuple: self.class.clone(),
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
        write!(f, "Tuple({})", self.class)
    }
}
