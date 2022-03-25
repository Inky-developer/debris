use std::{iter::zip, rc::Rc};

use debris_common::Ident;
use debris_error::{LangErrorKind, LangResult};

use crate::{
    class::{Class, ClassKind, ClassRef},
    impl_class,
    json_format::JsonFormatComponent,
    memory::MemoryLayout,
    objects::{obj_class::ObjClass, obj_function::FunctionContext, obj_int_static::ObjStaticInt},
    type_context::TypeContext,
    ObjectPayload, ObjectRef, Type,
};

use super::obj_class::HasClass;

pub type TupleRef = Rc<Tuple>;

#[derive(Debug, PartialEq, Eq)]
pub struct Tuple {
    pub layout: Vec<ClassRef>,
}

impl Tuple {
    /// Returns whether the other tuple matches this tuple,
    /// where this tuple is a pattern
    pub fn matches(&self, other: &Tuple) -> bool {
        self.layout.len() == other.layout.len()
            && zip(&self.layout, &other.layout).all(|(pat, got)| pat.matches(got))
    }

    /// Returns whether every type contained in this tuple
    /// can be encoded at runtime.
    pub fn runtime_encodable(&self) -> bool {
        self.layout
            .iter()
            .all(|class| class.kind.runtime_encodable())
    }

    /// Returns true if any of the contained fields of this tuple
    /// should be const
    pub fn comptime_encodable(&self) -> bool {
        self.layout
            .iter()
            .any(|class| class.kind.comptime_encodable())
    }
}

impl From<Vec<ClassRef>> for Tuple {
    fn from(values: Vec<ClassRef>) -> Self {
        Tuple { layout: values }
    }
}

impl std::fmt::Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.layout.as_slice() {
            [] => write!(f, "()"),
            [single] => write!(f, "({single})"),
            [first, rest @ ..] => {
                write!(f, "({first}")?;
                for value in rest {
                    write!(f, ", {value}")?;
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
#[derive(PartialEq, Eq)]
pub struct ObjTupleObject {
    pub class: TupleRef,
    pub values: Vec<ObjectRef>,
    memory_layout: MemoryLayout,
}

impl_class! {ObjTupleObject, Type::TupleObject, {
    "get" => |ctx: &FunctionContext, index: &ObjStaticInt| -> Option<LangResult<ObjectRef>> {
        let this = ctx.self_value_as::<ObjTupleObject>()?;

        let index = index.value;
        Some(index.try_into().ok().and_then(|idx: usize| this.values.get(idx).cloned()).ok_or(
            LangErrorKind::IndexOutOfBounds { index, max: this.values.len() as i64 }
        ))
    },

    "length" => |ctx: &FunctionContext| -> Option<i32> {
        let this = ctx.self_value_as::<ObjTupleObject>()?;
        Some(this.values.len().try_into().expect("Tuple contains too many elements"))
    },

    Promote => |ctx: &mut FunctionContext, this: &ObjTupleObject, target: &ObjClass| -> Option<LangResult<ObjectRef>> {
        match &target.class.kind {
            ClassKind::Tuple(tuple) | ClassKind::TupleObject { tuple } => {
                if tuple.layout.len() != this.values.len() {
                    return None;
                }

                let mut promoted_values = Vec::with_capacity(this.values.len());
                for (value, target) in zip(&this.values, &tuple.layout) {
                    // Check if the value must be promoted and if so, try to promote it
                    if value.class.matches_exact(target) {
                        promoted_values.push(value.clone());
                    } else {
                        let promoted = match ctx.promote_obj(value.clone(), ObjClass::new(target.clone()).into_object(ctx.type_ctx())) {
                            Some(Ok(value)) => value,
                            other => return other,
                        };
                        promoted_values.push(promoted);
                    }
                }

                let promoted_tuple = ObjTupleObject::new(promoted_values);
                Some(Ok(promoted_tuple.into_object(ctx.type_ctx())))
            }
            _ => None,
        }
    }
}}

impl ObjTupleObject {
    pub fn new(values: Vec<ObjectRef>) -> Self {
        let memory_layout = values
            .iter()
            .map(|obj| obj.payload.memory_layout())
            .collect();

        let type_patterns = values.iter().map(|value| value.class.clone()).collect();
        let class = Rc::new(Tuple {
            layout: type_patterns,
        });

        ObjTupleObject {
            class,
            values,
            memory_layout,
        }
    }

    pub fn length(&self) -> usize {
        self.values.len()
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

    fn get_property(&self, _ctx: &TypeContext, ident: &Ident) -> Option<ObjectRef> {
        match ident {
            Ident::Value(_) | Ident::Special(_) => None,
            Ident::Index(idx) => self.values.get(*idx).cloned(),
        }
    }

    fn json_fmt(&self, buf: &mut Vec<JsonFormatComponent>) {
        buf.push(JsonFormatComponent::RawText("(".into()));

        let mut iter = self.values.iter();
        if let Some(value) = iter.next() {
            value.payload.json_fmt(buf);
            let sep = ", ".into();
            for value in iter {
                buf.push(JsonFormatComponent::RawText(Rc::clone(&sep)));
                value.payload.json_fmt(buf);
            }
        }

        buf.push(JsonFormatComponent::RawText(")".into()));
    }
}

impl std::fmt::Display for ObjTupleObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tuple({})", self.class)
    }
}

impl std::fmt::Debug for ObjTupleObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ObjTupleObject")
            .field("values", &self.values)
            .finish_non_exhaustive()
    }
}
