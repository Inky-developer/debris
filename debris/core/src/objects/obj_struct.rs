use std::{fmt, rc::Rc};

use debris_common::Ident;
use debris_derive::object;
use rustc_hash::FxHashMap;

use crate::{
    class::{Class, ClassKind, ClassRef},
    memory::MemoryLayout,
    CompileContext, ObjectPayload, ObjectProperties, ObjectRef, Type, TypePattern,
};

pub type StructRef = Rc<Struct>;

#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub ident: Ident,
    pub fields: FxHashMap<Ident, TypePattern>,
    pub properties: ObjectProperties,
}

impl Struct {
    pub fn runtime_encodable(&self) -> bool {
        self.fields.values().all(|value| match value {
            TypePattern::Any => false,
            TypePattern::Class(cls) => cls.kind.runtime_encodable(),
        })
    }

    pub fn should_be_const(&self) -> bool {
        self.fields.values().any(|value| match value {
            TypePattern::Any => true,
            TypePattern::Class(class) => class.kind.should_be_const(),
        })
    }
}

/// Stores a user defined struct
#[derive(Debug, PartialEq, Eq)]
pub struct ObjStruct {
    pub struct_ref: StructRef,
}

#[object(Type::Struct)]
impl ObjStruct {
    pub fn new(strukt: StructRef) -> Self {
        ObjStruct { struct_ref: strukt }
    }
}

impl ObjectPayload for ObjStruct {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.struct_ref.properties.get(ident).cloned()
    }

    fn create_class(&self, _: &CompileContext) -> ClassRef {
        let kind = ClassKind::Struct(self.struct_ref.clone());
        let class = Class::new_empty(kind);
        ClassRef::new(class)
    }
}

impl std::ops::Deref for ObjStruct {
    type Target = StructRef;

    fn deref(&self) -> &Self::Target {
        &self.struct_ref
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("struct {} {{ .. }}", self.ident))
    }
}

impl fmt::Display for ObjStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.struct_ref, f)
    }
}
