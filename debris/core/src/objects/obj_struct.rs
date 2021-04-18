use std::{fmt, hash::BuildHasherDefault, rc::Rc};

use debris_common::Ident;
use debris_derive::object;
use generational_arena::Index;
use indexmap::IndexMap;
use rustc_hash::FxHasher;

use crate::{
    class::{Class, ClassKind, ClassRef},
    memory::MemoryLayout,
    CompileContext, ObjectPayload, Type, TypePattern,
};

pub type StructRef = Rc<Struct>;

#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub ident: Ident,
    /// The fields are stored in an indexmap so that the user defined
    /// order is preserved. Uses the fast [FxHasher]
    pub fields: IndexMap<Ident, TypePattern, BuildHasherDefault<FxHasher>>,
    /// Each struct has a namespace containing its objects (methods, ...)
    pub properties: Index,
}

impl Struct {
    pub fn runtime_encodable(&self) -> bool {
        self.fields.values().all(|value| match value {
            TypePattern::Any => false,
            TypePattern::Class(cls) => cls.kind.pattern_runtime_encodable(),
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
