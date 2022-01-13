use std::{fmt, hash::BuildHasherDefault, rc::Rc};

use debris_common::Ident;
use debris_mir::namespace::MirLocalNamespaceId;
use indexmap::IndexMap;
use rustc_hash::FxHasher;

use crate::{
    class::{Class, ClassKind, ClassRef},
    impl_class,
    memory::MemoryLayout,
    type_context::TypeContext,
    ObjectPayload, Type,
};

pub type StructRef = Rc<Struct>;

#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub ident: Ident,
    /// The fields are stored in an indexmap so that the user defined
    /// order is preserved. Uses the fast [FxHasher]
    pub fields: IndexMap<Ident, ClassRef, BuildHasherDefault<FxHasher>>,
    /// Stores a reference to the mir namespace so that properties access on this strukt can be resolved
    pub mir_namespace: MirLocalNamespaceId,
}

impl Struct {
    pub fn runtime_encodable(&self) -> bool {
        self.fields
            .values()
            .all(|value| value.kind.pattern_runtime_encodable())
    }

    pub fn comptime_encodable(&self) -> bool {
        self.fields
            .values()
            .any(|value| value.kind.comptime_encodable())
    }
}

/// Stores a user defined struct
#[derive(Debug, PartialEq, Eq)]
pub struct ObjStruct {
    pub struct_ref: StructRef,
}

impl_class! {ObjStruct, Type::Struct, {}}

impl ObjStruct {
    pub fn new(strukt: StructRef) -> Self {
        ObjStruct { struct_ref: strukt }
    }
}

impl ObjectPayload for ObjStruct {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn create_class(&self, _: &TypeContext) -> ClassRef {
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
        let ident = &self.ident;
        write!(f, "struct {ident} {{ .. }}")
    }
}

impl fmt::Display for ObjStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.struct_ref, f)
    }
}
