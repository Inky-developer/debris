use std::{fmt, hash::BuildHasherDefault, rc::Rc};

use debris_common::Ident;
use indexmap::IndexMap;
use itertools::Itertools;
use once_cell::unsync::OnceCell;
use rustc_hash::FxHasher;

use crate::{
    class::{Class, ClassKind, ClassRef},
    impl_class,
    memory::MemoryLayout,
    type_context::TypeContext,
    ObjectPayload, ObjectProperties, Type,
};

pub type StructRef = Rc<Struct>;

#[derive(PartialEq, Eq)]
pub struct Struct {
    pub ident: Ident,
    /// The fields are stored in an indexmap so that the user defined
    /// order is preserved. Uses the fast [FxHasher]
    pub fields: IndexMap<Ident, ClassRef, BuildHasherDefault<FxHasher>>,
    /// Namespace is in a once cell, because the struct must be created before
    /// its members can be added
    pub namespace: OnceCell<ObjectProperties>,
}

impl Struct {
    pub fn diverges(&self) -> bool {
        self.fields.values().any(|value| value.diverges())
    }

    pub fn runtime_encodable(&self) -> bool {
        self.fields.values().all(|value| {
            value
                .kind
                .as_value()
                .expect("Field classes of a struct can never be values")
                .runtime_encodable()
        })
    }

    pub fn comptime_encodable(&self) -> bool {
        self.fields
            .values()
            .any(|value| value.kind.comptime_encodable())
    }
}

impl fmt::Debug for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Struct")
            .field("ident", &self.ident)
            .field("fields", &self.fields)
            .field(
                "namespace",
                &self
                    .namespace
                    .get()
                    .map(|namespace| namespace.keys().collect_vec()),
            )
            .finish()
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

    fn get_property(&self, _ctx: &TypeContext, ident: &Ident) -> Option<crate::ObjectRef> {
        self.namespace
            .get()
            .expect("Evaluated struct property during struct initialization")
            .get(ident)
            .cloned()
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
