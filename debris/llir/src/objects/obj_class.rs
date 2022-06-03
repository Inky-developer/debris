use std::{fmt, ops::Deref};

use fmt::Debug;

use crate::{
    class::{ClassKind, ClassRef},
    impl_class,
    memory::MemoryLayout,
    type_context::TypeContext,
    ObjectPayload, ObjectProperties, Type,
};

/// Marks objects that have a class
///
/// Every object payload has to implement this trait.
pub trait HasClass {
    /// Returns all properties that belong to this class
    fn create_properties(ctx: &TypeContext) -> ObjectProperties
    where
        Self: Sized;

    /// Returns the static class that belongs to this Object
    fn static_class(ctx: &TypeContext) -> ClassRef
    where
        Self: Sized;
}

/// The class of a value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjClass {
    pub class: ClassRef,
}

impl_class! {ObjClass, Type::Type, {}}

impl ObjClass {
    pub fn new(value: ClassRef) -> Self {
        ObjClass { class: value }
    }
}

impl Deref for ObjClass {
    type Target = ClassRef;

    fn deref(&self) -> &Self::Target {
        &self.class
    }
}

impl From<ClassRef> for ObjClass {
    fn from(value: ClassRef) -> Self {
        ObjClass::new(value)
    }
}

impl ObjectPayload for ObjClass {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    // A bit hacky way to get members functions (etc.) from structs
    fn get_property(
        &self,
        _ctx: &TypeContext,
        ident: &debris_common::Ident,
    ) -> Option<crate::ObjectRef> {
        match &self.class.kind {
            ClassKind::Struct(strukt) | ClassKind::StructValue(strukt) => strukt
                .namespace
                .get()
                .and_then(|map| map.get(ident).cloned()),
            ClassKind::Tuple(_)
            | ClassKind::TupleValue(_)
            | ClassKind::Function(_)
            | ClassKind::Type(_) => None,
        }
    }
}

impl fmt::Display for ObjClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.class, f)
    }
}
