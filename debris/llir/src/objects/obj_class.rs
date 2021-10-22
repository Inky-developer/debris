use std::{fmt, ops::Deref};

use debris_common::Ident;
use fmt::Debug;

use crate::{
    class::ClassRef, impl_class, memory::MemoryLayout, type_context::TypeContext, ObjectPayload,
    ObjectRef, Type,
};

/// Marks objects that have a class
///
/// Every object payload has to implement this trait.
pub trait HasClass {
    /// Returns the class of this object
    ///
    /// Usually auto-implement by the proc macro `#[object]`
    fn class(ctx: &TypeContext) -> ClassRef
    where
        Self: Sized;
}

/// The class of a value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjClass {
    pub class: ClassRef,
}

impl_class! {ObjClass, Type::Class, {}}

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

    fn get_property(&self, ctx: &TypeContext, ident: &Ident) -> Option<ObjectRef> {
        self.class.get_property(ctx, ident)
    }
}

impl fmt::Display for ObjClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{{{}}}", self.class))
    }
}
