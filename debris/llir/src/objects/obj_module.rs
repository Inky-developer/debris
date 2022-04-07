use std::fmt::{self, Debug};

use debris_common::Ident;

use crate::{
    impl_class, memory::MemoryLayout, type_context::TypeContext, ObjectPayload, ObjectProperties,
    ObjectRef, Type, ValidPayload,
};

use super::obj_function::ObjFunction;

/// A module object
///
/// Contains other values, including nested modules.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjModule {
    /// The identifying name of this module
    pub ident: Ident,
    /// The members of this module
    pub members: ObjectProperties,
}

impl_class! {ObjModule, Type::Module, {}}

impl ObjModule {
    /// Creates a new empty module with this name
    pub fn new(name: impl Into<Ident>) -> Self {
        ObjModule {
            ident: name.into(),
            members: ObjectProperties::default(),
        }
    }

    pub fn with_members(name: Ident, members: ObjectProperties) -> Self {
        ObjModule {
            ident: name,
            members,
        }
    }

    /// Returns the ident of this module
    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn members(&self) -> impl Iterator<Item = (&Ident, &ObjectRef)> {
        self.members.iter()
    }

    /// Sets a property
    ///
    /// If it already exists, replaces the old value and returns it
    pub fn set_property<T: Into<Ident>>(&mut self, name: T, value: ObjectRef) -> Option<ObjectRef> {
        self.members.insert(name.into(), value)
    }

    /// Registers a value for the first time
    ///
    /// Panics if it already exists
    pub fn register<T: Into<Ident>>(&mut self, name: T, value: ObjectRef) {
        let old_value = self.set_property(name, value);
        assert!(
            old_value.is_none(),
            "Trying to register a value that already exists"
        );
    }

    /// A more concise way to register builtin functions without having
    /// to declare its name twice.
    pub fn register_function(&mut self, ctx: &TypeContext, function: ObjFunction) {
        self.register(function.name, function.into_object(ctx));
    }
}

impl ObjectPayload for ObjModule {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn get_property(&self, _ctx: &TypeContext, ident: &Ident) -> Option<ObjectRef> {
        self.members.get(ident).cloned()
    }
}

impl fmt::Display for ObjModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "mod '{}'", self.ident)
    }
}
