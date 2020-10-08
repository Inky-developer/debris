use std::fmt::Debug;

use debris_common::Ident;
use debris_derive::template;
use debris_type::Type;

use crate::{CompileContext, DebrisObject, ObjectPayload, ObjectProperties, ObjectRef};

use super::{ObjectType, TypeRef};

/// A module object
///
/// Contains other values, including nested modules.
#[derive(Debug)]
pub struct ObjectModule {
    /// The identifying name of this module
    ident: Ident,
    /// The members of this module
    members: ObjectProperties,
}

impl ObjectPayload for ObjectModule {
    fn typ(&self) -> Type {
        Type::Module
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new_ref(ctx.type_ctx.template_for_type(&self.typ()), self)
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other.ident == self.ident)
    }

    fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.members.get(ident).cloned()
    }
}

#[template]
impl ObjectModule {
    /// Creates a new empty module with this name
    pub fn new<T: Into<Ident>>(name: T) -> Self {
        ObjectModule {
            ident: name.into(),
            members: ObjectProperties::default(),
        }
    }

    pub fn template() -> TypeRef {
        ObjectType::new_ref(
            Type::Template(Box::new(Type::Module)),
            ObjectProperties::default(),
            None,
        )
    }

    /// Returns the ident of this module
    pub fn ident(&self) -> &Ident {
        &self.ident
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
        if old_value.is_some() {
            panic!("Trying to register a value that already exists")
        }
    }
}

/// A wrapper function for functions that return modules
///
/// # Examples
/// An example module factory function could look like this:
/// ```ignore
///  pub fn load(ctx: &CompileContext) -> ObjectModule {
///     // Creates a new object
///     let mut obj = ObjectModule::new("foo");
///     // Adds the property `hello_world` as type StaticInt with value 1 to the module
///     obj.register("hello_world", StaticInt::new(1).into_object(ctx));
///     obj
/// }
/// ```
///
/// If this module is passed to the mir, the user can use it:
/// ```debris
/// let my_value = foo.hello_world; // 1
/// ```
pub struct ModuleFactory(&'static dyn Fn(&CompileContext) -> ObjectModule);

impl ModuleFactory {
    pub fn call(&self, ctx: &CompileContext) -> ObjectModule {
        (self.0)(ctx)
    }
}

impl<F: Fn(&CompileContext) -> ObjectModule> From<&'static F> for ModuleFactory {
    fn from(value: &'static F) -> Self {
        ModuleFactory(value)
    }
}

impl Debug for ModuleFactory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ModuleFactory").finish()
    }
}
