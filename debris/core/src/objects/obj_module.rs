use std::fmt::{self, Debug};

use debris_common::Ident;
use debris_derive::object;

use crate::{
    function_interface::{ToFunctionInterface, ValidReturnType},
    memory::MemoryLayout,
    CompileContext, ObjectPayload, ObjectProperties, ObjectRef, Type, ValidPayload,
};

use super::obj_function::ObjFunction;

/// A module object
///
/// Contains other values, including nested modules.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjModule {
    /// The identifying name of this module
    ident: Ident,
    /// The members of this module
    members: ObjectProperties,
}

#[object(Type::Module)]
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
        if old_value.is_some() {
            panic!("Trying to register a value that already exists")
        }
    }

    /// Registers a simple api-function whose signature specifies which types are allowed
    ///
    /// # Example:
    /// ```ignore
    /// fn valid_api_function(ctx: &FunctionContext, param1: &ObjInt, param2: ObjString) -> LangResult<()> {
    ///     // [...]
    /// }
    /// ```
    ///
    /// An invalid function does not specify the return type (ie. returns `ObjectRef`).
    /// If the signature does not contain requirements for the parameter types, any function call is valid.
    ///
    /// # Panics
    /// This method panics if the passed function does not specify its return type
    pub fn register_typed_function<I, T, Params, Return>(
        &mut self,
        ctx: &CompileContext,
        name: I,
        value: &'static T,
    ) where
        I: Into<Ident>,
        T: ToFunctionInterface<Params, Return> + 'static,
        Return: ValidReturnType,
    {
        self.register(
            name.into(),
            ObjFunction::new_single(ctx, value).into_object(ctx),
        );
    }
}

impl ObjectPayload for ObjModule {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.members.get(ident).cloned()
    }
}

impl fmt::Display for ObjModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "module '{}'", self.ident)
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
pub struct ModuleFactory {
    factory_fn: &'static dyn Fn(&CompileContext) -> ObjModule,
    /// Whether to import every property of this module on its own
    import_members: bool,
}

impl ModuleFactory {
    pub fn new(
        function: &'static dyn Fn(&CompileContext) -> ObjModule,
        import_members: bool,
    ) -> Self {
        ModuleFactory {
            factory_fn: function,
            import_members,
        }
    }

    pub fn call(&self, ctx: &CompileContext) -> ObjModule {
        (self.factory_fn)(ctx)
    }

    pub fn import_members(&self) -> bool {
        self.import_members
    }
}

impl<F: Fn(&CompileContext) -> ObjModule> From<&'static F> for ModuleFactory {
    fn from(value: &'static F) -> Self {
        ModuleFactory {
            factory_fn: value,
            import_members: false,
        }
    }
}

impl Debug for ModuleFactory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ModuleFactory").finish()
    }
}
