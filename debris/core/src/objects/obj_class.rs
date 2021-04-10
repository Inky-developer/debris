use std::{
    cell::RefCell,
    fmt::{self, Display},
    ops::Deref,
    rc::{Rc, Weak},
};

use debris_common::Ident;
use debris_derive::object;
use fmt::Debug;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    memory::MemoryLayout, types::TypePattern, CompileContext, ObjectPayload, ObjectProperties,
    ObjectRef, Type,
};

/// A reference to a class
pub type ClassRef = Rc<Class>;

/// Marks objects that have a class
///
/// Every object payload has to implement this trait.
pub trait HasClass {
    /// Returns the class of this object
    ///
    /// Usually auto-implement by the proc macro `#[object]`
    fn class(ctx: &CompileContext) -> ClassRef
    where
        Self: Sized;
}

#[derive(Debug, Eq)]
pub struct Class {
    typ: Type,
    /// This contains all functions and properties of the default types.
    properties: RefCell<ObjectProperties>,
}

impl Class {
    /// Creates a new `Class`
    pub fn new_empty(typ: Type) -> Self {
        Class {
            typ,
            properties: Default::default(),
        }
    }

    // Clippy thinks self is taken by value
    #[allow(clippy::clippy::wrong_self_convention)]
    pub fn as_generic_ref(self: &Rc<Self>) -> GenericClassRef {
        GenericClass::new(self).into_class_ref()
    }

    /// Retrieves a property of this class
    pub fn get_property(&self, property: &Ident) -> Option<ObjectRef> {
        self.properties.borrow().get(property).cloned()
    }

    /// Changes a property of this class
    pub fn set_property(&self, key: Ident, value: ObjectRef) {
        self.properties.borrow_mut().insert(key, value);
    }
}

/// The class of a value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjClass {
    class: ClassRef,
}

#[object(Type::Class)]
impl ObjClass {}

impl Deref for ObjClass {
    type Target = Rc<Class>;

    fn deref(&self) -> &Self::Target {
        &self.class
    }
}

impl ObjectPayload for ObjClass {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{{{}}}", self.typ))
    }
}

impl fmt::Display for ObjClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.class, f)
    }
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

/// A reference to a class with generic parameters
pub type GenericClassRef = Rc<GenericClass>;

pub struct GenericClass {
    class: Weak<Class>,
    generics: FxHashMap<String, Vec<TypePattern>>,
}

impl GenericClass {
    pub fn new(class: &ClassRef) -> Self {
        GenericClass {
            class: Rc::downgrade(class),
            generics: Default::default(),
        }
    }

    pub fn into_class_ref(self) -> Rc<Self> {
        Rc::new(self)
    }

    #[inline]
    pub fn class(&self) -> ClassRef {
        self.class.upgrade().expect("Must be valid")
    }

    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.class().properties.borrow().get(ident).cloned()
    }

    pub fn typ(&self) -> Type {
        self.class().typ
    }

    /// Whether this class matches the other class
    pub fn matches(&self, other: &GenericClass) -> bool {
        self.typ() == other.typ() && self.generics == other.generics
    }

    pub fn set_generics(&mut self, name: String, patterns: Vec<TypePattern>) {
        self.generics.insert(name, patterns);
    }

    pub fn get_generics(&self, name: &str) -> Option<&[TypePattern]> {
        self.generics.get(name).map(|x| x.as_slice())
    }
}

impl Display for GenericClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn display_default(typ: Type, generics: &FxHashMap<String, Vec<TypePattern>>) -> String {
            let generics = if generics.is_empty() {
                String::new()
            } else {
                format!(
                    "<{}>",
                    generics
                        .iter()
                        .map(|(key, value)| {
                            format!(
                                "{}: {}",
                                key,
                                match value.as_slice() {
                                    [] => unreachable!(),
                                    [one] => format!("{}", one),
                                    multiple => multiple.iter().map(|x| x.to_string()).join(", "),
                                }
                            )
                        })
                        .join(", ")
                )
            };
            format!("{{{}{}}}", typ, generics)
        }

        fn display_function(generics: &FxHashMap<String, Vec<TypePattern>>) -> String {
            let parameters = generics
                .get("In")
                .map(|x| x.iter().map(|param| param.to_string()).join(", "))
                .unwrap_or_else(String::new);

            let return_type = match generics.get("Out") {
                Some(return_type) => format!(" -> {}", return_type[0].to_string()),
                None => String::new(),
            };

            format!("fn({}){}", parameters, return_type)
        }

        f.write_str(&match self.class().typ {
            Type::Function => display_function(&self.generics),
            typ => display_default(typ, &self.generics),
        })
    }
}

impl Debug for GenericClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as Display>::fmt(self, f)
    }
}

impl PartialEq for GenericClass {
    fn eq(&self, other: &GenericClass) -> bool {
        self.class.ptr_eq(&other.class) && self.generics == other.generics
    }
}

impl Eq for GenericClass {}
