use std::{
    cell::{Ref, RefCell},
    fmt::{self, Display},
    hash::{Hash, Hasher},
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
pub type ClassRef = Rc<ObjClass>;

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

/// The class of a value.
///
/// Contains all associated methods.
/// As of right now, only the typ is used for the hasher.
/// Once classes get more sophisticated, this has to be updated or it will lead
/// to strange bugs,
#[derive(Debug, Eq, Clone)]
pub struct ObjClass {
    typ: Type,
    properties: RefCell<ObjectProperties>,
}

#[object(Type::Class)]
impl ObjClass {
    /// Constructs a new class with a `typ` and class properties
    pub fn new(typ: Type, properties: ObjectProperties) -> Self {
        ObjClass {
            typ,
            properties: properties.into(),
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

    pub fn get_properties(&self) -> Ref<ObjectProperties> {
        self.properties.borrow()
    }

    /// Constructs a new class with a `typ`
    pub fn new_empty(typ: Type) -> Self {
        Self::new(typ, ObjectProperties::default())
    }

    /// Returns whether this class is the same class as `other`
    pub fn is(&self, other: &ObjClass) -> bool {
        self.typ == other.typ
    }

    pub fn typ(&self) -> Type {
        self.typ
    }
}

impl ObjectPayload for ObjClass {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Unsized
    }
}

impl fmt::Display for ObjClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{{{}}}", self.typ))
    }
}

impl PartialEq for ObjClass {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

impl Hash for ObjClass {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.typ.hash(state);
    }
}

pub type GenericClassRef = Rc<GenericClass>;

pub struct GenericClass {
    class: Weak<ObjClass>,
    generics: FxHashMap<String, Vec<TypePattern>>,
}

impl GenericClass {
    pub fn new(class: &Rc<ObjClass>) -> Self {
        GenericClass {
            class: Rc::downgrade(class),
            generics: Default::default(),
        }
    }

    pub fn into_class_ref(self) -> Rc<Self> {
        Rc::new(self)
    }

    pub fn class(&self) -> ClassRef {
        self.class.upgrade().expect("Must be valid")
    }

    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.class().get_property(ident)
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
