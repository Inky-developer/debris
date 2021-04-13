//! This module defines classes.
//! The concept of a class should be similar to the equivalent concept
//! in other languages.
//! Every value (`object`) has a class.
//! Builtin values have a class with an associated type, while
//! user defined classes, like structs, carry a reference to the concrete struct.

use std::{cell::RefCell, fmt, rc::Rc};

use debris_common::Ident;

use crate::{
    objects::{obj_function::FunctionParameters, obj_struct::StructRef},
    ObjectProperties, ObjectRef, Type, TypePattern,
};

pub type ClassRef = Rc<Class>;

// A class is either backed by a builtin [Type] or a custom struct implementation
#[derive(Debug, PartialEq, Eq)]
pub enum ClassKind {
    Type(Type),
    Struct(StructRef),
    Function {
        parameters: FunctionParameters,
        return_value: TypePattern,
    },
}

impl ClassKind {
    pub fn matches(&self, other: &ClassKind) -> bool {
        match self {
            ClassKind::Type(typ) => other.is_type(*typ),
            _ => self == other,
        }
    }

    pub fn runtime_encodable(&self) -> bool {
        match self {
            ClassKind::Type(typ) => typ.runtime_encodable(),
            ClassKind::Struct(strukt) => strukt.runtime_encodable(),
            ClassKind::Function { .. } => false,
        }
    }

    pub fn should_be_const(&self) -> bool {
        match self {
            ClassKind::Type(typ) => typ.should_be_const(),
            ClassKind::Struct(strukt) => strukt.should_be_const(),
            ClassKind::Function { .. } => Type::Function.should_be_const(),
        }
    }

    /// Returns `true` if the class_kind is [`Struct`].
    pub fn is_struct(&self) -> bool {
        matches!(self, Self::Struct(..))
    }

    pub fn is_type(&self, typ: Type) -> bool {
        match self {
            &ClassKind::Type(own_type) => own_type == typ,
            ClassKind::Struct(_) => typ == Type::Struct,
            ClassKind::Function { .. } => typ == Type::Function,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            ClassKind::Type(typ) => typ.is_bool(),
            _ => false,
        }
    }

    /// Returns `true` if the class_kind is [`Function`].
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function { .. })
    }
}

/// A class combines [ClassKind] and corresponding methods.
/// Due to the way classes get initialized, the methods have to be stored
/// in a [RefCell].
/// For more info see [crate::compile_context::TypeContext] and [debris_derive::object].
#[derive(Debug, PartialEq, Eq)]
pub struct Class {
    pub kind: ClassKind,
    pub properties: RefCell<ObjectProperties>,
}

impl Class {
    pub fn new_empty(kind: ClassKind) -> Self {
        Class {
            kind,
            properties: Default::default(),
        }
    }

    pub fn matches(&self, other: &Class) -> bool {
        self.kind.matches(&other.kind)
    }

    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.properties.borrow().get(ident).cloned()
    }

    pub fn set_property(&self, ident: Ident, obj_ref: ObjectRef) {
        self.properties.borrow_mut().insert(ident, obj_ref);
    }
}

impl From<Type> for ClassKind {
    fn from(value: Type) -> Self {
        ClassKind::Type(value)
    }
}

impl fmt::Display for ClassKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ClassKind::Type(typ) => fmt::Display::fmt(typ, f),
            ClassKind::Struct(strukt) => fmt::Display::fmt(strukt, f),
            ClassKind::Function {
                parameters,
                return_value,
            } => {
                write!(f, "fn (")?;
                match parameters {
                    FunctionParameters::Any => write!(f, "Any")?,
                    FunctionParameters::Specific(parameters) => {
                        let mut iter = parameters.iter();
                        if let Some(next) = iter.next() {
                            fmt::Display::fmt(next, f)?
                        }
                        for parameter in iter {
                            write!(f, ", ")?;
                            fmt::Display::fmt(parameter, f)?
                        }
                    }
                }
                write!(f, ") -> {}", return_value)
            }
        }
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{{{}}}", self.kind))
    }
}

// /// Since [Class] has a [RefCell] to other classes, circular dependencies will happen.
// /// To avoid memory leaks, destroy all properties here
// impl Drop for Class {
//     fn drop(&mut self) {
//         self.pro
//     }
// }
