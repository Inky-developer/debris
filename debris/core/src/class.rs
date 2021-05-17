//! This module defines classes.
//! The concept of a class should be similar to the equivalent concept
//! in other languages.
//! Every value (`object`) has a class.
//! Builtin values have a class with an associated type, while
//! user defined classes, like structs, carry a reference to the concrete struct.

use std::{cell::RefCell, fmt, rc::Rc};

use debris_common::Ident;

use crate::{
    llir::utils::ItemId,
    mir::{MirValue, NamespaceArena, NamespaceIndex},
    objects::{
        obj_function::FunctionParameters, obj_struct::StructRef, obj_struct_object::memory_ids,
        obj_tuple_object::TupleRef,
    },
    ObjectProperties, ObjectRef, Type, TypePattern,
};

pub type ClassRef = Rc<Class>;

// A class is either backed by a builtin [Type] or a custom struct implementation
#[derive(Debug, PartialEq, Eq)]
pub enum ClassKind {
    Type(Type),
    Struct(StructRef),
    StructObject {
        strukt: StructRef,
        namespace: NamespaceIndex,
    },
    Tuple(TupleRef),
    TupleObject {
        tuple: TupleRef,
        namespace: NamespaceIndex,
    },
    Function {
        parameters: FunctionParameters,
        return_value: TypePattern,
    },
}

impl ClassKind {
    pub fn get_property(&self, arena: &NamespaceArena, ident: &Ident) -> Option<MirValue> {
        match self {
            ClassKind::StructObject { strukt, namespace } => {
                let namespace = arena.get(*namespace);
                namespace
                    .get(arena, ident)
                    .map(|(_, entry)| entry.value().clone())
                    .or_else(|| ClassKind::Struct(strukt.clone()).get_property(arena, ident))
            }
            ClassKind::Struct(strukt) => {
                let namespace = arena.get(strukt.properties);
                namespace
                    .get(arena, ident)
                    .map(|(_, entry)| entry.value().clone())
            }
            _ => None,
        }
    }

    pub fn matches(&self, other: &ClassKind) -> bool {
        match other {
            ClassKind::Type(typ) => self.matches_type(*typ),
            ClassKind::StructObject {
                strukt,
                namespace: _,
            } => {
                if let ClassKind::Struct(other_strukt) = self {
                    strukt == other_strukt
                } else {
                    false
                }
            }
            ClassKind::Struct(_) => {
                matches!(self, ClassKind::Type(Type::Struct))
            }
            ClassKind::TupleObject {
                tuple,
                namespace: _,
            } => {
                if let ClassKind::Tuple(other_tuple) = self {
                    other_tuple.matches(tuple)
                } else {
                    false
                }
            }
            ClassKind::Tuple(_) => matches!(self, ClassKind::Type(Type::Tuple)),
            ClassKind::Function {
                parameters,
                return_value,
            } => {
                if let ClassKind::Function {
                    parameters: own_parameters,
                    return_value: own_return_value,
                } = &self
                {
                    own_return_value.matches(return_value.expect_class("Invalid right pattern"))
                        && own_parameters.matches_function_parameters(parameters)
                } else {
                    false
                }
            }
        }
    }

    /// Returns whether `other` is the same class as self
    /// Behavior is the same as testing for equality,
    /// but `StructObject` does not compare the namespace,
    /// since it does not change the type
    pub fn matches_exact(&self, other: &ClassKind) -> bool {
        match (self, other) {
            (
                ClassKind::StructObject {
                    strukt,
                    namespace: _,
                },
                ClassKind::StructObject {
                    strukt: strukt_other,
                    namespace: _,
                },
            ) => strukt == strukt_other,
            (
                ClassKind::TupleObject {
                    tuple,
                    namespace: _,
                },
                ClassKind::TupleObject {
                    tuple: tuple_other,
                    namespace: _,
                },
            ) => tuple.layout == tuple_other.layout,
            _ => self == other,
        }
    }

    pub fn memory_ids<'a>(
        &self,
        buf: &mut Vec<ItemId>,
        arena: &'a NamespaceArena,
        default: ItemId,
    ) {
        match self {
            ClassKind::StructObject { namespace, .. } => {
                let namespace = arena.get(*namespace);
                memory_ids(buf, arena, namespace)
            }
            _ => buf.push(default),
        }
    }

    pub fn typ(&self) -> Type {
        match self {
            ClassKind::Type(typ) => *typ,
            ClassKind::Struct(_) => Type::Struct,
            ClassKind::StructObject { .. } => Type::StructObject,
            ClassKind::Tuple(_) => Type::Tuple,
            ClassKind::TupleObject { .. } => Type::TupleObject,
            ClassKind::Function { .. } => Type::Function,
        }
    }

    /// Returns whether this class kind is of type [Type::Never]
    pub fn is_never(&self) -> bool {
        matches!(self, ClassKind::Type(Type::Never))
    }

    /// Returns whether this type can be fully encoded at runtime.
    pub fn runtime_encodable(&self) -> bool {
        match self {
            ClassKind::Type(typ) => typ.runtime_encodable(),
            ClassKind::Struct(_) => Type::Struct.runtime_encodable(),
            ClassKind::StructObject { strukt, .. } => strukt.runtime_encodable(),
            ClassKind::Tuple(_) => Type::Tuple.runtime_encodable(),
            ClassKind::TupleObject { tuple, .. } => tuple.runtime_encodable(),
            ClassKind::Function { .. } => Type::Function.runtime_encodable(),
        }
    }

    /// Yeah...
    pub fn pattern_runtime_encodable(&self) -> bool {
        match self {
            ClassKind::Type(typ) => typ.runtime_encodable(),
            ClassKind::Struct(strukt) => strukt.runtime_encodable(),
            ClassKind::Tuple(tuple) => tuple.runtime_encodable(),
            ClassKind::StructObject { .. } | ClassKind::TupleObject { .. } => {
                unreachable!("Strukt or tuple objects are never a pattern")
            }
            ClassKind::Function { .. } => Type::Function.runtime_encodable(),
        }
    }

    /// Returns whether this type can be fully encoded at compile time.
    pub fn comptime_encodable(&self) -> bool {
        match self {
            ClassKind::Type(typ) => typ.comptime_encodable(),
            ClassKind::Struct(_) => Type::Struct.comptime_encodable(),
            ClassKind::StructObject {
                namespace: _,
                strukt,
            } => strukt.comptime_encodable(),
            ClassKind::Tuple(_) => Type::Tuple.comptime_encodable(),
            ClassKind::TupleObject {
                namespace: _,
                tuple,
            } => tuple.comptime_encodable(),
            ClassKind::Function { .. } => Type::Function.comptime_encodable(),
        }
    }

    /// Returns `true` if the class_kind is [`Struct`].
    pub fn is_struct(&self) -> bool {
        matches!(self, Self::Struct(..))
    }

    pub fn matches_type(&self, typ: Type) -> bool {
        match self {
            &ClassKind::Type(own_type) => typ.matches(&own_type),
            ClassKind::Struct(_) => typ == Type::Struct,
            ClassKind::StructObject { .. } => typ == Type::StructObject,
            ClassKind::Tuple(_) => typ == Type::Tuple,
            ClassKind::TupleObject { .. } => typ == Type::TupleObject,
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

    pub fn matches_exact(&self, other: &Class) -> bool {
        self.kind.matches_exact(&other.kind)
    }

    pub fn get_property(&self, arena: &NamespaceArena, ident: &Ident) -> Option<MirValue> {
        self.properties
            .borrow()
            .get(ident)
            .map(|obj| MirValue::Concrete(obj.clone()))
            .or_else(|| self.kind.get_property(arena, ident))
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
            ClassKind::StructObject {
                strukt,
                namespace: _,
            } => write!(f, "{}", strukt),
            ClassKind::Tuple(tuple) => fmt::Display::fmt(tuple, f),
            ClassKind::TupleObject {
                tuple,
                namespace: _,
            } => write!(f, "{}", tuple),
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
        f.write_fmt(format_args!("{}", self.kind))
    }
}

// /// Since [Class] has a [RefCell] to other classes, circular dependencies will happen.
// /// To avoid memory leaks, destroy all properties here
// impl Drop for Class {
//     fn drop(&mut self) {
//         self.pro
//     }
// }
