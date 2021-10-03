//! This module defines classes.
//! The concept of a class should be similar to the equivalent concept
//! in other languages.
//! Every value (`object`) has a class.
//! Builtin values have a class with an associated type, while
//! user defined classes, like structs, carry a reference to the concrete struct.

use std::{cell::RefCell, fmt, rc::Rc};

use debris_common::Ident;

use crate::{
    llir::utils::ItemIdAllocator,
    objects::{
        obj_bool::ObjBool, obj_int::ObjInt, obj_never::ObjNever, obj_null::ObjNull,
        obj_struct::StructRef, obj_tuple_object::TupleRef,
    },
    CompileContext, ObjectProperties, ObjectRef, Type, ValidPayload,
};

pub type ClassRef = Rc<Class>;

// A class is either backed by a builtin [Type] or a custom struct implementation
#[derive(Debug, PartialEq, Eq)]
pub enum ClassKind {
    Type(Type),
    Struct(StructRef),
    StructObject { strukt: StructRef },
    Tuple(TupleRef),
    TupleObject { tuple: TupleRef },
    Function,
}

impl ClassKind {
    pub fn get_property(&self, _ctx: &CompileContext, _ident: &Ident) -> Option<ObjectRef> {
        match self {
            ClassKind::StructObject { strukt: _ } => {
                todo!()
            }
            ClassKind::Struct(_strukt) => {
                todo!()
            }
            _ => None,
        }
    }

    /// Returns whether the other class kind matches this class kind if this is interpreted as a pattern.
    /// For example, A struct object can match on a struct, if the underlying struct is equal.
    pub fn matches(&self, other: &ClassKind) -> bool {
        match other {
            ClassKind::Type(typ) => self.matches_type(*typ),
            ClassKind::StructObject { strukt } => {
                if let ClassKind::Struct(other_strukt) = self {
                    strukt == other_strukt
                } else {
                    false
                }
            }
            ClassKind::Struct(_) => {
                matches!(self, ClassKind::Type(Type::Struct))
            }
            ClassKind::TupleObject { tuple } => {
                if let ClassKind::Tuple(other_tuple) = self {
                    other_tuple.matches(tuple)
                } else {
                    self.typ() == Type::TupleObject
                }
            }
            ClassKind::Tuple(_) => matches!(self, ClassKind::Type(Type::Tuple)),
            ClassKind::Function {} => {
                todo!()
            }
        }
    }

    pub fn diverges(&self) -> bool {
        match self {
            ClassKind::Function {..} | ClassKind::Struct(_) | ClassKind::StructObject { .. } | ClassKind::Tuple(_) | ClassKind::TupleObject{..} => false,
            ClassKind::Type(typ) => typ.diverges()
        }
    }

    /// Returns whether `other` is the same class as self
    /// Behavior is the same as testing for equality,
    /// but `StructObject` does not compare the namespace,
    /// since it does not change the type
    pub fn matches_exact(&self, other: &ClassKind) -> bool {
        match (self, other) {
            (
                ClassKind::StructObject { strukt },
                ClassKind::StructObject {
                    strukt: strukt_other,
                },
            ) => strukt == strukt_other,
            (ClassKind::TupleObject { tuple }, ClassKind::TupleObject { tuple: tuple_other }) => {
                tuple.layout == tuple_other.layout
            }
            _ => self == other,
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
            ClassKind::StructObject { strukt } => strukt.comptime_encodable(),
            ClassKind::Tuple(_) => Type::Tuple.comptime_encodable(),
            ClassKind::TupleObject { tuple } => tuple.comptime_encodable(),
            ClassKind::Function { .. } => Type::Function.comptime_encodable(),
        }
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

    /// Returns `true` if the class_kind is [`ClassKind::Function`].
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

    /// Returns whether the other class has the same type as this class or diverges.
    pub fn matches_exact(&self, other: &Class) -> bool {
        self.kind.matches_exact(&other.kind)
    }

    /// Whether it is impossible to construct a value of this class
    pub fn diverges(&self) -> bool {
        self.kind.diverges()
    }

    pub fn get_property(&self, _ctx: &CompileContext, ident: &Ident) -> Option<ObjectRef> {
        self.properties.borrow().get(ident).cloned()
    }

    pub fn set_property(&self, ident: Ident, obj_ref: ObjectRef) {
        self.properties.borrow_mut().insert(ident, obj_ref);
    }

    // TODO: Should not be none for structs and tuples which contain runtime values
    pub fn new_obj_from_allocator(
        &self,
        ctx: &CompileContext,
        allocator: &mut ItemIdAllocator,
    ) -> Option<ObjectRef> {
        match self.kind {
            ClassKind::Function { .. }
            | ClassKind::Struct(_)
            | ClassKind::StructObject { .. }
            | ClassKind::Tuple(_)
            | ClassKind::TupleObject { .. } => None,
            ClassKind::Type(typ) => match typ {
                Type::Class
                | Type::ComptimeBool
                | Type::ComptimeInt
                | Type::FormatString
                | Type::Function
                | Type::Module
                | Type::String
                | Type::Struct
                | Type::StructObject
                | Type::Tuple
                | Type::TupleObject => None,
                Type::DynamicBool => Some(ObjBool::new(allocator.next_id()).into_object(ctx)),
                Type::DynamicInt => Some(ObjInt::new(allocator.next_id()).into_object(ctx)),
                Type::Never => Some(ObjNever.into_object(ctx)),
                Type::Null => Some(ObjNull.into_object(ctx)),
            },
        }
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
            ClassKind::StructObject { strukt } => write!(f, "{}", strukt),
            ClassKind::Tuple(tuple) => fmt::Display::fmt(tuple, f),
            ClassKind::TupleObject { tuple } => write!(f, "{}", tuple),
            ClassKind::Function {} => {
                write!(f, "fn (")?;
                write!(f, "ToDo")?;
                write!(f, ") -> ToDo")
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
