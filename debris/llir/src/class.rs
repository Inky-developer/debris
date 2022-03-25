//! TODO: Clean this mess up
//! This module defines classes.
//! The concept of a class should be similar to the equivalent concept
//! in other languages.
//! Every value (`object`) has a class.
//! Builtin values have a class with an associated type, while
//! user defined classes, like structs, carry a reference to the concrete struct.

use std::{fmt, rc::Rc};

use debris_common::Ident;
use once_cell::unsync::OnceCell;
use rustc_hash::FxHashMap;

use crate::{
    item_id::ItemIdAllocator,
    objects::{
        obj_bool::ObjBool,
        obj_function::FunctionClassRef,
        obj_int::ObjInt,
        obj_never::ObjNever,
        obj_null::ObjNull,
        obj_struct::StructRef,
        obj_struct_object::ObjStructObject,
        obj_tuple_object::{ObjTupleObject, TupleRef},
    },
    ObjectProperties, ObjectRef, Type, ValidPayload,
};

use super::type_context::TypeContext;

pub type ClassRef = Rc<Class>;

/// An enum over every type that exists in debris.
/// Either a simple [`Type`] or a complex type with additional data.
///
/// Most complex types come in two variants: As a pattern and as a value.
/// A pattern could be e.g. a struct which may be required in a function definition as parameter.
/// The value would then be the struct object that can actually be passed to that function.
#[derive(Debug, PartialEq, Eq)]
pub enum ClassKind {
    Type(Type),
    Struct(StructRef),
    StructValue(StructRef),
    Tuple(TupleRef),
    TupleValue(TupleRef),
    Function(FunctionClassRef),
}

impl ClassKind {
    /// Changes all types to their respective objects, e.g. `Struct` => `StructObject`.
    /// This can be used to figure out the [`ClassKind`] that matches on this kind
    ///
    /// Returns [`None`] if this is already an object variant
    pub fn as_value(&self) -> Option<ClassKind> {
        let value = match self {
            ClassKind::StructValue(_) | ClassKind::TupleValue(_) => return None,
            ClassKind::Type(typ) => ClassKind::Type(*typ),
            ClassKind::Struct(strukt) => ClassKind::StructValue(strukt.clone()),
            ClassKind::Tuple(tuple) => ClassKind::TupleValue(tuple.clone()),
            ClassKind::Function(function) => ClassKind::Function(function.clone()),
        };
        Some(value)
    }

    /// Returns whether the other class kind matches this class kind if this is interpreted as a pattern.
    /// For example, a struct object can match on a struct, if the underlying struct is equal.
    pub fn matches(&self, other: &ClassKind) -> bool {
        match (self, other) {
            (ClassKind::Type(typ), other) => typ.matches(other.typ()),
            // Cases when a specific struct is expected and other is an instance of that struct
            (ClassKind::Struct(strukt), ClassKind::StructValue(other_strukt)) => {
                strukt == other_strukt
            }
            (ClassKind::Tuple(tuple), ClassKind::TupleValue(other_tuple)) => {
                tuple.matches(other_tuple)
            }
            (ClassKind::Function(function), ClassKind::Function(other_function)) => {
                function.matches(other_function)
            }
            (ClassKind::Function(_) | ClassKind::Struct(_) | ClassKind::Tuple(_), _) => false,
            (ClassKind::StructValue(_) | ClassKind::TupleValue(_), _) => {
                panic!("Cannot match against concrete objects")
            }
        }
    }

    /// Returns whether this value diverges, aka is impossible to construct.
    /// A diverging class can match on any pattern.
    ///
    /// This is also used for some optimizations
    pub fn diverges(&self) -> bool {
        match self {
            ClassKind::Function(function) => function.diverges(),
            ClassKind::Struct(strukt) | ClassKind::StructValue(strukt) => strukt.diverges(),
            ClassKind::Tuple(tuple) | ClassKind::TupleValue(tuple) => tuple.diverges(),
            ClassKind::Type(typ) => typ.diverges(),
        }
    }

    /// Returns the simple [`Type`] of this [`ClassKind`]
    pub fn typ(&self) -> Type {
        match self {
            ClassKind::Type(typ) => *typ,
            ClassKind::Struct(_) => Type::Struct,
            ClassKind::StructValue(_) => Type::StructObject,
            ClassKind::Tuple(_) => Type::Tuple,
            ClassKind::TupleValue(_) => Type::TupleObject,
            ClassKind::Function(_) => Type::Function,
        }
    }

    /// Returns whether this type can be fully encoded at runtime.
    pub fn runtime_encodable(&self) -> bool {
        match self {
            ClassKind::Type(typ) => typ.runtime_encodable(),
            ClassKind::Struct(_) => Type::Struct.runtime_encodable(),
            ClassKind::StructValue(strukt) => strukt.runtime_encodable(),
            ClassKind::Tuple(_) => Type::Tuple.runtime_encodable(),
            ClassKind::TupleValue(tuple) => tuple.runtime_encodable(),
            ClassKind::Function(_) => Type::Function.runtime_encodable(),
        }
    }

    /// Returns whether this type can be fully encoded at compile time.
    pub fn comptime_encodable(&self) -> bool {
        match self {
            ClassKind::Type(typ) => typ.comptime_encodable(),
            ClassKind::Struct(strukt) | ClassKind::StructValue(strukt) => {
                strukt.comptime_encodable()
            }
            ClassKind::Tuple(tuple) | ClassKind::TupleValue(tuple) => tuple.comptime_encodable(),
            ClassKind::Function(_) => Type::Function.comptime_encodable(),
        }
    }
}

/// A class combines [`ClassKind`] and corresponding properties (Mostly associated methods).
#[derive(PartialEq, Eq)]
pub struct Class {
    pub kind: ClassKind,
    pub properties: OnceCell<ObjectProperties>,
}

impl Class {
    /// Constructs a new class with an empty properties map
    pub fn new_empty(kind: ClassKind) -> Self {
        let cell = OnceCell::new();
        cell.set(ObjectProperties::default()).unwrap();
        Class {
            kind,
            properties: cell,
        }
    }

    /// Returns if the value class `other` matches this pattern
    pub fn matches(&self, other: &Class) -> bool {
        self.kind.matches(&other.kind)
    }

    /// Returns whether the other class has the same type as this class or diverges.
    pub fn matches_type(&self, other: &Class) -> bool {
        self.kind.typ().matches(other.kind.typ())
    }

    /// Whether it is impossible to construct a value of this class
    pub fn diverges(&self) -> bool {
        self.kind.diverges()
    }

    /// Returns a property of this class
    ///
    /// # Panics
    /// panics if this class is not initialized yet
    pub fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.properties
            .get()
            .expect("Cannot get property on uninitialized class")
            .get(ident)
            .cloned()
    }

    /// Constructs a new runtime object that corresponds to this class, if possible
    pub fn new_obj_from_allocator(
        &self,
        ctx: &TypeContext,
        allocator: &ItemIdAllocator,
    ) -> Option<ObjectRef> {
        match &self.kind {
            ClassKind::Function(_) => None,
            ClassKind::Type(typ) => match typ {
                Type::Type
                | Type::Any
                | Type::ComptimeBool
                | Type::ComptimeInt
                | Type::FormatString
                | Type::Function
                | Type::FunctionRef
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
            ClassKind::Tuple(tuple) | ClassKind::TupleValue(tuple) => {
                let values = tuple
                    .layout
                    .iter()
                    .map(|class| class.new_obj_from_allocator(ctx, allocator))
                    .collect::<Option<Vec<_>>>()?;
                let tuple = ObjTupleObject::new(values);
                Some(tuple.into_object(ctx))
            }
            ClassKind::Struct(strukt) | ClassKind::StructValue(strukt) => {
                let mut values = FxHashMap::default();
                values.reserve(strukt.fields.len());
                for (ident, class) in &strukt.fields {
                    let obj = class.new_obj_from_allocator(ctx, allocator)?;
                    values.insert(ident.clone(), obj);
                }
                let strukt_obj = ObjStructObject::new(strukt.clone(), values)
                    .expect("Autogenerated values must be valid");
                Some(strukt_obj.into_object(ctx))
            }
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
            ClassKind::Struct(strukt) => write!(f, "<struct {}>", strukt.ident),
            ClassKind::StructValue(strukt) => fmt::Display::fmt(strukt, f),
            ClassKind::Tuple(tuple) | ClassKind::TupleValue(tuple) => fmt::Display::fmt(tuple, f),
            ClassKind::Function(func) => fmt::Display::fmt(func, f),
        }
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = &self.kind;
        write!(f, "{}", kind)
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Class")
            .field("kind", &self.kind)
            .finish_non_exhaustive()
    }
}
