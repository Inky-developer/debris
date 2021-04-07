use std::fmt;

use debris_common::Ident;
use debris_derive::object;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    memory::MemoryLayout, CompileContext, ObjectPayload, ObjectProperties, ObjectRef, Type,
    TypePattern,
};

/// Stores a user defined struct
#[derive(Debug, PartialEq, Eq)]
pub struct ObjStruct {
    /// The name of this struct
    pub ident: Ident,
    pub fields: FxHashMap<Ident, TypePattern>,
    /// Other properties of this struct, including
    /// Methods.
    pub properties: ObjectProperties,
}

#[object(Type::Struct)]
impl ObjStruct {}

impl ObjectPayload for ObjStruct {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Unsized
    }

    fn get_property(&self, ident: &Ident) -> Option<ObjectRef> {
        self.properties.get(ident).cloned()
    }
}

impl fmt::Display for ObjStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "struct {} {{ {} }}",
            self.ident,
            self.fields
                .iter()
                .map(|(name, pattern)| format!("{}: {}", name, pattern))
                .join(", ")
        ))
    }
}
