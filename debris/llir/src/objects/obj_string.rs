use std::{fmt, ops::Deref, rc::Rc};

use crate::{impl_class, memory::MemoryLayout, ObjectPayload, Type};

/// A static string object
///
/// Very basic right now and supports no runtime functionality.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjString {
    value: Rc<str>,
}

impl_class! {ObjString, Type::String, {
    "length" => |value: &ObjString| -> i32 {
        value.value.len().try_into().expect("What is that tuple???")
    }
}}

impl ObjString {
    pub fn new(value: Rc<str>) -> Self {
        ObjString { value }
    }

    pub fn value(&self) -> Rc<str> {
        self.value.clone()
    }
}

impl From<Rc<str>> for ObjString {
    fn from(value: Rc<str>) -> Self {
        ObjString { value }
    }
}

impl Deref for ObjString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl ObjectPayload for ObjString {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}

impl fmt::Display for ObjString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}
