use std::{fmt, ops::Deref, rc::Rc};

use crate::{
    impl_class,
    json_format::JsonFormatComponent,
    memory::MemoryLayout,
    objects::{obj_class::ObjClass, obj_format_string::ObjFormatString},
    ObjectPayload, Type,
};

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
    },

    Promote => |value: &ObjString, target: &ObjClass| {
        match target.class.kind.typ() {
            Type::FormatString => {
                Some(Ok(ObjFormatString::from(value.value.clone())))
            }
            _ => None
        }
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

    fn json_fmt(&self, buf: &mut Vec<JsonFormatComponent>) {
        buf.push(JsonFormatComponent::RawText(self.value.clone()));
    }
}

impl fmt::Display for ObjString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}
