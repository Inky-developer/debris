use debris_derive::object;
use itertools::Itertools;
use std::fmt;

use crate::{memory::MemoryLayout, mir::MirValue, CompileContext, ObjectPayload, Type};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FormatStringComponent {
    String(String),
    Value(MirValue),
}

/// A static string object
///
/// Very basic right now and supports no runtime functionality.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjFormatString {
    pub components: Vec<FormatStringComponent>,
}

#[object(Type::FormatString)]
impl ObjFormatString {
    pub fn new(value: Vec<FormatStringComponent>) -> Self {
        ObjFormatString { components: value }
    }
}

impl From<String> for ObjFormatString {
    fn from(value: String) -> Self {
        ObjFormatString::new(vec![FormatStringComponent::String(value)])
    }
}

impl ObjectPayload for ObjFormatString {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Unsized
    }
}

impl fmt::Display for ObjFormatString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "`{}`",
            self.components
                .iter()
                .map(|component| match component {
                    FormatStringComponent::String(string) => string.clone(),
                    FormatStringComponent::Value(obj) => format!("{:?}", obj),
                })
                .format(", ")
        ))
    }
}
