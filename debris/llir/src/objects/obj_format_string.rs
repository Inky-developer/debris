use std::{fmt, rc::Rc};

use itertools::Itertools;

use crate::{impl_class, memory::MemoryLayout, ObjectPayload, ObjectRef, Type};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FormatStringComponent {
    String(Rc<str>),
    Value(ObjectRef),
}

/// A static format string object
///
/// Very basic right now and supports no runtime functionality.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjFormatString {
    pub components: Vec<FormatStringComponent>,
}

impl_class! {ObjFormatString, Type::FormatString, {}}

impl ObjFormatString {
    pub fn new(value: Vec<FormatStringComponent>) -> Self {
        ObjFormatString { components: value }
    }
}

impl ObjectPayload for ObjFormatString {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}

impl fmt::Display for ObjFormatString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "`{}`",
            self.components
                .iter()
                .map(|component| match component {
                    FormatStringComponent::String(string) => string.to_string(),
                    FormatStringComponent::Value(obj) => format!("{:?}", obj),
                })
                .format(", ")
        ))
    }
}
