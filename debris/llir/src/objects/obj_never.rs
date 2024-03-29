use std::fmt;

use crate::{impl_class, memory::MemoryLayout, ObjectPayload, Type};

/// Signals that a block will never return.
/// The most interesting property of this type is its ability to match any other type.
/// This does not circumvent the entire type system, because any code that depends on
/// a value with the `Never` type must not be evaluated (in llir).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ObjNever;

impl_class! {ObjNever, Type::Never, {}}

impl ObjNever {}

impl ObjectPayload for ObjNever {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}

impl fmt::Display for ObjNever {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Never")
    }
}
