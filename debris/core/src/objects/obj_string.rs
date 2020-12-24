use debris_derive::object;
use std::ops::Deref;

use crate::{ObjectPayload, Type};

/// A static string object
///
/// Very basic right now and supports no runtime functionality.
#[derive(Debug, Eq, PartialEq)]
pub struct ObjString {
    value: String,
}

#[object(Type::String)]
impl ObjString {
    pub fn new(value: String) -> Self {
        ObjString::from(value)
    }
}

impl From<String> for ObjString {
    fn from(value: String) -> Self {
        ObjString { value }
    }
}

impl Deref for ObjString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl ObjectPayload for ObjString {}
