use debris_derive::object;
use std::ops::Deref;

use crate::{ObjectPayload, Type};

/// A static string object
///
/// Very basic right now and supports no runtime functionality.
#[derive(Debug, Eq, PartialEq)]
pub struct ObjectString {
    value: String,
}

#[object(Type::String)]
impl ObjectString {}

impl From<String> for ObjectString {
    fn from(value: String) -> Self {
        ObjectString { value }
    }
}

impl Deref for ObjectString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl ObjectPayload for ObjectString {}
