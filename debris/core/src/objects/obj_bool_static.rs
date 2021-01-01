use debris_derive::object;

use crate::{ObjectPayload, Type};

#[derive(Debug, Eq, PartialEq)]
pub struct ObjStaticBool {
    value: bool,
}

#[object(Type::StaticBool)]
impl ObjStaticBool {
    pub fn value(&self) -> bool {
        self.value
    }
}

impl ObjectPayload for ObjStaticBool {}

impl From<bool> for ObjStaticBool {
    fn from(value: bool) -> Self {
        ObjStaticBool { value }
    }
}
