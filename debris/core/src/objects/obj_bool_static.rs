use debris_derive::{object, ObjectCopy};

use crate::{memory::MemoryLayout, CompileContext, ObjectPayload, Type};

#[derive(Debug, Eq, PartialEq, ObjectCopy, Clone)]
pub struct ObjStaticBool {
    value: bool,
}

#[object(Type::StaticBool)]
impl ObjStaticBool {
    pub fn value(&self) -> bool {
        self.value
    }
}

impl ObjectPayload for ObjStaticBool {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Zero
    }
}

impl From<bool> for ObjStaticBool {
    fn from(value: bool) -> Self {
        ObjStaticBool { value }
    }
}
