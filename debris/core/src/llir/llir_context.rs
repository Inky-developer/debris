use std::rc::Rc;

use debris_common::{Code, LocalSpan, Span};

use crate::{
    mir::{MirNode, MirValue},
    CompileContext, ObjectRef,
};

#[derive(Debug)]
pub(crate) struct LLIRContext<'a> {
    /// The source code which contains this context
    pub(crate) code: Rc<Code>,
    /// The previous mir nodes
    pub(crate) mir_nodes: &'a Vec<MirNode>,
    /// All objects
    pub(crate) objects: Vec<MirValue>,
    /// The current context
    pub(crate) compile_context: Rc<CompileContext>,
    /// The id of this context
    pub(crate) context_id: u64,
}

impl<'a> LLIRContext<'a> {
    pub fn get_object(&self, value: &MirValue) -> Option<ObjectRef> {
        match value {
            MirValue::Concrete(obj) => Some(obj.clone()),
            MirValue::Template {
                id,
                template: _template,
            } => match self.objects.get(*id as usize) {
                Some(MirValue::Concrete(obj)) => Some(obj.clone()),
                Some(MirValue::Template { id: _, template: _ }) | None => None,
            },
        }
    }

    pub fn set_object(&mut self, value: ObjectRef, index: usize) {
        if self.objects.len() <= index {
            panic!(
                "Could not replace object at index {}, vec has only {} element(s)",
                index,
                self.objects.len()
            );
        }

        self.objects[index] = MirValue::Concrete(value);
    }

    pub fn as_span(&self, local_span: LocalSpan) -> Span {
        Span {
            code: self.code.clone(),
            local_span,
        }
    }
}
