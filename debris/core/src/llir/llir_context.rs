use std::rc::Rc;

use debris_common::{Code, LocalSpan, Span};

use crate::{
    mir::{MirNode, MirValue},
    CompileContext, ObjectRef,
};

/// A specific llir context
///
/// Similar to mir contexts, but a bit simpler.
/// Borrows MirNodes from an actual MirContext.
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
    /// Returns an object that corresponds to a `MirValue`
    ///
    /// If the objects is not yet computed, returns None.
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

    /// Replaces a `MirValue` with the given index with an actual value
    pub fn set_object(&mut self, value: ObjectRef, index: usize) {
        if self.objects.len() <= index {
            panic!(
                "Could not replace object at index {}, vec has only {} element(s)",
                index,
                self.objects.len()
            );
        }

        if let MirValue::Concrete(_) = self.objects[index] {
            panic!("Expected a template, got a concrete value");
        }

        self.objects[index] = MirValue::Concrete(value);
    }

    /// Converts a `LocalSpan` into a `Span`
    pub fn as_span(&self, local_span: LocalSpan) -> Span {
        Span {
            code: self.code.clone(),
            local_span,
        }
    }
}
