use std::{
    fmt::{self, Debug},
    rc::Rc,
};

use debris_common::Span;
use debris_derive::object;

use crate::llir::memory::MemoryLayout;
use crate::{
    function_interface::DebrisFunctionInterface,
    llir::{llir_nodes::Node, utils::ItemId},
    CompileContext, ObjectPayload, Type,
};

/// A function object
///
/// Has a map of available signatures.
/// The call parameters are unique identifiers for every signature
#[derive(Clone)]
pub struct ObjFunction {
    pub callback_function: Rc<DebrisFunctionInterface>,
    /// A debug name, to allow for
    /// a simpler mir debug representation
    pub debug_name: &'static str,
}

#[object(Type::Function)]
impl ObjFunction {
    pub fn new(debug_name: &'static str, callback_function: Rc<DebrisFunctionInterface>) -> Self {
        ObjFunction {
            debug_name,
            callback_function,
        }
    }
}

impl ObjectPayload for ObjFunction {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn as_function(&self) -> Option<&ObjFunction> {
        Some(self)
    }
}

impl PartialEq for ObjFunction {
    fn eq(&self, other: &ObjFunction) -> bool {
        Rc::ptr_eq(&self.callback_function, &other.callback_function)
    }
}

impl Eq for ObjFunction {}

impl Debug for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ObjectFunction")
            .field(&self.debug_name)
            .field(&Rc::as_ptr(&self.callback_function))
            .finish()
    }
}

impl fmt::Display for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Fn({})", self.debug_name)
    }
}

/// The context which gets passed to a function
pub struct FunctionContext {
    /// The id for the returned value
    pub item_id: ItemId,
    /// The current span
    pub span: Span,
}

impl FunctionContext {
    /// Adds a node to the previously emitted nodes
    pub fn emit(&mut self, _node: Node) {
        todo!()
    }

    pub fn compile_context(&self) -> &CompileContext {
        todo!()
    }
}
