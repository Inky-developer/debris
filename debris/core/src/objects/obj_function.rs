use std::{
    fmt::{self, Debug},
    rc::Rc,
};

use debris_common::Span;
use debris_derive::object;

use crate::llir::memory::MemoryLayout;
use crate::llir::utils::ItemIdAllocator;
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
    pub name: &'static str,
}

#[object(Type::Function)]
impl ObjFunction {
    pub fn new(name: &'static str, callback_function: Rc<DebrisFunctionInterface>) -> Self {
        ObjFunction {
            name,
            callback_function,
        }
    }
}

impl ObjectPayload for ObjFunction {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
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
            .field(&self.name)
            .field(&Rc::as_ptr(&self.callback_function))
            .finish()
    }
}

impl fmt::Display for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Fn({})", self.name)
    }
}

/// The context which gets passed to a function
pub struct FunctionContext<'a> {
    /// Generates new item ids
    pub item_id_allocator: &'a mut ItemIdAllocator,
    /// The id of the returned value
    pub item_id: ItemId,
    /// The nodes that are emitted by this function
    pub nodes: Vec<Node>,
    /// The current span
    pub span: Span,
    /// The compilation context
    pub ctx: &'a CompileContext,
}

impl<'a> FunctionContext<'a> {
    /// Adds a node to the previously emitted nodes
    pub fn emit(&mut self, node: Node) {
        self.nodes.push(node)
    }

    pub fn compile_context(&self) -> &'a CompileContext {
        self.ctx
    }
}
