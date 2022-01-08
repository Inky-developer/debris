use std::{
    fmt::{self, Debug},
    rc::Rc,
};

use debris_common::{Span, SpecialIdent};
use debris_error::LangResult;

use crate::{
    function_interface::DebrisFunctionInterface,
    impl_class,
    item_id::{ItemId, ItemIdAllocator},
    llir_function_builder::FunctionBuilderRuntime,
    llir_nodes::Node,
    memory::MemoryLayout,
    type_context::TypeContext,
    ObjectPayload, ObjectRef, Type,
};

/// A function object
///
/// Has a map of available signatures.
/// The call parameters are unique identifiers for every signature
#[derive(Clone)]
pub struct ObjFunction {
    pub name: &'static str,
    pub callback_function: Rc<DebrisFunctionInterface>,
}

impl_class! {ObjFunction, Type::Function, {}}

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

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionClass {
    pub parameters: Vec<ObjectRef>,
    pub return_class: ObjectRef,
}

impl fmt::Display for FunctionClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn(")?;
        let mut iter = self.parameters.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first)?;
        }
        for rest in iter {
            write!(f, ", {}", rest)?;
        }
        write!(f, ")")?;

        if !self.return_class.class.kind.is_null() {
            write!(f, " -> {}", self.return_class)?;
        }
        Ok(())
    }
}

/// The context which gets passed to a function
pub struct FunctionContext<'a> {
    /// Generates new item ids
    pub item_id_allocator: &'a mut ItemIdAllocator,
    /// The id of the returned value
    pub item_id: ItemId,
    /// The parameters for this function call, excluding the self value
    pub parameters: &'a [ObjectRef],
    /// The self value
    pub self_val: Option<ObjectRef>,
    /// The nodes that are emitted by this function
    pub nodes: Vec<Node>,
    pub span: Span,
    pub type_ctx: &'a TypeContext,
    pub runtime: &'a mut FunctionBuilderRuntime,
}

impl<'a> FunctionContext<'a> {
    /// Adds a node to the previously emitted nodes
    pub fn emit(&mut self, node: Node) {
        self.nodes.push(node);
    }

    /// Returns `self_val` downcasted to the desired type or None
    pub fn self_value_as<T: ObjectPayload>(&self) -> Option<&T> {
        self.self_val
            .as_ref()
            .and_then(|self_val| self_val.downcast_payload::<T>())
    }

    /// Generates a new function context which can be used for calling another function.
    pub fn with_new_function_context<T>(
        &mut self,
        parameters: &[ObjectRef],
        self_value: Option<ObjectRef>,
        f: impl FnOnce(&mut FunctionContext) -> T,
    ) -> (T, Vec<Node>) {
        let mut inner_ctx = FunctionContext {
            item_id: self.item_id_allocator.next_id(),
            item_id_allocator: self.item_id_allocator,
            parameters,
            self_val: self_value,
            nodes: Vec::new(),
            span: self.span,
            type_ctx: self.type_ctx,
            runtime: self.runtime,
        };
        let result = f(&mut inner_ctx);

        (result, inner_ctx.nodes)
    }

    pub fn call_function(
        &mut self,
        function: &ObjFunction,
        parameters: &[ObjectRef],
        self_value: Option<ObjectRef>,
    ) -> LangResult<ObjectRef> {
        let raw_result = self.call_function_raw(function, parameters, self_value);
        function
            .callback_function
            .handle_raw_result(self, raw_result)
    }

    pub fn call_function_raw(
        &mut self,
        function: &ObjFunction,
        parameters: &[ObjectRef],
        self_value: Option<ObjectRef>,
    ) -> Option<LangResult<ObjectRef>> {
        let (result, nodes) = self.with_new_function_context(parameters, self_value, |ctx| {
            function.callback_function.call_raw(ctx)
        });
        if let Some(Ok(_)) = result {
            self.nodes.extend(nodes);
        }
        result
    }

    pub fn promote_obj(
        &mut self,
        value: ObjectRef,
        target: ObjectRef,
    ) -> Option<LangResult<ObjectRef>> {
        let obj_fn = value.get_property(self.type_ctx, &SpecialIdent::Promote.into())?;
        let promote_fn = obj_fn.downcast_payload()?;
        self.call_function_raw(promote_fn, &[value, target], None)
    }
}
