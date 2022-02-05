use std::{
    fmt::{self, Debug},
    rc::Rc,
};

use debris_common::{Span, SpecialIdent};
use debris_error::{CompileError, LangResult};

use crate::{
    block_id::BlockId,
    function_interface::DebrisFunctionInterface,
    impl_class,
    item_id::{ItemId, ItemIdAllocator},
    llir_function_builder::{FunctionBuilderRuntime, LlirFunctionBuilder},
    llir_nodes::Node,
    memory::MemoryLayout,
    type_context::TypeContext,
    NativeFunctionId, ObjectPayload, ObjectRef, Type,
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
        write!(f, "Builtin '{}'", self.name)
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
            write!(f, "{first}")?;
        }
        for rest in iter {
            write!(f, ", {rest}")?;
        }
        write!(f, ")")?;

        if !self.return_class.class.kind.is_null() {
            write!(f, " -> {}", self.return_class)?;
        }
        Ok(())
    }
}

/// The context which gets passed to a function
pub struct FunctionContext<'llir_builder, 'ctx, 'params> {
    /// The id of the returned value
    pub item_id: ItemId,
    /// The parameters for this function call, excluding the self value
    pub parameters: &'params [ObjectRef],
    /// The self value
    pub self_val: Option<ObjectRef>,
    /// The nodes that are emitted by this function
    pub nodes: Vec<Node>,
    pub span: Span,
    pub(crate) llir_function_builder: &'params mut LlirFunctionBuilder<'llir_builder, 'ctx>,
}

impl<'llir_builder, 'ctx, 'params> FunctionContext<'llir_builder, 'ctx, 'params> {
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
    pub fn with_new_function_context<'a, T>(
        &'a mut self,
        parameters: &'a [ObjectRef],
        self_value: Option<ObjectRef>,
        f: impl FnOnce(&mut FunctionContext<'llir_builder, 'ctx, 'a>) -> T,
    ) -> (T, Vec<Node>) {
        let mut inner_ctx = FunctionContext {
            item_id: self.item_id_allocator().next_id(),
            parameters,
            self_val: self_value,
            nodes: Vec::new(),
            span: self.span,
            llir_function_builder: self.llir_function_builder,
        };
        let result = f(&mut inner_ctx);

        (result, inner_ctx.nodes)
    }

    pub fn item_id_allocator(&self) -> &ItemIdAllocator {
        &self.llir_function_builder.builder.item_id_allocator
    }

    pub fn type_ctx(&self) -> &TypeContext {
        &self.llir_function_builder.builder.type_context
    }

    pub fn runtime_mut(&mut self) -> &mut FunctionBuilderRuntime {
        &mut self.llir_function_builder.pending_runtime_functions
    }

    // TODO: Turn into proper result
    pub fn compile_native_function(
        &mut self,
        function_id: NativeFunctionId,
    ) -> LangResult<BlockId> {
        match self
            .llir_function_builder
            .compile_null_function(function_id, self.span)
        {
            Ok(result) => Ok(result),
            Err(err) => match err {
                CompileError::LangError(lang_error) => Err(lang_error.kind),
                CompileError::ParseError(_) => unreachable!("(I hope)"),
            },
        }
    }

    pub fn call_function<'a>(
        &'a mut self,
        function: &ObjFunction,
        parameters: &'a [ObjectRef],
        self_value: Option<ObjectRef>,
    ) -> LangResult<ObjectRef> {
        let raw_result = self.call_function_raw(function, parameters, self_value);
        function
            .callback_function
            .handle_raw_result(self, raw_result)
    }

    pub fn call_function_raw<'a>(
        &'a mut self,
        function: &ObjFunction,
        parameters: &'a [ObjectRef],
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
        let obj_fn = value.get_property(self.type_ctx(), &SpecialIdent::Promote.into())?;
        let promote_fn = obj_fn.downcast_payload()?;
        self.call_function_raw(promote_fn, &[value, target], None)
    }
}
