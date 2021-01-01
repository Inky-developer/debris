use crate::{
    error::Result,
    function_interface::FunctionCall,
    mir::{MirCall, MirContext, MirGotoContext, MirValue, MirVisitor, NamespaceArena},
    ObjectRef,
};

use super::{
    llir_nodes::{Call, Function, Node},
    utils::ItemId,
    LLIRContext, LlirHelper,
};

pub(crate) struct LLIRBuilder<'llir, 'ctx, 'arena> {
    context: LLIRContext<'ctx>,
    arena: &'arena mut NamespaceArena,
    nodes: Vec<Node>,
    mir_contexts: &'ctx [MirContext<'ctx>],
    llir_helper: &'llir mut LlirHelper,
    pub(crate) function_id: usize,
}

impl<'ctx, 'arena, 'llir> LLIRBuilder<'llir, 'ctx, 'arena> {
    /// Creates a new `LLIRBuilder`. when building, populates the `llir_helper` struct.
    pub fn new(
        context: &'ctx MirContext<'ctx>,
        arena: &'arena mut NamespaceArena,
        mir_contexts: &'ctx [MirContext<'ctx>],
        llir_helper: &'llir mut LlirHelper,
    ) -> Self {
        let llir_context = LLIRContext {
            code: context.code,
            mir_nodes: &context.nodes,
            namespace_idx: context.namespace_idx,
            compile_context: context.compile_context,
            context_id: context.id,
        };

        LLIRBuilder {
            context: llir_context,
            arena,
            nodes: Vec::new(),
            mir_contexts,
            function_id: llir_helper.next_id(),
            llir_helper,
        }
    }

    /// Populates the `llir_functions` vector
    ///
    /// Returns the last expression and the id of the function
    pub fn build(mut self) -> Result<ObjectRef> {
        let mut result = None;
        for node in self.context.mir_nodes {
            result = Some(self.visit_node(node)?);
        }
        let result = result.unwrap_or_else(|| self.context.compile_context.type_ctx().null());

        let function = Function {
            function_id: self.function_id,
            context_id: self.context.context_id,
            nodes: self.nodes,
            returned_value: result.clone(),
        };

        self.llir_helper.push(function);

        Ok(result)
    }

    fn emit(&mut self, node: Node) {
        self.nodes.push(node);
    }

    /// Converts a `MirValue` into an `ObjectRef`
    ///
    /// Every value should be computed in this stage
    fn get_object(&self, value: &MirValue) -> ObjectRef {
        self.context
            .get_object(self.arena, self.mir_contexts, value)
            .expect("This value was not computed yet. This is a bug in the compiler.")
    }

    /// Returns an object that belongs to this id. None if the object is still a template
    pub fn get_object_by_id(&self, id: ItemId) -> Option<ObjectRef> {
        let context_id = self
            .mir_contexts
            .iter()
            .find(|ctx| ctx.id == id.context_id)
            .expect("Invalid id")
            .namespace_idx;
        self.arena
            .get_by_id(id.id, context_id)
            .and_then(|value| value.concrete())
    }

    /// Updates the template with this id to an object
    fn set_object(&mut self, value: ObjectRef, id: ItemId) {
        self.context
            .set_object(self.arena, self.mir_contexts, value, id);
    }
}

impl MirVisitor for LLIRBuilder<'_, '_, '_> {
    type Output = Result<ObjectRef>;

    fn visit_call(&mut self, call: &MirCall) -> Self::Output {
        println!("{:?}", call);
        let parameters = call
            .parameters
            .iter()
            .map(|parameter| self.get_object(parameter))
            .collect::<Vec<_>>();
        println!("Called!\n");
        let callback = call.function.as_ref();

        let return_id = call
            .return_value
            .expect_template("Return value must be a template")
            .1;

        // Call the function
        let (result, mut nodes) = callback.call(FunctionCall {
            llir_ctx: &self.context,
            span: call.span,
            arena: self.arena,
            parameters: &parameters,
            id: return_id,
            mir_contexts: self.mir_contexts,
            llir_helper: self.llir_helper,
        })?;

        // Update the template with the correct return value
        if self.get_object_by_id(return_id).is_none() {
            self.set_object(result.clone(), return_id);
        }

        self.nodes.append(&mut nodes);

        Ok(result)
    }

    fn visit_goto_context(&mut self, goto_context: &MirGotoContext) -> Self::Output {
        let is_context_missing = goto_context.context_id != self.context.context_id;

        let (function_id, result) = if is_context_missing {
            // generate that context now if it is not generated yet
            let context = self
                .mir_contexts
                .iter()
                .find(|ctx| ctx.id == goto_context.context_id)
                .expect("This context must exist");
            let llir_builder =
                LLIRBuilder::new(context, self.arena, self.mir_contexts, self.llir_helper);
            (llir_builder.function_id, llir_builder.build()?)
        } else {
            // if the context is not missing it is either already generated or the current context.
            // Because no context gets called after it was already generated, it must be the current context.
            // However the result of this context is not yet known - so return null
            // For now this behaviour is fine, but it can be change if this gets a problem
            (
                self.function_id,
                self.context.compile_context.type_ctx().null(),
            )
        };

        self.emit(Node::Call(Call { id: function_id }));

        Ok(result)
    }
}
