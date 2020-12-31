use crate::{
    error::Result,
    mir::{MirCall, MirContext, MirGotoContext, MirValue, MirVisitor, NamespaceArena},
    ObjectRef,
};

use super::{
    llir_nodes::{Call, Function, Node},
    utils::ItemId,
    LLIRContext,
};

pub(crate) struct LLIRBuilder<'llir, 'ctx, 'arena> {
    context: LLIRContext<'ctx>,
    arena: &'arena mut NamespaceArena,
    nodes: Vec<Node>,
    mir_contexts: &'ctx [MirContext<'ctx>],
    llir_functions: &'llir mut Vec<Function>,
}

impl<'ctx, 'arena, 'llir> LLIRBuilder<'llir, 'ctx, 'arena> {
    pub fn new(
        context: &'ctx MirContext<'ctx>,
        arena: &'arena mut NamespaceArena,
        mir_contexts: &'ctx [MirContext<'ctx>],
        llir_functions: &'llir mut Vec<Function>,
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
            llir_functions,
        }
    }

    pub fn build(mut self) -> Result<Function> {
        let mut result = None;
        for node in self.context.mir_nodes {
            result = Some(self.visit_node(node)?);
        }

        Ok(Function {
            id: self.context.context_id,
            nodes: self.nodes,
            returned_value: result.expect("Empty contexts"),
        })
    }

    fn emit(&mut self, node: Node) {
        self.nodes.push(node);
    }

    fn item_id(&self, id: u64) -> ItemId {
        ItemId {
            context_id: self.context.context_id,
            id,
        }
    }

    /// Converts a `MirValue` into an `ObjectRef`
    ///
    /// Every value should be computed in this stage
    fn get_object(&self, value: &MirValue) -> ObjectRef {
        self.context
            .get_object(self.arena, value)
            .expect("This value was not computed yet. This is a bug in the compiler.")
    }

    /// Returns an object that belongs to this id. None if the object is still a template
    pub fn get_object_by_id(&self, id: u64) -> Option<ObjectRef> {
        self.context.get_object_by_id(self.arena, id)
    }

    /// Updates the template with this id to an object
    fn set_object(&mut self, value: ObjectRef, id: u64) {
        self.context.set_object(self.arena, value, id);
    }
}

impl MirVisitor for LLIRBuilder<'_, '_, '_> {
    type Output = Result<ObjectRef>;

    fn visit_call(&mut self, call: &MirCall) -> Self::Output {
        let parameters = call
            .parameters
            .iter()
            .map(|parameter| self.get_object(parameter))
            .collect::<Vec<_>>();

        let callback = call.function.as_ref();

        let return_id = call
            .return_value
            .expect_template("Return value must be a template")
            .1;

        // Call the function
        let (result, mut nodes) = callback.call(
            &self.context,
            call.span,
            self.arena,
            &parameters,
            self.item_id(return_id),
            self.mir_contexts,
            self.llir_functions,
        )?;

        // Update the template with the correct return value
        if self.get_object_by_id(return_id).is_none() {
            self.set_object(result.clone(), return_id);
        }

        self.nodes.append(&mut nodes);

        Ok(result)
    }

    fn visit_goto_context(&mut self, goto_context: &MirGotoContext) -> Self::Output {
        self.emit(Node::Call(Call {
            id: goto_context.context_id,
        }));

        let is_context_missing = goto_context.context_id != self.context.context_id
            && self
                .llir_functions
                .iter()
                .all(|ctx| ctx.id != goto_context.context_id);

        let result = if is_context_missing {
            // generate that context now if it is not generated yet
            let context = self
                .mir_contexts
                .iter()
                .find(|ctx| ctx.id == goto_context.context_id)
                .expect("This context must exist");
            let llit_builder =
                LLIRBuilder::new(context, self.arena, self.mir_contexts, self.llir_functions);
            let function = llit_builder.build()?;
            let return_value = function.returned_value.clone();
            self.llir_functions.push(function);
            return_value
        } else {
            self.llir_functions
                .iter()
                .find(|function| function.id == goto_context.context_id)
                .map(|function| function.returned_value.clone())
                .unwrap_or_else(|| self.context.compile_context.type_ctx().null())
        };

        Ok(result)
    }
}
