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

pub(crate) struct LLIRBuilder<'ctx, 'arena> {
    context: LLIRContext<'ctx>,
    arena: &'arena mut NamespaceArena,
    nodes: Vec<Node>,
}

impl<'ctx, 'arena> LLIRBuilder<'ctx, 'arena> {
    pub fn new(context: &'ctx MirContext<'ctx>, arena: &'arena mut NamespaceArena) -> Self {
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
        }
    }

    pub fn build(mut self) -> Result<Function> {
        for node in self.context.mir_nodes {
            self.visit_node(node)?;
        }

        Ok(Function {
            id: self.context.context_id,
            nodes: self.nodes,
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

    /// Updates the template with this id to an object
    fn set_object(&mut self, value: ObjectRef, id: u64) {
        self.context.set_object(self.arena, value, id);
    }
}

impl MirVisitor for LLIRBuilder<'_, '_> {
    type Output = Result<()>;

    // ToDo: Since the function stuff was already evaluated in mir,
    // send the correct overload with the `MirCall`
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
            &self.context.compile_context,
            call.span,
            &parameters,
            self.item_id(return_id),
        )?;

        // Update the template with the correct return value
        self.set_object(result, return_id);

        self.nodes.append(&mut nodes);

        Ok(())
    }

    fn visit_goto_context(&mut self, goto_context: &MirGotoContext) -> Self::Output {
        // Just emit a call node
        self.emit(Node::Call(Call {
            id: goto_context.context_id,
        }));

        Ok(())
    }
}
