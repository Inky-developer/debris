use crate::{
    error::Result,
    mir::{
        MirCall, MirContext, MirGotoContext, MirRawCommand, MirValue, MirVisitor, NamespaceArena,
    },
    objects::{ObjFunction, ObjString},
    ObjectRef,
};

use super::{
    llir_nodes::{Call, Execute, FastStoreFromResult, Function, Node},
    utils::{ItemId, Scoreboard},
    LLIRContext,
};

pub(crate) struct LLIRBuilder<'ctx, 'code, 'arena> {
    context: LLIRContext<'ctx, 'code>,
    arena: &'arena mut NamespaceArena,
    nodes: Vec<Node>,
}

impl<'ctx, 'code, 'arena> LLIRBuilder<'ctx, 'code, 'arena> {
    pub fn new(context: &'ctx MirContext<'code>, arena: &'arena mut NamespaceArena) -> Self {
        let llir_context = LLIRContext {
            code: context.code,
            mir_nodes: &context.nodes,
            namespace_idx: context.namespace_idx,
            compile_context: context.compile_context.clone(),
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

impl MirVisitor for LLIRBuilder<'_, '_, '_> {
    type Output = Result<()>;

    // ToDo: Since the function stuff was already evaluated in mir,
    // send the correct overload with the `MirCall`
    fn visit_call(&mut self, call: &MirCall) -> Self::Output {
        let parameters = call
            .parameters
            .iter()
            .map(|parameter| self.get_object(parameter))
            .collect::<Vec<_>>();

        let parameter_types = parameters.iter().map(|obj| obj.class.as_ref());

        // Get the raw function object
        let function_object = call
            .value
            .downcast_payload::<ObjFunction>()
            .expect("Only Function Objects should are callable");

        // Next, get the correct overload
        let callback = function_object
            .signature(parameter_types)
            .expect("It was proven in the mir-stage that an overload exists")
            .function();

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

    fn visit_raw_command(&mut self, raw_command: &MirRawCommand) -> Self::Output {
        // Get the string out of the generic object
        let string = raw_command
            .value
            .expect_concrete("Must be a concrete string by now")
            .downcast_payload::<ObjString>()
            .expect("Can only execute a string")
            .as_str()
            .to_string();

        let execute = Node::Execute(Execute { command: string });
        self.emit(Node::FastStoreFromResult(FastStoreFromResult {
            command: Box::new(execute),
            id: raw_command.var_id,
            scoreboard: Scoreboard::Main,
        }));

        Ok(())
    }
}
