use crate::{
    error::Result,
    function_interface::FunctionCall,
    mir::{
        ContextId, MirBranchIf, MirCall, MirContext, MirContextMap, MirGotoContext, MirValue,
        MirVisitor, NamespaceArena,
    },
    objects::{ObjBool, ObjStaticBool},
    ObjectRef,
};

use super::{
    llir_nodes::{Branch, Call, Condition::Compare, Function, Node},
    utils::{ItemId, ScoreboardComparison, ScoreboardValue},
    LLIRContext, LlirFunctions,
};

pub(crate) struct LLIRBuilder<'llir, 'ctx, 'arena> {
    context: LLIRContext<'ctx>,
    arena: &'arena mut NamespaceArena,
    nodes: Vec<Node>,
    mir_contexts: &'ctx MirContextMap<'ctx>,
    llir_helper: &'llir mut LlirFunctions,
}

impl<'ctx, 'arena, 'llir> LLIRBuilder<'llir, 'ctx, 'arena> {
    /// Creates a new `LLIRBuilder`. when building, populates the `llir_helper` struct.
    pub fn new(
        context: &'ctx MirContext<'ctx>,
        arena: &'arena mut NamespaceArena,
        mir_contexts: &'ctx MirContextMap<'ctx>,
        llir_helper: &'llir mut LlirFunctions,
    ) -> Self {
        let llir_context = LLIRContext {
            code: context.code,
            mir_nodes: &context.nodes,
            compile_context: context.compile_context,
            context_id: context.id,
        };

        LLIRBuilder {
            context: llir_context,
            arena,
            nodes: Vec::new(),
            mir_contexts,
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
            id: self.context.context_id,
            nodes: self.nodes,
            returned_value: result.clone(),
        };

        self.llir_helper.push(function);

        Ok(result)
    }

    pub fn context_id(&self) -> ContextId {
        self.context.context_id
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
        let context_id = self.mir_contexts.get(id.context).id;
        self.arena
            .get_by_id(id.id, context_id.as_inner())
            .and_then(|value| value.concrete())
    }

    /// Updates the template with this id to an object
    fn set_object(&mut self, value: ObjectRef, id: ItemId) {
        self.context
            .set_object(self.arena, self.mir_contexts, value, id);
    }

    /// Visits the context and optionally generates it.
    /// Returns the id and the return value
    fn visit_context(&mut self, id: ContextId) -> Result<(ContextId, ObjectRef)> {
        let is_same_context = id != self.context.context_id;

        Ok(if is_same_context {
            // If it is not the same context, it is safe to assume,
            // that this context has not been generated yet.
            // So generate it now.
            let context = self.mir_contexts.get(id);
            let llir_builder =
                LLIRBuilder::new(context, self.arena, self.mir_contexts, self.llir_helper);
            (llir_builder.context_id(), llir_builder.build()?)
        } else {
            // The result of this context is not yet known - so return null
            // For now this behaviour is fine, but it can be changed if this gets a problem
            (
                self.context_id(),
                self.context.compile_context.type_ctx().null(),
            )
        })
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
        let (function_id, result) = self.visit_context(goto_context.context_id)?;

        self.emit(Node::Call(Call { id: function_id }));

        Ok(result)
    }

    fn visit_branch_if(&mut self, branch_if: &MirBranchIf) -> Self::Output {
        let obj_ref = self.get_object(&branch_if.condition);

        if let Some(bool) = obj_ref.downcast_payload::<ObjBool>() {
            // Handler for normal boolean types

            let (function_id, result) = self.visit_context(branch_if.pos_branch)?;
            assert!(branch_if.neg_branch.is_none());

            // This condition can get automatically optimized out
            let condition = Compare {
                comparison: ScoreboardComparison::Equal,
                lhs: bool.as_scoreboard_value(),
                rhs: ScoreboardValue::Static(1),
            };

            self.emit(Node::Branch(Branch {
                condition,
                pos_branch: Box::new(Node::Call(Call { id: function_id })),
                neg_branch: None,
            }));

            Ok(result)
        } else if let Some(static_bool) = obj_ref.downcast_payload::<ObjStaticBool>() {
            // Handler for static boolean types
            // Does not even visit the context which is not called

            let static_context = if static_bool.value() {
                Some(self.visit_context(branch_if.pos_branch)?)
            } else {
                branch_if
                    .neg_branch
                    .map(|branch| self.visit_context(branch))
                    .transpose()?
            };

            let result = match static_context {
                Some((context, return_value)) => {
                    self.emit(Node::Call(Call { id: context }));
                    return_value
                }
                None => self.context.compile_context.type_ctx().null(),
            };
            Ok(result)
        } else {
            panic!("Expected a value of type bool, but got {}", obj_ref.class)
        }
    }
}
