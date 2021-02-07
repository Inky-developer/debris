use crate::{
    error::Result,
    memory::mem_move,
    mir::{
        ContextId, MirBranchIf, MirCall, MirContext, MirContextMap, MirGotoContext,
        MirJumpLocation, MirReturnValue, MirValue, MirVisitor, NamespaceArena,
    },
    objects::{obj_bool::ObjBool, obj_bool_static::ObjStaticBool, obj_function::FunctionContext},
    ObjectRef,
};

use super::{
    llir_impl::LLirFunction,
    llir_nodes::{Branch, Call, Condition::Compare, Node},
    opt::peephole_opt::PeepholeOptimizer,
    utils::{BlockId, ItemId, ScoreboardComparison, ScoreboardValue},
    LlirContext, LlirFunctions,
};

pub(crate) struct LlirBuilder<'llir, 'ctx, 'arena> {
    context: LlirContext<'ctx>,
    pub(super) current_function: BlockId,
    current_block_index: usize,
    arena: &'arena mut NamespaceArena,
    nodes: PeepholeOptimizer,
    mir_contexts: &'ctx MirContextMap<'ctx>,
    llir_helper: &'llir mut LlirFunctions,
}

impl<'ctx, 'arena, 'llir> LlirBuilder<'llir, 'ctx, 'arena> {
    /// Creates a new `LLIRBuilder`. when building, populates the `llir_helper` struct.
    pub fn new(
        context: &'ctx MirContext<'ctx>,
        arena: &'arena mut NamespaceArena,
        mir_contexts: &'ctx MirContextMap<'ctx>,
        llir_helper: &'llir mut LlirFunctions,
    ) -> Self {
        let llir_context = LlirContext {
            span: context.span,
            mir_nodes: &context.nodes,
            compile_context: context.compile_context,
            context_id: context.id,
        };

        let current_block_index = 0;
        let id = llir_helper.block_for((context.id, current_block_index));

        LlirBuilder {
            context: llir_context,
            current_function: id,
            current_block_index,
            arena,
            nodes: Default::default(),
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

        let function = LLirFunction {
            nodes: self.nodes,
            returned_value: result.clone(),
        };

        self.llir_helper.add(self.current_function, function);

        Ok(result)
    }

    pub fn context_id(&self) -> ContextId {
        self.context.context_id
    }

    fn emit(&mut self, node: Node) {
        self.nodes.push(node)
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
    fn visit_context(&mut self, id: ContextId) -> Result<(BlockId, ObjectRef)> {
        let is_same_context = id == self.context.context_id;

        if self.llir_helper.is_context_registered(&(id, 0)) {
            let block_id = self.llir_helper.block_for((id, 0));
            Ok((block_id, self.context.compile_context.type_ctx().null()))
        } else {
            let block_id = self.llir_helper.block_for((id, 0));
            Ok(if !is_same_context {
                // If it is not the same context, it is safe to assume,
                // that this context has not been generated yet.
                // So generate it now.
                let context = self.mir_contexts.get(id);

                let llir_builder =
                    LlirBuilder::new(context, self.arena, self.mir_contexts, self.llir_helper);
                let id = llir_builder.current_function;
                (id, llir_builder.build()?)
            } else {
                // The result of this context is not yet known - so return null
                // For now this behaviour is fine, but it can be changed if this gets a problem
                (block_id, self.context.compile_context.type_ctx().null())
            })
        }
    }
}

impl MirVisitor for LlirBuilder<'_, '_, '_> {
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
        let result = callback.call(
            &mut FunctionContext {
                compile_context: self.context.compile_context,
                llir_helper: self.llir_helper,
                mir_contexts: self.mir_contexts,
                span: call.span,
                nodes: &mut self.nodes,
                item_id: return_id,
                namespaces: self.arena,
                parent: self.context.context_id,
            },
            &parameters,
        )?;

        // Update the template with the correct return value
        if self.get_object_by_id(return_id).is_none() {
            self.set_object(result.clone(), return_id);
        }

        Ok(result)
    }

    fn visit_jump_location(&mut self, jump_location: &MirJumpLocation) -> Self::Output {
        let block = self
            .llir_helper
            .block_for((self.context_id(), jump_location.index));

        // Resets the nodes buffer
        let nodes = std::mem::take(&mut self.nodes);

        let function = LLirFunction {
            nodes,
            returned_value: self.context.compile_context.type_ctx().null(),
        };

        self.current_block_index = jump_location.index;

        self.llir_helper.add(self.current_function, function);
        self.current_function = block;

        Ok(self.context.compile_context.type_ctx().null())
    }

    fn visit_goto_context(&mut self, goto_context: &MirGotoContext) -> Self::Output {
        self.visit_context(goto_context.context_id)?;

        let id = (goto_context.context_id, goto_context.block_id);
        let function_id = self.llir_helper.block_for(id);
        self.emit(Node::Call(Call { id: function_id }));

        Ok(self.context.compile_context.type_ctx().null())
    }

    fn visit_return_value(&mut self, return_value: &MirReturnValue) -> Self::Output {
        let return_values = &self.mir_contexts.get(return_value.context_id).return_values;

        let value = return_values
            .get(return_value.return_index)
            .expect("Invalid return index");

        let object = self.get_object(value);

        let template = &return_values.get_template().expect("Must exist").0;
        let target_object = self
            .context
            .get_object(self.arena, self.mir_contexts, template);

        if let Some(target_object) = target_object {
            // Copy the returned object to the common memory address
            mem_move(|node| self.emit(node), &target_object, &object);
        } else {
            // Set the target object
            self.set_object(object, template.template().unwrap().1);
        }

        Ok(self.context.compile_context.type_ctx().null())
    }

    fn visit_branch_if(&mut self, branch_if: &MirBranchIf) -> Self::Output {
        let obj_ref = self.get_object(&branch_if.condition);

        if let Some(bool) = obj_ref.downcast_payload::<ObjBool>() {
            // Handler for normal boolean types
            let (function_id, pos_result) = self.visit_context(branch_if.pos_branch)?;

            // ToDo: Fix this in a better way
            if let Some(value_id) = branch_if.value_id {
                let value = self.get_object_by_id(value_id);
                if value.is_none() {
                    self.set_object(pos_result.clone(), value_id);
                }
            }

            let neg_branch_id = {
                let (id, neg_result) = self.visit_context(branch_if.neg_branch)?;

                let function = self.llir_helper.get_function(&id).unwrap();

                // Copy the neg_branch value to the pos_branch, so that both paths are valid
                mem_move(|node| function.nodes.push(node), &pos_result, &neg_result);

                id
            };

            // This condition can get automatically optimized out
            let condition = Compare {
                comparison: ScoreboardComparison::Equal,
                lhs: bool.as_scoreboard_value(),
                rhs: ScoreboardValue::Static(1),
            };

            self.emit(Node::Branch(Branch {
                condition,
                pos_branch: Box::new(Node::Call(Call { id: function_id })),
                neg_branch: Box::new(Node::Call(Call { id: neg_branch_id })),
            }));

            Ok(pos_result)
        } else if let Some(static_bool) = obj_ref.downcast_payload::<ObjStaticBool>() {
            let (branch_id, branch_value) = match static_bool.value {
                true => (branch_if.pos_branch, &branch_if.pos_value),
                false => (branch_if.neg_branch, &branch_if.neg_value),
            };

            let (branch_func_id, return_value) = self.visit_context(branch_id)?;
            if let Some(return_id) = branch_if.value_id {
                let object = self.get_object(branch_value);
                if let Some(old_value) = self.get_object_by_id(return_id) {
                    assert_eq!(
                        old_value, object,
                        "Trying to replace a value that was already computed"
                    )
                } else {
                    self.set_object(object, return_id);
                }
            }

            self.emit(Node::Call(Call { id: branch_func_id }));
            Ok(return_value)
        } else {
            panic!("Expected a value of type bool, but got {}", obj_ref.class)
        }
    }
}
