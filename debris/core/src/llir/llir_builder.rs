use crate::{
    error::Result,
    memory::mem_copy,
    mir::{
        ContextId, MirBranchIf, MirCall, MirContext, MirContextMap, MirGotoContext,
        MirJumpLocation, MirReturnValue, MirUpdateValue, MirValue, MirVisitor, NamespaceArena,
    },
    objects::{
        obj_bool::ObjBool, obj_bool_static::ObjStaticBool, obj_function::FunctionContext,
        obj_null::ObjNull,
    },
    ObjectRef, Type,
};

use super::{
    llir_impl::LlirFunction,
    llir_nodes::{Branch, Call, Condition::Compare, Node},
    opt::peephole_opt::PeepholeOptimizer,
    utils::{BlockId, ItemId, ScoreboardComparison, ScoreboardValue},
    LlirContext, LlirFunctions,
};

pub struct LlirBuilder<'llir, 'ctx, 'arena> {
    pub context: LlirContext<'ctx>,
    pub(super) current_function: BlockId,
    pub arena: &'arena mut NamespaceArena,
    pub nodes: PeepholeOptimizer,
    pub mir_contexts: &'ctx MirContextMap<'ctx>,
    pub llir_helper: &'llir mut LlirFunctions,
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
        for node in self.context.mir_nodes {
            self.visit_node(node)?;
        }

        // Get the result from the return stack
        let result = self.get_object(
            &self
                .mir_contexts
                .get(self.context_id())
                .return_values
                .get_template_or_default(),
        );

        let function = LlirFunction {
            returned_value: result.clone(),
            nodes: self.nodes,
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
    /// If the value is not computed, return the default value
    pub fn get_object(&self, value: &MirValue) -> ObjectRef {
        self.get_object_or_none(value).unwrap_or_else(|| {
            let default = self
                .mir_contexts
                .get(self.context_id())
                .return_values
                .default_return
                .clone();
            debug_assert!(
                value.class().matches(&default.class) || default.class.kind.is_never(),
                "Object {:?} was not set (default is {:?})",
                value,
                default
            );
            default
        })
    }

    pub fn get_object_or_none(&self, value: &MirValue) -> Option<ObjectRef> {
        match self
            .context
            .get_object(self.arena, self.mir_contexts, value)
        {
            Some(object) => Some(object),
            None => {
                if value.class().kind.matches_type(Type::Null) {
                    Some(ObjNull::instance(self.context.compile_context))
                } else {
                    None
                }
            }
        }
    }

    /// Returns an object that belongs to this id. None if the object is still a template
    pub fn get_object_by_id(&self, id: ItemId) -> Option<ObjectRef> {
        let context_id = self.mir_contexts.get(id.context).id;
        self.arena
            .get_by_id(id.id, context_id.as_inner())
            .and_then(|value| value.concrete())
    }

    /// Updates the template with this id to an object
    pub fn set_object(&mut self, value: ObjectRef, id: ItemId) {
        self.context
            .set_object(self.arena, self.mir_contexts, value, id);
    }

    fn replace_object(&mut self, value: ObjectRef, id: ItemId) {
        self.context
            .replace_object(self.arena, self.mir_contexts, value, id);
    }

    /// Visits the context and optionally generates it.
    /// Returns the id and the return value
    pub fn visit_context(&mut self, id: ContextId) -> Result<(BlockId, ObjectRef)> {
        let is_same_context = id == self.context.context_id;

        let registered = self.llir_helper.is_context_registered(&(id, 0));
        let block_id = self.llir_helper.block_for((id, 0));

        if registered {
            Ok((block_id, self.context.compile_context.type_ctx().null()))
        } else {
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
        let callback = &*call.function;

        let return_id = call
            .return_value
            .expect_template("Return value must be a template")
            .1;

        // Call the function
        let result = callback.call(
            &mut FunctionContext {
                span: call.span,
                item_id: return_id,
                llir_builder: self,
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

        let function = LlirFunction {
            nodes,
            returned_value: self.context.compile_context.type_ctx().null(),
        };

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

    fn visit_update_value(&mut self, update_value: &MirUpdateValue) -> Self::Output {
        let new_value = self.get_object(&update_value.new_value);
        let old_value = self.get_object_by_id(update_value.id);

        if let Some(old_value) = old_value {
            mem_copy(|node| self.emit(node), &old_value, &new_value);

            // if the value is comptime, override the old value
            if !old_value.class.kind.runtime_encodable() {
                self.replace_object(new_value, update_value.id);
            }
        } else {
            // If the old object does not exist, initialize it now.
            self.set_object(new_value, update_value.id);
        }

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
            mem_copy(|node| self.emit(node), &target_object, &object);
        } else {
            // Set the target object
            self.set_object(object, template.template().unwrap().1);
        }

        Ok(self.context.compile_context.type_ctx().null())
    }

    fn visit_branch_if(&mut self, branch_if: &MirBranchIf) -> Self::Output {
        let obj_ref = self.get_object(&branch_if.condition);

        if let Some(bool) = obj_ref.downcast_payload::<ObjBool>() {
            // Handle the positive branch
            let (pos_branch_id, pos_result) = self.visit_context(branch_if.pos_branch)?;

            let neg_branch_id = {
                let (id, _neg_result) = self.visit_context(branch_if.neg_branch)?;

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
                pos_branch: Box::new(Node::Call(Call { id: pos_branch_id })),
                neg_branch: Box::new(Node::Call(Call { id: neg_branch_id })),
            }));

            // The neg result is copied to pos_result, such that both are equivalent
            Ok(pos_result)
        } else if let Some(static_bool) = obj_ref.downcast_payload::<ObjStaticBool>() {
            let branch_id = match static_bool.value {
                true => branch_if.pos_branch,
                false => branch_if.neg_branch,
            };

            let (branch_func_id, return_value) = self.visit_context(branch_id)?;
            // let return_id = branch_if.value_id;
            // if let Some(old_value) = self.get_object_by_id(return_id) {
            //     assert_eq!(
            //         old_value, return_value,
            //         "Trying to replace a value that was already computed"
            //     )
            // } else {
            //     self.set_object(return_value.clone(), return_id);
            // }

            self.emit(Node::Call(Call { id: branch_func_id }));
            Ok(return_value)
        } else {
            panic!("Expected a value of type bool, but got {}", obj_ref.class)
        }
    }
}
