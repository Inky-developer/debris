use std::{
    collections::{HashSet, VecDeque},
    fmt::Debug,
    iter::zip,
    mem,
    rc::Rc,
};

use debris_common::{Ident, Span, SpecialIdent};
use debris_error::{CompileError, LangError, LangErrorKind, Result};
use debris_mir::{
    mir_context::{MirContext, MirContextId, ReturnContext},
    mir_nodes::{self, MirNode},
    mir_object::MirObjectId,
    mir_primitives::{MirFormatStringComponent, MirModule, MirPrimitive},
};
use debris_mir::{
    mir_nodes::{
        PropertyAccess, RuntimeCopy, VerifyPropertyExists, VerifyTupleLength, VerifyValueComptime,
    },
    namespace::MirNamespace,
};
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    block_id::BlockId,
    class::{Class, ClassKind, ClassRef},
    error_utils::unexpected_type,
    extern_item_path::ExternItemPath,
    llir_builder::{set_obj, CallStack, FunctionParameter, LlirBuilder},
    llir_nodes::{Branch, Condition, Function},
    match_object,
    minecraft_utils::{ScoreboardComparison, ScoreboardValue},
    objects::{
        obj_bool::ObjBool,
        obj_bool_static::ObjStaticBool,
        obj_class::{HasClass, ObjClass},
        obj_format_string::{FormatStringComponent, ObjFormatString},
        obj_function::{FunctionClass, FunctionContext, ObjFunction},
        obj_int_static::ObjStaticInt,
        obj_module::ObjModule,
        obj_native_function::ObjNativeFunction,
        obj_never::ObjNever,
        obj_null::ObjNull,
        obj_string::ObjString,
        obj_struct::{ObjStruct, Struct},
        obj_struct_object::ObjStructObject,
        obj_tuple_object::{ObjTupleObject, Tuple, TupleRef},
    },
    opt::peephole_opt::PeepholeOptimizer,
    NativeFunctionId, Runtime, Type,
};

use super::{
    llir_builder::{FunctionGenerics, MonomorphizedFunction},
    llir_nodes::{Call, Node},
    memory::mem_copy,
    ObjectRef, ValidPayload,
};

macro_rules! verify_value {
    (match, $self:ident, $expected:ident, $value:ident, $span:ident) => {{
        if $expected.matches(&$value.class) {
            Ok::<Option<ObjectRef>, CompileError>(Some($value))
        } else {
            verify_value!(just_promote $self, $expected, $value, $span)
        }
    }};
    (match_exact, $self:ident, $expected:ident, $value:ident, $span:ident) => {{
        if $expected.matches_type(&$value.class) {
            Ok::<Option<ObjectRef>, CompileError>(Some($value))
        } else {
            verify_value!(just_promote $self, $expected, $value, $span)
        }
    }};
    (just_promote $self:ident, $expected:ident, $value:ident, $span:ident) => {{
        // Try to promote the value to the expected type
        let mut function_context = FunctionContext {
            item_id: $self.builder.item_id_allocator.next_id(),
            parameters: &[
                $value.clone(),
                ObjClass::new($expected.clone()).into_object(&$self.builder.type_context),
            ],
            self_val: None,
            nodes: Vec::new(),
            span: $span,
            llir_function_builder: $self,
        };

        let promoted_opt = Self::promote_obj(&mut function_context).transpose()?;
        if let Some(promoted) = promoted_opt {
            let nodes = function_context.nodes;
            $self.nodes.extend(nodes);
            Ok(Some(promoted))
        } else {
            Ok(None)
        }
    }};
}

#[derive(Debug, Default)]
pub struct BuilderSharedState {
    compiled_contexts: FxHashMap<MirContextId, BlockId>,
    pub(super) functions: FxHashMap<BlockId, Function>,
    pub(super) object_mapping: FxHashMap<MirObjectId, ObjectRef>,
    /// The local runtime
    pub(super) local_runtime: Runtime,
}

impl BuilderSharedState {
    /// Adds the accumulated state of another function builder to the state
    /// of this
    /// If `perform_writes` is false
    fn unify_with(&mut self, state: BuilderSharedState, mode: EvaluationMode) {
        let change_objects = match mode {
            EvaluationMode::Full => true,
            EvaluationMode::Monomorphization => false,
            EvaluationMode::Check => return,
        };

        let BuilderSharedState {
            compiled_contexts,
            functions,
            local_runtime,
            object_mapping,
        } = state;

        self.compiled_contexts.extend(compiled_contexts.into_iter());
        self.functions.extend(functions.into_iter());
        self.local_runtime.extend(local_runtime);
        if change_objects {
            self.object_mapping.extend(object_mapping.into_iter());
        }
    }
}

pub struct LlirFunctionBuilder<'builder, 'ctx> {
    ancestor: Option<&'builder LlirFunctionBuilder<'builder, 'ctx>>,
    call_stack: CallStack<'builder>,
    pub(super) shared: BuilderSharedState,
    block_id: BlockId,
    nodes: PeepholeOptimizer,
    pub(super) builder: &'builder LlirBuilder<'ctx>,
    /// Stores function ids pending to be added to the runtime
    pub(crate) pending_runtime_functions: FunctionBuilderRuntime,
    contexts: &'ctx FxHashMap<MirContextId, MirContext>,
}

impl<'builder, 'ctx> LlirFunctionBuilder<'builder, 'ctx> {
    pub fn new(
        ancestor: Option<&'builder LlirFunctionBuilder<'builder, 'ctx>>,
        call_stack: CallStack<'builder>,
        block_id: BlockId,
        builder: &'builder LlirBuilder<'ctx>,
        contexts: &'ctx FxHashMap<MirContextId, MirContext>,
    ) -> Self {
        LlirFunctionBuilder {
            ancestor,
            call_stack,
            shared: Default::default(),
            block_id,
            nodes: PeepholeOptimizer::from_compile_context(builder.compile_context),
            builder,
            pending_runtime_functions: Default::default(),
            contexts,
        }
    }

    pub fn build(&mut self, context: &'ctx MirContext) -> Result<()> {
        for node in &context.nodes {
            self.handle_node(node)?;
        }

        match context.return_context {
            ReturnContext::Specific(context_id) => {
                // Special case if the function recurses, because then `compile_context` would fail.
                // Just using the block id of this builder works, though.
                let block_id = self
                    .compile_context(context_id, None, EvaluationMode::Full)?
                    .0;
                self.nodes.push(Node::Call(Call { id: block_id }));
            }
            ReturnContext::ManuallyHandled(_) | ReturnContext::Pass => {}
        }

        let return_value = self
            .get_obj_opt(
                context
                    .return_values(self.builder.return_values_arena)
                    .return_value(),
            )
            .unwrap_or_else(|| self.builder.type_context.never());

        self.handle_on_tick_functions()?;
        self.handle_exported_functions()?;

        let nodes = self.nodes.take();
        let result = Function {
            nodes,
            id: self.block_id,
            return_value,
        };
        self.shared.functions.insert(self.block_id, result);

        Ok(())
    }

    fn get_compiled_context(&self, context_id: MirContextId) -> Option<BlockId> {
        self.shared
            .compiled_contexts
            .get(&context_id)
            .copied()
            .or_else(|| {
                self.ancestor
                    .and_then(|ancestor| ancestor.get_compiled_context(context_id))
            })
    }

    fn get_function(&self, block_id: BlockId) -> Option<&Function> {
        self.shared.functions.get(&block_id).or_else(|| {
            self.ancestor
                .and_then(|ancestor| ancestor.get_function(block_id))
        })
    }

    pub(super) fn get_obj(&self, obj_id: MirObjectId) -> ObjectRef {
        self.get_obj_opt(obj_id)
            .unwrap_or_else(|| panic!("Bad MIR (Value {:?} accessed before it is defined", obj_id))
    }

    pub(super) fn get_obj_opt(&self, obj_id: MirObjectId) -> Option<ObjectRef> {
        self.shared
            .object_mapping
            .get(&obj_id)
            .cloned()
            .or_else(|| {
                self.ancestor
                    .and_then(|ancestor| ancestor.get_obj_opt(obj_id))
            })
    }

    pub(super) fn _set_obj(&mut self, obj_id: MirObjectId, value: ObjectRef) {
        set_obj(
            self.builder.global_namespace,
            &self.builder.type_context,
            &mut self.shared.object_mapping,
            obj_id,
            value,
        );
    }

    fn get_function_generics(
        &self,
        function_id: NativeFunctionId,
    ) -> std::cell::Ref<FunctionGenerics<'ctx>> {
        std::cell::Ref::map(
            self.builder.native_function_map.borrow(),
            |native_functions| native_functions.get(function_id).unwrap(),
        )
    }

    // Compiles any context that is not in the current context list
    // This modifies this context after compiling
    fn compile_context(
        &mut self,
        context_id: MirContextId,
        function_id: Option<NativeFunctionId>,
        mode: EvaluationMode,
    ) -> Result<(BlockId, ObjectRef)> {
        if let Some(block_id) = self.get_compiled_context(context_id) {
            let ret_val = match self.get_function(block_id) {
                Some(function) => function.return_value.clone(),
                // If the block is listed as compiled, but the function is not available yet, this must be a recursive call
                // Since the block was not compiled yet, just return null
                // I am not sure about the exact implications, but I'll just leave it until it causes problems
                None => self.builder.type_context.null(),
            };
            Ok((block_id, ret_val))
        } else {
            let block_id = self.builder.block_id_generator.next_id();

            // Insert this into the list of compiled contexts before the context is actually compiled,
            // This allows easier recursion (check this if statement)
            self.shared.compiled_contexts.insert(context_id, block_id);
            let (block_id, result) =
                self.force_compile_context(context_id, block_id, function_id, mode)?;
            // Don't actually add the mark the context as compiled if it was just checked,
            // because otherwise it would be assumed that the context is ready to be called
            if matches!(mode, EvaluationMode::Check) {
                self.shared.compiled_contexts.remove(&context_id);
            }
            Ok((block_id, result))
        }
    }

    /// Compiles any context, even if it is already compiled.
    /// Force compiling a context does not modify the state of this context (Except for maybe some id counters)
    /// Returns the generated function, the return value, and the accumulated state
    fn force_compile_context(
        &mut self,
        context_id: MirContextId,
        block_id: BlockId,
        function_id: Option<NativeFunctionId>,
        mode: EvaluationMode,
    ) -> Result<(BlockId, ObjectRef)> {
        let (return_value, shared) = {
            let mut builder = LlirFunctionBuilder::new(
                Some(self),
                self.call_stack.then(function_id.map(|id| (id, block_id))),
                block_id,
                self.builder,
                self.contexts,
            );
            builder.build(self.contexts.get(&context_id).unwrap())?;

            let llir_function = builder.get_function(builder.block_id).unwrap();
            let return_value = llir_function.return_value.clone();
            (return_value, builder.shared)
        };

        self.shared.unify_with(shared, mode);

        Ok((block_id, return_value))
    }

    // Checks the function builder local runtime for pending on_tick functions and adds them to the global runtime
    fn handle_on_tick_functions(&mut self) -> Result<()> {
        let mut handled_functions = HashSet::new();
        while let Some((span, ticking_function_id)) =
            self.pending_runtime_functions.ticking_functions.pop_back()
        {
            if handled_functions.contains(&ticking_function_id) {
                continue;
            }
            handled_functions.insert(ticking_function_id);

            let block_id = self.compile_null_function(ticking_function_id, span)?;
            self.shared.local_runtime.schedule(block_id);
        }
        Ok(())
    }

    // Checks the function builder local runtime for pending export functions and adds them to the global runtime
    fn handle_exported_functions(&mut self) -> Result<()> {
        let mut handled_functions = HashSet::new();
        let functions = mem::take(&mut self.pending_runtime_functions.exports).into_iter();
        for (span, function_id, name) in functions {
            if handled_functions.contains(&function_id) {
                return Err(LangError::new(LangErrorKind::FunctionAlreadyExported, span).into());
            }
            handled_functions.insert(function_id);

            let block_id = self.compile_null_function(function_id, span)?;
            let extern_item_path = ExternItemPath::new(name).map_err(|error| {
                LangError::new(
                    LangErrorKind::InvalidExternItemPath {
                        error: error.to_string(),
                        path: error.path(),
                    },
                    span,
                )
            })?;
            self.shared
                .local_runtime
                .add_extern_block(block_id, extern_item_path);
        }
        Ok(())
    }

    /// Helper to compile a function that takes no parameters and returns null
    pub(crate) fn compile_null_function(
        &mut self,
        function_id: NativeFunctionId,
        span: Span,
    ) -> Result<BlockId> {
        if let Some(block_id) = self.call_stack.block_id_for(function_id) {
            return Ok(block_id);
        }

        self.compile_native_function(function_id, &mut [], Span::EMPTY)?;
        let generics = self.get_function_generics(function_id);
        let function = generics.generic_instantiation(&[].iter()).unwrap().1;
        // Ticking functions must return type that matches null
        // TODO: The compiler should probably emit a warning if this function is a ticking function and the
        // Type is not exactly `Null` (eg. `Never`)
        if !function
            .return_value
            .class
            .matches(&ObjNull::static_class(&self.builder.type_context))
        {
            return Err(LangError::new(
                LangErrorKind::UnexpectedType {
                    got: function.return_value.class.to_string(),
                    expected: vec![ObjNull::static_class(&self.builder.type_context).to_string()],
                    declared: Some(span),
                },
                generics.mir_function.return_type_span,
            )
            .into());
        }
        Ok(function.block_id)
    }

    /// Sets an object, with `comptime_update_allowed` set. This can be used for e.g. declarations
    /// or for places where it is known that this operation is valid to be performed at compile time
    fn declare_obj(
        &mut self,
        target: MirObjectId,
        value: ObjectRef,
        target_span: Span,
    ) -> Result<()> {
        self.set_obj(target, value, target_span, true)
    }

    // Sets `target` to value. If target was already defined, performs a memory copy.
    fn set_obj(
        &mut self,
        target: MirObjectId,
        value: ObjectRef,
        target_span: Span,
        comptime_update_allowed: bool,
    ) -> Result<()> {
        let check_comptime_allowed = || -> Result<()> {
            if !comptime_update_allowed {
                return Err(LangError::new(LangErrorKind::ComptimeUpdate, target_span).into());
            }
            Ok(())
        };

        // The 'more correct' way would be to only perform runtime updates if `comptime_update_allowed` is false.
        // However, because the llir builder visits contexts depth-first, while the mir builder visits contexts breadth-first,
        // this could cause problems where variables are not correctly assigned.
        // TODO: Change this condition when the llir builder is rewritten to work breadth-first
        if let Some(target_value) = self.get_obj_opt(target) {
            // Promote the value if required
            let target_class = &target_value.class;
            let value_class = value.class.clone();
            let value = match verify_value!(match_exact, self, target_class, value, target_span)? {
                Some(value) => value,
                None => {
                    return Err(unexpected_type(
                        target_span,
                        &target_value.class,
                        &value_class,
                    ))
                }
            };

            // Special case if the target value is never, which means we can just update the mapping
            if target_value.class.diverges() {
                self._set_obj(target, value);
            } else if target_value.class.kind.runtime_encodable() {
                mem_copy(|node| self.nodes.push(node), &target_value, &value);
            } else {
                check_comptime_allowed()?;
                self._set_obj(target, value);
            }
        } else {
            self._set_obj(target, value);
        }
        Ok(())
    }

    /// Tries to promote an object to the `target` class and returns the promoted object in case of success.
    /// `target` must be a class object.
    fn promote_obj(ctx: &mut FunctionContext) -> Option<Result<ObjectRef>> {
        let function = match ctx.parameters[0]
            .get_property(ctx.type_ctx(), &Ident::Special(SpecialIdent::Promote))
        {
            Some(f) => f,
            None => return None,
        };

        let builtin_function: &ObjFunction = function
            .downcast_payload()
            .expect("Objects associated with `SpecialIdent::Promote` must be builtin functions");

        let result = builtin_function.callback_function.call_raw(ctx)?;
        Some(match result {
            Ok(result) => Ok(result),
            Err(err) => Err(LangError::new(err, ctx.span).into()),
        })
    }

    /// Verifies that the given `parameters` match the signature of a given function .
    /// and performs automatic value promotion if supported by the type (e.g. `ComptimeInt` -> `Int`).
    /// Returns a compile error if the parameters did not match the signature and value promotion was not possible.
    fn verify_parameters(
        &mut self,
        function_id: NativeFunctionId,
        parameters: &mut [ObjectRef],
        call_span: Span,
    ) -> Result<()> {
        let native_functions = self.builder.native_function_map.borrow();
        let function_parameters = &native_functions
            .get(function_id)
            .unwrap()
            .function_parameters;

        if parameters.len() == function_parameters.len() {
            let mut success = true;
            for (param, function_param) in zip(&mut *parameters, function_parameters) {
                let span = function_param.span();
                let expected_class = function_param.class().clone();
                let param_cloned = param.clone();
                if let Some(value) = verify_value!(match, self, expected_class, param_cloned, span)?
                {
                    *param = value;
                } else {
                    success = false;
                    break;
                }
            }
            if success {
                return Ok(());
            }
        };

        Err(LangError::new(
            LangErrorKind::UnexpectedOverload {
                expected: vec![function_parameters
                    .iter()
                    .map(FunctionParameter::class)
                    .map(ToString::to_string)
                    .collect()],
                parameters: parameters.iter().map(|obj| obj.class.to_string()).collect(),
                function_definition_span: Some(
                    self.get_function_generics(function_id)
                        .mir_function
                        .signature_span,
                ),
            },
            call_span,
        )
        .into())
    }

    fn call_builtin_function(
        &mut self,
        function: &ObjFunction,
        parameters: &[ObjectRef],
        self_value: Option<ObjectRef>,
        span: Span,
    ) -> Result<ObjectRef> {
        let mut function_ctx = FunctionContext {
            item_id: self.builder.item_id_allocator.next_id(),
            parameters,
            self_val: self_value,
            nodes: Vec::new(),
            span,
            llir_function_builder: self,
        };

        let result = function.callback_function.call(&mut function_ctx)?;

        let nodes = function_ctx.nodes;
        self.nodes.extend(nodes);
        Ok(result)
    }

    fn copy_if_runtime(&mut self, obj: ObjectRef) -> ObjectRef {
        if !obj.class.kind.runtime_encodable() {
            return obj;
        }

        let new_obj = obj
            .class
            .new_obj_from_allocator(&self.builder.type_context, &self.builder.item_id_allocator)
            .expect("Must be creatable");
        mem_copy(|node| self.nodes.push(node), &new_obj, &obj);
        new_obj
    }

    fn handle_node(&mut self, node: &'ctx MirNode) -> Result<()> {
        match node {
            MirNode::Branch(branch) => {
                self.handle_branch(branch)?;
                Ok(())
            }
            MirNode::FunctionCall(function_call) => {
                self.handle_function_call(function_call)?;
                Ok(())
            }
            MirNode::Goto(goto) => {
                self.handle_goto(goto)?;
                Ok(())
            }
            MirNode::RuntimePromotion(runtime_promotion) => {
                self.handle_runtime_promotion(runtime_promotion)
            }
            MirNode::RuntimeCopy(runtime_copy) => self.handle_runtime_copy(runtime_copy),
            MirNode::VerifyValueComptime(verify_value_comptime) => {
                self.verify_value_comptime(verify_value_comptime)
            }
            MirNode::VerifyTupleLength(tuple_length) => self.verify_tuple_length(tuple_length),
            MirNode::VerifyPropertyExists(verify_property_exists) => {
                self.verify_property_exists(verify_property_exists)
            }
            MirNode::PrimitiveDeclaration(primitive_declaration) => {
                self.handle_primitive_declaration(primitive_declaration)
            }
            MirNode::VariableUpdate(variable_update) => {
                self.handle_variable_update(variable_update)
            }
            MirNode::PropertyAccess(property_access) => {
                self.handle_property_access(property_access)
            }
        }
    }

    fn handle_branch(&mut self, branch: &mir_nodes::Branch) -> Result<ObjectRef> {
        let condition = self.get_obj(branch.condition);
        match_object! {condition,
            condition: ObjStaticBool => self.handle_static_branch(branch, condition),
            condition: ObjBool => self.handle_dynamic_branch(branch, condition),
            else => Err(LangError::new(
                LangErrorKind::UnexpectedType {
                    declared: None,
                    expected: vec![
                        ObjBool::static_class(&self.builder.type_context).to_string(),
                        ObjStaticBool::static_class(&self.builder.type_context).to_string(),
                    ],
                    got: condition.class.to_string(),
                },
                branch.condition_span,
            )
            .into()),
        }
    }

    fn handle_static_branch(
        &mut self,
        branch: &mir_nodes::Branch,
        condition: &ObjStaticBool,
    ) -> Result<ObjectRef> {
        let (pos_branch_id, neg_branch_id) = if condition.value {
            (branch.pos_branch, branch.neg_branch)
        } else {
            (branch.neg_branch, branch.pos_branch)
        };

        // First check that the other branch is correct
        self.compile_context(neg_branch_id, None, EvaluationMode::Check)?;

        let (block_id, value) = self.compile_context(pos_branch_id, None, EvaluationMode::Full)?;
        self.nodes.push(Node::Call(Call { id: block_id }));
        let ret_val = value;

        Ok(ret_val)
    }

    fn handle_dynamic_branch(
        &mut self,
        branch: &mir_nodes::Branch,
        condition: &ObjBool,
    ) -> Result<ObjectRef> {
        if branch.is_comptime {
            return Err(LangError::new(
                LangErrorKind::InvalidComptimeBranch,
                branch.condition_span,
            )
            .into());
        }

        // Evaluates both contexts and then decides at runtime to which branch to go to
        let (pos_block_id, pos_return_value) =
            self.compile_context(branch.pos_branch, None, EvaluationMode::Full)?;

        // The negative return value is not used because its layout has to be the same
        // as the positive return value and it is already guaranteed in `handle_variable_update` that its layout matches
        let (neg_block_id, _neg_return_value) =
            self.compile_context(branch.neg_branch, None, EvaluationMode::Full)?;

        self.nodes.push(Node::Branch(Branch {
            condition: Condition::Compare {
                comparison: ScoreboardComparison::Equal,
                lhs: condition.as_scoreboard_value(),
                rhs: ScoreboardValue::Static(1),
            },
            pos_branch: Box::new(Node::Call(Call { id: pos_block_id })),
            neg_branch: Box::new(Node::Call(Call { id: neg_block_id })),
        }));

        Ok(pos_return_value)
    }

    fn handle_primitive_declaration(
        &mut self,
        declaration: &'ctx mir_nodes::PrimitiveDeclaration,
    ) -> Result<()> {
        // Some objects carry an associated context with them that should be evaluated after the objects got created
        let mut associated_context = None;

        let obj = match &declaration.value {
            MirPrimitive::Int(val) => {
                ObjStaticInt::new(*val).into_object(&self.builder.type_context)
            }
            MirPrimitive::Bool(val) => {
                ObjStaticBool::from(*val).into_object(&self.builder.type_context)
            }
            MirPrimitive::String(val) => {
                ObjString::from(val.clone()).into_object(&self.builder.type_context)
            }
            MirPrimitive::FormatString(val) => {
                let components = val
                    .0
                    .iter()
                    .map(|cpt| match cpt {
                        MirFormatStringComponent::String(val) => {
                            FormatStringComponent::String(val.clone())
                        }
                        MirFormatStringComponent::Value(obj_id) => {
                            FormatStringComponent::Value(self.get_obj(*obj_id))
                        }
                    })
                    .collect();
                ObjFormatString::new(components).into_object(&self.builder.type_context)
            }
            MirPrimitive::Module(module) => self.handle_module(module)?,
            MirPrimitive::Tuple(tuple) => {
                ObjTupleObject::new(tuple.iter().map(|obj_id| self.get_obj(*obj_id)).collect())
                    .into_object(&self.builder.type_context)
            }
            MirPrimitive::TupleClass(tuple_class) => {
                let mut layout = Vec::with_capacity(tuple_class.len());
                for (obj_id, span) in tuple_class {
                    let obj = self.get_obj(*obj_id);
                    let class = match obj.downcast_payload::<ObjClass>() {
                        Some(class) => class,
                        None => {
                            return Err(LangError::new(
                                LangErrorKind::UnexpectedType {
                                    declared: None,
                                    expected: vec![ObjClass::static_class(
                                        &self.builder.type_context,
                                    )
                                    .to_string()],
                                    got: obj.class.to_string(),
                                },
                                *span,
                            )
                            .into())
                        }
                    };
                    layout.push(class.class.clone());
                }

                let tuple = Tuple { layout };
                let class = Class::new_empty(ClassKind::Tuple(TupleRef::new(tuple)));
                let obj_class = ObjClass::new(ClassRef::new(class));
                obj_class.into_object(&self.builder.type_context)
            }
            MirPrimitive::StructType(mir_struct) => {
                let fields = mir_struct
                    .properties
                    .iter()
                    .map(|(ident, (obj_id, span))| {
                        let obj = self.get_obj(*obj_id);
                        obj.downcast_class()
                            .ok_or_else(|| {
                                unexpected_type(
                                    *span,
                                    &ObjClass::static_class(&self.builder.type_context),
                                    &obj.class,
                                )
                            })
                            .map(|cls| (ident.clone(), cls))
                    })
                    .try_collect()?;

                let mir_namespace = self.contexts[&mir_struct.context_id].local_namespace_id;

                let strukt = Struct {
                    ident: mir_struct.name.clone(),
                    mir_namespace,
                    fields,
                };

                let obj_class = ObjClass::new(Rc::new(Class::new_empty(ClassKind::Struct(
                    Rc::new(strukt),
                ))));

                associated_context = Some(mir_struct.context_id);
                obj_class.into_object(&self.builder.type_context)
            }
            MirPrimitive::Struct(mir_struct) => {
                let struct_type_obj_ref = self.get_obj(mir_struct.struct_type);
                let struct_ref = struct_type_obj_ref
                    .downcast_class()
                    .and_then(|class| match &class.kind {
                        ClassKind::Struct(strukt) => Some(Rc::clone(strukt)),
                        _ => None,
                    })
                    .ok_or_else(|| {
                        unexpected_type(
                            mir_struct.ident_span,
                            &ObjStruct::static_class(&self.builder.type_context),
                            &struct_type_obj_ref.class,
                        )
                    })?;
                let mut properties: FxHashMap<Ident, ObjectRef> = mir_struct
                    .values
                    .iter()
                    .map(|(ident, (obj_id, _span))| (ident.clone(), self.get_obj(*obj_id)))
                    .collect();

                // verify that the properties have the correct types
                for (ident, value) in &mut properties {
                    // if there is no matching template an error gets produced in `ObjStructObject::new`
                    if let Some(template) = struct_ref.fields.get(ident) {
                        let span = mir_struct.values.get(ident).unwrap().1;
                        let value_clone = value.clone();
                        let new_value = verify_value!(match, self, template, value_clone, span)?;
                        match new_value {
                            Some(new_value) => {
                                *value = new_value;
                                // Since the value change also change it in the namespace for later lookups
                                self._set_obj(mir_struct.values[ident].0, value.clone());
                            }
                            None => {
                                return Err(LangError::new(
                                    LangErrorKind::UnexpectedType {
                                        got: value.class.to_string(),
                                        expected: vec![template.to_string()],
                                        declared: None,
                                    },
                                    span,
                                )
                                .into());
                            }
                        }
                    }
                }

                let struct_obj = ObjStructObject::new(Rc::clone(&struct_ref), properties)
                    .map_err(|err| LangError::new(err, mir_struct.ident_span))?;
                struct_obj.into_object(&self.builder.type_context)
            }
            MirPrimitive::Function(function) => {
                // Create the runtime parameter objects
                let mut function_parameters = Vec::new();
                for (index, param) in function.parameters.iter().enumerate() {
                    let param_type = self.get_obj(param.typ);
                    let param_type =
                        param_type.downcast_payload::<ObjClass>().ok_or_else(|| {
                            unexpected_type(
                                param.span,
                                &ObjClass::static_class(&self.builder.type_context),
                                &param_type.class,
                            )
                        })?;

                    if !param_type
                        .kind
                        .as_value()
                        .expect("A function parameter type can never be a value")
                        .runtime_encodable()
                    {
                        function_parameters.push(FunctionParameter::Generic {
                            span: param.span,
                            index,
                            class: param_type.class.clone(),
                            obj_id: param.value,
                        });
                        continue;
                    }

                    // This object will be valid when the function is called,
                    // because the call-site parameters will get cloned to these
                    // function parameters
                    let parameter = param_type
                        .class
                        .new_obj_from_allocator(
                            &self.builder.type_context,
                            &self.builder.item_id_allocator,
                        )
                        .expect("Must be creatable");
                    function_parameters.push(FunctionParameter::Parameter {
                        span: param.span,
                        index,
                        template: parameter.clone(),
                        class: param_type.class.clone(),
                        obj_id: param.value,
                    });
                }

                let return_class = function.return_type.map_or_else(
                    || self.builder.type_context.static_class_obj::<ObjNull>(),
                    |id| self.get_obj(id),
                );
                let return_class =
                    return_class.downcast_payload::<ObjClass>().ok_or_else(|| {
                        unexpected_type(
                            function.return_type_span,
                            &ObjClass::static_class(&self.builder.type_context),
                            &return_class.class,
                        )
                    })?;

                let generics = FunctionGenerics::new(
                    &self.builder.type_context,
                    function,
                    function_parameters,
                    return_class.class.clone(),
                );
                let function_class = generics.signature.clone();
                let index = self
                    .builder
                    .native_function_map
                    .borrow_mut()
                    .insert(generics);
                let function = ObjNativeFunction {
                    function_id: index,
                    signature: function_class,
                };
                function.into_object(&self.builder.type_context)
            }
            MirPrimitive::FunctionClass(params, ret) => {
                let params = params.iter().map(|obj| self.get_obj(*obj)).collect();
                let ret = ret.as_ref().map_or_else(
                    || self.builder.type_context.static_class_obj::<ObjNull>(),
                    |obj| self.get_obj(*obj),
                );
                let function = FunctionClass {
                    parameters: params,
                    return_class: ret,
                };
                let class = Class {
                    kind: ClassKind::Function(function.into()),
                    properties: Default::default(),
                };
                let obj_class = ObjClass::new(Rc::new(class));
                obj_class.into_object(&self.builder.type_context)
            }
            MirPrimitive::Null => ObjNull.into_object(&self.builder.type_context),
            MirPrimitive::Never => ObjNever.into_object(&self.builder.type_context),
        };

        self.declare_obj(declaration.target, obj, declaration.span)?;
        if let Some(associated_context) = associated_context {
            self.compile_context(associated_context, None, EvaluationMode::Full)?;
        }

        Ok(())
    }

    fn handle_variable_update(
        &mut self,
        variable_update: &mir_nodes::VariableUpdate,
    ) -> Result<()> {
        let source_value = self.get_obj(variable_update.value);
        self.set_obj(
            variable_update.target,
            source_value,
            variable_update.span,
            variable_update.comptime_update_allowed,
        )?;

        Ok(())
    }

    fn handle_property_access(&mut self, property_access: &PropertyAccess) -> Result<()> {
        fn get_struct_property(
            strukt: &Struct,
            ident: &Ident,
            namespace: &MirNamespace,
        ) -> Option<MirObjectId> {
            let mir_namespace_id = strukt.mir_namespace;
            namespace
                .get_local_namespace(mir_namespace_id)
                .get_property(ident)
        }

        let obj = self.get_obj(property_access.value_id);

        let property = obj
            .get_property(&self.builder.type_context, &property_access.property_ident)
            .or_else(|| {
                // Special case for struct objects and strukt classes
                let maybe_obj_id = match_object! {obj,
                    strukt_obj: ObjStructObject => get_struct_property(&strukt_obj.struct_type,& property_access.property_ident, self.builder.global_namespace),
                    class_obj: ObjClass => if let ClassKind::Struct(strukt) = &class_obj.kind {
                        get_struct_property(strukt, &property_access.property_ident, self.builder.global_namespace)
                    } else {
                        None
                    },
                    else => None,
                };
                maybe_obj_id.map(|id| self.get_obj(id))
            })
            .ok_or_else(|| {
                LangError::new(
                    LangErrorKind::UnexpectedProperty {
                        property: property_access.property_ident.to_string(),
                        value_class: obj.class.to_string(),
                    },
                    property_access.span,
                )
            })?;
        self.declare_obj(property_access.target_id, property, property_access.span)?;
        Ok(())
    }

    fn handle_goto(&mut self, goto: &mir_nodes::Goto) -> Result<()> {
        let (block_id, _return_value) =
            self.compile_context(goto.context_id, None, EvaluationMode::Full)?;

        self.nodes.push(Node::Call(Call { id: block_id }));

        Ok(())
    }

    fn handle_module(&mut self, module: &MirModule) -> Result<ObjectRef> {
        let (block_id, result) =
            self.compile_context(module.context_id, None, EvaluationMode::Full)?;
        self.nodes.push(Node::Call(Call { id: block_id }));

        // Create a new module object
        // Technically, a sentinel value would suffice (instead of an object with all members),
        // But that information might become handy at some point
        let namespace = self
            .builder
            .global_namespace
            .get_local_namespace(self.contexts[&module.context_id].local_namespace_id);
        let properties = namespace
            .iter()
            .map(|(name, (obj_id, _))| {
                let obj = self.get_obj(*obj_id);
                Result::Ok((name.clone(), obj))
            })
            .try_collect()?;
        let obj_module = ObjModule::with_members(module.ident.clone(), properties);
        let obj = obj_module.into_object(&self.builder.type_context);

        if !Type::Null.matches(result.class.kind.typ()) {
            return Err(unexpected_type(
                module.last_item_span,
                &ObjNull::static_class(&self.builder.type_context),
                &result.class,
            ));
        }

        Ok(obj)
    }

    fn handle_runtime_promotion(
        &mut self,
        runtime_promotion: &mir_nodes::RuntimePromotion,
    ) -> Result<()> {
        let mut obj = self.get_obj(runtime_promotion.value);
        let class_opt = obj.payload.runtime_class(&self.builder.type_context);

        if let Some(class) = class_opt {
            let obj_class = ObjClass::new(class).into_object(&self.builder.type_context);
            let mut ctx = FunctionContext {
                item_id: self.builder.item_id_allocator.next_id(),
                parameters: &[obj.clone(), obj_class],
                self_val: None,
                nodes: Vec::new(),
                span: runtime_promotion.span,
                llir_function_builder: self,
            };
            if let Some(promoted_obj) = Self::promote_obj(&mut ctx).transpose()? {
                let nodes = ctx.nodes;
                self.nodes.extend(nodes);
                self.declare_obj(
                    runtime_promotion.target,
                    promoted_obj,
                    runtime_promotion.span,
                )?;
                return Ok(());
            }
        }

        if !obj.class.kind.typ().is_reference() {
            obj = self.copy_if_runtime(obj);
        }

        self.declare_obj(runtime_promotion.target, obj, runtime_promotion.span)?;

        Ok(())
    }

    fn handle_runtime_copy(&mut self, runtime_copy: &RuntimeCopy) -> Result<()> {
        let obj = self.get_obj(runtime_copy.value);
        let copied = self.copy_if_runtime(obj);
        self.declare_obj(runtime_copy.target, copied, runtime_copy.span)?;
        Ok(())
    }

    fn handle_function_call(
        &mut self,
        function_call: &mir_nodes::FunctionCall,
    ) -> Result<ObjectRef> {
        let obj = self.get_obj(function_call.function);
        match_object! {obj,
            native_function: ObjNativeFunction => self.handle_native_function_call(function_call, native_function),
            function: ObjFunction => self.handle_builtin_function_call(function_call, function),
            else => Err(unexpected_type(
                function_call.ident_span,
                &ObjFunction::static_class(&self.builder.type_context),
                &obj.class,
            )),
        }
    }

    fn handle_native_function_call(
        &mut self,
        function_call: &mir_nodes::FunctionCall,
        function: &ObjNativeFunction,
    ) -> Result<ObjectRef> {
        let parameters = function_call
            .parameters
            .iter()
            .map(|obj_id| self.get_obj(*obj_id))
            .collect();
        let self_value = function_call
            .self_obj
            .map(|self_obj_id| self.get_obj(self_obj_id));
        let result = self.call_native_function(
            function.function_id,
            parameters,
            self_value,
            function_call.span,
        )?;
        self.declare_obj(
            function_call.return_value,
            result.clone(),
            function_call.ident_span,
        )?;
        Ok(result)
    }

    fn call_native_function(
        &mut self,
        function_id: NativeFunctionId,
        mut parameters: Vec<ObjectRef>,
        self_value: Option<ObjectRef>,
        call_span: Span,
    ) -> Result<ObjectRef> {
        if self.call_stack.contains(function_id) {
            return Err(LangError::new(
                LangErrorKind::NotYetImplemented {
                    msg: "Recursion".to_string(),
                },
                call_span,
            )
            .into());
        }

        // If exactly one parameter is not specified and the self value is specified, insert self as first parameter
        let function_parameter_count = self
            .get_function_generics(function_id)
            .function_parameters
            .len();
        if function_parameter_count == parameters.len() + 1 {
            if let Some(self_value) = self_value {
                parameters.insert(0, self_value);
            }
        }

        // If a function has aliasing references, this function call model (Coping references before the call in and after the call out) fails.
        // For this reason, all function calls with aliasing parameters have to be inlined.
        if has_overlapping_references(&parameters) {
            let native_functions = self.builder.native_function_map.borrow();
            let function_parameters = native_functions
                .get(function_id)
                .unwrap()
                .function_parameters
                .as_slice();

            self.verify_parameters(function_id, &mut parameters, call_span)?;

            // Overwrite the parameters for the function with the ones used for this specific call
            let mut previous_objects = Vec::with_capacity(parameters.len());
            for (source_param, target_param) in parameters.iter().zip_eq(function_parameters) {
                let target_id = target_param.obj_id();
                previous_objects.push((target_id, self.get_obj_opt(target_id)));
                set_obj(
                    self.builder.global_namespace,
                    &self.builder.type_context,
                    &mut self.shared.object_mapping,
                    target_id,
                    source_param.clone(),
                );
            }

            drop(native_functions);

            // Actually compile and call the function
            let (id, result) = self.monomorphize_raw(function_id)?;
            self.nodes.push(Node::Call(Call { id }));

            // Revert the changes to the parameters, since the runtime values are expected to be the default ones
            for (obj_id, obj_opt) in previous_objects {
                if let Some(obj) = obj_opt {
                    set_obj(
                        self.builder.global_namespace,
                        &self.builder.type_context,
                        &mut self.shared.object_mapping,
                        obj_id,
                        obj,
                    );
                }
            }
            Ok(result)
        } else {
            self.call_native_function_no_alias(function_id, parameters, call_span)
        }
    }

    /// Calls a native function where it has been verified that its parameters cannot reference the same value
    fn call_native_function_no_alias(
        &mut self,
        function_id: usize,
        mut parameters: Vec<ObjectRef>,
        call_span: Span,
    ) -> Result<ObjectRef> {
        let monomorphization_index =
            self.compile_native_function(function_id, &mut parameters, call_span)?;
        let native_functions = self.builder.native_function_map.borrow();
        let function_parameters = &native_functions
            .get(function_id)
            .unwrap()
            .function_parameters;

        // First, copy the parameters, then call the function, then return the return value
        let zipped_params = parameters.iter().zip_eq(function_parameters);
        for (source_param, target_param) in zipped_params {
            if let FunctionParameter::Parameter { template, .. } = target_param {
                mem_copy(|node| self.nodes.push(node), template, source_param);
            }
        }

        let monomorphized_function = native_functions
            .get(function_id)
            .unwrap()
            .generic_instantiation_by_index(monomorphization_index)
            .unwrap();

        self.nodes.push(Node::Call(Call {
            id: monomorphized_function.block_id,
        }));

        // Now, copy parameters, that are passed by reference, back.
        let zipped_params = parameters.iter().zip_eq(function_parameters);
        for (source_param, target_param) in zipped_params {
            if let FunctionParameter::Parameter { template, .. } = target_param {
                if source_param.class.kind.typ().is_reference() {
                    mem_copy(|node| self.nodes.push(node), source_param, template);
                }
            }
        }

        let raw_value = monomorphized_function.return_value.clone();
        drop(native_functions);

        let cloned_result = self.copy_if_runtime(raw_value);
        Ok(cloned_result)
    }

    /// Compiles function with `function_id`.
    /// Caller has to guarantee that the parameters don't alias.
    /// # Return
    /// The index of the monomorphized function
    fn compile_native_function(
        &mut self,
        function_id: NativeFunctionId,
        parameters: &mut [ObjectRef],
        call_span: Span,
    ) -> Result<usize> {
        self.verify_parameters(function_id, parameters, call_span)?;

        // Partition parameters into compile time parameters and runtime parameters
        // Functions get monomorphized per compile time parameters
        let mut cloned_parameters = parameters.to_vec();
        let partitioned_parameters = ParameterPartition::new(&mut cloned_parameters, |obj| {
            obj.class.kind.runtime_encodable()
        });

        if let Some((index, _)) = self
            .get_function_generics(function_id)
            .generic_instantiation(&partitioned_parameters.right().iter())
        {
            return Ok(index);
        }

        // If the function is called the first time with these specific generics, it has to be monomorphized.
        let monomorphized_function = self.monomorphize_native_function(function_id, parameters)?;
        let mut native_functions = self.builder.native_function_map.borrow_mut();
        let function_generics = native_functions.get_mut(function_id).unwrap();
        function_generics.instantiations.push((
            partitioned_parameters.right().to_vec(),
            monomorphized_function,
        ));

        Ok(function_generics.instantiations.len() - 1)
    }

    /// Compiles the function with the given parameters into a new block.
    /// It is executed with the specific parameters and an entry in the function instantiations gets created.
    fn monomorphize_native_function(
        &mut self,
        function_id: NativeFunctionId,
        params: &[ObjectRef],
    ) -> Result<MonomorphizedFunction> {
        let native_functions = self.builder.native_function_map.borrow();
        let function_params = native_functions
            .get(function_id)
            .unwrap()
            .function_parameters
            .as_slice();

        for (function_param, call_param) in zip(function_params, params) {
            match function_param {
                FunctionParameter::Parameter {
                    obj_id, template, ..
                } => self._set_obj(*obj_id, template.clone()),
                FunctionParameter::Generic { obj_id, .. } => {
                    self._set_obj(*obj_id, call_param.clone());
                }
            }
        }
        drop(native_functions);

        let (block_id, return_value) = self.monomorphize_raw(function_id)?;

        Ok(MonomorphizedFunction {
            block_id,
            return_value,
        })
    }

    /// Actual monomorphization happening here
    /// Compiles the context of the function and assumes that all required parameters are set
    /// Returns the id of the generated block and the return value of the block
    fn monomorphize_raw(&mut self, function_id: usize) -> Result<(BlockId, ObjectRef)> {
        let context_id = self
            .get_function_generics(function_id)
            .mir_function
            .context_id;
        let block_id = self.builder.block_id_generator.next_id();
        let return_value = self
            .force_compile_context(
                context_id,
                block_id,
                Some(function_id),
                EvaluationMode::Monomorphization,
            )?
            .1;
        let expected_result_class = self
            .get_function_generics(function_id)
            .mir_function
            .return_type
            .map_or_else(
                || ObjNull::static_class(&self.builder.type_context),
                |obj_id| {
                    self.get_obj(obj_id)
                        .downcast_class()
                        .expect("Must be a class")
                },
            );
        let return_value_span = self
            .get_function_generics(function_id)
            .mir_function
            .return_span;
        let raw_result_class = return_value.class.clone();
        let return_value_opt = verify_value!(
            match,
            self,
            expected_result_class,
            return_value,
            return_value_span
        )?;
        let return_value = if let Some(correct_return_value) = return_value_opt {
            correct_return_value
        } else {
            let mir_function = self.get_function_generics(function_id).mir_function;

            return Err(LangError::new(
                LangErrorKind::UnexpectedType {
                    got: raw_result_class.to_string(),
                    expected: vec![expected_result_class.to_string()],
                    declared: Some(mir_function.return_type_span),
                },
                mir_function.return_span,
            )
            .into());
        };
        Ok((block_id, return_value))
    }

    fn handle_builtin_function_call(
        &mut self,
        function_call: &mir_nodes::FunctionCall,
        function: &ObjFunction,
    ) -> Result<ObjectRef> {
        let parameters = function_call
            .parameters
            .iter()
            .map(|obj_id| self.get_obj(*obj_id))
            .collect_vec();
        let self_value = function_call.self_obj.map(|obj_id| self.get_obj(obj_id));

        let result = self.call_builtin_function(
            function,
            &parameters,
            self_value,
            function_call.ident_span,
        )?;

        self.declare_obj(
            function_call.return_value,
            result.clone(),
            function_call.ident_span,
        )?;

        Ok(result)
    }

    fn verify_value_comptime(&self, verify_value_comptime: &VerifyValueComptime) -> Result<()> {
        let value = self.get_obj(verify_value_comptime.value);
        if !value.class.kind.comptime_encodable() {
            return Err(LangError::new(
                LangErrorKind::NonComptimeVariable {
                    class: value.class.to_string(),
                    var_name: self
                        .builder
                        .compile_context
                        .input_files
                        .get_span_str(verify_value_comptime.span)
                        .to_string(),
                },
                verify_value_comptime.span,
            )
            .into());
        }
        Ok(())
    }

    fn verify_tuple_length(&self, tuple_length: &VerifyTupleLength) -> Result<()> {
        let obj = self.get_obj(tuple_length.value);
        let tuple = obj.downcast_payload::<ObjTupleObject>().ok_or_else(|| {
            unexpected_type(
                tuple_length.span,
                &ObjTupleObject::static_class(&self.builder.type_context),
                &obj.class,
            )
        })?;

        if tuple.length() != tuple_length.length {
            return Err(LangError::new(
                LangErrorKind::TupleMismatch {
                    lhs_count: tuple_length.length,
                    rhs_count: tuple.length(),
                },
                tuple_length.span,
            )
            .into());
        }

        Ok(())
    }

    fn verify_property_exists(&self, verify_property_exists: &VerifyPropertyExists) -> Result<()> {
        let obj_id = verify_property_exists.obj_id;
        let obj = self.get_obj(obj_id);
        let property = obj.get_property(&self.builder.type_context, &verify_property_exists.ident);

        if property.is_none() {
            return Err(LangError::new(
                LangErrorKind::UnexpectedProperty {
                    property: verify_property_exists.ident.to_string(),
                    value_class: obj.class.to_string(),
                },
                verify_property_exists.span,
            )
            .into());
        }

        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct FunctionBuilderRuntime {
    ticking_functions: VecDeque<(Span, NativeFunctionId)>,
    exports: VecDeque<(Span, NativeFunctionId, String)>,
}

impl FunctionBuilderRuntime {
    pub fn register_ticking_function(&mut self, function: NativeFunctionId, span: Span) {
        self.ticking_functions.push_front((span, function));
    }

    pub fn export(&mut self, function: NativeFunctionId, name: String, span: Span) {
        self.exports.push_front((span, function, name));
    }
}

/// Checks if any objects that are references contain the same data
fn has_overlapping_references(objects: &[ObjectRef]) -> bool {
    let mut blacklist = FxHashSet::default();
    for object in objects {
        if object.class.kind.typ().is_reference() {
            for item in object.payload.memory_layout().iter() {
                if blacklist.contains(item) {
                    return true;
                }
                blacklist.insert(*item);
            }
        }
    }
    false
}

/// Specifies how code should be evaluated, e.g. if a context is part of normal control flow
/// Or if a context should just be verified to be valid
#[derive(Debug, Clone, Copy)]
pub enum EvaluationMode {
    /// Normal evaluation
    Full,
    /// Function monomorphization evaluation: Keeps the object mapping
    Monomorphization,
    /// Does not change any meaningful state. Used to just check if code is semantically valid
    Check,
}

/// Partitions function parameters into a left half and a right half
/// How to split is determined by a passed predicate.
#[derive(Debug)]
struct ParameterPartition<'a, T> {
    data: &'a mut [T],
    pivot: usize,
}

impl<'a, T> ParameterPartition<'a, T> {
    pub fn new(data: &'a mut [T], predicate: impl Fn(&T) -> bool) -> Self {
        data.sort_by_key(|key| !predicate(key));
        let pivot = data
            .iter()
            .find_position(|item| !predicate(item))
            .map_or(data.len(), |(idx, _)| idx);

        ParameterPartition { data, pivot }
    }

    #[allow(unused)]
    fn left(&self) -> &[T] {
        &self.data[..self.pivot]
    }

    pub fn right(&self) -> &[T] {
        &self.data[self.pivot..]
    }

    #[cfg(test)]
    pub fn get_mut(&mut self) -> (&mut [T], &mut [T]) {
        self.data.split_at_mut(self.pivot)
    }
}

#[cfg(test)]
mod tests {
    use super::ParameterPartition;

    #[test]
    fn test_partition() {
        let mut items = [1, 2, 3, 4, 5, 6, 7];
        let mut partition = ParameterPartition::new(&mut items, |val| val % 2 == 0);
        assert_eq!(partition.data, &[2, 4, 6, 1, 3, 5, 7]);

        assert_eq!(partition.left(), &[2, 4, 6]);
        assert_eq!(partition.right(), &[1, 3, 5, 7]);
        assert_eq!(partition.get_mut().0.to_vec(), partition.left());
        assert_eq!(partition.get_mut().1.to_vec(), partition.right());
    }

    #[test]
    fn test_partition_even() {
        let mut items = [1, 2, 3, 4, 5, 6, 7, 8];
        let mut partition = ParameterPartition::new(&mut items, |val| val % 2 == 0);
        assert_eq!(partition.data, &[2, 4, 6, 8, 1, 3, 5, 7]);

        assert_eq!(partition.left(), &[2, 4, 6, 8]);
        assert_eq!(partition.right(), &[1, 3, 5, 7]);
        assert_eq!(partition.get_mut().0.to_vec(), partition.left());
        assert_eq!(partition.get_mut().1.to_vec(), partition.right());
    }

    #[test]
    fn test_partition_empty() {
        let mut items: [i32; 0] = [];
        let mut partition = ParameterPartition::new(&mut items, |val| val % 2 == 0);
        assert_eq!(partition.data, &[]);

        assert_eq!(partition.left(), &[]);
        assert_eq!(partition.right(), &[]);
        assert_eq!(partition.get_mut().0.to_vec(), partition.left());
        assert_eq!(partition.get_mut().1.to_vec(), partition.right());
    }

    #[test]
    fn test_partition_one_left() {
        let mut items = [1];
        let mut partition = ParameterPartition::new(&mut items, |val| val % 2 == 0);
        assert_eq!(partition.data, &[1]);

        assert_eq!(partition.left(), &[]);
        assert_eq!(partition.right(), &[1]);
        assert_eq!(partition.get_mut().0.to_vec(), partition.left());
        assert_eq!(partition.get_mut().1.to_vec(), partition.right());
    }

    #[test]
    fn test_partition_one_left_even() {
        let mut items = [0];
        let mut partition = ParameterPartition::new(&mut items, |val| val % 2 == 0);
        assert_eq!(partition.data, &[0]);

        assert_eq!(partition.left(), &[0]);
        assert_eq!(partition.right(), &[]);
        assert_eq!(partition.get_mut().0.to_vec(), partition.left());
        assert_eq!(partition.get_mut().1.to_vec(), partition.right());
    }
}
