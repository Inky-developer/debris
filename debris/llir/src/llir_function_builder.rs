use std::rc::Rc;

use debris_common::{Ident, Span, SpecialIdent};
use debris_error::{CompileError, LangError, LangErrorKind, Result};
use debris_mir::{
    mir_context::{MirContext, MirContextId, ReturnContext},
    mir_nodes::{self, MirNode},
    mir_object::MirObjectId,
    mir_primitives::{MirFormatStringComponent, MirModule, MirPrimitive},
};
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    class::{Class, ClassKind, ClassRef},
    llir_builder::{builder_set_obj, LlirBuilder},
    llir_nodes::{Branch, Condition, Function},
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
        obj_tuple_object::{ObjTupleObject, Tuple, TupleRef},
    },
    opt::peephole_opt::PeepholeOptimizer,
    utils::{BlockId, ScoreboardComparison, ScoreboardValue},
    TypePattern,
};

use super::{
    llir_builder::{FunctionGenerics, FunctionParameter, MonomorphizedFunction},
    llir_nodes::{Call, Node},
    memory::mem_copy,
    ObjectRef, ValidPayload,
};

macro_rules! verify_value {
    ($self:ident, $expected:ident, $value:ident, $span:ident) => {{
        if $expected.matches_exact(&$value.class) {
            Ok::<Option<ObjectRef>, CompileError>(Some($value))
        } else {
            // Try to promote the value to the expected type
            let mut function_context = FunctionContext {
                item_id: $self.builder.item_id_allocator.next_id(),
                item_id_allocator: &mut $self.builder.item_id_allocator,
                parameters: &[
                    $value.clone(),
                    ObjClass::new($expected.clone()).into_object(&$self.builder.type_context),
                ],
                self_val: None,
                nodes: Vec::new(),
                type_ctx: &$self.builder.type_context,
                span: $span,
            };

            let promoted_opt = Self::promote_obj(&mut function_context).transpose()?;
            if let Some(promoted) = promoted_opt {
                $self.nodes.extend(function_context.nodes);
                Ok(Some(promoted))
            } else {
                Ok(None)
            }
        }
    }};
}

pub struct LlirFunctionBuilder<'builder, 'ctx> {
    block_id: BlockId,
    nodes: PeepholeOptimizer,
    builder: &'builder mut LlirBuilder<'ctx>,
    contexts: &'ctx FxHashMap<MirContextId, MirContext>,
}

impl<'builder, 'ctx> LlirFunctionBuilder<'builder, 'ctx> {
    pub fn new(
        block_id: BlockId,
        builder: &'builder mut LlirBuilder<'ctx>,
        contexts: &'ctx FxHashMap<MirContextId, MirContext>,
    ) -> Self {
        LlirFunctionBuilder {
            block_id,
            nodes: PeepholeOptimizer::from_compile_context(builder.compile_context),
            builder,
            contexts,
        }
    }

    pub fn build(mut self, context: &'ctx MirContext) -> Result<Function> {
        for node in &context.nodes {
            self.handle_node(node)?;
        }

        let return_value = self
            .builder
            .try_get_obj(
                &context
                    .return_values(self.builder.return_values_arena)
                    .return_value(),
            )
            .unwrap_or_else(|| self.builder.type_context.never());

        match context.return_context {
            ReturnContext::Specific(context_id) => {
                // Special case if the function recurses, because then `compile_context` would fail.
                // Just using the block id of this builder works, though.
                let block_id = self.compile_context(context_id)?.0;
                self.nodes.push(Node::Call(Call { id: block_id }));
            }
            ReturnContext::ManuallyHandled(_) => {}
            ReturnContext::Pass => {}
        }

        let nodes = self.nodes.take();
        Ok(Function {
            nodes,
            id: self.block_id,
            return_value,
        })
    }

    /// Sets an object, with `comptime_update_allowed` set. This can be used for e.g. declarations
    /// or for places where it is known that this operation is valid to be performed at compile time
    fn declare_obj(&mut self, target: MirObjectId, value: ObjectRef, target_span: Span) -> Result<()> {
        self.set_obj(target, value, target_span, true)
    }

    // Sets `target` to value. If target was already defined, performs a memory copy.
    fn set_obj(&mut self, target: MirObjectId, value: ObjectRef, target_span: Span, comptime_update_allowed: bool) -> Result<()> {
        let check_comptime_allowed = || -> Result<()> {
            if !comptime_update_allowed {
                return Err(LangError::new(LangErrorKind::ComptimeUpdate, target_span).into())
            }
            Ok(())
        };
        
        if self.builder.object_mapping.contains_key(&target) {
            let target_value = self.builder.get_obj(&target);

            // Promote the value if required
            let target_class = &target_value.class;
            let value_class = &value.class.clone();
            let value = match verify_value!(self, target_class, value, target_span)? {
                Some(value) => value,
                None => {
                    return Err(unexpected_type(
                        target_span,
                        &target_value.class,
                        value_class,
                    ))
                }
            };

            // Special case if the target value is never, which means we can just update the mapping
            if value.payload.memory_layout().mem_size() > 0 && target_value.class.diverges() {
                check_comptime_allowed()?;
                self.builder._set_obj(target, value, target_span)?;
            } else if target_value.class.kind.runtime_encodable() {
                mem_copy(|node| self.nodes.push(node), &target_value, &value);
            } else {
                check_comptime_allowed()?;
                self.builder._set_obj(target, value, target_span)?;
            }
        } else {
            self.builder._set_obj(target, value, target_span)?;
        }
        Ok(())
    }

    /// Tries to promote an object to the `target` class and returns the promoted object in case of success.
    /// `target` must be a class object.
    fn promote_obj(ctx: &mut FunctionContext) -> Option<Result<ObjectRef>> {
        let function = match ctx.parameters[0]
            .get_property(ctx.type_ctx, &Ident::Special(SpecialIdent::Promote))
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
    /// and performs automatic value promotion if supported by the type (e.g. ComptimeInt -> Int).
    /// Returns a compile error if the parameters did not match the signature and value promotion was not possible.
    fn verify_parameters(
        &mut self,
        function: &ObjNativeFunction,
        parameters: &mut Vec<ObjectRef>,
        call_span: Span,
    ) -> Result<()> {
        let function_parameters =
            &self.builder.native_functions[function.function_id].function_parameters;

        if parameters.len() == function_parameters.len() {
            let mut success = true;
            for (param, function_param) in parameters.iter_mut().zip(function_parameters.iter()) {
                let span = function_param.span();
                let expected_class = function_param.class();
                let param_cloned = param.clone();
                match verify_value!(self, expected_class, param_cloned, span)? {
                    Some(value) => *param = value,
                    None => {
                        success = false;
                        break;
                    }
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
            item_id_allocator: &mut self.builder.item_id_allocator,
            parameters,
            self_val: self_value,
            nodes: Vec::new(),
            type_ctx: &self.builder.type_context,
            span,
        };

        let result = function.callback_function.call(&mut function_ctx)?;

        self.nodes.extend(function_ctx.nodes);
        Ok(result)
    }

    fn try_clone_obj(&mut self, obj: ObjectRef, span: Span) -> Result<ObjectRef> {
        let function = obj.get_property(
            &self.builder.type_context,
            &Ident::Special(SpecialIdent::Clone),
        );
        if let Some(function) = function
            .as_ref()
            .and_then(|function| function.downcast_payload())
        {
            self.call_builtin_function(function, &[obj], None, span)
        } else {
            Ok(obj)
        }
    }

    // Compiles a any context that is not in the current context list
    fn compile_context(&mut self, context_id: MirContextId) -> Result<(BlockId, ObjectRef)> {
        if let Some(block_id) = self.builder.compiled_contexts.get(&context_id) {
            let ret_val = match self.builder.functions.get(block_id) {
                Some(function) => function.return_value.clone(),
                // If the block is listed as compiled, but the function is not available yet, this must be a recursive call
                // Since the block was not compiled yet, just return null
                // I am not sure about the exact implications, but I'll just leave it until it causes problems
                None => self.builder.type_context.null(),
            };
            Ok((*block_id, ret_val))
        } else {
            let block_id = self.builder.block_id_generator.next_id();

            // Insert this into the list of compiled contexts before the context is acutally compiled,
            // This allows easier recursion (check this if statement)
            self.builder.compiled_contexts.insert(context_id, block_id);

            let builder = LlirFunctionBuilder::new(block_id, &mut self.builder, self.contexts);
            let llir_function = builder.build(self.contexts.get(&context_id).unwrap())?;
            let return_value = llir_function.return_value.clone();
            self.builder.functions.insert(block_id, llir_function);
            Ok((block_id, return_value))
        }
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
            MirNode::PrimitiveDeclaration(primitive_declaration) => {
                self.handle_primitive_declaration(primitive_declaration)
            }
            MirNode::VariableUpdate(variable_update) => {
                self.handle_variable_update(variable_update)
            }
        }
    }

    fn handle_branch(&mut self, branch: &mir_nodes::Branch) -> Result<ObjectRef> {
        let condition = self.builder.get_obj(&branch.condition);
        if let Some(condition) = condition.downcast_payload() {
            self.handle_static_branch(branch, condition)
        } else if let Some(condition) = condition.downcast_payload() {
            self.handle_dynamic_branch(branch, condition)
        } else {
            Err(LangError::new(
                LangErrorKind::UnexpectedType {
                    declared: None,
                    expected: vec![
                        TypePattern::Class(ObjBool::class(&self.builder.type_context)).to_string(),
                        TypePattern::Class(ObjStaticBool::class(&self.builder.type_context))
                            .to_string(),
                    ],
                    got: condition.class.to_string(),
                },
                branch.condition_span,
            )
            .into())
        }
    }

    fn handle_static_branch(
        &mut self,
        branch: &mir_nodes::Branch,
        condition: &ObjStaticBool,
    ) -> Result<ObjectRef> {
        let context_id = if condition.value {
            branch.pos_branch
        } else {
            branch.neg_branch
        };

        let (block_id, value) = self.compile_context(context_id)?;
        self.nodes.push(Node::Call(Call { id: block_id }));
        let ret_val = value;

        Ok(ret_val)
    }

    fn handle_dynamic_branch(
        &mut self,
        branch: &mir_nodes::Branch,
        condition: &ObjBool,
    ) -> Result<ObjectRef> {
        // Evaluates both contexts and then decides at runtime to which branch to go to

        let (pos_block_id, pos_return_value) = self.compile_context(branch.pos_branch)?;

        // The negative return value is not used because its layout has to be the same
        // as the positive return value and it is already guaranteed in `handle_variable_update` that its layout matches
        let (neg_block_id, _neg_return_value) = self.compile_context(branch.neg_branch)?;

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
                            FormatStringComponent::Value(self.builder.get_obj(obj_id))
                        }
                    })
                    .collect();
                ObjFormatString::new(components).into_object(&self.builder.type_context)
            }
            MirPrimitive::Module(module) => self.handle_module(module)?,
            MirPrimitive::Tuple(tuple) => ObjTupleObject::new(
                tuple
                    .iter()
                    .map(|obj_id| self.builder.get_obj(obj_id))
                    .collect(),
            )
            .into_object(&self.builder.type_context),
            MirPrimitive::TupleClass(tuple_class) => {
                let mut layout = Vec::with_capacity(tuple_class.len());
                for (obj_id, span) in tuple_class {
                    let obj = self.builder.get_obj(obj_id);
                    let class = match obj.downcast_payload::<ObjClass>() {
                        Some(class) => class,
                        None => {
                            return Err(LangError::new(
                                LangErrorKind::UnexpectedType {
                                    declared: None,
                                    expected: vec![
                                        ObjClass::class(&self.builder.type_context).to_string()
                                    ],
                                    got: obj.class.to_string(),
                                },
                                *span,
                            )
                            .into())
                        }
                    };
                    layout.push(TypePattern::Class(class.class.clone()))
                }

                let tuple = Tuple { layout };
                let class = Class::new_empty(ClassKind::Tuple(TupleRef::new(tuple)));
                let obj_class = ObjClass::new(ClassRef::new(class));
                obj_class.into_object(&self.builder.type_context)
            }
            MirPrimitive::Function(function) => {
                // Create the runtime parameter objects
                let mut function_parameters = Vec::new();
                for (index, param) in function.parameters.iter().enumerate() {
                    let param_type = self.builder.get_obj(&param.typ);
                    // TODO: improve error span
                    let param_type =
                        param_type.downcast_payload::<ObjClass>().ok_or_else(|| {
                            unexpected_type(
                                param.span,
                                &ObjClass::class(&self.builder.type_context),
                                &param_type.class,
                            )
                        })?;

                    if !param_type.kind.pattern_runtime_encodable() {
                        function_parameters.push(FunctionParameter::Generic {
                            span: param.span,
                            index,
                            class: param_type.class.clone(),
                            obj_id: param.value,
                        });
                        continue;
                    }

                    // This object will be valid when the function is called,
                    // because the callsite parameters will get cloned to these
                    // function parameters
                    let parameter = param_type
                        .class
                        .new_obj_from_allocator(
                            &self.builder.type_context,
                            &mut self.builder.item_id_allocator,
                        )
                        .expect("Must be creatable");
                    function_parameters.push(FunctionParameter::Parameter {
                        span: param.span,
                        index,
                        template: parameter.clone(),
                    });
                    self.declare_obj(param.value, parameter, param.span)?;
                }

                let index = self.builder.native_functions.len();
                self.builder
                    .native_functions
                    .push(FunctionGenerics::new(function, function_parameters));
                let function = ObjNativeFunction { function_id: index };
                function.into_object(&self.builder.type_context)
            }
            MirPrimitive::FunctionClass(params, ret) => {
                let params = params.iter().map(|obj| self.builder.get_obj(obj)).collect();
                let ret = ret
                    .as_ref()
                    .map(|obj| self.builder.get_obj(obj))
                    .unwrap_or_else(|| self.builder.type_context.null());
                let function = FunctionClass {
                    parameters: params,
                    return_class: ret,
                };
                let class = Class {
                    kind: ClassKind::Function(function),
                    properties: Default::default(),
                };
                let obj_class = ObjClass::new(Rc::new(class));
                obj_class.into_object(&self.builder.type_context)
            }
            MirPrimitive::Null => ObjNull.into_object(&self.builder.type_context),
            MirPrimitive::Never => ObjNever.into_object(&self.builder.type_context),
        };

        self.declare_obj(declaration.target, obj, declaration.span)?;

        Ok(())
    }

    fn handle_variable_update(
        &mut self,
        variable_update: &mir_nodes::VariableUpdate,
    ) -> Result<()> {
        let source_value = self.builder.get_obj(&variable_update.value);
        self.set_obj(variable_update.target, source_value, variable_update.span, variable_update.comptime_update_allowed)?;

        Ok(())
    }

    fn handle_goto(&mut self, goto: &mir_nodes::Goto) -> Result<()> {
        let (block_id, _return_value) = self.compile_context(goto.context_id)?;

        self.nodes.push(Node::Call(Call { id: block_id }));

        Ok(())
    }

    fn handle_module(&mut self, module: &MirModule) -> Result<ObjectRef> {
        let (block_id, result) = self.compile_context(module.context_id)?;
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
                let obj = self.builder.get_obj(obj_id);
                (name.clone(), obj)
            })
            .collect();
        let module = ObjModule::with_members(module.ident.clone(), properties);
        let obj = module.into_object(&self.builder.type_context);

        assert!(
            result
                .class
                .matches_exact(&self.builder.type_context.null().class),
            "A module must return null (TODO)"
        );

        Ok(obj)
    }

    fn handle_runtime_promotion(
        &mut self,
        runtime_promotion: &mir_nodes::RuntimePromotion,
    ) -> Result<()> {
        let obj = self.builder.get_obj(&runtime_promotion.value);
        let class_opt = obj.payload.runtime_class(&self.builder.type_context);

        if let Some(class) = class_opt {
            let obj_class = ObjClass::new(class).into_object(&self.builder.type_context);
            let mut ctx = FunctionContext {
                item_id: self.builder.item_id_allocator.next_id(),
                item_id_allocator: &mut self.builder.item_id_allocator,
                parameters: &[obj.clone(), obj_class],
                self_val: None,
                nodes: Vec::new(),
                type_ctx: &self.builder.type_context,
                span: runtime_promotion.span,
            };
            if let Some(promoted_obj) = Self::promote_obj(&mut ctx).transpose()? {
                self.nodes.extend(ctx.nodes);
                self.declare_obj(
                    runtime_promotion.target,
                    promoted_obj,
                    runtime_promotion.span,
                )?;
                return Ok(());
            }
        }

        self.declare_obj(runtime_promotion.target, obj, runtime_promotion.span)?;

        Ok(())
    }

    fn handle_function_call(
        &mut self,
        function_call: &mir_nodes::FunctionCall,
    ) -> Result<ObjectRef> {
        let obj = self
            .builder
            .object_mapping
            .get(&function_call.function)
            .expect("TODO: Throw error message function not found")
            .clone();

        if let Some(native_function) = obj.downcast_payload() {
            self.handle_native_function_call(function_call, native_function)
        } else if let Some(function) = obj.downcast_payload() {
            self.handle_builtin_function_call(function_call, function)
        } else {
            Err(unexpected_type(
                function_call.ident_span,
                &ObjFunction::class(&self.builder.type_context),
                &obj.class,
            ))
        }
    }

    fn handle_native_function_call(
        &mut self,
        function_call: &mir_nodes::FunctionCall,
        function: &ObjNativeFunction,
    ) -> Result<ObjectRef> {
        fn handle_monomorphized_function<'a>(
            nodes: &mut PeepholeOptimizer,
            function: &MonomorphizedFunction,
            callsite_parameters: impl Iterator<Item = &'a ObjectRef>,
            function_parameters: impl Iterator<Item = &'a ObjectRef>,
        ) -> Result<ObjectRef> {
            // First, copy the parameters, then call the function, then return the return value
            for (source_param, target_param) in callsite_parameters.zip_eq(function_parameters) {
                mem_copy(|node| nodes.push(node), target_param, source_param);
            }

            nodes.push(Node::Call(Call {
                id: function.block_id,
            }));

            Ok(function.return_value.clone())
        }

        let mut parameters = function_call
            .parameters
            .iter()
            .map(|obj_id| self.builder.get_obj(obj_id))
            .collect_vec();

        self.verify_parameters(function, &mut parameters, function_call.span)?;

        let callsite_parameters = parameters
            .iter()
            .filter(|obj| !obj.class.kind.comptime_encodable());
        let callsite_generics = parameters
            .iter()
            .filter(|obj| obj.class.kind.comptime_encodable())
            .cloned()
            .collect_vec();

        let result;
        if let Some(monomorphized_function) = self.builder.native_functions[function.function_id]
            .generic_instantiation(callsite_generics.iter())
        {
            let function_parameters =
                &self.builder.native_functions[function.function_id].function_parameters;
            result = handle_monomorphized_function(
                &mut self.nodes,
                monomorphized_function,
                callsite_parameters,
                function_parameters.iter().filter_map(|param| match param {
                    FunctionParameter::Generic { .. } => None,
                    FunctionParameter::Parameter {
                        span: _,
                        index: _,
                        template,
                    } => Some(template),
                }),
            )?;
        } else {
            // If the function is called the first time with these specific generics, it has to be monomorphized.
            // This means it is executed with the specific parameters and an entry in the function instantiations gets created.
            let function_generics = self.builder.native_functions[function.function_id]
                .function_parameters
                .iter()
                .filter_map(|param| match param {
                    FunctionParameter::Generic {
                        span,
                        index: _,
                        class: _,
                        obj_id,
                    } => Some((*obj_id, *span)),
                    FunctionParameter::Parameter { .. } => None,
                });
            for ((function_generic, function_generic_span), callsite_generic) in
                function_generics.zip(callsite_generics.iter())
            {
                builder_set_obj(
                    &mut self.builder.object_mapping,
                    self.builder.global_namespace,
                    &self.builder.type_context,
                    function_generic,
                    callsite_generic.clone(),
                    function_generic_span,
                )?;
            }

            let monomorphized_function = &self.builder.native_functions[function.function_id];
            let context_id = monomorphized_function.mir_function.context_id;

            let (block_id, return_value) = self.compile_context(context_id)?;

            self.builder.native_functions[function.function_id]
                .instantiations
                .push((
                    callsite_generics,
                    MonomorphizedFunction {
                        block_id,
                        return_value,
                    },
                ));

            let monomorphized_function = &self.builder.native_functions[function.function_id]
                .instantiations
                .last()
                .unwrap()
                .1;
            let function_parameters =
                &self.builder.native_functions[function.function_id].function_parameters;
            let raw_result = handle_monomorphized_function(
                &mut self.nodes,
                monomorphized_function,
                callsite_parameters,
                function_parameters.iter().filter_map(|param| match param {
                    FunctionParameter::Generic { .. } => None,
                    FunctionParameter::Parameter {
                        span: _,
                        index: _,
                        template,
                    } => Some(template),
                }),
            )?;

            // Check that the declared return type matches the actual return type
            let expected_result_class = self.builder.native_functions[function.function_id]
                .mir_function
                .return_type
                .map(|obj_id| {
                    self.builder
                        .get_obj(&obj_id)
                        .downcast_class()
                        .expect("Must be a class")
                })
                .unwrap_or_else(|| ObjNull::class(&self.builder.type_context));
            let return_value_span = self.builder.native_functions[function.function_id]
                .mir_function
                .return_span;
            let raw_result_class = raw_result.class.clone();
            match verify_value!(self, expected_result_class, raw_result, return_value_span)? {
                Some(correct_return_value) => {
                    result = correct_return_value;
                }
                None => {
                    let mir_function =
                        self.builder.native_functions[function.function_id].mir_function;

                    return Err(LangError::new(
                        LangErrorKind::UnexpectedType {
                            got: raw_result_class.to_string(),
                            expected: vec![expected_result_class.to_string()],
                            declared: Some(mir_function.return_type_span),
                        },
                        mir_function.return_span,
                    )
                    .into());
                }
            }
        };

        let result = self.try_clone_obj(result, function_call.span)?;
        self.declare_obj(
            function_call.return_value,
            result.clone(),
            function_call.ident_span,
        )?;
        Ok(result)
    }

    fn handle_builtin_function_call(
        &mut self,
        function_call: &mir_nodes::FunctionCall,
        function: &ObjFunction,
    ) -> Result<ObjectRef> {
        let parameters = function_call
            .parameters
            .iter()
            .map(|obj_id| self.builder.get_obj(obj_id))
            .collect_vec();
        let self_value = function_call
            .self_obj
            .map(|obj_id| self.builder.get_obj(&obj_id));

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
}

#[track_caller]
fn unexpected_type(span: Span, expected: &Class, actual: &Class) -> CompileError {
    LangError::new(
        LangErrorKind::UnexpectedType {
            got: actual.to_string(),
            expected: vec![expected.to_string()],
            declared: None,
        },
        span,
    )
    .into()
}
