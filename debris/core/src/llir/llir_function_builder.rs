use debris_common::{Ident, Span, SpecialIdent};
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    error::{LangError, LangErrorKind, Result},
    llir::{
        llir_builder::{builder_set_obj, LlirBuilder},
        llir_nodes::Function,
        opt::peephole_opt::PeepholeOptimizer,
        utils::BlockId,
    },
    mir::{
        mir_context::{MirContext, MirContextId},
        mir_nodes::{Assignment, FunctionCall, MirNode, PrimitiveDeclaration},
        mir_primitives::{MirFormatStringComponent, MirPrimitive},
    },
    objects::{
        obj_bool_static::ObjStaticBool,
        obj_class::ObjClass,
        obj_format_string::{FormatStringComponent, ObjFormatString},
        obj_function::{FunctionContext, ObjFunction},
        obj_int_static::ObjStaticInt,
        obj_native_function::ObjNativeFunction,
        obj_null::ObjNull,
        obj_string::ObjString,
    },
    ObjectRef, ValidPayload,
};

use super::{
    llir_builder::{FunctionGenerics, FunctionParameter, MonomorphizedFunction},
    llir_nodes::{Call, Node},
    memory::mem_copy,
};

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

        let return_value = match &context.return_value {
            Some(obj_id) => self.builder.get_obj(obj_id),
            None => ObjNull.into_object(self.builder.compile_context),
        };

        let nodes = self.nodes.take();
        Ok(Function {
            nodes,
            id: self.block_id,
            return_value,
        })
    }

    fn call_builtin_function(
        &mut self,
        function: &ObjFunction,
        parameters: &[ObjectRef],
        span: Span,
    ) -> Result<ObjectRef> {
        let mut function_ctx = FunctionContext {
            item_id: self.builder.item_id_allocator.next_id(),
            item_id_allocator: &mut self.builder.item_id_allocator,
            nodes: Vec::new(),
            ctx: self.builder.compile_context,
            span,
        };

        let result = function
            .callback_function
            .call(&mut function_ctx, &parameters)?;

        self.nodes.extend(function_ctx.nodes);
        Ok(result)
    }

    fn try_clone_obj(&mut self, obj: ObjectRef, span: Span) -> Result<ObjectRef> {
        let function = obj.get_property(
            &self.builder.compile_context,
            &Ident::Special(SpecialIdent::Clone),
        );
        if let Some(function) = function
            .as_ref()
            .and_then(|function| function.downcast_payload())
        {
            self.call_builtin_function(function, &[obj], span)
        } else {
            Ok(obj)
        }
    }

    fn handle_node(&mut self, node: &'ctx MirNode) -> Result<()> {
        match node {
            MirNode::Assignment(assignment) => self.handle_assignment(assignment),
            MirNode::FunctionCall(function_call) => {
                self.handle_function_call(function_call)?;
                Ok(())
            }
            MirNode::PrimitiveDeclaration(primitive_declaration) => {
                self.handle_primitive_declaration(primitive_declaration)
            }
        }
    }

    fn handle_primitive_declaration(
        &mut self,
        declaration: &'ctx PrimitiveDeclaration,
    ) -> Result<()> {
        let obj = match &declaration.value {
            MirPrimitive::Int(val) => {
                ObjStaticInt::new(*val).into_object(self.builder.compile_context)
            }
            MirPrimitive::Bool(val) => {
                ObjStaticBool::from(*val).into_object(self.builder.compile_context)
            }
            MirPrimitive::String(val) => {
                ObjString::from(val.clone()).into_object(self.builder.compile_context)
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
                ObjFormatString::new(components).into_object(self.builder.compile_context)
            }
            MirPrimitive::Function(function) => {
                // Create the runtime parameter objects
                let mut function_parameters = Vec::new();
                for (index, (param_type_id, param_id)) in function
                    .parameter_types
                    .iter()
                    .zip(function.parameters.iter())
                    .enumerate()
                {
                    let param_type = self.builder.get_obj(&param_type_id);
                    let param_type = param_type
                        .downcast_payload::<ObjClass>()
                        .expect("Must be a class");

                    if !param_type.kind.runtime_encodable() {
                        function_parameters.push(FunctionParameter::Generic {
                            index,
                            class: param_type.class.clone(),
                            obj_id: *param_id,
                        });
                        continue;
                    }

                    // This object will be valid when the function is called,
                    // because the callsite parameters will get cloned to these
                    // function parameters
                    let parameter = param_type
                        .class
                        .new_obj_from_allocator(
                            self.builder.compile_context,
                            &mut self.builder.item_id_allocator,
                        )
                        .expect("Must be creatable");
                    function_parameters.push(FunctionParameter::Parameter {
                        index,
                        template: parameter.clone(),
                    });
                    self.builder.set_obj(*param_id, parameter);
                }

                let index = self.builder.native_functions.len();
                self.builder
                    .native_functions
                    .push(FunctionGenerics::new(function, function_parameters));
                let function = ObjNativeFunction { function_id: index };
                function.into_object(self.builder.compile_context)
            }
        };

        self.builder.set_obj(declaration.target, obj);

        Ok(())
    }

    fn handle_assignment(&mut self, _assignment: &Assignment) -> Result<()> {
        todo!()
    }

    fn handle_function_call(&mut self, function_call: &FunctionCall) -> Result<ObjectRef> {
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
            todo!("Throw error invalid type")
        }
    }

    fn handle_native_function_call(
        &mut self,
        function_call: &FunctionCall,
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

        let parameters = function_call
            .parameters
            .iter()
            .map(|obj_id| self.builder.get_obj(obj_id))
            .collect_vec();

        let correct_call = {
            let function_params =
                &self.builder.native_functions[function.function_id].function_parameters;
            parameters.len() == function_params.len()
                && parameters
                    .iter()
                    .zip(function_params.iter())
                    .all(|(callsite_param, param)| {
                        param.class().matches_exact(&callsite_param.class)
                    })
        };
        if !correct_call {
            return Err(LangError::new(
                LangErrorKind::UnexpectedOverload {
                    expected: vec![self.builder.native_functions[function.function_id]
                        .function_parameters
                        .iter()
                        .map(FunctionParameter::class)
                        .cloned()
                        .collect()],
                    parameters: parameters.iter().map(|obj| obj.class.clone()).collect(),
                },
                function_call.span,
            )
            .into());
        }

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
                    FunctionParameter::Parameter { index: _, template } => Some(template),
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
                        index: _,
                        class: _,
                        obj_id,
                    } => Some(*obj_id),
                    FunctionParameter::Parameter { .. } => None,
                });
            for (function_generic, callsite_generic) in
                function_generics.zip(callsite_generics.iter())
            {
                builder_set_obj(
                    &mut self.builder.object_mapping,
                    &self.builder.global_namespace,
                    &self.builder.compile_context,
                    function_generic,
                    callsite_generic.clone(),
                );
            }

            let monomorphized_function = &self.builder.native_functions[function.function_id];
            let context_id = monomorphized_function.mir_function.context_id;

            let block_id = self.builder.block_id_generator.next_id();
            let builder = LlirFunctionBuilder::new(block_id, &mut self.builder, &self.contexts);
            let llir_function = builder.build(self.contexts.get(&context_id).unwrap())?;
            let return_value = llir_function.return_value.clone();
            self.builder.functions.insert(block_id, llir_function);

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
            result = handle_monomorphized_function(
                &mut self.nodes,
                monomorphized_function,
                callsite_parameters,
                function_parameters.iter().filter_map(|param| match param {
                    FunctionParameter::Generic { .. } => None,
                    FunctionParameter::Parameter { index: _, template } => Some(template),
                }),
            )?;
        };

        let result = self.try_clone_obj(result, function_call.span)?;
        self.builder
            .set_obj(function_call.return_value, result.clone());
        Ok(result)
    }

    fn handle_builtin_function_call(
        &mut self,
        function_call: &FunctionCall,
        function: &ObjFunction,
    ) -> Result<ObjectRef> {
        let parameters = function_call
            .parameters
            .iter()
            .map(|obj_id| self.builder.get_obj(obj_id))
            .collect_vec();

        let result = self.call_builtin_function(function, &parameters, function_call.span)?;

        self.builder
            .set_obj(function_call.return_value, result.clone());

        Ok(result)
    }
}
