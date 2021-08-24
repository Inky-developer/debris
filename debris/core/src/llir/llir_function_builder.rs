use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::error::{LangError, LangErrorKind, Result};
use crate::llir::llir_builder::LlirBuilder;
use crate::llir::llir_nodes::Function;
use crate::llir::opt::peephole_opt::PeepholeOptimizer;
use crate::llir::utils::BlockId;
use crate::mir::mir_context::{MirContext, MirContextId};
use crate::mir::mir_nodes::{Assignment, FunctionCall, MirNode, PrimitiveDeclaration};
use crate::mir::mir_primitives::{MirFormatStringComponent, MirPrimitive};
use crate::objects::obj_bool_static::ObjStaticBool;
use crate::objects::obj_format_string::{FormatStringComponent, ObjFormatString};
use crate::objects::obj_function::{FunctionContext, ObjFunction};
use crate::objects::obj_int_static::ObjStaticInt;
use crate::objects::obj_native_function::ObjNativeFunction;
use crate::objects::obj_null::ObjNull;
use crate::objects::obj_string::ObjString;
use crate::{ObjectRef, ValidPayload};

use super::llir_nodes::{Call, Node};
use super::memory::mem_copy;

pub struct LlirFunctionBuilder<'builder, 'ctx> {
    block_id: BlockId,
    nodes: PeepholeOptimizer,
    builder: &'builder mut LlirBuilder<'ctx>,
    contexts: &'builder FxHashMap<MirContextId, MirContext>,
}

impl<'builder, 'ctx> LlirFunctionBuilder<'builder, 'ctx> {
    pub fn new(
        block_id: BlockId,
        builder: &'builder mut LlirBuilder<'ctx>,
        contexts: &'builder FxHashMap<MirContextId, MirContext>,
    ) -> Self {
        LlirFunctionBuilder {
            block_id,
            nodes: Default::default(),
            builder,
            contexts,
        }
    }

    pub fn build(mut self, context: &MirContext) -> Result<Function> {
        for node in &context.nodes {
            self.handle_node(node)?;
        }

        let nodes = self.nodes.take();
        Ok(Function {
            nodes,
            id: self.block_id,
        })
    }

    fn handle_node(&mut self, node: &MirNode) -> Result<()> {
        match node {
            MirNode::Assignment(assignment) => self.handle_assignment(assignment),
            MirNode::ExternItem(extern_item) => {
                let obj = self
                    .builder
                    .extern_items
                    .get(&extern_item.ident)
                    .ok_or_else(|| {
                        LangError::new(
                            LangErrorKind::MissingVariable {
                                notes: vec![],
                                similar: vec![],
                                var_name: extern_item.ident.clone(),
                            },
                            extern_item.span,
                        )
                    })?
                    .clone();
                self.builder.set_obj(extern_item.obj_id, obj);
                Ok(())
            }
            MirNode::FunctionCall(function_call) => {
                self.handle_function_call(function_call)?;
                Ok(())
            }
            MirNode::PrimitiveDeclaration(primitive_declaration) => {
                self.handle_primitive_declaration(primitive_declaration)
            }
        }
    }

    fn handle_primitive_declaration(&mut self, declaration: &PrimitiveDeclaration) -> Result<()> {
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
                let block_id = self.builder.block_id_generator.next_id();
                let context = self.contexts.get(&function.context_id).unwrap();
                let sub_builder =
                    LlirFunctionBuilder::new(block_id, &mut self.builder, &self.contexts);
                let llir_function = sub_builder.build(context)?;
                self.builder.functions.insert(block_id, llir_function);
                ObjNativeFunction::Function {
                    block_id,
                    parameters: vec![],
                    return_value: ObjNull.into_object(self.builder.compile_context),
                }
                .into_object(self.builder.compile_context)
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
        match function {
            ObjNativeFunction::Function {
                block_id,
                parameters,
                return_value: fn_return_value,
            } => {
                for (obj_ref, parameter) in
                    function_call.parameters.iter().zip_eq(parameters.iter())
                {
                    let obj = self.builder.get_obj(obj_ref);
                    if !obj.class.matches(&parameter.class) {
                        todo!("Throw error for invalid type")
                    }
                    mem_copy(|node| self.nodes.push(node), &obj, parameter);
                }
                self.nodes.push(Node::Call(Call { id: *block_id }));

                let return_value = ObjNull.into_object(self.builder.compile_context); // TODO: Handle return values
                mem_copy(|node| self.nodes.push(node), &return_value, fn_return_value);

                Ok(return_value)
            }
        }
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

        let mut function_ctx = FunctionContext {
            item_id: self.builder.item_id_allocator.next_id(),
            item_id_allocator: &mut self.builder.item_id_allocator,
            nodes: Vec::new(),
            ctx: self.builder.compile_context,
            span: function_call.span,
        };

        let result = function
            .callback_function
            .call(&mut function_ctx, &parameters)?;
        self.nodes.extend(function_ctx.nodes);
        self.builder
            .set_obj(function_call.return_value, result.clone());

        Ok(result)
    }
}
