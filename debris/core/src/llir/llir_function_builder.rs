use itertools::Itertools;

use crate::error::Result;
use crate::llir::llir_builder::LlirBuilder;
use crate::llir::llir_nodes::Function;
use crate::llir::opt::peephole_opt::PeepholeOptimizer;
use crate::llir::utils::BlockId;
use crate::mir::mir_context::MirContext;
use crate::mir::mir_nodes::{Assignment, FunctionCall, MirNode, PrimitiveDeclaration};
use crate::mir::mir_primitives::{MirFormatStringComponent, MirPrimitive};
use crate::objects::obj_bool_static::ObjStaticBool;
use crate::objects::obj_format_string::{FormatStringComponent, ObjFormatString};
use crate::objects::obj_function::{FunctionContext, ObjFunction};
use crate::objects::obj_int_static::ObjStaticInt;
use crate::objects::obj_string::ObjString;
use crate::{ObjectRef, ValidPayload};

pub struct LlirFunctionBuilder<'builder, 'ctx> {
    block_id: BlockId,
    nodes: PeepholeOptimizer,
    builder: &'builder mut LlirBuilder<'ctx>,
}

impl<'builder, 'ctx> LlirFunctionBuilder<'builder, 'ctx> {
    pub fn new(block_id: BlockId, builder: &'builder mut LlirBuilder<'ctx>) -> Self {
        LlirFunctionBuilder {
            block_id,
            nodes: Default::default(),
            builder,
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
                    .expect("TODO: Throw error with message")
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
        };

        self.builder.set_obj(declaration.target, obj);

        Ok(())
    }

    fn handle_assignment(&mut self, _assignment: &Assignment) -> Result<()> {
        todo!()
    }

    fn handle_function_call(&mut self, function_call: &FunctionCall) -> Result<ObjectRef> {
        let function = self
            .builder
            .object_mapping
            .get(&function_call.function)
            .and_then(|func| func.downcast_payload::<ObjFunction>());

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
            .unwrap()
            .callback_function
            .call(&mut function_ctx, &parameters)?;
        self.nodes.extend(function_ctx.nodes);
        self.builder
            .set_obj(function_call.return_value, result.clone());

        Ok(result)
    }
}
