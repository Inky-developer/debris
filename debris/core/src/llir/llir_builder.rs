use debris_common::Ident;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::class::ClassRef;
use crate::error::Result;
use crate::llir::llir_function_builder::LlirFunctionBuilder;
use crate::llir::llir_nodes::Function;
use crate::llir::utils::{BlockId, ItemIdAllocator};
use crate::llir::{Llir, Runtime};
use crate::mir::mir_context::{MirContext, MirContextId, ReturnValuesArena};
use crate::mir::mir_object::MirObjectId;
use crate::mir::mir_primitives::MirFunction;
use crate::mir::namespace::MirNamespace;
use crate::objects::obj_module::ModuleFactory;
use crate::{CompileContext, ObjectRef, ValidPayload};

pub struct LlirBuilder<'ctx> {
    pub(super) compile_context: &'ctx CompileContext,
    pub(super) functions: FxHashMap<BlockId, Function>,
    pub(super) compiled_contexts: FxHashMap<MirContextId, BlockId>,
    /// A list of all used native functions and their instantiations
    pub(super) native_functions: Vec<FunctionGenerics<'ctx>>,
    pub(super) runtime: Runtime,
    pub(super) block_id_generator: BlockIdGenerator,
    pub(super) global_namespace: &'ctx MirNamespace,
    pub(super) return_values_arena: &'ctx ReturnValuesArena,
    pub(super) object_mapping: FxHashMap<MirObjectId, ObjectRef>,
    // /// All items that are defined externally and used within this llir step
    // pub(super) extern_items: FxHashMap<Ident, ObjectRef>,
    pub(super) item_id_allocator: ItemIdAllocator,
}

impl<'ctx> LlirBuilder<'ctx> {
    pub fn new(
        ctx: &'ctx CompileContext,
        extern_modules: &[ModuleFactory],
        mir_extern_items: &FxHashMap<Ident, MirObjectId>,
        namespace: &'ctx MirNamespace,
        return_values_arena: &'ctx ReturnValuesArena,
    ) -> Self {
        let mut extern_items = FxHashMap::default();
        for module_factory in extern_modules {
            let module = module_factory.call(ctx);
            if module_factory.import_members() {
                for (ident, value) in module.members() {
                    extern_items.insert(ident.clone(), value.clone());
                }
            } else {
                extern_items.insert(module.ident().clone(), module.into_object(ctx));
            }
        }

        let mut object_mapping = FxHashMap::default();
        for (extern_item_ident, extern_item_id) in mir_extern_items {
            let obj_ref = extern_items
                .get(extern_item_ident)
                .expect("TODO: Throw error message not found");
            object_mapping.insert(*extern_item_id, obj_ref.clone());
        }

        LlirBuilder {
            compile_context: ctx,
            functions: Default::default(),
            compiled_contexts: Default::default(),
            native_functions: Default::default(),
            runtime: Default::default(),
            block_id_generator: Default::default(),
            global_namespace: namespace,
            return_values_arena,
            object_mapping,
            // extern_items,
            item_id_allocator: Default::default(),
        }
    }

    pub fn build(
        mut self,
        entry_context_id: MirContextId,
        contexts: &'ctx FxHashMap<MirContextId, MirContext>,
    ) -> Result<Llir> {
        let entry_block_id = self.block_id_generator.next_id();
        self.runtime.add_on_load(entry_block_id);
        let entry_context = &contexts[&entry_context_id];

        let sub_builder = LlirFunctionBuilder::new(entry_block_id, &mut self, contexts);
        let result = sub_builder.build(entry_context)?;
        self.functions.insert(entry_block_id, result);

        Ok(Llir {
            functions: self.functions,
            runtime: self.runtime,
            entry_function: entry_block_id,
        })
    }

    pub(super) fn get_obj(&self, obj_id: &MirObjectId) -> ObjectRef {
        self.object_mapping
            .get(obj_id)
            .expect("Object not yet initialized!")
            .clone()
    }

    pub(super) fn set_obj(&mut self, obj_id: MirObjectId, value: ObjectRef) {
        builder_set_obj(
            &mut self.object_mapping,
            self.global_namespace,
            self.compile_context,
            obj_id,
            value,
        )
    }
}

/// Small hack to prevent borrow checker problems where rust would think that the entire `LlirBuilder` would get borrowed
pub(super) fn builder_set_obj(
    object_mapping: &mut FxHashMap<MirObjectId, ObjectRef>,
    global_namespace: &MirNamespace,
    ctx: &CompileContext,
    obj_id: MirObjectId,
    value: ObjectRef,
) {
    object_mapping.insert(obj_id, value.clone());

    let obj = global_namespace.get_obj(obj_id);
    for (ident, mir_obj_ref) in obj.local_namespace.iter() {
        let obj = value
            .get_property(ctx, ident)
            .expect("TODO: Throw compile error");
        builder_set_obj(object_mapping, global_namespace, ctx, *mir_obj_ref, obj);
    }
}

#[derive(Default)]
pub struct BlockIdGenerator {
    next_id: u32,
}

impl BlockIdGenerator {
    pub fn next_id(&mut self) -> BlockId {
        let id = self.next_id;
        self.next_id += 1;
        BlockId(id)
    }
}

#[derive(Debug)]
pub enum FunctionParameter {
    Parameter {
        index: usize,
        template: ObjectRef,
    },
    Generic {
        index: usize,
        class: ClassRef,
        obj_id: MirObjectId,
    },
}

impl FunctionParameter {
    pub fn class(&self) -> &ClassRef {
        match self {
            FunctionParameter::Generic { class, .. } => class,
            FunctionParameter::Parameter { template, .. } => &template.class,
        }
    }
}

pub type NativeFunctionId = usize;

#[derive(Debug)]
pub(super) struct MonomorphizedFunction {
    pub block_id: BlockId,
    pub return_value: ObjectRef,
}

#[derive(Debug)]
pub(super) struct FunctionGenerics<'a> {
    pub instantiations: Vec<(Vec<ObjectRef>, MonomorphizedFunction)>,
    // A vector containing all runtime default parameters
    pub function_parameters: Vec<FunctionParameter>,
    pub mir_function: &'a MirFunction,
}

impl<'a> FunctionGenerics<'a> {
    pub fn new(mir_function: &'a MirFunction, function_parameters: Vec<FunctionParameter>) -> Self {
        FunctionGenerics {
            instantiations: Default::default(),
            function_parameters,
            mir_function,
        }
    }

    pub fn generic_instantiation<'b>(
        &self,
        generics: impl Iterator<Item = &'b ObjectRef> + Clone,
    ) -> Option<&MonomorphizedFunction> {
        self.instantiations
            .iter()
            .find(|(instantiated_generics, _)| {
                instantiated_generics
                    .iter()
                    .zip_eq(generics.clone())
                    .all(|(required, got)| required == got)
            })
            .map(|(_, function)| function)
    }
}
