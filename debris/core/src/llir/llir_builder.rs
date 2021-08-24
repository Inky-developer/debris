use debris_common::Ident;
use rustc_hash::FxHashMap;

use crate::error::Result;
use crate::llir::llir_function_builder::LlirFunctionBuilder;
use crate::llir::llir_nodes::Function;
use crate::llir::utils::{BlockId, ItemIdAllocator};
use crate::llir::{Llir, Runtime};
use crate::mir::mir_context::{MirContext, MirContextId};
use crate::mir::mir_object::MirObjectId;
use crate::mir::namespace::MirNamespace;
use crate::objects::obj_module::ModuleFactory;
use crate::{CompileContext, ObjectRef, ValidPayload};

pub struct LlirBuilder<'ctx> {
    pub(super) compile_context: &'ctx CompileContext,
    pub(super) functions: FxHashMap<BlockId, Function>,
    pub(super) runtime: Runtime,
    pub(super) block_id_generator: BlockIdGenerator,
    pub(super) global_namespace: &'ctx MirNamespace,
    pub(super) object_mapping: FxHashMap<MirObjectId, ObjectRef>,
    /// All items that are defined externally and used within this llir step
    pub(super) extern_items: FxHashMap<Ident, ObjectRef>,
    pub(super) item_id_allocator: ItemIdAllocator,
}

impl<'ctx> LlirBuilder<'ctx> {
    pub fn new(
        ctx: &'ctx CompileContext,
        extern_modules: &[ModuleFactory],
        namespace: &'ctx MirNamespace,
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

        LlirBuilder {
            compile_context: ctx,
            functions: Default::default(),
            runtime: Default::default(),
            block_id_generator: Default::default(),
            global_namespace: namespace,
            object_mapping: Default::default(),
            extern_items,
            item_id_allocator: Default::default(),
        }
    }

    pub fn build(
        mut self,
        entry_context_id: MirContextId,
        contexts: &FxHashMap<MirContextId, MirContext>,
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
        self.object_mapping.insert(obj_id, value.clone());

        let obj = self.global_namespace.get_obj(obj_id);
        for (ident, mir_obj_ref) in obj.local_namespace.iter() {
            let obj = value
                .get_property(self.compile_context, ident)
                .expect("TODO: Throw compile error");
            self.set_obj(*mir_obj_ref, obj);
        }
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
