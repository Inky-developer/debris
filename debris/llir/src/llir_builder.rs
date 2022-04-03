use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    mem,
};

use debris_common::{CompileContext, Ident, Span};
use debris_error::{LangError, LangErrorKind, Result};
use debris_mir::{
    mir_context::{MirContext, MirContextId, ReturnValuesArena},
    mir_object::MirObjectId,
    mir_primitives::MirFunction,
    namespace::MirNamespace,
    MirExternItem,
};
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    block_id::BlockId,
    class::ClassRef,
    item_id::ItemIdAllocator,
    llir_function_builder::LlirFunctionBuilder,
    objects::{
        obj_class::ObjClass,
        obj_function::{FunctionClass, FunctionClassRef},
    },
    opt::global_opt::GlobalOptimizer,
    Llir, ObjectRef, Runtime, ValidPayload,
};

use super::type_context::TypeContext;

pub struct LlirBuilder<'ctx> {
    extern_items: FxHashMap<MirObjectId, ObjectRef>,
    pub(super) compile_context: &'ctx CompileContext,
    pub(super) type_context: TypeContext,
    /// Stores the already compiled functions
    /// This is not part of the llir function builder shared state, because a computed
    /// function should always be the same, no matter where it is computed from
    pub(super) native_function_map: RefCell<NativeFunctionMap<'ctx>>,
    pub(super) runtime: Runtime,
    pub(super) block_id_generator: BlockIdGenerator,
    pub(super) global_namespace: &'ctx MirNamespace,
    pub(super) return_values_arena: &'ctx ReturnValuesArena,
    // /// All items that are defined externally and used within this llir step
    pub(super) item_id_allocator: ItemIdAllocator,
}

impl<'ctx> LlirBuilder<'ctx> {
    pub fn new(
        ctx: &'ctx CompileContext,
        extern_items_factory: impl Fn(&TypeContext) -> HashMap<Ident, ObjectRef>,
        mir_extern_items: &FxHashMap<Ident, MirExternItem>,
        namespace: &'ctx MirNamespace,
        return_values_arena: &'ctx ReturnValuesArena,
    ) -> Result<Self> {
        let type_context = TypeContext::default();

        let extern_items = (extern_items_factory)(&type_context);

        // create a mapping from the mir ids to the extern items
        let mut object_mapping = FxHashMap::default();
        for (extern_item_ident, extern_item) in mir_extern_items {
            let obj_ref = extern_items.get(extern_item_ident).ok_or_else(|| {
                LangError::new(
                    LangErrorKind::MissingVariable {
                        notes: vec![],
                        similar: vec![],
                        var_name: extern_item_ident.clone(),
                    },
                    extern_item.definition_span,
                )
            })?;
            object_mapping.insert(extern_item.object_id, obj_ref.clone());
        }

        Ok(LlirBuilder {
            extern_items: object_mapping,
            compile_context: ctx,
            type_context,
            native_function_map: Default::default(),
            runtime: Default::default(),
            block_id_generator: Default::default(),
            global_namespace: namespace,
            return_values_arena,
            // extern_items,
            item_id_allocator: Default::default(),
        })
    }

    pub fn build(
        mut self,
        entry_context_id: MirContextId,
        contexts: &'ctx FxHashMap<MirContextId, MirContext>,
    ) -> Result<Llir> {
        let entry_block_id = self.block_id_generator.next_id();
        self.runtime.add_on_load(entry_block_id);
        let entry_context = &contexts[&entry_context_id];

        let extern_items = mem::take(&mut self.extern_items);
        let call_stack = CallStack::None;
        let mut sub_builder =
            LlirFunctionBuilder::new(None, call_stack, entry_block_id, &self, contexts);
        sub_builder
            .shared
            .object_mapping
            .extend(extern_items.iter().map(|(id, obj)| (*id, obj.clone())));
        sub_builder.build(entry_context)?;

        let functions = sub_builder.shared.functions;
        let local_runtime = sub_builder.shared.local_runtime;
        self.runtime.extend(local_runtime);

        let optimizer =
            GlobalOptimizer::new(&self.compile_context.config, &self.runtime, functions);
        let (functions, stats) = optimizer.run();

        Ok(Llir {
            functions,
            runtime: self.runtime,
            entry_function: entry_block_id,
            stats,
        })
    }
}

/// Small hack to prevent borrow checker problems where rust would think that the entire `LlirBuilder` would get borrowed
pub(super) fn set_obj(
    namespace: &MirNamespace,
    type_ctx: &TypeContext,
    object_mapping: &mut FxHashMap<MirObjectId, ObjectRef>,
    obj_id: MirObjectId,
    value: ObjectRef,
) {
    // Resolve required values as early as possible.
    // If the property does not exist yet, it has to set by a mir instruction
    // before it is read first (otherwise an ice occurs.)
    for (ident, (id, _)) in namespace.get_obj_namespace(obj_id).iter() {
        if let Some(property) = value.get_property(type_ctx, ident) {
            set_obj(namespace, type_ctx, object_mapping, *id, property);
        }
    }

    object_mapping.insert(obj_id, value);
}

#[derive(Default)]
pub struct BlockIdGenerator {
    next_id: Cell<u32>,
}

impl BlockIdGenerator {
    pub fn next_id(&self) -> BlockId {
        let id = self.next_id.get();
        self.next_id.set(id + 1);
        BlockId(id)
    }
}

#[derive(Debug, Clone)]
pub enum FunctionParameter {
    Parameter {
        span: Span,
        index: usize,
        template: ObjectRef,
        class: ClassRef,
        obj_id: MirObjectId,
    },
    Generic {
        span: Span,
        index: usize,
        class: ClassRef,
        obj_id: MirObjectId,
    },
}

impl FunctionParameter {
    pub fn class(&self) -> &ClassRef {
        match self {
            FunctionParameter::Generic { class, .. }
            | FunctionParameter::Parameter { class, .. } => class,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            FunctionParameter::Generic { span, .. } | FunctionParameter::Parameter { span, .. } => {
                *span
            }
        }
    }

    pub fn obj_id(&self) -> MirObjectId {
        match self {
            FunctionParameter::Generic { obj_id, .. }
            | FunctionParameter::Parameter { obj_id, .. } => *obj_id,
        }
    }
}

pub type NativeFunctionId = usize;

/// Stores the already compiled native functions
#[derive(Debug, Default)]
pub(super) struct NativeFunctionMap<'ctx> {
    max_id: usize,
    functions: FxHashMap<NativeFunctionId, FunctionGenerics<'ctx>>,
}

impl<'ctx> NativeFunctionMap<'ctx> {
    pub fn insert(&mut self, function_generics: FunctionGenerics<'ctx>) -> NativeFunctionId {
        let id = self.max_id + 1;
        self.max_id = id;
        self.functions.insert(id, function_generics);
        id
    }

    pub fn get<'a>(&'a self, id: NativeFunctionId) -> Option<&'a FunctionGenerics<'ctx>> {
        self.functions.get(&id)
    }

    pub fn get_mut<'a>(
        &'a mut self,
        id: NativeFunctionId,
    ) -> Option<&'a mut FunctionGenerics<'ctx>> {
        self.functions.get_mut(&id)
    }
}

#[derive(Debug, Clone)]
pub(super) struct MonomorphizedFunction {
    pub block_id: BlockId,
    pub return_value: ObjectRef,
}

#[derive(Debug, Clone)]
pub(super) struct FunctionGenerics<'a> {
    pub instantiations: Vec<(Vec<ObjectRef>, MonomorphizedFunction)>,
    // A vector containing all runtime default parameters
    pub function_parameters: Vec<FunctionParameter>,
    // The signature of this function
    pub signature: FunctionClassRef,
    pub mir_function: &'a MirFunction,
}

impl<'a> FunctionGenerics<'a> {
    pub fn new(
        ctx: &TypeContext,
        mir_function: &'a MirFunction,
        function_parameters: Vec<FunctionParameter>,
        return_class: ClassRef,
    ) -> Self {
        let signature = FunctionClass {
            parameters: function_parameters
                .iter()
                .map(|it| ObjClass::new(it.class().clone()).into_object(ctx))
                .collect(),
            return_class: ObjClass::new(return_class).into_object(ctx),
        }
        .into();
        FunctionGenerics {
            instantiations: Default::default(),
            function_parameters,
            signature,
            mir_function,
        }
    }

    pub fn generic_instantiation<'b>(
        &self,
        generics: &(impl Iterator<Item = &'b ObjectRef> + Clone),
    ) -> Option<(usize, &MonomorphizedFunction)> {
        self.instantiations
            .iter()
            .enumerate()
            .find(|(_, (instantiated_generics, _))| {
                instantiated_generics
                    .iter()
                    .zip_eq(generics.clone())
                    .all(|(required, got)| required == got)
            })
            .map(|(index, (_, function))| (index, function))
    }

    pub fn generic_instantiation_by_index(&self, index: usize) -> Option<&MonomorphizedFunction> {
        self.instantiations
            .get(index)
            .map(|(_, monomorphization)| monomorphization)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CallStack<'a> {
    Some {
        function_id: NativeFunctionId,
        block_id: BlockId,
        prev: &'a CallStack<'a>,
    },
    None,
}

impl CallStack<'_> {
    pub fn contains(&self, id: NativeFunctionId) -> bool {
        match self {
            &CallStack::Some {
                prev, function_id, ..
            } => function_id == id || prev.contains(id),
            CallStack::None => false,
        }
    }

    pub fn block_id_for(&self, id: NativeFunctionId) -> Option<BlockId> {
        match self {
            &CallStack::Some {
                prev,
                function_id,
                block_id,
            } => {
                if function_id == id {
                    Some(block_id)
                } else {
                    prev.block_id_for(id)
                }
            }
            CallStack::None => None,
        }
    }

    /// Returns a new [`CallStack`] that contains the given function id on top
    pub fn then(&self, id: Option<(NativeFunctionId, BlockId)>) -> CallStack {
        match id {
            Some((id, block_id)) => CallStack::Some {
                function_id: id,
                block_id,
                prev: self,
            },
            None => *self,
        }
    }
}
