use std::collections::HashMap;

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
    class::ClassRef,
    llir_function_builder::LlirFunctionBuilder,
    llir_nodes::Function,
    utils::{BlockId, ItemIdAllocator},
    Llir, ObjectRef, Runtime,
};

use super::type_context::TypeContext;

pub struct LlirBuilder<'ctx> {
    pub(super) compile_context: &'ctx CompileContext,
    pub(super) type_context: TypeContext,
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
    pub(super) call_stack: CallStack,
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
            compile_context: ctx,
            type_context,
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
            call_stack: Default::default(),
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

        let sub_builder = LlirFunctionBuilder::new(entry_block_id, &mut self, contexts);
        let result = sub_builder.build(entry_context)?;
        self.functions.insert(entry_block_id, result);

        Ok(Llir {
            functions: self.functions,
            runtime: self.runtime,
            entry_function: entry_block_id,
        })
    }

    pub(super) fn get_obj(&self, obj_id: MirObjectId) -> ObjectRef {
        self.get_obj_opt(obj_id)
            .expect("Bad MIR (Value accessed before it is defined")
    }

    pub(super) fn get_obj_opt(&self, obj_id: MirObjectId) -> Option<ObjectRef> {
        self.object_mapping.get(&obj_id).cloned()
    }

    pub(super) fn _set_obj(&mut self, obj_id: MirObjectId, value: ObjectRef) {
        builder_set_obj(
            self.global_namespace,
            &self.type_context,
            &mut self.object_mapping,
            obj_id,
            value,
        );
    }

    // Compiles any context that is not in the current context list
    #[allow(clippy::option_if_let_else)]
    pub fn compile_context(
        &mut self,
        contexts: &'ctx FxHashMap<MirContextId, MirContext>,
        context_id: MirContextId,
    ) -> Result<(BlockId, ObjectRef)> {
        if let Some(block_id) = self.compiled_contexts.get(&context_id) {
            let ret_val = match self.functions.get(block_id) {
                Some(function) => function.return_value.clone(),
                // If the block is listed as compiled, but the function is not available yet, this must be a recursive call
                // Since the block was not compiled yet, just return null
                // I am not sure about the exact implications, but I'll just leave it until it causes problems
                None => self.type_context.null(),
            };
            Ok((*block_id, ret_val))
        } else {
            let block_id = self.block_id_generator.next_id();

            // Insert this into the list of compiled contexts before the context is actually compiled,
            // This allows easier recursion (check this if statement)
            self.compiled_contexts.insert(context_id, block_id);
            self.force_compile_context(contexts, context_id, block_id)
        }
    }

    /// Compiles any context, even if it is already compiled.
    /// Force compiling a context does not insert it into the list of compiled contexts
    /// This is used for example when monomorphizing functions, where the same function
    /// gets evaluated with different compile time values
    /// `block_id` is the id of the block which will get populated by this function
    pub fn force_compile_context(
        &mut self,
        contexts: &'ctx FxHashMap<MirContextId, MirContext>,
        context_id: MirContextId,
        block_id: BlockId,
    ) -> Result<(BlockId, ObjectRef)> {
        let builder = LlirFunctionBuilder::new(block_id, self, contexts);
        let llir_function = builder.build(contexts.get(&context_id).unwrap())?;
        let return_value = llir_function.return_value.clone();
        self.functions.insert(block_id, llir_function);
        Ok((block_id, return_value))
    }
}

/// Small hack to prevent borrow checker problems where rust would think that the entire `LlirBuilder` would get borrowed
pub(super) fn builder_set_obj(
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
            builder_set_obj(namespace, type_ctx, object_mapping, *id, property);
        }
    }

    object_mapping.insert(obj_id, value);
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
        span: Span,
        index: usize,
        template: ObjectRef,
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
            FunctionParameter::Generic { class, .. } => class,
            FunctionParameter::Parameter { template, .. } => &template.class,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            FunctionParameter::Generic { span, .. } | FunctionParameter::Parameter { span, .. } => {
                *span
            }
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
        generics: &(impl Iterator<Item = &'b ObjectRef> + Clone),
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

#[derive(Debug, Default)]
pub struct CallStack {
    pub functions: Vec<usize>,
}
