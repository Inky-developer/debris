use std::{cell::Cell, mem};

use debris_common::clone_cell::CloneCell;
use debris_mir::{mir_context::MirContextId, mir_object::MirObjectId};
use elsa::FrozenMap;
use rustc_hash::FxHashMap;

use crate::{block_id::BlockId, llir_nodes::Function, ObjectRef, Runtime};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct SharedStateId(u32);

#[derive(Default)]
pub struct SharedStates {
    counter: Cell<u32>,
    inner: FrozenMap<SharedStateId, Box<SharedState>>,
}

impl SharedStates {
    pub fn next_id(&self) -> SharedStateId {
        let old_value = self.counter.get();
        self.counter.set(old_value.checked_add(1).unwrap());
        SharedStateId(old_value)
    }

    pub fn insert(&self, shared_state: SharedState) {
        let id = shared_state.id;
        assert!(self.inner.get(&id).is_none());
        self.inner.insert(id, Box::new(shared_state));
    }

    pub fn get(&self, id: SharedStateId) -> Option<&SharedState> {
        self.inner.get(&id)
    }
}

#[derive(Debug)]
pub struct SharedState {
    pub(super) id: SharedStateId,
    pub(super) ancestor: Option<SharedStateId>,
    pub(super) compiled_contexts: FxHashMap<MirContextId, BlockId>,
    pub(super) functions: FxHashMap<BlockId, Function>,
    pub(super) object_mapping: ObjectMapping,
    /// The local runtime
    pub(super) local_runtime: Runtime,
}

impl SharedState {
    pub fn new(id: SharedStateId, ancestor: Option<SharedStateId>) -> Self {
        SharedState {
            id,
            ancestor,
            compiled_contexts: Default::default(),
            functions: Default::default(),
            object_mapping: Default::default(),
            local_runtime: Default::default(),
        }
    }

    /// Adds the accumulated state of another function builder to the state
    /// of this if `perform_writes` is false
    #[must_use]
    pub fn unify_with(&mut self, mut state: SharedState, mode: EvaluationMode) -> SharedState {
        let is_monomorphization = match mode {
            EvaluationMode::Full => true,
            EvaluationMode::Monomorphization => false,
            EvaluationMode::Check => return state,
        };

        self.functions.extend(mem::take(&mut state.functions));
        self.local_runtime
            .extend(mem::take(&mut state.local_runtime));

        // If this context is a function body, don't add its functions and objects
        // to the previous scope. Otherwise, repeated monomorphization would lead to duplicate
        // namespaces and object mappings, which would be wrong.
        if is_monomorphization {
            self.compiled_contexts
                .extend(mem::take(&mut state.compiled_contexts));
            self.object_mapping.extend(state.object_mapping.iter());
        }
        state
    }
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

#[derive(Debug, Default)]
pub struct ObjectMapping {
    inner: FxHashMap<MirObjectId, CloneCell<ObjectRef>>,
}

impl ObjectMapping {
    pub fn get_raw(&self, id: MirObjectId) -> Option<&CloneCell<ObjectRef>> {
        self.inner.get(&id)
    }

    /// Inserts the entry into this map and returns the old value
    pub fn insert(&mut self, id: MirObjectId, value: ObjectRef) -> Option<ObjectRef> {
        self.inner.insert(id, value.into()).map(|cell| cell.get())
    }

    pub fn iter(&self) -> impl Iterator<Item = (MirObjectId, ObjectRef)> + '_ {
        self.inner.iter().map(|(id, cell)| (*id, cell.get()))
    }
}

impl Extend<(MirObjectId, ObjectRef)> for ObjectMapping {
    fn extend<T: IntoIterator<Item = (MirObjectId, ObjectRef)>>(&mut self, iter: T) {
        self.inner
            .extend(iter.into_iter().map(|(id, obj)| (id, obj.into())));
    }
}
