use debris_common::{Ident, Span};
use generational_arena::{Arena, Index};
use rustc_hash::FxHashMap;

use crate::{
    memory::MemoryCounter,
    mir::{ContextId, MirValue},
};

/// A value in the mir namespace
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum NamespaceEntry {
    /// A spanned entry corresponds to a variable declared somewhere
    Spanned { span: Span, value: MirValue },
    /// An anonymous value is usually a temporary value
    Anonymous(MirValue),
}

impl NamespaceEntry {
    pub fn value(&self) -> &MirValue {
        match self {
            NamespaceEntry::Spanned { span: _, value } => value,
            NamespaceEntry::Anonymous(value) => value,
        }
    }

    pub fn span(&self) -> Option<&Span> {
        match self {
            NamespaceEntry::Anonymous(_value) => None,
            NamespaceEntry::Spanned { span, value: _ } => Some(span),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Namespace {
    /// Own namespace id
    own_id: Index,
    /// Id of the ancestor namespace
    ancestor: Option<Index>,
    /// The actual values that this namespace contains.
    /// This map may never shrink
    values: FxHashMap<u64, NamespaceEntry>,
    /// Maps Idents to indexes of `values`
    keymap: FxHashMap<Ident, u64>,
    /// The current id counter for items that get added to this namespace
    pub id_counter: MemoryCounter,
}

impl Namespace {
    /// Creates a new Namespace
    pub fn new(own_id: ContextId, ancestor: Option<Index>) -> Self {
        Namespace {
            own_id: own_id.as_inner(),
            ancestor,
            values: FxHashMap::default(),
            keymap: FxHashMap::default(),
            id_counter: MemoryCounter::new(own_id, 0),
        }
    }

    pub fn ancestor(&self) -> Option<Index> {
        self.ancestor
    }

    /// Returns whether a given item has a corresponding key
    pub fn has_item_key(&self, item: u64) -> bool {
        self.keymap.values().any(|id| {
            let value = self.get_by_id(*id);
            value
                .map(|value| {
                    value
                        .value()
                        .template()
                        .map(|(_, real_id)| real_id.id == item)
                        .unwrap_or(false)
                })
                .unwrap_or(false)
        })
    }

    /// Adds an object with a name to this namespace
    ///
    /// If the name already exist, it gets overridden.
    /// The id of the old value will then get returned.
    pub fn add_object(&mut self, ident: Ident, value: NamespaceEntry) -> Option<u64> {
        let id = self.add_value(value);
        self.register_key_at(ident, id)
    }

    /// Adds an anonymous object (without name) to this namespace
    ///
    /// Returns a reference to the added value.
    pub fn add_value(&mut self, value: NamespaceEntry) -> u64 {
        let id = self.id_counter.next_id();
        self.values.insert(id, value);
        id
    }

    /// Inserts the value at this index and returns the
    /// last value, if existed
    pub fn add_value_at(&mut self, id: u64, value: NamespaceEntry) -> Option<NamespaceEntry> {
        self.values.insert(id, value)
    }

    /// Replaces the object with this id
    ///
    /// Returns the old value.
    /// Panics if the id does not exist.
    pub fn replace_object_at(&mut self, id: u64, value: NamespaceEntry) -> NamespaceEntry {
        self.values.insert(id, value).expect("Invalid access")
    }

    /// The amount of objects in this namespace
    ///
    /// Note: the parent namespace is not included
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Thanks, clippy...
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Retrieves a named object from this namespace
    pub fn get<'a>(&self, arena: &'a Arena<Self>, ident: &Ident) -> Option<&'a NamespaceEntry> {
        let mut current_object = arena.get(self.own_id);
        while let Some(object) = current_object {
            if let Some(value) = object.keymap.get(ident) {
                return Some(object.get_by_id(*value).expect("This object must exist"));
            }

            current_object = object
                .ancestor
                .map(|val| arena.get(val).expect("Invalid ancestor"));
        }

        None
    }

    /// Retrieves an object by its id from this namespace
    pub fn get_by_id(&self, id: u64) -> Option<&NamespaceEntry> {
        self.values.get(&id)
    }

    /// Registers the key with the next id slot
    ///
    /// Returns whether the key was overriden
    fn register_key_at(&mut self, key: Ident, id: u64) -> Option<u64> {
        self.keymap.insert(key, id)
    }
}

impl From<ContextId> for Namespace {
    fn from(value: ContextId) -> Self {
        Namespace::new(value, None)
    }
}
