use std::hash::BuildHasherDefault;

use debris_common::{Ident, Span};
use indexmap::IndexMap;
use rustc_hash::{FxHashMap, FxHasher};

use crate::{
    memory::MemoryCounter,
    mir::{ContextId, MirValue, NamespaceArena, NamespaceIndex},
};

/// A value in the mir namespace
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum NamespaceEntry {
    /// A spanned entry corresponds to a variable declared somewhere
    Variable { span: Span, value: MirValue },
    /// An anonymous value is usually a temporary value
    Anonymous(MirValue),
}

impl NamespaceEntry {
    pub fn value(&self) -> &MirValue {
        match self {
            NamespaceEntry::Variable { span: _, value } => value,
            NamespaceEntry::Anonymous(value) => value,
        }
    }

    pub fn span(&self) -> Option<&Span> {
        match self {
            NamespaceEntry::Anonymous(_value) => None,
            NamespaceEntry::Variable { span, value: _ } => Some(span),
        }
    }

    /// Returns `true` if the namespace_entry is [`NamespaceEntry::Variable`].
    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable { .. })
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Namespace {
    /// Own namespace id
    own_id: NamespaceIndex,
    /// Id of the ancestor namespace
    ancestor: Option<NamespaceIndex>,
    /// The actual values that this namespace contains.
    /// This map may never shrink
    values: FxHashMap<u32, NamespaceEntry>,
    /// Maps Idents to indexes of `values`
    keymap: IndexMap<Ident, u32, BuildHasherDefault<FxHasher>>,
    /// The current id counter for items that get added to this namespace
    pub id_counter: MemoryCounter,
}

impl Namespace {
    /// Creates a new Namespace
    pub fn new(own_id: ContextId, ancestor: Option<NamespaceIndex>) -> Self {
        Namespace {
            own_id: own_id.as_inner(),
            ancestor,
            values: Default::default(),
            keymap: Default::default(),
            id_counter: MemoryCounter::new(own_id, 0),
        }
    }

    pub fn iter(&self) -> NamespaceIterator {
        self.into_iter()
    }

    pub fn ancestor(&self) -> Option<NamespaceIndex> {
        self.ancestor
    }

    /// Declares an entry in this namespace as variable.
    /// This information can be used by `MirBuilder`.
    /// For example, entries marked as `Variable` get cloned when passed
    /// to debris functions, while anonymous values do not get cloned.
    pub fn declare_as_variable(&mut self, id: u32, span: Span) {
        match self.get_by_id(id).unwrap() {
            NamespaceEntry::Anonymous(value) => {
                let value = value.clone();
                self.replace_object_at(id, NamespaceEntry::Variable { span, value });
            }
            NamespaceEntry::Variable { .. } => {}
        }
    }

    /// Adds an object with a name to this namespace
    ///
    /// If the name already exist, it gets overridden.
    /// The id of the old value will then get returned.
    pub fn add_object(&mut self, ident: Ident, value: NamespaceEntry) -> Option<u32> {
        let id = self.add_value(value);
        self.register_key_at(ident, id)
    }

    /// Adds an anonymous object (without name) to this namespace
    ///
    /// Returns a reference to the added value.
    pub fn add_value(&mut self, value: NamespaceEntry) -> u32 {
        let id = self.id_counter.next_id();
        self.values.insert(id, value);
        id
    }

    /// Inserts the value at this index and returns the
    /// last value, if existed
    pub fn add_value_at(&mut self, id: u32, value: NamespaceEntry) -> Option<NamespaceEntry> {
        self.values.insert(id, value)
    }

    /// Replaces the object with this id
    ///
    /// Returns the old value.
    /// Panics if the id does not exist.
    pub fn replace_object_at(&mut self, id: u32, value: NamespaceEntry) -> NamespaceEntry {
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

    /// Retrieves a named object from this namespace together with its index
    pub fn get<'a>(
        &self,
        arena: &'a NamespaceArena,
        ident: &Ident,
    ) -> Option<(u32, &'a NamespaceEntry)> {
        let mut current_object = Some(arena.get(self.own_id));
        while let Some(object) = current_object {
            if let Some(value) = object.keymap.get(ident) {
                return Some((
                    *value,
                    object.get_by_id(*value).expect("This object must exist"),
                ));
            }

            current_object = object.ancestor.map(|val| arena.get(val));
        }

        None
    }

    /// Retrieves an object by its id from this namespace
    pub fn get_by_id(&self, id: u32) -> Option<&NamespaceEntry> {
        self.values.get(&id)
    }

    /// Registers the key with the next id slot
    ///
    /// Returns whether the key was overriden
    fn register_key_at(&mut self, key: Ident, id: u32) -> Option<u32> {
        self.keymap.insert(key, id)
    }
}

impl<'a> IntoIterator for &'a Namespace {
    type Item = <Self::IntoIter as Iterator>::Item;
    type IntoIter = NamespaceIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        NamespaceIterator {
            keymap_iter: self.keymap.iter(),
            namespace: self,
        }
    }
}

pub struct NamespaceIterator<'a> {
    namespace: &'a Namespace,
    keymap_iter: indexmap::map::Iter<'a, Ident, u32>,
}

impl<'a> Iterator for NamespaceIterator<'a> {
    type Item = (&'a Ident, &'a MirValue);

    fn next(&mut self) -> Option<Self::Item> {
        self.keymap_iter
            .next()
            .map(|(ident, id)| (ident, self.namespace.values.get(id).unwrap().value()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.keymap_iter.size_hint()
    }
}
