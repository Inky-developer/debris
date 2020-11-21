use debris_common::Ident;
use generational_arena::{Arena, Index};
use rustc_hash::FxHashMap;

#[derive(Eq, PartialEq, Clone)]
pub struct Namespace<T> {
    /// Own namespace id
    own_id: Index,
    /// Id of the ancestor namespace
    ancestor: Option<Index>,
    /// The actual values that this namespace contains.
    /// This vector may never shrink.
    values: Vec<T>,
    /// Maps Idents to indexes of `values`
    keymap: FxHashMap<Ident, u64>,
}

impl<T> Namespace<T> {
    /// Creates a new Namespace
    pub fn new(own_id: Index, ancestor: Option<Index>) -> Self {
        Namespace {
            own_id,
            ancestor,
            values: Vec::new(),
            keymap: FxHashMap::default(),
        }
    }

    pub fn ancestor(&self) -> Option<Index> {
        self.ancestor
    }

    /// Adds an object with a name to this namespace
    ///
    /// If the name already exist, it gets overridden.
    /// The id of the old value will then get returned.
    pub fn add_object(&mut self, ident: &Ident, value: T) -> Result<(), u64> {
        self.register_key(ident)?;
        self.add_value(value);

        Ok(())
    }

    /// Adds an anonymous object (without name) to this namespace
    ///
    /// Returns a reference to the added value.
    pub fn add_value(&mut self, value: T) {
        self.values.push(value);
    }

    /// Replaces the object with this id
    ///
    /// Returns the old value.
    /// Panics if the id does not exist.
    pub fn replace_object_at(&mut self, id: u64, value: T) -> T {
        if self.len() <= id {
            panic!("Invalid id access")
        }

        std::mem::replace(&mut self.values[id as usize], value)
    }

    /// The amount of objects in this namespace
    ///
    /// Note: the parent namespace is not included
    pub fn len(&self) -> u64 {
        self.values.len() as u64
    }

    /// Thanks, clippy...
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Retrieves a named object from this namespace
    pub fn get<'a>(&self, arena: &'a Arena<Self>, ident: &Ident) -> Option<&'a T> {
        let mut current_object = arena.get(self.own_id);
        while let Some(object) = current_object {
            if let Some(value) = object.keymap.get(ident) {
                return object.get_by_id(*value);
            }

            current_object = object
                .ancestor
                .map(|val| arena.get(val).expect("Invalid ancestor"));
        }

        None
    }

    /// Retrieves an object by its id from this namespace
    pub fn get_by_id(&self, id: u64) -> Option<&T> {
        self.values.get(id as usize)
    }

    /// Returns the id the next inserted value would get
    pub fn next_id(&self) -> u64 {
        self.len()
    }

    /// Registers the key with the next id slot
    ///
    /// Returns whether the key was overriden
    fn register_key(&mut self, key: &Ident) -> Result<(), u64> {
        let prev = self.keymap.insert(key.clone(), self.next_id());

        if let Some(id) = prev {
            Err(id)
        } else {
            Ok(())
        }
    }
}

impl<T> From<Index> for Namespace<T> {
    fn from(value: Index) -> Self {
        Namespace::new(value, None)
    }
}

impl<T> std::fmt::Debug for Namespace<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Namespace").field(&self.keymap).finish()
    }
}
