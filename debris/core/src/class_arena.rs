//! An arena that stores all classes used by the compiler

use generational_arena::{Arena, Index};

use crate::objects::obj_class::GenericClass;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct GenericClassId(pub(crate) Index);

impl From<Index> for GenericClassId {
    fn from(index: Index) -> Self {
        GenericClassId(index)
    }
}

#[derive(Debug, Default)]
pub struct GenericClassArena {
    pub(crate) arena: Arena<GenericClass>,
}

impl GenericClassArena {
    pub fn add(&mut self, class: GenericClass) -> GenericClassId {
        self.arena.insert(class).into()
    }

    pub fn get(&self, id: GenericClassId) -> &GenericClass {
        self.arena.get(id.0).expect("This arena can only grow")
    }

    pub fn get_mut(&mut self, id: GenericClassId) -> &mut GenericClass {
        self.arena.get_mut(id.0).expect("This arena can only grow")
    }
}
