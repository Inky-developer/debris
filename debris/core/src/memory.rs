use crate::{
    llir::{
        llir_nodes::{FastStore, Node},
        utils::{ItemId, Scoreboard, ScoreboardValue},
    },
    mir::ContextId,
    ObjectRef,
};

/// Copies a scoreboard value from source to destination
#[must_use]
pub fn copy(dest: ItemId, source: ItemId) -> Node {
    Node::FastStore(FastStore {
        id: dest,
        scoreboard: Scoreboard::Main,
        value: ScoreboardValue::Scoreboard(Scoreboard::Main, source),
    })
}

/// Copies all items from source over to destination
pub fn mem_copy<F>(mut add_node: F, dest: &ObjectRef, source: &ObjectRef)
where
    F: FnMut(Node),
{
    let dest_layout = &dest.layout;
    let source_layout = &source.layout;

    if dest_layout.mem_size() != source_layout.mem_size() {
        panic!("Unmatching layouts")
    }

    match (dest_layout, source_layout) {
        (MemoryLayout::Unsized, MemoryLayout::Unsized) => (),
        (MemoryLayout::One(dest), MemoryLayout::One(source)) => {
            if dest != source {
                add_node(copy(*dest, *source))
            }
        }
        (MemoryLayout::Multiple(dest_vec), MemoryLayout::Multiple(source_vec))
            if dest_vec.len() == source_vec.len() =>
        {
            for (dest, source) in dest_vec.iter().zip(source_vec.iter()) {
                if dest != source {
                    add_node(copy(*dest, *source));
                }
            }
        }
        (destination, source) => panic!("Incompatible layouts: {:?} and {:?}", destination, source),
    }
}
/// Specifies how a specific object lays out its runtime memory
#[derive(Debug, PartialEq, Eq)]
pub enum MemoryLayout {
    /// This type has no runtime memory
    Unsized,
    /// This type has exactly one integer sized field
    One(ItemId),
    /// This type is spread across multiple fields
    Multiple(Vec<ItemId>),
}

impl MemoryLayout {
    /// Returns the amount of words that this layout occupies
    fn mem_size(&self) -> usize {
        match self {
            MemoryLayout::Unsized => 0,
            MemoryLayout::One(_) => 1,
            MemoryLayout::Multiple(words) => words.len(),
        }
    }
}

/// Counter that can give out unique `ItemId`'s
#[derive(Debug, PartialEq, Eq)]
pub struct MemoryCounter {
    context_id: ContextId,
    id: usize,
}

impl MemoryCounter {
    pub fn new(context_id: ContextId, id: usize) -> Self {
        MemoryCounter { context_id, id }
    }

    pub fn next_item(&mut self) -> ItemId {
        ItemId {
            context: self.context_id,
            id: self.next_id(),
        }
    }

    pub fn next_id(&mut self) -> usize {
        let id = self.id;
        self.id += 1;
        id
    }
}
