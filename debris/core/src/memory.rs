use crate::{
    llir::{
        llir_nodes::{FastStore, Node},
        utils::{ItemId, Scoreboard, ScoreboardValue},
    },
    ObjectRef,
};

/// Specifies how a specific object lays out its runtime memory
#[derive(Debug, PartialEq, Eq)]
pub enum MemoryLayout {
    /// This type has no runtime memory
    Zero,
    /// This type has exactly one field
    One(ItemId),
    /// This type is spread across multiple fields
    Multiple(Vec<ItemId>),
}

/// Copies a scoreboard value from source to destination
pub fn copy(dest: ItemId, source: ItemId) -> Node {
    Node::FastStore(FastStore {
        id: dest,
        scoreboard: Scoreboard::Main,
        value: ScoreboardValue::Scoreboard(Scoreboard::Main, source),
    })
}

/// Copies all items from source over to destination
pub fn mem_move(nodes: &mut Vec<Node>, dest: &ObjectRef, source: &ObjectRef) {
    let dest_layout = &dest.layout;
    let source_layout = &source.layout;

    if dest_layout.mem_size() != source_layout.mem_size() {
        panic!("Unmatching layouts")
    }

    match (dest_layout, source_layout) {
        (MemoryLayout::Zero, MemoryLayout::Zero) => (),
        (MemoryLayout::One(dest), MemoryLayout::One(source)) => nodes.push(copy(*dest, *source)),
        (MemoryLayout::Multiple(dest_vec), MemoryLayout::Multiple(source_vec))
            if dest_vec.len() == source_vec.len() =>
        {
            for (dest, source) in dest_vec.iter().zip(source_vec.iter()) {
                nodes.push(copy(*dest, *source));
            }
        }
        (destination, source) => panic!("Incompatible layouts: {:?} and {:?}", destination, source),
    }
}

impl MemoryLayout {
    /// Returns the amount of words that this layout occupies
    fn mem_size(&self) -> usize {
        match self {
            MemoryLayout::Zero => 0,
            MemoryLayout::One(_) => 1,
            MemoryLayout::Multiple(words) => words.len(),
        }
    }
}
