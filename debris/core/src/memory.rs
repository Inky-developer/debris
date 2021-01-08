use crate::{
    llir::{
        llir_nodes::{FastStore, Node},
        utils::{ItemId, Scoreboard, ScoreboardValue},
    },
    objects::FunctionContext,
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
pub fn copy(ctx: &mut FunctionContext, dest: ItemId, source: ItemId) {
    ctx.emit(Node::FastStore(FastStore {
        id: dest,
        scoreboard: Scoreboard::Main,
        value: ScoreboardValue::Scoreboard(Scoreboard::Main, source),
    }))
}

/// Copies all items from source over to destination
pub fn mem_move(ctx: &mut FunctionContext, params: &[ObjectRef]) {
    assert_eq!(params.len(), 2, "Expected exactly two arguments");
    let dest = &params[0];
    let source = &params[1];

    let dest_layout = &dest.layout;
    let source_layout = &source.layout;

    if dest_layout.mem_size() != source_layout.mem_size() {
        panic!("Unmatching layouts")
    }

    match (dest_layout, source_layout) {
        (MemoryLayout::Zero, MemoryLayout::Zero) => (),
        (MemoryLayout::One(dest), MemoryLayout::One(source)) => copy(ctx, *dest, *source),
        (MemoryLayout::Multiple(dest_vec), MemoryLayout::Multiple(source_vec))
            if dest_vec.len() == source_vec.len() =>
        {
            for (dest, source) in dest_vec.iter().zip(source_vec.iter()) {
                copy(ctx, *dest, *source);
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
