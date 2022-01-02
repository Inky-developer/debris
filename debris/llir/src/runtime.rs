use rustc_hash::FxHashSet;

use super::utils::BlockId;

#[derive(Debug, Default)]
pub struct Runtime {
    pub scheduled_blocks: FxHashSet<BlockId>,
    pub load_blocks: FxHashSet<BlockId>,
}

/// The `Runtime` is used at the llir generation to
/// store metadata to be used by the backend
impl Runtime {
    /// Returns whether this function contains the given `id`.
    pub fn contains(&self, id: &BlockId) -> bool {
        self.scheduled_blocks.contains(id) || self.load_blocks.contains(id)
    }

    /// Gives an iterator over the blocks which are the root blocks.
    /// Root blocks are blocks which are called by the system,
    /// In this case the scheduled blocks and the on load blocks.
    pub fn root_blocks(&self) -> impl Iterator<Item = BlockId> + '_ {
        self.scheduled_blocks
            .iter()
            .chain(self.load_blocks.iter())
            .copied()
    }

    /// Schedules a specific block to run every tick.
    pub fn schedule(&mut self, block: BlockId) {
        self.scheduled_blocks.insert(block);
    }

    /// Adds a specific block to the list of blocks which
    /// should be run when the program is loaded.
    pub fn add_on_load(&mut self, block: BlockId) {
        self.load_blocks.insert(block);
    }
}
