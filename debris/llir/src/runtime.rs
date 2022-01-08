use rustc_hash::{FxHashMap, FxHashSet};

use crate::extern_item_path::ExternItemPath;

use super::block_id::BlockId;

#[derive(Debug, Default)]
pub struct Runtime {
    pub scheduled_blocks: FxHashSet<BlockId>,
    pub load_blocks: FxHashSet<BlockId>,
    pub extern_blocks: FxHashMap<BlockId, ExternItemPath>,
}

/// The `Runtime` is used at the llir generation to
/// store metadata to be used by the backend
impl Runtime {
    /// Returns whether this function contains the given `id`.
    pub fn contains(&self, id: &BlockId) -> bool {
        self.scheduled_blocks.contains(id)
            || self.load_blocks.contains(id)
            || self.extern_blocks.contains_key(id)
    }

    /// Gives an iterator over the blocks which are the root blocks.
    /// Root blocks are blocks which are called by the system,
    /// In this case the scheduled blocks and the on load blocks.
    pub fn root_blocks(&self) -> impl Iterator<Item = BlockId> + '_ {
        self.scheduled_blocks
            .iter()
            .chain(self.load_blocks.iter())
            .chain(self.extern_blocks.keys())
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

    /// Adds a specific block as an extern item with the given name
    /// These functions can be manually invoked
    pub fn add_extern_block(&mut self, block: BlockId, name: ExternItemPath) {
        self.extern_blocks.insert(block, name);
    }
}
