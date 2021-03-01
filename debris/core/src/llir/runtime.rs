use rustc_hash::{FxHashSet};

use super::utils::BlockId;

#[derive(Debug, Default)]
pub struct Runtime {
    scheduled_blocks: FxHashSet<BlockId>,
    load_blocks: FxHashSet<BlockId>,
}

/// The `Runtime` is used at the llir generation to
/// store metadata to be used by the backend
impl Runtime {
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
