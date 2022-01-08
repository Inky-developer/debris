/// Identifies a single callable block of code
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct BlockId(pub(super) u32);

impl BlockId {
    /// Creates a dummy [`BlockId`],
    /// For testing only
    pub fn dummy(value: u32) -> Self {
        BlockId(value)
    }
}
