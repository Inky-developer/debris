use std::fmt;

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

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Block({})", self.0)
    }
}
