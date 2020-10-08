/// Identifies a unique template
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ItemIdentifier {
    pub context_id: u64,
    pub item_id: u64,
}

impl ItemIdentifier {
    /// Creates a new item identifier
    pub fn new(context_id: u64, item_id: u64) -> Self {
        ItemIdentifier {
            context_id,
            item_id,
        }
    }
}
