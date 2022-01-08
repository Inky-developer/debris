use std::fmt;

/// A unique identifier for an item, used to determine the
#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct ItemId {
    pub id: u32,
}

impl fmt::Display for ItemId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("item_{}", self.id))
    }
}

impl fmt::Debug for ItemId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

#[derive(Default)]
pub struct ItemIdAllocator {
    current: u32,
}

impl ItemIdAllocator {
    pub fn next_id(&mut self) -> ItemId {
        let id = self.current;
        self.current += 1;
        ItemId { id }
    }
}
