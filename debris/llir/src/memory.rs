use crate::{
    llir_nodes::{FastStore, Node},
    utils::{ItemId, Scoreboard, ScoreboardValue},
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
    let dest_layout = dest.payload.memory_layout();
    let source_layout = source.payload.memory_layout();
    if dest_layout.mem_size() != source_layout.mem_size() {
        panic!("Layout mismatch")
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
#[derive(Debug, PartialEq, Eq, Clone)]
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
    pub fn mem_size(&self) -> usize {
        match self {
            MemoryLayout::Unsized => 0,
            MemoryLayout::One(_) => 1,
            MemoryLayout::Multiple(words) => words.len(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &ItemId> {
        let mut one_iter = None;
        let mut multiple_iter = None;
        match self {
            MemoryLayout::Unsized => {}
            MemoryLayout::One(item_id) => one_iter = Some(item_id),
            MemoryLayout::Multiple(item_ids) => multiple_iter = Some(item_ids.iter()),
        };
        one_iter
            .into_iter()
            .chain(multiple_iter.into_iter().flatten())
    }
}

impl From<Vec<ItemId>> for MemoryLayout {
    fn from(ids: Vec<ItemId>) -> Self {
        match ids.as_slice() {
            [] => MemoryLayout::Unsized,
            [one] => MemoryLayout::One(*one),
            _ => MemoryLayout::Multiple(ids),
        }
    }
}

impl<'a> FromIterator<&'a MemoryLayout> for MemoryLayout {
    fn from_iter<T: IntoIterator<Item = &'a MemoryLayout>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let first = iter.next();
        match first {
            None => MemoryLayout::Unsized,
            Some(first) => {
                let second = iter.next();
                match second {
                    None => first.clone(),
                    Some(second) => {
                        let mut items = Vec::with_capacity(first.mem_size() + second.mem_size());
                        items.extend(first.iter().copied());
                        items.extend(second.iter().copied());
                        items.extend(iter.map(MemoryLayout::iter).flatten().copied());
                        MemoryLayout::Multiple(items)
                    }
                }
            }
        }
    }
}
