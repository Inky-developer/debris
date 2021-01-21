//! This module contains structs which can hold metadata for variables.
//! This includes so called `ValueHints` which allow to deduce the set of possible
//! runtime values a variable can have.
//! This module also contains variable usage data which contain information about the amount
//! of reads and writes a specific variable gets
use std::usize;

use rustc_hash::FxHashMap;

use crate::llir::utils::ItemId;

/// A hint about the possible value of a variable
#[derive(Debug, Clone, Copy)]
pub(crate) enum Hint {
    /// Hints that the value is unknown (the variable can take any value)
    Unknown,
    /// Hints that the value can have exactly one value (the variable becomes comptime known)
    Exact(i32),
    // /// Hints that the runtime value of this variable lies within this inclusive range
    // Range(i32, i32),
}

impl Default for Hint {
    fn default() -> Self {
        Hint::Unknown
    }
}

/// Stores the possible range of values of a runtime variable
#[derive(Debug, Default)]
pub(crate) struct ValueHints {
    hints: FxHashMap<ItemId, Hint>,
}

impl ValueHints {
    pub fn set_hint(&mut self, id: ItemId, hint: Hint) {
        self.hints.insert(id, hint);
    }

    /// Clears all hints for this id
    pub fn clear_hint(&mut self, id: ItemId) {
        self.hints.insert(id, Hint::default());
    }

    /// Clears all hints
    pub fn clear_all(&mut self) {
        self.hints.clear()
    }

    pub fn get_hint(&self, id: &ItemId) -> Hint {
        self.hints.get(id).cloned().unwrap_or_default()
    }
}

/// General data about the usage of a specific variable
#[derive(Debug, Default)]
pub struct VariableUsage {
    pub reads: usize,
    pub writes: usize,
}

impl VariableUsage {
    pub fn add_read(&mut self) {
        self.reads += 1;
    }

    pub fn add_write(&mut self) {
        self.writes += 1;
    }
}