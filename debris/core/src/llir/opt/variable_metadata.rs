//! This module contains structs which can hold metadata for variables.
//! This includes so called `ValueHints` which allow to deduce the set of possible
//! runtime values a variable can have.
//! This module also contains variable usage data which contain information about the amount
//! of reads and writes a specific variable gets
use std::usize;

use rustc_hash::FxHashMap;

use crate::llir::{
    llir_nodes::{BinaryOperation, Call, Condition, FastStore, FastStoreFromResult, Node},
    utils::{ItemId, ScoreboardValue},
};

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

impl Hint {
    pub fn exact(&self) -> Option<i32> {
        match self {
            Hint::Unknown => None,
            Hint::Exact(val) => Some(*val),
        }
    }
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

    /// Tries to get the static value of a scoreboard value
    pub fn get_scoreboard_value(&self, value: &ScoreboardValue) -> Option<i32> {
        match value {
            ScoreboardValue::Scoreboard(_, id) => self.get_hint(id).exact(),
            &ScoreboardValue::Static(val) => Some(val),
        }
    }

    /// Updates the hints for all variables that this node modifies
    pub fn update_hints(&mut self, node: &Node) {
        node.iter_with_guarantee(&mut |node, guaranteed_run| match &node {
            Node::FastStore(FastStore {
                id,
                value,
                scoreboard: _,
            }) => {
                if guaranteed_run {
                    match value {
                        ScoreboardValue::Static(static_value) => {
                            self.set_hint(*id, Hint::Exact(*static_value));
                        }
                        ScoreboardValue::Scoreboard(_scoreboard, other_id) => {
                            let other_value_hint = self.get_hint(other_id);
                            self.set_hint(*id, other_value_hint);
                        }
                    }
                } else {
                    self.clear_hint(*id)
                }
            }
            Node::FastStoreFromResult(FastStoreFromResult {
                id,
                scoreboard: _,
                command: _,
            }) => {
                self.clear_hint(*id);
            }
            Node::BinaryOperation(BinaryOperation {
                scoreboard: _,
                id,
                lhs: _,
                rhs: _,
                operation: _,
            }) => {
                self.clear_hint(*id);
            }
            Node::Call(Call { id: _ }) => self.clear_all(),
            Node::Condition(_) => {}
            // Since we iterate the sub-nodes, no clearing needs to be done for branches
            Node::Branch(_) => {}
            // Any execute node that modifies a value that belongs to debris cause
            // undefined behavior!
            Node::Execute(_) => {}
            Node::Write(_) => {}
            Node::Nop => {}
        });
    }

    /// Tries to evaluate a binary operation with static values
    pub fn static_binary_operation(&self, bin_op: &BinaryOperation) -> Option<i32> {
        let BinaryOperation {
            scoreboard: _,
            id: _,
            lhs,
            rhs,
            operation,
        } = bin_op;

        let lhs_value = self.get_scoreboard_value(lhs)?;
        let rhs_value = self.get_scoreboard_value(rhs)?;

        Some(operation.evaluate(lhs_value, rhs_value))
    }

    /// Tries to evaluate a condition with static values
    pub fn static_condition(&self, condition: &Condition) -> Option<bool> {
        match condition {
            Condition::Compare {
                lhs,
                rhs,
                comparison,
            } => {
                let lhs_value = self.get_scoreboard_value(lhs)?;
                let rhs_value = self.get_scoreboard_value(rhs)?;
                Some(comparison.evaluate(lhs_value, rhs_value))
            }
            Condition::And(parts) => {
                for part in parts {
                    let value = self.static_condition(part)?;
                    if !value {
                        return Some(false);
                    }
                }
                Some(true)
            }
            Condition::Or(parts) => {
                for part in parts {
                    let value = self.static_condition(part)?;
                    if value {
                        return Some(true);
                    }
                }
                Some(false)
            }
        }
    }
}

/// General data about the usage of a specific variable
#[derive(Debug, Default, Clone)]
pub struct VariableUsage {
    pub reads: usize,
    pub writes: usize,
    pub constant_value: Option<i32>,
}

impl VariableUsage {
    pub fn add_read(&mut self) {
        self.reads += 1;
    }

    pub fn remove_read(&mut self) {
        self.reads -= 1;
    }

    pub fn add_write(&mut self, value: Option<i32>) {
        self.writes += 1;
        if self.writes == 1 {
            self.constant_value = value;
        } else {
            self.constant_value = None;
        }
    }

    pub fn remove_write(&mut self) {
        self.writes -= 1;
        self.constant_value = None;
    }
}
