//! This module contains structs which can hold metadata for variables.
//! This includes so called `ValueHints` which allow to deduce the set of possible
//! runtime values a variable can have.
//! This module also contains variable usage data which contain information about the amount
//! of reads and writes a specific variable gets
use std::usize;

use rustc_hash::FxHashMap;

use crate::{
    item_id::ItemId,
    llir_nodes::{
        BinaryOperation, Call, Condition, ExecuteRaw, ExecuteRawComponent, FastStore,
        FastStoreFromResult, Node,
    },
    minecraft_utils::ScoreboardValue,
};

/// A hint about the possible value of a variable
#[derive(Debug, Default, Clone, Copy)]
pub enum Hint {
    /// Hints that the value is unknown (the variable can take any value)
    #[default]
    Unknown,
    /// Hints that the value can have exactly one value (the variable becomes comptime known)
    Exact(i32),
    // /// Hints that the runtime value of this variable lies within this inclusive range
    // Range(i32, i32),
}

impl Hint {
    pub fn exact(self) -> Option<i32> {
        match self {
            Hint::Unknown => None,
            Hint::Exact(val) => Some(val),
        }
    }
}

/// Stores the possible range of values of a runtime variable
#[derive(Debug, Default)]
pub struct ValueHints {
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
        self.hints.clear();
    }

    pub fn get_hint(&self, id: ItemId) -> Hint {
        self.hints.get(&id).copied().unwrap_or_default()
    }

    /// Tries to get the static value of a scoreboard value
    pub fn get_scoreboard_value(&self, value: &ScoreboardValue) -> Option<i32> {
        match value {
            ScoreboardValue::Scoreboard(_, id) => self.get_hint(*id).exact(),
            &ScoreboardValue::Static(val) => Some(val),
        }
    }

    /// Updates the hints for all variables that this node modifies
    /// `guaranteed_run` specifies whether `node` will definitely execute
    pub fn update_hints(&mut self, node: &Node, guaranteed_run: bool) {
        match node {
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
                            let other_value_hint = self.get_hint(*other_id);
                            self.set_hint(*id, other_value_hint);
                        }
                    }
                } else {
                    self.clear_hint(*id);
                }
            }
            Node::FastStoreFromResult(FastStoreFromResult {
                id,
                scoreboard: _,
                command,
            }) => {
                self.update_hints(command, guaranteed_run);
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
            Node::Branch(branch) => {
                self.update_hints(&branch.pos_branch, false);
                self.update_hints(&branch.neg_branch, false);
            }
            Node::Execute(ExecuteRaw(components)) => {
                for component in components {
                    match component {
                        ExecuteRawComponent::ScoreboardValue(ScoreboardValue::Scoreboard(
                            _,
                            id,
                        )) => self.clear_hint(*id),
                        ExecuteRawComponent::Node(node) => self.update_hints(node, false),
                        ExecuteRawComponent::ScoreboardValue(_)
                        | ExecuteRawComponent::String(_) => {}
                    }
                }
            }
            // Conditions and chat writes should not modify any values
            Node::Condition(_) | Node::Write(_) | Node::Nop => {}
        }
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

    pub fn simplify_condition(&self, condition: &Condition) -> Option<Condition> {
        match condition {
            Condition::Compare {
                comparison,
                lhs,
                rhs,
            } => {
                let lhs = self
                    .get_scoreboard_value(lhs)
                    .map_or(*lhs, ScoreboardValue::Static);
                let rhs = self
                    .get_scoreboard_value(rhs)
                    .map_or(*rhs, ScoreboardValue::Static);
                let new_condition = Condition::Compare {
                    comparison: *comparison,
                    lhs,
                    rhs,
                };
                if &new_condition != condition {
                    return Some(new_condition);
                }
                None
            }
            Condition::And(parts) => {
                let new_parts = parts
                    .iter()
                    .map(|cond| {
                        self.simplify_condition(cond)
                            .unwrap_or_else(|| cond.clone())
                    })
                    .collect::<Vec<_>>();
                if &new_parts != parts {
                    return Some(Condition::And(new_parts));
                }
                None
            }
            Condition::Or(parts) => {
                let new_parts = parts
                    .iter()
                    .map(|cond| {
                        self.simplify_condition(cond)
                            .unwrap_or_else(|| cond.clone())
                    })
                    .collect::<Vec<_>>();
                if &new_parts != parts {
                    return Some(Condition::Or(new_parts));
                }
                None
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

    /// Returns whether the variable is written to at most once and read from at most once
    pub fn used_once(&self) -> bool {
        self.reads <= 1 && self.writes <= 1
    }
}
