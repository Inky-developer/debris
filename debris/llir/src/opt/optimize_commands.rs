use std::{
    cmp::Ordering,
    collections::VecDeque,
    ops::{Deref, DerefMut},
};

use crate::{
    llir_nodes::{Condition, Node},
    utils::{BlockId, ItemId, ScoreboardValue},
};

use super::NodeId;

/// Optimizing functions output commands that tell the optimizer what to do,
/// this is done so that there are no troubles with mutability
#[derive(Debug)]
pub enum OptimizeCommandKind {
    /// Deletes a single node
    Delete,
    /// Converts the [FastStoreFromResult](crate::llir::llir_nodes::FastStoreFromResult) node into its command, discarding
    /// the result
    DiscardResult,
    /// Discards the node and only keeps the branch that matches the bool.
    /// (true => pos_branch, false => neg_branch)
    InlineBranch(bool),
    /// Updates the specified branch with the new node
    UpdateBranch { branch: bool, new_node: Node },
    /// Updates the condition of a branch
    UpdateBranchCondition(Condition),
    /// Inlines the function of this function call.
    InlineFunction,
    /// Removes all aliases to a function which only redirects to another function
    /// The argument specifies the aliased function.
    RemoveAliasFunction(BlockId),
    /// Changes the variable this node writes to
    ChangeWrite(ItemId),
    /// Replaces all variables `.0` with `.1`
    ChangeReads(ItemId, ScoreboardValue),
    /// Changes the condition of this branch to a new condition
    /// Vec usize contains the exact index of the condition to replace
    /// (Vec since conditions can be nested)
    SetCondition(Condition, Vec<usize>),
    /// Replaces the old node completely
    Replace(Node),
    /// Inserts this node after
    InsertAfter(Node),
}

#[derive(Debug)]
pub struct OptimizeCommand {
    pub id: NodeId,
    pub kind: OptimizeCommandKind,
}

impl OptimizeCommand {
    pub fn new(id: NodeId, kind: OptimizeCommandKind) -> Self {
        OptimizeCommand { id, kind }
    }

    /// Shifts the node id of this command one back
    pub fn shift(&mut self, amt: i8) {
        match amt.cmp(&0) {
            Ordering::Greater => self.id.1 += amt as usize,
            Ordering::Less => self.id.1 -= amt.abs() as usize,
            Ordering::Equal => {}
        }
    }

    pub fn shift_back(&mut self) {
        self.shift(-1)
    }
    pub fn shift_forward(&mut self) {
        self.shift(1)
    }
}

/// Just a wrapper around deque with a simple push method
#[derive(Debug)]
pub struct OptimizeCommandDeque<T>(VecDeque<T>);

impl<T> OptimizeCommandDeque<T> {
    pub fn push(&mut self, value: T) {
        self.0.push_back(value);
    }
}

impl<T> Deref for OptimizeCommandDeque<T> {
    type Target = VecDeque<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for OptimizeCommandDeque<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Default for OptimizeCommandDeque<T> {
    fn default() -> Self {
        OptimizeCommandDeque(VecDeque::default())
    }
}
