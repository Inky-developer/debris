use crate::{mir::ContextId, ObjectRef};

use super::utils::{
    ItemId, Scoreboard, ScoreboardComparison, ScoreboardOperation, ScoreboardValue,
};

/// A function node, contains other nodes
#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    /// The id of the context that created this function
    /// The context id uniquely identifies this function
    pub id: ContextId,
    /// The nodes which this function contains
    pub nodes: Vec<Node>,
    /// The value that this function returns
    pub returned_value: ObjectRef,
}

/// Stores a 'fast' variable
///
/// Fast variables are scoreboard values.
#[derive(Debug, Eq, PartialEq)]
pub struct FastStore {
    /// The scoreboard of the target var
    pub scoreboard: Scoreboard,
    /// The id of the target var
    pub id: ItemId,
    /// The value to store into the target var
    pub value: ScoreboardValue,
}

/// Stores a 'fast' variable from the result of another node
#[derive(Debug, Eq, PartialEq)]
pub struct FastStoreFromResult {
    /// The scoreboard of the target var
    pub scoreboard: Scoreboard,
    /// The id of the target var
    pub id: ItemId,
    /// The command to use
    pub command: Box<Node>,
}

/// Operates on two scoreboard values and stores the result into the tagert var
#[derive(Debug, Eq, PartialEq)]
pub struct BinaryOperation {
    /// The scoreboard of the resulting value
    pub scoreboard: Scoreboard,
    /// The id of the resulting value
    pub id: ItemId,
    /// The left value
    pub lhs: ScoreboardValue,
    /// The right value
    pub rhs: ScoreboardValue,
    /// The kind of operation
    pub operation: ScoreboardOperation,
}

/// Calls a function
#[derive(Debug, Eq, PartialEq)]
pub struct Call {
    /// The id of that function
    pub id: ContextId,
}

/// Evaluates a condition and returns either true or false
///
/// Wip
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Condition {
    /// Comparison between two values, eg. val1 <= val2
    Compare {
        lhs: ScoreboardValue,
        rhs: ScoreboardValue,
        comparison: ScoreboardComparison,
    },
    And {
        conditions: Vec<Condition>,
    },
}

/// Branches based on a condition
///
/// Wip
#[derive(Debug, Eq, PartialEq)]
pub struct Branch {
    /// The condition to test
    pub condition: Condition,
    /// The node to execute if that condition is true
    pub pos_branch: Box<Node>,
    /// The node to execute if that condition is false
    pub neg_branch: Option<Box<Node>>,
}

/// Executes a literal string
#[derive(Debug, Eq, PartialEq)]
pub struct Execute {
    /// The command to execute
    pub command: String,
}

/// Any node
#[derive(Debug, Eq, PartialEq)]
pub enum Node {
    Function(Function),
    FastStore(FastStore),
    FastStoreFromResult(FastStoreFromResult),
    BinaryOperation(BinaryOperation),
    Call(Call),
    Condition(Condition),
    Branch(Branch),
    Execute(Execute),
}
