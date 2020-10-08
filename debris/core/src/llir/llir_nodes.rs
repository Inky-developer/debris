use super::utils::{ItemId, Scoreboard, ScoreboardOperation, ScoreboardValue};

/// A function nodes, contains other nodes
#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    /// The id of this specifc function
    pub id: u64,
    /// The nodes which this function contains
    pub nodes: Vec<Node>,
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
    /// The id of the target vat
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
    pub id: u64,
}

/// Evaluates a condition and returns either true or false
///
/// Wip
#[derive(Debug, Eq, PartialEq)]
pub enum Condition {}

/// Branches based on a condition
///
/// Wip
#[derive(Debug, Eq, PartialEq)]
pub struct Branch {
    /// The condition to test
    pub conditions: Vec<Condition>,
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
    Branch(Branch),
    Execute(Execute),
}
