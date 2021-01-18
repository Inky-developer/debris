use crate::{mir::ContextId, ObjectRef};

use super::{
    opt::peephole::PeepholeOptimizer,
    utils::{ItemId, Scoreboard, ScoreboardComparison, ScoreboardOperation, ScoreboardValue},
};

/// A function node, contains other nodes
#[derive(Debug)]
pub struct Function {
    /// The id of the context that created this function
    /// The context id uniquely identifies this function
    pub id: ContextId,
    /// The nodes which this function contains
    pub(crate) nodes: PeepholeOptimizer,
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
#[derive(Debug)]
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
    And(Vec<Condition>),
    Or(Vec<Condition>),
}

/// Branches based on a condition
///
/// Wip
#[derive(Debug)]
pub struct Branch {
    /// The condition to test
    pub condition: Condition,
    /// The node to execute if that condition is true
    pub pos_branch: Box<Node>,
    /// The node to execute if that condition is false
    pub neg_branch: Option<Box<Node>>,
}

/// Executes a literal string
///
/// Any String that modifies a variable that belongs to debris
/// or modifies the program state in any other way
/// causes undefined behavior!
#[derive(Debug, Eq, PartialEq)]
pub struct Execute {
    /// The command to execute
    pub command: String,
}

/// Any node
#[derive(Debug)]
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

impl Function {
    pub fn nodes(&self) -> &[Node] {
        self.nodes.as_slice()
    }
}

impl Condition {
    /// Returns a condition that is true, when
    /// this condition is false and the other
    /// way around
    pub fn not(&self) -> Condition {
        match self {
            Condition::Compare {
                comparison,
                lhs,
                rhs,
            } => Condition::Compare {
                comparison: comparison.invert(),
                lhs: *lhs,
                rhs: *rhs,
            },
            Condition::And(_) => {
                unimplemented!("ToDo: add support for negating or-ing conditions")
            }
            Condition::Or(_) => {
                unimplemented!("ToDo: add support for negating and-ing conditions")
            }
        }
    }

    /// Checks whether this condition is "simple",
    /// which means that it is not expensive to use this condition multiple times
    /// instead of a boolean.
    ///
    /// Right now, only `Condition::Compare` is considered simple.
    pub fn is_simple(&self) -> bool {
        matches!(self, Condition::Compare{..})
    }
}
