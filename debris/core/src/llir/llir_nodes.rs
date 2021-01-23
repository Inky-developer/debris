//! Contains every node that can be produced in the llir step.
//!
//! Note that changing any node kind can lead to miscompilations if it isn't also updated
//! at the optimizers!

use crate::{mir::ContextId, ObjectRef};

use super::{
    json_format::FormattedText,
    utils::{ItemId, Scoreboard, ScoreboardComparison, ScoreboardOperation, ScoreboardValue},
};

/// A function node, contains other nodes
#[derive(Debug)]
pub struct Function {
    /// The id of the context that created this function
    /// The context id uniquely identifies this function
    pub id: ContextId,
    /// The nodes which this function contains
    pub(crate) nodes: Vec<Node>,
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

/// Operates on two scoreboard values and stores the result into the target var
#[derive(Debug, Eq, PartialEq, Clone)]
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

/// Writes a formatted message
#[derive(Debug)]
pub struct Write {
    pub target: WriteTarget,
    pub message: FormattedText,
}

/// The buffer to write to
#[derive(Debug, Clone, Copy)]
pub enum WriteTarget {
    Chat,
    Actionbar,
    Title,
    Subtitle,
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
    Write(Write),
}

impl Function {
    pub fn nodes(&self) -> &[Node] {
        self.nodes.as_slice()
    }

    /// Checks if this function contains a node that calls this function
    /// Note that this is not a transitive check, ie. if this function calls
    /// another function and that other function calls this function,
    /// that is not consider as calls_itself
    pub fn calls_itself(&self) -> bool {
        for node in self.nodes() {
            let mut finished = false;
            node.iter(&mut |inner_node| match inner_node {
                Node::Call(Call { id }) if id == &self.id => finished = true,
                _ => {}
            });

            if finished {
                return true;
            }
        }
        false
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

    /// Recursively yields all variables that this condition reads from
    pub fn accessed_variables<F>(&self, func: &mut F)
    where
        F: FnMut(&ItemId),
    {
        match self {
            Condition::Compare { lhs, rhs, .. } => {
                if let Some(id) = lhs.id() {
                    func(id)
                }
                if let Some(id) = rhs.id() {
                    func(id)
                }
            }
            Condition::And(conditions) => {
                for condition in conditions {
                    condition.accessed_variables(func);
                }
            }
            Condition::Or(conditions) => {
                for condition in conditions {
                    condition.accessed_variables(func);
                }
            }
        }
    }
}

impl Node {
    /// Iterates over this node and all other nodes that
    /// this node contains
    pub fn iter<F>(&self, func: &mut F)
    where
        F: FnMut(&Node),
    {
        func(self);
        match self {
            Node::Branch(branch) => {
                branch.pos_branch.iter(func);
                if let Some(neg_branch) = &branch.neg_branch {
                    neg_branch.iter(func);
                }
            }
            Node::FastStoreFromResult(FastStoreFromResult { command, .. }) => {
                func(command.as_ref())
            }
            _ => {}
        }
    }

    /// Returns whether this command has no side effect
    pub fn is_effect_free(&self) -> bool {
        match self {
            Node::BinaryOperation(_) => false,
            Node::Branch(branch) => {
                branch.pos_branch.is_effect_free()
                    && match &branch.neg_branch {
                        None => true,
                        Some(branch) => branch.is_effect_free(),
                    }
            }
            // This could theoretically be true if we can check the called function
            Node::Call(_) => false,
            Node::Condition(_) => true,
            // Could sometimes be effect free though
            Node::Execute(_) => false,
            Node::FastStore(_) => false,
            Node::FastStoreFromResult(_) => false,
            Node::Function(_) => false,
            Node::Write(_) => false,
        }
    }

    /// If this node writes to a value, returns Some
    pub fn get_write(&self) -> Option<&ItemId> {
        match self {
            Node::BinaryOperation(op) => Some(&op.id),
            Node::FastStore(store) => Some(&store.id),
            Node::FastStoreFromResult(store) => Some(&store.id),
            _ => None,
        }
    }

    /// Modifies this so, so that it writes to `target_id`
    pub fn set_write_to(&mut self, target_id: ItemId) {
        match self {
            Node::BinaryOperation(op) => {
                op.id = target_id;
            }
            Node::FastStore(store) => {
                store.id = target_id;
            }
            Node::FastStoreFromResult(store) => {
                store.id = target_id;
            }
            _ => panic!("This node does not write any value"),
        }
    }
}
