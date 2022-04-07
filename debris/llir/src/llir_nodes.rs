//! Contains every node that can be produced in the llir step.
//!
//! Note that changing any node kind can lead to miscompilations if it isn't also updated
//! at the optimizers!

use std::ops::Index;
use std::{fmt, rc::Rc};

use itertools::Itertools;
use rustc_hash::FxHashSet;

use crate::{block_id::BlockId, item_id::ItemId, type_context::TypeContext};

use super::{
    json_format::{FormattedText, JsonFormatComponent},
    minecraft_utils::{Scoreboard, ScoreboardComparison, ScoreboardOperation, ScoreboardValue},
    ObjectRef,
};

/// A function node, contains other nodes
#[derive(Debug)]
pub struct Function {
    /// The id of this function
    pub id: BlockId,
    /// The nodes which this function contains
    pub(crate) nodes: Vec<Node>,
    pub(crate) return_value: ObjectRef,
}

/// Stores a 'fast' variable
///
/// Fast variables are scoreboard values.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FastStore {
    /// The scoreboard of the target var
    pub scoreboard: Scoreboard,
    /// The id of the target var
    pub id: ItemId,
    /// The value to store into the target var
    pub value: ScoreboardValue,
}

/// Stores a 'fast' variable from the result of another node
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FastStoreFromResult {
    /// The scoreboard of the target var
    pub scoreboard: Scoreboard,
    /// The id of the target var
    pub id: ItemId,
    /// The command to use
    pub command: Box<Node>,
}

/// Operates on two scoreboard values and stores the result into the target var
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Call {
    /// The id of that function
    pub id: BlockId,
}

/// Evaluates a condition and returns either true or false
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BranchKind {
    PosBranch,
    NegBranch,
    Both,
}

/// Branches based on a condition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Branch {
    /// The condition to test
    pub condition: Condition,
    /// The node to execute if that condition is true
    pub pos_branch: Box<Node>,
    /// The node to execute if that condition is false
    pub neg_branch: Box<Node>,
}

/// A component for a raw execute command
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExecuteRawComponent {
    /// Writes a string literal
    String(Rc<str>),
    /// References the value locations
    ScoreboardValue(ScoreboardValue),
    /// Embeds another node in the output
    Node(Node),
}

/// Executes a literal string
///
/// Any String that modifies a variable that belongs to debris
/// or modifies the program state in any other way
/// causes undefined behavior!
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecuteRaw(pub Vec<ExecuteRawComponent>);

/// Writes a formatted message
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WriteMessage {
    pub target: WriteTarget,
    pub message: FormattedText,
}

/// The buffer to write to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WriteTarget {
    Chat,
    Actionbar,
    Title,
    Subtitle,
}

impl fmt::Display for WriteTarget {
    // Debug is good enough for now
    #[allow(clippy::use_debug)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

/// Any node
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    FastStore(FastStore),
    FastStoreFromResult(FastStoreFromResult),
    BinaryOperation(BinaryOperation),
    Call(Call),
    Condition(Condition),
    Branch(Branch),
    Execute(ExecuteRaw),
    Write(WriteMessage),
    /// Does nothing
    Nop,
}

/// Denotes how a specific node accesses variables.
pub enum VariableAccess<'a> {
    /// Marks that a value is read by this node
    Read(&'a ScoreboardValue),
    /// Marks that a value is written to by this node.
    /// The second argument is the constant value that is written, if applicable.
    Write(&'a ItemId, Option<i32>),
    /// Marks that a value can be both read from and written to by this node.
    ReadWrite(&'a ScoreboardValue),
}

/// See [`VariableAccess`].
pub enum VariableAccessMut<'a> {
    Read(&'a mut ScoreboardValue),
    Write(&'a mut ItemId, Option<i32>),
    ReadWrite(&'a mut ScoreboardValue),
}

/// This awful macro contains code that otherwise would have to be copy-pasted
/// For the immutable and the mutable variable access visitor of [Node] and [Condition].
macro_rules! make_access_visitor {
    (node, $self:ident, $visitor:ident, $fn_name:ident, $VariableAccess:ident) => {
        match $self {
            Node::BinaryOperation(BinaryOperation {
                scoreboard: _,
                id,
                lhs,
                rhs,
                operation: _,
            }) => {
                $visitor($VariableAccess::Write(id, None));

                $visitor($VariableAccess::Read(lhs));
                $visitor($VariableAccess::Read(rhs));
            }
            Node::Branch(Branch {
                condition,
                pos_branch,
                neg_branch,
            }) => {
                pos_branch.$fn_name($visitor);
                neg_branch.$fn_name($visitor);
                condition.$fn_name($visitor);
            }
            Node::Call(Call { id: _ }) => {}
            Node::Condition(condition) => {
                condition.$fn_name($visitor);
            }
            Node::Execute(ExecuteRaw(components)) => {
                for component in components {
                    match component {
                        ExecuteRawComponent::ScoreboardValue(value) => {
                            $visitor($VariableAccess::ReadWrite(value));
                        }
                        ExecuteRawComponent::Node(node) => node.$fn_name($visitor),
                        ExecuteRawComponent::String(_) => {}
                    }
                }
            }
            Node::FastStore(FastStore {
                scoreboard: _,
                id,
                value,
            }) => {
                let static_val = if let ScoreboardValue::Static(value) = value {
                    Some(*value)
                } else {
                    None
                };
                $visitor($VariableAccess::Write(id, static_val));

                $visitor($VariableAccess::Read(value));
            }
            Node::FastStoreFromResult(FastStoreFromResult {
                scoreboard: _,
                id,
                command,
            }) => {
                $visitor($VariableAccess::Write(id, None));
                command.$fn_name($visitor);
            }
            Node::Nop => {}
            Node::Write(WriteMessage {
                target: _,
                message: FormattedText { components },
            }) => {
                for component in components {
                    match component {
                        JsonFormatComponent::Score(value) => {
                            $visitor($VariableAccess::Read(value));
                        }
                        JsonFormatComponent::RawText(_) | JsonFormatComponent::Function(_) => {}
                    }
                }
            }
        }
    };
    (condition, $self:ident, $visitor:ident, $fn_name:ident, $VariableAccess:ident) => {
        match $self {
            Condition::Compare {
                lhs,
                rhs,
                comparison: _,
            } => {
                $visitor($VariableAccess::Read(lhs));
                $visitor($VariableAccess::Read(rhs));
            }
            Condition::And(conditions) => {
                for condition in conditions {
                    condition.$fn_name($visitor);
                }
            }
            Condition::Or(conditions) => {
                for condition in conditions {
                    condition.$fn_name($visitor);
                }
            }
        }
    };
}

impl Function {
    pub fn nodes(&self) -> &[Node] {
        self.nodes.as_slice()
    }

    /// Checks if this function contains a node that calls the argument.
    /// Note that this is only a shallow check, no other functions will be
    /// visited.
    pub fn calls_function(&self, function_id: &BlockId) -> bool {
        for node in self.nodes() {
            let mut contains_call = false;
            node.scan(&mut |inner_node| match inner_node {
                Node::Call(Call { id }) if id == function_id => {
                    contains_call = true;
                }
                _ => {}
            });
            if contains_call {
                return true;
            }
        }
        false
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// This function should only be used for testing purposes
    #[doc(hidden)]
    pub fn _dummy(id: BlockId, nodes: Vec<Node>) -> Self {
        Function {
            id,
            nodes,
            return_value: TypeContext::default().null(),
        }
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
            Condition::And(conditions) => {
                // -(a AND b) = -a OR -b
                Condition::Or(conditions.iter().map(Condition::not).collect())
            }
            Condition::Or(conditions) => {
                // -(a OR b) = -a AND -b
                Condition::And(conditions.iter().map(Condition::not).collect())
            }
        }
    }

    /// Returns whether evaluating this condition has any side effects
    pub fn is_effect_free(&self) -> bool {
        match self {
            Condition::And(nested) | Condition::Or(nested) => {
                nested.iter().all(Condition::is_effect_free)
            }
            Condition::Compare { .. } => true,
        }
    }

    /// Checks whether this condition is "simple",
    /// which means that it is not expensive to use this condition multiple times
    /// instead of a boolean.
    ///
    /// Right now, only `Condition::Compare` is considered simple.
    pub fn is_simple(&self) -> bool {
        matches!(self, Condition::Compare { .. })
    }

    pub fn variable_reads(&self) -> FxHashSet<ItemId> {
        let mut reads = FxHashSet::default();
        self.variable_accesses(&mut |access| {
            if let VariableAccess::Read(var) | VariableAccess::ReadWrite(var) = access {
                if let ScoreboardValue::Scoreboard(_, item_id) = var {
                    reads.insert(*item_id);
                }
            }
        });
        reads
    }

    /// Recursively yields all variables that this condition reads from
    pub fn variable_accesses<F: FnMut(VariableAccess)>(&self, visitor: &mut F) {
        make_access_visitor!(condition, self, visitor, variable_accesses, VariableAccess);
    }

    pub fn variable_accesses_mut<F: FnMut(VariableAccessMut)>(&mut self, visitor: &mut F) {
        make_access_visitor!(
            condition,
            self,
            visitor,
            variable_accesses_mut,
            VariableAccessMut
        );
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Condition::Compare {
                lhs,
                rhs,
                comparison,
            } => write!(f, "{lhs} {} {rhs}", comparison.str_value()),
            Condition::And(parts) => {
                let nested = parts.iter().map(Condition::to_string).join(" and ");
                write!(f, "({nested})")
            }
            Condition::Or(parts) => {
                let nested = parts.iter().map(Condition::to_string).join(" or ");
                write!(f, "({nested})")
            }
        }
    }
}

impl Node {
    /// Iterates over this node and all other nodes that
    /// this node contains.
    /// Changing this function also requires changing [`Node::scan_mut`]
    pub fn scan<F>(&self, func: &mut F)
    where
        F: FnMut(&Node),
    {
        func(self);
        match self {
            Node::Branch(branch) => {
                branch.pos_branch.scan(func);
                branch.neg_branch.scan(func);
            }
            Node::FastStoreFromResult(FastStoreFromResult { command, .. }) => command.scan(func),
            Node::Execute(ExecuteRaw(components)) => {
                for component in components {
                    match component {
                        ExecuteRawComponent::Node(node) => {
                            node.scan(func);
                        }
                        ExecuteRawComponent::ScoreboardValue(_)
                        | ExecuteRawComponent::String(_) => {}
                    }
                }
            }
            _ => {}
        }
    }

    /// A copy of the above function, but with mutability enabled ...
    pub fn scan_mut<F>(&mut self, func: &mut F)
    where
        F: FnMut(&mut Node),
    {
        func(self);
        match self {
            Node::Branch(branch) => {
                branch.pos_branch.scan_mut(func);
                branch.neg_branch.scan_mut(func);
            }
            Node::FastStoreFromResult(FastStoreFromResult { command, .. }) => {
                command.scan_mut(func);
            }
            Node::Execute(ExecuteRaw(components)) => {
                for component in components {
                    match component {
                        ExecuteRawComponent::Node(node) => {
                            node.scan_mut(func);
                        }
                        ExecuteRawComponent::ScoreboardValue(_)
                        | ExecuteRawComponent::String(_) => {}
                    }
                }
            }
            _ => {}
        }
    }

    /// Returns whether this command has no side effect.
    /// sometimes its easier to just to restrict the parameter to a specific type...
    pub fn is_effect_free<'a, 'b: 'a, T>(&'a self, function_map: &'b T) -> bool
    where
        T: Index<&'a BlockId, Output = Function>,
    {
        match self {
            Node::Branch(branch) => {
                branch.condition.is_effect_free()
                    && branch.pos_branch.is_effect_free(function_map)
                    && branch.neg_branch.is_effect_free(function_map)
            }
            // ToDo: track already checked functions, so no infinite loop occurs
            Node::Call(Call { id }) => function_map[id]
                .nodes
                .iter()
                .all(|node| node.is_effect_free(function_map)),
            Node::Condition(condition) => condition.is_effect_free(),
            Node::BinaryOperation(_)
            | Node::Execute(_)
            | Node::FastStore(_)
            | Node::FastStoreFromResult(_)
            | Node::Write(_) => false,
            Node::Nop => true,
        }
    }

    /// Checks whether this node contains a call
    pub fn has_call(&self) -> bool {
        let mut has_call = false;
        self.scan(&mut |node| {
            if matches!(node, Node::Call(_)) {
                has_call = true;
            }
        });
        has_call
    }

    /// Accepts a callback function as argument and calls it with every variable
    /// accessed by this node.
    pub fn variable_accesses<F: FnMut(VariableAccess)>(&self, visitor: &mut F) {
        make_access_visitor!(node, self, visitor, variable_accesses, VariableAccess);
    }

    /// See `variable_accesses`.
    pub fn variable_accesses_mut<F: FnMut(VariableAccessMut)>(&mut self, visitor: &mut F) {
        make_access_visitor!(
            node,
            self,
            visitor,
            variable_accesses_mut,
            VariableAccessMut
        );
    }

    /// Whether this node could modify `item_id`.
    pub fn writes_to(&self, item_id: ItemId) -> bool {
        let mut found_write = false;
        self.variable_accesses(&mut |access| match access {
            VariableAccess::Write(id, _)
            | VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, id))
                if *id == item_id =>
            {
                found_write = true;
            }
            _ => {}
        });
        found_write
    }

    /// Whether this node has a read-dependency on `item_id`
    pub fn reads_from(&self, item_id: ItemId) -> bool {
        let mut has_read = false;
        self.variable_accesses(&mut |access| match access {
            VariableAccess::Read(ScoreboardValue::Scoreboard(_, id))
            | VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, id))
                if *id == item_id =>
            {
                has_read = true;
            }
            _ => {}
        });
        has_read
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.id.0;
        let content = self.nodes.iter().join("\n");
        write!(f, "Function {id}:\n{content}\n")
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::BinaryOperation(binop) => write!(
                f,
                "{} = {} {} {}",
                binop.id,
                binop.lhs,
                binop.operation.str_value(),
                binop.rhs
            ),
            Node::Branch(branch) => write!(
                f,
                "if ({}):\n\t{}\n\t{}",
                branch.condition, branch.pos_branch, branch.neg_branch
            ),
            Node::Call(call) => write!(f, "call {}", call.id.0),
            Node::Condition(condition) => condition.fmt(f),
            Node::Execute(ExecuteRaw(components)) => {
                for component in components {
                    match component {
                        ExecuteRawComponent::ScoreboardValue(
                            value @ ScoreboardValue::Scoreboard { .. },
                        ) => write!(f, "${value}")?,
                        ExecuteRawComponent::ScoreboardValue(ScoreboardValue::Static(value)) => {
                            write!(f, "{value}")?;
                        }
                        ExecuteRawComponent::Node(node) => write!(f, "${{{node}}}")?,
                        ExecuteRawComponent::String(string) => f.write_str(string)?,
                    }
                }
                Ok(())
            }
            Node::FastStore(FastStore {
                scoreboard: _,
                id,
                value,
            }) => write!(f, "{id} = {value}"),
            Node::FastStoreFromResult(FastStoreFromResult {
                scoreboard: _,
                id,
                command,
            }) => write!(f, "{id} = {command}"),
            Node::Write(WriteMessage { target, message }) => {
                write!(f, "write {target}: {message}")
            }
            Node::Nop => f.write_str("Nop"),
        }
    }
}
