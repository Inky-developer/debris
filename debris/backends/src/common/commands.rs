use std::{fmt, fmt::Display, rc::Rc};

use debris_llir::{
    llir_nodes::WriteTarget,
    utils::{ScoreboardComparison, ScoreboardOperation},
};
use fmt::Formatter;

/// Enumerates all minecraft commands that are used by any backend
#[derive(Debug, Clone)]
pub enum MinecraftCommand {
    /// Sets the scoreboard value to a specific integer
    ScoreboardSet {
        player: ScoreboardPlayer,
        value: i32,
    },
    /// Sets the scoreboard value to another scoreboard value
    ScoreboardSetEqual {
        player1: ScoreboardPlayer,
        player2: ScoreboardPlayer,
    },
    /// Sets the scoreboard value equal to the result of the other command
    ScoreboardSetFromResult {
        player: ScoreboardPlayer,
        command: Box<MinecraftCommand>,
    },
    ScoreboardOperation {
        player1: ScoreboardPlayer,
        player2: ScoreboardPlayer,
        operation: ScoreboardOperation,
    },
    /// Quick operation which adds or removes a static value
    ScoreboardOperationAdd {
        player: ScoreboardPlayer,
        value: i32,
    },
    /// Any used execute command
    Execute {
        parts: Vec<ExecuteComponent>,
        and_then: Option<Box<MinecraftCommand>>,
    },
    /// Calls another function
    Function {
        function: Rc<FunctionIdent>,
    },
    ScoreboardAdd {
        name: Rc<str>,
        criterion: ObjectiveCriterion,
        json_name: Option<String>,
    },
    ScoreboardRemove {
        name: Rc<str>,
    },
    RawCommand {
        command: Rc<str>,
    },
    JsonMessage {
        target: WriteTarget,
        message: String,
    },
}

/// A component in an execute command
#[derive(Debug, Clone)]
pub enum ExecuteComponent {
    /// Tests for a relation between to scores
    IfScoreRelation {
        player1: ScoreboardPlayer,
        player2: ScoreboardPlayer,
        comparison: ScoreboardComparison,
    },
    /// Tests for a relation between a score and a static value
    IfScoreRange {
        player: ScoreboardPlayer,
        range: MinecraftRange,
    },
}

impl ExecuteComponent {
    /// Returns whether this component is a condition
    /// (Right now always true)
    pub fn is_condition(&self) -> bool {
        match self {
            ExecuteComponent::IfScoreRange { .. } | ExecuteComponent::IfScoreRelation { .. } => {
                true
            }
        }
    }
}

/// Any valid minecraft range
///
/// A minecraft range is inclusive on both ends
#[derive(Debug, Copy, Clone)]
pub enum MinecraftRange {
    /// A full range, eg. 1..99
    Range { from: i32, to: i32 },
    /// A range with a lower bound, eg. 0..
    Minimum(i32),
    /// A range with an upper bound, eg. ..50
    Maximum(i32),
    /// A range that only contains one value
    Equal(i32),
    /// A range that contains every value except one
    NotEqual(i32),
}

/// A combination of scoreboard and player
#[derive(Debug, Clone)]
pub struct ScoreboardPlayer {
    pub player: Rc<str>,
    pub scoreboard: Rc<str>,
}

/// Any objective criterion
///
/// used in the command `scoreboard objectives add foo <criterion>`
#[derive(Debug, Clone)]
pub enum ObjectiveCriterion {
    /// Mostly used by debris
    Dummy,
    // /// potentially generate an enum of all possibilities from minecraft data
    // Other(String),
}

/// A unique minecraft function identifier
#[derive(Debug)]
pub struct FunctionIdent {
    /// The namespace of this function, for example `debris`
    pub namespace: Rc<str>,
    /// The path of this function, for example `foo/bar/baz`
    pub path: String,
    /// Whether this function is a collection, marked by a `#`
    pub is_collection: bool,
}

impl MinecraftRange {
    pub fn from_operator(value: i32, operator: ScoreboardComparison) -> Self {
        match operator {
            ScoreboardComparison::Equal => MinecraftRange::Equal(value),
            ScoreboardComparison::NotEqual => MinecraftRange::NotEqual(value),
            ScoreboardComparison::Greater => {
                MinecraftRange::Minimum(value.checked_add(1).expect("Invalid value"))
            }
            ScoreboardComparison::GreaterOrEqual => MinecraftRange::Minimum(value),
            ScoreboardComparison::Less => {
                MinecraftRange::Maximum(value.checked_sub(1).expect("Invalid value"))
            }
            ScoreboardComparison::LessOrEqual => MinecraftRange::Maximum(value),
        }
    }
}

impl Display for ObjectiveCriterion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ObjectiveCriterion::Dummy => "dummy",
        })
    }
}

impl Display for FunctionIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{}{}:{}",
            if self.is_collection { "#" } else { "" },
            self.namespace,
            self.path
        ))
    }
}
