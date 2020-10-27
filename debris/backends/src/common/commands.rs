use std::{fmt, fmt::Display, rc::Rc};

use debris_core::llir::utils::ScoreboardOperation;
use fmt::Formatter;

/// Enumerates all minecraft commands that are used by any backend
#[derive(Debug)]
pub enum MinecraftCommand {
    /// Sets the scoreboard value to a specific integer
    ScoreboardSet {
        player: Rc<String>,
        scoreboard: Rc<String>,
        value: i32,
    },
    /// Sets the scoreboard value to another scorboard value
    ScoreboardSetEqual {
        player1: Rc<String>,
        scoreboard1: Rc<String>,
        player2: Rc<String>,
        scoreboard2: Rc<String>,
    },
    /// Sets the scoreboard value equal to the result of the other command
    ScoreboardSetFromResult {
        player: Rc<String>,
        scoreboard: Rc<String>,
        command: Box<MinecraftCommand>,
    },
    ScoreboardOperation {
        player1: Rc<String>,
        scoreboard1: Rc<String>,
        player2: Rc<String>,
        scoreboard2: Rc<String>,
        operation: ScoreboardOperation,
    },
    /// Quick operation which adds or removes a static value
    ScoreboardOperationAdd {
        player: Rc<String>,
        scoreboard: Rc<String>,
        value: i32,
    },
    /// Calls another function
    Function {
        function: Rc<FunctionIdent>,
    },
    ScoreboardAdd {
        name: Rc<String>,
        criterion: ObjectiveCriterion,
        json_name: Option<String>,
    },
    ScoreboardRemove {
        name: Rc<String>,
    },
    RawCommand {
        command: Rc<String>,
    },
}

/// Any objective criterion
///
/// used in the command `scoreboard objectives add foo <criterion>`
#[derive(Debug)]
pub enum ObjectiveCriterion {
    /// Mostly used by debris
    Dummy,
    /// potentially generate an enum of all possibilities from minecraft data
    #[allow(dead_code)]
    Other(String),
}

/// A unique minecraft function identifier
#[derive(Debug)]
pub struct FunctionIdent {
    /// The namespace of this function, for example `debris`
    pub namespace: Rc<String>,
    /// The path of this function, for example `foo/bar/baz`
    pub path: String,
    /// Whether this function is a collection, marked by a `#`
    pub is_collection: bool,
}

impl Display for ObjectiveCriterion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ObjectiveCriterion::Dummy => "dummy",
            ObjectiveCriterion::Other(string) => &string,
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
