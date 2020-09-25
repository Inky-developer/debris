use std::{fmt, fmt::Display, rc::Rc};

use fmt::Formatter;

/// Enumerates all minecraft commands that are used by any backend
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
}

pub enum ObjectiveCriterion {
    Dummy,
    #[allow(dead_code)]
    Other(String),
}

pub struct FunctionIdent {
    pub namespace: Rc<String>,
    pub path: String,
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
