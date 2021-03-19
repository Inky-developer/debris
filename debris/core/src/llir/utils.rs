use std::fmt;

use crate::mir::ContextId;

/// Identifies a single callable block of code
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct BlockId(pub(super) usize);

/// Identifies a specific scoreboard
///
/// Debris has one main scoreboard and an arbitrary amount of custom scoreboards
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum Scoreboard {
    /// The Main scoreboard, where all operations are per default
    Main,
    /// Custom scoreboards, each with a unique identifier
    Custom(usize),
    /// Special scorbeoards used for internal tracking
    Internal(usize),
}

/// A Value that can be stored on a scoreboard
///
/// Either a real scoreboard value or a static number
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ScoreboardValue {
    Static(i32),
    Scoreboard(Scoreboard, ItemId),
}

impl ScoreboardValue {
    pub fn id(&self) -> Option<&ItemId> {
        match self {
            ScoreboardValue::Static(_) => None,
            ScoreboardValue::Scoreboard(_, id) => Some(id),
        }
    }
}

/// Any operation that can be executed on a scoreboard
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ScoreboardOperation {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    /// Copy the second value onto the first value
    Copy,
    /// Set the first value to min(first, last)
    Min,
    /// Set the second value to max(first, last)
    Max,
}

/// Any comparison that can be executed on two scoreboard values
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ScoreboardComparison {
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
}

impl ScoreboardComparison {
    /// Flips the comparison (converts OP such that `a OP b == b OP.flip_sides() a`)
    pub fn flip_sides(&self) -> ScoreboardComparison {
        use ScoreboardComparison::*;
        match self {
            Equal => Equal,
            NotEqual => NotEqual,
            Greater => Less,
            GreaterOrEqual => LessOrEqual,
            Less => Greater,
            LessOrEqual => GreaterOrEqual,
        }
    }

    /// Inverts the comparison, such that it is exactly and only then true
    /// when the original comparison is false
    pub fn invert(&self) -> ScoreboardComparison {
        use ScoreboardComparison::*;
        match self {
            Equal => NotEqual,
            NotEqual => Equal,
            Greater => LessOrEqual,
            GreaterOrEqual => Less,
            Less => GreaterOrEqual,
            LessOrEqual => Greater,
        }
    }
}

/// A unique identifier for an item
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub struct ItemId {
    pub id: usize,
    pub context: ContextId,
}

impl fmt::Display for ItemId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}.{}", self.context, self.id))
    }
}
