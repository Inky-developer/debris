/// Identifies a specific scoreboard
///
/// Debris has one main scoreboard and an arbitrary amount of custom scoreboards
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum Scoreboard {
    /// The Main scoreboard, where all operations are per default
    Main,
    /// Custom scoreboards, each with a unique identifier
    Custom(u64),
}

/// A Value that can be stored on a scoreboard
///
/// Either a real scoreboard value or a static number
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ScoreboardValue {
    Static(i32),
    Scoreboard(Scoreboard, ItemId),
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
    /// Flips the comparison
    pub fn flip(&self) -> ScoreboardComparison {
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
}

/// A unique identifier for a scoreboard item
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub struct ItemId {
    pub id: u64,
    pub context_id: u64,
}
