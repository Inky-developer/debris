/// Identifies which scoreboard to use
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum Scoreboard {
    /// The Main scoreboard, where all operations are per default
    Main,
    /// Custom scoreboards, each with a unique identifier
    Custom(u64),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ScoreboardValue {
    Static(i32),
    Scoreboard(Scoreboard, ItemId),
}

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

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub struct ItemId {
    pub id: u64,
    pub context_id: u64,
}
