use std::fmt;

/// Identifies a single callable block of code
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct BlockId(pub(super) u32);

impl BlockId {
    /// Creates a dummy BlockId,
    /// For testing only
    pub fn dummy(value: u32) -> Self {
        BlockId(value)
    }
}

/// Identifies a specific scoreboard
///
/// Debris has one main scoreboard and an arbitrary amount of custom scoreboards
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum Scoreboard {
    /// The Main scoreboard, where all operations are per default
    Main,
    /// Custom scoreboards, each with a unique identifier
    Custom(usize),
    /// Special scoreboards used for internal tracking
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

impl std::fmt::Display for ScoreboardValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScoreboardValue::Static(static_value) => write!(f, "{}", static_value),
            ScoreboardValue::Scoreboard(_, id) => write!(f, "{}", id),
        }
    }
}

/// Any operation that can be executed on a scoreboard-
/// This excludes copy, because scoreboard operations are assumed
/// to be trinary (a = b OP c), but copy is only a binary
/// Operation.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ScoreboardOperation {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Min,
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

impl ScoreboardOperation {
    pub fn evaluate(&self, lhs: i32, rhs: i32) -> i32 {
        use ScoreboardOperation::*;
        match self {
            Min => i32::min(lhs, rhs),
            Max => i32::max(lhs, rhs),
            Plus => lhs.wrapping_add(rhs),
            Minus => lhs.wrapping_sub(rhs),
            Times => lhs.wrapping_mul(rhs),
            Divide => {
                // Minecraft does not modify the lhs value on division by zero
                if rhs == 0 {
                    lhs
                } else {
                    lhs.wrapping_div(rhs)
                }
            }
            Modulo => {
                if rhs == 0 {
                    // If b is 0 minecraft throws an exception and does nothing to a
                    lhs
                } else {
                    // Rusts remainder implementation should be the same as javas
                    lhs.checked_rem(rhs).unwrap_or(0)
                }
            }
        }
    }
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

    pub fn evaluate(&self, lhs: i32, rhs: i32) -> bool {
        use ScoreboardComparison::*;
        match self {
            Equal => lhs == rhs,
            NotEqual => lhs != rhs,
            Greater => lhs > rhs,
            GreaterOrEqual => lhs >= rhs,
            Less => lhs < rhs,
            LessOrEqual => lhs <= rhs,
        }
    }
}

impl fmt::Display for ScoreboardComparison {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ScoreboardComparison::Equal => "==",
            ScoreboardComparison::NotEqual => "!=",
            ScoreboardComparison::Greater => ">",
            ScoreboardComparison::GreaterOrEqual => ">=",
            ScoreboardComparison::Less => "<",
            ScoreboardComparison::LessOrEqual => "<=",
        })
    }
}

/// A unique identifier for an item
#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct ItemId {
    pub id: u32,
    pub block: BlockId,
}

impl fmt::Display for ItemId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}.{}", self.block.0, self.id))
    }
}

impl fmt::Debug for ItemId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}
