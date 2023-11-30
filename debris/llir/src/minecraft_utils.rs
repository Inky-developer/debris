use std::fmt;

use crate::item_id::ItemId;

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

impl fmt::Display for Scoreboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Scoreboard::Main => write!(f, "Main"),
            Scoreboard::Custom(id) => write!(f, "{id}"),
            Scoreboard::Internal(id) => write!(f, "internal({id})"),
        }
    }
}

/// A Value that can be stored on a scoreboard
///
/// Either a real scoreboard value or a static number
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ScoreboardValue {
    Static(i32),
    Scoreboard(ItemId),
}

impl ScoreboardValue {
    pub fn id(&self) -> Option<&ItemId> {
        match self {
            ScoreboardValue::Static(_) => None,
            ScoreboardValue::Scoreboard(id) => Some(id),
        }
    }
}

impl fmt::Display for ScoreboardValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScoreboardValue::Static(static_value) => write!(f, "{static_value}"),
            ScoreboardValue::Scoreboard(id) => write!(f, "{id}"),
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

impl ScoreboardOperation {
    pub fn str_value(&self) -> &'static str {
        use ScoreboardOperation::*;

        match self {
            Plus => "+",
            Minus => "-",
            Times => "*",
            Divide => "/",
            Modulo => "%",
            Min => "min",
            Max => "max",
        }
    }
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
                    // Minecraft rounds towards -infinity, while rust rounds towards 0
                    let nat_div = lhs.wrapping_div(rhs);
                    let Some(prod) = rhs.checked_mul(nat_div) else {
                        return nat_div;
                    };
                    if lhs != prod && (lhs >= 0) != (rhs >= 0) {
                        nat_div - 1
                    } else {
                        nat_div
                    }
                }
            }
            Modulo => {
                // If b is 0 minecraft throws an exception and does nothing to a
                if rhs == 0 {
                    lhs
                } else {
                    // Minecraft rounds towards -infinity, while rust rounds towards 0
                    lhs.checked_rem(rhs).unwrap_or(0).abs()
                }
            }
        }
    }
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

    pub fn str_value(&self) -> &'static str {
        match self {
            ScoreboardComparison::Equal => "==",
            ScoreboardComparison::NotEqual => "!=",
            ScoreboardComparison::Greater => ">",
            ScoreboardComparison::GreaterOrEqual => ">=",
            ScoreboardComparison::Less => "<",
            ScoreboardComparison::LessOrEqual => "<=",
        }
    }
}
