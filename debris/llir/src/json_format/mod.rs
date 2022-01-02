//! (Current incomplete) interface for minecraft's text commands.

use std::{fmt, rc::Rc};

use itertools::Itertools;

use super::utils::ScoreboardValue;

/// Debris syntax:
/// `normal text $variable other text $other_variable, end after non-ident char \& 4escaped ampersand`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormattedText {
    pub components: Vec<JsonFormatComponent>,
}

impl fmt::Display for FormattedText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"")?;
        f.write_str(
            &self
                .components
                .iter()
                .map(JsonFormatComponent::to_string)
                .join(""),
        )?;
        write!(f, "\"")
    }
}

impl From<Vec<JsonFormatComponent>> for FormattedText {
    fn from(value: Vec<JsonFormatComponent>) -> Self {
        FormattedText { components: value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsonFormatComponent {
    RawText(Rc<str>),
    Score(ScoreboardValue),
}

impl fmt::Display for JsonFormatComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsonFormatComponent::RawText(text) => f.write_str(text),
            JsonFormatComponent::Score(value) => match value {
                ScoreboardValue::Static(int) => write!(f, "{}", int),
                ScoreboardValue::Scoreboard(scoreboard, value) => {
                    write!(f, "{{{{{}@{}}}}}", scoreboard, value)
                }
            },
        }
    }
}
