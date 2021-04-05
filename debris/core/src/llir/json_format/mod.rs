//! (Current incomplete) interface for minecrafts text commands.

use std::fmt;

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
        f.write_str(
            &self
                .components
                .iter()
                .map(|component| component.to_string())
                .join(", "),
        )
    }
}

impl From<String> for FormattedText {
    fn from(value: String) -> Self {
        FormattedText {
            components: vec![JsonFormatComponent::RawText(value)],
        }
    }
}

impl From<Vec<JsonFormatComponent>> for FormattedText {
    fn from(value: Vec<JsonFormatComponent>) -> Self {
        FormattedText { components: value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsonFormatComponent {
    RawText(String),
    Score(ScoreboardValue),
}

impl fmt::Display for JsonFormatComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsonFormatComponent::RawText(text) => f.write_str(text),
            JsonFormatComponent::Score(value) => {
                write!(f, "Score({:?})", value)
            }
        }
    }
}
