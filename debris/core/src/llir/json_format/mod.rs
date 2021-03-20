//! (Current incomplete) interface for minecrafts text commands.

use std::fmt;

use itertools::Itertools;

use super::utils::{ItemId, Scoreboard};

#[derive(Debug)]
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

#[derive(Debug)]
pub enum JsonFormatComponent {
    RawText(String),
    Score(Scoreboard, ItemId),
}

impl fmt::Display for JsonFormatComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsonFormatComponent::RawText(text) => f.write_str(text),
            JsonFormatComponent::Score(scoreboard, id) => {
                write!(f, "Score({} {:?})", id, scoreboard)
            }
        }
    }
}
