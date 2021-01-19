use super::utils::{ItemId, Scoreboard};

#[derive(Debug)]
pub struct FormattedText {
    pub components: Vec<JsonFormatComponent>,
}

#[derive(Debug)]
pub enum JsonFormatComponent {
    RawText(String),
    Score(Scoreboard, ItemId),
}
