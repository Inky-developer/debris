use std::fmt::Write;

use debris_core::llir::{
    json_format::{FormattedText, JsonFormatComponent},
    utils::ScoreboardValue,
};

use crate::common::ScoreboardPlayer;

use super::scoreboard_context::ScoreboardContext;

pub(super) fn format_json(message: &FormattedText, scoreboards: &mut ScoreboardContext) -> String {
    let mut buf = JsonTextWriter::default();

    for component in &message.components {
        buf.write(component, scoreboards);
    }

    buf.into_string()
}

#[derive(Debug)]
pub struct JsonTextWriter {
    buf: String,
    pending: String,
}

impl JsonTextWriter {
    pub fn write(&mut self, component: &JsonFormatComponent, scoreboards: &mut ScoreboardContext) {
        match component {
            JsonFormatComponent::RawText(text) => self.write_str(&text),
            JsonFormatComponent::Score(ScoreboardValue::Static(static_value)) => {
                self.write_str(&static_value.to_string())
            }
            JsonFormatComponent::Score(ScoreboardValue::Scoreboard(scoreboard, id)) => {
                let player = ScoreboardPlayer {
                    player: scoreboards.get_scoreboard_player(*id),
                    scoreboard: scoreboards.get_scoreboard(*scoreboard),
                };
                self.write_score(player);
            }
        }
    }

    pub fn into_string(mut self) -> String {
        // Removes the trailing comma...
        self.flush_pending();
        if let Some(last) = self.buf.chars().last() {
            if last == ',' {
                self.buf.pop();
            }
        }
        self.buf.push(']');
        self.buf
    }

    fn flush_pending(&mut self) {
        if !self.pending.is_empty() {
            self.buf
                .write_fmt(format_args!(r#"{{"text":"{}"}},"#, self.pending))
                .unwrap();
            self.pending.clear();
        }
    }

    fn write_str(&mut self, value: &str) {
        self.pending.push_str(value);
    }

    fn write_score(&mut self, scoreboard_player: ScoreboardPlayer) {
        self.flush_pending();
        self.buf
            .write_fmt(format_args!(
                r#"{{"score":{{"name":"{}","objective":"{}"}}}},"#,
                scoreboard_player.player, scoreboard_player.scoreboard
            ))
            .unwrap()
    }
}

impl Default for JsonTextWriter {
    fn default() -> Self {
        let mut writer = JsonTextWriter {
            buf: Default::default(),
            pending: Default::default(),
        };
        writer.buf.push('[');
        writer
    }
}

#[cfg(test)]
mod tests {
    use debris_core::{
        llir::{
            json_format::{FormattedText, JsonFormatComponent},
            utils::{ItemId, Scoreboard, ScoreboardValue},
        },
        mir::ContextId,
        BuildMode,
    };

    use crate::datapack::scoreboard_context::ScoreboardContext;

    use super::format_json;

    #[test]
    fn test_formatter() {
        let mut scoreboard_context = ScoreboardContext::new("temp".to_string(), BuildMode::Debug);

        assert_eq!(
            format_json(
                &FormattedText {
                    components: vec![JsonFormatComponent::RawText("Hello World!".into())]
                },
                &mut scoreboard_context
            ),
            r#"[{"text":"Hello World!"}]"#
        );
    }

    #[test]
    fn test_formatter_empty() {
        let mut scoreboard_context = ScoreboardContext::new("temp".to_string(), BuildMode::Debug);

        assert_eq!(
            format_json(
                &FormattedText { components: vec![] },
                &mut scoreboard_context
            ),
            r#"[]"#
        )
    }

    #[test]
    fn test_formatter_multiple_args() {
        let mut scoreboard_context = ScoreboardContext::new("temp".to_string(), BuildMode::Debug);

        let context_id = ContextId::dummy(0);

        assert_eq!(
            format_json(
                &FormattedText {
                    components: vec![
                        JsonFormatComponent::RawText("Hello World!".into()),
                        JsonFormatComponent::RawText(" The score is: ".into()),
                        JsonFormatComponent::Score(ScoreboardValue::Scoreboard(
                            Scoreboard::Main,
                            ItemId {
                                context: context_id,
                                id: 0
                            }
                        ))
                    ]
                },
                &mut scoreboard_context
            ),
            r#"[{"text":"Hello World! The score is: "},{"score":{"name":"var_0","objective":"temp"}}]"#
        );
    }
}
