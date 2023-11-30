use std::fmt::Write;

use debris_llir::minecraft_utils::Scoreboard;
use debris_llir::{
    json_format::{FormattedText, JsonFormatComponent},
    minecraft_utils::ScoreboardValue,
};

use crate::common::{string_escape::escape_minecraft, ScoreboardPlayer};

use super::{function_context::FunctionContext, scoreboard_context::ScoreboardContext};

pub(super) fn format_json(
    message: &FormattedText,
    scoreboards: &mut ScoreboardContext,
    functions: &mut FunctionContext,
) -> String {
    let mut buf = JsonTextWriter::default();

    for component in &message.components {
        buf.write(component, scoreboards, functions);
    }

    buf.into_string()
}

#[derive(Debug)]
pub(super) struct JsonTextWriter {
    buf: String,
    pending: String,
}

impl JsonTextWriter {
    pub fn write(
        &mut self,
        component: &JsonFormatComponent,
        scoreboards: &mut ScoreboardContext,
        functions: &mut FunctionContext,
    ) {
        match component {
            JsonFormatComponent::RawText(text) => self.write_str(text),
            JsonFormatComponent::Score(ScoreboardValue::Static(static_value)) => {
                self.write_str(&static_value.to_string());
            }
            JsonFormatComponent::Score(ScoreboardValue::Scoreboard(id)) => {
                let player = ScoreboardPlayer {
                    player: scoreboards.get_scoreboard_player(*id),
                    scoreboard: scoreboards.get_scoreboard(Scoreboard::Main),
                };
                self.write_score(&player);
            }
            JsonFormatComponent::Function(block_id) => {
                let id = functions.reserve_block(*block_id);
                let ident = functions.get_function_ident(id);
                self.write_str("function ");
                self.write_str(&ident.to_string());
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
            let pending = self.pending.as_str();
            self.buf
                .write_fmt(format_args!(r#"{{"text":"{pending}"}},"#))
                .unwrap();
            self.pending.clear();
        }
    }

    fn write_str(&mut self, value: &str) {
        self.pending.extend(escape_minecraft(value));
    }

    fn write_score(&mut self, ScoreboardPlayer { player, scoreboard }: &ScoreboardPlayer) {
        self.flush_pending();
        self.buf
            .write_fmt(format_args!(
                r#"{{"score":{{"name":"{player}","objective":"{scoreboard}"}}}},"#
            ))
            .unwrap();
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
    use debris_common::BuildMode;
    use debris_llir::{
        item_id::ItemId,
        json_format::{FormattedText, JsonFormatComponent},
        minecraft_utils::ScoreboardValue,
    };

    use crate::datapack::{
        function_context::FunctionContext, scoreboard_context::ScoreboardContext,
    };

    use super::format_json;

    #[test]
    fn test_formatter() {
        let mut scoreboard_context = ScoreboardContext::new("temp".to_string(), BuildMode::Debug);
        let mut function_context = FunctionContext::new("debris".into());

        assert_eq!(
            format_json(
                &FormattedText {
                    components: vec![JsonFormatComponent::RawText("Hello World!".into())]
                },
                &mut scoreboard_context,
                &mut function_context,
            ),
            r#"[{"text":"Hello World!"}]"#
        );
    }

    #[test]
    fn test_newlines() {
        let mut scoreboard_context = ScoreboardContext::new("temp".to_string(), BuildMode::Debug);
        let mut function_context = FunctionContext::new("debris".into());

        assert_eq!(
            format_json(
                &FormattedText {
                    components: vec![JsonFormatComponent::RawText(
                        "This\nhas\nmany\nlines".into()
                    )]
                },
                &mut scoreboard_context,
                &mut function_context,
            ),
            r#"[{"text":"This\nhas\nmany\nlines"}]"#
        );
    }

    #[test]
    fn test_formatter_empty() {
        let mut scoreboard_context = ScoreboardContext::new("temp".to_string(), BuildMode::Debug);
        let mut function_context = FunctionContext::new("debris".into());

        assert_eq!(
            format_json(
                &FormattedText { components: vec![] },
                &mut scoreboard_context,
                &mut function_context,
            ),
            r"[]"
        );
    }

    #[test]
    fn test_formatter_multiple_args() {
        let mut scoreboard_context = ScoreboardContext::new("temp".to_string(), BuildMode::Debug);
        let mut function_context = FunctionContext::new("debris".into());

        assert_eq!(
            format_json(
                &FormattedText {
                    components: vec![
                        JsonFormatComponent::RawText("Hello World!".into()),
                        JsonFormatComponent::RawText(" The score is: ".into()),
                        JsonFormatComponent::Score(ScoreboardValue::Scoreboard(ItemId { id: 0 })),
                    ]
                },
                &mut scoreboard_context,
                &mut function_context,
            ),
            r#"[{"text":"Hello World! The score is: "},{"score":{"name":"var_0","objective":"temp"}}]"#
        );
    }
}
