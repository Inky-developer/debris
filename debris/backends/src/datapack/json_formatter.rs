use debris_core::llir::{
    json_format::{FormattedText, JsonFormatComponent},
    utils::ScoreboardValue,
};

use super::scoreboard_context::ScoreboardContext;

// ToDo: Proper implementation
pub(super) fn format_json(message: &FormattedText, scoreboards: &mut ScoreboardContext) -> String {
    let mut buf = "[".to_string();

    for component in &message.components {
        match component {
            JsonFormatComponent::RawText(text) => {
                buf.push_str(&format!(r#"{{"text":"{}"}}"#, text.escape_default()))
            }
            JsonFormatComponent::Score(score) => match score {
                ScoreboardValue::Scoreboard(scoreboard, id) => buf.push_str(&format!(
                    r#"{{"score":{{"name":"{}","objective":"{}"}}}}"#,
                    scoreboards.get_scoreboard_player(*id),
                    scoreboards.get_scoreboard(*scoreboard),
                )),
                ScoreboardValue::Static(value) => {
                    buf.push_str(&format!(r#"{{"text":"{}"}}"#, value))
                }
            },
        }
        buf.push(',');
    }

    buf.pop();
    buf.push(']');

    buf
}

#[cfg(test)]
mod tests {
    use debris_core::{BuildMode, llir::{json_format::{FormattedText, JsonFormatComponent}, utils::{ItemId, Scoreboard, ScoreboardValue}}, mir::ContextId};

    use crate::datapack::scoreboard_context::ScoreboardContext;

    use super::format_json;

    #[test]
    fn test_formatter() {
        let mut scoreboard_context = ScoreboardContext::new("temp".to_string(), BuildMode::Debug);

        assert_eq!(
            format_json(
                &FormattedText {
                    components: vec![JsonFormatComponent::RawText("Hello World!".to_string())]
                },
                &mut scoreboard_context
            ),
            r#"[{"text":"Hello World!"}]"#
        );
    }

    #[test]
    fn test_formatter_multiple_args() {
        let mut scoreboard_context = ScoreboardContext::new("temp".to_string(), BuildMode::Debug);

        let context_id = ContextId::dummy(0);

        assert_eq!(
            format_json(
                &FormattedText {
                    components: vec![
                        JsonFormatComponent::RawText("Hello World!".to_string()),
                        JsonFormatComponent::RawText(" The score is: ".to_string()),
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
            r#"[{"text":"Hello World!"},{"text":" The score is: "},{"score":{"name":"var_0","objective":"temp"}}]"#
        );
    }
}
