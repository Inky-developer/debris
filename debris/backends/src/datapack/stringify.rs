use std::error::Error;

use debris_core::{llir::utils::ScoreboardOperation, Config};
use lazy_static::lazy_static;
use liquid::Parser;

use crate::common::MinecraftCommand;

/// Converts a `MinecraftCommand` to a String
pub(crate) fn stringify_command(command: &MinecraftCommand) -> String {
    match command {
        MinecraftCommand::ScoreboardSet {
            player,
            scoreboard,
            value,
        } => format!("scoreboard players set {} {} {}", player, scoreboard, value),
        MinecraftCommand::ScoreboardSetEqual {
            player1,
            scoreboard1,
            player2,
            scoreboard2,
        } => format!(
            "scoreboard players operation {} {} = {} {}",
            player1, scoreboard1, player2, scoreboard2
        ),
        MinecraftCommand::ScoreboardSetFromResult {
            player,
            scoreboard,
            command,
        } => format!(
            "execute store result score {} {} run {}",
            player,
            scoreboard,
            stringify_command(command)
        ),
        MinecraftCommand::ScoreboardOperation {
            player1,
            scoreboard1,
            player2,
            scoreboard2,
            operation,
        } => format!(
            "scoreboard players operation {} {} {} {} {}",
            player1,
            scoreboard1,
            stringify_scoreboard_operator(operation),
            player2,
            scoreboard2
        ),
        MinecraftCommand::Function { function } => format!("function {}", function),
        MinecraftCommand::ScoreboardAdd {
            name,
            criterion,
            json_name,
        } => {
            if let Some(json) = json_name {
                format!("scoreboard objectives add {} {} {}", name, criterion, json)
            } else {
                format!("scoreboard objectives add {} {}", name, criterion)
            }
        }
        MinecraftCommand::ScoreboardRemove { name } => {
            format!("scoreboard objectives remove {}", name)
        }
        MinecraftCommand::RawCommand { command } => format!("{}", command),
    }
}

/// Converts a Scoreboard operator into a string
fn stringify_scoreboard_operator(op: &ScoreboardOperation) -> &'static str {
    match op {
        ScoreboardOperation::Plus => "+=",
        ScoreboardOperation::Minus => "-=",
        ScoreboardOperation::Times => "*=",
        ScoreboardOperation::Divide => "/=",
        ScoreboardOperation::Modulo => "%=",
        ScoreboardOperation::Copy => "=",
        ScoreboardOperation::Max => ">",
        ScoreboardOperation::Min => "<",
    }
}

lazy_static! {
    static ref PARSER: Parser = liquid::ParserBuilder::with_stdlib().build().unwrap();
}

pub(crate) fn stringify_template(
    config: &Config,
    template: &'static str,
) -> Result<String, Box<dyn Error>> {
    let template = PARSER.parse(template)?;
    let globals = liquid::object!({
        "project": &config.project_name,
        "project_description": &config.project_description,
        "default_scoreboard": &config.default_scoreboard_name,
        "debug": config.build_mode.is_debug(),
        "release": config.build_mode.is_release()
    });

    Ok(template.render(&globals)?)
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use debris_core::{llir::utils::ScoreboardOperation, BuildMode, Config};

    use crate::common::{FunctionIdent, MinecraftCommand, ObjectiveCriterion};

    use super::{stringify_command, stringify_template};

    #[test]
    fn test_template_engine() {
        let config = Config {
            build_mode: BuildMode::Debug,
            default_scoreboard_name: "debris".to_string(),
            project_name: "debris_project".to_string(),
            ..Default::default()
        };

        let template = r#"{version: 6, name: "{{project}} by debris"}"#;
        assert_eq!(
            stringify_template(&config, template).expect("Failed to render template"),
            r#"{version: 6, name: "debris_project by debris"}"#
        )
    }

    #[test]
    fn test_scoreboard_set() {
        let command = MinecraftCommand::ScoreboardSet {
            player: Rc::new("@s".to_string()),
            scoreboard: Rc::new("debris".to_string()),
            value: 100,
        };

        assert_eq!(
            stringify_command(&command),
            "scoreboard players set @s debris 100"
        )
    }

    #[test]
    fn test_scoreboard_set_equal() {
        let command = MinecraftCommand::ScoreboardSetEqual {
            player1: Rc::new("@s".to_string()),
            scoreboard1: Rc::new("debris".to_string()),
            player2: Rc::new("foo".to_string()),
            scoreboard2: Rc::new("debris.0".to_string()),
        };

        assert_eq!(
            stringify_command(&command),
            "scoreboard players operation @s debris = foo debris.0"
        )
    }

    #[test]
    fn test_scoreboard_set_from_result() {
        let command1 = MinecraftCommand::ScoreboardSetEqual {
            player1: Rc::new("@s".to_string()),
            scoreboard1: Rc::new("debris".to_string()),
            player2: Rc::new("foo".to_string()),
            scoreboard2: Rc::new("debris.0".to_string()),
        };

        let command = MinecraftCommand::ScoreboardSetFromResult {
            player: Rc::new("me".to_string()),
            scoreboard: Rc::new("debris".to_string()),
            command: Box::new(command1),
        };

        assert_eq!(
            stringify_command(&command),
            "execute store result score me debris run scoreboard players operation @s debris = foo debris.0"
        )
    }

    #[test]
    fn test_scoreboard_operation() {
        let command = MinecraftCommand::ScoreboardOperation {
            player1: Rc::new("value_1".to_string()),
            scoreboard1: Rc::new("main".to_string()),
            operation: ScoreboardOperation::Modulo,
            player2: Rc::new("value_2".to_string()),
            scoreboard2: Rc::new("main".to_string()),
        };

        assert_eq!(
            stringify_command(&command),
            "scoreboard players operation value_1 main %= value_2 main"
        )
    }

    #[test]
    fn test_function() {
        let command = MinecraftCommand::Function {
            function: Rc::new(FunctionIdent {
                is_collection: false,
                namespace: Rc::new("debris".to_string()),
                path: "foo/bar".to_string(),
            }),
        };

        assert_eq!(stringify_command(&command), "function debris:foo/bar")
    }

    #[test]
    fn test_scoreboard_add() {
        let command = MinecraftCommand::ScoreboardAdd {
            name: Rc::new("foo".to_string()),
            criterion: ObjectiveCriterion::Dummy,
            json_name: None,
        };

        assert_eq!(
            stringify_command(&command),
            "scoreboard objectives add foo dummy"
        )
    }

    #[test]
    fn test_scoreboard_add_json_name() {
        let command = MinecraftCommand::ScoreboardAdd {
            name: Rc::new("foo".to_string()),
            criterion: ObjectiveCriterion::Other("Health".to_string()),
            json_name: Some(r#"{"text":"foo", "color":"green"}"#.to_string()),
        };

        assert_eq!(
            stringify_command(&command),
            r#"scoreboard objectives add foo Health {"text":"foo", "color":"green"}"#
        )
    }

    #[test]
    fn test_scoreboard_remove() {
        let command = MinecraftCommand::ScoreboardRemove {
            name: Rc::new("foo".to_string()),
        };

        assert_eq!(
            stringify_command(&command),
            "scoreboard objectives remove foo"
        )
    }

    #[test]
    fn test_raw_command() {
        let command = MinecraftCommand::RawCommand {
            command: Rc::new("Hallo Welt".to_owned()),
        };

        assert_eq!(stringify_command(&command), "Hallo Welt")
    }
}
