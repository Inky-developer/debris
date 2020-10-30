use std::error::Error;

use debris_core::{
    llir::utils::{ScoreboardComparison, ScoreboardOperation},
    Config,
};
use lazy_static::lazy_static;
use liquid::Parser;

use crate::common::{ExecuteComponent, MinecraftCommand, MinecraftRange};

/// Converts a minecraft command component into a command-compatible string
pub trait Stringify {
    type Output;
    fn stringify(&self) -> Self::Output
    where
        Self::Output: std::fmt::Display;
}

impl Stringify for MinecraftCommand {
    type Output = String;

    fn stringify(&self) -> String {
        match self {
            MinecraftCommand::ScoreboardSet { player, value } => format!(
                "scoreboard players set {} {} {}",
                player.player, player.scoreboard, value
            ),
            MinecraftCommand::ScoreboardSetEqual { player1, player2 } => format!(
                "scoreboard players operation {} {} = {} {}",
                player1.player, player1.scoreboard, player2.player, player2.scoreboard
            ),
            MinecraftCommand::ScoreboardSetFromResult { player, command } => format!(
                "execute store result score {} {} run {}",
                player.player,
                player.scoreboard,
                command.stringify()
            ),
            MinecraftCommand::ScoreboardOperation {
                player1,
                player2,
                operation,
            } => format!(
                "scoreboard players operation {} {} {} {} {}",
                player1.player,
                player1.scoreboard,
                operation.stringify(),
                player2.player,
                player2.scoreboard
            ),
            MinecraftCommand::ScoreboardOperationAdd { player, value } => {
                let (mode, value) = if *value < 0 {
                    ("remove", value * -1)
                } else {
                    ("add", *value)
                };

                format!(
                    "scoreboard players {} {} {} {}",
                    mode, player.player, player.scoreboard, value
                )
            }
            MinecraftCommand::Excute { parts, and_then } => {
                let execute_parts = parts
                    .iter()
                    .map(ExecuteComponent::stringify)
                    .collect::<Vec<_>>()
                    .join(" ");

                match and_then {
                    Some(command) => {
                        format!("execute {} run {}", execute_parts, command.stringify())
                    }
                    None => format!("execute {}", execute_parts),
                }
            }
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
}

impl Stringify for ScoreboardOperation {
    type Output = &'static str;

    fn stringify(&self) -> &'static str {
        match self {
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
}

impl Stringify for ScoreboardComparison {
    type Output = &'static str;

    fn stringify(&self) -> Self::Output {
        match self {
            ScoreboardComparison::Equal => "=",
            ScoreboardComparison::Less => "<",
            ScoreboardComparison::LessOrEqual => "<=",
            ScoreboardComparison::Greater => ">",
            ScoreboardComparison::GreaterOrEqual => ">=",
            ScoreboardComparison::NotEqual => panic!("Cannot encode not equal comparison "),
        }
    }
}

impl Stringify for ExecuteComponent {
    type Output = String;

    fn stringify(&self) -> Self::Output {
        match self {
            ExecuteComponent::IfScoreRelation {
                player1,
                player2,
                comparison,
            } if comparison == &ScoreboardComparison::NotEqual => format!(
                "unless score {} {} {} {} {}",
                player1.player,
                player1.scoreboard,
                ScoreboardComparison::Equal.stringify(),
                player2.player,
                player2.scoreboard,
            ),
            ExecuteComponent::IfScoreRelation {
                player1,
                player2,
                comparison,
            } => format!(
                "if score {} {} {} {} {}",
                player1.player,
                player1.scoreboard,
                comparison.stringify(),
                player2.player,
                player2.scoreboard
            ),
            ExecuteComponent::IfScoreRange {
                player,
                range: MinecraftRange::NotEqual(val),
            } => format!(
                "unless score {} {} matches {}",
                player.player,
                player.scoreboard,
                MinecraftRange::Equal(*val).stringify()
            ),
            ExecuteComponent::IfScoreRange { player, range } => format!(
                "if score {} {} matches {}",
                player.player,
                player.scoreboard,
                range.stringify()
            ),
        }
    }
}

impl Stringify for MinecraftRange {
    type Output = String;

    fn stringify(&self) -> Self::Output {
        match self {
            MinecraftRange::NotEqual(_) => panic!("Cannot stringify NotEqual range"),
            MinecraftRange::Equal(val) => format!("{}", val),
            MinecraftRange::Range { from, to } => format!("{}..{}", from, to),
            MinecraftRange::Minimum(min) => format!("{}..", min),
            MinecraftRange::Maximum(max) => format!("..{}", max),
        }
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

    use debris_core::{
        llir::utils::{ScoreboardComparison, ScoreboardOperation},
        BuildMode, Config,
    };

    use crate::common::{
        ExecuteComponent, FunctionIdent, MinecraftCommand, MinecraftRange, ObjectiveCriterion,
        ScoreboardPlayer,
    };

    use super::{stringify_template, Stringify};

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
            player: ScoreboardPlayer {
                player: Rc::new("@s".to_string()),
                scoreboard: Rc::new("debris".to_string()),
            },
            value: 100,
        };

        assert_eq!(command.stringify(), "scoreboard players set @s debris 100")
    }

    #[test]
    fn test_scoreboard_set_equal() {
        let command = MinecraftCommand::ScoreboardSetEqual {
            player1: ScoreboardPlayer {
                player: Rc::new("@s".to_string()),
                scoreboard: Rc::new("debris".to_string()),
            },
            player2: ScoreboardPlayer {
                player: Rc::new("foo".to_string()),
                scoreboard: Rc::new("debris.0".to_string()),
            },
        };

        assert_eq!(
            command.stringify(),
            "scoreboard players operation @s debris = foo debris.0"
        )
    }

    #[test]
    fn test_scoreboard_set_from_result() {
        let command1 = MinecraftCommand::ScoreboardSetEqual {
            player1: ScoreboardPlayer {
                player: Rc::new("@s".to_string()),
                scoreboard: Rc::new("debris".to_string()),
            },
            player2: ScoreboardPlayer {
                player: Rc::new("foo".to_string()),
                scoreboard: Rc::new("debris.0".to_string()),
            },
        };

        let command = MinecraftCommand::ScoreboardSetFromResult {
            player: ScoreboardPlayer {
                player: Rc::new("me".to_string()),
                scoreboard: Rc::new("debris".to_string()),
            },
            command: Box::new(command1),
        };

        assert_eq!(
            command.stringify(),
            "execute store result score me debris run scoreboard players operation @s debris = foo debris.0"
        )
    }

    #[test]
    fn test_scoreboard_operation() {
        let command = MinecraftCommand::ScoreboardOperation {
            player1: ScoreboardPlayer {
                player: Rc::new("value_1".to_string()),
                scoreboard: Rc::new("main".to_string()),
            },
            operation: ScoreboardOperation::Modulo,
            player2: ScoreboardPlayer {
                player: Rc::new("value_2".to_string()),
                scoreboard: Rc::new("main".to_string()),
            },
        };

        assert_eq!(
            command.stringify(),
            "scoreboard players operation value_1 main %= value_2 main"
        )
    }

    #[test]
    fn test_scoreboard_operation_add() {
        let command = MinecraftCommand::ScoreboardOperationAdd {
            player: ScoreboardPlayer {
                player: Rc::new("value_1".to_string()),
                scoreboard: Rc::new("main".to_string()),
            },
            value: 15,
        };

        assert_eq!(
            command.stringify(),
            "scoreboard players add value_1 main 15"
        )
    }

    #[test]
    fn test_scoreboard_operation_add_neg() {
        let command = MinecraftCommand::ScoreboardOperationAdd {
            player: ScoreboardPlayer {
                player: Rc::new("value_1".to_string()),
                scoreboard: Rc::new("main".to_string()),
            },
            value: -12,
        };

        assert_eq!(
            command.stringify(),
            "scoreboard players remove value_1 main 12"
        )
    }

    #[test]
    fn test_execute() {
        let command = MinecraftCommand::Excute {
            parts: vec![
                ExecuteComponent::IfScoreRelation {
                    comparison: ScoreboardComparison::GreaterOrEqual,
                    player1: ScoreboardPlayer {
                        player: Rc::new("val_1".to_string()),
                        scoreboard: Rc::new("main".to_string()),
                    },
                    player2: ScoreboardPlayer {
                        player: Rc::new("val_2".to_string()),
                        scoreboard: Rc::new("main2".to_string()),
                    },
                },
                ExecuteComponent::IfScoreRelation {
                    comparison: ScoreboardComparison::NotEqual,
                    player1: ScoreboardPlayer {
                        player: Rc::new("val_2".to_string()),
                        scoreboard: Rc::new("main2".to_string()),
                    },
                    player2: ScoreboardPlayer {
                        player: Rc::new("val_1".to_string()),
                        scoreboard: Rc::new("main".to_string()),
                    },
                },
            ],
            and_then: Some(Box::new(MinecraftCommand::RawCommand {
                command: Rc::new("do_something".into()),
            })),
        };

        assert_eq!(command.stringify(), "execute if score val_1 main >= val_2 main2 unless score val_2 main2 = val_1 main run do_something")
    }

    #[test]
    fn test_execute_no_command() {
        let command = MinecraftCommand::Excute {
            parts: vec![ExecuteComponent::IfScoreRelation {
                comparison: ScoreboardComparison::GreaterOrEqual,
                player1: ScoreboardPlayer {
                    player: Rc::new("val_1".to_string()),
                    scoreboard: Rc::new("main".to_string()),
                },
                player2: ScoreboardPlayer {
                    player: Rc::new("val_2".to_string()),
                    scoreboard: Rc::new("main2".to_string()),
                },
            }],
            and_then: None,
        };

        assert_eq!(
            command.stringify(),
            "execute if score val_1 main >= val_2 main2"
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

        assert_eq!(command.stringify(), "function debris:foo/bar")
    }

    #[test]
    fn test_scoreboard_add() {
        let command = MinecraftCommand::ScoreboardAdd {
            name: Rc::new("foo".to_string()),
            criterion: ObjectiveCriterion::Dummy,
            json_name: None,
        };

        assert_eq!(command.stringify(), "scoreboard objectives add foo dummy")
    }

    #[test]
    fn test_scoreboard_add_json_name() {
        let command = MinecraftCommand::ScoreboardAdd {
            name: Rc::new("foo".to_string()),
            criterion: ObjectiveCriterion::Other("Health".to_string()),
            json_name: Some(r#"{"text":"foo", "color":"green"}"#.to_string()),
        };

        assert_eq!(
            command.stringify(),
            r#"scoreboard objectives add foo Health {"text":"foo", "color":"green"}"#
        )
    }

    #[test]
    fn test_scoreboard_remove() {
        let command = MinecraftCommand::ScoreboardRemove {
            name: Rc::new("foo".to_string()),
        };

        assert_eq!(command.stringify(), "scoreboard objectives remove foo")
    }

    #[test]
    fn test_raw_command() {
        let command = MinecraftCommand::RawCommand {
            command: Rc::new("Hallo Welt".to_owned()),
        };

        assert_eq!(command.stringify(), "Hallo Welt")
    }

    #[test]
    fn test_stringify_execute_part_score_relation() {
        let part = ExecuteComponent::IfScoreRelation {
            comparison: ScoreboardComparison::Greater,
            player1: ScoreboardPlayer {
                player: Rc::new("val_1".to_string()),
                scoreboard: Rc::new("main".to_string()),
            },
            player2: ScoreboardPlayer {
                player: Rc::new("val_2".to_string()),
                scoreboard: Rc::new("main2".to_string()),
            },
        };

        assert_eq!(part.stringify(), "if score val_1 main > val_2 main2")
    }

    #[test]
    fn test_stringify_execute_part_score_range() {
        let part = ExecuteComponent::IfScoreRange {
            player: ScoreboardPlayer {
                player: Rc::new("val_1".to_string()),
                scoreboard: Rc::new("main".to_string()),
            },
            range: MinecraftRange::Range { from: 0, to: 99 },
        };

        assert_eq!(part.stringify(), "if score val_1 main matches 0..99")
    }

    #[test]
    fn test_stringify_execute_part_score_range_greater() {
        let part = ExecuteComponent::IfScoreRange {
            player: ScoreboardPlayer {
                player: Rc::new("val_1".to_string()),
                scoreboard: Rc::new("main".to_string()),
            },
            range: MinecraftRange::Minimum(4),
        };

        assert_eq!(part.stringify(), "if score val_1 main matches 4..")
    }

    #[test]
    fn test_stringify_execute_part_score_range_not() {
        let part = ExecuteComponent::IfScoreRange {
            player: ScoreboardPlayer {
                player: Rc::new("val_1".to_string()),
                scoreboard: Rc::new("main".to_string()),
            },
            range: MinecraftRange::NotEqual(-1),
        };

        assert_eq!(part.stringify(), "unless score val_1 main matches -1")
    }
}
