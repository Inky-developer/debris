use std::fmt;

use debris_llir::{
    llir_nodes::WriteTarget,
    utils::{ScoreboardComparison, ScoreboardOperation},
};
use fmt::Display;

use crate::common::{ExecuteComponent, MinecraftCommand, MinecraftRange};

pub trait Stringify {
    fn as_str(&self) -> &'static str;
}

impl fmt::Display for MinecraftCommand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MinecraftCommand::ScoreboardSet { player, value } => write!(
                f,
                "scoreboard players set {} {} {}",
                player.player, player.scoreboard, value
            ),
            MinecraftCommand::ScoreboardSetEqual { player1, player2 } => write!(
                f,
                "scoreboard players operation {} {} = {} {}",
                player1.player, player1.scoreboard, player2.player, player2.scoreboard
            ),
            MinecraftCommand::ScoreboardSetFromResult { player, command } => {
                write!(
                    f,
                    "execute store result score {} {} run ",
                    player.player, player.scoreboard,
                )?;
                command.fmt(f)
            }
            MinecraftCommand::ScoreboardOperation {
                player1,
                player2,
                operation,
            } => write!(
                f,
                "scoreboard players operation {} {} {} {} {}",
                player1.player,
                player1.scoreboard,
                operation.as_str(),
                player2.player,
                player2.scoreboard
            ),
            MinecraftCommand::ScoreboardOperationAdd { player, value } => {
                let (mode, value) = if *value < 0 {
                    ("remove", value * -1)
                } else {
                    ("add", *value)
                };

                write!(
                    f,
                    "scoreboard players {} {} {} {}",
                    mode, player.player, player.scoreboard, value
                )
            }
            MinecraftCommand::Execute { parts, and_then } => {
                write!(f, "execute ")?;

                for part in parts {
                    part.fmt(f)?;
                    write!(f, " ")?;
                }

                if let Some(and_then) = and_then {
                    write!(f, "run {}", and_then)?;
                }
                Ok(())
            }
            MinecraftCommand::Function { function } => write!(f, "function {}", function),
            MinecraftCommand::ScoreboardAdd {
                name,
                criterion,
                json_name,
            } => {
                if let Some(json) = json_name {
                    write!(
                        f,
                        "scoreboard objectives add {} {} {}",
                        name, criterion, json
                    )
                } else {
                    write!(f, "scoreboard objectives add {} {}", name, criterion)
                }
            }
            MinecraftCommand::ScoreboardRemove { name } => {
                write!(f, "scoreboard objectives remove {}", name)
            }
            MinecraftCommand::RawCommand { command } => write!(f, "{}", command),
            MinecraftCommand::JsonMessage { target, message } => match target {
                WriteTarget::Chat => write!(f, "tellraw @a {}", message),
                WriteTarget::Actionbar => write!(f, "title @a actionbar {}", message),
                WriteTarget::Subtitle => write!(f, "title @a subtitle {}", message),
                WriteTarget::Title => write!(f, "title @a title {}", message),
            },
        }
    }
}

impl fmt::Display for ExecuteComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExecuteComponent::IfScoreRelation {
                player1,
                player2,
                comparison,
            } if comparison == &ScoreboardComparison::NotEqual => write!(
                f,
                "unless score {} {} {} {} {}",
                player1.player,
                player1.scoreboard,
                ScoreboardComparison::Equal.as_str(),
                player2.player,
                player2.scoreboard,
            ),
            ExecuteComponent::IfScoreRelation {
                player1,
                player2,
                comparison,
            } => write!(
                f,
                "if score {} {} {} {} {}",
                player1.player,
                player1.scoreboard,
                comparison.as_str(),
                player2.player,
                player2.scoreboard
            ),
            ExecuteComponent::IfScoreRange {
                player,
                range: MinecraftRange::NotEqual(val),
            } => {
                write!(
                    f,
                    "unless score {} {} matches ",
                    player.player, player.scoreboard,
                )?;
                MinecraftRange::Equal(*val).fmt(f)
            }
            ExecuteComponent::IfScoreRange { player, range } => {
                write!(
                    f,
                    "if score {} {} matches ",
                    player.player, player.scoreboard,
                )?;
                range.fmt(f)
            }
        }
    }
}

impl Display for MinecraftRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MinecraftRange::NotEqual(_) => panic!("Cannot stringify NotEqual range"),
            MinecraftRange::Equal(val) => write!(f, "{}", val),
            MinecraftRange::Range { from, to } => write!(f, "{}..{}", from, to),
            MinecraftRange::Minimum(min) => write!(f, "{}..", min),
            MinecraftRange::Maximum(max) => write!(f, "..{}", max),
        }
    }
}

impl Stringify for ScoreboardOperation {
    fn as_str(&self) -> &'static str {
        match self {
            ScoreboardOperation::Plus => "+=",
            ScoreboardOperation::Minus => "-=",
            ScoreboardOperation::Times => "*=",
            ScoreboardOperation::Divide => "/=",
            ScoreboardOperation::Modulo => "%=",
            ScoreboardOperation::Max => ">",
            ScoreboardOperation::Min => "<",
        }
    }
}

impl Stringify for ScoreboardComparison {
    fn as_str(&self) -> &'static str {
        match self {
            ScoreboardComparison::Equal => "=",
            ScoreboardComparison::Less => "<",
            ScoreboardComparison::LessOrEqual => "<=",
            ScoreboardComparison::Greater => ">",
            ScoreboardComparison::GreaterOrEqual => ">=",
            ScoreboardComparison::NotEqual => "!=",
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use debris_llir::{
        llir_nodes::WriteTarget,
        utils::{ScoreboardComparison, ScoreboardOperation},
    };

    use crate::common::{
        ExecuteComponent, FunctionIdent, MinecraftCommand, MinecraftRange, ObjectiveCriterion,
        ScoreboardPlayer,
    };

    #[test]
    fn test_scoreboard_set() {
        let command = MinecraftCommand::ScoreboardSet {
            player: ScoreboardPlayer {
                player: "@s".into(),
                scoreboard: "debris".into(),
            },
            value: 100,
        };

        assert_eq!(command.to_string(), "scoreboard players set @s debris 100");
    }

    #[test]
    fn test_scoreboard_set_equal() {
        let command = MinecraftCommand::ScoreboardSetEqual {
            player1: ScoreboardPlayer {
                player: "@s".into(),
                scoreboard: "debris".into(),
            },
            player2: ScoreboardPlayer {
                player: "foo".into(),
                scoreboard: "debris.0".into(),
            },
        };

        assert_eq!(
            command.to_string(),
            "scoreboard players operation @s debris = foo debris.0"
        );
    }

    #[test]
    fn test_scoreboard_set_from_result() {
        let command1 = MinecraftCommand::ScoreboardSetEqual {
            player1: ScoreboardPlayer {
                player: "@s".into(),
                scoreboard: "debris".into(),
            },
            player2: ScoreboardPlayer {
                player: "foo".into(),
                scoreboard: "debris.0".into(),
            },
        };

        let command = MinecraftCommand::ScoreboardSetFromResult {
            player: ScoreboardPlayer {
                player: "me".into(),
                scoreboard: "debris".into(),
            },
            command: Box::new(command1),
        };

        assert_eq!(
            command.to_string(),
            "execute store result score me debris run scoreboard players operation @s debris = foo debris.0"
        );
    }

    #[test]
    fn test_scoreboard_operation() {
        let command = MinecraftCommand::ScoreboardOperation {
            player1: ScoreboardPlayer {
                player: "value_1".into(),
                scoreboard: "main".into(),
            },
            operation: ScoreboardOperation::Modulo,
            player2: ScoreboardPlayer {
                player: "value_2".into(),
                scoreboard: "main".into(),
            },
        };

        assert_eq!(
            command.to_string(),
            "scoreboard players operation value_1 main %= value_2 main"
        );
    }

    #[test]
    fn test_scoreboard_operation_add() {
        let command = MinecraftCommand::ScoreboardOperationAdd {
            player: ScoreboardPlayer {
                player: "value_1".into(),
                scoreboard: "main".into(),
            },
            value: 15,
        };

        assert_eq!(
            command.to_string(),
            "scoreboard players add value_1 main 15"
        );
    }

    #[test]
    fn test_scoreboard_operation_add_neg() {
        let command = MinecraftCommand::ScoreboardOperationAdd {
            player: ScoreboardPlayer {
                player: "value_1".into(),
                scoreboard: "main".into(),
            },
            value: -12,
        };

        assert_eq!(
            command.to_string(),
            "scoreboard players remove value_1 main 12"
        );
    }

    #[test]
    fn test_execute() {
        let command = MinecraftCommand::Execute {
            parts: vec![
                ExecuteComponent::IfScoreRelation {
                    comparison: ScoreboardComparison::GreaterOrEqual,
                    player1: ScoreboardPlayer {
                        player: "val_1".into(),
                        scoreboard: "main".into(),
                    },
                    player2: ScoreboardPlayer {
                        player: "val_2".into(),
                        scoreboard: "main2".into(),
                    },
                },
                ExecuteComponent::IfScoreRelation {
                    comparison: ScoreboardComparison::NotEqual,
                    player1: ScoreboardPlayer {
                        player: "val_2".into(),
                        scoreboard: "main2".into(),
                    },
                    player2: ScoreboardPlayer {
                        player: "val_1".into(),
                        scoreboard: "main".into(),
                    },
                },
            ],
            and_then: Some(Box::new(MinecraftCommand::RawCommand {
                command: "do_something".into(),
            })),
        };

        assert_eq!(command.to_string(), "execute if score val_1 main >= val_2 main2 unless score val_2 main2 = val_1 main run do_something");
    }

    #[test]
    fn test_execute_no_command() {
        let command = MinecraftCommand::Execute {
            parts: vec![ExecuteComponent::IfScoreRelation {
                comparison: ScoreboardComparison::GreaterOrEqual,
                player1: ScoreboardPlayer {
                    player: "val_1".into(),
                    scoreboard: "main".into(),
                },
                player2: ScoreboardPlayer {
                    player: "val_2".into(),
                    scoreboard: "main2".into(),
                },
            }],
            and_then: None,
        };

        assert_eq!(
            command.to_string(),
            "execute if score val_1 main >= val_2 main2 "
        );
    }

    #[test]
    fn test_function() {
        let command = MinecraftCommand::Function {
            function: Rc::new(FunctionIdent {
                is_collection: false,
                namespace: "debris".into(),
                path: "foo/bar".to_string(),
            }),
        };

        assert_eq!(command.to_string(), "function debris:foo/bar");
    }

    #[test]
    fn test_scoreboard_add() {
        let command = MinecraftCommand::ScoreboardAdd {
            name: "foo".into(),
            criterion: ObjectiveCriterion::Dummy,
            json_name: None,
        };

        assert_eq!(command.to_string(), "scoreboard objectives add foo dummy");
    }

    // #[test]
    // fn test_scoreboard_add_json_name() {
    //     let command = MinecraftCommand::ScoreboardAdd {
    //         name: "foo".into(),
    //         criterion: ObjectiveCriterion::Other("Health".to_string()),
    //         json_name: Some(r#"{"text":"foo", "color":"green"}"#.to_string()),
    //     };

    //     assert_eq!(
    //         command.stringify(),
    //         r#"scoreboard objectives add foo Health {"text":"foo", "color":"green"}"#
    //     )
    // }

    #[test]
    fn test_scoreboard_remove() {
        let command = MinecraftCommand::ScoreboardRemove { name: "foo".into() };

        assert_eq!(command.to_string(), "scoreboard objectives remove foo");
    }

    #[test]
    fn test_raw_command() {
        let command = MinecraftCommand::RawCommand {
            command: "Hallo Welt".into(),
        };

        assert_eq!(command.to_string(), "Hallo Welt");
    }

    #[test]
    fn test_write_message() {
        let command = MinecraftCommand::JsonMessage {
            target: WriteTarget::Actionbar,
            message: "Hello World".to_string(),
        };

        assert_eq!(command.to_string(), "title @a actionbar Hello World");
    }

    #[test]
    fn test_write_message_chat() {
        let command = MinecraftCommand::JsonMessage {
            target: WriteTarget::Chat,
            message: "Hello World".to_string(),
        };

        assert_eq!(command.to_string(), "tellraw @a Hello World");
    }

    #[test]
    fn test_stringify_execute_part_score_relation() {
        let part = ExecuteComponent::IfScoreRelation {
            comparison: ScoreboardComparison::Greater,
            player1: ScoreboardPlayer {
                player: "val_1".into(),
                scoreboard: "main".into(),
            },
            player2: ScoreboardPlayer {
                player: "val_2".into(),
                scoreboard: "main2".into(),
            },
        };

        assert_eq!(part.to_string(), "if score val_1 main > val_2 main2");
    }

    #[test]
    fn test_stringify_execute_part_score_range() {
        let part = ExecuteComponent::IfScoreRange {
            player: ScoreboardPlayer {
                player: "val_1".into(),
                scoreboard: "main".into(),
            },
            range: MinecraftRange::Range { from: 0, to: 99 },
        };

        assert_eq!(part.to_string(), "if score val_1 main matches 0..99");
    }

    #[test]
    fn test_stringify_execute_part_score_range_greater() {
        let part = ExecuteComponent::IfScoreRange {
            player: ScoreboardPlayer {
                player: "val_1".into(),
                scoreboard: "main".into(),
            },
            range: MinecraftRange::Minimum(4),
        };

        assert_eq!(part.to_string(), "if score val_1 main matches 4..");
    }

    #[test]
    fn test_stringify_execute_part_score_range_not() {
        let part = ExecuteComponent::IfScoreRange {
            player: ScoreboardPlayer {
                player: "val_1".into(),
                scoreboard: "main".into(),
            },
            range: MinecraftRange::NotEqual(-1),
        };

        assert_eq!(part.to_string(), "unless score val_1 main matches -1");
    }
}
