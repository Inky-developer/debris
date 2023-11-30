use datapack_common::vfs::Directory;
use debris_common::CompileContext;
use debris_llir::Llir;

use crate::Backend;

use super::generator::DatapackGenerator;

/// The Datapack Backend implementation
#[derive(Debug, Default)]
pub struct DatapackBackend;

impl DatapackBackend {
    pub const FILE_EXTENSION: &'static str = ".mcfunction";
    // Internal namespaces must be prefixed by double underscores, other namespaces may
    // be occupied by the user
    /// The directory which will contain all automatically generated minecraft files
    pub const FUNCTION_INTERNAL_PATH: &'static str = "__generated/";
}

impl Backend for DatapackBackend {
    fn generate(&self, llir: &Llir, ctx: &CompileContext) -> Directory {
        DatapackGenerator::new(ctx, llir).build()
    }
}

#[cfg(test)]
mod tests {
    use datapack_common::vfs::Directory;
    use debris_common::{CompilationId, CompileContext};
    use debris_llir::{
        block_id::BlockId,
        item_id::ItemId,
        json_format::{FormattedText, JsonFormatComponent},
        llir_nodes::{Branch, Condition, FastStore, Function, Node, WriteMessage, WriteTarget},
        minecraft_utils::{ScoreboardComparison, ScoreboardValue},
        CallGraph, CodeStats, Llir, Runtime,
    };
    use rustc_hash::FxHashMap;

    use crate::{Backend, DatapackBackend};

    fn text(msg: &str) -> FormattedText {
        FormattedText {
            components: vec![JsonFormatComponent::RawText(msg.into())],
        }
    }

    fn compile(nodes: Vec<Node>) -> Directory {
        let id = BlockId::dummy(0);
        let function = Function::_dummy(id, nodes);
        let mut map = FxHashMap::default();
        map.insert(id, function);
        let mut runtime = Runtime::default();
        runtime.add_on_load(id);
        let call_graph = CallGraph::from(&map);
        let mut stats = CodeStats::new(call_graph);
        stats.update(&runtime, &map);
        let llir = Llir {
            entry_function: id,
            functions: map,
            runtime,
            stats,
        };
        let mut dir = DatapackBackend.generate(&llir, &CompileContext::new(CompilationId(0)));
        let mut dir = dir.directories.remove("data").unwrap();
        let mut dir = dir.directories.remove("debris_project").unwrap();
        let mut dir = dir.directories.remove("functions").unwrap();
        dir.directories.remove("__generated").unwrap()
    }

    // ## Branch tests ##
    // Unfortunately, it is not possible to directly translate an if-else statement into minecraft functions.
    // The problem arises when `pos_branch` or `neg_branch` modify a value the condition depends on, because then both branches might be taken
    // The solution unfortunately contains many variants, depending on how exactly the if statement looks like, so for that reason
    // this many tests are required.

    #[test]
    fn test_branch_simple() {
        let value_id = ItemId { id: 0 };
        let nodes = vec![
            Node::FastStore(FastStore {
                id: value_id,
                value: ScoreboardValue::Static(1),
            }),
            Node::Branch(Branch {
                condition: Condition::Compare {
                    comparison: ScoreboardComparison::Equal,
                    lhs: ScoreboardValue::Scoreboard(value_id),
                    rhs: ScoreboardValue::Static(1),
                },
                pos_branch: Box::new(Node::Write(WriteMessage {
                    target: WriteTarget::Chat,
                    message: text("Equal to 1"),
                })),
                neg_branch: Box::new(Node::Write(WriteMessage {
                    target: WriteTarget::Chat,
                    message: text("Not equal to 1"),
                })),
            }),
        ];

        let mut pack = compile(nodes);
        let main = pack.file("main.mcfunction".to_string());
        assert_eq!(
            main.contents,
            concat!(
                "scoreboard objectives remove debris\n",
                "scoreboard objectives add debris dummy\n",
                "scoreboard players set var_0 debris 1\n",
                "execute if score var_0 debris matches 1 run tellraw @a [{\"text\":\"Equal to 1\"}]\n",
                "execute unless score var_0 debris matches 1 run tellraw @a [{\"text\":\"Not equal to 1\"}]\n",
            )
        );
    }

    #[test]
    fn test_branch_reordered_simple() {
        let value_id = ItemId { id: 0 };
        let nodes = vec![
            Node::FastStore(FastStore {
                id: value_id,
                value: ScoreboardValue::Static(1),
            }),
            Node::Branch(Branch {
                condition: Condition::Compare {
                    comparison: ScoreboardComparison::Equal,
                    lhs: ScoreboardValue::Scoreboard(value_id),
                    rhs: ScoreboardValue::Static(1),
                },
                pos_branch: Box::new(Node::FastStore(FastStore {
                    id: value_id,
                    value: ScoreboardValue::Static(0),
                })),
                neg_branch: Box::new(Node::Write(WriteMessage {
                    target: WriteTarget::Chat,
                    message: text("Not equal to 1"),
                })),
            }),
        ];

        let mut pack = compile(nodes);
        let main = pack.file("main.mcfunction".to_string());
        assert_eq!(
            main.contents,
            concat!(
                "scoreboard objectives remove debris\n",
                "scoreboard objectives add debris dummy\n",
                "scoreboard players set var_0 debris 1\n",
                "execute unless score var_0 debris matches 1 run tellraw @a [{\"text\":\"Not equal to 1\"}]\n",
                "execute if score var_0 debris matches 1 run scoreboard players set var_0 debris 0\n",
            )
        );
    }

    #[test]
    fn test_branch_reordered_complex() {
        let value_id = ItemId { id: 0 };
        let nodes = vec![
            Node::FastStore(FastStore {
                id: value_id,
                value: ScoreboardValue::Static(1),
            }),
            Node::Branch(Branch {
                condition: Condition::Or(vec![
                    Condition::Compare {
                        comparison: ScoreboardComparison::Equal,
                        lhs: ScoreboardValue::Scoreboard(value_id),
                        rhs: ScoreboardValue::Static(1),
                    },
                    Condition::Compare {
                        comparison: ScoreboardComparison::Equal,
                        lhs: ScoreboardValue::Static(0),
                        rhs: ScoreboardValue::Static(1),
                    },
                ]),
                pos_branch: Box::new(Node::FastStore(FastStore {
                    id: value_id,
                    value: ScoreboardValue::Static(0),
                })),
                neg_branch: Box::new(Node::Write(WriteMessage {
                    target: WriteTarget::Chat,
                    message: text("Not equal to 1"),
                })),
            }),
        ];

        let mut pack = compile(nodes);
        let main = pack.file("main.mcfunction".to_string());
        assert_eq!(
            main.contents,
            concat!(
                "scoreboard objectives remove debris\n",
                "scoreboard objectives add debris dummy\n",
                "scoreboard players set const_0 debris 0\n",
                "scoreboard players set const_1 debris 1\n",
                "scoreboard players set var_0 debris 1\n",
                "scoreboard players set var_1 debris 0\n",
                "execute store result score var_1 debris run execute unless score var_0 debris matches 1 unless score const_0 debris = const_1 debris \n",
                "execute unless score var_1 debris matches 0 run tellraw @a [{\"text\":\"Not equal to 1\"}]\n",
                "execute if score var_1 debris matches 0 run scoreboard players set var_0 debris 0\n",
            )
        );
    }

    #[test]
    fn test_branch_both_modify_simple() {
        let value_id = ItemId { id: 0 };
        let nodes = vec![
            Node::FastStore(FastStore {
                id: value_id,
                value: ScoreboardValue::Static(1),
            }),
            Node::Branch(Branch {
                condition: Condition::Compare {
                    comparison: ScoreboardComparison::Equal,
                    lhs: ScoreboardValue::Scoreboard(value_id),
                    rhs: ScoreboardValue::Static(1),
                },
                pos_branch: Box::new(Node::FastStore(FastStore {
                    id: value_id,
                    value: ScoreboardValue::Static(0),
                })),
                neg_branch: Box::new(Node::FastStore(FastStore {
                    id: value_id,
                    value: ScoreboardValue::Static(-1),
                })),
            }),
        ];

        let mut pack = compile(nodes);
        let main = pack.file("main.mcfunction".to_string());
        assert_eq!(
            main.contents,
            concat!(
                "scoreboard objectives remove debris\n",
                "scoreboard objectives add debris dummy\n",
                "scoreboard players set var_0 debris 1\n",
                "scoreboard players set var_1 debris 0\n",
                "execute if score var_0 debris matches 1 run function debris_project:__generated/block_0\n",
                "execute if score var_1 debris matches 0 unless score var_0 debris matches 1 run scoreboard players set var_0 debris -1\n",
            )
        );

        let block_0 = pack.file("block_0.mcfunction".to_string());
        assert_eq!(
            block_0.contents,
            concat!(
                "scoreboard players set var_0 debris 0\n",
                "scoreboard players set var_1 debris 1\n",
            )
        );
    }

    #[test]
    fn test_branch_both_modify_complex() {
        let value_id = ItemId { id: 0 };
        let nodes = vec![
            Node::FastStore(FastStore {
                id: value_id,
                value: ScoreboardValue::Static(1),
            }),
            Node::Branch(Branch {
                condition: Condition::Or(vec![
                    Condition::Compare {
                        comparison: ScoreboardComparison::Equal,
                        lhs: ScoreboardValue::Scoreboard(value_id),
                        rhs: ScoreboardValue::Static(1),
                    },
                    Condition::Compare {
                        comparison: ScoreboardComparison::Equal,
                        lhs: ScoreboardValue::Static(0),
                        rhs: ScoreboardValue::Static(1),
                    },
                ]),
                pos_branch: Box::new(Node::FastStore(FastStore {
                    id: value_id,
                    value: ScoreboardValue::Static(0),
                })),
                neg_branch: Box::new(Node::FastStore(FastStore {
                    id: value_id,
                    value: ScoreboardValue::Static(-1),
                })),
            }),
        ];

        let mut pack = compile(nodes);
        let main = pack.file("main.mcfunction".to_string());
        assert_eq!(
            main.contents,
            concat!(
                "scoreboard objectives remove debris\n",
                "scoreboard objectives add debris dummy\n",
                "scoreboard players set const_0 debris 0\n",
                "scoreboard players set const_1 debris 1\n",
                "scoreboard players set var_0 debris 1\n",
                "scoreboard players set var_1 debris 0\n",
                "scoreboard players set var_2 debris 0\n",
                "execute store result score var_2 debris run execute unless score var_0 debris matches 1 unless score const_0 debris = const_1 debris \n",
                "execute if score var_2 debris matches 0 run function debris_project:__generated/block_0\n",
                "execute if score var_1 debris matches 0 unless score var_2 debris matches 0 run scoreboard players set var_0 debris -1\n",
            )
        );

        let block_0 = pack.file("block_0.mcfunction".to_string());
        assert_eq!(
            block_0.contents,
            concat!(
                "scoreboard players set var_0 debris 0\n",
                "scoreboard players set var_1 debris 1\n",
            )
        );
    }
}
