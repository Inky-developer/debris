use std::{collections::HashMap, rc::Rc};

use debris_core::{
    llir::llir_nodes::BinaryOperation,
    llir::llir_nodes::Call,
    llir::llir_nodes::Execute,
    llir::llir_nodes::FastStoreFromResult,
    llir::utils::Scoreboard,
    llir::utils::ScoreboardValue,
    llir::{
        llir_nodes::FastStore,
        llir_nodes::Function,
        llir_nodes::{Condition, Node},
        utils::{ItemId, ScoreboardOperation},
        LLIR,
    },
    Config,
};
use vfs::Directory;

use crate::{
    common::FunctionIdent,
    common::MinecraftCommand,
    common::{ExecuteComponent, ObjectiveCriterion, ScoreboardPlayer},
    Backend,
};

use super::{stringify::stringify_command, Datapack};

/// The Datapack Backend implementation
#[derive(Default, Debug)]
pub struct DatapackBackend {
    /// The compilation configuration
    config: Rc<Config>,
    /// The name of the namespace which contains all generated functions
    function_namespace: Rc<String>,
    /// A map of all functions, uses the function name as the key
    functions: HashMap<String, Vec<MinecraftCommand>>,
    /// A map from uid to Function identifier
    function_identifiers: HashMap<u64, Rc<FunctionIdent>>,
    /// The current stack
    ///
    /// Commands are pushed into the last value of last context
    stack: Vec<Vec<MinecraftCommand>>,
    /// A context which keeps track of the currently used scoreboards
    scoreboard_ctx: ScoreboardContext,
}

impl DatapackBackend {
    /// Returns the filename that corresponds to the function id
    fn get_filename_for_function(&mut self, id: u64) -> String {
        let function_name = format!("block_{}_", id);
        let identifier = FunctionIdent {
            is_collection: false,
            namespace: self.function_namespace.clone(),
            path: function_name.clone(),
        };
        self.function_identifiers.insert(id, Rc::new(identifier));
        format!("{}.mcfunction", function_name)
    }

    /// Adds a command to the current stack
    fn add_command(&mut self, command: MinecraftCommand) {
        self.stack.last_mut().expect("Empty stack").push(command);
    }

    /// Handles the given command and returns the produced output
    fn catch_ouput(&mut self, node: &Node) -> Vec<MinecraftCommand> {
        self.stack.push(Vec::new());
        self.handle(node);
        self.stack.pop().unwrap()
    }

    /// Handles the main fucntion
    ///
    /// The `main_id` marks the main function.
    fn handle_main_function(&mut self, main_id: u64) {
        let name = "main.mcfunction".to_string();
        self.stack.push(Vec::new());

        {
            // Initialize all scoreboards
            let scoreboard_commands: Vec<_> = self
                .scoreboard_ctx
                .scoreboards
                .values()
                .flat_map(|scoreboard_name| {
                    vec![
                        MinecraftCommand::ScoreboardRemove {
                            name: scoreboard_name.clone(),
                        },
                        MinecraftCommand::ScoreboardAdd {
                            name: scoreboard_name.clone(),
                            criterion: ObjectiveCriterion::Dummy,
                            json_name: None,
                        },
                    ]
                })
                .collect();

            for command in scoreboard_commands {
                self.add_command(command)
            }

            // Handle the main function
            self.handle(&Node::Call(Call { id: main_id }));
        }

        let nodes = self.stack.pop().unwrap();
        self.functions.insert(name, nodes);
    }

    /// Handles any node
    ///
    /// Currently mostly unimplemented
    fn handle(&mut self, node: &Node) {
        match node {
            Node::Function(function) => self.handle_function(function),
            Node::FastStore(fast_store) => self.handle_fast_store(fast_store),
            Node::FastStoreFromResult(fast_store_from_result) => {
                self.handle_fast_store_from_result(fast_store_from_result)
            }
            Node::BinaryOperation(binop) => self.handle_binary_operation(binop),
            Node::Call(call) => self.handle_call(call),
            Node::Condition(condition) => self.handle_condition(condition),
            Node::Execute(execute) => self.handle_execute(execute),
            _ => todo!("Handler for '{:?}' is not yet implemented", node),
        }
    }

    // Node handlers

    fn handle_function(&mut self, function: &Function) {
        let name = self.get_filename_for_function(function.id);
        self.stack.push(Vec::new());

        for node in &function.nodes {
            self.handle(node);
        }

        let nodes = self.stack.pop().unwrap();
        self.functions.insert(name, nodes);
    }

    fn handle_fast_store(&mut self, fast_store: &FastStore) {
        let value = &fast_store.value;

        match value {
            ScoreboardValue::Static(static_value) => {
                let command = MinecraftCommand::ScoreboardSet {
                    player: ScoreboardPlayer {
                        player: self.scoreboard_ctx.get_scoreboard_player(fast_store.id),
                        scoreboard: self.scoreboard_ctx.get_scoreboard(fast_store.scoreboard),
                    },
                    value: *static_value,
                };
                self.add_command(command)
            }
            ScoreboardValue::Scoreboard(other_scoreboard, other_player) => {
                let command = MinecraftCommand::ScoreboardSetEqual {
                    player1: ScoreboardPlayer {
                        player: self.scoreboard_ctx.get_scoreboard_player(fast_store.id),
                        scoreboard: self.scoreboard_ctx.get_scoreboard(fast_store.scoreboard),
                    },
                    player2: ScoreboardPlayer {
                        player: self.scoreboard_ctx.get_scoreboard_player(*other_player),
                        scoreboard: self.scoreboard_ctx.get_scoreboard(*other_scoreboard),
                    },
                };
                self.add_command(command)
            }
        }
    }

    fn handle_fast_store_from_result(&mut self, fast_store_from_result: &FastStoreFromResult) {
        let inner_commands = self.catch_ouput(&fast_store_from_result.command);

        if inner_commands.len() == 1 {
            let command = MinecraftCommand::ScoreboardSetFromResult {
                player: ScoreboardPlayer {
                    player: self
                        .scoreboard_ctx
                        .get_scoreboard_player(fast_store_from_result.id),
                    scoreboard: self
                        .scoreboard_ctx
                        .get_scoreboard(fast_store_from_result.scoreboard),
                },
                command: Box::new(inner_commands.into_iter().next().unwrap()),
            };
            self.add_command(command);
        } else {
            panic!("Expected only one inner funciton");
        }
    }

    fn handle_binary_operation(&mut self, binary_operation: &BinaryOperation) {
        // Calculate the lhs scoreboard value and the rhs scoreboard value
        let (lhs_player, lhs_scoreboard, rhs_player, rhs_scoreboard, overwrite_lhs) =
            match (binary_operation.lhs, binary_operation.rhs) {
                (ScoreboardValue::Static(_), ScoreboardValue::Static(_)) => {
                    panic!("Expected at least one non-static operand")
                }
                (
                    ScoreboardValue::Static(lhs_val),
                    ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_id),
                ) => {
                    let lhs_scoreboard = self
                        .scoreboard_ctx
                        .get_scoreboard(binary_operation.scoreboard);
                    let lhs_id = self
                        .scoreboard_ctx
                        .get_scoreboard_player(binary_operation.id);

                    self.add_command(MinecraftCommand::ScoreboardSet {
                        player: ScoreboardPlayer {
                            player: lhs_id.clone(),
                            scoreboard: lhs_scoreboard.clone(),
                        },
                        value: lhs_val,
                    });
                    (
                        lhs_id,
                        lhs_scoreboard,
                        self.scoreboard_ctx.get_scoreboard_player(rhs_id),
                        self.scoreboard_ctx.get_scoreboard(rhs_scoreboard),
                        true,
                    )
                }
                (
                    ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                    ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_id),
                ) => (
                    self.scoreboard_ctx.get_scoreboard_player(lhs_id),
                    self.scoreboard_ctx.get_scoreboard(lhs_scoreboard),
                    self.scoreboard_ctx.get_scoreboard_player(rhs_id),
                    self.scoreboard_ctx.get_scoreboard(rhs_scoreboard),
                    lhs_id == binary_operation.id,
                ),
                (
                    ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                    ScoreboardValue::Static(value),
                ) if (binary_operation.operation == ScoreboardOperation::Plus
                    || binary_operation.operation == ScoreboardOperation::Minus)
                    && lhs_id == binary_operation.id =>
                {
                    let real_value = if binary_operation.operation == ScoreboardOperation::Minus {
                        -value
                    } else {
                        value
                    };

                    let player = self.scoreboard_ctx.get_scoreboard_player(lhs_id);
                    let scoreboard = self.scoreboard_ctx.get_scoreboard(lhs_scoreboard);

                    self.add_command(MinecraftCommand::ScoreboardOperationAdd {
                        player: ScoreboardPlayer { player, scoreboard },
                        value: real_value,
                    });

                    return;
                }
                (
                    ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                    ScoreboardValue::Static(value),
                ) => {
                    // Convert the rhs to a scoreboard value
                    let player = self.scoreboard_ctx.get_temporary_player();
                    let scoreboard = self.scoreboard_ctx.get_scoreboard(Scoreboard::Main);
                    self.add_command(MinecraftCommand::ScoreboardSet {
                        player: ScoreboardPlayer {
                            player: player.clone(),
                            scoreboard: scoreboard.clone(),
                        },
                        value,
                    });
                    (
                        self.scoreboard_ctx.get_scoreboard_player(lhs_id),
                        self.scoreboard_ctx.get_scoreboard(lhs_scoreboard),
                        player,
                        scoreboard,
                        lhs_id == binary_operation.id,
                    )
                }
            };

        // If lhs_id is not the same as target id, initialize it first
        if !overwrite_lhs {
            let player1 = self
                .scoreboard_ctx
                .get_scoreboard_player(binary_operation.id);
            let scoreboard1 = self
                .scoreboard_ctx
                .get_scoreboard(binary_operation.scoreboard);
            let player2 = lhs_player;
            let scoreboard2 = lhs_scoreboard;
            self.add_command(MinecraftCommand::ScoreboardSetEqual {
                player1: ScoreboardPlayer {
                    player: player1,
                    scoreboard: scoreboard1,
                },
                player2: ScoreboardPlayer {
                    player: player2,
                    scoreboard: scoreboard2,
                },
            });
        }

        // Then operate directly on target id
        let player1 = self
            .scoreboard_ctx
            .get_scoreboard_player(binary_operation.id);
        let scoreboard1 = self
            .scoreboard_ctx
            .get_scoreboard(binary_operation.scoreboard);
        self.add_command(MinecraftCommand::ScoreboardOperation {
            player1: ScoreboardPlayer {
                player: player1,
                scoreboard: scoreboard1,
            },
            player2: ScoreboardPlayer {
                player: rhs_player,
                scoreboard: rhs_scoreboard,
            },
            operation: binary_operation.operation,
        })
    }

    fn handle_condition(&mut self, condition: &Condition) {
        match condition {
            Condition::Compare {
                lhs,
                rhs,
                comparison,
            } => match (lhs, rhs) {
                (
                    ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                    ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_id),
                ) => {
                    let command = MinecraftCommand::Excute {
                        parts: vec![ExecuteComponent::IfScoreRelation {
                            comparison: *comparison,
                            player1: ScoreboardPlayer {
                                player: self.scoreboard_ctx.get_scoreboard_player(*lhs_id),
                                scoreboard: self.scoreboard_ctx.get_scoreboard(*lhs_scoreboard),
                            },
                            player2: ScoreboardPlayer {
                                player: self.scoreboard_ctx.get_scoreboard_player(*rhs_id),
                                scoreboard: self.scoreboard_ctx.get_scoreboard(*rhs_scoreboard),
                            },
                        }],
                        and_then: None,
                    };

                    self.add_command(command);
                }
                (_, _) => {
                    todo!("Comparison only implemented for (scoreboard, scoreboard) pairs for now")
                }
            },
        }
    }

    fn handle_call(&mut self, call: &Call) {
        let function_ident = self
            .function_identifiers
            .get(&call.id)
            .expect("Unknown function");

        let command = MinecraftCommand::Function {
            function: function_ident.clone(),
        };
        self.add_command(command)
    }

    fn handle_execute(&mut self, execute: &Execute) {
        self.add_command(MinecraftCommand::RawCommand {
            command: Rc::new(execute.command.clone()),
        });
    }
}

impl Backend for DatapackBackend {
    fn new(config: Rc<Config>) -> Self {
        let function_namespace = Rc::new(config.project_name.to_lowercase());
        let scoreboard_ctx = ScoreboardContext::new(config.default_scoreboard_name.clone());
        DatapackBackend {
            config,
            function_namespace,
            scoreboard_ctx,
            ..Default::default()
        }
    }

    fn handle_llir(&mut self, llir: &LLIR) -> Directory {
        let mut pack = Datapack::new(&self.config);

        // Assume the first function is the main function
        // Ignore the other functions unless they are called
        let function = &llir.functions[0];

        self.handle_function(function);
        self.handle_main_function(function.id);

        let functions = pack.functions();

        for (fn_name, fn_proto) in &self.functions {
            let contents = fn_proto.iter().map(|cmd| stringify_command(&cmd)).fold(
                String::new(),
                |mut prev, next| {
                    prev.push_str(&next);
                    prev.push('\n');
                    prev
                },
            );
            functions.file(fn_name.clone()).push_string(&contents);
        }

        pack.dir
    }
}

/// Holds data about specific scoreboard contexts
#[derive(Debug, Default)]
struct ScoreboardContext {
    scoreboard_players: HashMap<ScoreboardPlayerId, Rc<String>>,
    scoreboards: HashMap<Scoreboard, Rc<String>>,
    scoreboard_prefix: String,
}

impl ScoreboardContext {
    /// Creates a new scoreboard context with the default scoreboard name
    fn new(scoreboard_prefix: String) -> Self {
        ScoreboardContext {
            scoreboard_prefix,
            ..Default::default()
        }
    }

    /// Returns the name of this scoreboard
    ///
    /// Internally creates a scoreboard if it did not exist yet
    #[allow(clippy::map_entry)]
    fn get_scoreboard(&mut self, scoreboard: Scoreboard) -> Rc<String> {
        if !self.scoreboards.contains_key(&scoreboard) {
            self.scoreboards
                .insert(scoreboard, self.format_scoreboard(scoreboard));
        }
        self.scoreboards.get(&scoreboard).unwrap().clone()
    }

    /// Gets the scoreboard player that corresponds to this `ItemId`
    fn get_scoreboard_player(&mut self, item_id: ItemId) -> Rc<String> {
        let num_players = self.scoreboard_players.len() as u64;
        self.scoreboard_players
            .entry(item_id.into())
            .or_insert_with(|| Self::format_player(num_players))
            .clone()
    }

    /// Makes a new scoreboard player and returns the name
    fn get_temporary_player(&mut self) -> Rc<String> {
        let length = self.scoreboard_players.len() as u64;
        self.scoreboard_players
            .entry(ScoreboardPlayerId::Temporary(length))
            .or_insert_with(|| Self::format_player(length))
            .clone()
    }

    fn format_player(id: u64) -> Rc<String> {
        Rc::new(format!("var_{}", id))
    }

    fn format_scoreboard(&self, scoreboard: Scoreboard) -> Rc<String> {
        Rc::new(match scoreboard {
            Scoreboard::Main => self.scoreboard_prefix.to_string(),
            Scoreboard::Custom(id) => format!("{}.{}", self.scoreboard_prefix, id),
        })
    }
}

/// Used to differentiate between a generated id and a temporary id created by this backend
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum ScoreboardPlayerId {
    Normal(ItemId),
    Temporary(u64),
}

impl From<ItemId> for ScoreboardPlayerId {
    fn from(value: ItemId) -> Self {
        ScoreboardPlayerId::Normal(value)
    }
}
