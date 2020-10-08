use std::{collections::HashMap, rc::Rc};

use debris_core::{
    llir::llir_nodes::BinaryOperation,
    llir::llir_nodes::Call,
    llir::llir_nodes::Execute,
    llir::llir_nodes::FastStoreFromResult,
    llir::utils::Scoreboard,
    llir::utils::ScoreboardValue,
    llir::{llir_nodes::FastStore, llir_nodes::Function, llir_nodes::Node, utils::ItemId, LLIR},
    Config,
};
use vfs::Directory;

use crate::{common::FunctionIdent, common::MinecraftCommand, common::ObjectiveCriterion, Backend};

use super::{stringify::stringify_command, Datapack};

#[derive(Default, Debug)]
pub struct DatapackBackend {
    config: Rc<Config>,
    function_namespace: Rc<String>,
    functions: HashMap<String, Vec<MinecraftCommand>>,
    function_identifiers: HashMap<u64, Rc<FunctionIdent>>,
    stack: Vec<Vec<MinecraftCommand>>,
    scoreboard_ctx: ScoreboardContext,
}

impl DatapackBackend {
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

    fn handle(&mut self, node: &Node) {
        match node {
            Node::Function(function) => self.handle_function(function),
            Node::FastStore(fast_store) => self.handle_fast_store(fast_store),
            Node::FastStoreFromResult(fast_store_from_result) => {
                self.handle_fast_store_from_result(fast_store_from_result)
            }
            Node::BinaryOperation(binop) => self.handle_binary_operation(binop),
            Node::Call(call) => self.handle_call(call),
            Node::Execute(execute) => self.handle_execute(execute),
            _ => todo!("Handler for '{:?}' is not yet implemented", node),
        }
    }

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
                    player: self.scoreboard_ctx.get_scoreboard_player(fast_store.id),
                    scoreboard: self.scoreboard_ctx.get_scoreboard(fast_store.scoreboard),
                    value: *static_value,
                };
                self.add_command(command)
            }
            ScoreboardValue::Scoreboard(other_scoreboard, other_player) => {
                let command = MinecraftCommand::ScoreboardSetEqual {
                    player1: self.scoreboard_ctx.get_scoreboard_player(fast_store.id),
                    scoreboard1: self.scoreboard_ctx.get_scoreboard(fast_store.scoreboard),
                    player2: self.scoreboard_ctx.get_scoreboard_player(*other_player),
                    scoreboard2: self.scoreboard_ctx.get_scoreboard(*other_scoreboard),
                };
                self.add_command(command)
            }
        }
    }

    fn handle_fast_store_from_result(&mut self, fast_store_from_result: &FastStoreFromResult) {
        let inner_commands = self.catch_ouput(&fast_store_from_result.command);

        if inner_commands.len() == 1 {
            let command = MinecraftCommand::ScoreboardSetFromResult {
                player: self
                    .scoreboard_ctx
                    .get_scoreboard_player(fast_store_from_result.id),
                scoreboard: self
                    .scoreboard_ctx
                    .get_scoreboard(fast_store_from_result.scoreboard),
                command: Box::new(inner_commands.into_iter().next().unwrap()),
            };
            self.add_command(command);
        } else {
            panic!("Expected only one inner funciton");
        }
    }

    fn handle_binary_operation(&mut self, binary_operation: &BinaryOperation) {
        // Calculate the lhs scoreboard value and the rhs scoreboard value
        let (lhs_id, lhs_scoreboard, rhs_player, rhs_scoreboard) =
            match (binary_operation.lhs, binary_operation.rhs) {
                (ScoreboardValue::Static(_), _) => panic!("Expected first value to not be static"),
                (
                    ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                    ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_id),
                ) => (
                    lhs_id,
                    lhs_scoreboard,
                    self.scoreboard_ctx.get_scoreboard_player(rhs_id),
                    self.scoreboard_ctx.get_scoreboard(rhs_scoreboard),
                ),
                (
                    ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                    ScoreboardValue::Static(value),
                ) => {
                    // Convert the rhs to a scoreboard value
                    let player = self.scoreboard_ctx.get_temporary_player();
                    let scoreboard = self.scoreboard_ctx.get_scoreboard(Scoreboard::Main);
                    self.add_command(MinecraftCommand::ScoreboardSet {
                        player: player.clone(),
                        scoreboard: scoreboard.clone(),
                        value,
                    });
                    (lhs_id, lhs_scoreboard, player, scoreboard)
                }
            };

        // If lhs_id is not the same as target id, initialize it first
        if lhs_id != binary_operation.id {
            let player1 = self
                .scoreboard_ctx
                .get_scoreboard_player(binary_operation.id);
            let scoreboard1 = self
                .scoreboard_ctx
                .get_scoreboard(binary_operation.scoreboard);
            let player2 = self.scoreboard_ctx.get_scoreboard_player(lhs_id);
            let scoreboard2 = self.scoreboard_ctx.get_scoreboard(lhs_scoreboard);
            self.add_command(MinecraftCommand::ScoreboardSetEqual {
                player1,
                scoreboard1,
                player2,
                scoreboard2,
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
            player1,
            scoreboard1,
            player2: rhs_player,
            scoreboard2: rhs_scoreboard,
            operation: binary_operation.operation,
        })
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
                    prev.push_str("\n");
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
    scoreboard_players: HashMap<ScoreboardPlayer, Rc<String>>,
    scoreboards: HashMap<Scoreboard, Rc<String>>,
    scoreboard_prefix: String,
}

impl ScoreboardContext {
    fn new(scoreboard_prefix: String) -> Self {
        ScoreboardContext {
            scoreboard_prefix,
            ..Default::default()
        }
    }

    #[allow(clippy::map_entry)]
    fn get_scoreboard(&mut self, scoreboard: Scoreboard) -> Rc<String> {
        if !self.scoreboards.contains_key(&scoreboard) {
            self.scoreboards
                .insert(scoreboard, self.format_scoreboard(scoreboard));
        }
        self.scoreboards.get(&scoreboard).unwrap().clone()
    }

    fn get_scoreboard_player(&mut self, item_id: ItemId) -> Rc<String> {
        let num_players = self.scoreboard_players.len() as u64;
        self.scoreboard_players
            .entry(item_id.into())
            .or_insert_with(|| Self::format_player(num_players))
            .clone()
    }

    fn get_temporary_player(&mut self) -> Rc<String> {
        let length = self.scoreboard_players.len() as u64;
        self.scoreboard_players
            .entry(ScoreboardPlayer::Temporary(length))
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
enum ScoreboardPlayer {
    Normal(ItemId),
    Temporary(u64),
}

impl From<ItemId> for ScoreboardPlayer {
    fn from(value: ItemId) -> Self {
        ScoreboardPlayer::Normal(value)
    }
}
