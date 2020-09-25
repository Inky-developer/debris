use std::{collections::HashMap, rc::Rc};

use debris_core::{
    llir::llir_nodes::Call,
    llir::llir_nodes::FastStoreFromResult,
    llir::utils::Scoreboard,
    llir::utils::ScoreboardValue,
    llir::{llir_nodes::FastStore, llir_nodes::Function, llir_nodes::Node, utils::ItemId, LLIR},
    Config,
};
use vfs::Directory;

use crate::{common::FunctionIdent, common::MinecraftCommand, common::ObjectiveCriterion, Backend};

use super::{stringify::stringify_command, Datapack};

#[derive(Default)]
pub struct DatapackBackend {
    config: Rc<Config>,
    function_namespace: Rc<String>,
    functions: HashMap<String, Vec<MinecraftCommand>>,
    function_identifiers: HashMap<u64, Rc<FunctionIdent>>,
    stack: Vec<Vec<MinecraftCommand>>,
    scoreboard_values: HashMap<ItemId, Rc<String>>,
    scoreboards: HashMap<Scoreboard, Rc<String>>,
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

    fn get_scoreboard_player_for_id(&mut self, id: ItemId) -> Rc<String> {
        if self.scoreboard_values.contains_key(&id) {
            self.scoreboard_values.get(&id).unwrap().clone()
        } else {
            self.scoreboard_values.insert(
                id,
                Rc::new(format!(".val_{}", self.scoreboard_values.len())),
            );
            self.scoreboard_values.get(&id).unwrap().clone()
        }
    }

    fn get_scoreboard_name(&mut self, scoreboard: Scoreboard) -> Rc<String> {
        if self.scoreboards.contains_key(&scoreboard) {
            self.scoreboards.get(&scoreboard).unwrap().clone()
        } else {
            self.scoreboards.insert(
                scoreboard,
                Rc::new(match scoreboard {
                    Scoreboard::Main => format!("{}", self.config.default_scoreboard_name),
                    Scoreboard::Custom(id) => {
                        format!("{}.{}", self.config.default_scoreboard_name, id)
                    }
                }),
            );
            self.scoreboards.get(&scoreboard).unwrap().clone()
        }
    }

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
            Node::Call(call) => self.handle_call(call),
            _ => todo!(),
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
                    player: self.get_scoreboard_player_for_id(fast_store.id),
                    scoreboard: self.get_scoreboard_name(fast_store.scoreboard),
                    value: *static_value,
                };
                self.add_command(command)
            }
            ScoreboardValue::Scoreboard(other_scoreboard, other_player) => {
                let command = MinecraftCommand::ScoreboardSetEqual {
                    player1: self.get_scoreboard_player_for_id(fast_store.id),
                    scoreboard1: self.get_scoreboard_name(fast_store.scoreboard),
                    player2: self.get_scoreboard_player_for_id(*other_player),
                    scoreboard2: self.get_scoreboard_name(*other_scoreboard),
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
                    .get_scoreboard_player_for_id(fast_store_from_result.id)
                    .clone(),
                scoreboard: self
                    .get_scoreboard_name(fast_store_from_result.scoreboard)
                    .clone(),
                command: Box::new(inner_commands.into_iter().nth(0).unwrap()),
            };
            self.add_command(command);
        } else {
            panic!("Expected only one inner funciton");
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
}

impl Backend for DatapackBackend {
    fn new(config: Rc<Config>) -> Self {
        let function_namespace = Rc::new(config.project_name.to_lowercase());
        DatapackBackend {
            config,
            function_namespace,
            ..Default::default()
        }
    }

    fn handle_llir(&mut self, llir: &LLIR) -> Directory {
        let mut pack = Datapack::new(&self.config);

        // Assume the first function is the main function
        // Ignore the other functions unless they are called
        let ref function = llir.functions[0];

        self.handle_function(function);
        self.handle_main_function(function.id);

        let functions = pack.functions();

        for (fn_name, fn_proto) in &self.functions {
            let contents = fn_proto
                .into_iter()
                .map(|cmd| stringify_command(&cmd))
                .fold(String::new(), |mut prev, next| {
                    prev.push_str(&next);
                    prev.push_str("\n");
                    prev
                });
            functions.file(fn_name.clone()).push_string(&contents);
        }

        pack.dir
    }
}
