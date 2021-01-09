use std::{collections::HashSet, rc::Rc};

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
        llir_nodes::{Branch, Condition, Node},
        utils::ScoreboardOperation,
        Llir,
    },
    mir::ContextId,
    CompileContext,
};
use vfs::Directory;

use crate::{
    common::MinecraftCommand,
    common::{ExecuteComponent, MinecraftRange, ObjectiveCriterion, ScoreboardPlayer},
    Backend,
};

use super::{
    function_context::FunctionContext, scoreboard_context::ScoreboardContext, stringify::Stringify,
    Datapack, ScoreboardConstants,
};

/// The Datapack Backend implementation
#[derive(Debug)]
pub struct DatapackBackend<'a> {
    /// The compilation configuration
    compile_context: &'a CompileContext,
    /// Contains the already generated functions
    function_ctx: FunctionContext,
    /// Contains functions that are pending to be visited
    missing_functions: HashSet<ContextId>,
    /// The current stack
    ///
    /// Commands are pushed into the last value of last context
    stack: Vec<Vec<MinecraftCommand>>,
    /// A context which keeps track of the currently used scoreboards
    scoreboard_ctx: ScoreboardContext,
    /// Keeps track of all constants that are used throughout the code
    scoreboard_constants: ScoreboardConstants,
}

impl DatapackBackend<'_> {
    /// Returns the scoreboard name and the scoreboard player for this constant
    fn get_constant(&mut self, value: i32) -> (Rc<str>, Rc<str>) {
        let scoreboard = Scoreboard::Main;
        let value = self.scoreboard_constants.get_name(value);
        (value, self.scoreboard_ctx.get_scoreboard(scoreboard))
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
    fn handle_main_function(&mut self, main_id: ContextId) {
        let name = "main".to_string();
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

            // Handle all constants
            for constant in self.scoreboard_constants.constants().collect::<Vec<_>>() {
                let (player, scoreboard) = self.get_constant(constant);
                self.add_command(MinecraftCommand::ScoreboardSet {
                    player: ScoreboardPlayer { player, scoreboard },
                    value: constant,
                });
            }

            // Handle the main function
            self.handle(&Node::Call(Call { id: main_id }));
        }

        let nodes = self.stack.pop().unwrap();
        self.function_ctx.insert_with_name(name, nodes);
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
            Node::Branch(branch) => self.handle_branch(branch),
        }
    }

    // Node handlers

    fn handle_function(&mut self, function: &Function) {
        let id = self.function_ctx.register_function(function.id);
        self.stack.push(Vec::new());

        for node in &function.nodes {
            self.handle(node);
        }

        let nodes = self.stack.pop().unwrap();
        self.function_ctx.insert(id, nodes);
    }

    fn handle_fast_store(&mut self, fast_store: &FastStore) {
        let value = &fast_store.value;

        let command = match value {
            ScoreboardValue::Static(static_value) => MinecraftCommand::ScoreboardSet {
                player: ScoreboardPlayer {
                    player: self.scoreboard_ctx.get_scoreboard_player(fast_store.id),
                    scoreboard: self.scoreboard_ctx.get_scoreboard(fast_store.scoreboard),
                },
                value: *static_value,
            },
            ScoreboardValue::Scoreboard(other_scoreboard, other_player) => {
                MinecraftCommand::ScoreboardSetEqual {
                    player1: ScoreboardPlayer {
                        player: self.scoreboard_ctx.get_scoreboard_player(fast_store.id),
                        scoreboard: self.scoreboard_ctx.get_scoreboard(fast_store.scoreboard),
                    },
                    player2: ScoreboardPlayer {
                        player: self.scoreboard_ctx.get_scoreboard_player(*other_player),
                        scoreboard: self.scoreboard_ctx.get_scoreboard(*other_scoreboard),
                    },
                }
            }
        };

        self.add_command(command);
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
                    || binary_operation.operation == ScoreboardOperation::Minus) =>
                {
                    let real_value = if binary_operation.operation == ScoreboardOperation::Minus {
                        -value
                    } else {
                        value
                    };

                    // let player = self.scoreboard_ctx.get_scoreboard_player(lhs_id);
                    // let scoreboard = self.scoreboard_ctx.get_scoreboard(lhs_scoreboard);

                    let (player, scoreboard) = if lhs_id == binary_operation.id {
                        (
                            self.scoreboard_ctx.get_scoreboard_player(lhs_id),
                            self.scoreboard_ctx.get_scoreboard(lhs_scoreboard),
                        )
                    } else {
                        // Convert the lhs to a new scoreboard value
                        let player = self
                            .scoreboard_ctx
                            .get_scoreboard_player(binary_operation.id);
                        let scoreboard = self
                            .scoreboard_ctx
                            .get_scoreboard(binary_operation.scoreboard);
                        let lhs_player = self.scoreboard_ctx.get_scoreboard_player(lhs_id);
                        let lhs_scoreboard = self.scoreboard_ctx.get_scoreboard(lhs_scoreboard);
                        self.add_command(MinecraftCommand::ScoreboardSetEqual {
                            player1: ScoreboardPlayer {
                                player: player.clone(),
                                scoreboard: scoreboard.clone(),
                            },
                            player2: ScoreboardPlayer {
                                player: lhs_player,
                                scoreboard: lhs_scoreboard,
                            },
                        });
                        (player, scoreboard)
                    };

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
                    let (rhs_player, rhs_scoreboard) = self.get_constant(value);
                    (
                        self.scoreboard_ctx.get_scoreboard_player(lhs_id),
                        self.scoreboard_ctx.get_scoreboard(lhs_scoreboard),
                        rhs_player,
                        rhs_scoreboard,
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
        let condition = self.get_condition(condition, None);
        self.add_command(condition);
    }

    fn handle_branch(&mut self, branch: &Branch) {
        let condition = &branch.condition;
        let and_then = self.catch_ouput(&branch.pos_branch);
        let and_then = self.get_as_single_command(and_then);
        let and_then_command = self.get_condition(condition, Some(and_then));
        self.add_command(and_then_command);

        if let Some(neg_branch) = branch.neg_branch.as_deref() {
            let condition = condition.not();
            let and_then = self.catch_ouput(neg_branch);
            let and_then = self.get_as_single_command(and_then);
            let and_then_command = self.get_condition(&condition, Some(and_then));
            self.add_command(and_then_command);
        }
    }

    fn handle_call(&mut self, call: &Call) {
        let command = MinecraftCommand::Function {
            function: self
                .function_ctx
                .get_function_with_id(call.id)
                .unwrap_or_else(|| {
                    self.missing_functions.insert(call.id);
                    let id = self.function_ctx.register_function(call.id);
                    self.function_ctx.get_function(id).unwrap()
                }),
        };
        self.add_command(command)
    }

    fn handle_execute(&mut self, execute: &Execute) {
        self.add_command(MinecraftCommand::RawCommand {
            command: execute.command.clone().into(),
        });
    }

    /// Evaluates this condition and, if it is true, calls and_then.
    /// Returns the command instead of adding it to the current stack
    fn get_condition(
        &mut self,
        condition: &Condition,
        and_then: Option<MinecraftCommand>,
    ) -> MinecraftCommand {
        match condition {
            Condition::Compare {
                lhs,
                rhs,
                comparison,
            } => match (lhs, rhs) {
                (
                    ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                    ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_id),
                ) => MinecraftCommand::Excute {
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
                    and_then: and_then.map(Box::new),
                },
                (ScoreboardValue::Static(_), ScoreboardValue::Static(_)) => {
                    panic!("Expected at least one scoreboard value")
                }
                (lhs, rhs) => {
                    // 0 < a can be rewritten as a > 0, so we can make a (scoreboard, static) pair
                    // This means we can make an expression of the form `scoreboard comparison static value`
                    let (scoreboard, id, static_value, operator) = match (lhs, rhs) {
                        (
                            ScoreboardValue::Static(static_value),
                            ScoreboardValue::Scoreboard(scoreboard, id),
                        ) => (scoreboard, id, static_value, comparison.flip_sides()),
                        (
                            ScoreboardValue::Scoreboard(scoreboard, id),
                            ScoreboardValue::Static(static_value),
                        ) => (scoreboard, id, static_value, *comparison),
                        (_, _) => unreachable!(
                            "Verified that there is one static and one scoreboard value"
                        ),
                    };

                    MinecraftCommand::Excute {
                        parts: vec![ExecuteComponent::IfScoreRange {
                            player: ScoreboardPlayer {
                                player: self.scoreboard_ctx.get_scoreboard_player(*id),
                                scoreboard: self.scoreboard_ctx.get_scoreboard(*scoreboard),
                            },
                            range: MinecraftRange::from_operator(*static_value, operator),
                        }],
                        and_then: and_then.map(Box::new),
                    }
                }
            },
            Condition::And { conditions } => {
                let mut big_condition = and_then;
                for condition in conditions.iter().rev() {
                    big_condition = Some(self.get_condition(condition, big_condition));
                }
                big_condition.expect("Expected at least one condition")
            }
        }
    }

    /// Converts a bunch of minecraft commands into a single command
    fn get_as_single_command(&mut self, commands: Vec<MinecraftCommand>) -> MinecraftCommand {
        if commands.len() == 1 {
            commands.into_iter().next().unwrap()
        } else {
            let function = self.function_ctx.register_custom_function();
            self.function_ctx.insert(function, commands);
            MinecraftCommand::Function {
                function: self.function_ctx.get_function(function).unwrap(),
            }
        }
    }
}

impl<'a> Backend<'a> for DatapackBackend<'a> {
    fn new(ctx: &'a CompileContext) -> Self {
        let function_namespace = Rc::from(ctx.config.project_name.to_lowercase());
        let scoreboard_ctx = ScoreboardContext::new(ctx.config.default_scoreboard_name.clone());
        DatapackBackend {
            scoreboard_ctx,
            compile_context: ctx,
            function_ctx: FunctionContext::new(function_namespace),
            missing_functions: Default::default(),
            stack: Default::default(),
            scoreboard_constants: Default::default(),
        }
    }

    fn handle_llir(&mut self, llir: &Llir) -> Directory {
        let mut pack = Datapack::new(&self.compile_context.config);

        // Assume the last function is the main function
        // Ignore the other functions unless they are called
        let main_function = &llir.functions.last().unwrap();
        self.handle_function(main_function);

        while !self.missing_functions.is_empty() {
            for function in &llir.functions {
                if self.missing_functions.contains(&function.id) {
                    self.missing_functions.remove(&function.id);
                    self.handle_function(function);
                }
            }
        }

        self.handle_main_function(main_function.id);

        let functions_dir = pack.functions();

        for function in self.function_ctx.functions() {
            let contents = function
                .commands
                .iter()
                .map(MinecraftCommand::stringify)
                .fold(String::new(), |mut prev, next| {
                    prev.push_str(&next);
                    prev.push('\n');
                    prev
                });
            functions_dir
                .file(function.get_filename())
                .push_string(&contents);
        }

        pack.dir
    }
}
