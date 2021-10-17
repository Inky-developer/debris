use std::{borrow::Cow, fmt::Write, rc::Rc};

use debris_common::CompileContext;
use rustc_hash::FxHashMap;

use debris_llir::{
    llir_nodes::{
        BinaryOperation, Branch, Call, Condition, ExecuteRaw, ExecuteRawComponent, FastStore,
        FastStoreFromResult, Function, Node, WriteMessage,
    },
    utils::{BlockId, ScoreboardOperation, ScoreboardValue},
    CallGraph, Llir,
};
use vfs::Directory;

use crate::common::{
    ExecuteComponent, MinecraftCommand, MinecraftRange, ObjectiveCriterion, ScoreboardPlayer,
};

use super::{
    function_context::FunctionContext, json_formatter::format_json,
    scoreboard_constants::ScoreboardConstants, scoreboard_context::ScoreboardContext, Datapack,
};

/// This struct is used to generate a datapack from the llir representation
#[derive(Debug)]
pub struct DatapackGenerator<'a> {
    /// The compilation configuration
    compile_context: &'a CompileContext,
    /// The llir to compile
    llir: &'a Llir,
    /// Statistics for how often each function got called
    function_calls_stats: FxHashMap<BlockId, usize>,
    /// Contains the already generated functions
    function_ctx: FunctionContext,
    /// The current stack
    ///
    /// Commands are pushed into the last value of last context
    stack: Vec<Vec<MinecraftCommand>>,
    /// A context which keeps track of the currently used scoreboards
    scoreboard_ctx: ScoreboardContext,
    /// Keeps track of all constants that are used throughout the code
    scoreboard_constants: ScoreboardConstants,
}

impl<'a> DatapackGenerator<'a> {
    /// Adds a command to the current stack
    fn add_command(&mut self, command: MinecraftCommand) {
        // Check if the command stores the result of a complex condition.
        // Right now conditions are broken in minecraft, because complex conditions
        // can only return one, but sometimes not zero. For this reason,
        // a set to zero command must be issued before the subcommand gets evaluated.
        if let MinecraftCommand::ScoreboardSetFromResult {
            command: nested_command,
            player,
        } = &command
        {
            if let MinecraftCommand::Execute { parts, and_then } = &**nested_command {
                // I cannot figure out how to do the mutable declaration at the match!
                let mut parts = parts;
                let mut and_then = and_then;

                // This counts the amount of conditions. If there are more than 2,
                // minecrafts bug applies and it has to be worked around.
                let mut condition_count = 0;
                while condition_count < 2 {
                    condition_count += parts.iter().filter(|part| part.is_condition()).count();
                    if let Some(MinecraftCommand::Execute {
                        and_then: next_and_then,
                        parts: next_parts,
                    }) = and_then.as_deref()
                    {
                        parts = next_parts;
                        and_then = next_and_then;
                    } else {
                        break;
                    }
                }

                if condition_count >= 2 {
                    let player = player.clone();
                    self.add_command(MinecraftCommand::ScoreboardSet { player, value: 0 });
                }
            }
        }

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
    fn handle_main_function(&mut self, block_ids: impl Iterator<Item = BlockId>) -> bool {
        let name = "main".to_string();
        let id = self.function_ctx.register_custom_function();
        self.function_ctx.register_with_name(id, name);
        self.stack.push(Vec::new());

        self.stack.push(Vec::new());
        // Handle the main function
        for id in block_ids {
            self.handle_call(&Call { id });
        }
        let mut user_content = self.stack.pop().unwrap();
        {
            // Initialize all scoreboards
            let scoreboard_commands: Vec<_> = self
                .scoreboard_ctx
                .scoreboards
                .values()
                .flat_map(|scoreboard_name| {
                    std::array::IntoIter::new([
                        MinecraftCommand::ScoreboardRemove {
                            name: scoreboard_name.clone(),
                        },
                        MinecraftCommand::ScoreboardAdd {
                            name: scoreboard_name.clone(),
                            criterion: ObjectiveCriterion::Dummy,
                            json_name: None,
                        },
                    ])
                })
                .collect();
            for command in scoreboard_commands {
                self.add_command(command)
            }

            // Handle all constants
            for constant in self.scoreboard_constants.constants().collect::<Vec<_>>() {
                let player = self
                    .scoreboard_constants
                    .get_name(constant, &mut self.scoreboard_ctx);
                self.add_command(MinecraftCommand::ScoreboardSet {
                    player,
                    value: constant,
                });
            }
        }

        self.stack.last_mut().unwrap().append(&mut user_content);

        let nodes = self.stack.pop().unwrap();
        let generate = !nodes.is_empty();
        if generate {
            self.function_ctx.insert(id, nodes);
        }
        generate
    }

    /// Handles functions that run every tick
    fn handle_ticking_function(&mut self, block_ids: impl Iterator<Item = BlockId>) -> bool {
        let name = "tick".to_string();
        let id = self.function_ctx.register_custom_function();
        self.function_ctx.register_with_name(id, name);
        self.stack.push(Vec::new());

        // Handle the main function
        for id in block_ids {
            self.handle(&Node::Call(Call { id }));
        }

        let nodes = self.stack.pop().unwrap();
        let generate = !nodes.is_empty();
        if generate {
            self.function_ctx.insert(id, nodes);
        }
        generate
    }

    fn handle(&mut self, node: &Node) {
        match node {
            Node::FastStore(fast_store) => self.handle_fast_store(fast_store),
            Node::FastStoreFromResult(fast_store_from_result) => {
                self.handle_fast_store_from_result(fast_store_from_result)
            }
            Node::BinaryOperation(binop) => self.handle_binary_operation(binop),
            Node::Call(call) => self.handle_call(call),
            Node::Condition(condition) => self.handle_condition(condition),
            Node::Execute(execute) => self.handle_execute(execute),
            Node::Write(write) => self.handle_write(write),
            Node::Branch(branch) => self.handle_branch(branch),
            Node::Nop => (),
        }
    }

    // Node handlers

    fn handle_function(&mut self, function: &Function) {
        let id = self.function_ctx.register_function(function.id);
        self.stack.push(Vec::new());

        for node in function.nodes() {
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
        let mut inner_commands = self.catch_ouput(&fast_store_from_result.command);

        if let Some(last) = inner_commands.pop() {
            for command in inner_commands {
                self.add_command(command);
            }
            let command = MinecraftCommand::ScoreboardSetFromResult {
                player: ScoreboardPlayer {
                    player: self
                        .scoreboard_ctx
                        .get_scoreboard_player(fast_store_from_result.id),
                    scoreboard: self
                        .scoreboard_ctx
                        .get_scoreboard(fast_store_from_result.scoreboard),
                },
                command: Box::new(last),
            };
            self.add_command(command);
        } else {
            panic!("Expected at least one inner function, but got None",);
        }
    }

    fn handle_binary_operation(&mut self, binary_operation: &BinaryOperation) {
        // Calculate the lhs scoreboard value and the rhs scoreboard value
        let (lhs_player, lhs_scoreboard, rhs_player, rhs_scoreboard, overwrite_lhs) =
            match (binary_operation.lhs, binary_operation.rhs) {
                (ScoreboardValue::Static(lhs), ScoreboardValue::Static(rhs)) => {
                    let result = binary_operation.operation.evaluate(lhs, rhs);
                    let player = self
                        .scoreboard_ctx
                        .get_scoreboard_player(binary_operation.id);
                    let scoreboard = self
                        .scoreboard_ctx
                        .get_scoreboard(binary_operation.scoreboard);
                    let command = MinecraftCommand::ScoreboardSet {
                        player: ScoreboardPlayer { player, scoreboard },
                        value: result,
                    };
                    self.add_command(command);
                    return;
                }
                (
                    ScoreboardValue::Static(lhs_val),
                    ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_id),
                ) => {
                    if binary_operation.id == rhs_id {
                        let lhs_player = self.scoreboard_ctx.get_temporary_player();
                        let rhs_id = self.scoreboard_ctx.get_scoreboard_player(rhs_id);
                        let rhs_scoreboard = self.scoreboard_ctx.get_scoreboard(rhs_scoreboard);
                        self.add_command(MinecraftCommand::ScoreboardSet {
                            player: lhs_player.clone(),
                            value: lhs_val,
                        });
                        self.add_command(MinecraftCommand::ScoreboardOperation {
                            operation: binary_operation.operation,
                            player1: lhs_player,
                            player2: ScoreboardPlayer {
                                player: rhs_id,
                                scoreboard: rhs_scoreboard,
                            },
                        });
                        return;
                    }

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
                    let ScoreboardPlayer { player, scoreboard } = self
                        .scoreboard_constants
                        .get_name(value, &mut self.scoreboard_ctx);
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
        let condition = self.get_condition(condition, None);
        self.add_command(condition);
    }

    fn handle_branch(&mut self, branch: &Branch) {
        let pos_branch = self.catch_ouput(&branch.pos_branch);
        let pos_branch = self.get_as_single_command(pos_branch);
        let neg_branch = self.catch_ouput(&branch.neg_branch);
        let neg_branch = self.get_as_single_command(neg_branch);
        let condition = &branch.condition;

        if !condition.is_simple() && neg_branch.is_some() && pos_branch.is_some() {
            // If the condition is complex and both branches are run, evaluated the condition once and cache the result

            // If the condition is an or-condition (which is complex to evaluate), invert it and swap pos_branch and neg_branch
            let (pos_branch, neg_branch, condition) = if matches!(condition, Condition::Or(_)) {
                (neg_branch, pos_branch, Cow::Owned(condition.not()))
            } else {
                (pos_branch, neg_branch, Cow::Borrowed(condition))
            };

            let cached_condition = self.get_condition(&condition, None);
            let player = self.scoreboard_ctx.get_temporary_player();

            self.add_command(MinecraftCommand::ScoreboardSetFromResult {
                command: cached_condition.into(),
                player: player.clone(),
            });

            let pos_command = MinecraftCommand::Execute {
                parts: vec![ExecuteComponent::IfScoreRange {
                    player: player.clone(),
                    range: MinecraftRange::Equal(1),
                }],
                and_then: pos_branch.map(Box::new),
            };
            let neg_command = MinecraftCommand::Execute {
                parts: vec![ExecuteComponent::IfScoreRange {
                    player,
                    range: MinecraftRange::NotEqual(1),
                }],
                and_then: neg_branch.map(Box::new),
            };

            self.add_command(pos_command);
            self.add_command(neg_command);
        } else {
            // Otherwise evaluated both conditions individually
            if let Some(and_then) = pos_branch {
                let and_then_command = self.get_condition(condition, Some(and_then));
                self.add_command(and_then_command);
            }

            let condition = condition.not();
            if let Some(and_then) = neg_branch {
                let and_then_command = self.get_condition(&condition, Some(and_then));
                self.add_command(and_then_command);
            }
        };
    }

    fn handle_call(&mut self, call: &Call) {
        let num_calls = self.function_calls_stats[&call.id];

        // If the function only gets called once, just inline everything
        if num_calls == 1 {
            if let Some(function_id) = self.function_ctx.get_function_id(&call.id) {
                let ident = self.function_ctx.get_function_ident(function_id).unwrap();
                self.add_command(MinecraftCommand::Function { function: ident });
            } else {
                let function = self
                    .llir
                    .functions
                    .values()
                    .find(|func| func.id == call.id)
                    .expect("Missing function");
                for node in function.nodes() {
                    self.handle(node);
                }
            }
        } else {
            if self.function_ctx.get_function_id(&call.id).is_none() {
                self.function_ctx.register_function(call.id);
            }
            let function_id = self.function_ctx.get_function_id(&call.id).unwrap();
            let ident = self.function_ctx.get_function_ident(function_id).unwrap();
            self.add_command(MinecraftCommand::Function { function: ident });
        }
    }

    fn handle_execute(&mut self, execute: &ExecuteRaw) {
        let command = {
            let mut command = String::new();
            for part in execute.0.iter() {
                match part {
                    ExecuteRawComponent::String(val) => {
                        command.push_str(val);
                    }
                    ExecuteRawComponent::ScoreboardValue(val) => match val {
                        ScoreboardValue::Static(val) => {
                            command.push_str(&val.to_string());
                        }
                        ScoreboardValue::Scoreboard(scoreboard, id) => command.push_str(&format!(
                            "{} {}",
                            self.scoreboard_ctx.get_scoreboard_player(*id),
                            self.scoreboard_ctx.get_scoreboard(*scoreboard)
                        )),
                    },
                }
            }

            command
        };
        self.add_command(MinecraftCommand::RawCommand {
            command: command.into(),
        });
    }

    fn handle_write(&mut self, write: &WriteMessage) {
        let message = format_json(&write.message, &mut self.scoreboard_ctx);
        self.add_command(MinecraftCommand::JsonMessage {
            target: write.target,
            message,
        });
    }

    /// Evaluates this condition and, if it is true, calls and_then.
    /// Returns the command instead of adding it to the current stack.
    fn get_condition(
        &mut self,
        condition: &Condition,
        and_then: Option<MinecraftCommand>,
    ) -> MinecraftCommand {
        let mut parts = Vec::new();
        self.get_condition_inner(condition, &mut parts);
        MinecraftCommand::Execute {
            parts,
            and_then: and_then.map(Box::new),
        }
    }

    fn get_condition_inner(&mut self, condition: &Condition, parts: &mut Vec<ExecuteComponent>) {
        match condition {
            Condition::Compare {
                lhs,
                rhs,
                comparison,
            } => match (lhs, rhs) {
                (
                    ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                    ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_id),
                ) => parts.push(ExecuteComponent::IfScoreRelation {
                    comparison: *comparison,
                    player1: ScoreboardPlayer {
                        player: self.scoreboard_ctx.get_scoreboard_player(*lhs_id),
                        scoreboard: self.scoreboard_ctx.get_scoreboard(*lhs_scoreboard),
                    },
                    player2: ScoreboardPlayer {
                        player: self.scoreboard_ctx.get_scoreboard_player(*rhs_id),
                        scoreboard: self.scoreboard_ctx.get_scoreboard(*rhs_scoreboard),
                    },
                }),
                (ScoreboardValue::Static(lhs), ScoreboardValue::Static(rhs)) => {
                    let lhs = self
                        .scoreboard_constants
                        .get_name(*lhs, &mut self.scoreboard_ctx);
                    let rhs = self
                        .scoreboard_constants
                        .get_name(*rhs, &mut self.scoreboard_ctx);
                    parts.push(ExecuteComponent::IfScoreRelation {
                        comparison: *comparison,
                        player1: lhs,
                        player2: rhs,
                    })
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

                    parts.push(ExecuteComponent::IfScoreRange {
                        player: ScoreboardPlayer {
                            player: self.scoreboard_ctx.get_scoreboard_player(*id),
                            scoreboard: self.scoreboard_ctx.get_scoreboard(*scoreboard),
                        },
                        range: MinecraftRange::from_operator(*static_value, operator),
                    });
                }
            },
            Condition::And(conditions) => {
                for condition in conditions.iter() {
                    self.get_condition_inner(condition, parts);
                }
            }
            Condition::Or(conditions) => {
                // Negate all sub-conditions and then negate the result
                // (a || b) == !(!a && !b)
                let mut neg_parts = Vec::with_capacity(conditions.len());
                for condition in conditions {
                    self.get_condition_inner(&condition.not(), &mut neg_parts);
                }
                let neg_condition = MinecraftCommand::Execute {
                    and_then: None,
                    parts: neg_parts,
                };

                let temp_score = self.scoreboard_ctx.get_temporary_player();

                self.add_command(MinecraftCommand::ScoreboardSetFromResult {
                    player: temp_score.clone(),
                    command: Box::new(neg_condition),
                });

                parts.push(ExecuteComponent::IfScoreRange {
                    player: temp_score,
                    range: MinecraftRange::Equal(0),
                });
            }
        }
    }

    /// Converts a bunch of minecraft commands into a single command
    fn get_as_single_command(
        &mut self,
        commands: Vec<MinecraftCommand>,
    ) -> Option<MinecraftCommand> {
        match commands.as_slice() {
            [] => None,
            [_] => Some(commands.into_iter().next().unwrap()),
            _ => {
                let function = self.function_ctx.register_custom_function();
                self.function_ctx.insert(function, commands);
                Some(MinecraftCommand::Function {
                    function: self.function_ctx.get_function_ident(function).unwrap(),
                })
            }
        }
    }

    pub fn new(ctx: &'a CompileContext, llir: &'a Llir) -> Self {
        let function_namespace = Rc::from(ctx.config.project_name.to_lowercase());
        let scoreboard_ctx = ScoreboardContext::new(
            ctx.config.default_scoreboard_name.clone(),
            ctx.config.build_mode,
        );

        DatapackGenerator {
            compile_context: ctx,
            llir,
            function_calls_stats: llir.get_function_calls(),
            function_ctx: FunctionContext::new(function_namespace),
            stack: Default::default(),
            scoreboard_ctx,
            scoreboard_constants: Default::default(),
        }
    }

    pub fn build(mut self) -> Directory {
        let mut pack = Datapack::new(&self.compile_context.config);

        let mut call_graph = CallGraph::from(&self.llir.functions);
        for function in call_graph.iter_dfs(self.llir.runtime.root_blocks()) {
            // For aesthetics of the generated datapack, don't generate the root functions immediately,
            // but when they are handled on their own.
            if !self.llir.runtime.contains(&function) {
                let function = self.llir.functions.get(&function).unwrap();
                self.handle_function(function);
            }
        }

        let tick_json =
            self.handle_ticking_function(self.llir.runtime.scheduled_blocks.iter().copied());
        let load_json = self.handle_main_function(self.llir.runtime.load_blocks.iter().copied());

        if tick_json {
            pack.add_tick_json(&self.compile_context.config);
        }
        if load_json {
            pack.add_load_json(&self.compile_context.config);
        }

        let functions_dir = pack.functions();

        for function in self.function_ctx.functions() {
            let contents = function
                .commands
                .iter()
                .fold(String::new(), |mut prev, next| {
                    write!(prev, "{}", next).unwrap();
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
