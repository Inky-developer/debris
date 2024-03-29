use std::{any::type_name, fmt};

use debris_common::{Config, OptMode};
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    block_id::BlockId,
    item_id::ItemId,
    llir_nodes::{Branch, Call, Condition, ExecuteRawComponent, Function, Node, VariableAccessMut},
    log,
    minecraft_utils::ScoreboardValue,
    opt::{
        code_stats::CodeStats,
        optimizers::{
            optimize_alias_function, optimize_call_chain, optimize_common_path,
            simple_arithmetic_optimization, ConstOptimizer, RedundancyOptimizer,
            RedundantCopyOptimizer,
        },
    },
    Runtime,
};

use super::{
    call_graph::{CallGraph, InfiniteLoopDetector},
    optimize_commands::{
        ExecuteRawUpdate, OptimizeCommand, OptimizeCommandDeque, OptimizeCommandKind,
    },
    variable_metadata::VariableUsage,
    NodeId,
};

/// Does optimization on the whole program.
///
/// This allows (along others) for removing unused commands
#[derive(Debug)]
pub struct GlobalOptimizer<'a> {
    pub config: &'a Config,
    pub runtime: &'a Runtime,
    pub functions: FxHashMap<BlockId, Function>,
}

impl<'a> GlobalOptimizer<'a> {
    pub fn new(
        config: &'a Config,
        runtime: &'a Runtime,
        functions: FxHashMap<BlockId, Function>,
    ) -> Self {
        GlobalOptimizer {
            config,
            runtime,
            functions,
        }
    }

    /// Runs the optimization passes and returns the final function map
    pub fn run(mut self) -> (FxHashMap<BlockId, Function>, CodeStats) {
        if matches!(self.config.opt_mode, OptMode::None) {
            let call_graph = CallGraph::from(&self.functions);
            let mut stats = CodeStats::new(call_graph);
            stats.update(self.runtime, &self.functions);
            (self.functions, stats)
        } else {
            let stats = self.optimize();
            (self.functions, stats)
        }
    }
}

/// Maximum amount of Turing-complete iterations the optimizer performs
const MAX_ITERATIONS: usize = 4096;

impl GlobalOptimizer<'_> {
    fn optimize(&mut self) -> CodeStats {
        log!("Logging optimization steps:");

        let mut const_optimizer = ConstOptimizer::default();
        let mut redundancy_optimizer = RedundancyOptimizer::default();
        let mut copy_optimizer = RedundantCopyOptimizer::default();

        let mut run_optimize_pass =
            |commands: &mut Commands, aggressive_function_inlining: bool| -> bool {
                redundancy_optimizer.aggressive_function_inlining = aggressive_function_inlining;

                // Run the copy optimizer first, because inlining functions
                // can actually make the copy optimizer not be able to optimize something
                let could_optimize = commands.run_optimizer(&mut copy_optimizer)
                    | commands.run_optimizer(&mut optimize_common_path)
                    | commands.run_optimizer(&mut redundancy_optimizer)
                    | commands.run_optimizer(&mut optimize_alias_function)
                    | commands.run_optimizer(&mut simple_arithmetic_optimization)
                    | commands.run_optimizer(&mut const_optimizer);

                if could_optimize {
                    commands.retain_functions();
                }

                could_optimize
            };

        let mut commands_deque = OptimizeCommandDeque::default();
        let call_graph = CallGraph::from(&self.functions);
        let mut variable_info = CodeStats::new(call_graph);

        let mut commands = Commands::new(self, &mut variable_info, &mut commands_deque);

        let mut iteration = 0;

        let mut aggressive_function_inlining = commands
            .optimizer
            .config
            .opt_mode
            .aggressive_function_inlining();
        log!("Iteration 0\n{commands:?}\n===========");
        let mut at_exit = false;
        loop {
            if iteration >= MAX_ITERATIONS {
                aggressive_function_inlining = false;
            }
            let could_optimize = run_optimize_pass(&mut commands, aggressive_function_inlining);
            log!("Iteration {}\n{commands:?}\n===========", iteration + 1);
            if could_optimize {
                at_exit = false;
            } else {
                if at_exit {
                    commands
                        .stats
                        .update(commands.optimizer.runtime, &commands.optimizer.functions);
                    commands.retain_functions();
                    return variable_info;
                }
                // Todo: Remove this
                // Temporary ways to check one last time that everything was optimized.
                // Recomputes all statistics.
                at_exit = true;
                commands
                    .stats
                    .update(commands.optimizer.runtime, &commands.optimizer.functions);
                commands.retain_functions();
            }

            if iteration == 0 {
                // The common path optimizer requires accurate information here
                commands
                    .stats
                    .update(commands.optimizer.runtime, &commands.optimizer.functions);
                commands.retain_functions();
                loop {
                    let inner_could_optimize = commands.run_optimizer(&mut optimize_common_path);
                    if !inner_could_optimize {
                        break;
                    }
                }
                commands.run_optimizer(&mut optimize_call_chain);
            }
            iteration += 1;
        }
    }

    pub fn get_function(&self, id: BlockId) -> &Function {
        self.functions.get(&id).unwrap()
    }

    pub fn get_function_mut(&mut self, id: BlockId) -> &mut Function {
        self.functions.get_mut(&id).unwrap()
    }

    /// Iterates over all functions
    pub fn iter_functions(&self) -> impl Iterator<Item = impl Iterator<Item = (NodeId, &Node)>> {
        self.functions.iter().map(|(block_id, function)| {
            function
                .nodes
                .iter()
                .enumerate()
                .map(move |(index, node)| ((*block_id, index), node))
        })
    }

    /// Iterates over all nodes.
    pub fn iter_nodes(&self) -> impl Iterator<Item = (NodeId, &Node)> + '_ {
        self.iter_functions().flatten()
    }

    /// Iterates all subsequent nodes of this function
    pub fn iter_at(&self, item: &NodeId) -> impl Iterator<Item = (NodeId, &Node)> + '_ {
        let function = self.functions.get(&item.0).expect("Invalid function");
        function
            .nodes()
            .iter()
            .enumerate()
            .skip(item.1 + 1)
            .map(move |(index, node)| ((function.id, index), node))
    }

    pub fn previous_node(&self, node_id: &NodeId) -> Option<(NodeId, &Node)> {
        let index = node_id.1;
        if index == 0 {
            return None;
        }
        let new_index = index - 1;

        let function = self.get_function(node_id.0);
        Some(((node_id.0, new_index), &function.nodes[new_index]))
    }
}

/// Interface for optimizing functions to get data about the code and emit
/// optimization instructions
pub struct Commands<'opt, 'ctx> {
    pub optimizer: &'opt mut GlobalOptimizer<'ctx>,
    pub stats: &'opt mut CodeStats,
    pub infinite_loop_detector: InfiniteLoopDetector,
    pub commands: &'opt mut OptimizeCommandDeque<OptimizeCommand>,
}

impl<'opt, 'ctx> Commands<'opt, 'ctx> {
    fn new(
        optimizer: &'opt mut GlobalOptimizer<'ctx>,
        stats: &'opt mut CodeStats,
        commands: &'opt mut OptimizeCommandDeque<OptimizeCommand>,
    ) -> Self {
        let mut commands = Commands {
            optimizer,
            stats,
            commands,
            infinite_loop_detector: Default::default(),
        };
        commands
            .stats
            .update(commands.optimizer.runtime, &commands.optimizer.functions);

        commands.retain_functions();

        commands
    }

    fn retain_functions(&mut self) {
        let stats = &self.stats;
        self.optimizer
            .functions
            .retain(|k, _| stats.function_calls.get(k).copied().unwrap_or(0) != 0);
    }

    // returns whether a variable is unused
    pub fn is_id_unused(&self, id: ItemId, node: NodeId) -> bool {
        self.get_reads(id) == 0
            || ((!self.stats.function_parameters.is_dependency(id))
                && !self
                    .optimizer
                    .iter_at(&node)
                    .any(|(_, node)| node.reads_from(id)))
    }

    /// Returns the variable info for this node
    pub fn get_info(&self, var: ItemId) -> &VariableUsage {
        self.stats
            .variable_information
            .get(&var)
            .expect("Unknown variable")
    }

    pub fn get_reads(&self, var: ItemId) -> usize {
        self.stats
            .variable_information
            .get(&var)
            .map_or(0, |usage| usage.reads)
    }

    pub fn get_call_count(&self, function: BlockId) -> usize {
        *self.stats.function_calls.get(&function).unwrap_or(&0)
    }

    /// Runs an optimizing function
    ///
    /// Returns whether this function could optimize anything
    fn run_optimizer<F>(&mut self, optimizer: &mut F) -> bool
    where
        F: Optimizer,
    {
        optimizer.optimize(self);
        let len = self.commands.len();
        log!(
            "{}: {:?}",
            type_name::<F>().split("::").last().unwrap(),
            self.commands
        );
        self.execute_commands();
        len > 0
    }

    /// Execute every command that is in the current command stack
    fn execute_commands(&mut self) {
        while let Some(command) = self.commands.pop_front() {
            let id = command.id;
            // Don't do anything for commands that effect nodes of unused functions
            if self.get_call_count(id.0) == 0 {
                continue;
            }
            match command.kind {
                OptimizeCommandKind::Delete => {
                    // Shifts back all following nodes so that the ids still match
                    self.commands
                        .iter_mut()
                        .filter(|cmd| cmd.id.0 == id.0 && cmd.id.1 > id.1)
                        .for_each(OptimizeCommand::shift_back);
                    // Removes the node that was scheduled to be deleted
                    let node = self
                        .optimizer
                        .functions
                        .get_mut(&id.0)
                        .unwrap()
                        .nodes
                        .remove(id.1);
                    self.stats.remove_node(&node, &id);
                }
                OptimizeCommandKind::DiscardResult => {
                    let nodes = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes;
                    let old_value = std::mem::replace(&mut nodes[id.1], Node::Nop);
                    self.stats.remove_node(&old_value, &id);
                    nodes[id.1] = match old_value {
                        Node::FastStoreFromResult(store) => {
                            self.stats.add_node(&store.command, &id);
                            *store.command
                        }
                        other => panic!("Invalid node: {other:?}"),
                    }
                }
                OptimizeCommandKind::InlineBranch(condition) => {
                    let nodes = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes;
                    let old_value = std::mem::replace(&mut nodes[id.1], Node::Nop);
                    self.stats.remove_node(&old_value, &id);
                    let new_node = match old_value {
                        Node::Branch(Branch {
                            condition: _,
                            pos_branch,
                            neg_branch,
                        }) => *if condition { pos_branch } else { neg_branch },
                        other => panic!("Invalid node: {other:?}"),
                    };

                    self.stats.add_node(&new_node, &id);
                    nodes[id.1] = new_node;
                }
                OptimizeCommandKind::UpdateBranch {
                    branch: branch_selector,
                    new_node,
                } => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    let Node::Branch(branch) = node else {
                        unreachable!()
                    };
                    let branch_node = if branch_selector {
                        branch.pos_branch.as_mut()
                    } else {
                        branch.neg_branch.as_mut()
                    };
                    self.stats.remove_node(branch_node, &id);
                    *branch_node = new_node;
                    self.stats.add_node(branch_node, &id);
                }
                OptimizeCommandKind::UpdateBranchCondition(new_condition) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    self.stats.remove_node(node, &id);
                    if let Node::Branch(branch) = node {
                        branch.condition = new_condition;
                    } else {
                        unreachable!("Must be a branch!");
                    }
                    self.stats.add_node(node, &id);
                }
                OptimizeCommandKind::InlineFunction => {
                    let node = &self.optimizer.functions.get(&id.0).unwrap().nodes[id.1];
                    let inlined_function_id = match node {
                        Node::Call(Call { id }) => *id,
                        other => unreachable!("Invalid node: {:?}", other),
                    };

                    // SAFETY: The call will always be removed
                    self.stats.remove_node(node, &id);

                    // If this is the only call to this function, it can be consumed safely.
                    let consume = self.get_call_count(inlined_function_id) == 0;
                    let new_nodes = match consume {
                        true => {
                            self.optimizer
                                .functions
                                .remove(&inlined_function_id)
                                .unwrap()
                                .nodes
                        }
                        false => self
                            .optimizer
                            .functions
                            .get(&inlined_function_id)
                            .unwrap()
                            .nodes
                            .clone(),
                    };

                    // SAFETY: See above
                    for (rel_index, node) in new_nodes.iter().enumerate() {
                        let index = rel_index + id.1;
                        let node_id = (id.0, index);
                        self.stats.add_node(node, &node_id);
                    }

                    let nodes = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes;
                    nodes.splice(id.1..=id.1, new_nodes.into_iter());
                }
                OptimizeCommandKind::RemoveAliasFunction(aliased_function) => {
                    // For each node, update any reference to this function
                    for function in self.optimizer.functions.values_mut() {
                        for node in &mut function.nodes {
                            node.scan_mut(&mut |inner| {
                                if let Node::Call(Call { id: target_id }) = inner {
                                    if target_id == &id.0 {
                                        *target_id = aliased_function;
                                    }
                                }
                            });
                        }
                    }
                    // Since there is no sane way to do incremental updates,
                    // just recompute all stats here :(
                    self.stats
                        .update(self.optimizer.runtime, &self.optimizer.functions);
                    self.stats.call_graph.update(&self.optimizer.functions);
                    self.retain_functions();
                }
                OptimizeCommandKind::ChangeWrite(new_target) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    self.stats.remove_node(node, &id);
                    node.variable_accesses_mut(&mut |access| match access {
                        VariableAccessMut::Write(value, _)
                        | VariableAccessMut::ReadWrite(ScoreboardValue::Scoreboard(value)) => {
                            *value = new_target;
                        }
                        _ => {}
                    });
                    self.stats.add_node(node, &id);
                }
                OptimizeCommandKind::ChangeReads(old_id, new_value) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    self.stats.remove_node(node, &id);
                    node.variable_accesses_mut(&mut |access| match access {
                        VariableAccessMut::Read(value) | VariableAccessMut::ReadWrite(value) => {
                            if let ScoreboardValue::Scoreboard(id) = value {
                                if id == &old_id {
                                    *value = new_value;
                                }
                            }
                        }

                        VariableAccessMut::Write(_, _) => {}
                    });
                    self.stats.add_node(node, &id);
                }
                OptimizeCommandKind::SetCondition(condition, indices) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    let mut old_condition = match node {
                        Node::Branch(branch) => &mut branch.condition,
                        Node::FastStoreFromResult(store) => match store.command.as_mut() {
                            Node::Condition(condition) => condition,
                            other => unreachable!("Invalid command: {:?}", other),
                        },
                        other => panic!("Invalid node: {other:?}"),
                    };

                    for index in indices.into_iter().rev() {
                        old_condition = match old_condition {
                            Condition::And(values) | Condition::Or(values) => &mut values[index],
                            Condition::Compare { .. } => unreachable!(),
                        }
                    }

                    // SAFETY: Treating the condition as a separate node does not
                    // have any effect on the variable reads
                    let new_condition_node = Node::Condition(condition);
                    self.stats.add_node(&new_condition_node, &id);

                    let Node::Condition(new_condition) = new_condition_node else {
                        unreachable!()
                    };
                    let old_condition = std::mem::replace(old_condition, new_condition);
                    let old_condition_node = Node::Condition(old_condition);
                    self.stats.remove_node(&old_condition_node, &id);
                }
                OptimizeCommandKind::Replace(new_node) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    self.stats.remove_node(node, &id);
                    self.stats.add_node(&new_node, &id);
                    *node = new_node;
                }
                OptimizeCommandKind::InsertAfter(next_node) => {
                    self.stats.add_node(&next_node, &(id.0, id.1 + 1));
                    // Shift forward all following nodes
                    self.commands
                        .iter_mut()
                        .filter(|cmd| cmd.id.0 == id.0 && cmd.id.1 > id.1)
                        .for_each(OptimizeCommand::shift_forward);
                    self.optimizer
                        .get_function_mut(id.0)
                        .nodes
                        .insert(id.1 + 1, next_node);
                }
                OptimizeCommandKind::UpdateExecuteRaw(index, update) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    self.stats.remove_node(node, &id);
                    let Node::Execute(execute_raw) = node else {
                        unreachable!()
                    };
                    match update {
                        ExecuteRawUpdate::Replace(new_node) => {
                            if let ExecuteRawComponent::Node(old_node) = &mut execute_raw.0[index] {
                                *old_node = new_node;
                            } else {
                                unreachable!();
                            }
                        }
                        ExecuteRawUpdate::Delete => {
                            execute_raw.0.remove(index);
                        }
                    }
                    self.stats.add_node(
                        &self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1],
                        &id,
                    );
                }
            }
        }
    }
}

impl fmt::Debug for Commands<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_fn(f: &mut fmt::Formatter<'_>, func: &Function) -> fmt::Result {
            write!(f, "{func}")
        }

        let mut functions = self.optimizer.functions.iter().collect_vec();
        functions.sort_unstable_by_key(|(id, _)| id.0);

        for (_, func) in functions {
            write_fn(f, func)?;
        }

        Ok(())
    }
}

pub trait Optimizer {
    fn optimize(&mut self, commands: &mut Commands);
}

impl<F> Optimizer for F
where
    F: Fn(&mut Commands),
{
    fn optimize(&mut self, commands: &mut Commands) {
        (self)(commands);
    }
}
