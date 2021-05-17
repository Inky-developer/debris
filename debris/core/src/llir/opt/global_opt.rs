use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, VecDeque},
    ops::{Deref, DerefMut, Mul},
};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    llir::{
        llir_impl::LlirFunction,
        llir_nodes::{
            BinaryOperation, Branch, Call, Condition, FastStore, FastStoreFromResult, Function,
            Node, VariableAccess, VariableAccessMut,
        },
        utils::{
            BlockId, ItemId, Scoreboard, ScoreboardComparison, ScoreboardOperation, ScoreboardValue,
        },
        Runtime,
    },
    Config, OptMode,
};

use super::{
    call_graph::{CallGraph, InfiniteLoopDetector},
    function_parameters::FunctionParameters,
    variable_metadata::{Hint, ValueHints, VariableUsage},
};

/// If true prints some debug information to stdout
const DEBUG: bool = false;

/// Does optimization on the whole program.
///
/// This allows (along others) for removing unused commands
#[derive(Debug)]
pub struct GlobalOptimizer<'a> {
    config: &'a Config,
    runtime: &'a Runtime,
    functions: FxHashMap<BlockId, Function>,
    main_function: BlockId,
}

impl<'a> GlobalOptimizer<'a> {
    pub(crate) fn new(
        config: &'a Config,
        runtime: &'a Runtime,
        functions: FxHashMap<BlockId, LlirFunction>,
        main_function: BlockId,
    ) -> Self {
        GlobalOptimizer {
            config,
            runtime,
            functions: functions
                .into_iter()
                .map(|(id, func)| {
                    (
                        id,
                        Function {
                            id,
                            nodes: func.nodes.take(),
                            returned_value: func.returned_value,
                        },
                    )
                })
                .collect(),
            main_function,
        }
    }

    /// Runs the optimization passes and returns the final function map
    pub fn run(mut self) -> FxHashMap<BlockId, Function> {
        if !matches!(self.config.opt_mode, OptMode::None) {
            self.optimize();
        }
        self.functions
    }
}

impl GlobalOptimizer<'_> {
    fn optimize(&mut self) {
        let mut const_optimizer = ConstOptimizer::default();
        let mut redundancy_optimizer = RedundancyOptimizer::default();
        let mut copy_optimizer = RedundantCopyOptimizer::default();

        let mut run_optimize_pass =
            |commands: &mut Commands, aggressive_function_inlining: bool| -> bool {
                redundancy_optimizer.aggressive_function_inlining = aggressive_function_inlining;

                // Run the copy optimizer first, because inlining functions
                // can actually make the copy optimizer not be able to optimize something
                let could_optimize = commands.run_optimizer(&mut copy_optimizer)
                    | commands.run_optimizer(&mut redundancy_optimizer)
                    | commands.run_optimizer(&mut alias_function_optimizer)
                    | commands.run_optimizer(&mut simple_arithmetic_optimization)
                    | commands.run_optimizer(&mut const_optimizer);

                if could_optimize {
                    commands.retain_functions();
                }

                could_optimize
            };

        let mut commands_deque = OptimizeCommandDeque::new();
        let call_graph = CallGraph::from(&self.functions);
        let mut variable_info = CodeStats::new(call_graph);

        let mut commands = Commands::new(self, &mut variable_info, &mut commands_deque);

        const MAX_ITERATIONS: usize = 4096;
        let mut iteration = 0;

        let mut aggressive_function_inlining = commands
            .optimizer
            .config
            .opt_mode
            .aggressive_function_inlining();
        let mut at_exit = false;
        loop {
            // Print debug representation of llir
            if DEBUG {
                use itertools::Itertools;
                use std::fmt::Write;

                let mut buf = String::new();
                let fmt_function = |func: &Function, buf: &mut String| {
                    buf.write_fmt(format_args!(
                        "({} call(s)) - {}",
                        commands.get_call_count(&func.id),
                        func
                    ))
                    .unwrap()
                };

                for (_, function) in commands
                    .optimizer
                    .functions
                    .iter()
                    .sorted_by_key(|(_, func)| func.id)
                {
                    fmt_function(&function, &mut buf);
                    buf.push('\n');
                }
                println!("{}", buf);
            }
            if iteration >= MAX_ITERATIONS {
                aggressive_function_inlining = false;
            }
            let could_optimize = run_optimize_pass(&mut commands, aggressive_function_inlining);
            if !could_optimize {
                if at_exit {
                    return;
                }
                // Todo: Remove this
                // Temporary ways to check one last time that everything was optimized.
                // Recomputes all statistics.
                at_exit = true;
                commands
                    .stats
                    .update(commands.optimizer.runtime, &commands.optimizer.functions);
                commands.retain_functions();
            } else {
                at_exit = false;
            }

            if iteration == 0 {
                // The common path optimizer only runs once, so it requires
                // accurate stats information
                commands
                    .stats
                    .update(commands.optimizer.runtime, &commands.optimizer.functions);
                commands.retain_functions();
                // These optimizers only run once
                loop {
                    let could_optimize = commands.run_optimizer(&mut optimize_common_path);
                    if !could_optimize {
                        break;
                    }
                }
                commands.run_optimizer(&mut optimize_call_chain);
            }
            iteration += 1;
        }
    }

    fn get_function(&self, id: &BlockId) -> &Function {
        self.functions.get(id).unwrap()
    }

    fn get_function_mut(&mut self, id: &BlockId) -> &mut Function {
        self.functions.get_mut(id).unwrap()
    }

    /// Iterates over all functions
    fn iter_functions(&self) -> impl Iterator<Item = impl Iterator<Item = (NodeId, &Node)>> {
        self.functions.iter().map(|(block_id, function)| {
            function
                .nodes
                .iter()
                .enumerate()
                .map(move |(index, node)| ((*block_id, index), node))
        })
    }

    /// Iterates over all nodes.
    fn iter_nodes(&self) -> impl Iterator<Item = (NodeId, &Node)> + '_ {
        self.iter_functions().flatten()
    }

    /// Iterates all subsequent nodes of this function
    fn iter_at(&self, item: &NodeId) -> impl Iterator<Item = (NodeId, &Node)> + '_ {
        let function = self.functions.get(&item.0).expect("Invalid function");
        function
            .nodes()
            .iter()
            .enumerate()
            .skip(item.1 + 1)
            .map(move |(index, node)| ((function.id, index), node))
    }

    fn previous_node(&self, node_id: &NodeId) -> Option<(NodeId, &Node)> {
        let index = node_id.1;
        if index == 0 {
            return None;
        }
        let new_index = index - 1;

        let function = self.get_function(&node_id.0);
        Some(((node_id.0, new_index), &function.nodes[new_index]))
    }
}

/// The optimizer can uniquely identify each node with this type
type NodeId = (BlockId, usize);

/// Optimizing functions output commands that tell the optimizer what to do,
/// this is done so that there are no troubles with mutability
#[derive(Debug)]
enum OptimizeCommandKind {
    /// Deletes a single node
    Delete,
    /// Converts the [FastStoreFromResult] node into its command, discarding
    /// the result
    DiscardResult,
    /// Discards the node and only keeps the branch that matches the bool.
    /// (true => pos_branch, false => neg_branch)
    InlineBranch(bool),
    /// Updates the specified branch with the new node
    UpdateBranch { branch: bool, new_node: Node },
    /// Inlines the function of this function call.
    /// The bool specifies, whether the inlined function can be consumed.
    /// Otherwise, the function will be cloned.
    InlineFunction(bool),
    /// Removes all aliases to a function which only redirects to another function
    /// The argument specifies the aliased function.
    RemoveAliasFunction(BlockId),
    /// Changes the variable this node writes to
    ChangeWrite(ItemId),
    /// Replaces all variables `.0` with `.1`
    ChangeReads(ItemId, ScoreboardValue),
    /// Changes the condition of this branch to a new condition
    /// Vec usize contains the exact index of the condition to replace
    /// (Vec since conditions can be nested)
    SetCondition(Condition, Vec<usize>),
    /// Replaces the old node completely
    Replace(Node),
    /// Inserts this node after
    InsertAfter(Node),
}

#[derive(Debug)]
struct OptimizeCommand {
    id: NodeId,
    kind: OptimizeCommandKind,
}

impl OptimizeCommand {
    fn new(id: NodeId, kind: OptimizeCommandKind) -> Self {
        OptimizeCommand { id, kind }
    }

    /// Shifts the node id of this command one back
    fn shift(&mut self, amt: i8) {
        match amt.cmp(&0) {
            Ordering::Greater => self.id.1 += amt as usize,
            Ordering::Less => self.id.1 -= amt.abs() as usize,
            Ordering::Equal => {}
        }
    }

    fn shift_back(&mut self) {
        self.shift(-1)
    }
    fn shift_forward(&mut self) {
        self.shift(1)
    }
}

/// Just a wrapper arround deque with a simple push method
#[derive(Debug)]
struct OptimizeCommandDeque<T>(VecDeque<T>);

impl<T> OptimizeCommandDeque<T> {
    fn new() -> Self {
        OptimizeCommandDeque(Default::default())
    }

    fn push(&mut self, value: T) {
        self.0.push_back(value);
    }
}

impl<T> Deref for OptimizeCommandDeque<T> {
    type Target = VecDeque<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for OptimizeCommandDeque<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Default for OptimizeCommandDeque<T> {
    fn default() -> Self {
        OptimizeCommandDeque(VecDeque::default())
    }
}

/// Interface for optimizing functions to get data about the code and emit
/// optimization instructions
struct Commands<'opt, 'ctx> {
    optimizer: &'opt mut GlobalOptimizer<'ctx>,
    stats: &'opt mut CodeStats,
    infinite_loop_detector: InfiniteLoopDetector,
    commands: &'opt mut OptimizeCommandDeque<OptimizeCommand>,
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
    fn is_id_unused(&self, id: &ItemId, node: &NodeId) -> bool {
        self.get_reads(id) == 0
            || ((!self.stats.function_parameters.is_dependency(*id))
                && !self
                    .optimizer
                    .iter_at(node)
                    .any(|(_, node)| node.reads_from(id)))
    }

    /// Returns the variable info for this node
    fn get_info(&self, var: &ItemId) -> &VariableUsage {
        self.stats
            .variable_information
            .get(var)
            .expect("Unknown variable")
    }

    fn get_reads(&self, var: &ItemId) -> usize {
        self.stats
            .variable_information
            .get(var)
            .map_or(0, |usage| usage.reads)
    }

    fn get_call_count(&self, function: &BlockId) -> usize {
        *self.stats.function_calls.get(function).unwrap_or(&0)
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
        if DEBUG {
            // println!("{:?}", self.stats.variable_information);
            println!("{:?}", self.commands);
        }
        self.execute_commands();
        len > 0
    }

    /// Execute every command that is in the current command stack
    fn execute_commands(&mut self) {
        while let Some(command) = self.commands.pop_front() {
            let id = command.id;
            // Don't do anything for commands that effect nodes of unused functions
            if !self.stats.visited_functions.contains(&id.0) {
                continue;
            }
            match command.kind {
                OptimizeCommandKind::Delete => {
                    // Shifts back all following nodes so that the ids still match
                    self.commands
                        .iter_mut()
                        .filter(|cmd| cmd.id.0 == id.0 && cmd.id.1 > id.1)
                        .for_each(|cmd| cmd.shift_back());
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
                        other => panic!("Invalid node: {:?}", other),
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
                        other => panic!("Invalid node: {:?}", other,),
                    };

                    self.stats.add_node(&new_node, &id);
                    nodes[id.1] = new_node;
                }
                OptimizeCommandKind::UpdateBranch {
                    branch: branch_selector,
                    new_node,
                } => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    let branch = if let Node::Branch(branch) = node {
                        branch
                    } else {
                        unreachable!()
                    };
                    let node = if branch_selector {
                        branch.pos_branch.as_mut()
                    } else {
                        branch.neg_branch.as_mut()
                    };
                    self.stats.remove_node(node, &id);
                    *node = new_node;
                    self.stats.add_node(node, &id);
                }
                OptimizeCommandKind::InlineFunction(consume) => {
                    let node = &self.optimizer.functions.get(&id.0).unwrap().nodes[id.1];
                    let inlined_function_id = match node {
                        Node::Call(Call { id }) => *id,
                        other => unreachable!("Invalid node: {:?}", other),
                    };

                    // SAFETY: The call will always be removed
                    self.stats.remove_node(node, &id);

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
                        for node in function.nodes.iter_mut() {
                            node.iter_mut(&mut |inner| {
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
                        VariableAccessMut::Write(value)
                        | VariableAccessMut::ReadWrite(ScoreboardValue::Scoreboard(_, value)) => {
                            *value = new_target
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
                            if let ScoreboardValue::Scoreboard(_, id) = value {
                                if id == &old_id {
                                    *value = new_value
                                }
                            }
                        }

                        _ => {}
                    });
                    self.stats.add_node(node, &id);
                }
                OptimizeCommandKind::SetCondition(condition, indices) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    match node {
                        Node::Branch(branch) => {
                            let mut old_condition = &mut branch.condition;
                            for index in indices.into_iter().rev() {
                                old_condition = match old_condition {
                                    Condition::And(values) | Condition::Or(values) => {
                                        &mut values[index]
                                    }
                                    _ => unreachable!(),
                                }
                            }

                            // SAFETY: Treating the condition as a separate node does not
                            // have any effect on the variable reads
                            let new_condition_node = Node::Condition(condition);
                            self.stats.add_node(&new_condition_node, &id);

                            let new_condition = if let Node::Condition(cond) = new_condition_node {
                                cond
                            } else {
                                unreachable!()
                            };
                            let old_condition = std::mem::replace(old_condition, new_condition);
                            let old_condition_node = Node::Condition(old_condition);
                            self.stats.remove_node(&old_condition_node, &id);
                        }
                        other => panic!("Invalid node: {:?}", other),
                    }
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
                        .for_each(|cmd| cmd.shift_forward());
                    self.optimizer
                        .get_function_mut(&id.0)
                        .nodes
                        .insert(id.1 + 1, next_node);
                }
            }
        }
    }
}

trait Optimizer {
    fn optimize(&mut self, commands: &mut Commands);
}

impl<F> Optimizer for F
where
    F: Fn(&mut Commands),
{
    fn optimize(&mut self, commands: &mut Commands) {
        (self)(commands)
    }
}

/// Inlines the function call chain.
/// This optimizer only runs once, at the start.
fn optimize_call_chain(commands: &mut Commands) {
    // Cannot inline a function which has a call that was already inline without updating stats
    let mut encountered_functions = FxHashSet::default();
    encountered_functions.reserve(commands.optimizer.functions.len());
    for id in commands
        .stats
        .call_graph
        .iter_dfs(commands.optimizer.runtime.root_blocks())
    {
        // If this function only directs to another function, inline this function
        let function = commands.optimizer.get_function(&id);
        if let Some(Node::Call(Call { id: other_function })) = function.nodes().last() {
            if other_function != &id && !encountered_functions.contains(&id) {
                let idx = function.nodes.len() - 1;
                let consume = commands.stats.function_calls.get(other_function).unwrap() <= &1
                    && !commands.optimizer.runtime.contains(other_function);
                commands.commands.push(OptimizeCommand::new(
                    (id, idx),
                    OptimizeCommandKind::InlineFunction(consume),
                ));
                encountered_functions.insert(*other_function);
            }
        }
    }
}

#[derive(Default)]
struct RedundancyOptimizer {
    pub aggressive_function_inlining: bool,
}

impl Optimizer for RedundancyOptimizer {
    /// Removes useless nodes
    ///
    /// # Optimizations:
    ///   - Removes assignments to variables that are never read
    ///   - If a value a is copied to value b and value a is assigned directly before, remove assign directly to b
    ///   - If a value is copied but the original value could be used, the copy gets removed
    ///   - If a branch's condition indirects to another condition, inline that other condition (right now only checks the previous node)
    ///   - If both branches of a branch end up at the same next node, add one goto after the branch
    ///   - Removes function calls to functions which are empty
    fn optimize(&mut self, commands: &mut Commands) {
        use OptimizeCommandKind::*;

        /// Checks for a write to `id` after `node` and returns with false
        /// If it is read before a write.
        /// Also returns false if no further write in this branch exists
        fn write_after_write(optimizer: &GlobalOptimizer, id: ItemId, node: NodeId) -> bool {
            for (_, other_node) in optimizer.iter_at(&node) {
                let mut branches = false;
                other_node.iter(&mut |node| {
                    if matches!(node, Node::Branch(_) | Node::Call(_)) {
                        branches = true
                    }
                });
                if branches {
                    return false;
                }

                let mut writes_id = false;
                let mut reads_id = false;
                other_node.variable_accesses(&mut |access| match access {
                    VariableAccess::Read(ScoreboardValue::Scoreboard(_, read_id))
                    | VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, read_id))
                        if read_id == &id =>
                    {
                        reads_id = true;
                    }
                    VariableAccess::Write(write_id) if write_id == &id => {
                        writes_id = true;
                    }
                    _ => {}
                });

                if reads_id {
                    return false;
                }

                if writes_id {
                    return true;
                }
            }
            false
        }

        for (node_id, node) in commands.optimizer.iter_nodes() {
            match node {
                Node::Nop => commands
                    .commands
                    .push(OptimizeCommand::new(node_id, Delete)),
                // Copies to itself (`a = a`) are useless and can be removed.
                Node::FastStore(FastStore {
                    scoreboard: target_scoreboard,
                    id: target,
                    value: ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_value),
                }) if target_scoreboard == rhs_scoreboard && target == rhs_value => {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete));
                }
                // Matches a write that is never read
                Node::FastStore(FastStore { id, .. }) if commands.is_id_unused(id, &node_id) => {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete));
                }
                Node::FastStoreFromResult(FastStoreFromResult { id, command, .. })
                    if commands.is_id_unused(id, &node_id) =>
                {
                    if command.is_effect_free(&commands.optimizer.functions) {
                        commands
                            .commands
                            .push(OptimizeCommand::new(node_id, Delete));
                    } else {
                        commands
                            .commands
                            .push(OptimizeCommand::new(node_id, DiscardResult))
                    }
                }
                Node::BinaryOperation(BinaryOperation {
                    scoreboard: _,
                    id,
                    lhs: _,
                    rhs: _,
                    operation: _,
                }) if commands.is_id_unused(id, &node_id) => {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete));
                }
                // Checks if a variable is written to and then beeing overwritten in the same function,
                // without beeing read first
                Node::FastStore(FastStore {
                    scoreboard: _,
                    id,
                    value: _,
                }) if write_after_write(commands.optimizer, *id, node_id) => {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete));
                }
                Node::FastStoreFromResult(FastStoreFromResult {
                    scoreboard: _,
                    id,
                    command,
                }) if write_after_write(commands.optimizer, *id, node_id) => {
                    if command.is_effect_free(&commands.optimizer.functions) {
                        commands
                            .commands
                            .push(OptimizeCommand::new(node_id, Delete));
                    } else {
                        commands
                            .commands
                            .push(OptimizeCommand::new(node_id, DiscardResult));
                    }
                }
                // Checks if a variable x gets created and then immediately copied to y without beeing used later
                Node::FastStore(FastStore {
                    scoreboard: _,
                    id,
                    value: ScoreboardValue::Scoreboard(_, copy_from),
                }) if commands.get_info(copy_from).reads == 1 => {
                    if let Some((prev_node_id, prev_node)) =
                        commands.optimizer.previous_node(&node_id)
                    {
                        if prev_node.writes_to(copy_from) {
                            commands
                                .commands
                                .push(OptimizeCommand::new(prev_node_id, ChangeWrite(*id)));
                            commands
                                .commands
                                .push(OptimizeCommand::new(node_id, Delete));
                        }
                    }
                }
                // If the operation is commutative, move the static value to the right side.
                // This makes better consistency and is easier to implement in minecraft.
                Node::BinaryOperation(BinaryOperation {
                    scoreboard,
                    id,
                    lhs: ScoreboardValue::Static(value),
                    rhs: ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_id),
                    operation,
                }) if matches!(
                    operation,
                    ScoreboardOperation::Plus | ScoreboardOperation::Times
                ) =>
                {
                    commands.commands.push(OptimizeCommand::new(
                        node_id,
                        Replace(Node::BinaryOperation(BinaryOperation {
                            id: *id,
                            scoreboard: *scoreboard,
                            lhs: ScoreboardValue::Scoreboard(*rhs_scoreboard, *rhs_id),
                            rhs: ScoreboardValue::Static(*value),
                            operation: *operation,
                        })),
                    ));
                }
                // Useless math operation (a + 0, a - 0)
                Node::BinaryOperation(BinaryOperation {
                    scoreboard,
                    id,
                    lhs,
                    rhs: ScoreboardValue::Static(0),
                    operation,
                }) if matches!(
                    operation,
                    ScoreboardOperation::Plus | ScoreboardOperation::Minus
                ) =>
                {
                    commands.commands.push(OptimizeCommand::new(
                        node_id,
                        Replace(Node::FastStore(FastStore {
                            id: *id,
                            scoreboard: *scoreboard,
                            value: *lhs,
                        })),
                    ));
                }
                // Useless math operation (a * 1, a / 1, a % 1)
                Node::BinaryOperation(BinaryOperation {
                    scoreboard,
                    id,
                    lhs,
                    rhs: ScoreboardValue::Static(value),
                    operation,
                }) if matches!(
                    operation,
                    ScoreboardOperation::Times
                        | ScoreboardOperation::Divide
                        | ScoreboardOperation::Modulo
                ) && matches!(value, 0..=1) =>
                {
                    let kind = match value {
                        0 => OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                            id: *id,
                            scoreboard: *scoreboard,
                            value: ScoreboardValue::Static(0),
                        })),
                        1 => OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                            id: *id,
                            scoreboard: *scoreboard,
                            value: *lhs,
                        })),
                        _ => unreachable!(),
                    };
                    commands.commands.push(OptimizeCommand::new(node_id, kind));
                }
                // `let b = a - a` => `let b = 0`
                Node::BinaryOperation(BinaryOperation {
                    scoreboard,
                    id,
                    lhs: ScoreboardValue::Scoreboard(lhs_scoreboard, lhs),
                    rhs: ScoreboardValue::Scoreboard(rhs_scoreboard, rhs),
                    operation: ScoreboardOperation::Minus,
                }) if lhs == rhs && lhs_scoreboard == rhs_scoreboard => {
                    commands.commands.push(OptimizeCommand::new(
                        node_id,
                        OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                            id: *id,
                            scoreboard: *scoreboard,
                            value: ScoreboardValue::Static(0),
                        })),
                    ));
                }
                // `let b = a / a` => `let b = 1`
                Node::BinaryOperation(BinaryOperation {
                    scoreboard,
                    id,
                    lhs: ScoreboardValue::Scoreboard(lhs_scoreboard, lhs),
                    rhs: ScoreboardValue::Scoreboard(rhs_scoreboard, rhs),
                    operation: ScoreboardOperation::Divide,
                }) if lhs == rhs && lhs_scoreboard == rhs_scoreboard => {
                    commands.commands.push(OptimizeCommand::new(
                        node_id,
                        OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                            id: *id,
                            scoreboard: *scoreboard,
                            value: ScoreboardValue::Static(1),
                        })),
                    ));
                }
                Node::Call(Call { id }) => {
                    let function = commands.optimizer.get_function(id);
                    if function.is_empty() {
                        commands
                            .commands
                            .push(OptimizeCommand::new(node_id, Delete));
                    } else {
                        // If the called function is only called from here, the function may be inlined.
                        // Additionally, the function may be inlined, if it is not directly recursive,
                        // and does not call the calling function
                        if commands.get_call_count(id) == 1
                            || (self.aggressive_function_inlining
                                && !function.calls_function(&node_id.0)
                                && !commands
                                    .infinite_loop_detector
                                    .detect_infinite_loop(&commands.optimizer.functions, *id))
                        {
                            let remove_function = commands.get_call_count(id) == 1;
                            commands.commands.push(OptimizeCommand::new(
                                node_id,
                                InlineFunction(remove_function),
                            ));
                            // This command modifies a lot of nodes, so return here to not destroy the internal state.
                            return;
                        }
                    }
                }
                // If the branch does the same thing in both cases - remove it
                Node::Branch(branch)
                    if branch.pos_branch.as_ref() == branch.neg_branch.as_ref() =>
                {
                    let new_command = branch.pos_branch.as_ref().clone();
                    if branch.condition.is_effect_free() {
                        commands
                            .commands
                            .push(OptimizeCommand::new(node_id, Replace(new_command)));
                    } else {
                        commands
                            .commands
                            .push(OptimizeCommand::new(node_id, InsertAfter(new_command)));
                        commands.commands.push(OptimizeCommand::new(
                            node_id,
                            Replace(Node::Condition(branch.condition.clone())),
                        ));
                    }
                }
                // Checks if the branch depends on a condition that was just calculated
                // ToDo: Instead of only checking the last condition, check as long as the condition is valid
                Node::Branch(Branch {
                    condition,
                    pos_branch,
                    neg_branch,
                }) => {
                    fn simplify_condition(
                        optimize_commands: &mut OptimizeCommandDeque<OptimizeCommand>,
                        optimizer: &GlobalOptimizer,
                        node_id: NodeId,
                        prev_node: &Node,
                        condition: &Condition,
                    ) -> Option<(Vec<usize>, Condition)> {
                        match condition {
                            Condition::Compare {
                                comparison: ScoreboardComparison::Equal,
                                lhs: ScoreboardValue::Scoreboard(Scoreboard::Main, id),
                                rhs: ScoreboardValue::Static(1),
                            } => {
                                if let Node::FastStoreFromResult(FastStoreFromResult {
                                    scoreboard: Scoreboard::Main,
                                    id: cond_id,
                                    command,
                                }) = prev_node
                                {
                                    if id == cond_id {
                                        if let Node::Condition(condition) = command.as_ref() {
                                            return Some((vec![], condition.clone()));
                                        }
                                    }
                                }
                            }
                            Condition::Or(values) | Condition::And(values) => {
                                for (index, inner_condition) in values.iter().enumerate() {
                                    if let Some((mut result_index, condition)) = simplify_condition(
                                        optimize_commands,
                                        optimizer,
                                        node_id,
                                        prev_node,
                                        inner_condition,
                                    ) {
                                        result_index.push(index);
                                        return Some((result_index, condition));
                                    }
                                }
                            }
                            _ => (),
                        }
                        None
                    }

                    let mut could_optimize = false;
                    if let Some((prev_id, prev_node)) = commands.optimizer.previous_node(&node_id) {
                        if let Some((result_index, condition)) = simplify_condition(
                            commands.commands,
                            commands.optimizer,
                            node_id,
                            prev_node,
                            condition,
                        ) {
                            commands
                                .commands
                                .push(OptimizeCommand::new(prev_id, Delete));
                            commands.commands.push(OptimizeCommand::new(
                                node_id,
                                SetCondition(condition, result_index),
                            ));
                            could_optimize = true;
                        }
                    }

                    // Otherwise check if one of the branches is a nop or a single command
                    if !could_optimize {
                        for (branch, flag) in std::array::IntoIter::new([
                            (pos_branch.as_ref(), true),
                            (neg_branch.as_ref(), false),
                        ]) {
                            if let Node::Call(Call { id }) = branch {
                                let function = commands.optimizer.get_function(id);
                                match function.nodes() {
                                    [] => commands.commands.push(OptimizeCommand::new(
                                        node_id,
                                        UpdateBranch {
                                            branch: flag,
                                            new_node: Node::Nop,
                                        },
                                    )),
                                    [single] => commands.commands.push(OptimizeCommand::new(
                                        node_id,
                                        UpdateBranch {
                                            branch: flag,
                                            new_node: single.clone(),
                                        },
                                    )),
                                    _ => {}
                                }
                            }
                        }
                    }
                }
                _ => (),
            }
        }
    }
}

/// This optimizer handles functions that alias other function.
/// An aliasing function consists of only one node, which calls another function.
/// If such a function is found, it gets deleted and all references to it get moved
/// to the aliased function.
/// This prevents the optimizer from inlining into an aliasing function, reducing the code size.
fn alias_function_optimizer(commands: &mut Commands) {
    for (function_id, function) in &commands.optimizer.functions {
        if let [Node::Call(Call { id })] = function.nodes() {
            if id == function_id
                || commands.get_call_count(id) <= 1
                || commands.optimizer.runtime.contains(function_id)
            {
                continue;
            }

            // found an aliasing function, now remove all references to it
            commands.commands.push(OptimizeCommand::new(
                (*function_id, 0),
                OptimizeCommandKind::RemoveAliasFunction(*id),
            ));
            return;
        }
    }
}

/// This expensive optimization searches for common paths at conditionals.
///
/// A condition like `Condition {pos: Call(1), neg: Call(2)}`, where Block 2: `[..commands, Call(1)]`
/// Gets optimized such that the common calls to block 1 are removed in both calls and instead
/// inserted after the branch: `Condition {pos: Nop, neg: Call(2)}; Call(1)`, Block 2: `[..commands]`
/// Since both blocks which contain the common call are modified, it must be verified, that no other
/// context calls these.
///
/// This optimization is important, because without it, a lot of useless else statements would
/// be in the interpreter for a long time. This also reduces the code size. Additionally, it is easier
/// for humans to reason about linear code.
fn optimize_common_path(commands: &mut Commands) {
    /// Extracts the first common function call in the
    /// calls chains of block_a and block_b
    fn get_common_call(
        commands: &Commands,
        block_a: BlockId,
        block_b: BlockId,
    ) -> Option<(BlockId, NodeId, NodeId)> {
        let a_calls = {
            let mut calls = FxHashMap::default();
            let mut current_block = block_a;
            loop {
                let function = commands.optimizer.get_function(&current_block);
                if let Some(Node::Call(Call { id })) = function.nodes().last() {
                    // Stop at recursion
                    if calls.contains_key(id) {
                        break;
                    }

                    // Only insert if the node may be modified
                    if commands.get_call_count(&current_block) <= 1 {
                        calls.insert(*id, (current_block, function.nodes().len() - 1));
                    }
                    current_block = *id;
                } else {
                    break;
                }
            }
            calls
        };

        let mut visited_functions = FxHashSet::default();

        let mut current_block = block_b;
        loop {
            let function = commands.optimizer.get_function(&current_block);
            match function.nodes().last() {
                Some(Node::Call(Call { id })) => match a_calls.get(id) {
                    Some(a_call) if commands.get_call_count(&current_block) <= 1 => {
                        return Some((*id, *a_call, (current_block, function.nodes().len() - 1)));
                    }
                    _ => {
                        if visited_functions.contains(&current_block) {
                            break;
                        }
                        visited_functions.insert(*id);
                        current_block = *id
                    }
                },
                _ => break,
            }
        }

        None
    }

    for (node_id, node) in commands.optimizer.iter_nodes() {
        if let Node::Branch(branch) = node {
            let pos_branch = branch.pos_branch.as_ref();
            let neg_branch = branch.neg_branch.as_ref();
            if let (Node::Call(Call { id: pos_id }), Node::Call(Call { id: neg_id })) =
                (pos_branch, neg_branch)
            {
                let result = get_common_call(commands, *pos_id, *neg_id);
                if let Some((common_block, call_a, call_b)) = result {
                    if call_a != call_b {
                        commands
                            .commands
                            .push(OptimizeCommand::new(call_a, OptimizeCommandKind::Delete));

                        commands
                            .commands
                            .push(OptimizeCommand::new(call_b, OptimizeCommandKind::Delete));

                        commands.commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::InsertAfter(Node::Call(Call { id: common_block })),
                        ));

                        // This change must be commited before continuing with this optimization
                        return;
                    }
                }
            }
        }
    }
}

/// Optimizes basic arithmetic expressions with the shape
/// x OP constant OP constant ...
/// NOTE: This optimizer changes the runtime behavior!
/// An operation like `a * 2000 / 1000` can cause an overflow,
/// while the optimized version `a * 2` might not.
/// For this reason it is **very important** that this optimizer is
/// not toggled between release and debug mode.
fn simple_arithmetic_optimization(commands: &mut Commands) {
    #[derive(Debug)]
    struct Fraction {
        numerator: i32,
        denominator: i32,
    }

    impl Fraction {
        const ONE: Fraction = Fraction {
            numerator: 1,
            denominator: 1,
        };

        fn simplify(&mut self) {
            let mut a = self.numerator;
            let mut b = self.denominator;
            while b != 0 {
                let temp = b;
                b = a % b;
                a = temp;
            }

            self.numerator /= a;
            self.denominator /= a;
        }

        fn inverse(self) -> Fraction {
            Fraction {
                numerator: self.denominator,
                denominator: self.numerator,
            }
        }
    }

    impl From<i32> for Fraction {
        fn from(val: i32) -> Self {
            Fraction {
                numerator: val,
                denominator: 1,
            }
        }
    }

    impl Mul for Fraction {
        type Output = Self;

        fn mul(self, rhs: Self) -> Self::Output {
            let mut res = Fraction {
                numerator: self.numerator * rhs.numerator,
                denominator: self.denominator * rhs.denominator,
            };
            res.simplify();
            res
        }
    }

    fn optimize_additive(operations: impl Iterator<Item = i32>) -> (i32, u32) {
        operations.fold((0, 0), |(acc, len), x| {
            (ScoreboardOperation::Plus.evaluate(acc, x), len + 1)
        })
    }

    fn optimize_multiplicative(operations: impl Iterator<Item = Fraction>) -> (Fraction, u32) {
        operations.fold((Fraction::ONE, 0), |(acc, len), x| (acc * x, len + 1))
    }

    'functions: for function in commands.optimizer.iter_functions() {
        for (node_id, node) in function {
            if let Node::BinaryOperation(BinaryOperation {
                scoreboard,
                id: target,
                lhs: ScoreboardValue::Scoreboard(lhs_scoreboard, lhs_id),
                rhs: ScoreboardValue::Static(value),
                operation,
            }) = node
            {
                if matches!(
                    operation,
                    ScoreboardOperation::Plus | ScoreboardOperation::Minus
                ) {
                    let mut subsequent_nodes = commands
                        .optimizer
                        .iter_at(&node_id)
                        .map(|(_, other_node)| {
                            if let Node::BinaryOperation(BinaryOperation {
                                scoreboard: _,
                                id: new_target,
                                lhs: ScoreboardValue::Scoreboard(_, also_new_target),
                                rhs: ScoreboardValue::Static(new_value),
                                operation,
                            }) = other_node
                            {
                                if new_target == also_new_target
                                    && new_target == target
                                    && matches!(
                                        operation,
                                        ScoreboardOperation::Plus | ScoreboardOperation::Minus
                                    )
                                {
                                    return Some(match operation {
                                        ScoreboardOperation::Plus => *new_value,
                                        ScoreboardOperation::Minus => -new_value,
                                        _ => unreachable!(),
                                    });
                                }
                            }
                            None
                        })
                        .take_while(Option::is_some)
                        .map(Option::unwrap)
                        .peekable();
                    if subsequent_nodes.peek().is_some() {
                        let (optimized_value, len) =
                            optimize_additive(std::iter::once(*value).chain(subsequent_nodes));
                        for i in 0..((len - 1) as usize) {
                            commands.commands.push(OptimizeCommand::new(
                                (node_id.0, node_id.1 + i),
                                OptimizeCommandKind::Delete,
                            ));
                        }
                        commands.commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::Replace(Node::BinaryOperation(BinaryOperation {
                                scoreboard: *scoreboard,
                                id: *target,
                                lhs: ScoreboardValue::Scoreboard(*lhs_scoreboard, *lhs_id),
                                rhs: ScoreboardValue::Static(optimized_value),
                                operation: *operation,
                            })),
                        ));
                        continue 'functions;
                    }
                } else if matches!(
                    operation,
                    ScoreboardOperation::Times | ScoreboardOperation::Divide
                ) {
                    let original_fraction = match operation {
                        ScoreboardOperation::Times => Fraction::from(*value),
                        ScoreboardOperation::Divide => Fraction::from(*value).inverse(),
                        _ => unreachable!(),
                    };
                    let mut subsequent_nodes = commands
                        .optimizer
                        .iter_at(&node_id)
                        .map(|(_, other_node)| {
                            if let Node::BinaryOperation(BinaryOperation {
                                scoreboard: _,
                                id: new_target,
                                lhs: ScoreboardValue::Scoreboard(_, also_new_target),
                                rhs: ScoreboardValue::Static(new_value),
                                operation,
                            }) = other_node
                            {
                                if new_target == also_new_target
                                    && new_target == target
                                    && matches!(
                                        operation,
                                        ScoreboardOperation::Times | ScoreboardOperation::Divide
                                    )
                                {
                                    return Some(match operation {
                                        ScoreboardOperation::Times => Fraction::from(*new_value),
                                        ScoreboardOperation::Divide => {
                                            Fraction::from(*new_value).inverse()
                                        }
                                        _ => unreachable!(),
                                    });
                                }
                            }
                            None
                        })
                        .take_while(Option::is_some)
                        .map(Option::unwrap)
                        .peekable();
                    if subsequent_nodes.peek().is_some() {
                        let (optimized_value, len) = optimize_multiplicative(
                            std::iter::once(original_fraction).chain(subsequent_nodes),
                        );
                        let (new_operation, new_value) = match optimized_value {
                            Fraction {
                                numerator,
                                denominator: 1,
                            } => (ScoreboardOperation::Times, numerator),
                            Fraction {
                                numerator: 1,
                                denominator,
                            } => (ScoreboardOperation::Divide, denominator),
                            _ => break,
                        };
                        for i in 0..((len - 1) as usize) {
                            commands.commands.push(OptimizeCommand::new(
                                (node_id.0, node_id.1 + i),
                                OptimizeCommandKind::Delete,
                            ));
                        }
                        commands.commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::Replace(Node::BinaryOperation(BinaryOperation {
                                scoreboard: *scoreboard,
                                id: *target,
                                lhs: ScoreboardValue::Scoreboard(*lhs_scoreboard, *lhs_id),
                                rhs: ScoreboardValue::Static(new_value),
                                operation: new_operation,
                            })),
                        ));
                        continue 'functions;
                    }
                }
            }
        }
    }
}

#[derive(Default)]
pub struct RedundantCopyOptimizer {
    pending_commands: OptimizeCommandDeque<OptimizeCommand>,
}

impl Optimizer for RedundantCopyOptimizer {
    /// A node which copies a value `a` to `b` is often redundant.
    /// This is especially important, because the compiler loves to generate operations
    /// in the shape of `let temp = a; operation_with_temp(temp); a = temp`.
    /// This optimizer agressively removes copy instructions (`let temp = a`) and changes all
    /// subsequent reads to the previous variable (`a`).
    /// This optimization becomes invalid when a node which reads from the previous variable `a`
    /// is encountered before a write to a occurs. If that happens, scrap the optimization
    /// attempt entirely.
    fn optimize(&mut self, commands: &mut Commands) {
        for (function_id, function) in &commands.optimizer.functions {
            'node_loop: for (idx, node) in function.nodes().iter().enumerate() {
                let (temp_id, original_id, original_scoreboard, is_bin_op) = match node {
                    Node::FastStore(FastStore {
                        scoreboard: _,
                        id: temp_id,
                        value: ScoreboardValue::Scoreboard(original_scoreboard, original_id),
                    }) => (temp_id, original_id, original_scoreboard, false),
                    Node::BinaryOperation(BinaryOperation {
                        scoreboard: _,
                        id: temp_id,
                        lhs: ScoreboardValue::Scoreboard(original_scoreboard, original_id),
                        rhs: _,
                        operation: _,
                    }) => (temp_id, original_id, original_scoreboard, true),
                    _ => continue,
                };

                if temp_id == original_id {
                    continue;
                }

                self.pending_commands.clear();
                let mut optimization_success = false;
                let total_temp_reads = commands.get_reads(temp_id);
                let mut encountered_temp_reads = 0;
                let mut encountered_original_reads = 0;
                let mut optimization_modifies_original_value = false;
                // If the first node is a binary operation, the original value gets modified.
                let mut modifies_original_value = is_bin_op;
                let mut reads_from_original_value = false;
                for (node_id, node) in commands.optimizer.iter_at(&(*function_id, idx)) {
                    let mut reads_from_original = false;
                    let mut write_to_original = false;
                    let mut reads_from_copy = false;
                    let mut write_to_copy = false;
                    node.variable_accesses(&mut |access| match access {
                        VariableAccess::Read(ScoreboardValue::Scoreboard(_, id)) => {
                            if id == original_id {
                                encountered_original_reads += 1;
                                reads_from_original = true;
                            } else if id == temp_id {
                                encountered_temp_reads += 1;
                                reads_from_copy = true;
                            }
                        }
                        VariableAccess::Write(id) => {
                            if id == original_id {
                                write_to_original = true;
                            } else if id == temp_id {
                                write_to_copy = true;
                            }
                        }
                        VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, id)) => {
                            if id == original_id {
                                encountered_original_reads += 1;
                                write_to_original = true;
                                reads_from_original = true;
                            } else if id == temp_id {
                                encountered_temp_reads += 1;
                                write_to_copy = true;
                                reads_from_copy = true;
                            }
                        }
                        _ => {}
                    });

                    if write_to_copy {
                        self.pending_commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::ChangeWrite(*original_id),
                        ));
                        optimization_modifies_original_value = true;
                    }
                    if reads_from_copy {
                        self.pending_commands.push(OptimizeCommand::new(
                            node_id,
                            OptimizeCommandKind::ChangeReads(
                                *temp_id,
                                ScoreboardValue::Scoreboard(*original_scoreboard, *original_id),
                            ),
                        ));
                    }
                    if reads_from_original {
                        reads_from_original_value = true;
                    }
                    let is_copy_back = match node {
                        Node::FastStore(FastStore {
                            scoreboard: _,
                            id: dest,
                            value: ScoreboardValue::Scoreboard(_, src),
                        }) => dest == original_id && src == temp_id,
                        _ => false,
                    };
                    // If the original value gets modified, stop the search
                    if write_to_original {
                        if is_copy_back {
                            if encountered_temp_reads == total_temp_reads {
                                // Successful case
                                optimization_success = true;
                                break;
                            } else {
                                modifies_original_value = true;
                            }
                        } else {
                            // error case
                            continue 'node_loop;
                        }
                    }
                }

                // If the original value is accessed, but this optimization modifies the original value,
                // the entire optimization becomes invalid.
                if reads_from_original_value && modifies_original_value {
                    continue;
                }

                // If no function has a dependency on the original variable,
                // the optimizer is free to inline it.
                let is_unused_after = |commands: &Commands, id: ItemId| {
                    commands.get_reads(&id) == 0
                        || ((!commands.stats.function_parameters.is_dependency(id))
                            && (commands
                                .optimizer
                                .iter_at(&(*function_id, idx))
                                .all(|(_, node)| !node.reads_from(&id))))
                };

                // If the value just gets copied to be read from, in the same function,
                // this optimization is valid
                if !optimization_success
                    && !modifies_original_value
                    && !optimization_modifies_original_value
                    && !commands.stats.function_parameters.is_dependency(*temp_id)
                {
                    optimization_success = true;
                }

                // println!(
                //     "!!!!!{:?}, {}, {}, {:?}",
                //     original_id,
                //     optimization_success,
                //     is_unused_after(commands, *original_id),
                //     commands.stats.function_parameters
                // );

                // what a lovely condition
                if optimization_success
                    || (is_unused_after(commands, *original_id)
                        && ((encountered_temp_reads == total_temp_reads
                            && (!matches!(node, Node::BinaryOperation(_))
                                || optimization_modifies_original_value))
                            || (is_unused_after(commands, *original_id)
                                && !commands.stats.function_parameters.is_dependency(*temp_id))))
                {
                    // If this code runs, the optimization was successful
                    // Now write all the nodes.
                    // First, update the copy node:
                    match node {
                        Node::BinaryOperation(BinaryOperation {
                            scoreboard,
                            id: _,
                            lhs,
                            rhs,
                            operation,
                        }) => {
                            commands.commands.push(OptimizeCommand::new(
                                (*function_id, idx),
                                OptimizeCommandKind::Replace(Node::BinaryOperation(
                                    BinaryOperation {
                                        id: *original_id,
                                        scoreboard: *scoreboard,
                                        lhs: *lhs,
                                        operation: *operation,
                                        rhs: *rhs,
                                    },
                                )),
                            ));
                        }
                        Node::FastStore(_) => commands.commands.push(OptimizeCommand::new(
                            (*function_id, idx),
                            OptimizeCommandKind::Delete,
                        )),
                        _ => unreachable!(),
                    }

                    // Then, modify all the changed node
                    commands.commands.append(&mut self.pending_commands);
                    // Continue at the next function
                    break;
                }
            }
        }
    }
}

/// Optimizes nodes which are const-evaluatable.
/// This optimizer tracks all const assignments to variables in
/// a given function and replaces reads from const variables by their
/// const value. Also contains functionality to evaluate [BinaryOperation] and [Condition].
/// In order to be more efficient, this optimizer optimizes an entire function.
/// This means that the current state must always be synced correctly!
#[derive(Default)]
struct ConstOptimizer {
    value_hints: ValueHints,
}

impl Optimizer for ConstOptimizer {
    fn optimize(&mut self, commands: &mut Commands) {
        for function_iter in commands.optimizer.iter_functions() {
            self.value_hints.clear_all();
            for (node_id, node) in function_iter {
                let mut did_optimize = false;
                let commands_vec = &mut *commands.commands;
                node.variable_accesses(&mut |access| {
                    if let VariableAccess::Read(ScoreboardValue::Scoreboard(_, id)) = access {
                        if let Hint::Exact(exact_value) = self.value_hints.get_hint(id) {
                            commands_vec.push(OptimizeCommand::new(
                                node_id,
                                OptimizeCommandKind::ChangeReads(
                                    *id,
                                    ScoreboardValue::Static(exact_value),
                                ),
                            ));
                            self.value_hints.update_hints(node);
                            did_optimize = true;
                        }
                    }
                });

                // Keep in mind that the new node must be update in self.value_hints!
                if !did_optimize {
                    // Evaluate static binary operations
                    match node {
                        Node::BinaryOperation(bin_op) => {
                            if let Some(result) = self.value_hints.static_binary_operation(bin_op) {
                                commands.commands.push(OptimizeCommand::new(
                                    node_id,
                                    OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                                        id: bin_op.id,
                                        scoreboard: bin_op.scoreboard,
                                        value: ScoreboardValue::Static(result),
                                    })),
                                ));
                                self.value_hints.set_hint(bin_op.id, Hint::Exact(result))
                            } else {
                                self.value_hints.update_hints(node);
                            }
                        }
                        Node::FastStoreFromResult(FastStoreFromResult {
                            scoreboard,
                            id,
                            command,
                        }) => {
                            if let Node::Condition(condition) = command.as_ref() {
                                if let Some(result) = self.value_hints.static_condition(condition) {
                                    let value = if result { 1 } else { 0 };
                                    commands.commands.push(OptimizeCommand::new(
                                        node_id,
                                        OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                                            id: *id,
                                            scoreboard: *scoreboard,
                                            value: ScoreboardValue::Static(value),
                                        })),
                                    ));
                                    self.value_hints.set_hint(*id, Hint::Exact(value));
                                } else {
                                    self.value_hints.update_hints(node);
                                }
                            } else {
                                self.value_hints.update_hints(node);
                            }
                        }
                        Node::Branch(Branch {
                            condition,
                            pos_branch,
                            neg_branch,
                        }) => {
                            if let Some(result) = self.value_hints.static_condition(condition) {
                                commands.commands.push(OptimizeCommand::new(
                                    node_id,
                                    OptimizeCommandKind::InlineBranch(result),
                                ));
                                let branch = if result { pos_branch } else { neg_branch };
                                self.value_hints.update_hints(&branch);
                            } else {
                                self.value_hints.update_hints(node);
                            }
                        }
                        _ => {
                            self.value_hints.update_hints(node);
                        }
                    }
                    // self.value_hints.update_hints(node);
                }
            }
        }
    }
}

/// tracks statistics about the global code which can be used
/// to allow some optimizations
#[derive(Debug)]
struct CodeStats {
    variable_information: FxHashMap<ItemId, VariableUsage>,
    function_calls: FxHashMap<BlockId, usize>,
    call_graph: CallGraph,
    /// Tracks which parameters a function takes.
    /// Due to the incremental updates, a function can declare to read a variable,
    /// Even if that is not the case anymore. It will never declare to not read a variable
    /// if it does so, though.
    function_parameters: FunctionParameters,
    /// This local variable is cached, so no repeated allocations are required
    visited_functions: FxHashSet<BlockId>,
}

impl CodeStats {
    pub fn new(call_graph: CallGraph) -> Self {
        CodeStats {
            variable_information: Default::default(),
            function_calls: Default::default(),
            call_graph,
            function_parameters: Default::default(),
            visited_functions: Default::default(),
        }
    }

    fn clear(&mut self) {
        self.variable_information.clear();
        self.function_calls.clear();
        self.function_parameters.clear();
        self.visited_functions.clear();
    }

    /// Updates the variable reads and writes.
    /// The iterator does not technically need to give mutable nodes.
    /// However, due to some rust limitations (how to abstract over & and &mut at the same time?)
    /// Mutable references are required.
    fn update(&mut self, runtime: &Runtime, functions: &FxHashMap<BlockId, Function>) {
        self.clear();

        self.visited_functions.reserve(functions.len());
        let mut pending_functions = FxHashSet::default();

        for on_load_block in &runtime.load_blocks {
            *self.function_calls.entry(*on_load_block).or_default() += 1;
            pending_functions.insert(*on_load_block);
        }

        for on_tick_block in &runtime.scheduled_blocks {
            *self.function_calls.entry(*on_tick_block).or_default() += 1;
            pending_functions.insert(*on_tick_block);
        }

        while let Some(function_id) = pending_functions.iter().next().copied() {
            pending_functions.remove(&function_id);
            self.visited_functions.insert(function_id);

            let function = functions.get(&function_id).unwrap();
            for node in function.nodes() {
                self.update_node(
                    node,
                    VariableUsage::add_read,
                    VariableUsage::add_write,
                    None,
                );
                // Also update calling statistics
                node.iter(&mut |inner_node| {
                    if let Node::Call(Call { id }) = inner_node {
                        *self.function_calls.entry(*id).or_default() += 1;
                        if !self.visited_functions.contains(id) {
                            pending_functions.insert(*id);
                        }
                    }
                });
                // It is important that writes are not written if there exist reads,
                // since reads are more important to store heres
                node.variable_accesses(&mut |access| match access {
                    VariableAccess::Read(ScoreboardValue::Scoreboard(_, id)) => {
                        self.function_parameters.set_read_weak(function_id, *id)
                    }
                    VariableAccess::Write(id) => {
                        if !node.reads_from(id) {
                            self.function_parameters.set_write_weak(function_id, *id)
                        }
                    }
                    VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, id)) => {
                        self.function_parameters.set_read_weak(function_id, *id)
                    }
                    _ => {}
                });
            }
        }
    }

    fn add_node(&mut self, node: &Node, id: &NodeId) {
        self.update_node(
            node,
            VariableUsage::add_read,
            VariableUsage::add_write,
            Some(id.0),
        );

        node.iter(&mut |inner_node| {
            if let Node::Call(Call { id: call_id }) = inner_node {
                *self.function_calls.entry(*call_id).or_default() += 1;
                self.call_graph.modify_call(id.0, *call_id, 1)
            }
        });
    }

    fn remove_node(&mut self, node: &Node, id: &NodeId) {
        self.update_node(
            node,
            VariableUsage::remove_read,
            VariableUsage::remove_write,
            None,
        );

        node.iter(&mut |inner_node| {
            if let Node::Call(Call { id: call_id }) = inner_node {
                match self.function_calls.entry(*call_id) {
                    Entry::Occupied(entry) => *entry.into_mut() -= 1,
                    Entry::Vacant(_) => unreachable!("This function should exist"),
                }
                self.call_graph.modify_call(id.0, *call_id, -1);
                // println!("Removing call to {} - now at {} calls", self.function_calls.en)
            }
        });
    }

    fn update_node<FR, FW>(
        &mut self,
        node: &Node,
        read: FR,
        write: FW,
        parameter_read: Option<BlockId>,
    ) where
        FR: Fn(&mut VariableUsage),
        FW: Fn(&mut VariableUsage),
    {
        node.variable_accesses(&mut |access| match access {
            VariableAccess::Read(ScoreboardValue::Scoreboard(_, value)) => {
                if let Some(function) = parameter_read {
                    self.function_parameters.set_read(function, *value);
                }
                read(self.variable_information.entry(*value).or_default())
            }
            VariableAccess::Write(value) => {
                write(self.variable_information.entry(*value).or_default())
            }
            VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, value)) => {
                if let Some(function) = parameter_read {
                    self.function_parameters.set_read(function, *value);
                }
                read(self.variable_information.entry(*value).or_default());
                write(self.variable_information.entry(*value).or_default());
            }
            _ => {}
        });
    }
}

#[cfg(test)]
mod tests {
    use super::DEBUG;

    #[test]
    fn test_not_accidentally_debug() {
        assert_eq!(DEBUG, false);
    }
}
