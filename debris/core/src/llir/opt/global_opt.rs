use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap, VecDeque},
    hash::BuildHasher,
    ops::{Deref, DerefMut},
};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    llir::{
        llir_impl::LLirFunction,
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
    cfg::LoopDetector,
    variable_metadata::{Hint, ValueHints, VariableUsage},
};

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
        functions: FxHashMap<BlockId, LLirFunction>,
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
        if matches!(self.config.opt_mode, OptMode::Full) {
            self.optimize();
        }
        self.functions
    }
}

impl GlobalOptimizer<'_> {
    fn optimize(&mut self) {
        let mut const_optimizer = ConstOptimizer::default();

        let mut run_optimize_pass = |commands: &mut Commands| -> bool {
            commands.run_optimizer(&mut optimize_unused_code)
                | commands.run_optimizer(&mut optimize_redundancy)
                | commands.run_optimizer(&mut optimize_common_path)
                | commands.run_optimizer(&mut const_optimizer)
        };

        let mut commands_deque = OptimizeCommandDeque::new();
        let mut variable_info = Default::default();

        let mut commands = Commands::new(self, &mut variable_info, &mut commands_deque);

        const MAX_ITERATIONS: usize = 4096;

        for _ in 0..MAX_ITERATIONS {
            // // Print debug representation of llir
            // if true {
            //     use itertools::Itertools;
            //     use std::fmt::Write;

            //     let mut buf = String::new();
            //     let fmt_function = |func: &Function, buf: &mut String| {
            //         buf.write_fmt(format_args!(
            //             "({} call(s)) - {}",
            //             commands.get_call_count(&func.id),
            //             func
            //         ))
            //         .unwrap()
            //     };

            //     for (_, function) in commands
            //         .optimizer
            //         .functions
            //         .iter()
            //         .sorted_by_key(|(_, func)| func.id)
            //     {
            //         fmt_function(&function, &mut buf);
            //         buf.push('\n');
            //     }
            //     println!("{}", buf);
            // }
            let could_optimize = run_optimize_pass(&mut commands);
            if !could_optimize {
                return;
            }
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
    /// Inlines the function of this function call.
    /// The bool specifies, whether the inlined function can be consumed.
    /// Otherwise, the function will be cloned.
    InlineFunction(bool),
    /// Deletes a single function
    RemoveFunction,
    /// Changes the variable this node writes to
    ChangeWrite(ItemId),
    /// Replaces all variables `.0` with `.1`
    ChangeReads(ItemId, ScoreboardValue),
    /// Changes the function called by this node to the new function
    ChangeCall { from: BlockId, to: BlockId },
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
#[derive(Debug, Default)]
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

/// Interface for optimizing functions to get data about the code and emit
/// optimization instructions
struct Commands<'opt, 'ctx> {
    optimizer: &'opt mut GlobalOptimizer<'ctx>,
    stats: &'opt mut CodeStats,
    commands: &'opt mut OptimizeCommandDeque<OptimizeCommand>,
    loop_detector: LoopDetector,
}

impl<'opt, 'ctx> Commands<'opt, 'ctx> {
    fn new(
        optimizer: &'opt mut GlobalOptimizer<'ctx>,
        stats: &'opt mut CodeStats,
        commands: &'opt mut OptimizeCommandDeque<OptimizeCommand>,
    ) -> Self {
        let commands = Commands {
            optimizer,
            stats,
            commands,
            loop_detector: Default::default(),
        };
        commands
            .stats
            .update(commands.optimizer.runtime, &commands.optimizer.functions);
        commands
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
        // println!("{:?}", self.stats.variable_information);
        // println!("{:?}", self.commands);
        self.execute_commands();
        len > 0
    }

    /// Execute every command that is in the current command stack
    fn execute_commands(&mut self) {
        while let Some(command) = self.commands.pop_front() {
            let id = command.id;
            // Don't do anything for commands that effect nodes of unused functions
            if !matches!(command.kind, OptimizeCommandKind::RemoveFunction { .. })
                && !self.stats.visited_functions.contains(&id.0)
            {
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
                    self.stats.remove_node(&node);
                }
                OptimizeCommandKind::DiscardResult => {
                    let nodes = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes;
                    let old_value = std::mem::replace(&mut nodes[id.1], Node::Nop);
                    self.stats.remove_node(&old_value);
                    nodes[id.1] = match old_value {
                        Node::FastStoreFromResult(store) => {
                            self.stats.add_node(&store.command);
                            *store.command
                        }
                        other => panic!("Invalid node: {:?}", other),
                    }
                }
                OptimizeCommandKind::InlineBranch(condition) => {
                    let nodes = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes;
                    let old_value = std::mem::replace(&mut nodes[id.1], Node::Nop);
                    self.stats.remove_node(&old_value);
                    let new_node = match old_value {
                        Node::Branch(Branch {
                            condition: _,
                            pos_branch,
                            neg_branch,
                        }) => *if condition { pos_branch } else { neg_branch },
                        other => panic!("Invalid node: {:?}", other,),
                    };

                    self.stats.add_node(&new_node);
                    nodes[id.1] = new_node;
                }
                OptimizeCommandKind::InlineFunction(consume) => {
                    let node = &self.optimizer.functions.get(&id.0).unwrap().nodes[id.1];
                    let inlined_function_id = match node {
                        Node::Call(Call { id }) => *id,
                        other => panic!("Invalid node: {:?}", other),
                    };

                    // SAFETY: If the function is consumed, nothing changes
                    if !consume {
                        self.stats.remove_node(node);
                    }

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
                    if !consume {
                        for node in &new_nodes {
                            self.stats.add_node(node);
                        }
                    }

                    let nodes = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes;
                    nodes.splice(id.1..=id.1, new_nodes.into_iter());
                }
                OptimizeCommandKind::RemoveFunction => {
                    self.optimizer.functions.remove(&id.0).unwrap();
                    // SAFETY: functions are only deleted if they are unused - no need to update the stats
                    // for node in function.nodes {
                    //     self.stats.remove_node(&node);
                    // }
                }
                OptimizeCommandKind::ChangeWrite(new_target) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    self.stats.remove_node(node);
                    node.variable_accesses_mut(&mut |access| match access {
                        VariableAccessMut::Write(value)
                        | VariableAccessMut::ReadWrite(ScoreboardValue::Scoreboard(_, value)) => {
                            *value = new_target
                        }
                        _ => {}
                    });
                    self.stats.add_node(node);
                }
                OptimizeCommandKind::ChangeReads(old_id, new_value) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    self.stats.remove_node(node);
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
                    self.stats.add_node(node);
                }
                OptimizeCommandKind::ChangeCall { from, to } => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    self.stats.remove_node(node);
                    node.iter_mut(&mut |node| {
                        if let Node::Call(Call { id }) = node {
                            if id == &from {
                                *id = to;
                            }
                        }
                    });
                    self.stats.add_node(node);
                }
                OptimizeCommandKind::SetCondition(condition, indices) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    match node {
                        Node::Branch(branch) => {
                            let mut old_condition = &mut branch.condition;
                            for index in indices {
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
                            self.stats.add_node(&new_condition_node);

                            let new_condition = if let Node::Condition(cond) = new_condition_node {
                                cond
                            } else {
                                unreachable!()
                            };
                            let old_condition = std::mem::replace(old_condition, new_condition);
                            let old_condition_node = Node::Condition(old_condition);
                            self.stats.remove_node(&old_condition_node);
                        }
                        other => panic!("Invalid node: {:?}", other),
                    }
                }
                OptimizeCommandKind::Replace(new_node) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    self.stats.remove_node(node);
                    self.stats.add_node(&new_node);
                    *node = new_node;
                }
                OptimizeCommandKind::InsertAfter(next_node) => {
                    self.stats.add_node(&next_node);
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

/// Removes useless nodes
///
/// # Optimizations:
///   - Removes assignments to variables that are never read
///   - If a value a is copied to value b and value a is assigned directly before, remove assign directly to b
///   - If a value is copied but the original value could be used, the copy gets removed
///   - If a branch's condition indirects to another condition, inline that other condition (right now only checks the previous node)
///   - If both branches of a branch end up at the same next node, add one goto after the branch
///   - Removes function calls to functions which are empty
fn optimize_redundancy(commands: &mut Commands) {
    use OptimizeCommandKind::*;

    /// Checks for a write to `id` after `node` and returns with false
    /// If it is read before a write.
    /// Also returns false if no further write in this branch exists
    fn write_after_write(optimizer: &GlobalOptimizer, id: ItemId, node: NodeId) -> bool {
        for (_, other_node) in optimizer.iter_at(&node) {
            let mut branches = false;
            other_node.iter(&mut |node| {
                if matches!(node, Node::Branch(_)) {
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
            // Copies to itself (`a = a`) are useless and can be removed.
            Node::FastStore(FastStore {
                scoreboard: target_scoreboard,
                id: target,
                value: ScoreboardValue::Scoreboard(rhs_scoreboard, rhs_value),
            }) if target_scoreboard == rhs_scoreboard && target == rhs_value => {
                commands
                    .commands
                    .push(OptimizeCommand::new(node_id, Delete));
                return;
            }
            // Matches a write that is never read
            Node::FastStore(FastStore { id, .. }) if commands.get_reads(id) == 0 => {
                commands
                    .commands
                    .push(OptimizeCommand::new(node_id, Delete));
            }
            Node::FastStoreFromResult(FastStoreFromResult { id, command, .. })
                if commands.get_info(id).reads == 0 =>
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
                if let Some((prev_node_id, prev_node)) = commands.optimizer.previous_node(&node_id)
                {
                    if prev_node.writes_to(copy_from) {
                        commands
                            .commands
                            .push(OptimizeCommand::new(prev_node_id, ChangeWrite(*id)));
                        commands
                            .commands
                            .push(OptimizeCommand::new(node_id, Delete));

                        // Prevent the optimizer from inlining twice without returning
                        if commands.get_info(id).reads == 1 {
                            return;
                        }
                    }
                }
            }
            // If value a is copied to b, replace every use of b by a, until a or b is written to
            Node::FastStore(FastStore {
                scoreboard: _,
                id,
                value: ScoreboardValue::Scoreboard(from_scoreboard, copy_from),
            }) => {
                let mut any_update = false;
                for (other_node_id, other_node) in commands.optimizer.iter_at(&node_id) {
                    if other_node.writes_to(id) || other_node.writes_to(copy_from) {
                        break;
                    }

                    if other_node.reads_from(id) {
                        commands.commands.push(OptimizeCommand::new(
                            other_node_id,
                            ChangeReads(
                                *id,
                                ScoreboardValue::Scoreboard(*from_scoreboard, *copy_from),
                            ),
                        ));
                        any_update = true;
                    }
                }

                // Since this function iterates over every node,
                // Only the current node may be changed without getting out of sync.
                // To prevent trouble, return here and start a new iteration.
                if any_update {
                    return;
                }
            }
            // ??? Please rework that
            Node::BinaryOperation(BinaryOperation {
                id: new_id,
                lhs: ScoreboardValue::Scoreboard(lhs_scoreboard, copy_from),
                rhs: rhs @ ScoreboardValue::Static(_),
                operation,
                scoreboard,
            }) if commands.get_info(copy_from).reads == 1
                && new_id != copy_from
                && scoreboard == lhs_scoreboard =>
            {
                let mut any_update = false;
                // set the write target for every node from copy_from to id
                for (other_node_id, other_node) in commands.optimizer.iter_nodes() {
                    if other_node.writes_to(copy_from) {
                        commands
                            .commands
                            .push(OptimizeCommand::new(other_node_id, ChangeWrite(*new_id)));
                        any_update = true;
                    }
                }
                commands.commands.push(OptimizeCommand::new(
                    node_id,
                    Replace(Node::BinaryOperation(BinaryOperation {
                        id: *new_id,
                        lhs: ScoreboardValue::Scoreboard(*lhs_scoreboard, *new_id),
                        operation: *operation,
                        rhs: *rhs,
                        scoreboard: *scoreboard,
                    })),
                ));

                // See above
                if any_update {
                    return;
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
            Node::Call(Call { id }) => {
                let function = commands.optimizer.get_function(id);
                if function.is_empty() {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete))
                } else {
                    // If the called function is only called from here, the function may be inlined.
                    // Additionally, the function may be inlined, if it is not directly recursive,
                    // and does not call the calling function
                    if commands.get_call_count(id) == 1
                        || (!function.calls_function(&node_id.0)
                            && !commands
                                .loop_detector
                                .is_infinite_loop(&commands.optimizer.functions, *id))
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
            // Checks if the branch depends on a condition that was just calculated
            // ToDo: Instead of only checking the last condition, check as long as the condition is valid
            Node::Branch(Branch { condition, .. }) => {
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
                    }
                }
            }
            _ => (),
        }
    }
}

// ToDo: Enable this code once a node for swapping values exists.
// /// Various optimizations for common patterns.
// /// Right now:
// ///     - Optimizes the sequence `temp = a; a = b; b = temp` to `swap a, b`
// fn optimize_common_patterns(commands: &mut Commands) {
//     for (function_id, function) in &commands.optimizer.functions {
//         for ((idx1, node1), (idx2, node2), (idx3, node3)) in
//             function.nodes().iter().enumerate().tuple_windows()
//         {
//             if let Node::FastStore(FastStore {
//                 scoreboard: Scoreboard::Main,
//                 id: id_temp,
//                 value: ScoreboardValue::Scoreboard(Scoreboard::Main, id_a),
//             }) = node1
//             {
//                 if let Node::FastStore(FastStore {
//                     scoreboard: Scoreboard::Main,
//                     id,
//                     value: ScoreboardValue::Scoreboard(Scoreboard::Main, id_b),
//                 }) = node2
//                 {
//                     if id == id_a {
//                         if let Node::FastStore(FastStore {
//                             scoreboard: Scoreboard::Main,
//                             id: id_b2,
//                             value: ScoreboardValue::Scoreboard(Scoreboard::Main, id_temp2),
//                         }) = node3
//                         {
//                             if id_b2 == id_b && id_temp2 == id_temp {
//                                 let id1 = (*function_id, idx1);
//                                 let id2 = (*function_id, idx2);
//                                 let id3 = (*function_id, idx3);
//                                 commands
//                                     .commands
//                                     .push(OptimizeCommand::new(id1, OptimizeCommandKind::Delete));
//                                 commands
//                                     .commands
//                                     .push(OptimizeCommand::new(id2, OptimizeCommandKind::Delete));

//                                 commands.commands.push(OptimizeCommand::new(
//                                     id3,
//                                     OptimizeCommandKind::Replace(Node::Execute(ExecuteRaw(vec![
//                                         ExecuteRawComponent::String(
//                                             "scoreboard players operation ".to_string(),
//                                         ),
//                                         ExecuteRawComponent::ScoreboardValue(
//                                             ScoreboardValue::Scoreboard(Scoreboard::Main, *id_a),
//                                         ),
//                                         ExecuteRawComponent::String(" >< ".to_string()),
//                                         ExecuteRawComponent::ScoreboardValue(
//                                             ScoreboardValue::Scoreboard(Scoreboard::Main, *id_b),
//                                         ),
//                                     ]))),
//                                 ));
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//     }
// }

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
            let mut calls = HashMap::new();
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

        let mut current_block = block_b;
        loop {
            let function = commands.optimizer.get_function(&current_block);
            match function.nodes().last() {
                Some(Node::Call(Call { id })) => match a_calls.get(id) {
                    Some(a_call) if commands.get_call_count(&current_block) <= 1 => {
                        return Some((*id, *a_call, (current_block, function.nodes().len() - 1)));
                    }
                    _ => current_block = *id,
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

/// Optimizes functions which are never called
fn optimize_unused_code(commands: &mut Commands) {
    for (id, _) in commands.optimizer.functions.iter() {
        let id = *id;
        if commands.get_call_count(&id) == 0 {
            commands.commands.push(OptimizeCommand::new(
                (id, 0),
                OptimizeCommandKind::RemoveFunction,
            ));
        } else if !commands.optimizer.runtime.contains(&id) {
            // If this function only directs to another function, also delete this function
            let function = commands.optimizer.get_function(&id);
            if let [Node::Call(Call { id: other_function })] = function.nodes() {
                if other_function != &id {
                    let other_function = *other_function;

                    for (node_id, node) in commands.optimizer.iter_nodes() {
                        // Limitation of the borrow checker: using `commands.commands` in the closure
                        // borrows the entire struct
                        let commands_deque = &mut commands.commands;
                        node.iter(&mut |node| {
                            if let Node::Call(Call { id: block_id }) = node {
                                if block_id == &id {
                                    commands_deque.push(OptimizeCommand::new(
                                        node_id,
                                        OptimizeCommandKind::ChangeCall {
                                            from: *block_id,
                                            to: other_function,
                                        },
                                    ));
                                }
                            }
                        });
                    }
                    for command in commands
                        .optimizer
                        .get_function(&id)
                        .nodes()
                        .iter()
                        .enumerate()
                        .rev()
                    {
                        commands.commands.push(OptimizeCommand::new(
                            (id, command.0),
                            OptimizeCommandKind::Delete,
                        ));
                    }
                    commands.commands.push(OptimizeCommand::new(
                        (id, 0),
                        OptimizeCommandKind::RemoveFunction,
                    ));
                    // return, since this modifies many other functions
                    return;
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
#[derive(Debug, Default)]
struct CodeStats {
    variable_information: FxHashMap<ItemId, VariableUsage>,
    function_calls: FxHashMap<BlockId, usize>,
    visited_functions: FxHashSet<BlockId>,
}

impl CodeStats {
    fn clear(&mut self) {
        self.variable_information.clear();
        self.function_calls.clear();
        self.visited_functions.clear();
    }

    /// Updates the variable reads and writes.
    /// The iterator does not technically need to give mutable nodes.
    /// However, due to some rust limitations (how to abstract over & and &mut at the same time?)
    /// Mutable references are required.
    fn update<'a, H>(&mut self, runtime: &Runtime, functions: &'a HashMap<BlockId, Function, H>)
    where
        H: BuildHasher,
    {
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
                self.update_node(node, VariableUsage::add_read, VariableUsage::add_write);
                // Also update calling statistics
                node.iter(&mut |inner_node| {
                    if let Node::Call(Call { id }) = inner_node {
                        *self.function_calls.entry(*id).or_default() += 1;
                        if !self.visited_functions.contains(id) {
                            pending_functions.insert(*id);
                        }
                    }
                });
            }
        }
    }

    fn add_node(&mut self, node: &Node) {
        self.update_node(node, VariableUsage::add_read, VariableUsage::add_write);

        node.iter(&mut |inner_node| {
            if let Node::Call(Call { id }) = inner_node {
                *self.function_calls.entry(*id).or_default() += 1;
            }
        });
    }

    fn remove_node(&mut self, node: &Node) {
        self.update_node(
            node,
            VariableUsage::remove_read,
            VariableUsage::remove_write,
        );

        node.iter(&mut |inner_node| {
            if let Node::Call(Call { id }) = inner_node {
                match self.function_calls.entry(*id) {
                    Entry::Occupied(entry) => *entry.into_mut() -= 1,
                    Entry::Vacant(_) => unreachable!("This function should exist"),
                }
                // println!("Removing call to {} - now at {} calls", self.function_calls.en)
            }
        });
    }

    fn update_node<FR, FW>(&mut self, node: &Node, read: FR, write: FW)
    where
        FR: Fn(&mut VariableUsage),
        FW: Fn(&mut VariableUsage),
    {
        node.variable_accesses(&mut |access| match access {
            VariableAccess::Read(ScoreboardValue::Scoreboard(_, value)) => {
                read(self.variable_information.entry(*value).or_default())
            }
            VariableAccess::Write(value) => {
                write(self.variable_information.entry(*value).or_default())
            }
            VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(_, value)) => {
                read(self.variable_information.entry(*value).or_default());
                write(self.variable_information.entry(*value).or_default());
            }
            _ => {}
        });
    }
}
