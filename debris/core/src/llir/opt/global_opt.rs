use std::{cmp::Ordering, collections::HashMap};

use rustc_hash::FxHashMap;

use crate::{
    llir::{
        json_format::JsonFormatComponent,
        llir_impl::LLirFunction,
        llir_nodes::{
            BinaryOperation, Branch, Call, Condition, ExecuteRawComponent, FastStore,
            FastStoreFromResult, Function, Node,
        },
        utils::{BlockId, ItemId, Scoreboard, ScoreboardComparison, ScoreboardValue},
        Runtime,
    },
    Config, OptMode,
};

use super::variable_metadata::VariableUsage;

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
        fn run_optimize_pass(commands: &mut Commands) -> bool {
            commands.run_optimizer(optimize_unused_code)
                | commands.run_optimizer(optimize_redundancy)
                | commands.run_optimizer(optimize_common_path)
        }

        let mut commands_vec = Vec::new();
        let mut variable_info = Default::default();

        let mut commands = Commands {
            commands: &mut commands_vec,
            stats: &mut variable_info,
            optimizer: self,
        };

        loop {
            let could_optimize = run_optimize_pass(&mut commands);
            if !could_optimize {
                break;
            }
        }
    }

    fn get_function(&self, id: &BlockId) -> &Function {
        self.functions.get(id).unwrap()
    }

    fn get_function_mut(&mut self, id: &BlockId) -> &mut Function {
        self.functions.get_mut(id).unwrap()
    }

    /// Iterates over all nodes (minus inner nodes)
    fn iter_nodes(&self) -> impl Iterator<Item = (NodeId, &Node)> + '_ {
        self.functions.iter().flat_map(|(id, func)| {
            func.nodes
                .iter()
                .enumerate()
                .map(move |(index, node)| ((*id, index), node))
        })
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
    /// Deletes a single function
    RemoveFunction,
    /// Changes the variable this node writes to
    ChangeWrite(ItemId),
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
    fn shift(&mut self, amt: isize) {
        match amt.cmp(&0) {
            Ordering::Greater => self.id.1 += amt as usize,
            Ordering::Less => self.id.1 -= amt.abs() as usize,
            Ordering::Equal => (),
        }
    }

    fn shift_back(&mut self) {
        self.shift(-1)
    }
    fn shift_forward(&mut self) {
        self.shift(1)
    }
}

/// Interface for optimizing functions to get data about the code and emit
/// optimization instructions
struct Commands<'opt, 'ctx> {
    optimizer: &'opt mut GlobalOptimizer<'ctx>,
    stats: &'opt mut CodeStats,
    commands: &'opt mut Vec<OptimizeCommand>,
}

impl<'opt> Commands<'opt, '_> {
    /// Returns the variable info for this node
    fn get_info(&self, var: &ItemId) -> &VariableUsage {
        self.stats
            .variable_information
            .get(var)
            .expect("Unknown variable")
    }

    fn get_call_count(&self, function: &BlockId) -> usize {
        *self.stats.function_calls.get(function).unwrap_or(&0)
    }

    /// Runs an optimizing function
    ///
    /// Returns whether this function could optimize anything
    fn run_optimizer<F>(&mut self, optimizer: F) -> bool
    where
        F: Fn(&mut Commands),
    {
        // ToDo: Only update the parts that changed since the last pass
        self.stats
            .update(self.optimizer.runtime, self.optimizer.iter_nodes());
        optimizer(self);
        let len = self.commands.len();
        self.execute_commands();
        len > 0
    }

    /// Execute every command that is in the current command stack
    fn execute_commands(&mut self) {
        while let Some(command) = self.commands.pop() {
            let id = command.id;
            match command.kind {
                OptimizeCommandKind::Delete => {
                    // Shifts back all following nodes so that the ids still match
                    self.commands
                        .iter_mut()
                        .filter(|cmd| cmd.id.0 == id.0 && cmd.id.1 > id.1)
                        .for_each(|cmd| cmd.shift_back());
                    // Removes the node that was scheduled to be deleted
                    self.optimizer
                        .functions
                        .get_mut(&id.0)
                        .unwrap()
                        .nodes
                        .remove(id.1);
                }
                OptimizeCommandKind::DiscardResult => {
                    let nodes = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes;
                    let old_value = std::mem::replace(&mut nodes[id.1], Node::Nop);
                    nodes[id.1] = match old_value {
                        Node::FastStoreFromResult(store) => *store.command,
                        other => panic!("Invalid node: {:?}", other),
                    }
                }
                OptimizeCommandKind::RemoveFunction => {
                    self.optimizer.functions.remove(&id.0);
                }
                OptimizeCommandKind::ChangeWrite(new_target) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    node.set_write_to(new_target);
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

                            *old_condition = condition;
                        }
                        other => panic!("Invalid node: {:?}", other),
                    }
                }
                OptimizeCommandKind::Replace(new_node) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    *node = new_node;
                }
                OptimizeCommandKind::InsertAfter(next_node) => {
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

/// Removes useless nodes
///
/// # Optimizations:
///   - Removes assignments to variables that are never read
///   - If a value a is copied to value b and value a is assigned directly before, remove assign directly to b
///   - If a branch's condition indirects to another condition, inline that other condition (right now only checks the previous node)
///   - If both branches of a branch end up at the same next node, add one goto after the branch
///   - Removes function calls to functions which are empty
fn optimize_redundancy(commands: &mut Commands) {
    use OptimizeCommandKind::*;

    for (node_id, node) in commands.optimizer.iter_nodes() {
        match node {
            Node::FastStore(FastStore { id, .. }) if commands.get_info(id).reads == 0 => {
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
            Node::BinaryOperation(BinaryOperation {
                id: new_id,
                lhs: ScoreboardValue::Scoreboard(lhs_scoreboard, copy_from),
                rhs: rhs @ ScoreboardValue::Static(_),
                operation,
                scoreboard,
            }) if commands.get_info(copy_from).reads == 1 => {
                // set the write target for every node from copy_from to id
                for (other_node_id, other_node) in commands.optimizer.iter_nodes() {
                    if other_node.writes_to(copy_from) {
                        commands
                            .commands
                            .push(OptimizeCommand::new(other_node_id, ChangeWrite(*new_id)));
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
                if commands.get_info(new_id).reads == 1 {
                    return;
                }
            }
            Node::Call(Call { id }) => {
                let function = commands.optimizer.get_function(id);
                if function.is_empty() {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, OptimizeCommandKind::Delete))
                }
            }
            // Checks if the branch depends on a condition that was just calculated
            // ToDo: Instead of only checking the last condition, check as long as the condition is valid
            Node::Branch(Branch { condition, .. }) => {
                fn simplify_condition(
                    optimize_commands: &mut Vec<OptimizeCommand>,
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
                    if commands.get_call_count(&current_block) <= 1
                        && commands.get_call_count(id) <= 1
                    {
                        calls.insert(*id, (current_block, function.nodes().len() - 1));
                    }
                    current_block = *id;
                } else {
                    break;
                }
            }
            calls
        };

        // This part might cause an infinite loop once I implement loops
        // But I don't care until it really happens
        let mut current_block = block_b;
        loop {
            let function = commands.optimizer.get_function(&current_block);
            match function.nodes().last() {
                Some(Node::Call(Call { id })) => {
                    if let Some(a_call) = a_calls.get(id) {
                        return Some((*id, *a_call, (current_block, function.nodes().len() - 1)));
                    } else {
                        current_block = *id;
                    }
                }
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
        if commands.get_call_count(id) == 0 {
            commands.commands.push(OptimizeCommand::new(
                (*id, 0),
                OptimizeCommandKind::RemoveFunction,
            ));
        }
    }
}

/// tracks statistics about the global code which can be used
/// to allow some optimizations
#[derive(Debug, Default)]
struct CodeStats {
    variable_information: FxHashMap<ItemId, VariableUsage>,
    function_calls: FxHashMap<BlockId, usize>,
}

impl CodeStats {
    fn clear(&mut self) {
        self.variable_information.clear();
        self.function_calls.clear();
    }

    fn update<'a>(&mut self, runtime: &Runtime, nodes: impl Iterator<Item = (NodeId, &'a Node)>) {
        fn read(map: &mut FxHashMap<ItemId, VariableUsage>, item: ItemId) {
            map.entry(item).or_default().add_read()
        }

        fn write(map: &mut FxHashMap<ItemId, VariableUsage>, item: ItemId) {
            map.entry(item).or_default().add_write()
        }

        fn update_node(map: &mut FxHashMap<ItemId, VariableUsage>, node: &Node) {
            match node {
                Node::BinaryOperation(BinaryOperation { id, lhs, rhs, .. }) => {
                    write(map, *id);
                    if let Some(id) = lhs.id() {
                        read(map, *id)
                    }
                    if let Some(id) = rhs.id() {
                        read(map, *id)
                    }
                }
                Node::Branch(Branch {
                    condition,
                    pos_branch,
                    neg_branch,
                }) => {
                    update_node(map, pos_branch.as_ref());
                    update_node(map, neg_branch.as_ref());
                    condition.accessed_variables(&mut |var| read(map, *var));
                }
                Node::Call(_) => {}
                Node::Condition(condition) => {
                    condition.accessed_variables(&mut |var| read(map, *var));
                }
                Node::Execute(execute) => {
                    for part in execute.0.iter() {
                        if let ExecuteRawComponent::ScoreboardValue(ScoreboardValue::Scoreboard(
                            _,
                            id,
                        )) = part
                        {
                            read(map, *id)
                        }
                    }
                }
                Node::FastStore(FastStore { id, value, .. }) => {
                    write(map, *id);
                    if let Some(id) = value.id() {
                        read(map, *id)
                    };
                }
                Node::FastStoreFromResult(FastStoreFromResult { id, command, .. }) => {
                    write(map, *id);
                    update_node(map, command.as_ref());
                }
                Node::Write(write) => {
                    let read_scores =
                        write
                            .message
                            .components
                            .iter()
                            .filter_map(|component| match component {
                                JsonFormatComponent::RawText(_) => None,
                                JsonFormatComponent::Score(_scoreboard, id) => Some(id),
                            });

                    for score in read_scores {
                        read(map, *score);
                    }
                }
                Node::Nop => {}
            }
        }

        self.clear();
        for (_node_id, node) in nodes {
            update_node(&mut self.variable_information, node);

            // Also update calling statistics
            node.iter(&mut |inner_node| {
                if let Node::Call(Call { id }) = inner_node {
                    *self.function_calls.entry(*id).or_default() += 1;
                }
            });
        }

        for on_load_block in &runtime.load_blocks {
            *self.function_calls.entry(*on_load_block).or_default() += 1;
        }

        for on_tick_block in &runtime.scheduled_blocks {
            *self.function_calls.entry(*on_tick_block).or_default() += 1;
        }
    }
}
