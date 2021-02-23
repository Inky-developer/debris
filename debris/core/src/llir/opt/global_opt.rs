use std::{cmp::Ordering, collections::HashMap};

use rustc_hash::FxHashMap;

use crate::{
    llir::{
        json_format::JsonFormatComponent,
        llir_impl::LLirFunction,
        llir_nodes::{
            BinaryOperation, Branch, Call, ExecuteRawComponent, FastStore, FastStoreFromResult,
            Function, Node,
        },
        utils::{BlockId, ItemId, ScoreboardValue},
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
    functions: FxHashMap<BlockId, Function>,
    main_function: BlockId,
}

impl<'a> GlobalOptimizer<'a> {
    pub(crate) fn new(
        config: &'a Config,
        functions: FxHashMap<BlockId, LLirFunction>,
        main_function: BlockId,
    ) -> Self {
        GlobalOptimizer {
            config,
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
}

/// The optimizer can uniquely identify each node with this type
type NodeId = (BlockId, usize);

/// Optimizing functions output commands that tell the optimizer what to do,
/// this is done so that there are no troubles with mutability
enum OptimizeCommandKind {
    /// Deletes a single node
    Delete,
    /// Deletes a single function
    RemoveFunction,
    /// Changes the variable this node writes to
    ChangeWrite(ItemId),
    /// Replaces the old node completely
    Replace(Node),
    /// Inserts this node after
    InsertAfter(Node),
}

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
            Ordering::Less => self.id.1 -= amt as usize,
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
        let main_function = self.optimizer.main_function;
        // ToDo: Only update the parts that changed since the last pass
        self.stats
            .update(main_function, self.optimizer.iter_nodes());
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
                OptimizeCommandKind::RemoveFunction => {
                    self.optimizer.functions.remove(&id.0);
                }
                OptimizeCommandKind::ChangeWrite(new_target) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    node.set_write_to(new_target);
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
///   - If a value a is copied to value b and value a is only ever read onc22e (in that copy),
///     removes value a and replaces it with value b
///   - if both branches of a condition go to the same next node, add one goto after the branch
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
                if command.is_effect_free() && commands.get_info(id).reads == 0 =>
            {
                commands
                    .commands
                    .push(OptimizeCommand::new(node_id, Delete));
            }
            // A variable that copies its value to another node and is never read
            // apart from that copy
            Node::FastStore(FastStore {
                id: new_id,
                value: ScoreboardValue::Scoreboard(_, copy_from),
                ..
            }) if commands.get_info(copy_from).reads == 1
                && commands.get_info(new_id).writes == 1 =>
            {
                // set the write target for every node from copy_from to id
                for (other_node_id, other_node) in commands.optimizer.iter_nodes() {
                    let writes_old_id = other_node
                        .get_write()
                        .map_or(false, |item| item == copy_from);

                    if writes_old_id {
                        commands
                            .commands
                            .push(OptimizeCommand::new(other_node_id, ChangeWrite(*new_id)));
                    }
                }
                commands
                    .commands
                    .push(OptimizeCommand::new(node_id, Delete));

                // Unfortunately we might need to return with this optimization since the `new_id`
                // Could also only have one read and then things might get out of sync
                if commands.get_info(new_id).reads == 1 {
                    return;
                }
            }
            // Similar to the above optimization, but matches node of the form `x = a op static_value`,
            // where `a` does not actually need to survive
            Node::BinaryOperation(BinaryOperation {
                id: new_id,
                lhs: ScoreboardValue::Scoreboard(lhs_scoreboard, copy_from),
                rhs: rhs @ ScoreboardValue::Static(_),
                operation,
                scoreboard,
            }) if commands.get_info(copy_from).reads == 1 => {
                // set the write target for every node from copy_from to id
                for (other_node_id, other_node) in commands.optimizer.iter_nodes() {
                    let writes_old_id = other_node
                        .get_write()
                        .map_or(false, |item| item == copy_from);

                    if writes_old_id {
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
            _ => {}
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

    fn update<'a>(
        &mut self,
        main_function: BlockId,
        nodes: impl Iterator<Item = (NodeId, &'a Node)>,
    ) {
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
        *self.function_calls.entry(main_function).or_default() += 1;
    }
}
