use rustc_hash::FxHashMap;

use crate::{
    llir::{
        json_format::JsonFormatComponent,
        llir_impl::LLirFunction,
        llir_nodes::{BinaryOperation, Branch, FastStore, FastStoreFromResult, Function, Node},
        utils::{ItemId, ScoreboardValue},
    },
    mir::ContextId,
};

use super::variable_metadata::VariableUsage;

/// Does optimization on the whole program.
///
/// This allows (along others) for removing unused commands
#[derive(Debug)]
pub struct GlobalOptimizer {
    functions: FxHashMap<ContextId, Function>,
    main_function: ContextId,
}

impl GlobalOptimizer {
    pub(crate) fn new(
        functions: FxHashMap<ContextId, LLirFunction>,
        main_function: ContextId,
    ) -> Self {
        GlobalOptimizer {
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
    pub fn run(mut self) -> FxHashMap<ContextId, Function> {
        self.optimize();
        self.functions
    }
}

impl GlobalOptimizer {
    fn optimize(&mut self) {
        fn run_optimize_pass(commands: &mut Commands) -> bool {
            commands.run_optimizer(optimize_redundancy)
        }

        let mut commands_vec = Vec::new();
        let mut variable_info = FxHashMap::default();

        let mut commands = Commands {
            commands: &mut commands_vec,
            variable_info: &mut variable_info,
            optimizer: self,
        };

        loop {
            let could_optimize = run_optimize_pass(&mut commands);
            if !could_optimize {
                break;
            }
        }
    }

    fn iter_nodes(&self) -> impl Iterator<Item = (NodeId, &Node)> + '_ {
        self.functions.iter().flat_map(|(id, func)| {
            func.nodes
                .iter()
                .enumerate()
                .map(move |(index, node)| ((*id, index), node))
        })
    }

    // fn get_function(&self, id: &ContextId) -> &Function {
    //     &self.functions[id]
    // }

    /// The optimizer keeps a list of all writes and stores for every variable. This function
    /// computes that list
    fn update_variable_information(&self, map: &mut FxHashMap<ItemId, VariableUsage>) {
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
                    if let Some(neg_branch) = neg_branch {
                        update_node(map, neg_branch);
                    }
                    condition.accessed_variables(&mut |var| read(map, *var));
                }
                Node::Call(_) => {}
                Node::Condition(condition) => {
                    condition.accessed_variables(&mut |var| read(map, *var));
                }
                // As in Peephole, assume that the execute node does not interfere in any way
                Node::Execute(_) => {}
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
                Node::Function(_) => {}
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

        map.clear();
        for (_node_id, node) in self.iter_nodes() {
            update_node(map, node);
        }
    }
}

/// The optimizer can uniquely identify each node with this type
type NodeId = (ContextId, usize);

/// Optimizing functions output commands that tell the optimizer what to do,
/// this is done so that there are no troubles with mutability
enum OptimizeCommandKind {
    /// Deletes a single node
    Delete,
    /// Changes the variable this node writes to
    ChangeWrite(ItemId),
    /// Replaces the old node completely
    Replace(Node),
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
    fn shift_back(&mut self) {
        self.id.1 -= 1;
    }
}

/// Interface for optimizing functions to get data about the code and emit
/// optimization instructions
struct Commands<'opt> {
    optimizer: &'opt mut GlobalOptimizer,
    variable_info: &'opt mut FxHashMap<ItemId, VariableUsage>,
    commands: &'opt mut Vec<OptimizeCommand>,
}

impl<'opt> Commands<'opt> {
    /// Returns the variable info for this node
    fn get_info(&self, var: &ItemId) -> &VariableUsage {
        self.variable_info.get(var).expect("Unknown variable")
    }

    /// Runs an optimizing function
    ///
    /// Returns whether this function could optimize anything
    fn run_optimizer<F>(&mut self, optimizer: F) -> bool
    where
        F: Fn(&mut Commands),
    {
        self.optimizer
            .update_variable_information(self.variable_info);
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
                OptimizeCommandKind::ChangeWrite(new_target) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    node.set_write_to(new_target);
                }
                OptimizeCommandKind::Replace(new_node) => {
                    let node = &mut self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1];
                    *node = new_node;
                }
            }
        }
    }
}

/// Removes useless nodes
///
/// # Optimizations:
///   - Removes assignments to variables that are never read
///   - If a value a is copied to value b and value a is only ever read one (in that copy),
///     removes value a and replaces it with value b
///   - inlines branch blocks if they only contain one inlinable node
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
            _ => {}
        }
    }
}
