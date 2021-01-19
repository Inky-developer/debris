use rustc_hash::FxHashMap;

use crate::{
    llir::{
        llir_impl::LLirFunction,
        llir_nodes::{BinaryOperation, Branch, FastStore, FastStoreFromResult, Function, Node},
        utils::ItemId,
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
            commands.run_optimizer(optimize_assignments)
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

    fn iter_nodes(&self) -> impl Iterator<Item = ((ContextId, usize), &Node)> + '_ {
        self.functions.iter().flat_map(|(id, func)| {
            func.nodes
                .iter()
                .enumerate()
                .map(move |(index, node)| ((*id, index), node))
        })
    }

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
                    lhs.id().map(|id| read(map, *id));
                    rhs.id().map(|id| read(map, *id));
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
                    value.id().map(|id| read(map, *id));
                }
                Node::FastStoreFromResult(FastStoreFromResult { id, command, .. }) => {
                    write(map, *id);
                    update_node(map, command.as_ref());
                }
                Node::Function(_) => {}
            }
        }

        map.clear();
        for (_node_id, node) in self.iter_nodes() {
            update_node(map, node);
        }
    }
}

enum OptimizeCommand {
    Delete((ContextId, usize)),
    // Modify((ContextId, usize), Node),
}

impl OptimizeCommand {
    fn id(&self) -> &(ContextId, usize) {
        match self {
            OptimizeCommand::Delete(id) => id,
            // OptimizeCommand::Modify(id, _) => id,
        }
    }

    /// Shifts the node id of this command one back
    fn shift_back(&mut self) {
        match self {
            OptimizeCommand::Delete(id) => id.1 -= 1,
            // OptimizeCommand::Modify(id, _) => id.1 -= 1,
        }
    }
}

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
            match command {
                OptimizeCommand::Delete(id) => {
                    // Shifts back all following nodes so that the ids still match
                    self.commands
                        .iter_mut()
                        .filter(|cmd| cmd.id().0 == id.0 && cmd.id().1 > id.1)
                        .for_each(|cmd| cmd.shift_back());
                    // Removes the node that was scheduled to be deleted
                    self.optimizer
                        .functions
                        .get_mut(&id.0)
                        .unwrap()
                        .nodes
                        .remove(id.1);
                } // OptimizeCommand::Modify(id, new_node) => {
                  //     self.optimizer.functions.get_mut(&id.0).unwrap().nodes[id.1] = new_node
                  // }
            }
        }
    }
}

/// Removes assignments for variables that are never read
fn optimize_assignments(commands: &mut Commands) {
    for (node_id, node) in commands.optimizer.iter_nodes() {
        match node {
            Node::FastStore(FastStore { id, .. }) if commands.get_info(id).reads == 0 => {
                commands.commands.push(OptimizeCommand::Delete(node_id));
            }
            Node::FastStoreFromResult(FastStoreFromResult { id, command, .. })
                if command.is_effect_free() && commands.get_info(id).reads == 0 =>
            {
                commands.commands.push(OptimizeCommand::Delete(node_id));
            }
            _ => {}
        }
    }
}
