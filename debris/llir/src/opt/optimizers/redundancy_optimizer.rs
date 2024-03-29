use crate::{
    item_id::ItemId,
    llir_nodes::{
        BinaryOperation, Branch, Call, Condition, ExecuteRaw, ExecuteRawComponent, FastStore,
        FastStoreFromResult, Node, VariableAccess,
    },
    minecraft_utils::{ScoreboardComparison, ScoreboardOperation, ScoreboardValue},
    opt::{
        global_opt::{Commands, GlobalOptimizer, Optimizer},
        optimize_commands::{ExecuteRawUpdate, OptimizeCommand, OptimizeCommandKind},
        NodeId,
    },
};

/// Removes useless nodes
///
/// # Optimizations:
///   - Removes assignments to variables that are never read
///   - If a value a is copied to value b and value a is assigned directly before, remove assign directly to b
///   - If a value is copied but the original value could be used, the copy gets removed
///   - If a branch's condition in-directs to another condition, inline that other condition (right now only checks the previous node)
///   - If both branches of a branch end up at the same next node, add one goto after the branch
///   - Removes function calls to functions which are empty
#[derive(Default)]
pub struct RedundancyOptimizer {
    pub aggressive_function_inlining: bool,
}

impl Optimizer for RedundancyOptimizer {
    fn optimize(&mut self, commands: &mut Commands) {
        use OptimizeCommandKind::*;

        for (node_id, node) in commands.optimizer.iter_nodes() {
            match node {
                Node::Nop => commands
                    .commands
                    .push(OptimizeCommand::new(node_id, Delete)),
                // Copies to itself (`a = a`) are useless and can be removed.
                Node::FastStore(FastStore {
                    id: target,
                    value: ScoreboardValue::Scoreboard(rhs_value),
                }) if target == rhs_value => {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete));
                }
                // Matches a write that is never read
                Node::FastStore(FastStore { id, .. }) if commands.is_id_unused(*id, node_id) => {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete));
                }
                Node::FastStoreFromResult(FastStoreFromResult { id, command, .. })
                    if commands.is_id_unused(*id, node_id) =>
                {
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
                Node::BinaryOperation(BinaryOperation {
                    id,
                    lhs: _,
                    rhs: _,
                    operation: _,
                }) if commands.is_id_unused(*id, node_id) => {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete));
                }
                // Checks if a variable is written to and then being overwritten in the same function,
                // without being read first
                Node::FastStore(FastStore { id, value: _ })
                    if write_after_write(commands.optimizer, *id, node_id) =>
                {
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete));
                }
                Node::FastStoreFromResult(FastStoreFromResult { id, command })
                    if write_after_write(commands.optimizer, *id, node_id) =>
                {
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
                // Checks if a variable y gets copied to variable x and changes all writes to y to write to x instead if
                // y is only read here
                Node::FastStore(FastStore {
                    id,
                    value: ScoreboardValue::Scoreboard(copy_from),
                }) if commands.get_info(*copy_from).reads == 1 => {
                    // Now delete this assignment and update all writes to `copy_from`
                    commands
                        .commands
                        .push(OptimizeCommand::new(node_id, Delete));
                    for (other_node_id, other_node) in commands.optimizer.iter_nodes() {
                        if other_node.writes_to(*copy_from) {
                            commands
                                .commands
                                .push(OptimizeCommand::new(other_node_id, ChangeWrite(*id)));
                        }
                    }
                    // This optimization modifies commands other than the current one,
                    // So to avoid inconsistency, return here
                    return;
                }
                Node::FastStoreFromResult(FastStoreFromResult { id, command }) => {
                    if let Node::Condition(condition) = &**command {
                        if let Some(simplified_condition) = simplify_condition(condition) {
                            let id = *id;

                            match simplified_condition {
                                SimplifiedCondition::False | SimplifiedCondition::True => {
                                    let value = i32::from(matches!(
                                        simplified_condition,
                                        SimplifiedCondition::True
                                    ));

                                    commands.commands.push(OptimizeCommand::new(
                                        node_id,
                                        Replace(Node::FastStore(FastStore {
                                            id,
                                            value: ScoreboardValue::Static(value),
                                        })),
                                    ));
                                }
                                SimplifiedCondition::NewCondition(condition) => {
                                    commands.commands.push(OptimizeCommand::new(
                                        node_id,
                                        Replace(Node::FastStoreFromResult(FastStoreFromResult {
                                            command: Box::new(Node::Condition(condition)),
                                            id,
                                        })),
                                    ));
                                }
                            }
                        } else if let Some((_, prev_node)) =
                            commands.optimizer.previous_node(&node_id)
                        {
                            if let Some((index, updated_condition)) =
                                merge_condition(prev_node, condition)
                            {
                                commands.commands.push(OptimizeCommand::new(
                                    node_id,
                                    SetCondition(updated_condition, index),
                                ));
                            }
                        }
                    }
                }
                // If the operation is commutative, move the static value to the right side.
                // This makes better consistency and is easier to implement in minecraft.
                Node::BinaryOperation(BinaryOperation {
                    id,
                    lhs: ScoreboardValue::Static(value),
                    rhs: ScoreboardValue::Scoreboard(rhs_id),
                    operation: operation @ (ScoreboardOperation::Plus | ScoreboardOperation::Times),
                }) => {
                    commands.commands.push(OptimizeCommand::new(
                        node_id,
                        Replace(Node::BinaryOperation(BinaryOperation {
                            id: *id,
                            lhs: ScoreboardValue::Scoreboard(*rhs_id),
                            rhs: ScoreboardValue::Static(*value),
                            operation: *operation,
                        })),
                    ));
                }
                // Useless math operation (a + 0, a - 0)
                Node::BinaryOperation(BinaryOperation {
                    id,
                    lhs,
                    rhs: ScoreboardValue::Static(0),
                    operation: ScoreboardOperation::Plus | ScoreboardOperation::Minus,
                }) => {
                    commands.commands.push(OptimizeCommand::new(
                        node_id,
                        Replace(Node::FastStore(FastStore {
                            id: *id,
                            value: *lhs,
                        })),
                    ));
                }
                // Useless math operation (a * 1, a / 1, a % 1)
                Node::BinaryOperation(BinaryOperation {
                    id,
                    lhs,
                    rhs: ScoreboardValue::Static(value @ 0..=1),
                    operation:
                        ScoreboardOperation::Times
                        | ScoreboardOperation::Divide
                        | ScoreboardOperation::Modulo,
                }) => {
                    let kind = match value {
                        0 => OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                            id: *id,
                            value: ScoreboardValue::Static(0),
                        })),
                        1 => OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                            id: *id,
                            value: *lhs,
                        })),
                        _ => unreachable!(),
                    };
                    commands.commands.push(OptimizeCommand::new(node_id, kind));
                }
                // `let b = a - a` => `let b = 0`
                Node::BinaryOperation(BinaryOperation {
                    id,
                    lhs: ScoreboardValue::Scoreboard(lhs),
                    rhs: ScoreboardValue::Scoreboard(rhs),
                    operation: ScoreboardOperation::Minus,
                }) if lhs == rhs => {
                    commands.commands.push(OptimizeCommand::new(
                        node_id,
                        OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                            id: *id,
                            value: ScoreboardValue::Static(0),
                        })),
                    ));
                }
                // `let b = a / a` => `let b = 1`
                Node::BinaryOperation(BinaryOperation {
                    id,
                    lhs: ScoreboardValue::Scoreboard(lhs),
                    rhs: ScoreboardValue::Scoreboard(rhs),
                    operation: ScoreboardOperation::Divide,
                }) if lhs == rhs => {
                    commands.commands.push(OptimizeCommand::new(
                        node_id,
                        OptimizeCommandKind::Replace(Node::FastStore(FastStore {
                            id: *id,
                            value: ScoreboardValue::Static(1),
                        })),
                    ));
                }
                &Node::Call(Call { id }) => {
                    let function = commands.optimizer.get_function(id);
                    if function.is_empty() {
                        commands
                            .commands
                            .push(OptimizeCommand::new(node_id, Delete));
                    } else {
                        // If the called function is only called from here, the function may be inlined.
                        // Additionally, the function may be inlined, if it is not directly recursive,
                        // and does not call the calling function
                        let calls_no_function = || {
                            commands
                                .stats
                                .call_graph
                                .get_called_functions(id)
                                .next()
                                .is_none()
                        };
                        if commands.get_call_count(id) == 1
                            || calls_no_function()
                            || (self.aggressive_function_inlining
                                && !function.calls_function(&node_id.0)
                                && !commands
                                    .infinite_loop_detector
                                    .detect_infinite_loop(&commands.optimizer.functions, id))
                        {
                            commands
                                .commands
                                .push(OptimizeCommand::new(node_id, InlineFunction));
                            // This command modifies a lot of nodes, so return here to not destroy the internal state.
                            return;
                        }
                    }
                }
                Node::Execute(ExecuteRaw(components)) => {
                    for (index, component) in components.iter().enumerate() {
                        if let &ExecuteRawComponent::Node(Node::Call(Call { id })) = component {
                            let function = commands.optimizer.get_function(id);
                            if function.is_empty() {
                                commands.commands.push(OptimizeCommand::new(
                                    node_id,
                                    UpdateExecuteRaw(index, ExecuteRawUpdate::Delete),
                                ));
                                break;
                            }
                            let calls_no_function = || {
                                commands
                                    .stats
                                    .call_graph
                                    .get_called_functions(id)
                                    .next()
                                    .is_none()
                            };
                            let has_single_node = function.nodes.len() == 1;
                            if has_single_node
                                && (commands.get_call_count(id) == 1 || calls_no_function())
                            {
                                let node = function.nodes()[0].clone();
                                commands.commands.push(OptimizeCommand::new(
                                    node_id,
                                    UpdateExecuteRaw(index, ExecuteRawUpdate::Replace(node)),
                                ));
                                break;
                            }
                        }
                    }
                }
                // If the branch does the same thing in both cases - remove it
                Node::Branch(branch) if *branch.pos_branch == *branch.neg_branch => {
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
                    let mut could_optimize = false;

                    // first try to run some trivial optimizations on the condition
                    if let Some(new_condition) = simplify_condition(condition) {
                        match new_condition {
                            SimplifiedCondition::False | SimplifiedCondition::True => {
                                let inlined_branch =
                                    matches!(new_condition, SimplifiedCondition::True);
                                commands.commands.push(OptimizeCommand::new(
                                    node_id,
                                    InlineBranch(inlined_branch),
                                ));
                            }
                            SimplifiedCondition::NewCondition(condition) => {
                                commands.commands.push(OptimizeCommand::new(
                                    node_id,
                                    UpdateBranchCondition(condition),
                                ));
                            }
                        }
                        return;
                    }

                    if let Some((prev_id, prev_node)) = commands.optimizer.previous_node(&node_id) {
                        if let Some((result_index, condition)) =
                            merge_condition(prev_node, condition)
                        {
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
                        for (branch, flag) in [(&**pos_branch, true), (&**neg_branch, false)] {
                            if let &Node::Call(Call { id }) = branch {
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

enum SimplifiedCondition {
    True,
    False,
    NewCondition(Condition),
}

impl From<bool> for SimplifiedCondition {
    fn from(val: bool) -> Self {
        match val {
            true => SimplifiedCondition::True,
            false => SimplifiedCondition::False,
        }
    }
}

/// Simplifies trivial conditions
fn simplify_condition(condition: &Condition) -> Option<SimplifiedCondition> {
    match condition {
        &Condition::Compare {
            comparison,
            lhs: ScoreboardValue::Static(lhs_val),
            rhs: ScoreboardValue::Static(rhs_val),
        } => Some(comparison.evaluate(lhs_val, rhs_val).into()),
        Condition::Compare { .. } => None,
        Condition::And(parts) | Condition::Or(parts) => {
            let is_and = matches!(condition, Condition::And(_));

            let mut new_parts = parts.clone();
            let mut anything_changed = false;
            for (index, part) in parts.iter().enumerate().rev() {
                match simplify_condition(part) {
                    None => {}
                    Some(simplified_cond) => {
                        anything_changed = true;
                        if is_and && matches!(simplified_cond, SimplifiedCondition::False)
                            || !is_and && matches!(simplified_cond, SimplifiedCondition::True)
                        {
                            return Some(simplified_cond);
                        }

                        match simplified_cond {
                            SimplifiedCondition::False | SimplifiedCondition::True => {
                                new_parts.remove(index);
                            }
                            SimplifiedCondition::NewCondition(condition) => {
                                new_parts[index] = condition;
                            }
                        }
                    }
                }
            }

            if !anything_changed {
                return None;
            }

            if new_parts.is_empty() {
                return Some(if is_and {
                    SimplifiedCondition::True
                } else {
                    SimplifiedCondition::False
                });
            }

            if new_parts.len() == 1 {
                return Some(SimplifiedCondition::NewCondition(
                    new_parts.into_iter().next().unwrap(),
                ));
            }

            match is_and {
                true => Some(SimplifiedCondition::NewCondition(Condition::And(new_parts))),
                false => Some(SimplifiedCondition::NewCondition(Condition::Or(new_parts))),
            }
        }
    }
}

/// Tries to merge the condition and the previous node into one node
/// eg: `a := b > c; d := a == 1 => d := b > c`
fn merge_condition(prev_node: &Node, condition: &Condition) -> Option<(Vec<usize>, Condition)> {
    match condition {
        Condition::Compare {
            comparison: ScoreboardComparison::Equal,
            lhs: ScoreboardValue::Scoreboard(id),
            rhs: ScoreboardValue::Static(1),
        }
        | Condition::Compare {
            comparison: ScoreboardComparison::NotEqual,
            lhs: ScoreboardValue::Scoreboard(id),
            rhs: ScoreboardValue::Static(0),
        } => {
            if let Node::FastStoreFromResult(FastStoreFromResult {
                id: cond_id,
                command,
            }) = prev_node
            {
                if id == cond_id {
                    if let Node::Condition(condition) = &**command {
                        return Some((vec![], condition.clone()));
                    }
                }
            }
        }
        Condition::Compare {
            comparison: ScoreboardComparison::Equal,
            lhs: ScoreboardValue::Scoreboard(id),
            rhs: ScoreboardValue::Static(0),
        }
        | Condition::Compare {
            comparison: ScoreboardComparison::NotEqual,
            lhs: ScoreboardValue::Scoreboard(id),
            rhs: ScoreboardValue::Static(1),
        } => {
            if let Node::FastStoreFromResult(FastStoreFromResult {
                id: cond_id,
                command,
            }) = prev_node
            {
                if id == cond_id {
                    if let Node::Condition(condition) = &**command {
                        return Some((vec![], condition.not()));
                    }
                }
            }
        }

        Condition::Or(values) | Condition::And(values) => {
            for (index, inner_condition) in values.iter().enumerate() {
                if let Some((mut result_index, condition)) =
                    merge_condition(prev_node, inner_condition)
                {
                    result_index.push(index);
                    return Some((result_index, condition));
                }
            }
        }
        Condition::Compare { .. } => (),
    }
    None
}

/// Checks for a write to `id` after `node` and returns with false
/// If it is read before a write.
/// Also returns false if no further write in this branch exists
fn write_after_write(optimizer: &GlobalOptimizer, id: ItemId, node: NodeId) -> bool {
    for (_, other_node) in optimizer.iter_at(&node) {
        let mut branches = false;
        other_node.scan(&mut |node| {
            if matches!(node, Node::Branch(_) | Node::Call(_)) {
                branches = true;
            }
        });
        if branches {
            return false;
        }

        let mut writes_id = false;
        let mut reads_id = false;
        other_node.variable_accesses(&mut |access| match access {
            VariableAccess::Read(ScoreboardValue::Scoreboard(target_id))
            | VariableAccess::ReadWrite(ScoreboardValue::Scoreboard(target_id))
                if target_id == &id =>
            {
                reads_id = true;
            }
            VariableAccess::Write(target_id, _) if target_id == &id => {
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
