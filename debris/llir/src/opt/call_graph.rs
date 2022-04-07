use std::{cmp::Ordering, fmt, num::NonZeroU32};

use rustc_hash::{FxHashMap, FxHashSet};

use debris_common::graph::{GraphDfs, GraphLoopDetector, GraphMatrix};

use crate::{
    block_id::BlockId,
    llir_nodes::{Call, Function, Node},
};

fn graph_for(functions: &FxHashMap<BlockId, Function>) -> GraphMatrix<NonZeroU32> {
    let mut graph = GraphMatrix::<NonZeroU32>::new(
        (functions.keys().map(|block| block.0).max().unwrap() + 1) as usize,
    );
    for (block_id, function) in functions {
        for node in function.nodes() {
            node.scan(&mut |node| {
                if let Node::Call(Call { id }) = node {
                    match &mut graph[block_id.0 as usize][id.0 as usize] {
                        Some(cnt) => *cnt = NonZeroU32::new(cnt.get() + 1).unwrap(),
                        value @ None => *value = Some(NonZeroU32::new(1).unwrap()),
                    }
                }
            });
        }
    }

    graph
}

pub struct CallGraph {
    graph: GraphMatrix<NonZeroU32>,
    loop_detector: GraphLoopDetector,
    visitor: GraphDfs,
}

impl CallGraph {
    pub fn update(&mut self, functions: &FxHashMap<BlockId, Function>) {
        self.graph = graph_for(functions);
    }

    pub fn get_called_functions(&self, function: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        self.graph
            .edges(function.0 as usize)
            .map(|id| BlockId(id.try_into().unwrap()))
    }

    #[allow(clippy::cast_sign_loss)]
    pub fn modify_call(&mut self, caller: BlockId, called_block: BlockId, delta: i32) {
        match delta.cmp(&0) {
            Ordering::Equal => {}
            Ordering::Greater => {
                match &mut self.graph[caller.0 as usize][called_block.0 as usize] {
                    Some(cnt) => *cnt = NonZeroU32::new(cnt.get() + delta as u32).unwrap(),
                    value @ None => *value = Some(NonZeroU32::new(delta as u32).unwrap()),
                }
            }
            Ordering::Less => match &mut self.graph[caller.0 as usize][called_block.0 as usize] {
                Some(cnt) => {
                    let new_value = cnt.get().checked_sub(delta.abs() as u32).unwrap();
                    if new_value == 0 {
                        self.graph[caller.0 as usize][called_block.0 as usize] = None;
                    } else {
                        *cnt = NonZeroU32::new(new_value).unwrap();
                    }
                }
                None => unreachable!("Cannot remove call from a function which gets never called"),
            },
        }
    }

    pub fn has_loop(&mut self, start: BlockId) -> bool {
        self.loop_detector.has_loop(&self.graph, start.0)
    }

    pub fn iter_dfs<'a>(
        &'a mut self,
        root: impl Iterator<Item = BlockId> + 'a,
    ) -> impl Iterator<Item = BlockId> + 'a {
        self.visitor
            .iter(&self.graph, root.map(|id| id.0 as usize))
            .map(|val| BlockId(val.try_into().unwrap()))
    }

    /// Returns a set of all functions that can be reached from `root`
    pub fn get_reachable_from(&self, root: impl Iterator<Item = BlockId>) -> FxHashSet<BlockId> {
        let mut reachable = FxHashSet::default();
        let mut pending: FxHashSet<BlockId> = root.collect();

        while let Some(next) = pending.iter().next() {
            let next = *next;
            pending.remove(&next);
            reachable.insert(next);
            for other in self.get_called_functions(next) {
                if !reachable.contains(&other) {
                    pending.insert(other);
                }
            }
        }

        reachable
    }
}

impl From<&FxHashMap<BlockId, Function>> for CallGraph {
    fn from(functions: &FxHashMap<BlockId, Function>) -> Self {
        Self {
            graph: graph_for(functions),
            loop_detector: Default::default(),
            visitor: Default::default(),
        }
    }
}

impl fmt::Debug for CallGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, row) in self.graph.rows().enumerate() {
            write!(f, "{index}: ")?;
            for (column, value) in row.iter().enumerate() {
                if let Some(val) = value {
                    write!(f, "{column}({val}) ")?;
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct InfiniteLoopDetector {
    visited_functions: FxHashSet<BlockId>,
    pending_functions: FxHashSet<BlockId>,
}

impl InfiniteLoopDetector {
    /// This function returns true if start definitely contains an infinite loop.
    /// It might return false negatives, as it can't solve the halting problem.
    pub fn detect_infinite_loop(
        &mut self,
        functions: &FxHashMap<BlockId, Function>,
        start: BlockId,
    ) -> bool {
        self.visited_functions.clear();
        self.pending_functions.clear();

        self.pending_functions.insert(start);

        while let Some(current_block) = self.pending_functions.iter().next() {
            let current_block = *current_block;
            self.pending_functions.remove(&current_block);
            self.visited_functions.insert(current_block);

            let function = &functions[&current_block];
            for node in function.nodes() {
                if let Node::Call(Call { id }) = node {
                    if !self.visited_functions.contains(id) && !self.pending_functions.contains(id)
                    {
                        self.pending_functions.insert(*id);
                    }
                    if *id == start {
                        return true;
                    }
                }
            }
        }

        false
    }
}
