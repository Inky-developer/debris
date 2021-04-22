use std::{cmp::Ordering, fmt, num::NonZeroU32};

use debris_common::graph::{GraphDfs, GraphLoopDetector, GraphMatrix};
use rustc_hash::FxHashMap;

use crate::llir::{
    llir_nodes::{Call, Function, Node},
    utils::BlockId,
};

pub struct CallGraph {
    graph: GraphMatrix<NonZeroU32>,
    loop_detector: GraphLoopDetector,
    visitor: GraphDfs,
}

impl CallGraph {
    pub fn from(functions: &FxHashMap<BlockId, Function>) -> Self {
        let mut graph = GraphMatrix::<NonZeroU32>::new(functions.len());

        for (block_id, function) in functions {
            for node in function.nodes() {
                if let Node::Call(Call { id }) = node {
                    match &mut graph[block_id.0][id.0] {
                        Some(cnt) => *cnt = NonZeroU32::new(cnt.get() + 1).unwrap(),
                        value @ None => *value = Some(NonZeroU32::new(1).unwrap()),
                    }
                }
            }
        }

        Self {
            graph,
            loop_detector: Default::default(),
            visitor: Default::default(),
        }
    }

    pub fn modify_call(&mut self, caller: BlockId, callee: BlockId, delta: i32) {
        match delta.cmp(&0) {
            Ordering::Equal => {}
            Ordering::Greater => match &mut self.graph[caller.0][callee.0] {
                Some(cnt) => *cnt = NonZeroU32::new(cnt.get() + delta as u32).unwrap(),
                value @ None => *value = Some(NonZeroU32::new(delta as u32).unwrap()),
            },
            Ordering::Less => match &mut self.graph[caller.0][callee.0] {
                Some(cnt) => {
                    let new_value = cnt.get().checked_sub(delta.abs() as u32).unwrap();
                    if new_value == 0 {
                        self.graph[caller.0][callee.0] = None
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

    pub fn iter_dfs(&mut self, start: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        self.visitor.iter(&self.graph, start.0).map(BlockId)
    }
}

impl fmt::Debug for CallGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, row) in self.graph.rows().enumerate() {
            write!(f, "{}: ", index)?;
            for (column, value) in row.iter().enumerate() {
                if let Some(val) = value {
                    write!(f, "{}({}) ", column, val)?;
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }
}
