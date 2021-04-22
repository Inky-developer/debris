use std::fmt;

use debris_common::graph::{GraphDfs, GraphLoopDetector, GraphMatrix};
use rustc_hash::FxHashMap;

use crate::llir::{
    llir_nodes::{Call, Function, Node},
    utils::BlockId,
};

pub struct CallGraph {
    graph: GraphMatrix,
    loop_detector: GraphLoopDetector,
    visitor: GraphDfs,
}

impl CallGraph {
    pub fn from(functions: &FxHashMap<BlockId, Function>) -> Self {
        let mut graph = GraphMatrix::<()>::new(functions.len());

        for (block_id, function) in functions {
            for node in function.nodes() {
                if let Node::Call(Call { id }) = node {
                    graph[block_id.0][id.0] = Some(());
                }
            }
        }

        Self {
            graph,
            loop_detector: Default::default(),
            visitor: Default::default(),
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
        f.debug_tuple("CallGraph").field(&self.graph).finish()
    }
}
