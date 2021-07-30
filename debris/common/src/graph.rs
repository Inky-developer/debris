//! Very basic graph implementation which is specialized for `llir` block analysis

use std::{
    hash::BuildHasherDefault,
    ops::{Index, IndexMut},
};

use indexmap::IndexSet;
use rustc_hash::{FxHashSet, FxHasher};

/// A directed graph backed by a matrix.
/// `T is the type of weights and defaults to `()`.
/// Since this struct is only intended to be used for the specific use case
/// of control flow analysis a lot of functionality, like resizing, is not implemented.
#[derive(Debug)]
pub struct GraphMatrix<T = ()> {
    data: Box<[Option<T>]>,
    size: usize,
}

impl<T> GraphMatrix<T> {
    /// Creates a new [GraphMatrix] with a `size` rows and `size` columns.
    #[inline]
    pub fn new(size: usize) -> Self {
        let data = std::iter::repeat_with(|| None).take(size * size).collect();
        GraphMatrix { data, size }
    }
}

impl<T> GraphMatrix<T> {
    #[inline]
    pub fn get_row(&self, row: usize) -> Option<&[Option<T>]> {
        self.data
            .get((self.size * row)..(self.size * row + self.size))
    }

    #[inline]
    pub fn get_row_mut(&mut self, row: usize) -> Option<&mut [Option<T>]> {
        self.data
            .get_mut((self.size * row)..(self.size * row + self.size))
    }

    pub fn rows(&self) -> impl Iterator<Item = &[Option<T>]> {
        self.data.chunks_exact(self.size)
    }

    /// Returns the indices of all [Option::Some] variants in the given row.
    #[inline]
    pub fn edges(&self, row: usize) -> impl Iterator<Item = usize> + '_ {
        self[row]
            .iter()
            .enumerate()
            .filter_map(|(index, connected)| connected.as_ref().map(|_| index))
    }
}

impl<T> Index<usize> for GraphMatrix<T> {
    type Output = [Option<T>];

    fn index(&self, index: usize) -> &Self::Output {
        self.get_row(index).unwrap()
    }
}

impl<T> IndexMut<usize> for GraphMatrix<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_row_mut(index).unwrap()
    }
}

/// Detects loops in a graph.
/// This is a custom struct in order to save on allocations
/// in hot code.
#[derive(Default)]
pub struct GraphLoopDetector {
    to_visit: FxHashSet<u32>,
    visited: FxHashSet<u32>,
}

impl GraphLoopDetector {
    /// Returns `true` if the graph has a loop which is reachable from `node`.
    pub fn has_loop<T>(&mut self, graph: &GraphMatrix<T>, node: u32) -> bool {
        self.visited.clear();
        self.to_visit.clear();
        self.to_visit.insert(node);

        while let Some(current) = self.to_visit.iter().next() {
            let current = *current;
            self.to_visit.remove(&current);
            self.visited.insert(current);

            let edges = graph.edges(current as usize).map(|val| val as u32);
            self.to_visit.reserve(edges.size_hint().0);
            for edge in edges {
                if edge == node {
                    return true;
                }
                if !self.visited.contains(&edge) {
                    self.to_visit.insert(edge);
                }
            }
        }

        false
    }
}

/// A struct that implements a non-recursive Depth-first-search on a [GraphMatrix].
/// This is a struct in order to save on allocations in hot code.
/// The elements are yielded in post-order, i.e. every node gets only yielded if
/// all of its children are yielded.
#[derive(Default)]
pub struct GraphDfs {
    to_visit: FxHashSet<usize>,
    iter_order: IndexSet<usize, BuildHasherDefault<FxHasher>>,
}

impl GraphDfs {
    pub fn iter<'graph, 'this: 'graph, T>(
        &'this mut self,
        graph: &'graph GraphMatrix<T>,
        root: impl Iterator<Item = usize>,
    ) -> impl Iterator<Item = usize> + 'graph {
        self.to_visit.clear();
        self.iter_order.clear();
        self.to_visit.extend(root);

        while let Some(current) = self.to_visit.iter().next() {
            let current = *current;
            self.to_visit.remove(&current);
            self.iter_order.insert(current);

            for edge in graph.edges(current) {
                if !self.iter_order.contains(&edge) {
                    self.to_visit.insert(edge);
                }
            }
        }

        self.iter_order.iter().rev().cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::{GraphDfs, GraphLoopDetector, GraphMatrix};

    #[test]
    fn test_graph_matrix() {
        let mut matrix = GraphMatrix::new(4);
        matrix[0][1] = Some(());
        matrix[1][2] = Some(());
        matrix[1][3] = Some(());
        matrix[3][1] = Some(());
        matrix[3][2] = Some(());

        assert_eq!(&matrix[0], &[None, Some(()), None, None]);
        assert_eq!(&matrix[1], &[None, None, Some(()), Some(())]);
        assert_eq!(&matrix[2], &[None, None, None, None]);
        assert_eq!(&matrix[3], &[None, Some(()), Some(()), None]);
    }

    #[test]
    fn test_loop_detector() {
        let mut matrix = GraphMatrix::new(4);
        matrix[0][1] = Some(());
        matrix[1][2] = Some(());
        matrix[1][3] = Some(());
        matrix[3][1] = Some(());
        matrix[3][2] = Some(());

        assert!(!GraphLoopDetector::default().has_loop(&matrix, 0));
        assert!(GraphLoopDetector::default().has_loop(&matrix, 1));

        let mut matrix = GraphMatrix::new(4);
        matrix[0][1] = Some(());
        matrix[1][2] = Some(());
        matrix[1][3] = Some(());
        matrix[3][2] = Some(());
        matrix[3][2] = Some(());

        assert!(!GraphLoopDetector::default().has_loop(&matrix, 1));
    }

    #[test]
    fn test_graph_dfs() {
        let mut matrix = GraphMatrix::new(4);
        matrix[0][1] = Some(());
        matrix[1][2] = Some(());
        matrix[1][3] = Some(());
        matrix[3][1] = Some(());
        matrix[3][2] = Some(());

        assert_eq!(
            GraphDfs::default()
                .iter(&matrix, std::iter::once(0))
                .collect::<Vec<_>>(),
            [3, 2, 1, 0]
        );
    }
}
