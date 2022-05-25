//! High-level intermediate representation
//!
//! This intermediate representation is very similar to a typical abstract syntax tree,
//! but with some desugaring applied.

mod hir_impl;
pub mod hir_nodes;

mod hir_context;
use debris_common::{CodeId, Ident, Span};
pub use hir_context::HirContext;

use hir_nodes::{HirBlock, HirModule};

mod identifier;
pub use identifier::{IdentifierPath, SpannedIdentifier};

pub use hir_impl::HirFile;
use indexmap::IndexSet;

/// The hir representation of an input file and all of its dependencies
#[derive(Debug)]
pub struct Hir {
    pub main_function: HirBlock,
    pub code_id: CodeId,
    pub imported_modules: Vec<HirModule>,
}

/// Keeps track of all imported modules, uses indexes as keys
#[derive(Debug, Default)]
pub struct ImportDependencies {
    modules: IndexSet<Ident>,
    /// The spans that correspond to the modules.
    /// Access via the index of the module
    spans: Vec<Span>,
}

impl ImportDependencies {
    /// Inserts a dependency and the code span and returns its index
    pub fn insert(&mut self, value: Ident, span: Span) -> usize {
        let (index, inserted) = self.modules.insert_full(value);

        // If the module is already listed,
        // ignore the span of the second import
        if inserted {
            self.spans.push(span);
        }

        index
    }

    pub fn len(&self) -> usize {
        self.modules.len()
    }

    pub fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }

    pub fn get(&self, index: usize) -> (&Ident, Span) {
        (&self.modules[index], self.spans[index])
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Ident, Span)> {
        self.modules
            .iter()
            .enumerate()
            .map(move |(index, ident)| (ident, self.spans[index]))
    }
}

#[cfg(test)]
mod tests {
    // TODO: Rerun parser tests
}
