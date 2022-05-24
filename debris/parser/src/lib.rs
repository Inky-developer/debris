use std::ops::Deref;

use debris_common::Span;

pub mod ast;
mod format_string_parser;
pub mod node;
pub mod parser;
pub mod syntax_tree;
pub mod token;

/// Wrapper over normal spans, to ensure that parser spans are not accidentally mixed with
/// hir spans
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct LocalSpan(pub Span);

impl Deref for LocalSpan {
    type Target = Span;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests;
