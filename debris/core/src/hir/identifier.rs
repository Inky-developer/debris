use debris_common::Span;
use itertools::Itertools;

use crate::CompileContext;

/// Identifies a variable or value based on its span
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct SpannedIdentifier {
    pub span: Span,
}

/// A list of [SpannedIdentifier]s, can be a dotted path
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct IdentifierPath {
    idents: Vec<SpannedIdentifier>,
}

impl SpannedIdentifier {
    /// Creates a new `SpannedIdentifier` from the [Span]
    pub fn new(span: Span) -> Self {
        SpannedIdentifier { span }
    }
}

impl From<Span> for SpannedIdentifier {
    fn from(span: Span) -> Self {
        SpannedIdentifier::new(span)
    }
}

impl IdentifierPath {
    /// Creates a new IdentifierPath from the vec of identifiers
    pub fn new(identifiers: Vec<SpannedIdentifier>) -> Self {
        assert!(
            !identifiers.is_empty(),
            "An identifier path must not be empty"
        );
        IdentifierPath {
            idents: identifiers,
        }
    }

    pub fn idents(&self) -> &[SpannedIdentifier] {
        &self.idents
    }

    pub fn single_ident(&self) -> Option<&SpannedIdentifier> {
        match self.idents.as_slice() {
            [] => None,
            [one] => Some(one),
            _ => None,
        }
    }

    pub fn display(&self, ctx: &CompileContext) -> String {
        self.idents
            .iter()
            .map(|ident| ctx.input_files.get_span_str(ident.span))
            .join(".")
    }

    pub fn span(&self) -> Span {
        match self.idents.as_slice() {
            [first, .., last] => first.span.until(last.span),
            [first] => first.span,
            [] => panic!("Expected at least one ident"),
        }
    }

    pub fn last(&self) -> &SpannedIdentifier {
        self.idents.last().expect("Cannot be empty")
    }
}

impl From<SpannedIdentifier> for IdentifierPath {
    fn from(span: SpannedIdentifier) -> Self {
        IdentifierPath::new(vec![span])
    }
}
