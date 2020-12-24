use debris_common::LocalSpan;
use pest::Span;

use super::get_span;

/// Identifies a variable or value based on its span
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct SpannedIdentifier {
    pub span: LocalSpan,
}

/// A list of [SpannedIdentifier]s, can be a dotted path
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct IdentifierPath {
    pub idents: Vec<SpannedIdentifier>,
}

impl SpannedIdentifier {
    /// Creates a new `SpannedIdentifier` from the [LocalSpan]
    pub fn new(span: LocalSpan) -> Self {
        SpannedIdentifier { span }
    }
}

impl From<LocalSpan> for SpannedIdentifier {
    fn from(span: LocalSpan) -> Self {
        SpannedIdentifier::new(span)
    }
}

impl<'a> From<Span<'a>> for SpannedIdentifier {
    fn from(span: Span) -> Self {
        get_span(span).into()
    }
}

impl IdentifierPath {
    /// Creates a new IdentifierPath from the vec of identifiers
    pub fn new(identifiers: Vec<SpannedIdentifier>) -> Self {
        IdentifierPath {
            idents: identifiers,
        }
    }

    pub fn span(&self) -> LocalSpan {
        match self.idents.as_slice() {
            [first, .., last] => first.span.until(&last.span),
            [first] => first.span,
            [] => panic!("Expected at least one ident"),
        }
    }
}

impl From<SpannedIdentifier> for IdentifierPath {
    fn from(span: SpannedIdentifier) -> Self {
        IdentifierPath::new(vec![span])
    }
}
