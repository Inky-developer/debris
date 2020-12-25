use debris_common::Span;

/// Identifies a variable or value based on its span
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct SpannedIdentifier {
    pub span: Span,
}

/// A list of [SpannedIdentifier]s, can be a dotted path
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct IdentifierPath {
    pub idents: Vec<SpannedIdentifier>,
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
        IdentifierPath {
            idents: identifiers,
        }
    }

    pub fn span(&self) -> Span {
        match self.idents.as_slice() {
            [first, .., last] => first.span.until(last.span),
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
