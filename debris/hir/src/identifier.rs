use debris_common::{CompileContext, Span};
use itertools::Itertools;

/// Identifies a variable or value based on its span
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct SpannedIdentifier {
    pub span: Span,
}

/// A list of [`SpannedIdentifier`]s, can be a dotted path
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
    /// Creates a new [`IdentifierPath`] from the vec of identifiers
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

    pub fn span_without_last(&self) -> Option<Span> {
        match self.idents.as_slice() {
            [first, .., second_last, _] => Some(first.span.until(second_last.span)),
            [second_last, _] => Some(second_last.span),
            [_] => None,
            [] => panic!("Expected at least one ident"),
        }
    }

    pub fn last(&self) -> &SpannedIdentifier {
        self.idents.last().expect("Cannot be empty")
    }

    pub fn split_at_last(mut self) -> (Option<IdentifierPath>, SpannedIdentifier) {
        let last = self.idents.pop().unwrap();
        let new_self = if self.idents.is_empty() {
            None
        } else {
            Some(self)
        };

        (new_self, last)
    }
}

impl From<SpannedIdentifier> for IdentifierPath {
    fn from(span: SpannedIdentifier) -> Self {
        IdentifierPath::new(vec![span])
    }
}
