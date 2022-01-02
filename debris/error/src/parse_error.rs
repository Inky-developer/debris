use std::{
    borrow::Cow,
    fmt::{self, Display, Formatter},
};

use annotate_snippets::snippet::AnnotationType;
use debris_common::Span;

use crate::CompileContext;

use super::{
    snippet::{AnnotationOwned, SliceOwned, SnippetOwned, SourceAnnotationOwned},
    AsAnnotationSnippet,
};

/// Thrown when parsing bad input
///
/// Contains the location in the source where the error occurred and what symbols were expected
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ParseError {
    /// The span where this error ocurred
    pub span: Span,
    /// Symbols which were expected instead
    pub expected: Vec<String>,
}

impl<'a> AsAnnotationSnippet<'a> for ParseError {
    fn as_annotation_snippet(&self, ctx: &'a CompileContext) -> SnippetOwned<'a> {
        let code = ctx.input_files.get_span_code(self.span);
        let footer_help = match self.expected.as_slice() {
            [] => Cow::Borrowed("Expected nothing"),
            [one] => Cow::Owned(format!("Expected {}", one)),
            multiple => Cow::Owned(format!("Expected one of: {}", multiple.join(", "))),
        };

        SnippetOwned {
            id: Some(Cow::Borrowed("Parse")),
            annotation_type: AnnotationType::Error,
            title: Cow::Borrowed("Parsing error"),
            slices: vec![SliceOwned {
                fold: true,
                origin: code.get_code().path.as_ref().and_then(|path| path.to_str()),
                source: &code.get_code().source,
                annotations: vec![SourceAnnotationOwned {
                    annotation_type: AnnotationType::Error,
                    label: "Error Here".into(),
                    range: code.get_relative_span(self.span),
                }],
            }],
            footer: vec![AnnotationOwned {
                annotation_type: AnnotationType::Info,
                id: None,
                label: Some(footer_help),
            }],
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
