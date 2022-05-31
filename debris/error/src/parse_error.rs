use std::{borrow::Cow, num::ParseIntError};

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
pub enum ParseError {
    InvalidIntLiteral {
        span: Span,
        error: ParseIntError,
    },
    LeftoverInput {
        span: Span,
    },
    UnexpectedComma {
        span: Span,
    },
    UnexpectedFunctionIdent {
        span: Span,
    },
    UnexpectedPath {
        span: Span,
    },
    UnexpectedToken {
        /// The span where this error ocurred
        span: Span,
        /// Symbols which were expected instead
        expected: Vec<String>,
    },
}

impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::InvalidIntLiteral { span, .. }
            | ParseError::LeftoverInput { span }
            | ParseError::UnexpectedComma { span }
            | ParseError::UnexpectedFunctionIdent { span }
            | ParseError::UnexpectedPath { span }
            | ParseError::UnexpectedToken { span, .. } => *span,
        }
    }
}

impl<'a> AsAnnotationSnippet<'a> for ParseError {
    fn as_annotation_snippet(&self, ctx: &'a CompileContext) -> SnippetOwned<'a> {
        let code = ctx.input_files.get_span_code(self.span());
        let footer_help = match self {
            ParseError::InvalidIntLiteral { .. } => "Invalid int literal".into(),
            ParseError::LeftoverInput { .. } => "Could not parse leftover input".into(),
            ParseError::UnexpectedComma { .. } => "Expected no comma here".into(),
            ParseError::UnexpectedFunctionIdent { .. } => {
                "Functions in expression position should be anonymous".into()
            }
            ParseError::UnexpectedPath { .. } => "Paths are not allowed here".into(),
            ParseError::UnexpectedToken { expected, .. } => match expected.as_slice() {
                [] => Cow::Borrowed("Expected nothing"),
                [one] => Cow::Owned(format!("Expected {}", one)),
                multiple => Cow::Owned(format!("Expected one of: {}", multiple.join(", "))),
            },
        };

        let mut footer = vec![AnnotationOwned {
            annotation_type: AnnotationType::Info,
            id: None,
            label: Some(footer_help),
        }];

        match self {
            ParseError::UnexpectedComma { .. } => footer.push(AnnotationOwned {
                annotation_type: AnnotationType::Help,
                id: None,
                label: Some("Try removing the comma".into()),
            }),
            ParseError::UnexpectedFunctionIdent { .. } => {
                footer.push(AnnotationOwned {
                    annotation_type: AnnotationType::Help,
                    id: None,
                    label: Some("Try removing the function name".into()),
                });
            }
            ParseError::InvalidIntLiteral { error, .. } => {
                footer.push(AnnotationOwned {
                    annotation_type: AnnotationType::Help,
                    id: None,
                    label: Some(error.to_string().into()),
                });
            }

            _ => {}
        }

        SnippetOwned {
            id: Some(Cow::Borrowed("Parse")),
            annotation_type: AnnotationType::Error,
            title: Cow::Borrowed("Parsing error"),
            slices: vec![SliceOwned {
                origin: code.get_code().path.as_deref(),
                source: &code.get_code().source,
                annotations: vec![SourceAnnotationOwned {
                    annotation_type: AnnotationType::Error,
                    label: "Error Here".into(),
                    range: code.get_relative_span(self.span()).unwrap(),
                }],
            }],
            footer,
        }
    }
}
