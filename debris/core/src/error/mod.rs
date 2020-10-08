use std::fmt;
use std::fmt::{Display, Formatter};

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use debris_common::{Ident, Span, SpecialIdent};
use debris_type::Type;
use thiserror::Error;

/// The result type used by most of the core functions
pub type Result<T> = std::result::Result<T, CompileError>;
/// A result type which allows quick error throwing since no span and other boileplate is needed
pub type LangResult<T> = std::result::Result<T, LangErrorKind>;

#[derive(Debug, Error, Eq, PartialEq, Clone)]
pub enum CompileError {
    #[error("Could not parse the input: {}", .0)]
    ParseError(#[from] ParseError),
    #[error("Compiler Error:\n{}", .0)]
    LangError(#[from] LangError),
}

#[derive(Debug, Eq, PartialEq, Error, Clone)]
pub struct ParseError {
    pub span: Span,
    pub expected: Vec<String>,
}

#[derive(Debug, Error, Eq, PartialEq, Clone)]
pub struct LangError {
    kind: LangErrorKind,
    span: Span,
}

#[derive(Debug, Error, Eq, PartialEq, Clone)]
pub enum LangErrorKind {
    #[error("Variable {} is already defined", .name)]
    VariableAlreadyDefined {
        name: String,
        previous_definition: Span,
    },
    #[error("Expected type {}, but received {}", .expected, .got)]
    UnexpectedType { expected: Type, got: Type },
    #[error("No overload was found for parameters ({})", .parameters.iter().map(|typ| format!("{}", typ)).collect::<Vec<_>>().join(", "))]
    UnexpectedOverload { parameters: Vec<Type> },
    #[error("Variable {} does not exist", .var_name.to_string())]
    MissingVariable {
        var_name: Ident,
        similar: Vec<String>,
    },
    #[error("Property {} of {} does not exist", .property, .parent)]
    MissingProperty {
        property: Ident,
        parent: Ident,
        similar: Vec<String>,
    },
    #[error("Operator {} is not defined between type {} and {}", .operator, .lhs, .rhs)]
    UnexpectedOperator {
        operator: SpecialIdent,
        lhs: Type,
        rhs: Type,
    },
    #[error("This feature is not yet implemented: {}", .msg)]
    NotYetImplemented { msg: String },
}

// Impls
// ----------

impl CompileError {
    pub fn with_display_list<T, F: FnOnce(DisplayList) -> T>(&self, f: F) -> T {
        match self {
            CompileError::LangError(err) => err.with_display_list(|dl| (f)(dl)),
            CompileError::ParseError(err) => err.with_display_list(|dl| (f)(dl)),
        }
    }
}

impl ParseError {
    pub fn with_display_list<T, F: FnOnce(DisplayList) -> T>(&self, f: F) -> T {
        let help_label = match self.expected.as_slice() {
            [] => None,
            [one] => Some(format!("Expected {}", one)),
            _ => Some(format!("Expected one of: {}", self.expected.join(", "))),
        };

        let snippet = Snippet {
            title: Some(Annotation {
                annotation_type: AnnotationType::Error,
                id: Some("Parse"),
                label: Some("Could not parse input"),
            }),
            slices: vec![Slice {
                fold: true,
                line_start: self.span.line_start(),
                origin: self.span.code.path.as_deref(),
                source: &self.span.code.source,
                annotations: vec![SourceAnnotation {
                    annotation_type: AnnotationType::Error,
                    label: "Error here",
                    range: self.span.as_tuple(),
                }],
            }],
            footer: vec![Annotation {
                annotation_type: AnnotationType::Info,
                id: None,
                label: help_label.as_deref(),
            }],
            opt: FormatOptions {
                color: true,
                ..Default::default()
            },
        };

        let dl = DisplayList::from(snippet);

        (f)(dl)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.with_display_list(|dl| write!(f, "{}", dl))
    }
}

impl LangError {
    pub fn new(kind: LangErrorKind, span: Span) -> Self {
        LangError { kind, span }
    }

    pub fn with_display_list<T, F: FnOnce(DisplayList) -> T>(&self, f: F) -> T {
        let error_string = self.kind.to_string();
        let source_annotations = self.kind.annotations(&self.span);

        let slices = if source_annotations.len() > 0 {
            vec![Slice {
                annotations: source_annotations,
                fold: true,
                line_start: self.span.line_start(),
                origin: self.span.code.path.as_deref(),
                source: &self.span.code.source,
            }]
        } else {
            vec![]
        };

        let snippet = Snippet {
            title: Some(Annotation {
                label: Some(&error_string),
                id: Some("Compile"),
                annotation_type: AnnotationType::Error,
            }),
            footer: vec![],
            slices: slices,
            opt: FormatOptions {
                color: true,
                ..Default::default()
            },
        };

        (f)(DisplayList::from(snippet))
    }
}

impl Display for LangError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.with_display_list(|dl| f.write_str(&dl.to_string()))
    }
}

impl LangErrorKind {
    pub fn annotations<'a>(&self, span: &'a Span) -> Vec<SourceAnnotation<'a>> {
        match self {
            LangErrorKind::VariableAlreadyDefined {
                name: _,
                previous_definition,
            } => vec![
                SourceAnnotation {
                    annotation_type: AnnotationType::Error,
                    range: span.as_tuple(),
                    label: "Later defined here",
                },
                SourceAnnotation {
                    annotation_type: AnnotationType::Info,
                    range: previous_definition.as_tuple(),
                    label: "Variable previously defined here",
                },
            ],
            LangErrorKind::UnexpectedType {
                expected: _,
                got: _,
            } => vec![SourceAnnotation {
                annotation_type: AnnotationType::Error,
                range: span.as_tuple(),
                label: "Unexpected type",
            }],
            LangErrorKind::UnexpectedOverload { parameters: _ } => vec![SourceAnnotation {
                annotation_type: AnnotationType::Error,
                range: span.as_tuple(),
                label: "This function can not handle those parameters",
            }],
            LangErrorKind::MissingVariable {
                var_name: _,
                similar: _,
            } => vec![SourceAnnotation {
                annotation_type: AnnotationType::Error,
                range: span.as_tuple(),
                label: "Could not find this variable in scope",
            }],
            LangErrorKind::MissingProperty {
                property: _,
                parent: _,
                similar: _,
            } => vec![SourceAnnotation {
                annotation_type: AnnotationType::Error,
                range: span.as_tuple(),
                label: "Could not find this property",
            }],
            LangErrorKind::UnexpectedOperator {
                operator: _,
                lhs: _,
                rhs: _,
            } => vec![SourceAnnotation {
                annotation_type: AnnotationType::Error,
                range: span.as_tuple(),
                label: "This operator is not defined for these types",
            }],
            LangErrorKind::NotYetImplemented { msg: _ } => vec![SourceAnnotation {
                annotation_type: AnnotationType::Error,
                range: span.as_tuple(),
                label: "This feature is not yet implemented",
            }],
        }
    }
}
