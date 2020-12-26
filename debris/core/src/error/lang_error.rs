use annotate_snippets::snippet::AnnotationType;
use debris_common::{Ident, Span, SpecialIdent};
use thiserror::Error;

use crate::{objects::ClassRef, CompileContext};

use super::{
    snippet::AnnotationOwned, AsAnnotationSnippet, SliceOwned, SnippetOwned, SourceAnnotationOwned,
};

/// A generic error which gets thrown when compiling
///
/// Contains a more specific [LangErrorKind]
#[derive(Debug, Error, Eq, PartialEq, Clone)]
pub struct LangError {
    /// The specific error
    pub kind: LangErrorKind,
    pub span: Span,
}

impl LangError {
    pub fn new(kind: LangErrorKind, span: Span) -> Self {
        LangError { kind, span }
    }
}

/// Specifies a specific error reason
#[derive(Debug, Error, Eq, PartialEq, Clone)]
pub enum LangErrorKind {
    #[error("Variable {} is already defined", .name)]
    VariableAlreadyDefined {
        name: String,
        previous_definition: Span,
    },
    #[error("Expected type {}, but received {}", .expected, .got)]
    UnexpectedType { expected: ClassRef, got: ClassRef },
    #[error("No overload was found for parameters ({})", .parameters.iter().map(|typ| format!("{}", typ)).collect::<Vec<_>>().join(", "))]
    UnexpectedOverload { parameters: Vec<ClassRef> },
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
        lhs: ClassRef,
        rhs: ClassRef,
    },
    #[error("This feature is not yet implemented: {}", .msg)]
    NotYetImplemented { msg: String },
}

impl<'a> AsAnnotationSnippet<'a> for LangError {
    fn as_annotation_snippet(&self, ctx: &'a CompileContext) -> SnippetOwned<'a> {
        let LangErrorSnippet { slices, footer } = self.kind.get_snippet(self.span, ctx);
        SnippetOwned {
            annotation_type: AnnotationType::Error,
            id: Some("Lang".into()),
            title: self.kind.to_string().into(),
            slices,
            footer,
        }
    }
}

struct LangErrorSnippet<'a> {
    slices: Vec<SliceOwned<'a>>,
    footer: Vec<AnnotationOwned<'a>>,
}

impl LangErrorKind {
    fn get_snippet<'a>(&self, span: Span, ctx: &'a CompileContext) -> LangErrorSnippet<'a> {
        let code = ctx.input_files.get_span_code(span);
        let origin = code.get_code().path.as_ref().and_then(|path| path.to_str());
        let source = code.get_code().source.as_ref();
        let range = code.get_relative_span(span);

        match self {
            LangErrorKind::VariableAlreadyDefined {
                name,
                previous_definition,
            } => {
                let other_file = ctx.input_files.get_span_code(*previous_definition);

                let mut snippet = LangErrorSnippet {
                    slices: vec![SliceOwned {
                        fold: true,
                        origin,
                        source,
                        annotations: vec![SourceAnnotationOwned {
                            annotation_type: AnnotationType::Error,
                            label: format!("The variable '{}' was defined previously", name),
                            range,
                        }],
                    }],
                    footer: vec![],
                };

                // Note the previous definition
                let other_annotation = SourceAnnotationOwned {
                    annotation_type: AnnotationType::Note,
                    label: "Previous definition here".into(),
                    range: other_file.get_relative_span(*previous_definition),
                };

                // If the previous definition was in another file, it needs its own snippet
                if other_file.file == code.file {
                    snippet.slices[0].annotations.push(other_annotation);
                } else {
                    let origin = other_file
                        .get_code()
                        .path
                        .as_ref()
                        .and_then(|path| path.to_str());
                    let source = other_file.get_code().source.as_ref();

                    snippet.slices.push(SliceOwned {
                        fold: true,

                        origin,
                        source,
                        annotations: vec![other_annotation],
                    });
                }

                snippet
            }
            LangErrorKind::UnexpectedType { expected, got } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: format!("Expected this value to be {}, but got {}", expected, got),
                        range,
                    }],
                }],
                footer: vec![],
            },
            LangErrorKind::UnexpectedOverload { parameters: _ } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "No valid overload for this function call exists".to_string(),
                        range,
                    }],
                }],
                footer: vec![],
            },
            LangErrorKind::MissingVariable { similar, var_name } => {
                let footer = match similar.as_slice() {
                    [] => None,
                    [one] => Some(format!("Did you mean: '{}'?", one).into()),
                    multiple => {
                        Some(format!("Similar names exist: {}", multiple.join(", ")).into())
                    }
                };

                LangErrorSnippet {
                    slices: vec![SliceOwned {
                        fold: true,
                        origin,
                        source,
                        annotations: vec![SourceAnnotationOwned {
                            annotation_type: AnnotationType::Error,
                            label: format!("Variable {} not found in this scope", var_name),
                            range,
                        }],
                    }],
                    footer: vec![AnnotationOwned {
                        id: None,
                        annotation_type: AnnotationType::Help,
                        label: footer,
                    }],
                }
            }
            LangErrorKind::MissingProperty {
                parent,
                property,
                similar,
            } => {
                let similar_string = match similar.as_slice() {
                    [] => None,
                    [one] => Some(format!("Did you mean '{}.{}'?", parent, one).into()),
                    multiple => {
                        Some(format!("Similar properties exist: {}", multiple.join(", ")).into())
                    }
                };

                LangErrorSnippet {
                    slices: vec![SliceOwned {
                        fold: true,
                        origin,
                        source,
                        annotations: vec![SourceAnnotationOwned {
                            annotation_type: AnnotationType::Error,
                            label: format!("The property '{}.{}' does not exist", parent, property),
                            range,
                        }],
                    }],
                    footer: vec![AnnotationOwned {
                        id: None,
                        annotation_type: AnnotationType::Help,
                        label: similar_string,
                    }],
                }
            }
            LangErrorKind::UnexpectedOperator {
                lhs,
                rhs: _,
                operator,
            } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: format!("{} does not implement operator {}", lhs, operator),
                        range,
                    }],
                }],
                footer: vec![],
            },
            LangErrorKind::NotYetImplemented { msg: _ } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "This feature is not yet implemented!".to_string(),
                        range,
                    }],
                }],
                footer: vec![AnnotationOwned {
                    id: None,
                    annotation_type: AnnotationType::Note,
                    label: Some("If you think this is a bug, please submit an issue at the github repository.".into())
                }],
            }
        }
    }
}
