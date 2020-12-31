use std::borrow::Cow;
#[allow(unused_imports)]
use std::panic::Location;

use annotate_snippets::snippet::AnnotationType;
use debris_common::{Ident, Span, SpecialIdent};
use itertools::Itertools;
use thiserror::Error;

use crate::{
    objects::{ClassRef, FunctionParameters},
    CompileContext, TypePattern,
};

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
    /// In debug mode stores the caller to provide additional
    /// debugging help
    #[cfg(debug_assertions)]
    caller: &'static Location<'static>,
}

impl LangError {
    #[cfg(not(debug_assertions))]
    pub fn new(kind: LangErrorKind, span: Span) -> Self {
        LangError { kind, span }
    }

    #[cfg(debug_assertions)]
    #[track_caller]
    pub fn new(kind: LangErrorKind, span: Span) -> Self {
        LangError {
            kind,
            span,
            caller: Location::caller(),
        }
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
    UnexpectedType {
        expected: TypePattern,
        got: ClassRef,
        declared: Option<Span>,
    },
    #[error("Expected a valid pattern or type, but got {}", .got)]
    UnexpectedPattern { got: String },
    #[error("Cannot convert from type {} to {}", .got, .target)]
    UnexpectedConversion {
        got: ClassRef,
        target: ClassRef,
        note: String,
    },
    #[error("No overload was found for parameters ({})", .parameters.iter().map(|typ| format!("{}", typ)).collect::<Vec<_>>().join(", "))]
    UnexpectedOverload {
        parameters: Vec<ClassRef>,
        expected: Vec<(FunctionParameters, ClassRef)>,
    },
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
        #[allow(unused_mut)]
        let LangErrorSnippet { slices, mut footer } = self.kind.get_snippet(self.span, ctx);

        #[cfg(debug_assertions)]
        footer.push(AnnotationOwned {
            annotation_type: AnnotationType::Info,
            id: None,
            label: Some(Cow::Owned(format!("Error thrown at {}", self.caller))),
        });

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
            LangErrorKind::UnexpectedType { expected, got, declared } => {
                let mut snippet = 
                LangErrorSnippet {
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
                };

                if let Some(declared) = declared {
                    snippet.slices[0].annotations.push(SourceAnnotationOwned {
                        annotation_type: AnnotationType::Info,
                        label: format!("Here declared as {}", expected),
                        range: code.get_relative_span(*declared)
                    });
                }

                snippet
            },
            LangErrorKind::UnexpectedPattern { got } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: format!("Expected this value to be a function parameter pattern, but got {}", got),
                        range,
                    }],
                }],
                footer: vec![],
            },
            LangErrorKind::UnexpectedConversion{got:_,note, target} => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: format!("Cannot convert to {}", target),
                        range
                    }]
                }],
                footer: vec![AnnotationOwned {
                    annotation_type: AnnotationType::Note,
                    id: None,
                    label: Some(Cow::Owned(note.clone()))
                }]
            },
            LangErrorKind::UnexpectedOverload { parameters , expected} => {
                let mut possible_overloads = expected.iter().map(|(params, _ret)| {
                    format!("({})", match params {
                        FunctionParameters::Any => Cow::Borrowed("{Any}"),
                        FunctionParameters::Specific(params) => params.iter().map(|param| format!("{:?}", param)).join(", ").into()
                    })
                });

                let parameters_string = format!("({})", parameters.iter().map(|param| param.to_string()).join(", "));

                let message = if expected.len() == 1 {
                    format!("Got {} but expected {}", parameters_string, possible_overloads.next().unwrap())
                } else {
                    format!("Expected one of:\n  * {}",  possible_overloads.join("\n  * "))
                };

                LangErrorSnippet {
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
                    footer: vec![AnnotationOwned {
                        annotation_type: AnnotationType::Note,
                        id: None,
                        label: Some(message.into())
                    }],
                }
            }
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
