#[cfg(debug_assertions)]
use std::panic::Location;
use std::{borrow::Cow, cmp::Ordering, path::PathBuf};

use annotate_snippets::snippet::AnnotationType;
use debris_common::{CompileContext, Ident, Span, SpecialIdent};
use itertools::Itertools;

use super::{
    snippet::AnnotationOwned,
    utils::{display_expected_of_all, display_expected_of_any},
    AsAnnotationSnippet, SliceOwned, SnippetOwned, SourceAnnotationOwned,
};

/// A generic error which gets thrown when compiling
///
/// Contains a more specific [LangErrorKind]
#[derive(Debug, Eq, PartialEq, Clone)]
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
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum LangErrorKind {
    UnexpectedProperty {
        property: String,
        value_class: String,
    },
    TupleMismatch {
        value_span: Span,
        lhs_count: usize,
        rhs_count: usize,
    },
    IndexOutOfBounds {
        index: i32,
        max: i32,
    },
    UnexpectedType {
        expected: Vec<String>,
        got: String,
        declared: Option<Span>,
    },
    UnexpectedStructInitializer {
        ident: Ident,
        strukt: Ident,
        available: Vec<Ident>,
    },
    MissingStructInitializer {
        strukt: Ident,
        missing: Vec<Ident>,
    },
    UnexpectedPattern {
        got: String,
    },
    UnexpectedOverload {
        parameters: Vec<String>,
        expected: Vec<Vec<String>>,
    },
    MissingVariable {
        var_name: Ident,
        similar: Vec<String>,
        notes: Vec<String>,
    },
    MissingProperty {
        property: Ident,
        parent: String,
        similar: Vec<String>,
    },
    ConstVariable {
        var_name: String,
    },
    ComptimeVariable {
        var_name: Ident,
        ctx_span: Span,
    },
    NonComptimeVariable {
        var_name: String,
        class: String,
    },
    UnexpectedOperator {
        operator: SpecialIdent,
        lhs: String,
        rhs: String,
    },
    MissingModule {
        path: PathBuf,
        error: std::io::ErrorKind,
    },
    CircularImport {
        module: String,
    },
    InvalidControlFlow {
        control_flow: String,
        requires: ControlFlowRequires,
    },
    UnreachableCode,
    InvalidConversion {
        this: String,
        target: String,
    },
    NotYetImplemented {
        msg: String,
    },
    ComptimeUpdate,
    ContinueWithValue,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ControlFlowRequires {
    Function,
    Loop,
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

impl std::error::Error for LangErrorKind {}

impl std::fmt::Display for LangErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LangErrorKind::UnexpectedProperty {
                property,
                value_class: _,
            } => {
                write!(f, "'{}' does not exist on this value", property)
            }
            LangErrorKind::TupleMismatch {
                value_span: _,
                lhs_count,
                rhs_count,
            } => write!(
                f,
                "Expected a tuple with {} elements, but got {}",
                lhs_count, rhs_count
            ),
            LangErrorKind::IndexOutOfBounds { index, max } => {
                if *index > 0 {
                    write!(f, "Index {} is out of bounds (max: {})", index, max)
                } else {
                    write!(f, "Index must be greater than 0 (got: {})", index)
                }
            }
            LangErrorKind::UnexpectedType {
                expected: _,
                got,
                declared: _,
            } => write!(f, "Received unexpected type {}", got),
            LangErrorKind::UnexpectedStructInitializer {
                ident,
                strukt,
                available: _,
            } => write!(f, "Unexpected member {} of {}", ident, strukt),
            LangErrorKind::MissingStructInitializer {
                strukt: _,
                missing: _,
            } => {
                write!(f, "Incomplete struct instantiation")
            }
            LangErrorKind::UnexpectedPattern { got } => {
                write!(f, "Expected a valid pattern or type, but got {}", got)
            }
            LangErrorKind::UnexpectedOverload {
                parameters,
                expected: _,
            } => write!(
                f,
                "No overload was found for parameters ({})",
                parameters.iter().join(", ")
            ),
            LangErrorKind::MissingVariable {
                var_name,
                similar: _,
                notes: _,
            } => write!(f, "Variable {} does not exist", var_name.to_string()),
            LangErrorKind::MissingProperty {
                property,
                parent,
                similar: _,
            } => write!(f, "Property {} of {} does not exist", property, parent),
            LangErrorKind::ConstVariable { var_name } => {
                write!(f, "Const variable \'{}\' cannot be modified", var_name)
            }
            LangErrorKind::ComptimeVariable {
                var_name,
                ctx_span: _,
            } => write!(
                f,
                "Comptime variable \'{}\' cannot be modified at runtime",
                var_name
            ),
            LangErrorKind::NonComptimeVariable { var_name, class: _ } => write!(
                f,
                "Cannot assign non-comptime value to const variable \'{}\'",
                var_name
            ),
            LangErrorKind::UnexpectedOperator { operator, lhs, rhs } => write!(
                f,
                "Operator {} is not defined between type {} and {}",
                operator, lhs, rhs
            ),
            LangErrorKind::MissingModule { path, error: _ } => {
                write!(f, "Cannot find module at {}", path.display())
            }
            LangErrorKind::CircularImport { module } => {
                write!(f, "Cannot import \'{}\' multiple times", module)
            }
            LangErrorKind::InvalidControlFlow {
                control_flow,
                requires: _,
            } => write!(f, "Invalid control flow statement: {}", control_flow),
            LangErrorKind::UnreachableCode {} => write!(f, "This code will never be executed"),
            LangErrorKind::InvalidConversion { this, target } => {
                write!(f, "Cannot convert {} to {}", this, target)
            }
            LangErrorKind::NotYetImplemented { msg } => {
                write!(f, "This feature is not yet implemented: {}", msg)
            }
            LangErrorKind::ComptimeUpdate => {
                write!(
                    f,
                    "Cannot update this variable at runtime, only at compile time"
                )
            }
            LangErrorKind::ContinueWithValue => {
                write!(f, "Cannot continue with a value")
            }
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
        let source = code.get_code().source.as_str();
        let range = code.get_relative_span(span);

        match self {
            LangErrorKind::UnexpectedProperty { value_class, property } => {
                LangErrorSnippet {
                    slices: vec![SliceOwned {
                        fold: true,
                        origin,
                        source,
                        annotations: vec![SourceAnnotationOwned {
                            annotation_type: AnnotationType::Error,
                            label: format!("{} has no property '{}'", value_class, property),
                            range,
                        }],
                    }],
                    footer: vec![],
                }
            }
            LangErrorKind::TupleMismatch {lhs_count,rhs_count, value_span} => {
                let message = match lhs_count.cmp(rhs_count) {
                    Ordering::Greater => "Not enough values to unpack",
                    Ordering::Less => "Too many values to unpack",
                    Ordering::Equal => unreachable!()
                };
                LangErrorSnippet {
                    slices: vec![SliceOwned {
                        fold: true,
                        origin ,
                        source,
                        annotations: vec![SourceAnnotationOwned {
                            annotation_type: AnnotationType::Error,
                            label: format!("{}: expected {} but got {}", message, lhs_count, rhs_count),
                            range: *value_span
                        }]
                    }],
                    footer: vec![]
                }
            }
            LangErrorKind::IndexOutOfBounds{..} => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin ,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "Invalid access here".into(),
                        range,
                    }]
                }],
                footer: vec![]
            },
            LangErrorKind::UnexpectedType { expected, got, declared } => {
                let expected_msg = display_expected_of_any(expected);
                let mut snippet = LangErrorSnippet {
                    slices: vec![SliceOwned {
                        fold: true,
                        origin,
                        source,
                        annotations: vec![SourceAnnotationOwned {
                            annotation_type: AnnotationType::Error,
                            label: format!("{}, but got {}", expected_msg, got),
                            range,
                        }],
                    }],
                    footer: vec![],
                };

                if let Some(declared) = declared {
                    snippet.slices[0].annotations.push(SourceAnnotationOwned {
                        annotation_type: AnnotationType::Info,
                        label: "Type declared here".to_string(),
                        range: code.get_relative_span(*declared)
                    });
                }

                snippet
            },
            LangErrorKind::UnexpectedStructInitializer { ident, strukt:_, available } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: format!("Unexpected member: {}", ident),
                        range,
                    }],
                }],
                footer: vec![AnnotationOwned {
                    annotation_type: AnnotationType::Help,
                    id: None,
                    label: Some(Cow::Owned(display_expected_of_any(available)))
                }],
            },
            LangErrorKind::MissingStructInitializer { missing, strukt } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: format!("Incomplete struct initialization of {}", strukt),
                        range,
                    }],
                }],
                footer: vec![AnnotationOwned {
                    annotation_type: AnnotationType::Help,
                    id: None,
                    label: Some(Cow::Owned(display_expected_of_all(missing)))
                }],
            },
            LangErrorKind::UnexpectedPattern { got } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: format!("Could not find '{}' in this scope", got),
                        range,
                    }],
                }],
                footer: vec![],
            },
            LangErrorKind::UnexpectedOverload { parameters, expected} => {
                let parameters_string = format!("({})", parameters.iter().map(|param| param.to_string()).join(", "));
                let mut possible_overloads = expected.iter().map(|params| {
                    params.iter().map(|param| param.to_string()).join(", ")
                });
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
            LangErrorKind::MissingVariable { similar, var_name, notes } => {
                let mut notes = notes.iter().map(|note| AnnotationOwned {
                    id: None,
                    annotation_type: AnnotationType::Note,
                    label: Some(note.clone().into())
                }).collect_vec();

                let footer = match similar.as_slice() {
                    [] => None,
                    [one] => Some(format!("Did you mean: '{}'?", one).into()),
                    multiple => {
                        Some(format!("Similar names exist: {}", multiple.join(", ")).into())
                    }
                };

                notes.push(AnnotationOwned {
                    id: None,
                    annotation_type: AnnotationType::Help,
                    label: footer,
                });

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
                    footer: notes,
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
            },
            LangErrorKind::ConstVariable {
                var_name,
            } => LangErrorSnippet {
                slices:
                    vec![SliceOwned {
                        fold: true,
                        origin,
                        source,
                        annotations: vec![SourceAnnotationOwned {
                            annotation_type: AnnotationType::Error,
                            label: format!("'{}' cannot be modified because it is constant", var_name),
                            range,
                        }],
                    }],
                footer: vec![],
            },
            LangErrorKind::ComptimeVariable {
                var_name,
                ctx_span,
            } => LangErrorSnippet {
                slices:
                    vec![SliceOwned {
                        fold: true,
                        origin,
                        source,
                        annotations: vec![
                            SourceAnnotationOwned {
                                annotation_type: AnnotationType::Note,
                                label: "This context cannot be evaluated at compile time".to_string(),
                                range: ctx_span.at_start(),
                            },
                            SourceAnnotationOwned {
                                annotation_type: AnnotationType::Error,
                                label: format!("'{}' cannot be modified at runtime", var_name),
                                range,
                            }
                        ],
                    }],
                footer: vec![],
            },
            LangErrorKind::NonComptimeVariable {
                class, var_name,
            } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: format!("Cannot assign non-comptime value to '{}'", var_name),
                        range,
                    }],
                }],
                footer: vec![AnnotationOwned {
                    id: None,
                    annotation_type: AnnotationType::Note,
                    label: Some(format!("The value of type {} cannot be known at compile time", class).into())
                }],
            },
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
            LangErrorKind::MissingModule {
                path: _,
                error,
            } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: match error {
                            std::io::ErrorKind::NotFound => "Cannot find this module".to_string(),
                            std::io::ErrorKind::PermissionDenied => "Cannot access this module because the permission was denied".to_string(),
                            other => format!("Error reading this module: {:?}", other)
                        },
                        range,
                    }],
                }],
                footer: vec![],
            },
            LangErrorKind::CircularImport {
                module: _
            } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "Trying to import this module multiple times".to_string(),
                        range,
                    }],
                }],
                footer: vec![],
            },
            LangErrorKind::InvalidControlFlow { control_flow, requires } => {
                let message = match requires {
                    ControlFlowRequires::Function => "only valid in a function",
                    ControlFlowRequires::Loop => "only valid in a loop",
                };
                LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: format!("{} is {}", control_flow, message),
                        range,
                    }],
                }],
                footer: vec![],
            }},
            LangErrorKind::UnreachableCode => LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![
                    SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "This code cannot be reached".to_string(),
                        range,
                    }
                    ],
                }],
                footer: vec![],
            },
            LangErrorKind::InvalidConversion{this: _, target: _} =>  LangErrorSnippet {
                slices: vec![SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![
                    SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "Implicite conversion here".to_string(),
                        range,
                    }
                    ],
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
            },
            LangErrorKind::ComptimeUpdate => LangErrorSnippet {
                slices: vec! [SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "Cannot perform runtime updates for this value".to_string(),
                        range,
                    }]
                }],
                footer: vec! [AnnotationOwned {
                    id: None,
                    annotation_type: AnnotationType::Note,
                    label: Some("".into())
                }]
            },
            LangErrorKind::ContinueWithValue => LangErrorSnippet {
                slices: vec! [SliceOwned {
                    fold: true,
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "".to_string(),
                        range,
                    }]
                }],
                footer: vec! [AnnotationOwned {
                    id: None,
                    annotation_type: AnnotationType::Note,
                    label: Some("".into())
                }]
            }
            
        }
    }
}
