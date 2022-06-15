#[cfg(debug_assertions)]
use std::panic::Location;
use std::{borrow::Cow, cmp::Ordering};

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
/// Contains a more specific [`LangErrorKind`]
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
        lhs_count: usize,
        rhs_count: usize,
    },
    IndexOutOfBounds {
        index: i32,
        max: i64,
    },
    ImmutableProperty,
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
    UnexpectedOverload {
        parameters: Vec<String>,
        expected: Vec<Vec<String>>,
        function_definition_span: Option<Span>,
    },
    MissingVariable {
        var_name: Ident,
        similar: Vec<String>,
        notes: Vec<String>,
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
        path: String,
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
    NotYetImplemented {
        msg: String,
    },
    ComptimeUpdate,
    ComptimeCall,
    InvalidComptimeBranch,
    ContinueWithValue,
    InvalidExternItemPath {
        path: String,
        error: String,
    },
    FunctionAlreadyExported,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ControlFlowRequires {
    Function,
    Loop,
}

impl<'a> AsAnnotationSnippet<'a> for LangError {
    #[allow(unused_mut)]
    fn as_annotation_snippet(&self, ctx: &'a CompileContext) -> SnippetOwned<'a> {
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
                write!(f, "'{property}' does not exist on this value")
            }
            LangErrorKind::TupleMismatch {
                lhs_count,
                rhs_count,
            } => write!(
                f,
                "Expected a tuple with {lhs_count} elements, but got {rhs_count}"
            ),
            LangErrorKind::IndexOutOfBounds { index, max } => {
                if *index > 0 {
                    write!(f, "Index {index} is out of bounds (max: {max})")
                } else {
                    write!(f, "Index must be greater than 0 (got: {index})")
                }
            }
            LangErrorKind::ImmutableProperty => write!(f, "Cannot change immutable value"),
            LangErrorKind::UnexpectedType {
                expected: _,
                got,
                declared: _,
            } => write!(f, "Received unexpected type {got}"),
            LangErrorKind::UnexpectedStructInitializer {
                ident,
                strukt,
                available: _,
            } => write!(f, "Unexpected member {ident} of {strukt}"),
            LangErrorKind::MissingStructInitializer {
                strukt: _,
                missing: _,
            } => {
                write!(f, "Incomplete struct instantiation")
            }
            LangErrorKind::UnexpectedOverload {
                parameters,
                expected: _,
                function_definition_span: _,
            } => write!(
                f,
                "No overload was found for parameters ({})",
                parameters.iter().join(", ")
            ),
            LangErrorKind::MissingVariable {
                var_name,
                similar: _,
                notes: _,
            } => write!(f, "Variable '{var_name}' does not exist"),
            LangErrorKind::NonComptimeVariable { var_name, class: _ } => write!(
                f,
                "Cannot assign non-comptime value to const variable '{var_name}'"
            ),
            LangErrorKind::UnexpectedOperator { operator, lhs, rhs } => write!(
                f,
                "Operator {operator} is not defined between type {lhs} and {rhs}"
            ),
            LangErrorKind::MissingModule { path, error: _ } => {
                write!(f, "Cannot find module at {path}")
            }
            LangErrorKind::CircularImport { module } => {
                write!(f, "Cannot import '{module}' multiple times")
            }
            LangErrorKind::InvalidControlFlow {
                control_flow,
                requires: _,
            } => write!(f, "Invalid control flow statement: {control_flow}"),
            LangErrorKind::UnreachableCode {} => write!(f, "This code will never be executed"),
            LangErrorKind::NotYetImplemented { msg } => {
                write!(f, "This feature is not yet implemented: {msg}")
            }
            LangErrorKind::ComptimeUpdate => {
                write!(
                    f,
                    "Cannot update this variable at runtime, only at compile time"
                )
            }
            LangErrorKind::ComptimeCall => write!(
                f,
                "Cannot call comptime function in a a non-comptime context"
            ),
            LangErrorKind::InvalidComptimeBranch => {
                write!(f, "Cannot evaluate this condition at compile time")
            }
            LangErrorKind::ContinueWithValue => {
                write!(f, "Cannot continue with a value")
            }
            LangErrorKind::InvalidExternItemPath { .. } => {
                write!(f, "Invalid extern item path")
            }
            LangErrorKind::FunctionAlreadyExported => {
                write!(f, "This function is already exported")
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
        let origin = code.get_code().path.as_deref();
        let source = code.get_code().source.as_ref();
        let range = code.get_relative_span(span).unwrap();

        match self {
            LangErrorKind::UnexpectedProperty { value_class, property } => {
                LangErrorSnippet {
                    slices: vec![SliceOwned {
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
            LangErrorKind::TupleMismatch {lhs_count,rhs_count} => {
                let message = match lhs_count.cmp(rhs_count) {
                    Ordering::Greater => "Not enough values to unpack",
                    Ordering::Less => "Too many values to unpack",
                    Ordering::Equal => unreachable!()
                };
                LangErrorSnippet {
                    slices: vec![SliceOwned {
                        origin ,
                        source,
                        annotations: vec![SourceAnnotationOwned {
                            annotation_type: AnnotationType::Error,
                            label: format!("{}: expected {} but got {}", message, lhs_count, rhs_count),
                            range,
                        }]
                    }],
                    footer: vec![]
                }
            }
            LangErrorKind::IndexOutOfBounds{..} => LangErrorSnippet {
                slices: vec![SliceOwned {
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
            LangErrorKind::ImmutableProperty => LangErrorSnippet {
                slices: vec![SliceOwned {
                    origin ,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "Comptime properties are immutable".into(),
                        range,
                    }]
                }],
                footer: vec![]
            },
            LangErrorKind::UnexpectedType { expected, got, declared } => {
                let expected_msg = display_expected_of_any(expected);
                let mut snippet = LangErrorSnippet {
                    slices: vec![SliceOwned {
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
                    if let Some(declared) = code.get_relative_span(*declared) {
                        snippet.slices[0].annotations.push(SourceAnnotationOwned {
                            annotation_type: AnnotationType::Info,
                            label: "Type declared here".to_string(),
                            range: declared
                        });
                    }
                }

                snippet
            },
            LangErrorKind::UnexpectedStructInitializer { ident, strukt:_, available } => LangErrorSnippet {
                slices: vec![SliceOwned {
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
            LangErrorKind::UnexpectedOverload { parameters, expected, function_definition_span} => {
                let parameters_string = format!("({})", parameters.iter().map(String::clone).join(", "));
                let mut possible_overloads = expected.iter().map(|params| {
                    if params.is_empty() {
                        "no parameters".to_string()
                    } else {
                        params.iter().map(String::clone).join(", ")
                    }
                });
                let message = if expected.len() == 1 {
                    format!("Called with {} but expected ({})", parameters_string, possible_overloads.next().unwrap())
                } else {
                    format!("Expected one of:\n  * {}",  possible_overloads.join("\n  * "))
                };

                let mut annotations =  vec![SourceAnnotationOwned {
                    annotation_type: AnnotationType::Error,
                    label: "No valid overload for this function call exists".to_string(),
                    range,
                }];

                if let Some(span) = function_definition_span {
                    let local_span = code.get_relative_span(*span);
                    if let Some(span) = local_span {
                        annotations.push(SourceAnnotationOwned {
                            annotation_type: AnnotationType::Info,
                            label: "Function defined here".to_string(),
                            range: span,
                        });
                    }
                }

                LangErrorSnippet {
                    slices: vec![SliceOwned {
                        source,
                        origin,
                        annotations,
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
            LangErrorKind::NonComptimeVariable {
                class, var_name,
            } => LangErrorSnippet {
                slices: vec![SliceOwned {
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
            LangErrorKind::NotYetImplemented { msg: _ } => LangErrorSnippet {
                slices: vec![SliceOwned {
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
                slices: vec![SliceOwned {
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "Cannot perform runtime updates for this value".to_string(),
                        range,
                    }]
                }],
                footer: vec![]
            },
            LangErrorKind::ComptimeCall => LangErrorSnippet {
                slices: vec![SliceOwned {
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "Cannot perform comptime calls in this context".to_string(),
                        range,
                    }]
                }],
                footer: vec![]
            },
            LangErrorKind::InvalidComptimeBranch => LangErrorSnippet {
                slices: vec![SliceOwned {
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "Cannot know the value of this condition at compile time".to_string(),
                        range,
                    }]
                }],
                footer: vec![AnnotationOwned {
                    id: None,
                    annotation_type: AnnotationType::Help,
                    label: Some("Try removing the comptime keyword".into())
                }]
            },
            LangErrorKind::ContinueWithValue => LangErrorSnippet {
                slices: vec![SliceOwned {
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "".to_string(),
                        range,
                    }]
                }],
                footer: vec![]
            },
            LangErrorKind::InvalidExternItemPath{ error, .. } => LangErrorSnippet {
                slices: vec![SliceOwned {
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: error.into(),
                        range,
                    }]
                }],
                footer: vec![]
            },
            LangErrorKind::FunctionAlreadyExported => LangErrorSnippet {
                slices: vec![SliceOwned {
                    origin,
                    source,
                    annotations: vec![SourceAnnotationOwned {
                        annotation_type: AnnotationType::Error,
                        label: "Cannot export a function multiple times".into(),
                        range,
                    }]
                }],
                footer: vec![]
            }
        }
    }
}
