//! Compilation error handling
//!
//! Exports the default Result type which is used in [debris_core] where errors can happen
use std::fmt;
use std::fmt::{Display, Formatter};

use debris_common::{Ident, Span, SpecialIdent};
use thiserror::Error;

use crate::{objects::ClassRef, Type};

/// The result type used by most of the core functions
pub type Result<T> = std::result::Result<T, CompileError>;

/// A result type which allows quick error throwing since no span and other boileplate is needed
pub type LangResult<T> = std::result::Result<T, LangErrorKind>;

/// A Compile Error
///
/// This type is the Err value for most of this crate.
/// It is compatible with the `annotate_snippets` library.
/// That means that nice rust-style error messages can be printed.
#[derive(Debug, Error, Eq, PartialEq, Clone)]
pub enum CompileError {
    /// An error which happens when parsing the input
    #[error("Could not parse the input: {}", .0)]
    ParseError(#[from] ParseError),
    /// An error which happens when compiling the input
    #[error("Compiler Error:\n{}", .0)]
    LangError(#[from] LangError),
}

/// Thrown when parsing bad input
///
/// Contains the location in the source where the error occured and what symbols were expected
#[derive(Debug, Eq, PartialEq, Error, Clone)]
pub struct ParseError {
    /// The span where this error occured
    pub span: Span,
    /// Symbols which were expected instead
    pub expected: Vec<String>,
}

/// A generic error which gets thrown when compiling
///
/// Contains a more specific [LangErrorKind]
#[derive(Debug, Error, Eq, PartialEq, Clone)]
pub struct LangError {
    /// The specific error
    kind: LangErrorKind,
    span: Span,
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
        lhs: Type,
        rhs: Type,
    },
    #[error("This feature is not yet implemented: {}", .msg)]
    NotYetImplemented { msg: String },
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

impl LangError {
    pub fn new(kind: LangErrorKind, span: Span) -> Self {
        LangError { kind, span }
    }
}

impl Display for LangError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
