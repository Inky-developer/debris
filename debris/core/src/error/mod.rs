//! Compilation error handling
//!
//! Exports the default Result type which is used in [debris_core] where errors can happen
use std::fmt;
use std::fmt::{Display, Formatter};

use annotate_snippets::display_list::DisplayList;
use thiserror::Error;

pub use lang_error::{LangError, LangErrorKind};
pub use parse_error::ParseError;
pub use snippet::{SliceOwned, SnippetOwned, SourceAnnotationOwned};

use crate::CompileContext;

mod utils;

mod snippet;
mod parse_error;
mod lang_error;

/// Used to determine whether the error messages should use console colors
/// Or be plain text
pub const COLORED: bool = cfg!(feature = "colored_errors");

/// The result type used by most of the core functions
pub type Result<T> = std::result::Result<T, CompileError>;

/// A result type which allows quick error throwing since no span and other boileplate is needed
pub type LangResult<T> = std::result::Result<T, LangErrorKind>;

pub trait AsAnnotationSnippet<'a> {
    fn as_annotation_snippet(&self, ctx: &'a CompileContext) -> SnippetOwned<'a>;

    fn to_string(&self, ctx: &'a CompileContext) -> String {
        let snippet = self.as_annotation_snippet(ctx);
        let display_list = DisplayList::from(snippet.as_snippet(&ctx.input_files));
        display_list.to_string()
    }
}

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

impl CompileError {
    pub fn format(&self, ctx: &CompileContext) -> String {
        <Self as AsAnnotationSnippet>::to_string(self, ctx)
    }
}

impl Display for LangError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

impl<'a> AsAnnotationSnippet<'a> for CompileError {
    fn as_annotation_snippet(&self, ctx: &'a CompileContext) -> SnippetOwned<'a> {
        match self {
            CompileError::ParseError(parse_error) => parse_error.as_annotation_snippet(ctx),
            CompileError::LangError(lang_error) => lang_error.as_annotation_snippet(ctx),
        }
    }
}
