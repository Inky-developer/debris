//! Compilation error handling
//!
//! Exports the default Result type which is used throughout the compiler

use annotate_snippets::display_list::DisplayList;
use debris_common::CompileContext;

use itertools::Itertools;
pub use lang_error::{ControlFlowRequires, LangError, LangErrorKind};
pub use parse_error::ParseError;
pub use snippet::{SliceOwned, SnippetOwned, SourceAnnotationOwned};

mod utils;

mod lang_error;
mod macros;
mod parse_error;
mod snippet;

/// Used to determine whether the error messages should use console colors
/// Or be plain text
pub const COLORED: bool = cfg!(feature = "colored_errors");

/// The result type used by most of the core functions
pub type Result<T> = std::result::Result<T, SingleCompileError>;

/// A result type which allows quick error throwing since no span and other boilerplate is needed
pub type LangResult<T> = std::result::Result<T, LangErrorKind>;

pub trait AsAnnotationSnippet<'a> {
    fn as_annotation_snippet(&self, ctx: &'a CompileContext) -> SnippetOwned<'a>;

    fn to_display_list(&self, ctx: &'a CompileContext) -> String {
        let snippet = self.as_annotation_snippet(ctx);
        let display_list = DisplayList::from(snippet.as_snippet(&ctx.input_files));
        display_list.to_string()
    }
}

#[derive(Debug)]
pub struct CompileErrors(pub Vec<SingleCompileError>);

impl CompileErrors {
    pub fn format(&self, ctx: &CompileContext) -> String {
        self.0.iter().map(|error| error.format(ctx)).join("\n\n")
    }
}

impl From<Vec<SingleCompileError>> for CompileErrors {
    fn from(errors: Vec<SingleCompileError>) -> Self {
        CompileErrors(errors)
    }
}

/// A Compile Error
///
/// This type is the Err value for most of this crate.
/// It is compatible with the `annotate_snippets` library.
/// That means that nice rust-style error messages can be printed.
#[derive(Debug)]
pub enum SingleCompileError {
    /// An error which happens when parsing the input
    ParseError(Box<ParseError>),
    /// An error which happens when compiling the input
    LangError(Box<LangError>),
}

impl SingleCompileError {
    pub fn format(&self, ctx: &CompileContext) -> String {
        <Self as AsAnnotationSnippet>::to_display_list(self, ctx)
    }
}

impl From<ParseError> for SingleCompileError {
    fn from(parse_error: ParseError) -> Self {
        SingleCompileError::ParseError(Box::new(parse_error))
    }
}

impl From<LangError> for SingleCompileError {
    fn from(lang_error: LangError) -> Self {
        SingleCompileError::LangError(Box::new(lang_error))
    }
}

impl<'a> AsAnnotationSnippet<'a> for SingleCompileError {
    fn as_annotation_snippet(&self, ctx: &'a CompileContext) -> SnippetOwned<'a> {
        match self {
            SingleCompileError::ParseError(parse_error) => parse_error.as_annotation_snippet(ctx),
            SingleCompileError::LangError(lang_error) => lang_error.as_annotation_snippet(ctx),
        }
    }
}
