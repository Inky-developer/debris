use debris_common::Span;
use debris_error::{LangError, LangErrorKind, SingleCompileError};

use crate::class::Class;

#[track_caller]
pub fn unexpected_type(span: Span, expected: &Class, actual: &Class) -> SingleCompileError {
    LangError::new(
        LangErrorKind::UnexpectedType {
            got: actual.to_string(),
            expected: vec![expected.to_string()],
            declared: None,
        },
        span,
    )
    .into()
}
