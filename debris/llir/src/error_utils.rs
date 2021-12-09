use debris_common::Span;
use debris_error::{CompileError, LangError, LangErrorKind};

use crate::class::Class;

#[track_caller]
pub fn unexpected_type(span: Span, expected: &Class, actual: &Class) -> CompileError {
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
