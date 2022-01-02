//! This module contains some macros for the most common error messages

#[macro_export]
macro_rules! debris_unimplemented {
    ($span:expr, $msg:expr) => {
        return ::std::result::Result::Err(
            ::debris_core::error::LangError::new(
                ::debris_core::error::LangErrorKind::NotYetImplemented { msg: $msg.into() },
                $span,
            )
            .into(),
        );
    };
}
