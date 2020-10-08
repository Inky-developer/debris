//! Common items used by the compiler

mod ident;
pub use ident::{Ident, SpecialIdent};

mod accessor;
pub use accessor::Accessor;

mod input_file;
pub use input_file::{Code, CodeRef, InputFile};

mod span;
pub use span::{LocalSpan, Span};
