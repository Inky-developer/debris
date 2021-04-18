//! Common items used by the compiler

pub mod graph;
mod ident;
pub use ident::{Ident, SpecialIdent};

mod accessor;
pub use accessor::Accessor;

mod input_file;
pub use input_file::{Code, CodeId, CodeRef, InputFiles};

mod span;
pub use span::Span;
