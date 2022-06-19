//! Common items used by the compiler

pub mod clone_cell;
pub mod file_provider;
pub mod graph;
mod ident;

pub use ident::{Ident, SpecialIdent};

mod accessor;
pub use accessor::Accessor;

mod input_file;
pub use input_file::{Code, CodeId, CodeRef, InputFile, InputFiles};

mod span;
pub use span::{character_width_at_index, Span};

mod compile_context;
pub use compile_context::{CompilationId, CompileContext};

mod config;
pub use config::{BuildMode, Config, OptMode};

use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;
pub type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;
pub type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;
