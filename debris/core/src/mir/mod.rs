//! Mid-level intermediate representation
//!
//! Once the high lever representation has be created, it can be lowered into a mir.
//! The mir is quite simplistic and focuses on control flow.
//!
//! An input of `let a = 3 * 5 + 2` would be represented as a bunch of `FunctionCall(...)` and look somewhat like this:
//! ```pseudocode
//! Call(
//!     StaticInt.*,
//!     parameters: [StaticInt(3), StaticInt(5)],
//!     return_val: {id: 1, type: StaticInt}
//! );
//!
//! Call(
//!     StaticInt.+,
//!     parameters: [{id: 1, type: StaticInt}, StaticInt(2)],
//!     return_val {id: 2, type: StaticInt}
//! );
//! ```
//!
//! The actual value of the returned static ints will be lazily evaluated at [llir](debris_core::llir)
//!
//! A [context](debris_core::mir::MirContext) is used to keep track of all variables that have ben created, as well as their identifiers.
use debris_common::InputFile;
use std::rc::Rc;

use crate::error::Result;
use crate::{hir::HirParser, objects::ModuleFactory};

mod utils;
pub use utils::ItemIdentifier;

mod mir_nodes;
pub use mir_nodes::{MirNode, MirValue};

mod mir_context;
pub use mir_context::{MirContext, MirContextInfo, MirInfo, MirNamespaceEntry, NamespaceArena};

mod mir_impl;
pub use mir_impl::Mir;

#[salsa::query_group(MirParserStorage)]
pub trait MirParser: HirParser {
    /// Converts the hir into mir and imports the global extern crates
    fn parse_hir_global(&self, key: InputFile) -> Rc<Result<Mir>>;

    /// Converts the hir into mir without importing the global extern crates
    fn parse_hir(&self, key: InputFile) -> Rc<Result<Mir>>;
}

fn parse_hir(db: &dyn MirParser, key: InputFile) -> Rc<Result<Mir>> {
    inner_parse_hir(db, key, &[])
}

fn parse_hir_global(db: &dyn MirParser, key: InputFile) -> Rc<Result<Mir>> {
    inner_parse_hir(db, key, &db.extern_modules())
}

fn inner_parse_hir(
    db: &dyn MirParser,
    key: InputFile,
    extern_modules: &[ModuleFactory],
) -> Rc<Result<Mir>> {
    let hir = db.parse(key);

    Rc::new(match hir.as_ref() {
        Ok(value) => Mir::from_hir(&value, db.compile_context(), extern_modules),
        Err(err) => Err(err.clone()),
    })
}
