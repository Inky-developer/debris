use debris_common::InputFile;
use std::rc::Rc;

use crate::error::Result;
use crate::{hir::HirParser, objects::ModuleFactory};

mod utils;
pub use utils::ItemIdentifier;

mod mir_nodes;
pub use mir_nodes::{MirNode, MirValue};

mod mir_context;
pub use mir_context::MirContext;

mod mir;
pub use mir::Mir;

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
