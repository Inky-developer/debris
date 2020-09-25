use debris_common::InputFile;
use std::rc::Rc;

use crate::error::Result;
use crate::hir::HirParser;
use crate::CompileContext;

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
    fn parse_hir(&self, key: InputFile) -> Rc<Result<Mir>>;
}

fn parse_hir(db: &dyn MirParser, key: InputFile) -> Rc<Result<Mir>> {
    let hir = db.parse(key);

    Rc::new(match hir.as_ref() {
        Ok(value) => Mir::from_hir(&value, Rc::new(CompileContext::default())),
        Err(err) => Err(err.clone()),
    })
}
