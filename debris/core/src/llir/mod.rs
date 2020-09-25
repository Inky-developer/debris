use debris_common::InputFile;
use std::rc::Rc;

use crate::error::Result;
use crate::mir::MirParser;
use crate::Inputs;

mod llir;
pub use llir::LLIR;

pub mod llir_nodes;

pub mod utils;

#[salsa::query_group(LLIRParserStorage)]
pub trait LLIRParser: MirParser + Inputs {
    fn parse_mir(&self, key: InputFile) -> Rc<Result<LLIR>>;
}

fn parse_mir(db: &dyn LLIRParser, key: InputFile) -> Rc<Result<LLIR>> {
    let config = db.config(key.clone());
    let mir = db.parse_hir(key);

    Rc::new(match mir.as_ref() {
        Ok(mir) => LLIR::from_mir(&mir, config),
        Err(err) => Err(err.clone()),
    })
}
