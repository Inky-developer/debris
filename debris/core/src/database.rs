use crate::hir::HirParserStorage;
use crate::llir::LLIRParserStorage;
use crate::mir::MirParserStorage;
use crate::InputsStorage;

#[salsa::database(InputsStorage, HirParserStorage, MirParserStorage, LLIRParserStorage)]
#[derive(Default)]
pub struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}
