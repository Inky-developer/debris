use crate::error::Result;
use crate::Inputs;
use debris_common::{InputFile, LocalSpan};
use pest::Span;
use std::rc::Rc;

mod hir;
pub mod hir_nodes;

mod identifier;
pub use identifier::{IdentifierPath, SpannedIdentifier};

pub use hir::Hir;

pub fn get_span(span: Span) -> LocalSpan {
    LocalSpan::new(span.start(), span.end() - span.start())
}

#[derive(Parser)]
#[grammar = "hir/arithmetic.pest"]
pub struct ArithmeticParser;

#[salsa::query_group(HirParserStorage)]
pub trait HirParser: Inputs {
    fn parse(&self, key: InputFile) -> Rc<Result<Hir>>;
}

fn parse(db: &dyn HirParser, file: InputFile) -> Rc<Result<Hir>> {
    let code = db.input_text(file);
    Rc::new(Hir::from_code(code))
}
