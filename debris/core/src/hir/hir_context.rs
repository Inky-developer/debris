use debris_common::{CodeRef, Span};

use crate::CompileContext;

/// Contains state data that are used during the hir construction
#[derive(Debug)]
pub struct HirContext<'a> {
    pub input_file: CodeRef<'a>,
    pub compile_context: &'a CompileContext,
}

impl HirContext<'_> {
    pub fn span(&self, pest_span: pest::Span) -> Span {
        Span::new(
            pest_span.start() + self.input_file.get_offset(),
            pest_span.end() - pest_span.start(),
        )
    }
}
