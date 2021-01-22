use debris_common::{CodeRef, Span};

use crate::CompileContext;

/// Contains state data that are used during the hir construction
#[derive(Debug)]
pub struct HirContext<'a> {
    pub input_file: CodeRef<'a>,
    pub compile_context: &'a CompileContext,
    pub file_offset: usize,
}

impl HirContext<'_> {
    #[inline]
    pub fn span(&self, pest_span: pest::Span) -> Span {
        Self::normalize_pest_span(pest_span, self.file_offset)
    }

    #[inline]
    pub fn normalize_pest_span(pest_span: pest::Span, offset: usize) -> Span {
        Span::new(
            pest_span.start() + offset,
            pest_span.end() - pest_span.start(),
        )
    }
}
