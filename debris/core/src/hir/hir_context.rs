use debris_common::{CodeRef, Ident, Span};

use crate::CompileContext;

use super::{ImportDependencies, SpannedIdentifier};

/// Contains state data that are used during the hir construction
#[derive(Debug)]
pub struct HirContext<'a, 'dep> {
    pub input_file: CodeRef<'a>,
    pub compile_context: &'a CompileContext,
    pub file_offset: usize,
    pub dependencies: &'dep mut ImportDependencies,
}

impl<'a, 'dep> HirContext<'a, 'dep> {
    pub fn new(
        input_file: CodeRef<'a>,
        compile_context: &'a CompileContext,
        dependencies: &'dep mut ImportDependencies,
    ) -> Self {
        let file_offset = input_file.get_offset();
        HirContext {
            input_file,
            compile_context,
            file_offset,
            dependencies,
        }
    }

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

    pub fn get_ident(&self, spanned_ident: SpannedIdentifier) -> Ident {
        self.compile_context
            .input_files
            .get_span_str(spanned_ident.span)
            .into()
    }

    pub fn add_import_file(&mut self, spanned_ident: SpannedIdentifier) -> usize {
        let span = spanned_ident.span;
        let ident = self.get_ident(spanned_ident);
        self.dependencies.insert(ident, span)
    }
}
