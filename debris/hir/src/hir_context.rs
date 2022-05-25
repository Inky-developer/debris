use debris_common::{CodeRef, CompileContext, Ident, Span};
use debris_parser::{
    ast::{AstItem, AstNodeOrToken},
    LocalSpan,
};

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

    pub fn span(&self, item: impl Into<AstNodeOrToken>) -> Span {
        let span = item.into().span();
        self.normalize_local_span(span)
    }

    pub fn item_span(&self, ast_item: &impl AstItem) -> Span {
        let item = ast_item.to_item();
        let local_span = item.span();
        self.normalize_local_span(local_span)
    }

    pub fn normalize_local_span(&self, local_span: LocalSpan) -> Span {
        Span::new(local_span.start() + self.file_offset, local_span.len())
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
