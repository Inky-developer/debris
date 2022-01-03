//! Owned variants of the `annotate_snippets` library structs
use std::borrow::Cow;

use annotate_snippets::{
    display_list::FormatOptions,
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

use debris_common::{InputFiles, Span};

/// An owned counterpart to the `annotate_snippets::Snippet` struct
#[derive(Debug)]
pub struct SnippetOwned<'a> {
    pub title: Cow<'a, str>,
    pub id: Option<Cow<'a, str>>,
    pub annotation_type: AnnotationType,
    pub slices: Vec<SliceOwned<'a>>,
    pub footer: Vec<AnnotationOwned<'a>>,
}

/// An owned counterpart to the `annotate_snippets::Slice` struct
#[derive(Debug)]
pub struct SliceOwned<'a> {
    pub source: &'a str,
    pub origin: Option<&'a str>,
    pub annotations: Vec<SourceAnnotationOwned>,
}

/// An owned counterpart to the `annotate_snippets::SourceAnnotation` struct
#[derive(Debug)]
pub struct AnnotationOwned<'a> {
    pub id: Option<Cow<'a, str>>,
    pub label: Option<Cow<'a, str>>,
    pub annotation_type: AnnotationType,
}

/// An owned counterpart to the `annotate_snippets::SourceAnnotation` struct
#[derive(Debug)]
pub struct SourceAnnotationOwned {
    pub annotation_type: AnnotationType,
    pub range: Span,
    pub label: String,
}

impl SnippetOwned<'_> {
    pub fn as_snippet(&self, input_files: &InputFiles) -> Snippet {
        Snippet {
            title: Some(Annotation {
                annotation_type: self.annotation_type,
                id: self.id.as_deref(),
                label: Some(&self.title),
            }),
            slices: self
                .slices
                .iter()
                .map(|slice| slice.as_slice(input_files))
                .collect(),
            footer: self
                .footer
                .iter()
                .map(AnnotationOwned::as_annotation)
                .collect(),
            opt: FormatOptions {
                color: super::COLORED,
                ..Default::default()
            },
        }
    }
}

impl SliceOwned<'_> {
    pub fn as_slice(&self, input_files: &InputFiles) -> Slice {
        Slice {
            source: self.source,
            line_start: 1,
            annotations: self
                .annotations
                .iter()
                .map(|ann| ann.as_source_annotation(input_files))
                .collect(),
            origin: self.origin,
            fold: true,
        }
    }
}

impl AnnotationOwned<'_> {
    pub fn as_annotation(&self) -> Annotation {
        Annotation {
            annotation_type: self.annotation_type,
            id: self.id.as_deref(),
            label: self.label.as_deref(),
        }
    }
}

impl SourceAnnotationOwned {
    pub fn as_source_annotation(&self, input_files: &InputFiles) -> SourceAnnotation {
        let text = &input_files.get_span_code(self.range).get_code().source;
        let range = self.range.char_bounds(text);
        SourceAnnotation {
            range,
            label: &self.label,
            annotation_type: self.annotation_type,
        }
    }
}
