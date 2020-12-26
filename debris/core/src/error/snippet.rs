//! Owned variants of the `annotate_snippets` library structs
use std::borrow::Cow;

use annotate_snippets::{
    display_list::FormatOptions,
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use debris_common::Span;

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
    pub fold: bool,
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
    pub fn as_snippet(&self) -> Snippet {
        Snippet {
            title: Some(Annotation {
                annotation_type: self.annotation_type,
                id: self.id.as_deref(),
                label: Some(&self.title),
            }),
            slices: self.slices.iter().map(SliceOwned::as_slice).collect(),
            footer: self
                .footer
                .iter()
                .map(AnnotationOwned::as_annotation)
                .collect(),
            opt: FormatOptions {
                color: true,
                ..Default::default()
            },
        }
    }
}

impl SliceOwned<'_> {
    pub fn as_slice(&self) -> Slice {
        Slice {
            source: &self.source,
            line_start: 1,
            annotations: self
                .annotations
                .iter()
                .map(SourceAnnotationOwned::as_source_annotation)
                .collect(),
            origin: self.origin,
            fold: self.fold,
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
    pub fn as_source_annotation(&self) -> SourceAnnotation {
        SourceAnnotation {
            annotation_type: self.annotation_type,
            range: (self.range.start(), self.range.end()),
            label: &self.label,
        }
    }
}
