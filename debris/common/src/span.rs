use std::ops::Deref;

use crate::CodeRef;

/// A Span which specifies a unique span of characters in a local file
///
/// Because this span does not specify its source code, it cannot be sent accros different files.
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct LocalSpan {
    start: usize,
    len: usize,
}

/// A span which uniquely specifies a span of characters in their corresponsing file
///
/// This span is more expensive to use than a [LocalSpan]
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Span {
    pub local_span: LocalSpan,
    pub code: CodeRef,
}

impl LocalSpan {
    /// Creates a new `LocalSpan` from the starting character and its length
    pub fn new(start: usize, len: usize) -> Self {
        LocalSpan { start, len }
    }

    /// Returns the start of this span
    pub fn start(&self) -> usize {
        self.start
    }

    /// Returns the end of this span
    pub fn end(&self) -> usize {
        self.start + self.len
    }

    /// Returns the length of this span
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns, whether this span is empty
    ///
    /// Thanks, clippy.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Constructs a new span which ranges from the start of this span to the end of the other span
    pub fn until(&self, other: &LocalSpan) -> Self {
        if self.start > other.end() {
            panic!("Span length must not be negative");
        }
        LocalSpan::new(self.start, other.end() - self.start)
    }

    /// Returns a tuple of the format (start_index, end_index)
    pub fn as_tuple(&self) -> (usize, usize) {
        (self.start, self.end())
    }

    /// Slices `text` and returns the corresponding string slice
    pub fn as_str<'a>(&self, text: &'a str) -> &'a str {
        &text[self.start as usize..(self.start + self.len) as usize]
    }
}

impl Span {
    /// Returns the line number of the line where this span begins
    ///
    /// Starts counting at 1!
    pub fn line_start(&self) -> usize {
        self.code
            .source
            .chars()
            .take(self.local_span.start as usize)
            .fold(1, |sum, char| sum + if char == '\n' { 1 } else { 0 })
    }
}

impl Deref for Span {
    type Target = LocalSpan;

    fn deref(&self) -> &Self::Target {
        &self.local_span
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{Code, Span};

    use super::LocalSpan;

    #[test]
    fn local_span_correct() {
        let span = LocalSpan::new(0, 1);

        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 1);
        assert_eq!(span.len(), 1);
        assert_eq!(span.as_tuple(), (0, 1));
        assert_eq!(span.as_str("Some text"), "S");
    }

    #[test]
    fn span_correct() {
        let code = Rc::new(Code {
            path: None,
            source: "1\n2\n3".to_string(),
        });
        let local_span = LocalSpan::new(4, 1);

        let span = Span { code, local_span };

        assert_eq!(span.line_start(), 3);
    }
}
