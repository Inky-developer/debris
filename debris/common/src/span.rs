use std::ops::Deref;

use crate::CodeRef;

/// A Span which specifies a unique span of characters in a local file
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct LocalSpan {
    start: usize,
    len: usize,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Span {
    pub local_span: LocalSpan,
    pub code: CodeRef,
}

impl LocalSpan {
    pub fn new(start: usize, len: usize) -> Self {
        LocalSpan { start, len }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.start + self.len
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn until(&self, other: &LocalSpan) -> Self {
        LocalSpan::new(self.start, other.end() - self.start)
    }

    pub fn as_tuple(&self) -> (usize, usize) {
        (self.start, self.end())
    }

    pub fn as_str<'a>(&self, text: &'a str) -> &'a str {
        &text[self.start as usize..(self.start + self.len) as usize]
    }
}

impl Span {
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
