/// A span which uniquely specifies a span of characters in their corresponsing file
/// All spans are dependent on the [InputFiles](super::InputFiles) which contains the file
/// that spans can refer to.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct Span {
    start: usize,
    len: usize,
}

impl Span {
    /// Creates a new `Span` from the starting character and its length
    pub fn new(start: usize, len: usize) -> Self {
        Span { start, len }
    }

    /// Creates an empty Span
    pub fn empty() -> Self {
        Span::new(0, 0)
    }

    /// Returns a span with the same start and a length of one
    pub fn at_start(&self) -> Span {
        Span::new(self.start(), 1)
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
    pub fn until(&self, other: Span) -> Self {
        if self.start > other.end() {
            panic!("Span length must not be negative");
        }
        Span::new(self.start, other.end() - self.start)
    }
}

#[cfg(test)]
mod tests {

    use crate::Span;

    #[test]
    fn local_span_correct() {
        let span = Span::new(0, 1);

        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 1);
        assert_eq!(span.len(), 1);
    }
}
