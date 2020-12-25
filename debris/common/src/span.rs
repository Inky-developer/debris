/// A span which uniquely specifies a span of characters in their corresponsing file
///
/// This span is more expensive to use than a [LocalSpan]
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

impl Span {
    /// Creates a new `Span` from the starting character and its length
    pub fn new(start: usize, len: usize) -> Self {
        Span { start, len }
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

    /// Returns a tuple of the format (start_index, end_index)
    pub fn as_tuple(&self) -> (usize, usize) {
        (self.start, self.end())
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
        assert_eq!(span.as_tuple(), (0, 1));
    }
}
