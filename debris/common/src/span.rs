/// Utility function to get the width of a character at a given byte index
/// Panics if the index is not the start of a character
pub fn character_width_at_index(index: usize, value: &str) -> usize {
    let mut current_index = 0;
    for char in value.chars() {
        if current_index == index {
            return char.len_utf8();
        }
        assert!(
            current_index < index,
            "Index was not at the start of a character"
        );
        current_index += char.len_utf8();
    }
    panic!("Index out of bounds")
}

/// A span which uniquely specifies a span of characters in their corresponding file
/// All spans are dependent on the [`InputFiles`](super::InputFiles) which contains the file
/// that spans can refer to.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct Span {
    start: usize,
    len: usize,
}

impl Span {
    pub const EMPTY: Span = Span::new(0, 0);

    /// Creates a new `Span` from the starting character and its length
    pub const fn new(start: usize, len: usize) -> Self {
        Span { start, len }
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
        assert!(
            self.start <= other.end(),
            "Span length must not be negative"
        );
        Span::new(self.start, other.end() - self.start)
    }

    /// Since ranges are used to index into a str on a byte level,
    /// a span starting at index 10 is **not** necessarily the character at index 10.
    /// This methods iterates over the source chars until it finds the character at the
    /// byte positions marked by this span. This methods panics if the span is out of bounds.
    /// # Returns:
    ///  The returned tuple has the shape (`start_character_index`, `end_character_index`)
    pub fn char_bounds(&self, source: &str) -> (usize, usize) {
        let mut byte_idx = 0;

        let mut start_idx = None;
        let mut end_idx = None;

        if self.start() == 0 {
            start_idx = Some(0);
        }
        if self.end() == 0 {
            end_idx = Some(0);
        }
        for (idx, char) in source.chars().enumerate() {
            byte_idx += char.len_utf8();
            if self.start() == byte_idx {
                start_idx = Some(idx + 1);
            }
            if self.end() == byte_idx {
                end_idx = Some(idx + 1);
                break;
            }
        }
        let start_idx = start_idx.expect("Span start is out of bounds");
        let end_idx = end_idx.expect("Span end is out of bounds");
        (start_idx, end_idx)
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

    #[test]
    fn span_char_bounds() {
        let span = Span::new(2, 3);
        let text = "öaöa";
        assert_eq!(span.char_bounds(text), (1, 3));
    }
}
