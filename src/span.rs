use std::{num::TryFromIntError, ops::Range};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub len: u32,
}

impl Span {
    pub fn as_slice(self) -> Range<usize> {
        (self.start as usize)..((self.start + self.len) as usize)
    }

    pub fn end(self) -> u32 {
        self.start + self.len
    }

    pub fn until(self, other: Span) -> Span {
        Span {
            start: self.start,
            len: other
                .end()
                .checked_sub(self.start)
                .expect("other must end after self"),
        }
    }
}

impl From<Range<u32>> for Span {
    fn from(value: Range<u32>) -> Self {
        Span {
            start: value.start,
            len: value
                .end
                .checked_sub(value.start)
                .expect("end must be greater than start"),
        }
    }
}

impl TryFrom<Range<usize>> for Span {
    type Error = TryFromIntError;

    fn try_from(value: Range<usize>) -> Result<Self, Self::Error> {
        let smaller_value: Range<u32> = (value.start.try_into()?)..(value.end.try_into()?);
        Ok(smaller_value.into())
    }
}
