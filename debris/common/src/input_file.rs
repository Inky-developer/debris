//! Input files are used to store the files required for successful compilation
//! The big advantage of have a global storage of all possible input files is that
//! this allows for extremely cheap spans which only consist of a (start, length) tuple.
//! To get the corresponding file for a span, simply check whether the span lies withing
//! file.offset and file.offset + file.length

use crate::Span;

/// The type of a code id, currently just a usize
pub type CodeId = usize;

/// A code object contains the source code and optionally a path to the corresponding file
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Code {
    pub source: Box<str>,
    pub path: Option<Box<str>>,
}

/// A handy object to work in a specific input file
#[derive(Debug, Clone, Copy)]
pub struct CodeRef<'a> {
    input_files: &'a InputFiles,
    pub file: CodeId,
}

impl<'a> CodeRef<'a> {
    pub fn get_code(&self) -> &'a Code {
        self.input_files.get_input(self.file)
    }

    pub fn get_offset(&self) -> usize {
        self.input_files.get_input_offset(self.file)
    }

    pub fn get_span(&self) -> Span {
        let input = &self.input_files.input_files[self.file];
        Span::new(input.offset, input.code.source.len())
    }

    /// Returns a span that is relative to the start of this code file
    /// Returns [`None`] if the span is not within this file
    pub fn get_relative_span(&self, span: Span) -> Option<Span> {
        let start = span.start().checked_sub(self.get_offset())?;
        if start >= self.input_files.input_files[self.file].code.source.len() {
            None
        } else {
            Some(Span::new(start, span.len()))
        }
    }
}

/// A single input file, implementation detail
#[derive(Debug)]
pub struct InputFile {
    /// The code of the input file
    code: Code,
    /// The global offset
    offset: usize,
}

/// Keeps track of all input files and allows to make cheap copy-able spans
#[derive(Debug, Default)]
pub struct InputFiles {
    /// All input files which are used in this compiler
    ///
    /// This vector is append-only and the offset of the input files
    /// is guaranteed to be increasing.
    input_files: Vec<InputFile>,
}

impl InputFiles {
    pub fn new() -> Self {
        InputFiles::default()
    }

    /// Adds a code unit and returns its ID
    pub fn add_input(&mut self, code: Code) -> CodeId {
        let offset = self.get_total_offset();
        let input_file = InputFile { code, offset };
        self.input_files.push(input_file);

        self.input_files.len() - 1
    }

    pub fn get_code_ref(&self, code_id: CodeId) -> CodeRef {
        CodeRef {
            file: code_id,
            input_files: self,
        }
    }

    /// Searches the registered [`InputFile`]s and returns the first with a matching filename
    pub fn find_by_filename(&self, filename: &str) -> Option<CodeId> {
        self.input_files
            .iter()
            .enumerate()
            .find(|(_, file)| {
                file.code
                    .path
                    .as_ref()
                    .map_or(false, |fname| fname.as_ref() == filename)
            })
            .map(|(index, _)| index)
    }

    /// Gets the input file with this id
    pub fn get_input(&self, code_id: CodeId) -> &Code {
        &self.input_files[code_id].code
    }

    /// Gets the offset of the file with this id
    pub fn get_input_offset(&self, code_id: CodeId) -> usize {
        self.input_files[code_id].offset
    }

    /// Calculates the total byte offset
    pub fn get_total_offset(&self) -> usize {
        self.input_files
            .last()
            .map_or(0, |file| file.offset + file.code.source.len())
    }

    /// Searches for the input file that contains the given span
    ///
    /// Returns a `(index, &input_file)` tuple.
    /// Since the input files are stored sorted, the search can be
    /// completed with O(log n) time complexity
    fn get_span_file(&self, span: Span) -> (usize, &InputFile) {
        // Determine the index of the file that contains this span
        let index = match self
            .input_files
            .binary_search_by_key(&span.start(), |input_file| input_file.offset)
        {
            Ok(x) => x,
            // The method returns `Err`, if no input_file has the exact same offset
            // as the given span, which is the common case.
            // The `x` marks the index of the file with the lowest offset greater than
            // span's offset, so we have to subtract one from it to get the correct file
            Err(x) => x - 1,
        };

        (index, &self.input_files[index])
    }

    pub fn get_span_code(&self, span: Span) -> CodeRef {
        let (index, _) = self.get_span_file(span);
        CodeRef {
            file: index,
            input_files: self,
        }
    }

    /// Returns the str that corresponds to a span
    pub fn get_span_str(&self, span: Span) -> &str {
        let input_file = self.get_span_file(span).1;

        let start = span.start() - input_file.offset;
        &input_file.code.source[start..span.len() + start]
    }

    /// Returns the line in a file at which this span begins and the column
    /// This function starts counting at zero, so that the first character in a file is (0, 0)
    pub fn line_col(&self, span: Span) -> (usize, usize) {
        let input_file = self.get_span_file(span).1;
        let relative_start = span.start() - input_file.offset;
        let (lines, line_start_idx) = input_file.code.source[..relative_start]
            .char_indices()
            .filter(|(_, chr)| *chr == '\n')
            .fold((0_usize, 0), |(lines, _), (idx, _)| (lines + 1, idx + 1));
        let col = input_file.code.source[line_start_idx..relative_start]
            .chars()
            .count();
        (lines, col)
    }
}

#[cfg(test)]
mod tests {
    use crate::{Code, InputFiles, Span};

    const FILE_1: &str = "Hello World!\nFoo";
    const FILE_2: &str = "Bar\nBaz";

    fn input_files() -> InputFiles {
        let mut input_files = InputFiles::new();

        input_files.add_input(Code {
            path: None,
            source: FILE_1.into(),
        });

        input_files.add_input(Code {
            path: None,
            source: FILE_2.into(),
        });

        input_files
    }

    #[test]
    fn test_input_files_members() {
        let input_files = input_files();
        assert_eq!(input_files.get_input(0).source.as_ref(), FILE_1);
        assert_eq!(input_files.get_input(1).source.as_ref(), FILE_2);
    }

    #[test]
    fn test_input_files_offset() {
        let input_files = input_files();
        let offset_1 = input_files.get_input_offset(0);
        let offset_2 = input_files.get_input_offset(1);

        assert_eq!(offset_1, 0);
        assert_eq!(offset_2, FILE_1.len());
    }

    #[test]
    fn test_code_span() {
        let input_files = input_files();
        let code_1 = input_files.get_code_ref(0);
        let code_2 = input_files.get_code_ref(1);

        assert_eq!(code_1.get_span(), Span::new(0, FILE_1.len()));
        assert_eq!(code_2.get_span(), Span::new(FILE_1.len(), FILE_2.len()));
    }

    #[test]
    fn test_code_from_span() {
        let input_files = input_files();

        assert_eq!(input_files.get_span_file(Span::new(0, 1)).0, 0);
        assert_eq!(
            input_files.get_span_file(Span::new(FILE_1.len() - 1, 1)).0,
            0
        );
        assert_eq!(input_files.get_span_file(Span::new(FILE_1.len(), 0)).0, 1);
        assert_eq!(
            input_files.get_span_file(Span::new(FILE_1.len() + 1, 1)).0,
            1
        );
    }

    #[test]
    fn test_span_str() {
        let input_files = input_files();

        assert_eq!(input_files.get_span_str(Span::new(0, 0)), "");
        assert_eq!(input_files.get_span_str(Span::new(0, 1)), "H");
        assert_eq!(input_files.get_span_str(Span::new(0, 5)), "Hello");
        assert_eq!(input_files.get_span_str(Span::new(6, 5)), "World");
        assert_eq!(input_files.get_span_str(Span::new(13, 3)), "Foo");
        assert_eq!(input_files.get_span_str(Span::new(16, 3)), "Bar");
        assert_eq!(input_files.get_span_str(Span::new(20, 3)), "Baz");
    }

    #[test]
    #[should_panic]
    fn test_span_str_panic_a() {
        input_files().get_span_str(Span::new(15, 3));
    }
    #[test]
    #[should_panic]
    fn test_span_str_panic_b() {
        input_files().get_span_str(Span::new(15, 3));
    }

    #[test]
    fn test_span_line() {
        let input_files = input_files();

        assert_eq!(input_files.line_col(Span::new(0, 0)), (0, 0));
        assert_eq!(input_files.line_col(Span::new(0, 1)), (0, 0));
        assert_eq!(input_files.line_col(Span::new(0, 5)), (0, 0));
        assert_eq!(input_files.line_col(Span::new(6, 5)), (0, 6));
        assert_eq!(input_files.line_col(Span::new(13, 3)), (1, 0));
        assert_eq!(input_files.line_col(Span::new(16, 3)), (0, 0));
        assert_eq!(input_files.line_col(Span::new(21, 2)), (1, 1));
    }
}
