use std::path::PathBuf;

use crate::Span;

/// The type of a code if
pub type CodeId = usize;

/// A code object contains the source code and optionally a path to the corresponding file
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Code {
    pub source: String,
    pub path: Option<PathBuf>,
}

/// A handy object to work in a specific input file
#[derive(Debug, Clone, Copy)]
pub struct CodeRef<'a> {
    input_files: &'a InputFiles,
    pub file: CodeId,
}

/// Keeps track of all input files and allows to make cheap copy-able spans
#[derive(Debug, Default)]
pub struct InputFiles {
    /// All input files which are used in this compiler
    input_files: Vec<InputFile>,
}

/// A single input file, implementation detail
#[derive(Debug)]
struct InputFile {
    /// The code of the input file
    code: Code,
    /// The global offset
    offset: usize,
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
    pub fn get_relative_span(&self, span: Span) -> Span {
        Span::new(span.start() - self.get_offset(), span.len())
    }
}

impl InputFiles {
    pub fn new() -> Self {
        Default::default()
    }

    /// Adds a code unit and returns its ID
    pub fn add_input(&mut self, code: Code) -> CodeId {
        let offset = self.get_total_offset();
        let input_file = InputFile { code, offset };
        self.input_files.push(input_file);

        self.input_files.len() - 1
    }

    pub fn code_ref(&self, code_id: CodeId) -> CodeRef {
        CodeRef {
            file: code_id,
            input_files: self,
        }
    }

    /// Gets the input file with this id
    pub fn get_input(&self, code_id: CodeId) -> &Code {
        &self.input_files[code_id].code
    }

    /// Gets the offset of the file with this id
    pub fn get_input_offset(&self, code_id: CodeId) -> usize {
        self.input_files[code_id].offset
    }

    pub fn get_input_span(&self, code_id: CodeId) -> Span {
        let input = &self.input_files[code_id];
        Span::new(input.offset, input.code.source.len())
    }

    /// Calculates the total byte offset
    pub fn get_total_offset(&self) -> usize {
        if let Some(file) = self.input_files.last() {
            file.offset + file.code.source.len()
        } else {
            0
        }
    }

    fn get_span_file(&self, span: Span) -> (usize, &InputFile) {
        self.input_files
            .iter()
            .enumerate()
            .rev()
            .find(|(_, input_file)| input_file.offset <= span.start())
            .expect("Invalid span")
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

    /// Returns the line in a file at which this span begins
    pub fn get_span_start_line(&self, span: Span) -> usize {
        let input_file = self.get_span_file(span).1;
        let relative_start = span.start() - input_file.offset;
        input_file.code.source[..relative_start]
            .chars()
            .filter(|chr| *chr == '\n')
            .count()
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
            source: FILE_1.to_string(),
        });

        input_files.add_input(Code {
            path: None,
            source: FILE_2.to_string(),
        });

        input_files
    }

    #[test]
    fn test_input_files_members() {
        let input_files = input_files();
        assert_eq!(input_files.get_input(0).source, FILE_1);
        assert_eq!(input_files.get_input(1).source, FILE_2);
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
        let code_1 = input_files.code_ref(0);
        let code_2 = input_files.code_ref(1);

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

        assert_eq!(input_files.get_span_start_line(Span::new(0, 0)), 0);
        assert_eq!(input_files.get_span_start_line(Span::new(0, 1)), 0);
        assert_eq!(input_files.get_span_start_line(Span::new(0, 5)), 0);
        assert_eq!(input_files.get_span_start_line(Span::new(6, 5)), 0);
        assert_eq!(input_files.get_span_start_line(Span::new(13, 3)), 1);
        assert_eq!(input_files.get_span_start_line(Span::new(16, 3)), 0);
        assert_eq!(input_files.get_span_start_line(Span::new(20, 3)), 1);
    }
}