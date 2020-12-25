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
    file: CodeId,
}

/// Keeps track of all input files an allows to make cheap copy-able spans
#[derive(Debug, Default)]
pub struct InputFiles {
    /// All input files which are used in this compiler
    input_files: Vec<InputFile>,
}

/// A single input file
#[derive(Debug)]
struct InputFile {
    /// The code of the input file
    code: Code,
    /// The global offset
    offset: usize,
}

impl CodeRef<'_> {
    pub fn get_code(&self) -> &Code {
        self.input_files.get_input(self.file)
    }

    pub fn get_offset(&self) -> usize {
        self.input_files.get_input_offset(self.file)
    }

    pub fn get_span(&self) -> Span {
        let input = &self.input_files.input_files[self.file];
        Span::new(input.offset, input.code.source.len())
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

    fn get_span_file(&self, span: Span) -> (usize, &InputFile) {
        self.input_files
            .iter()
            .enumerate()
            .rev()
            .find(|(_, input_file)| input_file.offset <= span.start)
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

        let start = span.start - input_file.offset;
        &input_file.code.source[start..span.len + start]
    }

    /// Returns the line in a file at which this span begins
    pub fn get_span_start_line(&self, span: Span) -> usize {
        let input_file = self.get_span_file(span).1;
        let relative_start = span.start - input_file.offset;
        input_file.code.source[..relative_start]
            .chars()
            .filter(|chr| *chr == '\n')
            .count()
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
}
