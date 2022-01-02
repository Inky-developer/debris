use std::{cell::Cell, default::Default};

use crate::{Code, CodeId, Config, InputFiles};

/// The id of the current compilation unit. Used to generate ids that are unique
/// across all compilation units
#[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd, Hash)]
pub struct CompilationId(pub u32);

/// The Compilation context stores various information about the current compilation
#[derive(Debug)]
pub struct CompileContext {
    pub compilation_id: CompilationId,
    /// The current config which specifies how to compile
    pub config: Config,
    /// The code files
    pub input_files: InputFiles,
    /// The current unique id system.
    /// Note that this is different from ids that are used in mir and llir.
    current_uid: Cell<usize>,
}

impl CompileContext {
    pub fn new(compilation_id: CompilationId) -> Self {
        CompileContext {
            compilation_id,
            config: Default::default(),
            input_files: Default::default(),
            current_uid: Default::default(),
        }
    }

    pub fn add_input_file(&mut self, code: Code) -> CodeId {
        self.input_files.add_input(code)
    }

    pub fn get_input_file(&self, id: CodeId) -> &Code {
        self.input_files.get_input(id)
    }

    /// Returns a unique id
    pub fn get_unique_id(&self) -> usize {
        let old = self.current_uid.get();
        self.current_uid.set(old + 1);
        old
    }
}
