use std::rc::Rc;

/// Marks any Input file. Will change or be deleted
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum InputFile {
    /// The main file
    Main,
    /// Any imported file
    Imported(u64),
}

/// A reference to a code object
pub type CodeRef = Rc<Code>;

/// A code object contains the source code and optionally a path to the corresponding file
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Code {
    pub source: String,
    pub path: Option<String>,
}
