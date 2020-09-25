use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum InputFile {
    /// The main file
    Main,
    /// Any imported file
    Imported(u64),
}

pub type CodeRef = Rc<Code>;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Code {
    pub source: String,
    pub path: Option<String>,
}
