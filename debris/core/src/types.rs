/// The type of a class object
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Type {
    StaticInt,
    DynamicInt,
    String,
    Function,
    Class,
    Module,
}

/// Error messages can safely use the debug impl for display
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
