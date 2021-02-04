/// Contains all possible 'kinds' of contexts.
/// This is for example used to determine wt
/// which context to return when evaluating a return statement.
/// Another use case is finding out, whether this context can
/// be evaluated at comptime.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ContextKind {
    /// A simple block created by `{}`
    Block,
    /// A built-in function
    Function,
    /// A function created by a user
    NativeFunction,
    /// A block that is conditonally executed
    ConditionalBlock,
}

impl ContextKind {
    /// Whether a return statement can select this block
    /// as an exit point
    pub fn can_return(&self) -> bool {
        // A built-in function shouldn't use control flow
        matches!(self, ContextKind::NativeFunction | ContextKind::Function)
    }
}
