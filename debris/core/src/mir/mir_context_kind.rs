use crate::hir::hir_nodes::HirControlKind;

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
    /// A block that is conditonally executed at compile time
    ComptimeConditionalBlock,
    /// A block that is conditionally executed at runtime
    RuntimeConditionalBlock,
}

impl ContextKind {
    /// Returns whether this context can only be executed at runtime
    pub fn is_dynamic(&self) -> bool {
        matches!(self, ContextKind::RuntimeConditionalBlock)
    }

    /// Whether a return statement can select this block
    /// as an exit point
    pub fn can_return(&self) -> bool {
        // A built-in function shouldn't use control flow
        matches!(self, ContextKind::NativeFunction | ContextKind::Function)
    }

    /// Returns whether this context matches this control flow kind
    pub fn matches_control_flow(&self, control_flow: HirControlKind) -> bool {
        match control_flow {
            HirControlKind::Return => self.can_return(),
        }
    }
}
