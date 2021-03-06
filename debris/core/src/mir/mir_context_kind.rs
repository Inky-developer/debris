use crate::{hir::hir_nodes::HirControlKind, CompileContext, ObjectRef};

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
    /// A runtime loop
    Loop,
    /// The body of a struct definition
    Struct,
}

impl ContextKind {
    /// Returns whether this context can only be executed at runtime
    pub fn is_dynamic(&self) -> bool {
        matches!(
            self,
            ContextKind::RuntimeConditionalBlock | ContextKind::Loop
        )
    }

    /// Returns the default return value of this context
    pub fn default_return(&self, ctx: &CompileContext) -> ObjectRef {
        match self {
            ContextKind::Block => ctx.type_ctx().null(),
            ContextKind::Function => ctx.type_ctx().null(),
            ContextKind::NativeFunction => ctx.type_ctx().null(),
            ContextKind::ComptimeConditionalBlock => ctx.type_ctx().null(),
            ContextKind::RuntimeConditionalBlock => ctx.type_ctx().null(),
            ContextKind::Loop => ctx.type_ctx().never(),
            ContextKind::Struct => ctx.type_ctx().null(),
        }
    }

    /// Whether a return statement can select this block
    /// as an exit point
    pub fn is_function(&self) -> bool {
        // A built-in function shouldn't use control flow
        matches!(self, ContextKind::NativeFunction | ContextKind::Function)
    }

    /// Whether a break or continue statement can select this block
    /// As an exit point
    pub fn is_loop(&self) -> bool {
        matches!(self, ContextKind::Loop)
    }

    /// Returns whether this context matches this control flow kind
    pub fn matches_control_flow(&self, control_flow: HirControlKind) -> bool {
        match control_flow {
            HirControlKind::Return => self.is_function(),
            HirControlKind::Break => self.is_loop(),
            HirControlKind::Continue => self.is_loop(),
        }
    }
}
