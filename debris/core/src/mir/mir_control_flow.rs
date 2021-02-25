use crate::hir::hir_nodes::HirControlKind;

/// The mode how a `MirContext` exits
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ControlFlowMode {
    /// Normal exit, just continue with the next nodes
    Normal,
    /// Look for the current function and
    /// call the context after it
    Return,
    /// Calls the context after the loop
    Break,
    /// Directly jumps to the top of the loop
    Continue,
}

impl ControlFlowMode {
    pub fn is_normal(&self) -> bool {
        matches!(self, ControlFlowMode::Normal)
    }

    /// Returns whether this control flow mode exits the current block.
    /// Right now, false is only returned for continue.
    pub fn exits_block(&self) -> bool {
        !matches!(self, ControlFlowMode::Continue)
    }
}

impl From<HirControlKind> for ControlFlowMode {
    fn from(kind: HirControlKind) -> Self {
        match kind {
            HirControlKind::Return => ControlFlowMode::Return,
            HirControlKind::Break => ControlFlowMode::Break,
            HirControlKind::Continue => ControlFlowMode::Continue,
        }
    }
}

impl Default for ControlFlowMode {
    fn default() -> Self {
        ControlFlowMode::Normal
    }
}
