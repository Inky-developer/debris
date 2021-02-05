use crate::hir::hir_nodes::HirControlKind;

/// The mode how a `MirContext` exits
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ControlFlowMode {
    /// Normal exit, just continue with the next nodes
    Normal,
    /// Look for the current function and
    /// call the context after it
    Return,
}

impl ControlFlowMode {
    pub fn is_normal(&self) -> bool {
        matches!(self, ControlFlowMode::Normal)
    }
}

impl From<HirControlKind> for ControlFlowMode {
    fn from(kind: HirControlKind) -> Self {
        match kind {
            HirControlKind::Return => ControlFlowMode::Return,
        }
    }
}

impl Default for ControlFlowMode {
    fn default() -> Self {
        ControlFlowMode::Normal
    }
}
