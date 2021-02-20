use crate::hir::hir_nodes::HirControlKind;

/// The mode how a `MirContext` exits
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ControlFlowMode {
    /// Normal exit, just continue with the next nodes
    Normal,
    /// Look for the current function and
    /// call the context after it
    Return,
    /// This function can never return a value,
    /// because the return point can't be reached (due to an infinite loop)
    Never,
}

impl ControlFlowMode {
    pub fn is_normal(&self) -> bool {
        matches!(self, ControlFlowMode::Normal)
    }

    /// Returns `true` if the control_flow_mode is [`Never`].
    pub fn is_never(&self) -> bool {
        matches!(self, Self::Never)
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
