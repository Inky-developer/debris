use super::mir_nodes::{MirCall, MirGotoContext, MirRawCommand};

pub trait MirVisitor {
    type Output;

    fn visit_call(&mut self, call: &MirCall) -> Self::Output;

    fn visit_goto_context(&mut self, goto_context: &MirGotoContext) -> Self::Output;

    fn visit_raw_command(&mut self, raw_command: &MirRawCommand) -> Self::Output;
}
