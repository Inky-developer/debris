use super::{
    mir_nodes::{MirCall, MirGotoContext, MirRawCommand},
    MirNode,
};

pub trait MirVisitor {
    type Output;

    fn visit_node(&mut self, node: &MirNode) -> Self::Output {
        match node {
            MirNode::Call(call) => self.visit_call(call),
            MirNode::GotoContext(goto_context) => self.visit_goto_context(goto_context),
            MirNode::RawCommand(raw_command) => self.visit_raw_command(raw_command),
        }
    }

    fn visit_call(&mut self, call: &MirCall) -> Self::Output;

    fn visit_goto_context(&mut self, goto_context: &MirGotoContext) -> Self::Output;

    fn visit_raw_command(&mut self, raw_command: &MirRawCommand) -> Self::Output;
}
