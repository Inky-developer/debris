use super::{
    mir_nodes::{MirBranchIf, MirCall, MirGotoContext},
    MirNode,
};

pub trait MirVisitor {
    type Output;

    fn visit_node(&mut self, node: &MirNode) -> Self::Output {
        match node {
            MirNode::Call(call) => self.visit_call(call),
            MirNode::GotoContext(goto_context) => self.visit_goto_context(goto_context),
            MirNode::BranchIf(branch) => self.visit_branch_if(branch),
        }
    }

    fn visit_call(&mut self, call: &MirCall) -> Self::Output;

    fn visit_goto_context(&mut self, goto_context: &MirGotoContext) -> Self::Output;

    fn visit_branch_if(&mut self, branch_if: &MirBranchIf) -> Self::Output;
}
