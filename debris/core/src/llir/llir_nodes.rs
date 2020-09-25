use super::utils::{ItemId, Scoreboard, ScoreboardOperation, ScoreboardValue};

#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    pub id: u64,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FastStore {
    pub scoreboard: Scoreboard,
    pub id: ItemId,
    pub value: ScoreboardValue,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FastStoreFromResult {
    pub scoreboard: Scoreboard,
    pub id: ItemId,
    pub command: Box<Node>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BinaryOperation {
    pub scoreboard: Scoreboard,
    pub id: ItemId,
    pub value: ScoreboardValue,
    pub operation: ScoreboardOperation,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Call {
    pub id: u64,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Condition {}

#[derive(Debug, Eq, PartialEq)]
pub struct Branch {
    pub conditions: Vec<Condition>,
    pub pos_branch: Box<Node>,
    pub neg_branch: Option<Box<Node>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Execute {
    pub command: String,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Node {
    Function(Function),
    FastStore(FastStore),
    FastStoreFromResult(FastStoreFromResult),
    BinaryOperation(BinaryOperation),
    Call(Call),
    Branch(Branch),
    Execute(Execute),
}

// impl Node {
//     fn visit_inner_nodes<F: Fn(&Node)>(&self, visitor: F) {
//         match self {
//             Node::Function(fun) => {
//                 fun.nodes.iter().for_each(visitor);
//             }
//             Node::FastStore(_) => (),
//             Node::FastStoreFromResult(_) => (),
//             Node::BinaryOperation(_) => (),
//             Node::Branch(branch) => {
//                 (visitor)(&branch.pos_branch);
//                 if let Some(ref neg_branch) = branch.neg_branch {
//                     (visitor)(neg_branch)
//                 }
//             }
//             Node::Execute(_) => (),
//         }
//     }
// }
