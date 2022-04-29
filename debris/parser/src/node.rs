use core::fmt;
use std::fmt::Display;

use debris_common::Span;

use crate::{syntax_tree::SyntaxTree, token::Token};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct NodeId(pub(super) usize);

#[derive(Debug, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub children: Box<[NodeChild]>,
}

impl Node {
    pub fn has_child(&self, st: &SyntaxTree, kind: NodeKind) -> bool {
        for child in self.children.as_ref() {
            if let NodeChild::Node(id) = child {
                let node = &st[*id];
                if node.kind == kind {
                    return true;
                }
            }
        }
        false
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NodeChild {
    Token(Token),
    Node(NodeId),
}

impl NodeChild {
    pub fn span(&self, ast: &SyntaxTree) -> Span {
        match self {
            NodeChild::Token(token) => token.span,
            NodeChild::Node(node) => ast[*node].span,
        }
    }

    pub fn token(&self) -> Option<Token> {
        match self {
            NodeChild::Token(token) => Some(*token),
            NodeChild::Node(_) => None,
        }
    }

    pub fn node_id(&self) -> Option<NodeId> {
        match self {
            NodeChild::Node(node_id) => Some(*node_id),
            NodeChild::Token(_) => None,
        }
    }
}

impl From<NodeId> for NodeChild {
    fn from(v: NodeId) -> Self {
        Self::Node(v)
    }
}

impl From<Token> for NodeChild {
    fn from(v: Token) -> Self {
        Self::Token(v)
    }
}

#[macro_export]
macro_rules! nodes {
    ($($node:expr),*) => {
        Box::new([$($node.into()),*])
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NodeKind {
    Assignment,
    Block,
    Error,
    Function,
    InfixOp,
    ParamDeclaration,
    ParamListDeclaration,
    ParamList,
    ParensValue,
    Path,
    Pattern,
    PostfixOp,
    PrefixOp,
    Root,
    Statement,
    Tuple,
    Update,
    Value,
}

impl fmt::Display for NodeKind {
    #[allow(clippy::use_debug)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

pub struct NodeDisplay<'a> {
    pub(super) ast: &'a SyntaxTree,
    pub(super) source: &'a str,
    pub(super) node_id: NodeId,
    pub(super) indent: usize,
}

impl NodeDisplay<'_> {
    fn writeln(&self, f: &mut fmt::Formatter<'_>, value: impl Display) -> fmt::Result {
        Self::write_indented(f, value, self.indent)?;
        writeln!(f)
    }

    fn write_indented(
        f: &mut fmt::Formatter<'_>,
        value: impl Display,
        indent: usize,
    ) -> fmt::Result {
        write!(f, "{:indent$}{}", "", value, indent = indent * 2)
    }
}

impl fmt::Display for NodeDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let node = &self.ast[self.node_id];
        // self.writeln(f, node.children.len())?;
        self.writeln(f, &node.kind)?;

        for child in node.children.as_ref() {
            match child {
                NodeChild::Token(token) => {
                    Self::write_indented(f, "", self.indent + 1)?;
                    let src = &self.source[token.span.as_slice()].escape_default();
                    writeln!(f, "{token}: '{src}'")
                }
                NodeChild::Node(node_id) => fmt::Display::fmt(
                    &NodeDisplay {
                        ast: self.ast,
                        indent: self.indent + 1,
                        node_id: *node_id,
                        source: self.source,
                    },
                    f,
                ),
            }?;
        }

        Ok(())
    }
}
