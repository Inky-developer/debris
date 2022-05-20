use std::{
    fmt::{Display, Write},
    ops::Index,
};

use crate::{
    node::{Node, NodeChild, NodeDisplay, NodeId, NodeKind},
    token::TokenKind,
    LocalSpan,
};

#[derive(Debug, Default)]
pub struct SyntaxTree {
    nodes: Vec<Node>,
    pub errors: Vec<()>,
    pub root: Option<NodeId>,
}

impl SyntaxTree {
    pub fn debug_fmt<'a>(&'a self, source: &'a str) -> impl Display + '_ {
        NodeDisplay {
            ast: self,
            source,
            node_id: self.root.expect("Incomplete ast"),
            indent: 0,
        }
    }

    pub fn to_string(&self, source: &str) -> Box<str> {
        let mut buf = String::new();
        self.write_string(&mut buf, source, self.root.expect("Incomplete ast"));
        buf.into()
    }

    pub fn insert(&mut self, kind: NodeKind, children: Box<[NodeChild]>) -> NodeId {
        assert!(!children.is_empty(), "Got unexpected empty node");

        let idx = self.nodes.len();

        let span_start = children.first().unwrap().span(self);
        let span_end = children.last().unwrap().span(self);
        let span = LocalSpan(span_start.until(span_end.0));
        self.nodes.push(Node {
            kind,
            span,
            children,
        });
        NodeId(idx)
    }

    pub fn get(&self, NodeId(idx): NodeId) -> &Node {
        &self.nodes[idx]
    }

    fn write_string(&self, buf: &mut String, source: &str, node_id: NodeId) {
        let node = &self[node_id];
        for child in node.children.as_ref() {
            match child {
                NodeChild::Node(node) => self.write_string(buf, source, *node),
                NodeChild::Token(token) => {
                    if token.kind != TokenKind::UnexpectedToken {
                        let str = &source[token.span.as_slice()];
                        buf.write_str(str).unwrap();
                    }
                }
            }
        }
    }
}

impl Index<NodeId> for SyntaxTree {
    type Output = Node;

    fn index(&self, value: NodeId) -> &Self::Output {
        self.get(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn does_not_crash(s in "\\PC*") {
            parse(&s);
        }

        #[test]
        fn parsed_to_str_eq_input(s in "\\PC*") {
            let ast = parse(&s);
            assert_eq!(ast.to_string(&s).as_ref(), s);
        }
    }

    #[test]
    fn empty_input() {
        parse("");
    }

    #[test]
    fn empty_error_node() {
        parse("=0");
    }
}
