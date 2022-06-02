use std::{
    fmt::{Display, Write},
    ops::{Index, IndexMut},
};

use crate::{
    error::ParseErrorKind,
    node::{Node, NodeChild, NodeDisplay, NodeId, NodeKind},
    token::TokenKind,
    LocalSpan,
};

#[derive(Debug, Default)]
pub struct SyntaxTree {
    nodes: Vec<Node>,
    pub errors: Vec<ParseErrorKind>,
    pub root: Option<NodeId>,
}

impl SyntaxTree {
    /// Combines similar errors to avoid some visual noise
    pub(super) fn combine_errors(&mut self) {
        if self.errors.is_empty() {
            return;
        }

        let mut new_errors = Vec::new();
        let mut iter = self.errors.iter_mut();
        let mut current_error = iter.next().unwrap().clone();
        for error in iter {
            match (&mut current_error, error) {
                (
                    ParseErrorKind::UnexpectedToken {
                        got: got_a,
                        expected: expected_a,
                    },
                    ParseErrorKind::UnexpectedToken {
                        got: got_b,
                        expected: expected_b,
                    },
                ) if got_a.span == got_b.span => {
                    for other in expected_b {
                        if !expected_a.contains(other) {
                            expected_a.push(other.clone());
                        }
                    }
                }
                (_, other) => {
                    new_errors.push(current_error);
                    current_error = other.clone();
                }
            }
        }
        new_errors.push(current_error);
        self.errors = new_errors;
    }

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
        assert!(
            !children.is_empty(),
            "Got unexpected empty node of kind {}",
            kind
        );

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

    pub fn get_mut(&mut self, NodeId(idx): NodeId) -> &mut Node {
        &mut self.nodes[idx]
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

impl IndexMut<NodeId> for SyntaxTree {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        self.get_mut(index)
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
