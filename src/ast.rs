use std::{
    ops::{ControlFlow, Deref},
    rc::Rc,
};

use crate::{
    ast_visitor::AstVisitor,
    node::{Node, NodeChild, NodeId, NodeKind},
    syntax_tree::SyntaxTree,
    token::{Token, TokenKind},
};

pub struct Ast {
    pub program: Program,
}

impl Ast {
    pub fn visit(&self, visitor: &impl AstVisitor) {
        let _ = self.program.visit(visitor);
    }
}

impl From<Rc<SyntaxTree>> for Ast {
    fn from(tree: Rc<SyntaxTree>) -> Ast {
        let root_node = AstNode::new(Rc::clone(&tree), tree.root.unwrap());
        let program = Program(root_node);
        Ast { program }
    }
}

#[derive(Debug, Clone)]
struct AstNode(Rc<AstNodeInner>);

impl AstNode {
    pub(crate) fn new(syntax_tree: Rc<SyntaxTree>, syntax_node: NodeId) -> Self {
        AstNode(Rc::new(AstNodeInner {
            syntax_tree,
            syntax_node,
        }))
    }

    pub fn enum_child(&self, enum_kind: NodeKind) -> Option<AstNode> {
        if self.syntax().kind != enum_kind {
            return None;
        }

        let mut children = self.all_nodes();
        let first = children.next()?;

        if children.next().is_some() {
            None
        } else {
            Some(first)
        }
    }

    pub fn find_token<T: AstToken + 'static>(&self) -> Option<T> {
        self.tokens().next()
    }

    pub fn tokens<T: AstToken + 'static>(&self) -> impl Iterator<Item = T> + '_ {
        self.all_tokens().filter_map(T::from_token)
    }

    pub fn all_tokens(&self) -> impl Iterator<Item = Token> + '_ {
        self.syntax().children.iter().filter_map(NodeChild::token)
    }

    pub fn find_node<T: AstItem + 'static>(&self) -> Option<T> {
        self.nodes().next()
    }

    pub fn nodes<T: AstItem + 'static>(&self) -> impl Iterator<Item = T> + '_ {
        self.all_nodes().filter_map(T::from_node)
    }

    pub fn all_nodes(&self) -> impl Iterator<Item = AstNode> + '_ {
        self.syntax()
            .children
            .iter()
            .filter_map(|child| child.node_id())
            .map(|id| AstNode::new(self.syntax_tree.clone(), id))
    }
}

impl Deref for AstNode {
    type Target = AstNodeInner;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

#[derive(Debug)]
struct AstNodeInner {
    pub syntax_tree: Rc<SyntaxTree>,
    pub syntax_node: NodeId,
}

impl AstNode {
    pub fn syntax(&self) -> &Node {
        &self.syntax_tree[self.syntax_node]
    }
}

trait AstItem: Sized {
    fn from_node(node: AstNode) -> Option<Self>;

    #[must_use]
    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()>;
}

trait AstToken: Sized {
    fn from_token(token: Token) -> Option<Self>;

    #[must_use]
    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()>;
}

pub struct Program(AstNode);
impl AstItem for Program {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Root).then(|| Self(node))
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_program(self)?;
        for statement in self.statements() {
            statement.visit(visitor)?;
        }
        ControlFlow::Continue(())
    }
}
impl Program {
    pub fn statements(&self) -> impl Iterator<Item = Statement> + '_ {
        self.0.nodes()
    }
}

pub enum Statement {
    Assignment(Assignment),
}
impl AstItem for Statement {
    fn from_node(node: AstNode) -> Option<Self> {
        let node = node.enum_child(NodeKind::Statement)?;
        let value = match node.syntax().kind {
            NodeKind::Assignment => Statement::Assignment(Assignment(node)),
            _ => return None,
        };
        Some(value)
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_statement(&self)?;
        match self {
            Statement::Assignment(assignment) => assignment.visit(visitor),
        }
    }
}

pub struct Assignment(AstNode);
impl AstItem for Assignment {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Assignment).then(|| Self(node))
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_assignment(self)?;
        self.pattern().visit(visitor)?;
        self.value().visit(visitor)
    }
}
impl Assignment {
    pub fn pattern(&self) -> Pattern {
        self.0.find_node().unwrap()
    }

    pub fn value(&self) -> Expression {
        self.0.find_node().unwrap()
    }
}

pub enum Pattern {
    Ident(Ident),
    Pattern(Box<Pattern>)
}
impl AstItem for Pattern {
    fn from_node(node: AstNode) -> Option<Self> {
        if node.syntax().kind != NodeKind::Pattern {
            return None;
        }

        node.find_token().map(Pattern::Ident).or_else(|| node.find_node().map(Box::new).map(Pattern::Pattern))
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_pattern(self)?;
        match self {
            Pattern::Ident(ident) => ident.visit(visitor),
            Pattern::Pattern(pattern) => pattern.visit(visitor),
        }
    }
}

pub enum Expression {
    InfixOp(InfixOp),
    Value(Value),
}
impl AstItem for Expression {
    fn from_node(node: AstNode) -> Option<Self> {
        // TODO: check for node type
        let value = match node.syntax().kind {
            NodeKind::InfixOp => Expression::InfixOp(InfixOp(node)),
            NodeKind::Value => Expression::Value(Value::from_node(node).unwrap()),
            _ => return None,
        };
        Some(value)
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_expression(self)?;
        match self {
            Expression::InfixOp(op) => op.visit(visitor),
            Expression::Value(value) => value.visit(visitor),
        }
    }
}

pub struct InfixOp(AstNode);
impl AstItem for InfixOp {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::InfixOp).then(|| Self(node))
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_infix_op(self)?;
        if let Some(op) = self.operator() {
            op.visit(visitor)?;
        }
        self.left().visit(visitor)?;
        if let Some(expr) = self.right() {
            expr.visit(visitor)?;
        }
        ControlFlow::Continue(())
    }
}
impl InfixOp {
    pub fn left(&self) -> Expression {
        self.0.nodes().next().unwrap()
    }

    pub fn right(&self) -> Option<Expression> {
        self.0.nodes().nth(1)
    }

    pub fn operator(&self) -> Option<Operator> {
        self.0.find_token()
    }
}

pub enum Value {
    Int(Int),
    Ident(Ident),
}
impl AstItem for Value {
    fn from_node(node: AstNode) -> Option<Self> {
        if node.syntax().kind != NodeKind::Value {
            return None;
        }

        let child = node.syntax().children.first().unwrap();
        let value = match child {
            NodeChild::Token(token) if token.kind == TokenKind::Int => Value::Int(Int(*token)),
            NodeChild::Token(token) if token.kind == TokenKind::Ident => {
                Value::Ident(Ident(*token))
            }
            _ => unreachable!(),
        };
        Some(value)
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_value(self)?;
        match self {
            Value::Ident(ident) => ident.visit(visitor),
            Value::Int(int) => int.visit(visitor),
        }
    }
}

pub struct Int(Token);
impl AstToken for Int {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::Int).then(|| Self(token))
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_int(self)
    }
}

pub struct Ident(Token);
impl AstToken for Ident {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::Ident).then(|| Self(token))
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_ident(self)
    }
}

pub struct Operator(Token);
impl AstToken for Operator {
    fn from_token(token: Token) -> Option<Self> {
        token.kind.operator().is_some().then(|| Self(token))
    }

    fn visit(&self, visitor: &impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_operator(self)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use proptest::prelude::*;

    use crate::{ast::Ast, parser::parse, syntax_tree::SyntaxTree, ast_visitor::AstVisitor};

    prop_compose! {
        fn ast()(s in "\\PC*") -> (String, SyntaxTree) {
            let ast = parse(&s);
            (s, ast)
        }
    }

    struct Visitor;

    impl AstVisitor for Visitor {

    }

    proptest! {
        #[test]
        fn does_not_crash((_, tree) in ast().prop_filter("Ast may not contain errors", |ast|  ast.1.errors.is_empty())) {
            let ast = Ast::from(Rc::new(tree));
            ast.visit(&Visitor);
        }
    }
}
