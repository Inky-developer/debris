use core::fmt;
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
    pub fn visit(&self, visitor: &mut impl AstVisitor) {
        let _ = self.program.visit(visitor);
    }
}

impl From<Rc<SyntaxTree>> for Ast {
    fn from(tree: Rc<SyntaxTree>) -> Ast {
        let root_node = AstNode::new(
            Rc::clone(&tree),
            tree.root.expect("Tree must be initialized"),
        );
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

    pub fn find_token<T: AstToken + 'static>(&self) -> Option<T> {
        self.tokens().next()
    }

    pub fn tokens<T: AstToken + 'static>(&self) -> impl Iterator<Item = T> + '_ {
        self.all_tokens().filter_map(T::from_token)
    }

    pub fn all_tokens(&self) -> impl Iterator<Item = Token> + '_ {
        self.all_children().filter_map(AstNodeOrToken::token)
    }

    pub fn find_node<T: AstItem + 'static>(&self) -> Option<T> {
        self.nodes().next()
    }

    pub fn nodes<T: AstItem + 'static>(&self) -> impl Iterator<Item = T> + '_ {
        self.all_nodes().filter_map(T::from_node)
    }

    pub fn all_nodes(&self) -> impl Iterator<Item = AstNode> + '_ {
        self.all_children().filter_map(AstNodeOrToken::node)
    }

    pub fn all_children(&self) -> impl Iterator<Item = AstNodeOrToken> + '_ {
        self.syntax().children.iter().map(|child| match child {
            NodeChild::Token(token) => AstNodeOrToken::Token(*token),
            NodeChild::Node(node_id) => {
                AstNodeOrToken::Node(AstNode::new(self.syntax_tree.clone(), *node_id))
            }
        })
    }
}

impl Deref for AstNode {
    type Target = AstNodeInner;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

struct AstNodeInner {
    pub syntax_tree: Rc<SyntaxTree>,
    pub syntax_node: NodeId,
}

impl AstNodeInner {
    pub fn syntax(&self) -> &Node {
        &self.syntax_tree[self.syntax_node]
    }
}

impl fmt::Debug for AstNodeInner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.syntax(), f)
    }
}

#[derive(Debug)]
enum AstNodeOrToken {
    Token(Token),
    Node(AstNode),
}

impl AstNodeOrToken {
    pub fn token(self) -> Option<Token> {
        match self {
            AstNodeOrToken::Node(_) => None,
            AstNodeOrToken::Token(token) => Some(token),
        }
    }

    pub fn node(self) -> Option<AstNode> {
        match self {
            AstNodeOrToken::Node(node) => Some(node),
            AstNodeOrToken::Token(_) => None,
        }
    }
}

trait AstItem: Sized {
    fn from_node(node: AstNode) -> Option<Self>;

    #[must_use]
    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()>;
}

trait AstToken: Sized {
    fn from_token(token: Token) -> Option<Self>;

    #[must_use]
    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()>;
}

pub struct Program(AstNode);
impl AstItem for Program {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Root).then(|| Self(node))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
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
    Update(Update),
}
impl AstItem for Statement {
    fn from_node(node: AstNode) -> Option<Self> {
        if node.syntax().kind != NodeKind::Statement {
            return None;
        }
        let value = node
            .find_node()
            .map(Statement::Assignment)
            .or_else(|| node.find_node().map(Statement::Update))
            .unwrap();
        Some(value)
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_statement(self)?;
        match self {
            Statement::Assignment(assignment) => assignment.visit(visitor),
            Statement::Update(update) => update.visit(visitor),
        }
    }
}

pub struct Assignment(AstNode);
impl AstItem for Assignment {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Assignment).then(|| Self(node))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
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

pub struct Update(AstNode);
impl AstItem for Update {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Update).then(|| Self(node))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_update(self)?;
        self.pattern().visit(visitor)?;
        self.value().visit(visitor)
    }
}
impl Update {
    pub fn pattern(&self) -> Pattern {
        self.0.find_node().unwrap()
    }

    pub fn value(&self) -> Expression {
        self.0.find_node().unwrap()
    }
}

pub enum Pattern {
    Ident(Ident),
    Pattern(Box<Pattern>),
}
impl AstItem for Pattern {
    fn from_node(node: AstNode) -> Option<Self> {
        if node.syntax().kind != NodeKind::Pattern {
            return None;
        }

        node.find_token()
            .map(Pattern::Ident)
            .or_else(|| node.find_node().map(Box::new).map(Pattern::Pattern))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
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
        InfixOp::from_node(node.clone())
            .map(Self::InfixOp)
            .or_else(|| AstItem::from_node(node.clone()).map(Self::Value))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
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

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
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

    pub fn operator(&self) -> Option<InfixOperator> {
        self.0.find_token()
    }
}

pub struct PostfixOp(AstNode);
impl AstItem for PostfixOp {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::PostfixOp).then(|| Self(node))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_postfix_op(self)?;
        self.value().visit(visitor)?;
        self.op().visit(visitor)
    }
}
impl PostfixOp {
    pub fn value(&self) -> Value {
        self.0
            .find_node()
            .or_else(|| self.0.find_node().map(Value::PostfixOp))
            .unwrap()
    }

    pub fn op(&self) -> PostfixOperator {
        self.0.find_node().unwrap()
    }
}

pub enum PostfixOperator {
    ParamList(ParamList),
}
impl AstItem for PostfixOperator {
    fn from_node(node: AstNode) -> Option<Self> {
        ParamList::from_node(node).map(Self::ParamList)
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_postfix_operator(self)?;
        match self {
            PostfixOperator::ParamList(param_list) => param_list.visit(visitor),
        }
    }
}

pub struct PrefixOp(AstNode);
impl AstItem for PrefixOp {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::PrefixOp).then(|| Self(node))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_prefix_op(self)?;
        self.value().visit(visitor)?;
        self.op().visit(visitor)
    }
}
impl PrefixOp {
    pub fn value(&self) -> Value {
        self.0.find_node().unwrap()
    }

    pub fn op(&self) -> PrefixOperator {
        self.0.find_token().unwrap()
    }
}

pub enum PrefixOperator {
    Negation(Token),
}
impl AstToken for PrefixOperator {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::OpMinus).then(|| Self::Negation(token))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_prefix_operator(self)
    }
}

pub struct ParamList(AstNode);
impl AstItem for ParamList {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::ParamList).then(|| Self(node))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_param_list(self)?;
        for arg in self.arguments() {
            arg.visit(visitor)?;
        }
        ControlFlow::Continue(())
    }
}
impl ParamList {
    pub fn arguments(&self) -> impl Iterator<Item = Expression> + '_ {
        self.0.nodes()
    }
}

pub enum Value {
    Bool(Bool),
    FormatString(FormatString),
    Ident(Ident),
    Int(Int),
    ParenthesisValue(ParensValue),
    PostfixOp(PostfixOp),
    PrefixOp(PrefixOp),
    String(String),
    Tuple(Tuple),
}
impl AstItem for Value {
    fn from_node(node: AstNode) -> Option<Self> {
        let implicit_value_opt = node
            .find_node()
            .map(Value::PostfixOp)
            .or_else(|| node.find_node().map(Value::PrefixOp));
        if let Some(implicit_value) = implicit_value_opt {
            return Some(implicit_value);
        }

        if node.syntax().kind != NodeKind::Value {
            return None;
        }

        let value = node
            .find_node()
            .map(Value::ParenthesisValue)
            .or_else(|| node.find_node().map(Value::Tuple))
            .or_else(|| node.find_token().map(Value::Bool))
            .or_else(|| node.find_token().map(Value::Int))
            .or_else(|| node.find_token().map(Value::Ident))
            .or_else(|| node.find_token().map(Value::String))
            .or_else(|| node.find_token().map(Value::FormatString))
            .unwrap();

        Some(value)
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_value(self)?;
        match self {
            Value::Bool(bool) => bool.visit(visitor),
            Value::FormatString(fmt_string) => fmt_string.visit(visitor),
            Value::Ident(ident) => ident.visit(visitor),
            Value::Int(int) => int.visit(visitor),
            Value::ParenthesisValue(value) => value.visit(visitor),
            Value::PrefixOp(op) => op.visit(visitor),
            Value::PostfixOp(op) => op.visit(visitor),
            Value::String(string) => string.visit(visitor),
            Value::Tuple(tuple) => tuple.visit(visitor),
        }
    }
}

pub struct ParensValue(AstNode);
impl AstItem for ParensValue {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::ParensValue).then(|| Self(node))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_parens_value(self)?;
        self.expr().visit(visitor)
    }
}
impl ParensValue {
    pub fn expr(&self) -> Expression {
        self.0.find_node().unwrap()
    }
}

pub struct Tuple(AstNode);
impl AstItem for Tuple {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Tuple).then(|| Self(node))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_tuple(self)?;
        for item in self.items() {
            item.visit(visitor)?;
        }
        ControlFlow::Continue(())
    }
}
impl Tuple {
    pub fn items(&self) -> impl Iterator<Item = Expression> + '_ {
        self.0.nodes()
    }
}

pub struct Int(Token);
impl AstToken for Int {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::Int).then(|| Self(token))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_int(self)
    }
}

pub struct Ident(Token);
impl AstToken for Ident {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::Ident).then(|| Self(token))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_ident(self)
    }
}

pub struct String(Token);
impl AstToken for String {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::String).then(|| Self(token))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_string(self)
    }
}

pub struct FormatString(Token);
impl AstToken for FormatString {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::FormatString).then(|| Self(token))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_format_string(self)
    }
}

pub enum Bool {
    True(Token),
    False(Token),
}
impl AstToken for Bool {
    fn from_token(token: Token) -> Option<Self> {
        match token.kind {
            TokenKind::BoolTrue => Some(Bool::True(token)),
            TokenKind::BoolFalse => Some(Bool::False(token)),
            _ => None,
        }
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_bool(self)
    }
}

pub struct InfixOperator(Token);
impl AstToken for InfixOperator {
    fn from_token(token: Token) -> Option<Self> {
        token.kind.infix_operator().is_some().then(|| Self(token))
    }

    fn visit(&self, visitor: &mut impl AstVisitor) -> ControlFlow<()> {
        visitor.visit_infix_operator(self)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use proptest::prelude::*;

    use crate::{ast::Ast, ast_visitor::AstVisitor, parser::parse, syntax_tree::SyntaxTree};

    prop_compose! {
        fn ast()(s in "\\PC*") -> (String, SyntaxTree) {
            let ast = parse(&s);
            (s, ast)
        }
    }

    struct Visitor;

    impl AstVisitor for Visitor {}

    proptest! {
        #[test]
        fn does_not_crash((_, tree) in ast().prop_filter("Ast may not contain errors", |ast|  ast.1.errors.is_empty())) {
            let ast = Ast::from(Rc::new(tree));
            ast.visit(&mut Visitor);
        }
    }
}
