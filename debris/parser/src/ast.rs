use core::fmt;
use std::{ops::Deref, rc::Rc};

use crate::{
    node::{Node, NodeChild, NodeId, NodeKind},
    syntax_tree::SyntaxTree,
    token::{Token, TokenKind},
    LocalSpan,
};

#[derive(Debug)]
pub struct Ast {
    pub program: Program,
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
pub struct AstNode(Rc<AstNodeInner>);

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

pub struct AstNodeInner {
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
pub enum AstNodeOrToken {
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

    pub fn span(&self) -> LocalSpan {
        match self {
            AstNodeOrToken::Node(node) => node.syntax().span,
            AstNodeOrToken::Token(token) => token.span,
        }
    }
}

impl From<Token> for AstNodeOrToken {
    fn from(token: Token) -> Self {
        AstNodeOrToken::Token(token)
    }
}

impl From<AstNode> for AstNodeOrToken {
    fn from(node: AstNode) -> Self {
        AstNodeOrToken::Node(node)
    }
}

pub trait AstItem: Sized {
    fn from_node(node: AstNode) -> Option<Self>;

    fn to_item(&self) -> AstNodeOrToken;
}

pub trait AstToken: Sized {
    fn from_token(token: Token) -> Option<Self>;

    fn to_token(&self) -> Token;
}

#[derive(Debug)]
pub struct Program(AstNode);
impl AstItem for Program {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Root).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Program {
    pub fn statements(&self) -> impl Iterator<Item = Statement> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub struct Comment(pub Token);
impl AstToken for Comment {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::Comment).then(|| Self(token))
    }

    fn to_token(&self) -> Token {
        self.0
    }
}

#[derive(Debug)]
pub struct AttributeList(AstNode);
impl AstItem for AttributeList {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::AttributeList).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl AttributeList {
    pub fn attributes(&self) -> impl Iterator<Item = Expression> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub struct Struct(AstNode);
impl AstItem for Struct {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::StructDef).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Struct {
    pub fn ident(&self) -> Option<Ident> {
        self.0.find_token()
    }

    pub fn variables(&self) -> Option<StructVars> {
        self.0.find_node()
    }

    pub fn items(&self) -> impl Iterator<Item = Function> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub struct StructVars(AstNode);
impl AstItem for StructVars {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::StructVariables).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl StructVars {
    pub fn vars(&self) -> impl Iterator<Item = StructVar> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub struct StructVar(AstNode);
impl AstItem for StructVar {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::StructVar).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl StructVar {
    pub fn ident(&self) -> Ident {
        self.0.find_token().unwrap()
    }

    pub fn typ(&self) -> Path {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub struct Function(AstNode);
impl AstItem for Function {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Function).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Function {
    pub fn attributes(&self) -> Option<AttributeList> {
        self.0.find_node()
    }

    pub fn ident(&self) -> Option<Ident> {
        self.0.find_token()
    }

    pub fn param_declaration_list(&self) -> ParamListDecl {
        self.0.find_node().unwrap()
    }

    pub fn ret_declaration(&self) -> Option<Pattern> {
        self.0.find_node()
    }

    pub fn block(&self) -> Block {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub struct ParamListDecl(AstNode);
impl AstItem for ParamListDecl {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::ParamListDeclaration).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl ParamListDecl {
    pub fn declarations(&self) -> impl Iterator<Item = ParamDecl> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub struct ParamDecl(AstNode);
impl AstItem for ParamDecl {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::ParamDeclaration).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl ParamDecl {
    pub fn lhs(&self) -> Pattern {
        self.0.find_node().unwrap()
    }

    pub fn rhs(&self) -> Pattern {
        self.0.nodes().nth(1).unwrap()
    }
}

#[derive(Debug)]
pub struct Module(AstNode);
impl AstItem for Module {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Module).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Module {
    pub fn ident(&self) -> Ident {
        self.0.find_token().unwrap()
    }

    pub fn block(&self) -> Block {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub struct Import(AstNode);
impl AstItem for Import {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Import).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Import {
    pub fn ident(&self) -> Path {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub struct Block(AstNode);
impl AstItem for Block {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Block).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Block {
    pub fn statements(&self) -> impl Iterator<Item = Statement> + '_ {
        self.0.nodes()
    }

    pub fn last_expr(&self) -> Option<Expression> {
        self.0.find_node()
    }
}

#[derive(Debug)]
pub enum Statement {
    Assignment(Assignment),
    Block(Block),
    Branch(Branch),
    Comment(Comment),
    Expression(Expression),
    Function(Function),
    Import(Import),
    InfLoop(InfLoop),
    Module(Module),
    Update(Update),
    Struct(Struct),
    WhileLoop(WhileLoop),
}
impl AstItem for Statement {
    fn from_node(node: AstNode) -> Option<Self> {
        if node.syntax().kind != NodeKind::Statement {
            return None;
        }
        let value = node
            .find_node()
            .map(Statement::Assignment)
            .or_else(|| node.find_node().map(Statement::Block))
            .or_else(|| node.find_node().map(Statement::Update))
            .or_else(|| node.find_node().map(Statement::Expression))
            .or_else(|| node.find_node().map(Statement::InfLoop))
            .or_else(|| node.find_node().map(Statement::WhileLoop))
            .or_else(|| node.find_node().map(Statement::Function))
            .or_else(|| node.find_node().map(Statement::Module))
            .or_else(|| node.find_node().map(Statement::Import))
            .or_else(|| node.find_node().map(Statement::Branch))
            .or_else(|| node.find_node().map(Statement::Struct))
            .or_else(|| node.find_token().map(Statement::Comment))
            .unwrap();
        Some(value)
    }

    fn to_item(&self) -> AstNodeOrToken {
        match self {
            Statement::Assignment(node) => node.to_item(),
            Statement::Block(node) => node.to_item(),
            Statement::Branch(node) => node.to_item(),
            Statement::Expression(node) => node.to_item(),
            Statement::Function(node) => node.to_item(),
            Statement::Import(node) => node.to_item(),
            Statement::InfLoop(node) => node.to_item(),
            Statement::Module(node) => node.to_item(),
            Statement::Update(node) => node.to_item(),
            Statement::Struct(node) => node.to_item(),
            Statement::WhileLoop(node) => node.to_item(),
            Statement::Comment(_) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct Branch(AstNode);
impl AstItem for Branch {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Branch).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Branch {
    pub fn comptime_token(&self) -> Option<Token> {
        self.0
            .all_tokens()
            .find(|token| token.kind == TokenKind::KwComptime)
    }

    pub fn condition(&self) -> Expression {
        self.0.find_node().unwrap()
    }

    pub fn block(&self) -> Block {
        self.0.find_node().unwrap()
    }

    pub fn else_branch(&self) -> Option<BranchElse> {
        self.0.find_node()
    }
}

#[derive(Debug)]
pub enum BranchElse {
    Block(Block),
    Branch(Branch),
}
impl AstItem for BranchElse {
    fn from_node(node: AstNode) -> Option<Self> {
        if node.syntax().kind != NodeKind::BranchElse {
            return None;
        }
        node.find_node()
            .map(BranchElse::Block)
            .or_else(|| node.find_node().map(BranchElse::Branch))
    }

    fn to_item(&self) -> AstNodeOrToken {
        match self {
            BranchElse::Block(node) => node.to_item(),
            BranchElse::Branch(node) => node.to_item(),
        }
    }
}

#[derive(Debug)]
pub struct InfLoop(AstNode);
impl AstItem for InfLoop {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::InfLoop).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl InfLoop {
    pub fn block(&self) -> Block {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub struct WhileLoop(AstNode);
impl AstItem for WhileLoop {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::WhileLoop).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl WhileLoop {
    pub fn condition(&self) -> Expression {
        self.0.find_node().unwrap()
    }

    pub fn block(&self) -> Block {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub struct Assignment(AstNode);
impl AstItem for Assignment {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Assignment).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Assignment {
    pub fn assign_mode(&self) -> AssignMode {
        self.0.find_token().unwrap()
    }

    pub fn pattern(&self) -> Pattern {
        self.0.find_node().unwrap()
    }

    pub fn value(&self) -> Expression {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub enum AssignMode {
    Let(Token),
    Comptime(Token),
}
impl AstToken for AssignMode {
    fn from_token(token: Token) -> Option<Self> {
        let value = match token.kind {
            TokenKind::KwLet => Self::Let(token),
            TokenKind::KwComptime => Self::Comptime(token),
            _ => return None,
        };
        Some(value)
    }

    fn to_token(&self) -> Token {
        match self {
            AssignMode::Let(token) | AssignMode::Comptime(token) => *token,
        }
    }
}

#[derive(Debug)]
pub struct Update(AstNode);
impl AstItem for Update {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Update).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Update {
    pub fn pattern(&self) -> Pattern {
        self.0.find_node().unwrap()
    }

    pub fn op(&self) -> AssignOperator {
        self.0.find_token().unwrap()
    }

    pub fn expr(&self) -> Expression {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub struct AssignOperator(Token);
impl AstToken for AssignOperator {
    fn from_token(token: Token) -> Option<Self> {
        token
            .kind
            .assign_operator()
            .is_some()
            .then(|| AssignOperator(token))
    }

    fn to_token(&self) -> Token {
        self.0
    }
}

#[derive(Debug)]
pub enum Pattern {
    Function(FunctionPattern),
    Path(Path),
    Tuple(TuplePattern),
}
impl AstItem for Pattern {
    fn from_node(node: AstNode) -> Option<Self> {
        if node.syntax().kind != NodeKind::Pattern {
            return None;
        }

        node.find_node()
            .map(Pattern::Path)
            .or_else(|| node.find_node().map(Pattern::Function))
            .or_else(|| AstItem::from_node(node).map(Pattern::Tuple))
    }

    fn to_item(&self) -> AstNodeOrToken {
        match self {
            Pattern::Function(node) => node.to_item(),
            Pattern::Path(node) => node.to_item(),
            Pattern::Tuple(node) => node.to_item(),
        }
    }
}

#[derive(Debug)]
pub struct TuplePattern(AstNode);
impl AstItem for TuplePattern {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Pattern).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl TuplePattern {
    pub fn sub_patterns(&self) -> impl Iterator<Item = Pattern> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub struct FunctionPattern(AstNode);
impl AstItem for FunctionPattern {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::FunctionPattern).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl FunctionPattern {
    pub fn parameters(&self) -> FunctionPatternParams {
        self.0.find_node().unwrap()
    }

    pub fn ret(&self) -> Option<Pattern> {
        self.0.nodes().nth(2)
    }
}

#[derive(Debug)]
pub struct FunctionPatternParams(AstNode);
impl AstItem for FunctionPatternParams {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::FunctionPatternParams).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl FunctionPatternParams {
    pub fn items(&self) -> impl Iterator<Item = Pattern> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub struct Path(AstNode);
impl AstItem for Path {
    fn from_node(node: AstNode) -> Option<Self> {
        if node.syntax().kind != NodeKind::Path {
            return None;
        }

        Some(Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Path {
    pub fn segments(&self) -> impl Iterator<Item = Ident> + '_ {
        self.0.tokens()
    }
}

#[derive(Debug)]
pub enum Expression {
    InfixOp(InfixOp),
    PrefixOp(PrefixOp),
    PostfixOp(PostfixOp),
    Value(Value),
}
impl AstItem for Expression {
    fn from_node(node: AstNode) -> Option<Self> {
        InfixOp::from_node(node.clone())
            .map(Self::InfixOp)
            .or_else(|| AstItem::from_node(node.clone()).map(Self::PrefixOp))
            .or_else(|| AstItem::from_node(node.clone()).map(Self::PostfixOp))
            .or_else(|| AstItem::from_node(node.clone()).map(Self::Value))
    }

    fn to_item(&self) -> AstNodeOrToken {
        match self {
            Expression::InfixOp(node) => node.to_item(),
            Expression::PrefixOp(node) => node.to_item(),
            Expression::PostfixOp(node) => node.to_item(),
            Expression::Value(node) => node.to_item(),
        }
    }
}

#[derive(Debug)]
pub struct InfixOp(AstNode);
impl AstItem for InfixOp {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::InfixOp).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
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

#[derive(Debug)]
pub struct PostfixOp(AstNode);
impl AstItem for PostfixOp {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::PostfixOp).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl PostfixOp {
    pub fn value(&self) -> Expression {
        self.0.find_node().unwrap()
    }

    pub fn op(&self) -> PostfixOperator {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub enum PostfixOperator {
    ParamList(ParamList),
}
impl AstItem for PostfixOperator {
    fn from_node(node: AstNode) -> Option<Self> {
        ParamList::from_node(node).map(Self::ParamList)
    }

    fn to_item(&self) -> AstNodeOrToken {
        match self {
            PostfixOperator::ParamList(node) => node.to_item(),
        }
    }
}

#[derive(Debug)]
pub struct PrefixOp(AstNode);
impl AstItem for PrefixOp {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::PrefixOp).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl PrefixOp {
    pub fn expr(&self) -> Expression {
        self.0.find_node().unwrap()
    }

    pub fn op(&self) -> PrefixOperator {
        self.0.find_token().unwrap()
    }
}

#[derive(Debug)]
pub enum PrefixOperator {
    Negation(Token),
}
impl AstToken for PrefixOperator {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::OpMinus).then(|| Self::Negation(token))
    }

    fn to_token(&self) -> Token {
        match self {
            PrefixOperator::Negation(token) => *token,
        }
    }
}

#[derive(Debug)]
pub struct ParamList(AstNode);
impl AstItem for ParamList {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::ParamList).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl ParamList {
    pub fn arguments(&self) -> impl Iterator<Item = Expression> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub enum Value {
    Block(Block),
    Bool(Bool),
    Branch(Branch),
    ControlFlow(ControlFlowOperation),
    FormatString(FormatString),
    Function(Function),
    Ident(Ident),
    Int(Int),
    InfLoop(InfLoop),
    ParenthesisValue(ParensValue),
    String(String),
    StructLiteral(StructLiteral),
    Tuple(Tuple),
    WhileLoop(WhileLoop),
}
impl AstItem for Value {
    fn from_node(node: AstNode) -> Option<Self> {
        if node.syntax().kind != NodeKind::Value {
            return None;
        }

        let value = node
            .find_node()
            .map(Value::Block)
            .or_else(|| node.find_node().map(Value::Function))
            .or_else(|| node.find_node().map(Value::ParenthesisValue))
            .or_else(|| node.find_node().map(Value::Tuple))
            .or_else(|| node.find_node().map(Value::InfLoop))
            .or_else(|| node.find_node().map(Value::WhileLoop))
            .or_else(|| node.find_node().map(Value::ControlFlow))
            .or_else(|| node.find_node().map(Value::Branch))
            .or_else(|| node.find_node().map(Value::StructLiteral))
            .or_else(|| node.find_token().map(Value::Bool))
            .or_else(|| node.find_token().map(Value::Int))
            .or_else(|| node.find_token().map(Value::Ident))
            .or_else(|| node.find_token().map(Value::String))
            .or_else(|| node.find_token().map(Value::FormatString))
            .unwrap();

        Some(value)
    }

    fn to_item(&self) -> AstNodeOrToken {
        match self {
            Value::Block(node) => node.to_item(),
            Value::Bool(token) => token.to_token().into(),
            Value::Branch(node) => node.to_item(),
            Value::ControlFlow(node) => node.to_item(),
            Value::FormatString(token) => token.to_token().into(),
            Value::Function(node) => node.to_item(),
            Value::Ident(token) => token.to_token().into(),
            Value::Int(token) => token.to_token().into(),
            Value::InfLoop(node) => node.to_item(),
            Value::ParenthesisValue(node) => node.to_item(),
            Value::String(token) => token.to_token().into(),
            Value::StructLiteral(node) => node.to_item(),
            Value::Tuple(node) => node.to_item(),
            Value::WhileLoop(node) => node.to_item(),
        }
    }
}

#[derive(Debug)]
pub struct StructLiteral(AstNode);
impl AstItem for StructLiteral {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::StructLiteral).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl StructLiteral {
    pub fn ident(&self) -> Ident {
        self.0.find_token().unwrap()
    }

    pub fn items(&self) -> impl Iterator<Item = StructLiteralItem> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub struct StructLiteralItem(AstNode);
impl AstItem for StructLiteralItem {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::StructLiteralItem).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl StructLiteralItem {
    pub fn ident(&self) -> Ident {
        self.0.find_token().unwrap()
    }

    pub fn expr(&self) -> Expression {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub struct ControlFlowOperation(AstNode);
impl AstItem for ControlFlowOperation {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::ControlFlow).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl ControlFlowOperation {
    pub fn op(&self) -> ControlFlowOperator {
        self.0.find_token().unwrap()
    }

    pub fn expr(&self) -> Option<Expression> {
        self.0.find_node()
    }
}

#[derive(Debug)]
pub struct ControlFlowOperator(Token);
impl AstToken for ControlFlowOperator {
    fn from_token(token: Token) -> Option<Self> {
        token
            .kind
            .control_flow_operator()
            .is_some()
            .then(|| Self(token))
    }

    fn to_token(&self) -> Token {
        self.0
    }
}

#[derive(Debug)]
pub struct ParensValue(AstNode);
impl AstItem for ParensValue {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::ParensValue).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl ParensValue {
    pub fn expr(&self) -> Expression {
        self.0.find_node().unwrap()
    }
}

#[derive(Debug)]
pub struct Tuple(AstNode);
impl AstItem for Tuple {
    fn from_node(node: AstNode) -> Option<Self> {
        (node.syntax().kind == NodeKind::Tuple).then(|| Self(node))
    }

    fn to_item(&self) -> AstNodeOrToken {
        self.0.clone().into()
    }
}
impl Tuple {
    pub fn items(&self) -> impl Iterator<Item = Expression> + '_ {
        self.0.nodes()
    }
}

#[derive(Debug)]
pub struct Int(Token);
impl AstToken for Int {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::Int).then(|| Self(token))
    }

    fn to_token(&self) -> Token {
        self.0
    }
}

#[derive(Debug)]
pub struct Ident(Token);
impl AstToken for Ident {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::Ident).then(|| Self(token))
    }

    fn to_token(&self) -> Token {
        self.0
    }
}

#[derive(Debug)]
pub struct String(Token);
impl AstToken for String {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::String).then(|| Self(token))
    }

    fn to_token(&self) -> Token {
        self.0
    }
}

#[derive(Debug)]
pub struct FormatString(Token);
impl AstToken for FormatString {
    fn from_token(token: Token) -> Option<Self> {
        (token.kind == TokenKind::FormatString).then(|| Self(token))
    }

    fn to_token(&self) -> Token {
        self.0
    }
}

#[derive(Debug)]
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

    fn to_token(&self) -> Token {
        match self {
            Bool::True(token) | Bool::False(token) => *token,
        }
    }
}

#[derive(Debug)]
pub struct InfixOperator(Token);
impl AstToken for InfixOperator {
    fn from_token(token: Token) -> Option<Self> {
        token.kind.infix_operator().is_some().then(|| Self(token))
    }

    fn to_token(&self) -> Token {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use crate::{parser::parse, syntax_tree::SyntaxTree};

    prop_compose! {
        fn ast()(s in "\\PC*") -> (String, SyntaxTree) {
            let ast = parse(&s);
            (s, ast)
        }
    }
}
