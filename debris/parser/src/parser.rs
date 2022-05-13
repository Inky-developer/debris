use core::fmt;
use std::collections::VecDeque;

use debris_common::Span;

use logos::{Lexer, Logos};

use crate::{
    node::{NodeChild, NodeKind},
    syntax_tree::SyntaxTree,
    token::{InfixOperator, PostfixOperator, PrefixOperator, Token, TokenKind},
};

pub fn parse(input: &str) -> SyntaxTree {
    parse_with(input, &parse_root)
}

pub fn parse_with(input: &str, parse_fn: &dyn Fn(&mut Parser) -> ParseResult<()>) -> SyntaxTree {
    let mut parser = Parser::new(input);
    let result = parse_fn(&mut parser);
    if result.is_err() || parser.current.kind != TokenKind::EndOfInput {
        parser
            .recover_with(
                NodeKind::Root,
                [TokenKind::EndOfInput],
                RecoveryOptions {
                    allow_defer: false,
                    ..Default::default()
                },
            )
            .unwrap();
        if parser.stack.len() > 1 {
            parser.end();
        }
    }
    parser.end_root();
    parser.st
}

pub type ParseResult<T> = Result<T, ()>;

/// Options passed to the parsers recover function
#[derive(Debug, Clone, Copy)]
struct RecoveryOptions {
    /// Whether the recovery may be deferred, in case a better recovery path is available
    allow_defer: bool,
    /// Whether to consume the first encountered safety token
    consume_safety_token: bool,
}

impl Default for RecoveryOptions {
    fn default() -> Self {
        RecoveryOptions {
            allow_defer: true,
            consume_safety_token: true,
        }
    }
}

pub struct Parser<'a> {
    tokens: Lexer<'a, TokenKind>,
    peeked_tokens: VecDeque<Token>,
    current: Token,
    st: SyntaxTree,
    stack: Vec<(NodeKind, Vec<NodeChild>)>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let tokens = TokenKind::lexer(input);
        let current = Token {
            kind: TokenKind::Error,
            span: Span::EMPTY,
        };
        let ast = SyntaxTree::default();
        let peeked_tokens = VecDeque::default();
        let stack = Vec::default();
        let mut parser = Parser {
            tokens,
            peeked_tokens,
            current,
            st: ast,
            stack,
        };
        parser.begin(NodeKind::Root);
        parser.skip();
        parser
    }

    /// Consumes the current token
    fn bump(&mut self) -> Token {
        let tok = self.current;
        self.insert(tok);
        self.skip();
        tok
    }

    /// Skips the current token and goes to the next.
    /// The skipped token MUST NOT be discarded.
    /// It must always be possible to reconstruct the input string from the [`Ast`].
    fn skip(&mut self) {
        if let Some(token) = self.peeked_tokens.pop_front() {
            self.current = token;
        } else {
            self.current = self._next_token();
        }
    }

    fn nth_next(&mut self, n: usize) -> Token {
        if n == 0 {
            self.current
        } else if n <= self.peeked_tokens.len() {
            self.peeked_tokens[n - 1]
        } else {
            let n = n;
            let current = self.peeked_tokens.len();
            for _ in current..n {
                let token = self._next_token();
                self.peeked_tokens.push_back(token);
            }
            *self.peeked_tokens.back().unwrap()
        }
    }

    fn nth_non_whitespace(&mut self, index: &mut usize) -> Token {
        loop {
            let current = self.nth_next(*index);
            *index += 1;
            if current.kind != TokenKind::Whitespace {
                return current;
            }
        }
    }

    /// Should only be used by `nth_next` and `skip`
    fn _next_token(&mut self) -> Token {
        let kind = self.tokens.next().unwrap_or(TokenKind::EndOfInput);
        let span = self
            .tokens
            .span()
            .try_into()
            .expect("Input source too large");
        Token { span, kind }
    }

    fn current_stripped(&mut self) -> Token {
        self.consume_whitespace();
        self.current
    }

    /// Consumes all consecutive whitespace and
    /// expects that the current token has given `kind`.
    /// Errors if that is not the case.
    fn expect(&mut self, kind: TokenKind) -> ParseResult<Token> {
        assert_ne!(kind, TokenKind::Whitespace, "Cannot expect() whitespace");
        self.consume_whitespace();

        if self.current.kind == kind {
            Ok(self.current)
        } else {
            Err(())
        }
    }

    /// Consume all consecutive whitespace and then consumes the next token
    /// if it equals `kind`.
    /// Returns whether the first non-whitespace token was consumed.
    ///
    /// # Panics
    /// panics if the passed `kind` is [`TokenKind::Whitespace`]
    fn consume(&mut self, kind: TokenKind) -> ParseResult<Token> {
        self.expect(kind)?;
        Ok(self.bump())
    }

    /// Consumes the first matching token of `options`
    /// Returns [`Err`] if no option matches
    fn consume_first_of(&mut self, options: &[TokenKind]) -> ParseResult<Token> {
        self.consume_first_with(|kind| options.contains(&kind))
    }

    fn consume_first_with(&mut self, predicate: impl Fn(TokenKind) -> bool) -> ParseResult<Token> {
        self.consume_whitespace();
        if predicate(self.current.kind) {
            Ok(self.bump())
        } else {
            Err(())
        }
    }

    fn consume_or_recover_to(&mut self, to_node: NodeKind, kind: TokenKind) -> ParseResult<()> {
        if self.consume(kind).is_err() {
            self.recover(to_node, [kind])
        } else {
            Ok(())
        }
    }

    /// Consumes a single whitespace token or does nothing
    fn consume_whitespace(&mut self) {
        if self.current.kind == TokenKind::Whitespace {
            self.bump();
        }
    }

    fn replace_stack(&mut self, kind: NodeKind) {
        assert!(
            self.stack.len() > 1,
            "At least one non-root context required for modifying stack kind"
        );
        self.stack.last_mut().unwrap().0 = kind;
    }

    /// Begins a node
    fn begin(&mut self, kind: NodeKind) {
        self.consume_whitespace();
        self.stack.push((kind, Vec::default()));
    }

    /// Ends a node
    fn end(&mut self) {
        self.end_with(0);
    }

    /// Ends a node.
    ///
    /// If the node has less than `children_for_flat` children, the node gets inlined.
    fn end_with(&mut self, children_for_flat: usize) {
        assert!(self.stack.len() > 1, "Illegal `end()`, no stack entry left");
        let (kind, children) = self.stack.pop().unwrap();
        self.insert_full_node(children_for_flat, kind, children.into());
    }

    /// Discards the current stack kind and appends the elements to the previous node
    fn end_inline(&mut self) {
        assert!(
            self.stack.len() > 1,
            "Can only inline if at least two nodes are available"
        );
        let (_, mut children) = self.stack.pop().unwrap();
        self.stack.last_mut().unwrap().1.append(&mut children);
    }

    /// Replaces the current context by a new context of `node_kind` and inserts the
    /// elements of the old context as the firsts node to the new context
    fn end_to_new(&mut self, children_for_flat: usize, node_kind: NodeKind) {
        assert!(self.stack.len() > 1, "Illegal `end()`, no stack entry left");
        let (kind, children) = self.stack.pop().unwrap();
        self.begin(node_kind);
        self.insert_full_node(children_for_flat, kind, children.into());
    }

    fn insert_full_node(
        &mut self,
        children_for_flat: usize,
        kind: NodeKind,
        children: Box<[NodeChild]>,
    ) {
        // Inline the children if desired
        if children.len() <= children_for_flat {
            for child in children.as_ref() {
                self.insert(*child);
            }
        } else {
            let id = self.st.insert(kind, children);
            self.insert(id);
        }
    }

    /// Ends the root node and sets `ast.root_node` accordingly
    fn end_root(&mut self) {
        assert_eq!(
            self.stack.len(),
            1,
            "Call to `end_root()` with not one stack entry left"
        );
        let (kind, children) = self.stack.pop().unwrap();
        let id = self.st.insert(kind, children.into());
        self.st.root = Some(id);
    }

    /// Adds a [`NodeChild`] to the current node
    fn insert(&mut self, kind: impl Into<NodeChild>) {
        self.stack.last_mut().unwrap().1.push(kind.into());
    }

    /// Tries to advance the parser to a known safe state, so that parsing can continue
    ///
    /// `to_node` is the node in the parsers stack that should be closed on success.
    /// `safety_tokens` is a list of tokens that, when encountered, allow successful recovery.
    fn recover<const N: usize>(
        &mut self,
        to_node: NodeKind,
        safety_tokens: [TokenKind; N],
    ) -> ParseResult<()> {
        self.recover_with(to_node, safety_tokens, Default::default())
    }

    fn recover_with<const N: usize>(
        &mut self,
        to_node: NodeKind,
        safety_tokens: [TokenKind; N],
        options: RecoveryOptions,
    ) -> ParseResult<()> {
        self.st.errors.push(());

        let mut in_statement = false;
        let mut in_parenthesis = false;
        if options.allow_defer {
            for (kind, _) in &self.stack {
                match kind {
                    NodeKind::Statement => in_statement = true,
                    NodeKind::ParensValue | NodeKind::ParamList => {
                        in_parenthesis = true;
                    }
                    _ => {}
                }
            }
        }

        let mut index = 0;
        loop {
            let token = self.nth_next(index);
            index += 1;

            if safety_tokens.contains(&token.kind) {
                self.begin(NodeKind::Error);
                for _ in 0..(index - 1) {
                    self.bump();
                }

                // Insert dummy token to make sure the error is not empty
                if index == 1 {
                    self.insert(Token {
                        span: self.current.span,
                        kind: TokenKind::UnexpectedToken,
                    });
                }
                self.end();

                // Go to the save stack entry
                while self.stack[self.stack.len() - 1].0 != to_node {
                    self.end();
                }

                if options.consume_safety_token {
                    // Add the save token
                    self.bump();
                }

                return Ok(());
            }

            if token.kind == TokenKind::EndOfInput
                || token.kind == TokenKind::Semicolon && in_statement
                || token.kind == TokenKind::ParenthesisClose && in_parenthesis
            {
                // Defer recovery to a better suited stack entry
                return Err(());
            }
        }
    }
}

impl fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Parser")
            .field("tokens", &(..))
            .field("peeked_tokens", &self.peeked_tokens)
            .field("current", &self.current)
            .field("ast", &(..))
            .field("stack", &self.stack)
            .finish()
    }
}

/// Parses a comma separated list of items
/// Returns a tuple of (`number_of_items`, `number_of_commas`)
fn parse_comma_separated<const N: usize>(
    parser: &mut Parser,
    parse: impl FnMut(&mut Parser) -> ParseResult<()>,
    end: [TokenKind; N],
) -> ParseResult<(usize, usize)> {
    let mut items_parsed = 0;
    let mut commas_parsed = 0;

    parse_comma_separated_inner(parser, parse, end, &mut items_parsed, &mut commas_parsed)
        .map(|()| (items_parsed, commas_parsed))
}

fn parse_comma_separated_inner<const N: usize>(
    parser: &mut Parser,
    mut parse: impl FnMut(&mut Parser) -> ParseResult<()>,
    end: [TokenKind; N],
    items_parsed: &mut usize,
    commas_parsed: &mut usize,
) -> ParseResult<()> {
    let stack_for_recovery = parser.stack.last().unwrap().0;

    if parser.consume(TokenKind::Comma).is_ok() {
        *commas_parsed += 1;
        // Special case, if only a comma is found
        if parser.consume_first_of(&end).is_err() {
            return parser.recover(stack_for_recovery, end);
        }
        return Ok(());
    }

    if parser.consume_first_of(&end).is_ok() {
        return Ok(());
    }

    let mut comma_expected = false;
    while parser.consume_first_of(&end).is_err() {
        match parser.current_stripped().kind {
            TokenKind::Comma if comma_expected => {
                parser.bump();
                *commas_parsed += 1;
                comma_expected = false;
            }
            TokenKind::Comma if !comma_expected => {
                parser.st.errors.push(());
                parser.bump();
                *commas_parsed += 1;
            }
            _ => {
                if comma_expected {
                    parser.st.errors.push(());
                }
                let result = parse(parser);
                if result.is_err() {
                    parser.recover(stack_for_recovery, end)?;
                    break;
                }
                *items_parsed += 1;
                comma_expected = true;
            }
        }
    }

    Ok(())
}

pub(crate) fn parse_root(parser: &mut Parser) -> ParseResult<()> {
    if parser.consume(TokenKind::EndOfInput).is_ok() {
        return Ok(());
    }

    while parser.current_stripped().kind != TokenKind::EndOfInput {
        parse_statement(parser, false)?;
    }

    Ok(())
}

pub(crate) fn parse_block(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Block);

    parser.consume(TokenKind::BraceOpen)?;

    while parser.current_stripped().kind != TokenKind::BraceClose {
        let is_expr = parse_statement(parser, true)?;
        if is_expr {
            break;
        }
    }
    parser.consume_or_recover_to(NodeKind::Block, TokenKind::BraceClose)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_attribute_list_maybe(parser: &mut Parser) -> ParseResult<()> {
    if parser.current_stripped().kind != TokenKind::BracketOpen {
        return Ok(());
    }

    parser.begin(NodeKind::AttributeList);

    parser.consume(TokenKind::BracketOpen)?;
    parse_comma_separated(
        parser,
        |parser| parse_expr(parser, 0, Default::default()),
        [TokenKind::BracketClose],
    )?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_struct_def(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::StructDef);

    parser.consume(TokenKind::KwStruct)?;
    if parser.consume(TokenKind::Ident).is_err() {
        let options = RecoveryOptions {
            consume_safety_token: false,
            ..Default::default()
        };
        parser.recover_with(NodeKind::StructDef, [TokenKind::BraceOpen], options)?;
    }

    parser.consume(TokenKind::BraceOpen)?;
    parse_struct_vars(parser)?;

    while matches!(
        parser.current_stripped().kind,
        TokenKind::KwFunction | TokenKind::BraceOpen
    ) {
        parse_function(parser, false)?;
    }

    parser.consume(TokenKind::BraceClose)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_struct_vars(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::StructVariables);

    let mut expect_comma = false;
    loop {
        match parser.current_stripped().kind {
            TokenKind::Ident => {
                if expect_comma {
                    parser.st.errors.push(());
                }

                parser.begin(NodeKind::StructVar);
                parser.bump();
                if parser.consume(TokenKind::Colon).is_err() || parse_path(parser, true).is_err() {
                    let options = RecoveryOptions {
                        consume_safety_token: false,
                        ..Default::default()
                    };
                    parser.recover_with(
                        NodeKind::StructVar,
                        [TokenKind::Comma, TokenKind::Ident, TokenKind::BraceClose],
                        options,
                    )?;
                }
                parser.end();
                expect_comma = true;
            }
            TokenKind::Comma if expect_comma => {
                expect_comma = false;
                parser.bump();
            }
            TokenKind::Comma if !expect_comma => {
                parser.st.errors.push(());
                parser.bump();
            }
            _ => break,
        }
    }

    parser.end();
    Ok(())
}

pub(crate) fn parse_function(parser: &mut Parser, is_expr: bool) -> ParseResult<()> {
    parser.begin(NodeKind::Function);

    parse_attribute_list_maybe(parser)?;
    parser.consume(TokenKind::KwFunction)?;

    if parser.current_stripped().kind == TokenKind::Ident {
        if is_expr {
            parser.st.errors.push(());
        }

        parser.bump();
    } else if !is_expr {
        parser.st.errors.push(());
    }

    parse_param_list_declaration(parser)?;
    parse_ret_maybe(parser)?;
    parse_block(parser)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_param_list_declaration(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::ParamListDeclaration);

    parser.consume(TokenKind::ParenthesisOpen)?;

    parse_comma_separated(
        parser,
        parse_param_declaration,
        [TokenKind::ParenthesisClose],
    )?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_param_declaration(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::ParamDeclaration);

    let allow_dotted_path = false;
    parse_pattern(parser, allow_dotted_path)?;
    parser.consume(TokenKind::Colon)?;
    parse_pattern(parser, true)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_ret_maybe(parser: &mut Parser) -> ParseResult<()> {
    if parser.current_stripped().kind != TokenKind::ThinArrow {
        return Ok(());
    }

    parser.consume(TokenKind::ThinArrow)?;
    parse_pattern(parser, true)?;

    Ok(())
}

pub(crate) fn parse_module(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Module);

    parser.consume(TokenKind::KwMod)?;

    let allow_dotted_path = false;
    if parse_path(parser, allow_dotted_path).is_err() {
        let options = RecoveryOptions {
            allow_defer: true,
            consume_safety_token: false,
        };
        parser.recover_with(NodeKind::Module, [TokenKind::BraceOpen], options)?;
    }
    parse_block(parser)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_import(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Import);

    parser.consume(TokenKind::KwImport)?;
    let allow_path = false;
    parse_path(parser, allow_path)?;

    parser.end();
    Ok(())
}

/// Parses a statement
///
/// Optionally parses it as an expression and returns whether that happened.
pub(crate) fn parse_statement(parser: &mut Parser, allow_expr: bool) -> ParseResult<bool> {
    parser.begin(NodeKind::Statement);

    let mut require_semicolon = true;

    let result = (|| {
        let kind = parser.current.kind;
        let next = parser.nth_non_whitespace(&mut 1).kind;
        match kind {
            TokenKind::BraceOpen => {
                require_semicolon = false;
                parse_block(parser)
            }
            TokenKind::KwIf => {
                require_semicolon = false;
                parse_branch(parser)
            }
            TokenKind::KwComptime if next == TokenKind::KwIf => {
                require_semicolon = false;
                parse_branch(parser)
            }
            TokenKind::KwLet | TokenKind::KwComptime => parse_assignment(parser),
            TokenKind::BracketOpen | TokenKind::KwFunction => {
                require_semicolon = false;
                parse_function(parser, false)
            }
            TokenKind::KwStruct => {
                require_semicolon = false;
                parse_struct_def(parser)
            }
            TokenKind::KwMod => {
                require_semicolon = false;
                parse_module(parser)
            }
            TokenKind::KwImport => parse_import(parser),
            TokenKind::KwLoop => {
                require_semicolon = false;
                parse_loop(parser)
            }
            TokenKind::KwWhile => {
                require_semicolon = false;
                parse_while(parser)
            }
            _ if lookahead_update(parser) => parse_update(parser),
            _ => {
                parse_expr(parser, 0, Default::default())?;
                if !allow_expr || parser.current_stripped().kind == TokenKind::Semicolon {
                    parser.consume(TokenKind::Semicolon)?;
                    return Ok(false);
                }
                return Ok(true);
            }
        }?;

        if require_semicolon {
            parser.consume(TokenKind::Semicolon)?;
        }
        ParseResult::Ok(false)
    })();

    let parsed_exr = match result {
        Ok(true) => {
            parser.end_inline();
            true
        }
        Ok(false) => {
            parser.end();
            false
        }
        Err(()) => {
            parser.recover(NodeKind::Statement, [TokenKind::Semicolon])?;
            parser.end();
            false
        }
    };

    Ok(parsed_exr)
}

pub(crate) fn parse_assignment(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Assignment);

    let allow_dotted_path = false;
    let tokens = &[TokenKind::KwLet, TokenKind::KwComptime];
    if parser.consume_first_of(tokens).is_err() || parse_pattern(parser, allow_dotted_path).is_err()
    {
        parser.recover(NodeKind::Assignment, [TokenKind::Assign])?;
    } else {
        parser.consume(TokenKind::Assign)?;
    }

    parse_expr(parser, 0, Default::default())?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_update(parser: &mut Parser) -> ParseResult<()> {
    const ASSIGN_OPS: [TokenKind; 6] = [
        TokenKind::Assign,
        TokenKind::AssignPlus,
        TokenKind::AssignMinus,
        TokenKind::AssignTimes,
        TokenKind::AssignDivide,
        TokenKind::AssignModulo,
    ];

    parser.begin(NodeKind::Update);

    let allow_dotted_path = true;
    if parse_pattern(parser, allow_dotted_path).is_err()
        || parser.consume_first_of(&ASSIGN_OPS).is_err()
    {
        parser.recover(NodeKind::Update, ASSIGN_OPS)?;
    }

    parse_expr(parser, 0, Default::default())?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_pattern(parser: &mut Parser, allow_dotted_path: bool) -> ParseResult<()> {
    parser.begin(NodeKind::Pattern);

    match parser.current_stripped().kind {
        TokenKind::ParenthesisOpen => {
            parser.bump();
            parse_comma_separated(
                parser,
                |parser| parse_pattern(parser, allow_dotted_path),
                [TokenKind::ParenthesisClose],
            )?;
        }
        TokenKind::KwFunction if allow_dotted_path => parse_function_pattern(parser)?,
        _ => parse_path(parser, allow_dotted_path)?,
    }

    parser.end();
    Ok(())
}

pub(crate) fn parse_function_pattern(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::FunctionPattern);

    parser.consume(TokenKind::KwFunction)?;
    parser.consume(TokenKind::ParenthesisOpen)?;
    parse_comma_separated(
        parser,
        |parser| parse_pattern(parser, true),
        [TokenKind::ParenthesisClose],
    )?;

    if parser.current_stripped().kind == TokenKind::ThinArrow {
        parser.bump();
        parse_pattern(parser, true)?;
    }

    parser.end();
    Ok(())
}

pub(crate) fn parse_path(parser: &mut Parser, allow_dotted_path: bool) -> ParseResult<()> {
    parser.begin(NodeKind::Path);

    parser.consume(TokenKind::Ident)?;

    let mut is_invalid_path = false;
    while parser.current_stripped().kind == TokenKind::Dot {
        if !allow_dotted_path {
            is_invalid_path = true;
        }

        parser.bump();
        parser.consume(TokenKind::Ident)?;
    }

    if is_invalid_path {
        parser.st.errors.push(());
    }

    parser.end();
    Ok(())
}

/// Looks forward and checks if the current item can be an update.
/// This is a hack to resolve the statement-ambiguity where both of expr and update are valid.
/// For example, `(a, b, c)` could be parsed as a tuple or as a pattern for an update.
///
/// IMPORTANT: This function does not verify that the update is valid,
/// it just verifies that there is something pattern-like followed by an assign op.
fn lookahead_update(parser: &mut Parser) -> bool {
    let mut i = 0;

    let mut parens_level = 0_u32;
    loop {
        match parser.nth_non_whitespace(&mut i).kind {
            TokenKind::ParenthesisOpen => parens_level += 1,
            TokenKind::ParenthesisClose => match parens_level.checked_sub(1) {
                Some(new_val) => parens_level = new_val,
                // If the parenthesis are invalid, use expr for better error recovery
                None => return false,
            },
            TokenKind::Ident | TokenKind::Dot | TokenKind::Comma => {}
            kind if kind.assign_operator().is_some() => return true,
            _ => return false,
        }
    }
}

pub(crate) fn parse_branch(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Branch);

    if parser.current_stripped().kind == TokenKind::KwComptime {
        parser.bump();
    }
    parser.consume(TokenKind::KwIf)?;

    let config = ExpressionConfig {
        allow_complex: false,
    };
    if parse_expr(parser, 0, config).is_err() {
        let options = RecoveryOptions {
            consume_safety_token: false,
            ..Default::default()
        };
        parser.recover_with(NodeKind::Branch, [TokenKind::BraceOpen], options)?;
    }

    parse_block(parser)?;

    if parser.current_stripped().kind == TokenKind::KwElse {
        parser.begin(NodeKind::BranchElse);

        parser.consume(TokenKind::KwElse)?;

        match parser.current_stripped().kind {
            TokenKind::KwIf => parse_branch(parser),
            TokenKind::KwComptime if parser.nth_next(2).kind == TokenKind::KwIf => {
                parse_branch(parser)
            }
            TokenKind::BraceOpen => parse_block(parser),
            _ => {
                parser.st.errors.push(());
                return Err(());
            }
        }?;

        parser.end();
    }

    parser.end();
    Ok(())
}

pub(crate) fn parse_loop(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::InfLoop);

    parser.consume(TokenKind::KwLoop)?;

    if parser.current_stripped().kind != TokenKind::BraceOpen {
        let options = RecoveryOptions {
            consume_safety_token: false,
            ..Default::default()
        };
        parser.recover_with(NodeKind::InfLoop, [TokenKind::BraceOpen], options)?;
    }
    parse_block(parser)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_while(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::WhileLoop);

    parser.consume(TokenKind::KwWhile)?;
    let config = ExpressionConfig {
        allow_complex: false,
    };
    parse_expr(parser, 0, config)?;
    parse_block(parser)?;

    parser.end();
    Ok(())
}

#[derive(Debug, Clone, Copy)]
pub struct ExpressionConfig {
    pub allow_complex: bool,
}
impl Default for ExpressionConfig {
    fn default() -> Self {
        ExpressionConfig {
            allow_complex: true,
        }
    }
}

pub(crate) fn parse_expr(
    parser: &mut Parser,
    min_precedence: u8,
    config: ExpressionConfig,
) -> ParseResult<()> {
    match parse_expr_maybe(parser, min_precedence, config) {
        Ok(true) => Ok(()),
        _ => Err(()),
    }
}

pub(crate) fn parse_expr_maybe(
    parser: &mut Parser,
    min_precedence: u8,
    config: ExpressionConfig,
) -> ParseResult<bool> {
    parser.begin(NodeKind::InfixOp);

    if !parse_value_maybe(parser, config)? {
        parser.stack.pop().unwrap();
        return Ok(false);
    }
    parse_postfix_maybe(parser, min_precedence)?;
    parse_expr_inner(parser, min_precedence, config)?;
    Ok(true)
}

fn parse_expr_inner(
    parser: &mut Parser,
    min_precedence: u8,
    config: ExpressionConfig,
) -> ParseResult<()> {
    loop {
        let peeked = parser.current_stripped();
        if let Some(operator) = peeked.kind.infix_operator() {
            let precedence = operator.precedence();
            if precedence < min_precedence {
                break;
            }
            parser.bump();

            // Small hack to not parse more paths than allowed
            if matches!(operator, InfixOperator::Dot) {
                // On error just parse as normal, because the parser has no trouble recovering.
                let result = parser.expect(TokenKind::Ident);
                if result.is_err() {
                    parser.st.errors.push(());
                }
            }

            parse_expr(parser, precedence + 1, config)?;
            parse_postfix_maybe(parser, min_precedence)?;

            // If a loop completes, the generated expression has to be turned into a single node
            parser.end_to_new(1, NodeKind::InfixOp);
        } else {
            break;
        }
    }

    parser.end_with(1);
    Ok(())
}

fn parse_value_maybe(parser: &mut Parser, config: ExpressionConfig) -> ParseResult<bool> {
    if parse_prefix_maybe(parser, config)? {
        return Ok(true);
    };

    parser.begin(NodeKind::Value);

    let next = parser.nth_non_whitespace(&mut 1).kind;
    match parser.current.kind {
        TokenKind::Int
        | TokenKind::String
        | TokenKind::FormatString
        | TokenKind::BoolTrue
        | TokenKind::BoolFalse => {
            parser.bump();
            Ok(())
        }
        TokenKind::Ident => parse_ident_or_struct_literal(parser, config),
        TokenKind::BracketOpen | TokenKind::KwFunction => parse_function(parser, true),
        TokenKind::ParenthesisOpen => parse_parenthesis_or_tuple(parser),
        TokenKind::BraceOpen if config.allow_complex => parse_block(parser),
        TokenKind::KwIf => parse_branch(parser),
        TokenKind::KwComptime if next == TokenKind::KwIf => parse_branch(parser),
        TokenKind::KwLoop => parse_loop(parser),
        TokenKind::KwWhile => parse_while(parser),
        kind if kind.control_flow_operator().is_some() => parse_control_flow(parser),
        _ => {
            parser.stack.pop().unwrap();
            return Ok(false);
        }
    }?;

    parser.end();
    Ok(true)
}

pub(crate) fn parse_ident_or_struct_literal(
    parser: &mut Parser,
    config: ExpressionConfig,
) -> ParseResult<()> {
    let next = parser.nth_non_whitespace(&mut 1).kind;

    if config.allow_complex && next == TokenKind::BraceOpen {
        parse_struct_literal(parser)
    } else {
        parser.bump();
        Ok(())
    }
}

pub(crate) fn parse_struct_literal(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::StructLiteral);

    parser.consume(TokenKind::Ident)?;
    parser.consume(TokenKind::BraceOpen)?;
    parse_comma_separated(parser, parse_struct_literal_item, [TokenKind::BraceClose])?;

    parser.end();
    Ok(())
}

fn parse_struct_literal_item(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::StructLiteralItem);

    parser.consume(TokenKind::Ident)?;
    parser.consume(TokenKind::Colon)?;
    parse_expr(parser, 0, Default::default())?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_parenthesis_or_tuple(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Tuple);

    parser.consume(TokenKind::ParenthesisOpen)?;
    let result = parse_comma_separated(
        parser,
        |parser| parse_expr(parser, 0, Default::default()),
        [TokenKind::ParenthesisClose],
    );
    match result {
        Ok((items, commas)) if items == 1 && commas == 0 => {
            parser.replace_stack(NodeKind::ParensValue);
        }
        _ => {}
    }

    parser.end();
    Ok(())
}

/// Parses leading prefix operators and control flow operators and returns whether a prefix was parsed
pub(crate) fn parse_prefix_maybe(
    parser: &mut Parser,
    config: ExpressionConfig,
) -> ParseResult<bool> {
    let kind = parser.current_stripped().kind;
    if kind.prefix_operator().is_some() {
        parse_prefix(parser, config)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Parses a single prefix
pub(crate) fn parse_prefix(parser: &mut Parser, config: ExpressionConfig) -> ParseResult<()> {
    parser.begin(NodeKind::PrefixOp);

    let prefix_op = parser.current_stripped().kind.prefix_operator().ok_or(())?;
    match prefix_op {
        PrefixOperator::Minus => {
            parser.bump();
        }
    }

    parse_expr(parser, prefix_op.precedence() + 1, config)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_postfix_maybe(parser: &mut Parser, min_precedence: u8) -> ParseResult<()> {
    // Right now, postfix operations are treated to have infinite precedence.
    // This works, because the only postfix operator is the '.', which just has the highest priority.
    while let Some(postfix) = parser.current_stripped().kind.postfix_operator() {
        if postfix.precedence() >= min_precedence {
            parse_postfix(parser)?;
        } else {
            break;
        }
    }

    Ok(())
}

// Right now only function calls are postfix expressions
pub(crate) fn parse_postfix(parser: &mut Parser) -> ParseResult<()> {
    match parser.current_stripped().kind.postfix_operator() {
        Some(PostfixOperator::Call) => {
            parser.end_to_new(1, NodeKind::PostfixOp);
            parse_param_list(parser)?;
            Ok(())
        }
        None => Err(()),
    }
}

pub(crate) fn parse_param_list(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::ParamList);

    parser.consume(TokenKind::ParenthesisOpen)?;
    parse_comma_separated(
        parser,
        |parser| parse_expr(parser, 0, Default::default()),
        [TokenKind::ParenthesisClose],
    )?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_control_flow(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::ControlFlow);

    parser.consume_first_with(|kind| kind.control_flow_operator().is_some())?;
    parse_expr_maybe(parser, 0, Default::default())?;

    parser.end();
    Ok(())
}
