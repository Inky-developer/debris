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
            .recover(NodeKind::Root, &[TokenKind::EndOfInput])
            .unwrap();
        if parser.stack.len() > 1 {
            parser.end();
        }
    }
    parser.end_root();
    parser.ast
}

pub type ParseResult<T> = Result<T, ()>;

pub struct Parser<'a> {
    tokens: Lexer<'a, TokenKind>,
    peeked_tokens: VecDeque<Token>,
    current: Token,
    ast: SyntaxTree,
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
            ast,
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

    /// Consumes a single whitespace token or does nothing
    fn consume_whitespace(&mut self) {
        if self.current.kind == TokenKind::Whitespace {
            self.bump();
        }
    }

    /// Consumes the first token kind of `options` that matches the current token kind.
    /// Returns false if no matching token kind was found.
    fn consume_any_of(&mut self, options: &[TokenKind]) -> ParseResult<Token> {
        options
            .iter()
            .find_map(|kind| self.consume(*kind).ok())
            .ok_or(())
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
            let id = self.ast.insert(kind, children);
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
        let id = self.ast.insert(kind, children.into());
        self.ast.root = Some(id);
    }

    /// Adds a [`NodeChild`] to the current node
    fn insert(&mut self, kind: impl Into<NodeChild>) {
        self.stack.last_mut().unwrap().1.push(kind.into());
    }

    /// Tries to advance the parser to a known safe state, so that parsing can continue
    ///
    /// `to_node` is the node in the parsers stack that should be closed on success.
    /// `safety_tokens` is a list of tokens that, when encountered, allow successful recovery.
    fn recover(&mut self, to_node: NodeKind, safety_tokens: &[TokenKind]) -> ParseResult<()> {
        self.ast.errors.push(());

        let mut in_statement = false;
        let mut in_parenthesis = false;
        for (kind, _) in &self.stack {
            match kind {
                NodeKind::Statement => in_statement = true,
                NodeKind::Pattern | NodeKind::ParensValue | NodeKind::ParamList => {
                    in_parenthesis = true;
                }
                _ => {}
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
                        span: self.tokens.span().try_into().unwrap(),
                        kind: TokenKind::UnexpectedToken,
                    });
                }
                self.end();

                // Go to the save stack entry
                while self.stack[self.stack.len() - 1].0 != to_node {
                    self.end();
                }

                // Add the save token
                self.bump();
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
fn parse_comma_separated(
    parser: &mut Parser,
    parse: impl FnMut(&mut Parser) -> ParseResult<()>,
    end: &[TokenKind],
) -> ParseResult<(usize, usize)> {
    let mut items_parsed = 0;
    let mut commas_parsed = 0;

    parse_comma_separated_inner(parser, parse, end, &mut items_parsed, &mut commas_parsed)
        .map(|()| (items_parsed, commas_parsed))
}

fn parse_comma_separated_inner(
    parser: &mut Parser,
    mut parse: impl FnMut(&mut Parser) -> ParseResult<()>,
    end: &[TokenKind],
    items_parsed: &mut usize,
    commas_parsed: &mut usize,
) -> ParseResult<()> {
    let stack_for_recovery = parser.stack.last().unwrap().0;

    if parser.consume(TokenKind::Comma).is_ok() {
        *commas_parsed += 1;
        // Special case, if only a comma is found
        if parser.consume_any_of(end).is_err() {
            return parser.recover(stack_for_recovery, end);
        }
        return Ok(());
    }

    if parser.consume_any_of(end).is_ok() {
        return Ok(());
    }

    let result = (|| {
        parse(parser)?;
        *items_parsed += 1;

        while parser.consume_any_of(end).is_err() {
            parser.consume(TokenKind::Comma)?;
            *commas_parsed += 1;

            if parser.consume_any_of(end).is_ok() {
                break;
            }
            parse(parser)?;
            *items_parsed += 1;
        }
        ParseResult::Ok(())
    })();

    if result.is_err() {
        parser.recover(stack_for_recovery, end)?;
    }

    Ok(())
}

pub(crate) fn parse_root(parser: &mut Parser) -> ParseResult<()> {
    if parser.consume(TokenKind::EndOfInput).is_ok() {
        return Ok(());
    }

    while parser.current_stripped().kind != TokenKind::EndOfInput {
        parse_statement(parser)?;
    }

    Ok(())
}

pub(crate) fn parse_statement(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Statement);

    let result = (|| {
        match parser.current.kind {
            TokenKind::Let => parse_assignment(parser),
            _ => Err(()),
        }?;

        parser.consume(TokenKind::Semicolon)
    })();

    if result.is_err() {
        parser.recover(NodeKind::Statement, &[TokenKind::Semicolon])?;
    }

    parser.end();
    Ok(())
}

pub(crate) fn parse_assignment(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Assignment);
    if parser.consume(TokenKind::Let).is_err() || parse_pattern(parser).is_err() {
        parser.recover(NodeKind::Assignment, &[TokenKind::Assign])?;
    } else {
        parser.consume(TokenKind::Assign)?;
    }

    parse_exp(parser, 0)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_pattern(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Pattern);

    if parser.consume(TokenKind::ParenthesisOpen).is_ok() {
        parse_comma_separated(parser, parse_pattern, &[TokenKind::ParenthesisClose])?;
    } else {
        parser.consume(TokenKind::Ident)?;
    }

    parser.end();
    Ok(())
}

pub(crate) fn parse_exp(parser: &mut Parser, min_precedence: u8) -> ParseResult<()> {
    parser.begin(NodeKind::InfixOp);

    parse_value(parser)?;

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
                    parser.ast.errors.push(());
                }
            }

            parse_exp(parser, precedence + 1)?;
            parse_postfix_maybe(parser)?;

            // If a loop completes, the generated expression has to be turned into a single node
            parser.end_to_new(1, NodeKind::InfixOp);
        } else {
            break;
        }
    }

    parser.end_with(1);
    Ok(())
}

pub(crate) fn parse_value(parser: &mut Parser) -> ParseResult<()> {
    if parse_prefix_maybe(parser)? {
        return Ok(());
    };

    parser.begin(NodeKind::Value);

    match parser.current.kind {
        TokenKind::Int
        | TokenKind::Ident
        | TokenKind::String
        | TokenKind::FormatString
        | TokenKind::BoolTrue
        | TokenKind::BoolFalse => {
            parser.bump();
        }
        TokenKind::ParenthesisOpen => parse_parenthesis_or_tuple(parser)?,
        _ => return Err(()),
    };

    parse_postfix_maybe(parser)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_parenthesis_or_tuple(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::Tuple);

    parser.consume(TokenKind::ParenthesisOpen)?;
    let result = parse_comma_separated(
        parser,
        |parser| parse_exp(parser, 0),
        &[TokenKind::ParenthesisClose],
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

/// Parses leading prefix operators and returns whether a prefix was parsed
pub(crate) fn parse_prefix_maybe(parser: &mut Parser) -> ParseResult<bool> {
    if parser.current_stripped().kind.prefix_operator().is_some() {
        parse_prefix(parser)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Parses a single prefix
pub(crate) fn parse_prefix(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::PrefixOp);

    match parser.current_stripped().kind.prefix_operator() {
        Some(PrefixOperator::Minus) => {
            parser.bump();
        }
        None => return Err(()),
    }

    parse_value(parser)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_postfix_maybe(parser: &mut Parser) -> ParseResult<()> {
    // Right now, postfix operations are treated to have infinite precedence.
    // This works, because the only postfix operator is the '.', which just has the highest priority.
    while parser.current_stripped().kind.postfix_operator().is_some() {
        parse_postfix(parser)?;
    }

    Ok(())
}

// Right now only function calls are postfix expressions
pub(crate) fn parse_postfix(parser: &mut Parser) -> ParseResult<()> {
    match parser.current_stripped().kind.postfix_operator() {
        Some(PostfixOperator::Call) => {
            parser.end_to_new(0, NodeKind::PostfixOp);
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
        |parser| parse_exp(parser, 0),
        &[TokenKind::ParenthesisClose],
    )?;

    parser.end();
    Ok(())
}
