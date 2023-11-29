use core::fmt;
use std::collections::VecDeque;

use debris_common::Span;

use logos::{Lexer, Logos};

use crate::{
    error::{ExpectedItem, ParseErrorKind},
    format_string_parser::{self, FormatStringParser},
    node::{NodeChild, NodeKind},
    syntax_tree::SyntaxTree,
    token::{InfixOperator, PostfixOperator, Token, TokenKind},
    LocalSpan,
};

pub fn parse(input: &str) -> SyntaxTree {
    parse_with(input, &parse_root)
}

pub fn parse_with(input: &str, parse_fn: &dyn Fn(&mut Parser) -> ParseResult<()>) -> SyntaxTree {
    let mut parser = Parser::new(input);
    let result = parse_fn(&mut parser);
    if result.is_err() || parser.current.kind != TokenKind::EndOfInput {
        if result.is_ok() {
            parser
                .st
                .errors
                .push(ParseErrorKind::LeftOverInput(parser.current));
        }
        parser
            .recover_with(
                0,
                [TokenKind::EndOfInput],
                RecoveryOptions {
                    allow_defer: false,
                    ..Default::default()
                },
            )
            .unwrap();
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
    /// Whether to automatically create an unexpected token error
    create_error: bool,
}

impl Default for RecoveryOptions {
    fn default() -> Self {
        RecoveryOptions {
            allow_defer: true,
            consume_safety_token: true,
            create_error: false,
        }
    }
}

pub struct Parser<'a> {
    pub input: &'a str,
    tokens: Lexer<'a, TokenKind>,
    peeked_tokens: VecDeque<Token>,
    current: Token,
    pub st: SyntaxTree,
    pub stack: Vec<(NodeKind, Vec<NodeChild>, usize)>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let tokens = TokenKind::lexer(input);
        let current = Token {
            kind: TokenKind::Error,
            span: LocalSpan(Span::EMPTY),
        };
        let ast = SyntaxTree::default();
        let peeked_tokens = VecDeque::default();
        let stack = Vec::default();
        let mut parser = Parser {
            input,
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
    /// It must always be possible to reconstruct the input string from the `Ast`.
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
            if !current.kind.is_whitespace() {
                return current;
            }
        }
    }

    /// Should only be used by `nth_next` and `skip`
    fn _next_token(&mut self) -> Token {
        let kind = match self.tokens.next() {
            Some(Ok(kind)) => kind,
            Some(Err(())) => TokenKind::Error,
            None => TokenKind::EndOfInput,
        };
        let span = self
            .tokens
            .span()
            .try_into()
            .map(LocalSpan)
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
        assert!(!kind.is_whitespace(), "Cannot expect() whitespace");
        self.consume_whitespace();

        if self.current.kind == kind {
            Ok(self.current)
        } else {
            self.st.errors.push(ParseErrorKind::UnexpectedToken {
                got: self.current,
                expected: vec![kind.into()],
            });
            Err(())
        }
    }

    /// Consume all consecutive whitespace and then consumes the next token
    /// if it equals `kind`.
    /// Returns whether the first non-whitespace token was consumed.
    ///
    /// # Panics
    /// panics if the passed `kind` is a whitespace
    fn consume(&mut self, kind: TokenKind) -> ParseResult<Token> {
        if self.expect(kind).is_err() {
            return Err(());
        }
        Ok(self.bump())
    }

    /// Consumes the first matching token of `options`
    /// Returns [`Err`] if no option matches
    fn consume_first_of(&mut self, options: &[TokenKind]) -> ParseResult<Token> {
        if let Some(token) = self.consume_first_with(|kind| options.contains(&kind)) {
            Ok(token)
        } else {
            self.st.errors.push(ParseErrorKind::UnexpectedToken {
                got: self.current,
                expected: options.iter().map(Clone::clone).map(Into::into).collect(),
            });
            Err(())
        }
    }

    fn consume_first_with(&mut self, predicate: impl Fn(TokenKind) -> bool) -> Option<Token> {
        self.consume_whitespace();
        if predicate(self.current.kind) {
            Some(self.bump())
        } else {
            None
        }
    }

    /// Consumes a single whitespace token or does nothing
    fn consume_whitespace(&mut self) {
        while self.current.kind.is_whitespace() {
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
    pub fn begin(&mut self, kind: NodeKind) -> usize {
        self.begin_with(kind, 0)
    }

    pub fn begin_with(&mut self, kind: NodeKind, children_for_flat: usize) -> usize {
        self.consume_whitespace();
        self.stack.push((kind, Vec::default(), children_for_flat));
        self.stack.len() - 1
    }

    /// Ends a node
    pub fn end(&mut self) {
        assert!(self.stack.len() > 1, "Illegal `end()`, no stack entry left");
        let (kind, children, children_for_flat) = self.stack.pop().unwrap();
        self.insert_full_node(children_for_flat, kind, children.into());
    }

    /// Discards the current stack kind and appends the elements to the previous node
    fn end_inline(&mut self) {
        assert!(
            self.stack.len() > 1,
            "Can only inline if at least two nodes are available"
        );
        let (_, mut children, _) = self.stack.pop().unwrap();
        self.stack.last_mut().unwrap().1.append(&mut children);
    }

    /// Creates a new node of kind `node_kind` and ends the current node as the first child into it
    fn end_to_new(&mut self, node_kind: NodeKind, children_for_flat: usize) {
        assert!(self.stack.len() > 1, "Illegal `end()`, no stack entry left");
        let last = self.stack.last_mut().unwrap();
        let last_kind = last.0;
        last.0 = node_kind;
        let mut children = std::mem::take(&mut last.1);
        self.begin_with(last_kind, children_for_flat);
        self.stack.last_mut().unwrap().1.append(&mut children);
        self.end();
    }

    fn insert_full_node(
        &mut self,
        children_for_flat: usize,
        kind: NodeKind,
        children: Box<[NodeChild]>,
    ) {
        // Inline the children if desired
        let significant_children = children
            .iter()
            .filter(|child| {
                !matches!(
                    child,
                    NodeChild::Token(Token {
                        span: _,
                        kind,
                    }) if kind.is_whitespace()
                )
            })
            .count();
        if significant_children <= children_for_flat {
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
        let (kind, children, _) = self.stack.pop().unwrap();
        let id = self.st.insert(kind, children.into());
        self.st.root = Some(id);

        self.st.combine_errors();
    }

    /// Adds a [`NodeChild`] to the current node
    pub fn insert(&mut self, kind: impl Into<NodeChild>) {
        self.stack.last_mut().unwrap().1.push(kind.into());
    }

    /// Tries to advance the parser to a known safe state, so that parsing can continue
    ///
    /// `to_node` is the node in the parsers stack that should be closed on success.
    /// `safety_tokens` is a list of tokens that, when encountered, allow successful recovery.
    fn recover<const N: usize>(
        &mut self,
        to_stack_idx: usize,
        safety_tokens: [TokenKind; N],
    ) -> ParseResult<()> {
        self.recover_with(to_stack_idx, safety_tokens, Default::default())
    }

    fn recover_with<const N: usize>(
        &mut self,
        to_stack_idx: usize,
        safety_tokens: [TokenKind; N],
        options: RecoveryOptions,
    ) -> ParseResult<()> {
        if options.create_error {
            self.st.errors.push(ParseErrorKind::UnexpectedToken {
                got: self.current,
                expected: safety_tokens
                    .iter()
                    .map(Clone::clone)
                    .map(Into::into)
                    .collect(),
            });
        }

        let mut in_statement = false;
        let mut in_parenthesis = false;
        if options.allow_defer {
            for (kind, _, _) in &self.stack {
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
                while self.stack.len() - 1 > to_stack_idx {
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
    let recovery_stack_idx = parser.stack.len() - 1;

    if parser.current_stripped().kind == TokenKind::Comma {
        parser.bump();
        *commas_parsed += 1;
        // Special case, if only a comma is found
        if parser.consume_first_of(&end).is_err() {
            return parser.recover(recovery_stack_idx, end);
        }
        return Ok(());
    }

    let mut comma_expected = false;
    while !end.contains(&parser.current_stripped().kind) {
        match parser.current_stripped().kind {
            TokenKind::Comma if comma_expected => {
                parser.bump();
                *commas_parsed += 1;
                comma_expected = false;
            }
            TokenKind::Comma if !comma_expected => {
                parser
                    .st
                    .errors
                    .push(ParseErrorKind::UnexpectedComma(parser.current));
                parser.bump();
                *commas_parsed += 1;
            }
            _ => {
                if comma_expected {
                    parser.st.errors.push(ParseErrorKind::UnexpectedToken {
                        got: parser.current,
                        expected: vec![TokenKind::Comma.into()],
                    });
                }
                let result = parse(parser);
                if result.is_err() {
                    parser.recover(recovery_stack_idx, end)?;
                    return Ok(());
                }
                *items_parsed += 1;
                comma_expected = true;
            }
        }
    }
    parser.bump();

    Ok(())
}

pub(crate) fn parse_root(parser: &mut Parser) -> ParseResult<()> {
    if parser.current_stripped().kind == TokenKind::EndOfInput {
        parser.bump();
        return Ok(());
    }

    while parser.current_stripped().kind != TokenKind::EndOfInput {
        parse_statement(parser, false)?;
    }

    Ok(())
}

pub(crate) fn parse_block(parser: &mut Parser, implicit_return: bool) -> ParseResult<()> {
    let stack_idx = parser.begin(NodeKind::Block);

    parser.consume(TokenKind::BraceOpen)?;

    let (mut is_expr, mut can_be_expression) = (false, false);
    let mut node = None;
    while parser.current_stripped().kind != TokenKind::BraceClose {
        (is_expr, can_be_expression) = parse_statement(parser, true)?;
        if !is_expr && can_be_expression {
            node = match parser.stack.last().unwrap().1.last().unwrap() {
                NodeChild::Token(_) => unreachable!(),
                NodeChild::Node(id) => Some(*id),
            };
        }
        if is_expr {
            break;
        }
    }

    // Convert the last node into an expression, if possible
    if implicit_return && !is_expr && can_be_expression {
        parser.st[node.unwrap()].kind = NodeKind::Value;
    }
    if parser.current.kind == TokenKind::BraceClose {
        parser.bump();
    } else {
        parser.st.errors.push(ParseErrorKind::UnexpectedToken {
            got: parser.current,
            expected: vec![TokenKind::BraceClose.into(), TokenKind::Semicolon.into()],
        });
        parser.recover(stack_idx, [TokenKind::BraceClose])?;
    }

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
    let stack_idx = parser.begin(NodeKind::StructDef);

    parser.consume(TokenKind::KwStruct)?;
    if parser.consume(TokenKind::Ident).is_err() {
        let options = RecoveryOptions {
            consume_safety_token: false,
            ..Default::default()
        };
        parser.recover_with(stack_idx, [TokenKind::BraceOpen], options)?;
    }

    parser.consume(TokenKind::BraceOpen)?;
    parse_struct_vars(parser)?;

    while matches!(
        parser.current_stripped().kind,
        TokenKind::KwFunction | TokenKind::BracketOpen | TokenKind::KwComptime
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
                    parser.st.errors.push(ParseErrorKind::UnexpectedToken {
                        got: parser.current,
                        expected: vec![TokenKind::Comma.into()],
                    });
                }

                let stack_idx = parser.begin(NodeKind::StructVar);
                parser.bump();
                if parser.consume(TokenKind::Colon).is_err() || parse_path(parser, true).is_err() {
                    let options = RecoveryOptions {
                        consume_safety_token: false,
                        ..Default::default()
                    };
                    parser.recover_with(
                        stack_idx,
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
                parser
                    .st
                    .errors
                    .push(ParseErrorKind::UnexpectedComma(parser.current));
                parser.bump();
            }
            _ => break,
        }
    }

    parser.end();
    Ok(())
}

pub(crate) fn parse_function(parser: &mut Parser, allow_expr: bool) -> ParseResult<()> {
    parser.begin(NodeKind::Function);

    parse_attribute_list_maybe(parser)?;
    if parser.current_stripped().kind == TokenKind::KwComptime {
        parser.bump();
    }
    parser.consume(TokenKind::KwFunction)?;

    if parser.current_stripped().kind == TokenKind::Ident {
        parser.bump();
    } else if !allow_expr {
        parser.st.errors.push(ParseErrorKind::UnexpectedToken {
            got: parser.current,
            expected: vec![TokenKind::Ident.into()],
        });
    }

    parse_param_list_declaration(parser)?;
    parse_ret_maybe(parser)?;
    parse_block(parser, true)?;

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
    let stack_idx = parser.begin(NodeKind::Module);

    parser.consume(TokenKind::KwMod)?;

    if parser.consume(TokenKind::Ident).is_err() {
        let options = RecoveryOptions {
            consume_safety_token: false,
            ..Default::default()
        };
        parser.recover_with(stack_idx, [TokenKind::BraceOpen], options)?;
    }
    parse_block(parser, false)?;

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
pub(crate) fn parse_statement(parser: &mut Parser, allow_expr: bool) -> ParseResult<(bool, bool)> {
    let stack_idx = parser.begin(NodeKind::Statement);

    let mut require_semicolon = true;

    let result = (|| {
        let mut can_transform_into_value = false;
        let kind = parser.current.kind;
        let next = parser.nth_non_whitespace(&mut 1).kind;
        match kind {
            TokenKind::BraceOpen => {
                require_semicolon = false;
                can_transform_into_value = true;
                parse_block(parser, true)
            }
            TokenKind::KwIf => {
                require_semicolon = false;
                can_transform_into_value = true;
                parse_branch(parser)
            }
            TokenKind::KwComptime if next == TokenKind::KwIf => {
                require_semicolon = false;
                can_transform_into_value = true;
                parse_branch(parser)
            }
            TokenKind::BracketOpen | TokenKind::KwFunction => {
                require_semicolon = false;
                can_transform_into_value = true;
                parse_function(parser, true)
            }
            TokenKind::KwComptime if next == TokenKind::KwFunction => {
                require_semicolon = false;
                can_transform_into_value = true;
                parse_function(parser, true)
            }
            TokenKind::KwLet | TokenKind::KwComptime => parse_assignment(parser),
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
                can_transform_into_value = true;
                parse_loop(parser)
            }
            TokenKind::KwWhile => {
                require_semicolon = false;
                can_transform_into_value = true;
                parse_while(parser)
            }
            _ if lookahead_update(parser) => parse_update(parser),
            _ => {
                parse_expr(parser, 0, Default::default())?;
                if !allow_expr || parser.current_stripped().kind == TokenKind::Semicolon {
                    parser.consume(TokenKind::Semicolon)?;
                    return Ok((false, false));
                }
                return Ok((true, false));
            }
        }?;

        if require_semicolon {
            parser.consume(TokenKind::Semicolon)?;
        }
        ParseResult::Ok((false, can_transform_into_value && !require_semicolon))
    })();

    let parsed_exr = match result {
        Ok((true, convertible)) => {
            parser.end_inline();
            (true, convertible)
        }
        Ok((false, convertible)) => {
            parser.end();
            (false, convertible)
        }
        Err(()) => {
            parser.recover(stack_idx, [TokenKind::Semicolon])?;
            parser.end();
            (false, false)
        }
    };

    Ok(parsed_exr)
}

pub(crate) fn parse_assignment(parser: &mut Parser) -> ParseResult<()> {
    let stack_idx = parser.begin(NodeKind::Assignment);

    let allow_dotted_path = false;
    let tokens = &[TokenKind::KwLet, TokenKind::KwComptime];
    if parser.consume_first_of(tokens).is_err() || parse_pattern(parser, allow_dotted_path).is_err()
    {
        parser.recover(stack_idx, [TokenKind::Assign])?;
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

    let stack_idx = parser.begin(NodeKind::Update);

    let allow_dotted_path = true;
    if parse_pattern(parser, allow_dotted_path).is_err()
        || parser.consume_first_of(&ASSIGN_OPS).is_err()
    {
        parser.recover(stack_idx, ASSIGN_OPS)?;
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

    parser.begin(NodeKind::FunctionPatternParams);
    parse_comma_separated(
        parser,
        |parser| parse_pattern(parser, true),
        [TokenKind::ParenthesisClose],
    )?;
    parser.end();

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

    let mut invalid_path_span: Option<LocalSpan> = None;
    while parser.current_stripped().kind == TokenKind::Dot {
        if !allow_dotted_path && invalid_path_span.is_none() {
            invalid_path_span = Some(parser.current.span);
        }

        parser.bump();
        let token = parser.consume(TokenKind::Ident)?;
        invalid_path_span = invalid_path_span.map(|span| LocalSpan(span.until(token.span.0)));
    }

    if let Some(span) = invalid_path_span {
        parser.st.errors.push(ParseErrorKind::UnexpectedPath(span));
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
    let stack_idx = parser.begin(NodeKind::Branch);

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
        parser.recover_with(stack_idx, [TokenKind::BraceOpen], options)?;
    }

    parse_block(parser, true)?;

    if parser.current_stripped().kind == TokenKind::KwElse {
        parser.begin(NodeKind::BranchElse);

        parser.consume(TokenKind::KwElse)?;

        match parser.current_stripped().kind {
            TokenKind::KwIf => parse_branch(parser),
            TokenKind::KwComptime if parser.nth_next(2).kind == TokenKind::KwIf => {
                parse_branch(parser)
            }
            TokenKind::BraceOpen => parse_block(parser, true),
            _ => {
                parser.st.errors.push(ParseErrorKind::UnexpectedToken {
                    got: parser.current,
                    expected: vec![
                        TokenKind::KwIf.into(),
                        TokenKind::KwComptime.into(),
                        TokenKind::BraceOpen.into(),
                    ],
                });
                return Err(());
            }
        }?;

        parser.end();
    }

    parser.end();
    Ok(())
}

pub(crate) fn parse_loop(parser: &mut Parser) -> ParseResult<()> {
    let stack_idx = parser.begin(NodeKind::InfLoop);

    parser.consume(TokenKind::KwLoop)?;

    if parser.current_stripped().kind != TokenKind::BraceOpen {
        let options = RecoveryOptions {
            consume_safety_token: false,
            create_error: true,
            ..Default::default()
        };
        parser.recover_with(stack_idx, [TokenKind::BraceOpen], options)?;
    }
    parse_block(parser, false)?;

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
    parse_block(parser, false)?;

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
        Ok(None) => Ok(()),
        Ok(Some(error)) => {
            parser.st.errors.push(error);
            Err(())
        }
        Err(()) => Err(()),
    }
}

pub(crate) fn parse_expr_maybe(
    parser: &mut Parser,
    min_precedence: u8,
    config: ExpressionConfig,
) -> ParseResult<Option<ParseErrorKind>> {
    parser.begin_with(NodeKind::InfixOp, 1);
    parser.begin_with(NodeKind::InfixOp, 1);

    if let Some(error) = parse_value_maybe(parser, config)? {
        parser.stack.pop().unwrap();
        parser.stack.pop().unwrap();
        return Ok(Some(error));
    }

    parse_postfix_maybe(parser, min_precedence, config)?;
    parser.end();
    parse_expr_inner(parser, min_precedence, config)?;
    Ok(None)
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
                    parser.st.errors.push(ParseErrorKind::UnexpectedToken {
                        got: parser.current,
                        expected: vec![TokenKind::Ident.into()],
                    });
                }
            }

            parse_expr(parser, precedence + 1, config)?;
            parse_postfix_maybe(parser, min_precedence, config)?;

            // If a loop completes, the generated expression has to be turned into a single node
            parser.end_to_new(NodeKind::InfixOp, 1);
        } else {
            break;
        }
    }

    parser.end();
    Ok(())
}

fn parse_value_maybe(
    parser: &mut Parser,
    config: ExpressionConfig,
) -> ParseResult<Option<ParseErrorKind>> {
    create_neg_int_literal_maybe(parser);

    if parse_prefix_maybe(parser, config)? {
        return Ok(None);
    };

    parser.begin(NodeKind::Value);

    let next = parser.nth_non_whitespace(&mut 1).kind;
    match parser.current.kind {
        TokenKind::Int
        | TokenKind::String
        | TokenKind::BoolTrue
        | TokenKind::BoolFalse
        | TokenKind::Ident => {
            parser.bump();
            Ok(())
        }
        TokenKind::FormatString => {
            parse_format_string(parser);
            Ok(())
        }
        TokenKind::BracketOpen | TokenKind::KwFunction => parse_function(parser, true),
        TokenKind::KwComptime if next == TokenKind::KwFunction => parse_function(parser, true),
        TokenKind::ParenthesisOpen => parse_parenthesis_or_tuple(parser),
        TokenKind::BraceOpen if config.allow_complex => parse_block(parser, true),
        TokenKind::KwIf => parse_branch(parser),
        TokenKind::KwComptime if next == TokenKind::KwIf => parse_branch(parser),
        TokenKind::KwLoop => parse_loop(parser),
        TokenKind::KwWhile => parse_while(parser),
        kind if kind.control_flow_operator().is_some() => parse_control_flow(parser),
        _ => {
            parser.stack.pop().unwrap();
            return Ok(Some(ParseErrorKind::UnexpectedToken {
                got: parser.current,
                expected: vec![ExpectedItem::Value],
            }));
        }
    }?;

    parser.end();
    Ok(None)
}

fn parse_format_string(parser: &mut Parser) {
    let current = parser.current;
    let mut fmt_string_parser = FormatStringParser::new(parser, current.span);
    format_string_parser::parse_format_string(&mut fmt_string_parser);

    // Ignore the actual format string token
    parser.skip();
}

/// This function implements a hack that combines a minus
/// token with an int token.
/// This is not done in general, because this behavior should not be exhibited in
/// all places. E.g. `a-2` should not parse a negative number literal.
fn create_neg_int_literal_maybe(parser: &mut Parser) {
    parser.consume_whitespace();

    let first = parser.current;
    let second = parser.nth_next(1);

    if first.kind == TokenKind::OpMinus && second.kind == TokenKind::Int {
        parser.skip();
        assert_eq!(parser.current.kind, TokenKind::Int);
        let new_span = LocalSpan(first.span.until(second.span.0));
        parser.current.span = new_span;
    }
}

fn parse_struct_literal(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::StructLiteral);

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
        Ok(_) => {}
        Err(()) => return Err(()),
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
    parser.bump();

    parse_expr(parser, prefix_op.precedence() + 1, config)?;

    parser.end();
    Ok(())
}

pub(crate) fn parse_postfix_maybe(
    parser: &mut Parser,
    min_precedence: u8,
    config: ExpressionConfig,
) -> ParseResult<()> {
    while let Some(postfix) = parser.current_stripped().kind.postfix_operator() {
        if postfix.precedence() >= min_precedence {
            match postfix {
                PostfixOperator::Call => {
                    parser.end_to_new(NodeKind::PostfixOp, 1);
                    parse_param_list(parser, config)?;
                }
                PostfixOperator::StructLiteral if config.allow_complex => {
                    parser.end_to_new(NodeKind::PostfixOp, 1);
                    parse_struct_literal(parser)?;
                }
                PostfixOperator::StructLiteral => break,
            }
        } else {
            break;
        }
    }

    Ok(())
}

pub(crate) fn parse_param_list(parser: &mut Parser, config: ExpressionConfig) -> ParseResult<()> {
    parser.begin(NodeKind::ParamList);

    parser.consume(TokenKind::ParenthesisOpen)?;
    parse_comma_separated(
        parser,
        |parser| parse_expr(parser, 0, Default::default()),
        [TokenKind::ParenthesisClose],
    )?;

    match parser.current_stripped().kind {
        TokenKind::BraceOpen if config.allow_complex => {
            parse_block(parser, true)?;
        }
        _ => {}
    }

    parser.end();
    Ok(())
}

pub(crate) fn parse_control_flow(parser: &mut Parser) -> ParseResult<()> {
    parser.begin(NodeKind::ControlFlow);

    parser
        .consume_first_with(|kind| kind.control_flow_operator().is_some())
        .ok_or_else(|| {
            parser.st.errors.push(ParseErrorKind::UnexpectedToken {
                got: parser.current,
                expected: vec![ExpectedItem::ControlFlowOperator],
            });
        })?;
    parse_expr_maybe(parser, 0, Default::default())?;

    parser.end();
    Ok(())
}
