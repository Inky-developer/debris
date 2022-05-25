use debris_common::Span;
use logos::{Lexer, Logos};

use crate::{node::NodeKind, parser::Parser, LocalSpan};

#[derive(Debug, Eq, PartialEq, Clone, Copy, logos::Logos)]
enum TokenKind {
    #[regex(r#"[^$\\`]+"#)]
    String,

    #[token("`")]
    Tick,

    #[regex("\\$[a-zA-Z_][a-zA-Z_0-9]*")]
    Variable,

    #[regex(r#"\\[`\$]"#)]
    EscapedChar,

    EndOfInput,

    #[error]
    Error,
}

impl From<TokenKind> for crate::token::TokenKind {
    fn from(token: TokenKind) -> Self {
        use TokenKind::*;
        match token {
            String => crate::token::TokenKind::StringInner,
            Tick => crate::token::TokenKind::Tick,
            Variable => crate::token::TokenKind::FormatStringVariable,
            EscapedChar => crate::token::TokenKind::EscapedChar,
            EndOfInput => {
                unreachable!(
                    "Should not convert end of input of format string to file end of input"
                )
            }
            Error => crate::token::TokenKind::Error,
        }
    }
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    span: LocalSpan,
}

impl From<Token> for crate::token::Token {
    fn from(value: Token) -> Self {
        crate::token::Token {
            kind: value.kind.into(),
            span: value.span,
        }
    }
}

pub struct FormatStringParser<'a, 'b> {
    tokens: Lexer<'b, TokenKind>,
    offset: usize,
    parser: &'b mut Parser<'a>,
}

impl<'a, 'b> FormatStringParser<'a, 'b> {
    pub fn new(parser: &'b mut Parser<'a>, span: LocalSpan) -> Self {
        let input = &parser.input[span.as_slice()];
        let tokens = TokenKind::lexer(input);
        Self {
            offset: span.start(),
            parser,
            tokens,
        }
    }

    fn next(&mut self) -> Token {
        let kind = self.tokens.next().unwrap_or(TokenKind::EndOfInput);
        let span = LocalSpan(self.offset + Span::from(self.tokens.span()));
        Token { kind, span }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), Token> {
        let current = self.next();
        if current.kind == expected {
            self.insert(current);
            Ok(())
        } else {
            Err(current)
        }
    }

    fn insert(&mut self, token: Token) {
        let crate_token = crate::token::Token::from(token);
        self.parser.insert(crate_token);
    }
}

pub fn parse_format_string(parser: &mut FormatStringParser) {
    parser.parser.begin(NodeKind::FormatString);

    if let Err(token) = parse_format_string_inner(parser) {
        parser.parser.st.errors.push(());
        parser.parser.begin(NodeKind::Error);

        parser.insert(token);
        loop {
            let next = parser.next();
            if next.kind == TokenKind::EndOfInput {
                break;
            }
            parser.insert(next);
        }

        parser.parser.end();
    }

    parser.parser.end();
}

fn parse_format_string_inner(parser: &mut FormatStringParser) -> Result<(), Token> {
    parser.expect(TokenKind::Tick)?;

    let mut current = parser.next();
    loop {
        match current.kind {
            TokenKind::EscapedChar | TokenKind::String | TokenKind::Variable => {
                parser.insert(current);
            }
            TokenKind::EndOfInput | TokenKind::Error => return Err(current),
            TokenKind::Tick => break,
        }
        current = parser.next();
    }

    parser.insert(current);

    let next = parser.next();
    if next.kind != TokenKind::EndOfInput {
        return Err(next);
    }

    Ok(())
}
