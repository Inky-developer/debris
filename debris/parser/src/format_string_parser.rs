use debris_common::Span;
use logos::{Lexer, Logos};

use crate::{
    error::{ExpectedItem, ParseErrorKind},
    node::NodeKind,
    parser::Parser,
    LocalSpan,
};

#[derive(Debug, Eq, PartialEq, Clone, Copy, logos::Logos)]
enum TokenKind {
    #[token(".")]
    Dot,

    #[token("$")]
    Dollar,

    #[regex(r#"\\[`\$]"#)]
    EscapedChar,

    EndOfInput,

    #[regex(r#"[^$a-zA-Z_\\`]+"#)]
    String,

    #[token("`")]
    Tick,

    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,

    Error,
}

impl TryFrom<TokenKind> for crate::token::TokenKind {
    type Error = ();
    fn try_from(token: TokenKind) -> Result<Self, ()> {
        use TokenKind::*;
        let value = match token {
            Dot => crate::token::TokenKind::Dot,
            Dollar => crate::token::TokenKind::Dollar,
            String => crate::token::TokenKind::StringInner,
            Tick => crate::token::TokenKind::Tick,
            Ident => crate::token::TokenKind::Ident,
            EscapedChar => crate::token::TokenKind::EscapedChar,
            EndOfInput => return Err(()),
            Error => crate::token::TokenKind::Error,
        };
        Ok(value)
    }
}

#[derive(Debug, Clone, Copy)]
struct Token {
    kind: TokenKind,
    span: LocalSpan,
}

impl TryFrom<Token> for crate::token::Token {
    type Error = ();
    fn try_from(value: Token) -> Result<Self, ()> {
        Ok(crate::token::Token {
            kind: value.kind.try_into()?,
            span: value.span,
        })
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
        let kind = match self.tokens.next() {
            Some(Ok(kind)) => kind,
            Some(Err(())) => TokenKind::Error,
            None => TokenKind::EndOfInput,
        };
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
        if let Ok(crate_token) = crate::token::Token::try_from(token) {
            self.parser.insert(crate_token);
        }
    }
}

pub fn parse_format_string(parser: &mut FormatStringParser) {
    let idx = parser.parser.begin(NodeKind::FormatString);

    if let Err(token) = parse_format_string_inner(parser) {
        while parser.parser.stack.len() - 1 > idx {
            parser.parser.end();
        }
        parser
            .parser
            .st
            .errors
            .push(ParseErrorKind::UnexpectedToken {
                got: token.try_into().unwrap_or(crate::token::Token {
                    kind: crate::token::TokenKind::EndOfInput,
                    span: token.span,
                }),
                expected: vec![ExpectedItem::FormatString],
            });
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
    let mut string_span = current.span;
    let flush = |parser: &mut FormatStringParser, span: LocalSpan, current: LocalSpan| {
        if current != span {
            parser.insert(Token {
                kind: TokenKind::String,
                span,
            });
        }
    };
    loop {
        match current.kind {
            TokenKind::String | TokenKind::Ident | TokenKind::Dot => {}
            TokenKind::EscapedChar => {
                flush(parser, string_span, current.span);
                parser.insert(current);
                current = parser.next();
                string_span = current.span;
                continue;
            }
            TokenKind::Dollar => {
                flush(parser, string_span, current.span);
                current = parse_path(parser, current)?;
                string_span = current.span;
                continue;
            }
            TokenKind::EndOfInput | TokenKind::Error => {
                flush(parser, string_span, current.span);
                return Err(current);
            }
            TokenKind::Tick => break,
        }
        string_span = LocalSpan(string_span.until(current.span.0));
        current = parser.next();
    }
    flush(parser, string_span, current.span);
    parser.insert(current);

    let next = parser.next();
    if next.kind != TokenKind::EndOfInput {
        return Err(next);
    }

    Ok(())
}

fn parse_path(parser: &mut FormatStringParser, current: Token) -> Result<Token, Token> {
    parser.parser.begin(NodeKind::Path);
    parser.insert(current);
    parser.expect(TokenKind::Ident)?;
    let token = loop {
        let current = parser.next();
        match current.kind {
            TokenKind::Dot => {
                parser.insert(current);
                parser.expect(TokenKind::Ident)?;
            }
            _ => break current,
        }
    };
    parser.parser.end();

    Ok(token)
}
