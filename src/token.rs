use core::fmt;

use crate::span::Span;

#[derive(Eq, PartialEq, Clone, Copy)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let start = self.span.start;
        let end = self.span.end();
        write!(f, "{:?}@{start}..{end}", self.kind)
    }
}

#[derive(logos::Logos, Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    #[token("=")]
    Assign,

    #[token(",")]
    Comma,

    #[regex(r"`(:?[^`]|\\`)*`")]
    FormatString,

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,

    #[regex(r"\d+")]
    Int,

    #[token("let")]
    Let,

    #[token("+")]
    OpPlus,

    #[token("-")]
    OpMinus,

    #[token("*")]
    OpTimes,

    #[token("/")]
    OpDivide,

    #[token("%")]
    OpModulo,

    #[token("(")]
    ParenthesisOpen,

    #[token(")")]
    ParenthesisClose,

    #[token(";")]
    Semicolon,

    #[regex(r#""(:?[^"]|\\")*""#)]
    String,

    #[regex("[ \n]+")]
    Whitespace,

    #[error]
    Error,

    EndOfInput,

    UnexpectedToken,
}

impl TokenKind {
    pub fn operator(self) -> Option<Operator> {
        let operator = match self {
            TokenKind::OpPlus => Operator::Plus,
            TokenKind::OpMinus => Operator::Minus,
            TokenKind::OpTimes => Operator::Times,
            TokenKind::OpDivide => Operator::Divide,
            TokenKind::OpModulo => Operator::Modulo,
            _ => return None,
        };

        Some(operator)
    }
}

pub enum Operator {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
}

impl Operator {
    /// Returns the precedence of this [`Operator`].
    /// The higher the precedence the tighter the operator binds.
    pub fn precedence(&self) -> u8 {
        match self {
            Operator::Plus => 1,
            Operator::Minus => 1,
            Operator::Times => 2,
            Operator::Divide => 2,
            Operator::Modulo => 2,
        }
    }
}
