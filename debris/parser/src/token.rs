use core::fmt;

use debris_common::Span;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let start = self.span.start();
        let end = self.span.end();
        write!(f, "{}@{start}..{end}", self.kind)
    }
}

#[derive(logos::Logos, Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    #[token("=")]
    Assign,

    #[token("+=")]
    AssignAdd,

    #[token("-=")]
    AssignMinus,

    #[token("*=")]
    AssignTimes,

    #[token("/=")]
    AssignDivide,

    #[token("%=")]
    AssignModulo,

    #[token("true")]
    BoolTrue,

    #[token("false")]
    BoolFalse,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

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

impl fmt::Display for TokenKind {
    #[allow(clippy::use_debug)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl TokenKind {
    pub fn postfix_operator(self) -> Option<PostfixOperator> {
        let operator = match self {
            TokenKind::ParenthesisOpen => PostfixOperator::Call,
            _ => return None,
        };
        Some(operator)
    }

    pub fn prefix_operator(self) -> Option<PrefixOperator> {
        let operator = match self {
            TokenKind::OpMinus => PrefixOperator::Minus,
            _ => return None,
        };
        Some(operator)
    }

    pub fn infix_operator(self) -> Option<InfixOperator> {
        let operator = match self {
            TokenKind::Dot => InfixOperator::Dot,
            TokenKind::OpPlus => InfixOperator::Plus,
            TokenKind::OpMinus => InfixOperator::Minus,
            TokenKind::OpTimes => InfixOperator::Times,
            TokenKind::OpDivide => InfixOperator::Divide,
            TokenKind::OpModulo => InfixOperator::Modulo,
            _ => return None,
        };

        Some(operator)
    }
}

/// Postfix operator with infinite precedence
pub enum PostfixOperator {
    Call,
}

pub enum PrefixOperator {
    Minus,
}

pub enum InfixOperator {
    Dot,
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
}

impl InfixOperator {
    /// Returns the precedence of this [`InfixOperator`].
    /// The higher the precedence the tighter the operator binds.
    #[allow(clippy::match_same_arms)]
    pub fn precedence(&self) -> u8 {
        match self {
            InfixOperator::Minus => 1,
            InfixOperator::Plus => 1,
            InfixOperator::Divide => 2,
            InfixOperator::Modulo => 2,
            InfixOperator::Times => 2,
            InfixOperator::Dot => 3,
        }
    }
}
