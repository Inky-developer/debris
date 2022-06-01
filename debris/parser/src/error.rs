//! This file contains the error type that is used for diagnostics

use crate::{
    token::{Token, TokenKind},
    LocalSpan,
};

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    LeftOverInput(Token),
    UnexpectedComma(Token),
    UnexpectedFunctionIdent(Token),
    UnexpectedPath(LocalSpan),
    UnexpectedToken {
        got: Token,
        expected: Vec<ExpectedItem>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedItem {
    ControlFlowOperator,
    FormatString,
    TokenKind(TokenKind),
    Statement,
    Value,
}

impl From<TokenKind> for ExpectedItem {
    fn from(kind: TokenKind) -> Self {
        Self::TokenKind(kind)
    }
}
