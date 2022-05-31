use core::fmt;

use crate::LocalSpan;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Token {
    pub span: LocalSpan,
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
    AssignPlus,

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

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token("[")]
    BracketOpen,

    #[token("]")]
    BracketClose,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[regex("#[^\n]*")]
    Comment,

    #[token(".")]
    Dot,

    /// Manually created in format strings
    /// Always consists of two characters, where only the second character should be used.
    EscapedChar,

    #[regex(r"`(:?[^`]|\\`)*`")]
    FormatString,

    /// Manually created in format strings
    /// Always starts with a '$'
    FormatStringVariable,

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,

    #[regex(r"\d+")]
    Int,

    #[token("break")]
    KwBreak,

    #[token("comptime")]
    KwComptime,

    #[token("continue")]
    KwContinue,

    #[token("else")]
    KwElse,

    #[token("fn")]
    KwFunction,

    #[token("if")]
    KwIf,

    #[token("import")]
    KwImport,

    #[token("let")]
    KwLet,

    #[token("loop")]
    KwLoop,

    #[token("mod")]
    KwMod,

    #[token("struct")]
    KwStruct,

    #[token("return")]
    KwReturn,

    #[token("while")]
    KwWhile,

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

    #[token("==")]
    OpEqual,

    #[token("!=")]
    OpNotEqual,

    #[token(">=")]
    OpGreaterOrEqual,

    #[token(">")]
    OpGreater,

    #[token("<=")]
    OpLessOrEqual,

    #[token("<")]
    OpLess,

    #[token("and")]
    OpAnd,

    #[token("or")]
    OpOr,

    #[token("not")]
    OpNot,

    #[token("(")]
    ParenthesisOpen,

    #[token(")")]
    ParenthesisClose,

    #[token(";")]
    Semicolon,

    #[regex(r#""(:?[^"]|\\")*""#)]
    String,

    /// Created manually
    StringInner,

    #[token("->")]
    ThinArrow,

    /// Created manually
    Tick,

    #[regex("[ \n\r]+")]
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
    /// Tokens that have no semantic meaning attached are considered whitespace
    pub fn is_whitespace(self) -> bool {
        matches!(self, TokenKind::Whitespace | TokenKind::Comment)
    }

    pub fn assign_operator(self) -> Option<AssignOperator> {
        let operator = match self {
            TokenKind::Assign => AssignOperator::Assign,
            TokenKind::AssignPlus => AssignOperator::AssignPlus,
            TokenKind::AssignMinus => AssignOperator::AssignMinus,
            TokenKind::AssignTimes => AssignOperator::AssignTimes,
            TokenKind::AssignDivide => AssignOperator::AssignDivide,
            TokenKind::AssignModulo => AssignOperator::AssignModulo,
            _ => return None,
        };
        Some(operator)
    }

    pub fn postfix_operator(self) -> Option<PostfixOperator> {
        let operator = match self {
            TokenKind::ParenthesisOpen => PostfixOperator::Call,
            TokenKind::BraceOpen => PostfixOperator::StructLiteral,
            _ => return None,
        };
        Some(operator)
    }

    pub fn prefix_operator(self) -> Option<PrefixOperator> {
        let operator = match self {
            TokenKind::OpMinus => PrefixOperator::Minus,
            TokenKind::OpNot => PrefixOperator::Not,
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
            TokenKind::OpEqual => InfixOperator::Equal,
            TokenKind::OpNotEqual => InfixOperator::NotEqual,
            TokenKind::OpGreaterOrEqual => InfixOperator::GreaterOrEqual,
            TokenKind::OpGreater => InfixOperator::Greater,
            TokenKind::OpLessOrEqual => InfixOperator::LessOrEqual,
            TokenKind::OpLess => InfixOperator::Less,
            TokenKind::OpAnd => InfixOperator::And,
            TokenKind::OpOr => InfixOperator::Or,
            _ => return None,
        };

        Some(operator)
    }

    pub fn control_flow_operator(self) -> Option<ControlFlowOperator> {
        let control_flow = match self {
            TokenKind::KwBreak => ControlFlowOperator::Break,
            TokenKind::KwContinue => ControlFlowOperator::Continue,
            TokenKind::KwReturn => ControlFlowOperator::Return,
            _ => return None,
        };
        Some(control_flow)
    }
}

pub enum AssignOperator {
    Assign,
    AssignPlus,
    AssignMinus,
    AssignTimes,
    AssignDivide,
    AssignModulo,
}

/// Postfix operator with infinite precedence
pub enum PostfixOperator {
    Call,
    StructLiteral,
}

impl PostfixOperator {
    pub fn precedence(&self) -> u8 {
        match self {
            PostfixOperator::Call | PostfixOperator::StructLiteral => 7,
        }
    }
}

pub enum PrefixOperator {
    Minus,
    Not,
}

impl PrefixOperator {
    pub fn precedence(&self) -> u8 {
        match self {
            PrefixOperator::Minus | PrefixOperator::Not => 6,
        }
    }
}

pub enum InfixOperator {
    Dot,
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    GreaterOrEqual,
    Greater,
    LessOrEqual,
    Less,
    And,
    Or,
}

impl InfixOperator {
    /// Returns the precedence of this [`InfixOperator`].
    /// The higher the precedence the tighter the operator binds.
    #[allow(clippy::match_same_arms)]
    pub fn precedence(&self) -> u8 {
        match self {
            InfixOperator::Or => 1,
            InfixOperator::And => 2,
            InfixOperator::Equal => 3,
            InfixOperator::NotEqual => 3,
            InfixOperator::Greater => 3,
            InfixOperator::GreaterOrEqual => 3,
            InfixOperator::Less => 3,
            InfixOperator::LessOrEqual => 3,
            InfixOperator::Minus => 4,
            InfixOperator::Plus => 4,
            InfixOperator::Divide => 5,
            InfixOperator::Modulo => 5,
            InfixOperator::Times => 5,
            InfixOperator::Dot => 8,
        }
    }
}

pub enum ControlFlowOperator {
    Break,
    Continue,
    Return,
}
