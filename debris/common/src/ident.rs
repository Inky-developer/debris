use smol_str::SmolStr;
use std::{
    convert::AsRef,
    fmt::Display,
    fmt::{self, Formatter},
};

/// identifier for a value
///
/// The special variant is used to access special variants of types, like binary operation functions.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ident {
    /// Since Idents have be cloned quite often a generic value is
    /// represented by a `SmolString` which is stack-allocated for the
    /// majority of practical values
    Value(SmolStr),
    Index(usize),
    Special(SpecialIdent),
}

/// Every special identifier
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum SpecialIdent {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    UnaryMinus,
    And,
    Or,
    Not,
    CmpEq,
    CmpNe,
    CmpGt,
    CmpGe,
    CmpLt,
    CmpLe,
    Dot,
    Promote,
}

impl Ident {
    /// Creates a new identifier
    pub fn new<T: AsRef<str>>(value: T) -> Self {
        Ident::Value(SmolStr::new(value))
    }
}

impl From<SpecialIdent> for Ident {
    fn from(special: SpecialIdent) -> Self {
        Ident::Special(special)
    }
}

impl<T> From<T> for Ident
where
    T: Into<SmolStr>,
{
    fn from(value: T) -> Self {
        Ident::Value(value.into())
    }
}

impl Display for SpecialIdent {
    // Debug is good enough for now
    #[allow(clippy::use_debug)]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ident::Value(string) => write!(f, "{string}"),
            Ident::Index(idx) => write!(f, "{idx}"),
            Ident::Special(special) => write!(f, "{special}"),
        }
    }
}

impl PartialEq<&str> for Ident {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Ident::Index(_) | Ident::Special(_) => false,
            Ident::Value(val) => val == other,
        }
    }
}

impl PartialEq<Ident> for &str {
    fn eq(&self, other: &Ident) -> bool {
        other == self
    }
}
