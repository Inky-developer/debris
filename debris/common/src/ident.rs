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
    Value(SmolStr),
    Special(SpecialIdent),
}

/// Every special identifier
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum SpecialIdent {
    Add,
    Sub,
    Mul,
    Div,
    Modu,
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
    Clone,
    PromoteRuntime,
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ident::Value(string) => f.write_str(&string),
            Ident::Special(special) => f.write_str(&special.to_string()),
        }
    }
}
