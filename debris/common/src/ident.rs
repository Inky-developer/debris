use smol_str::SmolStr;
use std::{
    convert::AsRef,
    fmt::Display,
    fmt::{self, Formatter},
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ident {
    Value(SmolStr),
    Special(SpecialIdent),
}

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
    Clone,
}

impl Ident {
    pub fn new<T: AsRef<str>>(value: T) -> Self {
        Ident::Value(SmolStr::new(value))
    }

    pub fn to_string(&self) -> String {
        match self {
            Ident::Value(string) => string.to_string(),
            Ident::Special(special) => special.to_string(),
        }
    }
}

impl From<SpecialIdent> for Ident {
    fn from(special: SpecialIdent) -> Self {
        Ident::Special(special)
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
            Ident::Special(special) => special.fmt(f),
            Ident::Value(val) => f.write_str(&val),
        }
    }
}
