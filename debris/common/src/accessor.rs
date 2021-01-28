use std::fmt::Debug;

use super::Ident;

/// An accessor represents a dotted path
///
/// Probably it would be a good idea to intern accessors, like idents
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Accessor {
    /// A path like a.b.c
    Path(Vec<Ident>),
}

impl Accessor {
    pub fn equals_ident(&self, value: impl AsRef<str>) -> bool {
        match self {
            Accessor::Path(path) => match path.as_slice() {
                [ident] => match ident {
                    Ident::Value(name) => name == value.as_ref(),
                    Ident::Special(_) => false,
                },
                _ => false,
            },
        }
    }
}

impl From<Ident> for Accessor {
    fn from(other: Ident) -> Self {
        Accessor::Path(vec![other])
    }
}

impl Debug for Accessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Accessor::Path(path) => f.write_fmt(format_args!(
                "Path({})",
                path.iter()
                    .map(|ident| match ident {
                        Ident::Value(val) => format!("{}", val),
                        Ident::Special(special) => format!("{:?}", special),
                    })
                    .collect::<Vec<String>>()
                    .join(".")
            )),
        }
    }
}
