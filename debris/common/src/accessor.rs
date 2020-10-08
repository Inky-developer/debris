use std::fmt::Debug;

use super::Ident;

#[macro_export]
macro_rules! accessor {
    ( $( $x:expr ),+ ) => {
        Accessor::Path(vec![$($x,)*])
    };
}

/// An accessor. Currently a mess
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Accessor {
    /// A path like a.b.c
    Path(Vec<Ident>),
    /// An accessor that is generated and used internally, which only has an id
    Internal(u64),
    /// a special accessor for the main resource of the current context
    Main,
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
            Accessor::Main => f.write_str("Main"),
            Accessor::Internal(uid) => f.write_fmt(format_args!("Internal({})", uid)),
        }
    }
}
