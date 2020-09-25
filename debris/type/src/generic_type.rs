use std::fmt::{Debug, Display};

#[derive(Eq, Debug, PartialEq, Clone, Hash)]
pub enum Type {
    Int,
    Fixed,
    Bool,
    Function,
    Type,
    // The template for a type, basically the parent clas
    Template(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => f.write_fmt(format_args!("Int")),
            Type::Fixed => f.write_fmt(format_args!("Fixed")),
            Type::Bool => f.write_fmt(format_args!("Bool")),
            Type::Function => f.write_fmt(format_args!("Function")),
            Type::Type => f.write_fmt(format_args!("Type")),
            Type::Template(inner) => f.write_str(&format!("Template<{:?}>", inner)),
        }
    }
}
