use std::fmt::{Debug, Display};

#[derive(Eq, Debug, PartialEq, Clone, Hash)]
pub enum Type {
    Bool,
    Fixed,
    Function,
    StaticInt,
    DynamicInt,
    String,
    Type,
    // The template for a type, basically the parent class
    Template(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => f.write_fmt(format_args!("Bool")),
            Type::Fixed => f.write_fmt(format_args!("Fixed")),
            Type::Function => f.write_fmt(format_args!("Function")),
            Type::StaticInt => f.write_fmt(format_args!("StaticInt")),
            Type::DynamicInt => f.write_fmt(format_args!("DynamicInt")),
            Type::String => f.write_fmt(format_args!("String")),
            Type::Type => f.write_fmt(format_args!("Type")),
            Type::Template(inner) => f.write_str(&format!("Template<{:?}>", inner)),
        }
    }
}
