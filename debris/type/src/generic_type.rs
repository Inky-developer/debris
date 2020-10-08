use std::fmt::{Debug, Display};

#[derive(Eq, Debug, PartialEq, Clone, Hash)]
pub enum Type {
    Bool,
    Fixed,
    Function,
    StaticInt,
    DynamicInt,
    Module,
    String,
    Type,
    // The template for a type, basically the parent class
    Template(Box<Type>),
}

impl Type {
    /// Returns whether the other type can match on this one
    /// Generally, this returns true if the other type is the same as this type
    /// I plan to extend this api to support for example that a StaticInt matches on a general Integer
    pub fn matches(&self, other: &Type) -> bool {
        self == other
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => f.write_fmt(format_args!("Bool")),
            Type::Fixed => f.write_fmt(format_args!("Fixed")),
            Type::Function => f.write_fmt(format_args!("Function")),
            Type::StaticInt => f.write_fmt(format_args!("StaticInt")),
            Type::DynamicInt => f.write_fmt(format_args!("DynamicInt")),
            Type::Module => f.write_fmt(format_args!("Module")),
            Type::String => f.write_fmt(format_args!("String")),
            Type::Type => f.write_fmt(format_args!("Type")),
            Type::Template(inner) => f.write_str(&format!("Template<{:?}>", inner)),
        }
    }
}
