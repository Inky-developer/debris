use std::str::FromStr;

use crate::{
    objects::obj_class::{GenericClass, GenericClassRef},
    CompileContext,
};

/// The enumeration of patterns allowed as function arguments
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypePattern {
    /// The Any pattern matches every type
    Any,
    /// The Int pattern matches every integer
    Int,
    /// The Boolean pattern matches every boolean
    Bool,
    /// A type pattern can also take any normal type
    Class(GenericClassRef),
}

impl TypePattern {
    /// Returns whether the type matches on this pattern
    pub fn matches(&self, class: &GenericClass) -> bool {
        match self {
            TypePattern::Any => true,
            TypePattern::Int => class.typ().is_int(),
            TypePattern::Bool => class.typ().is_bool(),
            TypePattern::Class(other_class) => other_class.matches(class),
        }
    }

    pub fn from_str(s: &str, ctx: &CompileContext) -> Option<Self> {
        match s {
            "Any" => Some(TypePattern::Any),
            "Int" => Some(TypePattern::Int),
            "Bool" => Some(TypePattern::Bool),
            other => Some(TypePattern::Class(
                GenericClass::new(&ctx.type_ctx().from_type(Type::from_str(other).ok()?))
                    .into_class_ref(),
            )),
        }
    }
}

impl From<GenericClassRef> for TypePattern {
    fn from(cls: GenericClassRef) -> Self {
        TypePattern::Class(cls)
    }
}

impl std::fmt::Display for TypePattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypePattern::Any => f.write_str("{Any}"),
            TypePattern::Int => f.write_str("{Int}"),
            TypePattern::Bool => f.write_str("{Bool}"),
            TypePattern::Class(class) => f.write_str(&class.to_string()),
        }
    }
}

/// The type of a class object
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum Type {
    /// The null type, implicitely return by function
    /// which don't specify a return type, also the
    /// value that statements return (since everything
    /// is an expression)
    Null,
    /// Marks a value that cannot be constructed, for example
    /// The return value of `loop {}`
    Never,
    /// Compile time known 32-bit signed integer
    StaticInt,
    /// 32-bit signed integer known at runtime
    DynamicInt,
    /// A boolean value known at compile time
    StaticBool,
    /// Runtime boolean
    DynamicBool,
    /// A compile time known string
    String,
    /// A compile time known format string
    FormatString,
    /// Any function, native or api
    Function,
    /// The type of a class
    Class,
    /// Module type
    Module,
    /// Type of a struct definition
    Struct,
    /// An instantiated struct
    StructObject,
}

impl Type {
    pub fn is_int(&self) -> bool {
        matches!(self, Type::DynamicInt | Type::StaticInt)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::DynamicBool | Type::StaticBool)
    }

    pub fn is_never(&self) -> bool {
        matches!(self, Type::Never)
    }

    /// Returns whether this type can be completely encoded at runtime
    pub fn runtime_encodable(&self) -> bool {
        matches!(
            self,
            Type::DynamicBool | Type::DynamicInt | Type::Null | Type::Never | Type::StructObject
        )
    }

    /// Returns whether this type should be const.
    /// Const types are a bit more powerful, because the compiler
    /// can track these better. For example, functions are const
    /// which means the user won't be able to override them, but
    /// this allows the compiler to easily compile higher-order functions.
    pub fn should_be_const(&self) -> bool {
        matches!(
            self,
            Type::Class | Type::Function | Type::Module | Type::Struct | Type::StructObject
        )
    }
}

impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Type::*;
        match s {
            "Null" => Ok(Null),
            "StaticInt" => Ok(StaticInt),
            "DynamicInt" => Ok(DynamicInt),
            "StaticBool" => Ok(StaticBool),
            "DynamicBool" => Ok(DynamicBool),
            "String" => Ok(String),
            "FormatString" => Ok(FormatString),
            "Module" => Ok(Module),
            "Struct" => Ok(Struct),
            _ => Err(()),
        }
    }
}

/// Error messages can safely use the debug impl for display
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
