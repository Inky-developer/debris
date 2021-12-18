use super::class::{Class, ClassRef};

/// The enumeration of patterns allowed as function arguments
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypePattern {
    /// The Any pattern matches every type
    Any,
    /// A type pattern can also take any normal type
    Class(ClassRef),
}

impl TypePattern {
    /// Returns whether the type matches on this pattern
    pub fn matches(&self, class: &Class) -> bool {
        match self {
            TypePattern::Any => true,
            TypePattern::Class(other_class) => other_class.matches(class),
        }
    }

    #[track_caller]
    pub fn expect_class(&self, msg: &str) -> &ClassRef {
        match self {
            TypePattern::Any => panic!("{}", msg),
            TypePattern::Class(class) => class,
        }
    }
}

impl From<ClassRef> for TypePattern {
    fn from(cls: ClassRef) -> Self {
        TypePattern::Class(cls)
    }
}

impl std::fmt::Display for TypePattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypePattern::Any => f.write_str("{Any}"),
            TypePattern::Class(class) => f.write_str(&class.to_string()),
        }
    }
}

/// The type of a class object
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum Type {
    /// The null type, implicitly return by function
    /// which don't specify a return type, also the
    /// value that statements return (since everything
    /// is an expression)
    Null,
    /// Marks a value that cannot be constructed, for example
    /// The return value of `loop {}`
    Never,
    /// Compile time known 32-bit signed integer
    ComptimeInt,
    /// 32-bit signed integer known at runtime
    DynamicInt,
    /// A boolean value known at compile time
    ComptimeBool,
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
    /// Type of a tuple
    Tuple,
    /// An instantiated tuple
    TupleObject,
}

impl Type {
    pub fn is_int(&self) -> bool {
        matches!(self, Type::DynamicInt | Type::ComptimeInt)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::DynamicBool | Type::ComptimeBool)
    }

    pub fn is_never(&self) -> bool {
        matches!(self, Type::Never)
    }

    pub fn diverges(&self) -> bool {
        self.is_never()
    }

    pub fn is_valid_param(&self) -> bool {
        !matches!(self, Type::Module | Type::Struct | Type::Tuple)
    }

    /// Returns whether this type can be completely encoded at runtime
    pub fn runtime_encodable(&self) -> bool {
        matches!(
            self,
            Type::DynamicBool
                | Type::DynamicInt
                | Type::Null
                | Type::Never
                | Type::StructObject
                | Type::TupleObject
        )
    }

    /// Returns whether this type can be encoded at compile time.
    pub fn comptime_encodable(&self) -> bool {
        !self.runtime_encodable()
    }

    /// Returns whether this type should be const.
    /// Const types are a bit more powerful, because the compiler
    /// can track these better. For example, functions are const
    /// which means the user won't be able to override them, but
    /// this allows the compiler to easily compile higher-order functions.
    pub fn should_be_const(&self) -> bool {
        matches!(
            self,
            Type::Class | Type::Function | Type::Module | Type::Struct | Type::Tuple
        )
    }

    /// Returns whether `self` matches the pattern of `other`
    pub fn matches(&self, other: &Type) -> bool {
        match (self, other) {
            // The never type matches always
            (Type::Never, _) => true,
            (a, b) => a == b,
        }
    }
}

/// Error messages can safely use the debug impl for display
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
