/// The type of a class object
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum Type {
    /// Type which matches with every other type
    /// Objects of type any should not exist.
    Any,
    /// The null type, implicitly returned by functions
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
    /// A reference to a compiled function
    /// A compiled function is uniquely identified by its block id
    /// Basically a function after monomorphization
    FunctionRef,
    /// The type of a type
    Type,
    /// Module type
    Module,
    /// Type of a struct definition
    Struct,
    /// An instantiated struct
    StructObject,
    /// Type of a tuple
    /// TODO: Could a [`Type::Tuple`] not just be a [`Type::TupleObject`] with types as elements?
    Tuple,
    /// An instantiated tuple
    TupleObject,
}

impl Type {
    pub fn is_int(&self) -> bool {
        matches!(self, Type::DynamicInt | Type::ComptimeInt)
    }

    pub fn is_never(&self) -> bool {
        matches!(self, Type::Never)
    }

    pub fn diverges(&self) -> bool {
        self.is_never()
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

    /// Returns whether `other` matches the pattern of `self`
    #[allow(clippy::match_same_arms)]
    pub fn matches(&self, other: Type) -> bool {
        match (self, other) {
            // The never type matches always
            (Type::Never, _) | (_, Type::Never) => true,
            // `Any` matches every other type only as a pattern
            (Type::Any, _) => true,
            (a, b) => *a == b,
        }
    }

    /// Returns whether this type should be treated as a reference.
    /// This effects e.g. whether variables get copied when assigned or passed to functions
    /// TODO: implement some proper reference object, to make this less implicit and confusing.
    /// Right now, only structs are treated as references
    pub fn is_reference(&self) -> bool {
        matches!(self, Type::StructObject)
    }
}

/// Error messages can safely use the debug impl for display
impl std::fmt::Display for Type {
    #[allow(clippy::use_debug)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
