use debris_common::{Ident, LocalSpan, SpecialIdent};

use super::{IdentifierPath, SpannedIdentifier};

/// A constant literal, already parsed
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirConstValue {
    Integer { span: LocalSpan, value: i32 },
    Fixed { span: LocalSpan, value: i32 },
    String { span: LocalSpan, value: String },
}

/// Any supported comparison operator
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirComparisonOperator {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

/// Any operator that can be used as an infix
///
/// That means that this operator has to be between to values
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirInfixOperator {
    /// Any comparison like <, >, <=, >=, ==, !=
    Comparison(HirComparisonOperator),
    /// Logical and
    And,
    /// Logical or
    Or,
    /// Mathematical addition
    Plus,
    /// Mathematical substraction
    Minus,
    /// Mathematical multiplication
    Times,
    /// Mathematical division
    Divide,
    /// Mathematical modulo
    Modulo,
}

/// Holds an infix operator combined with its span
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirInfix {
    pub span: LocalSpan,
    pub operator: HirInfixOperator,
}

/// Any prefix operator
///
/// A prefix operator operates on a single value and is written before it
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirPrefixOperator {
    /// Mathematical minus
    Minus,
    /// Logical negation
    Not,
}

/// Holds a prefix operator combined with its span
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirPrefix {
    pub span: LocalSpan,
    pub operator: HirPrefixOperator,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct HirVariableDeclaration {
    pub span: LocalSpan,
    pub ident: SpannedIdentifier,
    pub value: Box<HirExpression>,
}

/// Declaration of a property in a struct definition
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirPropertyDeclaration {
    /// The span of the declaration
    pub span: LocalSpan,
    /// The identifier inside of the struct
    pub identifier: SpannedIdentifier,
    /// The type of the property
    pub datatype: SpannedIdentifier,
}

/// Any function call, can be dotted
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirFunctionCall {
    pub span: LocalSpan,
    pub accessor: IdentifierPath,
    pub parameters: Vec<HirExpression>,
}

/// Any expression
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirExpression {
    /// A variable, for example `a`
    Variable(SpannedIdentifier),
    /// A path to a variable, for example `for.bar.a`
    Path(IdentifierPath),
    /// A literal value, for example `2.0` or `"Hello World"`
    Value(HirConstValue),
    /// A unary operation, for example `-a`
    UnaryOperation {
        operation: HirPrefix,
        value: Box<HirExpression>,
    },
    /// A binary operation, for example `a + b`
    BinaryOperation {
        operation: HirInfix,
        lhs: Box<HirExpression>,
        rhs: Box<HirExpression>,
    },
    /// A function call, for example `foo()` or `path.to.foo()`
    FunctionCall(HirFunctionCall),
    /// A block which returns something
    Block(HirBlock),
    /// An execute statement which compiles to its argument, for example `execute "say foo"`
    ///
    /// This statement is temporary and will be eventually replace by an std function
    Execute(Box<HirExpression>),
}

/// Any statement, the difference to an expression is that a statement does not return anything
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirStatement {
    /// A variable declaration, for example `let foo = 1`
    VariableDecl(HirVariableDeclaration),
    /// A function call, which can be both an expression and statement
    FunctionCall(HirFunctionCall),
    Block(HirBlock),
}

/// A function, which contains other statements
///
/// Apparently it does not store any parameters, so that is #todo
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirFunction {
    pub span: LocalSpan,
    /// The block of the function
    pub block: HirBlock,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirBlock {
    pub span: LocalSpan,
    /// The statements of this block
    pub statements: Vec<HirStatement>,
    /// The objects that got declared within this block
    pub inner_objects: Vec<HirObject>,
}

/// A struct definition
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirStruct {
    pub span: LocalSpan,
    /// All declaraed properties of this struct
    pub properties: Vec<HirPropertyDeclaration>,
    /// Objects that were defined inside this struct, for example associated methods
    pub inner_objects: Vec<HirObject>,
}

/// Any Object
///
/// Unused at the moment
#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirObject {
    Function(HirFunction),
    Struct(HirStruct),
}

impl HirConstValue {
    pub fn span(&self) -> LocalSpan {
        match self {
            HirConstValue::Fixed { span, value: _ } => *span,
            HirConstValue::Integer { span, value: _ } => *span,
            HirConstValue::String { span, value: _ } => *span,
        }
    }
}

impl HirComparisonOperator {
    /// Returns the associated [SpecialIdent]
    pub fn get_raw_special_ident(&self) -> SpecialIdent {
        use HirComparisonOperator::*;
        match self {
            Eq => SpecialIdent::CmpEq,
            Ne => SpecialIdent::CmpNe,
            Gt => SpecialIdent::CmpGt,
            Ge => SpecialIdent::CmpGe,
            Lt => SpecialIdent::CmpLt,
            Le => SpecialIdent::CmpLe,
        }
    }
}

impl HirInfixOperator {
    /// Returns the associated [SpecialIdent]
    pub fn get_special_ident(&self) -> SpecialIdent {
        use HirInfixOperator::*;
        match self {
            And => SpecialIdent::And,
            Or => SpecialIdent::Or,
            Plus => SpecialIdent::Add,
            Minus => SpecialIdent::Sub,
            Times => SpecialIdent::Mul,
            Divide => SpecialIdent::Div,
            Modulo => SpecialIdent::Modu,
            Comparison(value) => value.get_raw_special_ident(),
        }
    }
}

impl HirPrefixOperator {
    /// Returns the associated [Ident]
    pub fn get_ident(&self) -> Ident {
        Ident::Special(match self {
            HirPrefixOperator::Minus => SpecialIdent::UnaryMinus,
            HirPrefixOperator::Not => SpecialIdent::Not,
        })
    }
}

impl HirExpression {
    pub fn span(&self) -> LocalSpan {
        match self {
            HirExpression::FunctionCall(call) => call.span,
            HirExpression::Block(block) => block.span,
            HirExpression::Value(number) => number.span(),
            HirExpression::Variable(var) => var.span,
            HirExpression::Path(path) => match path.idents.as_slice() {
                [first, .., last] => first.span.until(&last.span),
                _ => unreachable!("A path has at least two values"),
            },
            HirExpression::BinaryOperation {
                lhs,
                operation: _,
                rhs,
            } => lhs.span().until(&rhs.span()),
            HirExpression::UnaryOperation { operation, value } => {
                operation.span.until(&value.span())
            }
            HirExpression::Execute(execute) => execute.span(),
        }
    }
}
