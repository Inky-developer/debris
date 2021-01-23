//! Defines every node used in the hir representation

use debris_common::{Ident, Span, SpecialIdent};

use super::{IdentifierPath, SpannedIdentifier};

/// A constant literal, already parsed
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum HirConstValue {
    Integer { span: Span, value: i32 },
    Fixed { span: Span, value: i32 },
    String { span: Span, value: String },
}

/// Any supported comparison operator
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
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
#[derive(Debug, Eq, PartialEq, Clone)]
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
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct HirInfix {
    pub span: Span,
    pub operator: HirInfixOperator,
}

/// Any prefix operator
///
/// A prefix operator operates on a single value and is written before it
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum HirPrefixOperator {
    /// Mathematical minus
    Minus,
    /// Logical negation
    Not,
}

/// Holds a prefix operator combined with its span
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct HirPrefix {
    pub span: Span,
    pub operator: HirPrefixOperator,
}

/// Marks an import statement.
/// The id specifies the index of the matching [HirModule]
#[derive(Debug, PartialEq, Eq)]
pub struct HirImport {
    pub span: Span,
    pub ident_span: Span,
    pub id: usize,
}

/// Holds a variable type declaration like `foo: String`
/// This is used in method signatures
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HirVariableDeclaration {
    pub span: Span,
    pub ident: SpannedIdentifier,
    pub typ: HirTypePattern,
}

/// Sets a variable like `let a = expression();`
#[derive(Debug, PartialEq, Eq)]
pub struct HirVariableInitialization {
    pub span: Span,
    pub ident: SpannedIdentifier,
    pub value: Box<HirExpression>,
}

/// Declaration of a property in a struct definition
#[derive(Debug, Eq, PartialEq)]
pub struct HirPropertyDeclaration {
    /// The span of the declaration
    pub span: Span,
    /// The identifier inside of the struct
    pub identifier: SpannedIdentifier,
    /// The type of the property
    pub datatype: SpannedIdentifier,
}

/// Any function call, can be dotted
#[derive(Debug, Eq, PartialEq)]
pub struct HirFunctionCall {
    pub span: Span,
    pub accessor: IdentifierPath,
    pub parameters_span: Span,
    pub parameters: Vec<HirExpression>,
}

/// An if-branch which checks a condition and runs code
/// depending on whether the condition is true or not
#[derive(Debug, Eq, PartialEq)]
pub struct HirConditionalBranch {
    pub span: Span,
    pub condition: Box<HirExpression>,
    pub block_positive: Box<HirBlock>,
    pub block_negative: Option<Box<HirBlock>>,
}

/// Any expression
#[derive(Debug, Eq, PartialEq)]
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
    /// A block which returns something
    Block(HirBlock),
    /// A function call, for example `foo()` or `path.to.foo()`
    FunctionCall(HirFunctionCall),
    ConditionalBranch(HirConditionalBranch),
}

/// Any statement, the difference to an expression is that a statement does not return anything
#[derive(Debug, Eq, PartialEq)]
pub enum HirStatement {
    /// A variable declaration, for example `let foo = 1`
    VariableDecl(HirVariableInitialization),
    /// A function call, which can be both an expression and statement
    FunctionCall(HirFunctionCall),
    /// Imports another debris file
    Import(HirImport),
    /// A normal block
    Block(HirBlock),
    /// A normal if statement
    CondiitonalBranch(HirConditionalBranch),
}

/// Any pattern that is allowed to specify a function parameter type
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum HirTypePattern {
    /// A normal type, like `Int`
    Path(IdentifierPath),
    /// A function, like `fn(Int, Int) -> Int`
    Function {
        parameters: Vec<HirTypePattern>,
        return_type: Option<Box<HirTypePattern>>,
        span: Span,
    },
}

/// A block of code. Usually contained withing a pair of {} parenthesis.
#[derive(Debug, Eq, PartialEq)]
pub struct HirBlock {
    pub span: Span,
    /// The statements of this block
    pub statements: Vec<HirStatement>,
    /// The returned value:
    pub return_value: Option<Box<HirExpression>>,
    /// The objects that got declared within this block
    pub objects: Vec<HirObject>,
}

/// Attributes are a form of metadata that can be applied
/// to any object.
#[derive(Debug, PartialEq, Eq)]
pub struct Attribute {
    pub accessor: IdentifierPath,
}

/// A function, which contains other statements
#[derive(Debug, Eq, PartialEq)]
pub struct HirFunction {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub ident: SpannedIdentifier,
    /// The block containing all statements of the function
    pub block: HirBlock,
    pub parameters: Vec<HirVariableDeclaration>,
    pub parameter_span: Span,
    pub return_type: Option<HirTypePattern>,
}

/// A struct definition
#[derive(Debug, Eq, PartialEq)]
pub struct HirStruct {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    /// All declaraed properties of this struct
    pub properties: Vec<HirPropertyDeclaration>,
}

/// A module with an associated name
#[derive(Debug, PartialEq, Eq)]
pub struct HirModule {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub ident: SpannedIdentifier,
    pub block: HirBlock,
}

#[derive(Debug, PartialEq, Eq)]
pub enum HirObject {
    Function(HirFunction),
    Struct(HirStruct),
    Module(HirModule),
}

/// Any Item
#[derive(Debug, Eq, PartialEq)]
pub enum HirItem {
    Object(HirObject),
    Statement(HirStatement),
}

impl HirConstValue {
    pub fn span(&self) -> Span {
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

impl HirBlock {
    /// Returns the span of the item in the block which is responsible
    /// for the return type
    pub fn last_item_span(&self) -> Span {
        if let Some(value) = &self.return_value {
            value.span()
        } else if let [.., last] = self.statements.as_slice() {
            last.span()
        } else {
            self.span
        }
    }
}

impl HirExpression {
    pub fn span(&self) -> Span {
        match self {
            HirExpression::Value(number) => number.span(),
            HirExpression::Variable(var) => var.span,
            HirExpression::Path(path) => match path.idents.as_slice() {
                [first, .., last] => first.span.until(last.span),
                _ => unreachable!("A path has at least two values"),
            },
            HirExpression::BinaryOperation {
                lhs,
                operation: _,
                rhs,
            } => lhs.span().until(rhs.span()),
            HirExpression::UnaryOperation { operation, value } => {
                operation.span.until(value.span())
            }
            HirExpression::Block(block) => block.span,
            HirExpression::FunctionCall(call) => call.span,
            HirExpression::ConditionalBranch(branch) => branch.span,
        }
    }
}

impl HirStatement {
    pub fn span(&self) -> Span {
        match self {
            HirStatement::VariableDecl(var_decl) => var_decl.span,
            HirStatement::FunctionCall(call) => call.span,
            HirStatement::Import(import) => import.span,
            HirStatement::Block(block) => block.span,
            HirStatement::CondiitonalBranch(branch) => branch.span,
        }
        // // The inner_span does not contains the ending semicolon
        // Span::new(inner_span.start(), inner_span.len() + 1)
        // ToDo: Resolve this. Not all statements have to contain a trailing
        // semicolon
    }
}
impl HirTypePattern {
    pub fn span(&self) -> Span {
        match self {
            HirTypePattern::Function { span, .. } => *span,
            HirTypePattern::Path(path) => path.span(),
        }
    }
}

impl Attribute {
    pub fn span(&self) -> Span {
        self.accessor.span()
    }
}

impl HirFunction {
    pub fn return_type_span(&self) -> Span {
        match &self.return_type {
            Some(path) => path.span(),
            None => Span::new(self.parameter_span.end(), 1),
        }
    }
}
