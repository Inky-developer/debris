//! Defines every node used in the hir representation

use std::rc::Rc;

use debris_common::{CodeId, Ident, InputFiles, Span, SpecialIdent};

use super::{IdentifierPath, SpannedIdentifier};

/// A constant literal, already parsed
#[derive(Debug, Eq, PartialEq)]
pub enum HirConstValue {
    Integer {
        span: Span,
        value: i32,
    },
    Bool {
        span: Span,
        value: bool,
    },
    Fixed {
        span: Span,
        value: i32,
    },
    String {
        span: Span,
        value: Rc<str>,
    },
    FormatString {
        span: Span,
        value: Vec<HirFormatStringMember>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum HirFormatStringMember {
    String(Rc<str>),
    Variable(Box<HirExpression>),
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
    /// Mathematical subtraction
    Minus,
    /// Mathematical multiplication
    Times,
    /// Mathematical division
    Divide,
    /// Mathematical modulo
    Modulo,
    /// Accesses a property of an object
    Dot,
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
/// The id specifies the index of the matching [`HirModule`]
#[derive(Debug, PartialEq, Eq)]
pub struct HirImport {
    pub span: Span,
    pub ident: SpannedIdentifier,
    pub id: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum HirControlKind {
    Return,
    Break,
    Continue,
}

impl HirControlKind {
    pub fn returns(self) -> bool {
        match self {
            HirControlKind::Return | HirControlKind::Break => true,
            HirControlKind::Continue => false,
        }
    }

    pub fn takes_value(self) -> bool {
        match self {
            HirControlKind::Return | HirControlKind::Break => true,
            HirControlKind::Continue => false,
        }
    }
}

impl std::fmt::Display for HirControlKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HirControlKind::Break => write!(f, "break"),
            HirControlKind::Continue => write!(f, "continue"),
            HirControlKind::Return => write!(f, "return"),
        }
    }
}

/// Represents a control flow statement like return or break
#[derive(Debug, PartialEq, Eq)]
pub struct HirControlFlow {
    pub span: Span,
    pub kind: HirControlKind,
    pub expression: Option<Box<HirExpression>>,
}

/// An infinite loop
/// (Can be exited using control keywords like `break` and `return`)
#[derive(Debug, PartialEq, Eq)]
pub struct HirInfiniteLoop {
    pub span: Span,
    pub block: Box<HirBlock>,
}

/// Holds a variable type declaration like `foo: String`
/// This is used in method signatures
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HirParameterDeclaration {
    pub span: Span,
    pub ident: SpannedIdentifier,
    pub typ: HirTypePattern,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum HirDeclarationMode {
    Let,
    Comptime,
}

#[derive(Debug, PartialEq, Eq)]
pub enum HirVariablePattern {
    Path(IdentifierPath),
    Tuple(Vec<HirVariablePattern>),
}

/// Sets a variable like `let a = expression();`
#[derive(Debug, PartialEq, Eq)]
pub struct HirVariableInitialization {
    pub span: Span,
    pub pattern: HirVariablePattern,
    pub value: Box<HirExpression>,
    pub mode: HirDeclarationMode,
}

/// Similar to `HirVariableInitialization`, however this node
/// marks a write to an already initialized value
#[derive(Debug, PartialEq, Eq)]
pub struct HirVariableUpdate {
    pub span: Span,
    pub pattern: HirVariablePattern,
    pub value: Box<HirExpression>,
}

/// Declaration of a property in a struct definition
#[derive(Debug, Eq, PartialEq)]
pub struct HirPropertyDeclaration {
    /// The span of the declaration
    pub span: Span,
    /// The identifier inside of the struct
    pub ident: SpannedIdentifier,
    /// The type of the property
    pub datatype: HirTypePattern,
}

/// Any function call, can be dotted
#[derive(Debug, Eq, PartialEq)]
pub struct HirFunctionCall {
    pub span: Span,
    pub value: Box<HirExpression>,
    pub parameters_span: Span,
    pub parameters: Vec<HirExpression>,
}

/// An if-branch which checks a condition and runs code
/// depending on whether the condition is true or not
#[derive(Debug, Eq, PartialEq)]
pub struct HirConditionalBranch {
    pub span: Span,
    pub is_comptime: bool,
    pub condition: Box<HirExpression>,
    pub block_positive: Box<HirBlock>,
    pub block_negative: Option<Box<HirBlock>>,
}

/// Creates a Struct Object from a struct
#[derive(Debug, PartialEq, Eq)]
pub struct HirStructInitialization {
    pub span: Span,
    pub base: Box<HirExpression>,
    pub values: Vec<(SpannedIdentifier, HirExpression)>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct HirTupleInitialization {
    pub span: Span,
    pub values: Vec<HirExpression>,
}

/// Any expression
#[derive(Debug, Eq, PartialEq)]
pub enum HirExpression {
    /// A variable, for example `a`
    Variable(SpannedIdentifier),
    /// A path, for example `a.b.c`
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
    /// An anonymous function
    Function(HirFunction),
    /// A function call, for example `foo()` or `path.to.foo()`
    FunctionCall(HirFunctionCall),
    ConditionalBranch(HirConditionalBranch),
    StructInitialization(HirStructInitialization),
    TupleInitialization(HirTupleInitialization),
    InfiniteLoop(HirInfiniteLoop),
    ControlFlow(HirControlFlow),
}

/// Any statement, the difference to an expression is that a statement does not return anything
#[derive(Debug, Eq, PartialEq)]
pub enum HirStatement {
    /// A variable declaration, for example `let foo = 1`
    VariableDecl(HirVariableInitialization),
    /// A write to an already existing variable
    VariableUpdate(HirVariableUpdate),
    /// Controls the program flow
    ControlFlow(HirControlFlow),
    /// A normal block
    Block(HirBlock),
    /// A normal if statement
    ConditionalBranch(HirConditionalBranch),
    InfiniteLoop(HirInfiniteLoop),
    Expression(HirExpression),
}

/// Any pattern that is allowed to specify a function parameter type
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum HirTypePattern {
    /// A normal type, like `Int`
    Path(IdentifierPath),
    /// A function, like `fn(Int, Int) -> Int`
    Function {
        span: Span,
        parameters: Vec<HirTypePattern>,
        return_type: Option<Box<HirTypePattern>>,
    },
    Tuple {
        span: Span,
        values: Vec<HirTypePattern>,
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
    pub expression: HirExpression,
}

/// Anonymous functions don't have an explicit identifier,
/// So if the function is unnamed, the hir builder will try to infer it.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum HirFunctionName {
    Ident(SpannedIdentifier),
    Unnamed { file: CodeId, pos: Span },
}

impl HirFunctionName {
    pub fn span(&self) -> Span {
        match self {
            HirFunctionName::Ident(ident) => ident.span,
            HirFunctionName::Unnamed { pos, .. } => *pos,
        }
    }

    pub fn spanned_ident(&self) -> Option<SpannedIdentifier> {
        match self {
            HirFunctionName::Ident(spanned_ident) => Some(*spanned_ident),
            HirFunctionName::Unnamed { .. } => None,
        }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, HirFunctionName::Unnamed { .. })
    }

    pub fn into_ident(&self, input_files: &InputFiles) -> Ident {
        match self {
            HirFunctionName::Ident(span) => input_files.get_span_str(span.span).into(),
            HirFunctionName::Unnamed { file, pos } => {
                let code_ref = input_files.get_code_ref(*file);

                let file_name = code_ref
                    .get_code()
                    .path
                    .as_ref()
                    .map_or_else(|| file.to_string(), ToString::to_string);

                let (line, col) = input_files.line_col(*pos);
                let line = line + 1;
                let col = col + 1;
                Ident::Value(format!("<'{file_name}:{line}:{col}'>").into())
            }
        }
    }
}

/// A function, which contains other statements
#[derive(Debug, Eq, PartialEq)]
pub struct HirFunction {
    pub span: Span,
    pub signature_span: Span,
    pub parameter_span: Span,
    pub attributes: Vec<Attribute>,
    pub ident: HirFunctionName,
    /// The block containing all statements of the function
    pub block: HirBlock,
    pub parameters: Vec<HirParameterDeclaration>,
    pub return_type: Option<HirTypePattern>,
}

/// A struct definition
#[derive(Debug, Eq, PartialEq)]
pub struct HirStruct {
    pub span: Span,
    pub ident: SpannedIdentifier,
    pub attributes: Vec<Attribute>,
    /// All declared properties of this struct
    pub properties: Vec<HirPropertyDeclaration>,
    pub objects: Vec<HirObject>,
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
    /// Syntax sugar for a module from another file
    Import(HirImport),
}

impl HirObject {
    pub fn spanned_ident(&self) -> Option<SpannedIdentifier> {
        Some(match self {
            HirObject::Function(func) => func.ident.spanned_ident()?,
            HirObject::Struct(strukt) => strukt.ident,
            HirObject::Module(module) => module.ident,
            HirObject::Import(import) => import.ident,
        })
    }
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
            HirConstValue::Fixed { span, .. }
            | HirConstValue::Integer { span, .. }
            | HirConstValue::Bool { span, .. }
            | HirConstValue::String { span, .. }
            | HirConstValue::FormatString { span, .. } => *span,
        }
    }
}

impl HirComparisonOperator {
    /// Returns the associated [`SpecialIdent`]
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
    /// Returns the associated [`SpecialIdent`]
    pub fn get_special_ident(&self) -> SpecialIdent {
        use HirInfixOperator::*;
        match self {
            And => SpecialIdent::And,
            Or => SpecialIdent::Or,
            Plus => SpecialIdent::Add,
            Minus => SpecialIdent::Sub,
            Times => SpecialIdent::Mul,
            Divide => SpecialIdent::Div,
            Modulo => SpecialIdent::Mod,
            Comparison(value) => value.get_raw_special_ident(),
            Dot => SpecialIdent::Dot,
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

impl HirVariablePattern {
    pub fn span(&self) -> Span {
        match self {
            HirVariablePattern::Path(path) => path.span(),
            HirVariablePattern::Tuple(tuple) => match tuple.as_slice() {
                [] => Span::EMPTY,
                [single] => single.span(),
                [first, .., last] => first.span().until(last.span()),
            },
        }
    }
}

impl HirExpression {
    pub fn span(&self) -> Span {
        match self {
            HirExpression::Value(number) => number.span(),
            HirExpression::Variable(var) => var.span,
            HirExpression::Path(path) => path.span(),
            HirExpression::BinaryOperation {
                lhs,
                operation: _,
                rhs,
            } => lhs.span().until(rhs.span()),
            HirExpression::UnaryOperation { operation, value } => {
                operation.span.until(value.span())
            }
            HirExpression::Block(block) => block.span,
            HirExpression::Function(function) => function.span,
            HirExpression::FunctionCall(call) => call.span,
            HirExpression::ConditionalBranch(branch) => branch.span,
            HirExpression::StructInitialization(struct_instantiation) => struct_instantiation.span,
            HirExpression::TupleInitialization(tuple_initialization) => tuple_initialization.span,
            HirExpression::InfiniteLoop(inf_loop) => inf_loop.span,
            HirExpression::ControlFlow(control_flow) => control_flow.span,
        }
    }
}

impl HirStatement {
    pub fn span(&self) -> Span {
        match self {
            HirStatement::VariableDecl(var_decl) => var_decl.span,
            HirStatement::VariableUpdate(var_update) => var_update.span,
            HirStatement::ControlFlow(control_flow) => control_flow.span,
            HirStatement::Block(block) => block.span,
            HirStatement::ConditionalBranch(branch) => branch.span,
            HirStatement::InfiniteLoop(inf_loop) => inf_loop.span,
            HirStatement::Expression(expr) => expr.span(),
        }
    }
}
impl HirTypePattern {
    pub fn span(&self) -> Span {
        match self {
            HirTypePattern::Function { span, .. } | HirTypePattern::Tuple { span, .. } => *span,
            HirTypePattern::Path(path) => path.span(),
        }
    }
}

impl Attribute {
    pub fn span(&self) -> Span {
        self.expression.span()
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
