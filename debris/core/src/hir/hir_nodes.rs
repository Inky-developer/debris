use debris_common::{Ident, LocalSpan, SpecialIdent};

use super::{IdentifierPath, SpannedIdentifier};

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirConstValue {
    Integer { span: LocalSpan, value: i32 },
    Fixed { span: LocalSpan, value: i32 },
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirComparisonOperator {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirInfixOperator {
    Comparison(HirComparisonOperator),
    And,
    Or,
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirInfix {
    pub span: LocalSpan,
    pub operator: HirInfixOperator,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirPrefixOperator {
    Minus,
    Not,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirPrefix {
    pub span: LocalSpan,
    pub operator: HirPrefixOperator,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirPropertyDeclaration {
    pub span: LocalSpan,
    pub identifier: SpannedIdentifier,
    pub datatype: SpannedIdentifier,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirFunctionCall {
    pub span: LocalSpan,
    pub accessor: IdentifierPath,
    pub parameters: Vec<HirExpression>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirExpression {
    Variable(SpannedIdentifier),
    Number(HirConstValue),
    UnaryOperation {
        operation: HirPrefix,
        value: Box<HirExpression>,
    },
    BinaryOperation {
        operation: HirInfix,
        lhs: Box<HirExpression>,
        rhs: Box<HirExpression>,
    },
    FunctionCall(HirFunctionCall),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirStatement {
    VariableDecl {
        span: LocalSpan,
        ident: SpannedIdentifier,
        value: Box<HirExpression>,
    },
    FunctionCall(HirFunctionCall),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirFunction {
    pub span: LocalSpan,
    pub statements: Vec<HirStatement>,
    pub inner_objects: Vec<HirObject>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct HirStruct {
    pub span: LocalSpan,
    pub properties: Vec<HirPropertyDeclaration>,
    pub inner_objects: Vec<HirObject>,
}

/// Temporary
#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum HirObject {
    Function(HirFunction),
    Struct(HirStruct),
}

impl HirConstValue {
    pub fn span(&self) -> LocalSpan {
        match self {
            HirConstValue::Fixed { span, value: _ } => span.clone(),
            HirConstValue::Integer { span, value: _ } => span.clone(),
        }
    }
}

impl HirComparisonOperator {
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
        }
    }
}

impl HirPrefixOperator {
    pub fn get_special_ident(&self) -> Ident {
        Ident::Special(match self {
            HirPrefixOperator::Minus => SpecialIdent::UnaryMinus,
            HirPrefixOperator::Not => SpecialIdent::Not,
        })
    }
}

impl HirExpression {
    pub fn span(&self) -> LocalSpan {
        match self {
            HirExpression::FunctionCall(call) => call.span.clone(),
            HirExpression::Number(number) => number.span(),
            HirExpression::Variable(var) => var.span.clone(),
            HirExpression::BinaryOperation {
                lhs,
                operation: _,
                rhs,
            } => lhs.span().until(&rhs.span()),
            HirExpression::UnaryOperation { operation, value } => {
                operation.span.until(&value.span())
            }
        }
    }
}
