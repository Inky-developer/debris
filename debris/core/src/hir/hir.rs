use debris_common::{Code, LocalSpan, Span};
use lazy_static::lazy_static;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;
use std::rc::Rc;

use super::{
    get_span,
    hir_nodes::HirInfix,
    hir_nodes::HirPrefix,
    hir_nodes::{
        HirComparisonOperator, HirConstValue, HirExpression, HirFunction, HirFunctionCall,
        HirInfixOperator, HirPrefixOperator, HirStatement,
    },
    IdentifierPath, SpannedIdentifier,
};
use super::{ArithmeticParser, Rule};

use crate::error::{ParseError, Result};

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Hir {
    pub main_function: HirFunction,
    pub code: Rc<Code>,
}

impl Hir {
    pub fn from_code(input: Rc<Code>) -> Result<Self> {
        let program = ArithmeticParser::parse(Rule::program, &input.source)
            .map_err(|err: pest::error::Error<super::Rule>| {
                let (span_start, span_size) = match err.location {
                    pest::error::InputLocation::Pos(a) => (a - 1, 1),
                    pest::error::InputLocation::Span(span) => span,
                };
                ParseError {
                    expected: match err.variant {
                        pest::error::ErrorVariant::ParsingError {
                            positives,
                            negatives: _,
                        } => positives.iter().map(|rule| format!("{:?}", rule)).collect(),
                        pest::error::ErrorVariant::CustomError { message: _ } => vec![],
                    },
                    span: Span {
                        code: input.clone(),
                        local_span: LocalSpan::new(span_start, span_size),
                    },
                }
            })?
            .next()
            .unwrap();

        let hir_nodes: Result<_> = program
            .into_inner()
            .filter(|pair| !matches!(pair.as_rule(), Rule::EOI))
            .map(get_statement)
            .collect();

        Ok(Hir {
            main_function: HirFunction {
                inner_objects: Vec::new(),
                statements: hir_nodes?,
                span: LocalSpan::new(0, input.source.len()),
            },
            code: input,
        })
    }
}

fn get_statement(pair: Pair<Rule>) -> Result<HirStatement> {
    let inner = pair.into_inner().next().unwrap();

    Ok(match inner.as_rule() {
        Rule::assignment => {
            let span = inner.as_span();
            let mut values = inner.into_inner();
            let ident = values.next().unwrap().as_span().into();
            let expression = get_expression(values.next().unwrap())?;
            HirStatement::VariableDecl {
                span: get_span(span),
                ident,
                value: Box::new(expression),
            }
        }
        Rule::function_call => HirStatement::FunctionCall(get_function_call(inner)?),
        _ => unreachable!(),
    })
}

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(Rule::compare_eq, Left)
                | Operator::new(Rule::compare_ne, Left)
                | Operator::new(Rule::compare_gt, Left)
                | Operator::new(Rule::compare_ge, Left)
                | Operator::new(Rule::compare_lt, Left)
                | Operator::new(Rule::compare_le, Left),
            Operator::new(Rule::infix_and, Left) | Operator::new(Rule::infix_or, Left),
            Operator::new(Rule::infix_plus, Left) | Operator::new(Rule::infix_minus, Left),
            Operator::new(Rule::infix_times, Left)
                | Operator::new(Rule::infix_divide, Left)
                | Operator::new(Rule::infix_modulo, Left),
        ])
    };
}

fn get_expression(pair: Pair<Rule>) -> Result<HirExpression> {
    let pairs = pair.into_inner();

    PREC_CLIMBER.climb(
        pairs,
        |pair: Pair<Rule>| get_expression_primary(pair),
        |lhs: Result<HirExpression>, op: Pair<Rule>, rhs: Result<HirExpression>| {
            Ok(HirExpression::BinaryOperation {
                operation: get_operator(op),
                lhs: Box::new(lhs?),
                rhs: Box::new(rhs?),
            })
        },
    )
}

fn get_expression_primary(pair: Pair<Rule>) -> Result<HirExpression> {
    match pair.as_rule() {
        Rule::expression => get_expression(pair),
        Rule::prefix_value => {
            let mut inner = pair.into_inner();
            let prefix = get_unary_operator(inner.next().unwrap());
            let value = get_expression_primary(inner.next().unwrap())?;

            Ok(HirExpression::UnaryOperation {
                operation: prefix,
                value: Box::new(value),
            })
        }
        Rule::value => get_value(pair),
        _ => unreachable!(),
    }
}

fn get_value(pair: Pair<Rule>) -> Result<HirExpression> {
    let value = pair.into_inner().next().unwrap();
    Ok(match value.as_rule() {
        Rule::function_call => HirExpression::FunctionCall(get_function_call(value)?),
        Rule::integer => HirExpression::Value(HirConstValue::Integer {
            span: get_span(value.as_span()),
            value: value.as_str().parse().expect("Could not parse int literal"),
        }),
        Rule::fixed => HirExpression::Value(HirConstValue::Fixed {
            span: get_span(value.as_span()),
            value: value
                .as_str()
                .parse()
                .expect("Could not parse fixed literal"),
        }),
        Rule::string => HirExpression::Value(HirConstValue::String {
            span: get_span(value.as_span()),
            value: value.into_inner().next().unwrap().as_str().to_owned(),
        }),
        Rule::accessor => get_accessor(value.into_inner())?,
        Rule::execute => HirExpression::Execute(Box::new(get_expression(
            value.into_inner().next().unwrap(),
        )?)),
        _ => unreachable!(),
    })
}

fn get_function_call(pair: Pair<Rule>) -> Result<HirFunctionCall> {
    let span = pair.as_span();
    let mut function_call = pair.into_inner();

    let identifier: SpannedIdentifier = function_call.next().unwrap().as_span().into();
    let parameters: Result<_> = function_call
        .next()
        .unwrap()
        .into_inner()
        .map(get_expression)
        .collect();

    Ok(HirFunctionCall {
        span: get_span(span),
        accessor: identifier.into(),
        parameters: parameters?,
    })
}

fn get_accessor(pairs: Pairs<Rule>) -> Result<HirExpression> {
    let spanned_idents = pairs
        .map(|pair| SpannedIdentifier::new(get_span(pair.as_span())))
        .collect::<Vec<_>>();

    Ok(if spanned_idents.len() == 1 {
        HirExpression::Variable(spanned_idents.into_iter().nth(0).unwrap())
    } else {
        HirExpression::Path(IdentifierPath::new(spanned_idents))
    })
}

fn get_operator(pair: Pair<Rule>) -> HirInfix {
    let operator = match pair.as_rule() {
        Rule::infix_times => HirInfixOperator::Times,
        Rule::infix_divide => HirInfixOperator::Divide,
        Rule::infix_modulo => HirInfixOperator::Modulo,
        Rule::infix_plus => HirInfixOperator::Plus,
        Rule::infix_minus => HirInfixOperator::Minus,
        Rule::infix_and => HirInfixOperator::And,
        Rule::infix_or => HirInfixOperator::Or,
        Rule::compare_eq => HirInfixOperator::Comparison(HirComparisonOperator::Eq),
        Rule::compare_ne => HirInfixOperator::Comparison(HirComparisonOperator::Ne),
        Rule::compare_gt => HirInfixOperator::Comparison(HirComparisonOperator::Gt),
        Rule::compare_ge => HirInfixOperator::Comparison(HirComparisonOperator::Ge),
        Rule::compare_lt => HirInfixOperator::Comparison(HirComparisonOperator::Lt),
        Rule::compare_le => HirInfixOperator::Comparison(HirComparisonOperator::Le),
        _ => unreachable!(),
    };

    HirInfix {
        operator,
        span: get_span(pair.as_span()),
    }
}

fn get_unary_operator(pair: Pair<Rule>) -> HirPrefix {
    let span = pair.as_span();
    let operator = match pair.as_rule() {
        Rule::prefix_minus => HirPrefixOperator::Minus,
        Rule::prefix_not => HirPrefixOperator::Not,
        _ => unreachable!(),
    };
    HirPrefix {
        span: get_span(span),
        operator,
    }
}
