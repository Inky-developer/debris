//! Converts the high-level representation from the pest parser.
//! ToDo: Better, visitor-based design which does not use unwrap calls everywhere
use debris_common::{CodeRef, Span};
use lazy_static::lazy_static;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

use super::{
    hir_nodes::HirInfix,
    hir_nodes::HirPrefix,
    hir_nodes::{
        HirBlock, HirComparisonOperator, HirConstValue, HirExpression, HirFunction,
        HirFunctionCall, HirInfixOperator, HirItem, HirObject, HirPrefixOperator, HirStatement,
        HirVariableDeclaration, HirVariableInitialization,
    },
    DebrisParser, HirContext, IdentifierPath, Rule, SpannedIdentifier,
};

use crate::{
    error::{ParseError, Result},
    CompileContext,
};

/// A high level intermediate representation
///
/// Mostly work in progress
#[derive(Debug)]
pub struct Hir<'code> {
    pub main_function: HirBlock,
    pub code_ref: CodeRef<'code>,
}

impl<'code> Hir<'code> {
    /// Creates a `Hir` from code
    pub fn from_code(input: CodeRef<'code>, compile_context: &CompileContext) -> Result<Self> {
        let program = DebrisParser::parse(Rule::program, &input.get_code().source)
            .map_err(|err: pest::error::Error<super::Rule>| {
                let (span_start, span_size) = match err.location {
                    pest::error::InputLocation::Pos(a) => (input.get_offset() + a, 1),
                    pest::error::InputLocation::Span((start, len)) => {
                        (start + input.get_offset(), len)
                    }
                };
                ParseError {
                    expected: match err.variant {
                        pest::error::ErrorVariant::ParsingError {
                            positives,
                            negatives: _,
                        } => positives.iter().map(|rule| format!("{:?}", rule)).collect(),
                        pest::error::ErrorVariant::CustomError { message: _ } => vec![],
                    },
                    span: Span::new(span_start, span_size),
                }
            })?
            .next()
            .unwrap();

        let context = HirContext {
            input_file: input,
            compile_context,
        };
        let span = context.span(program.as_span());

        let mut objects = Vec::new();
        let mut statements = Vec::new();
        for item in program
            .into_inner()
            .filter(|rule| !matches!(rule.as_rule(), Rule::EOI))
        {
            let item = get_item(&context, item)?;
            match item {
                HirItem::Statement(stmt) => statements.push(stmt),
                HirItem::Object(obj) => objects.push(obj),
            }
        }

        Ok(Hir {
            main_function: HirBlock {
                objects,
                return_value: None,
                statements,
                span,
            },
            code_ref: input,
        })
    }
}

fn get_item(ctx: &HirContext, pair: Pair<Rule>) -> Result<HirItem> {
    let inner = pair.into_inner().next().unwrap();
    Ok(match inner.as_rule() {
        Rule::statement => HirItem::Statement(get_statement(ctx, inner)?),
        Rule::function_def => HirItem::Object(HirObject::Function(get_function_def(ctx, inner)?)),
        other => unreachable!("Unknown rule: {:?}", other),
    })
}

fn get_block(ctx: &HirContext, pair: Pair<Rule>) -> Result<HirBlock> {
    let span = ctx.span(pair.as_span());

    let mut rev_iter = pair.into_inner().rev();
    let last_item = rev_iter.next();

    let mut objects = Vec::new();
    let mut statements = Vec::new();
    for item in rev_iter.rev() {
        let item = get_item(ctx, item)?;
        match item {
            HirItem::Statement(stmt) => statements.push(stmt),
            HirItem::Object(obj) => objects.push(obj),
        }
    }

    let return_value = if let Some(last_item) = last_item {
        match last_item.as_rule() {
            Rule::expression => Some(get_expression(ctx, last_item)?.into()),
            _ => {
                statements.push(get_statement(ctx, last_item.into_inner().next().unwrap())?);
                None
            }
        }
    } else {
        None
    };

    Ok(HirBlock {
        span,
        objects: Vec::new(),
        statements,
        return_value,
    })
}

fn get_function_def(ctx: &HirContext, pair: Pair<Rule>) -> Result<HirFunction> {
    let span = ctx.span(pair.as_span());
    let mut inner_iter = pair.into_inner();
    let ident = SpannedIdentifier::new(ctx.span(inner_iter.next().unwrap().as_span()));
    let params = inner_iter.next().unwrap();
    let parameter_span = ctx.span(params.as_span());
    let param_list = get_param_list(ctx, params)?;
    let (return_type, block) = {
        let next = inner_iter.next().unwrap();
        match next.as_rule() {
            Rule::accessor => (
                Some(get_identifier_path(ctx, next.into_inner())?),
                get_block(ctx, inner_iter.next().unwrap())?,
            ),
            Rule::block => (None, get_block(ctx, next)?),
            _ => unreachable!(),
        }
    };

    Ok(HirFunction {
        block,
        ident,
        parameters: param_list,
        parameter_span,
        return_type,
        span,
    })
}

fn get_param_list(ctx: &HirContext, pair: Pair<Rule>) -> Result<Vec<HirVariableDeclaration>> {
    pair.into_inner()
        .map(|param| {
            let span = ctx.span(param.as_span());
            let mut iter = param.into_inner();
            let ident = SpannedIdentifier::new(ctx.span(iter.next().unwrap().as_span()));
            let typ = get_identifier_path(ctx, iter.next().unwrap().into_inner())?;
            Ok(HirVariableDeclaration { ident, typ, span })
        })
        .collect()
}

fn get_statement(ctx: &HirContext, pair: Pair<Rule>) -> Result<HirStatement> {
    let inner = pair.into_inner().next().unwrap();

    Ok(match inner.as_rule() {
        Rule::assignment => {
            let span = inner.as_span();
            let mut values = inner.into_inner();
            let ident = SpannedIdentifier::new(ctx.span(values.next().unwrap().as_span()));

            let expression = get_expression(ctx, values.next().unwrap())?;
            HirStatement::VariableDecl(HirVariableInitialization {
                span: ctx.span(span),
                ident,
                value: Box::new(expression),
            })
        }
        Rule::function_call => HirStatement::FunctionCall(get_function_call(ctx, inner)?),
        other => unreachable!("Got invalid rule: {:?}", other),
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

fn get_expression(ctx: &HirContext, pair: Pair<Rule>) -> Result<HirExpression> {
    let pairs = pair.into_inner();

    PREC_CLIMBER.climb(
        pairs,
        |expr_primary| get_expression_primary(ctx, expr_primary),
        |lhs: Result<HirExpression>, op: Pair<Rule>, rhs: Result<HirExpression>| {
            Ok(HirExpression::BinaryOperation {
                operation: get_operator(ctx, op),
                lhs: Box::new(lhs?),
                rhs: Box::new(rhs?),
            })
        },
    )
}

fn get_expression_primary(ctx: &HirContext, pair: Pair<Rule>) -> Result<HirExpression> {
    match pair.as_rule() {
        Rule::expression => get_expression(ctx, pair),
        Rule::prefix_value => {
            let mut inner = pair.into_inner();
            let prefix = get_unary_operator(ctx, inner.next().unwrap());
            let value = get_expression_primary(ctx, inner.next().unwrap())?;

            Ok(HirExpression::UnaryOperation {
                operation: prefix,
                value: Box::new(value),
            })
        }
        Rule::value => get_value(ctx, pair),
        _ => unreachable!(),
    }
}

fn get_value(ctx: &HirContext, pair: Pair<Rule>) -> Result<HirExpression> {
    let value = pair.into_inner().next().unwrap();
    Ok(match value.as_rule() {
        Rule::function_call => HirExpression::FunctionCall(get_function_call(ctx, value)?),
        Rule::integer => HirExpression::Value(HirConstValue::Integer {
            span: ctx.span(value.as_span()),
            value: value.as_str().parse().expect("Could not parse int literal"),
        }),
        Rule::fixed => HirExpression::Value(HirConstValue::Fixed {
            span: ctx.span(value.as_span()),
            value: value
                .as_str()
                .parse()
                .expect("Could not parse fixed literal"),
        }),
        Rule::string => HirExpression::Value(HirConstValue::String {
            span: ctx.span(value.as_span()),
            value: value.into_inner().next().unwrap().as_str().to_owned(),
        }),
        Rule::accessor => get_accessor(ctx, value.into_inner())?,
        Rule::block => HirExpression::Block(get_block(ctx, value)?),
        _ => unreachable!(),
    })
}

fn get_function_call(ctx: &HirContext, pair: Pair<Rule>) -> Result<HirFunctionCall> {
    let span = pair.as_span();
    let mut function_call = pair.into_inner();

    let accessor = match get_accessor(ctx, function_call.next().unwrap().into_inner())? {
        HirExpression::Path(path) => path,
        HirExpression::Variable(var) => var.into(),
        _ => unreachable!("get_accessor only returns a path or and ident"),
    };
    let parameters: Result<_> = function_call
        .next()
        .unwrap()
        .into_inner()
        .map(|expr| get_expression(ctx, expr))
        .collect();

    Ok(HirFunctionCall {
        span: ctx.span(span),
        accessor,
        parameters: parameters?,
    })
}

fn get_accessor(ctx: &HirContext, pairs: Pairs<Rule>) -> Result<HirExpression> {
    let spanned_idents = pairs
        .map(|pair| SpannedIdentifier::new(ctx.span(pair.as_span())))
        .collect::<Vec<_>>();

    Ok(if spanned_idents.len() == 1 {
        HirExpression::Variable(spanned_idents.into_iter().next().unwrap())
    } else {
        HirExpression::Path(IdentifierPath::new(spanned_idents))
    })
}

fn get_identifier_path(ctx: &HirContext, pairs: Pairs<Rule>) -> Result<IdentifierPath> {
    match get_accessor(ctx, pairs)? {
        HirExpression::Path(path) => Ok(path),
        HirExpression::Variable(var) => Ok(var.into()),
        _ => unreachable!(),
    }
}

fn get_operator(ctx: &HirContext, pair: Pair<Rule>) -> HirInfix {
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
        span: ctx.span(pair.as_span()),
    }
}

fn get_unary_operator(ctx: &HirContext, pair: Pair<Rule>) -> HirPrefix {
    let span = pair.as_span();
    let operator = match pair.as_rule() {
        Rule::prefix_minus => HirPrefixOperator::Minus,
        Rule::prefix_not => HirPrefixOperator::Not,
        _ => unreachable!(),
    };
    HirPrefix {
        span: ctx.span(span),
        operator,
    }
}
