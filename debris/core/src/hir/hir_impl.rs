//! Converts the high-level representation from the pest parser.
//! ToDo: Better, visitor-based design which does not use unwrap calls everywhere
//! Maybe switch to a different parser generator or implement the parser by hand
use debris_common::{CodeId, CodeRef, Span};
use lazy_static::lazy_static;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

use super::{
    hir_nodes::HirInfix,
    hir_nodes::HirPrefix,
    hir_nodes::{
        Attribute, HirBlock, HirComparisonOperator, HirConditionalBranch, HirConstValue,
        HirExpression, HirFunction, HirFunctionCall, HirImport, HirInfixOperator, HirItem,
        HirModule, HirObject, HirPrefixOperator, HirStatement, HirTypePattern,
        HirVariableDeclaration, HirVariableInitialization,
    },
    DebrisParser, HirContext, IdentifierPath, ImportDependencies, Rule, SpannedIdentifier,
};

use crate::{
    error::{ParseError, Result},
    CompileContext,
};

/// This struct stores the high-level intermediate representation of a single file.
/// A [HirFile] is very similar to a [super::hir_nodes::HirModule], but it can store a
/// list of imports
#[derive(Debug)]
pub struct HirFile {
    pub main_function: HirBlock,
    pub code_id: CodeId,
}

impl HirFile {
    /// Creates a [HirFile] from code.
    pub fn from_code(
        input: CodeRef,
        compile_context: &CompileContext,
        dependencies: &mut ImportDependencies,
    ) -> Result<Self> {
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

        let mut context = HirContext::new(input, compile_context, dependencies);
        let span = context.span(program.as_span());

        let mut objects = Vec::new();
        let mut statements = Vec::new();
        for item in program
            .into_inner()
            .filter(|rule| !matches!(rule.as_rule(), Rule::EOI))
        {
            let item = get_item(&mut context, item)?;
            match item {
                HirItem::Statement(stmt) => statements.push(stmt),
                HirItem::Object(obj) => objects.push(obj),
            }
        }

        let hir_file = HirFile {
            main_function: HirBlock {
                objects,
                return_value: None,
                statements,
                span,
            },
            code_id: input.file,
        };

        Ok(hir_file)
    }
}

fn get_item(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirItem> {
    let inner = pair.into_inner().next().unwrap();
    Ok(match inner.as_rule() {
        Rule::statement => HirItem::Statement(get_statement(ctx, inner)?),
        Rule::object_def => HirItem::Object(get_object_def(ctx, inner)?),
        other => unreachable!("Unknown rule: {:?}", other),
    })
}

fn get_object_def(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirObject> {
    let mut inner = pair.into_inner();
    let next = inner.next().unwrap();

    match next.as_rule() {
        Rule::attribute_list => {
            let attributes = next
                .into_inner()
                .map(|attr| get_attribute(ctx, attr))
                .collect::<Result<_>>()?;
            get_object(ctx, inner.next().unwrap(), attributes)
        }
        _ => get_object(ctx, next, Vec::new()),
    }
}

fn get_object(
    ctx: &mut HirContext,
    pair: Pair<Rule>,
    attributes: Vec<Attribute>,
) -> Result<HirObject> {
    let obj = pair.into_inner().next().unwrap();
    match obj.as_rule() {
        Rule::function_def => Ok(HirObject::Function(get_function_def(ctx, obj, attributes)?)),
        Rule::module => Ok(HirObject::Module(get_module(ctx, obj, attributes)?)),
        other => unreachable!("{:?}", other),
    }
}

fn get_attribute(ctx: &HirContext, pair: Pair<Rule>) -> Result<Attribute> {
    let accessor = get_identifier_path(ctx, pair.into_inner())?;
    Ok(Attribute { accessor })
}

fn get_module(
    ctx: &mut HirContext,
    pair: Pair<Rule>,
    attributes: Vec<Attribute>,
) -> Result<HirModule> {
    let span = ctx.span(pair.as_span());
    let mut inner = pair.into_inner();

    let ident = SpannedIdentifier::new(ctx.span(inner.next().unwrap().as_span()));
    let block = get_block(ctx, inner.next().unwrap())?;
    Ok(HirModule {
        span,
        ident,
        block,
        attributes,
    })
}

fn get_block(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirBlock> {
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
                match get_item(ctx, last_item)? {
                    HirItem::Statement(stmt) => statements.push(stmt),
                    HirItem::Object(obj) => objects.push(obj),
                }
                None
            }
        }
    } else {
        None
    };

    Ok(HirBlock {
        span,
        objects,
        statements,
        return_value,
    })
}

fn get_conditional_branch(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirConditionalBranch> {
    let span = ctx.span(pair.as_span());
    let mut values = pair.into_inner();
    let condition = get_expression(ctx, values.next().unwrap())?;
    let block_positive = get_block(ctx, values.next().unwrap())?;
    let block_negative = values
        .next()
        .map(|v| match v.as_rule() {
            Rule::block => get_block(ctx, v),
            // Syntax sugar:
            // if a {} else if b {} else {}
            // is equivalent to
            // if a {} else { if b {} else {} }
            Rule::if_branch => {
                let span = ctx.span(v.as_span());
                let expression = HirExpression::ConditionalBranch(get_conditional_branch(ctx, v)?);
                Ok(HirBlock {
                    objects: vec![],
                    statements: vec![],
                    return_value: Some(Box::new(expression)),
                    span,
                })
            }
            other => unimplemented!("{:?}", other),
        })
        .transpose()?;
    Ok(HirConditionalBranch {
        condition: Box::new(condition),
        block_positive: Box::new(block_positive),
        block_negative: block_negative.map(Box::new),
        span,
    })
}

fn get_function_def(
    ctx: &mut HirContext,
    pair: Pair<Rule>,
    attributes: Vec<Attribute>,
) -> Result<HirFunction> {
    let span = ctx.span(pair.as_span());
    let mut inner_iter = pair.into_inner();
    let ident = SpannedIdentifier::new(ctx.span(inner_iter.next().unwrap().as_span()));

    let mut signature = inner_iter.next().unwrap().into_inner();
    let params = signature.next().unwrap();
    let parameter_span = ctx.span(params.as_span());
    let param_list = get_param_list(ctx, params)?;

    let return_type = signature
        .next()
        .map(|pair| get_type_pattern(ctx, pair))
        .transpose()?;

    let block = get_block(ctx, inner_iter.next().unwrap())?;

    Ok(HirFunction {
        attributes,
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
            let typ = get_type_pattern(ctx, iter.next().unwrap())?;
            Ok(HirVariableDeclaration { ident, typ, span })
        })
        .collect()
}

fn get_statement(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirStatement> {
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
        Rule::import => HirStatement::Import(get_import(ctx, inner)?),
        other => unreachable!("Got invalid rule: {:?}", other),
    })
}

fn get_import(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirImport> {
    let import_span = ctx.span(pair.as_span());
    let spanned_ident =
        SpannedIdentifier::new(ctx.span(pair.into_inner().next().unwrap().as_span()));
    let ident_span = spanned_ident.span;

    let id = ctx.add_import_file(spanned_ident);
    Ok(HirImport {
        id,
        ident_span,
        span: import_span,
    })
}

// ToDo: implement rules for prefix operators, since debris has `not x` and `- x`
lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(Rule::infix_and, Left),
            Operator::new(Rule::infix_or, Left),
            Operator::new(Rule::compare_eq, Left)
                | Operator::new(Rule::compare_ne, Left)
                | Operator::new(Rule::compare_gt, Left)
                | Operator::new(Rule::compare_ge, Left)
                | Operator::new(Rule::compare_lt, Left)
                | Operator::new(Rule::compare_le, Left),
            Operator::new(Rule::infix_plus, Left) | Operator::new(Rule::infix_minus, Left),
            Operator::new(Rule::infix_times, Left)
                | Operator::new(Rule::infix_divide, Left)
                | Operator::new(Rule::infix_modulo, Left),
        ])
    };
}

fn get_expression(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirExpression> {
    let pairs = pair.into_inner();

    // Move this out so that the lifetimes don't get messed up
    let file_offset = ctx.file_offset;

    PREC_CLIMBER.climb(
        pairs,
        |expr_primary| get_expression_primary(ctx, expr_primary),
        |lhs: Result<HirExpression>, op: Pair<Rule>, rhs: Result<HirExpression>| {
            Ok(HirExpression::BinaryOperation {
                operation: get_operator(file_offset, op),
                lhs: Box::new(lhs?),
                rhs: Box::new(rhs?),
            })
        },
    )
}

fn get_expression_primary(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirExpression> {
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

fn get_value(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirExpression> {
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
        Rule::if_branch => HirExpression::ConditionalBranch(get_conditional_branch(ctx, value)?),
        _ => unreachable!(),
    })
}

fn get_function_call(ctx: &mut HirContext, pair: Pair<Rule>) -> Result<HirFunctionCall> {
    let span = pair.as_span();
    let mut function_call = pair.into_inner();

    let accessor = match get_accessor(ctx, function_call.next().unwrap().into_inner())? {
        HirExpression::Path(path) => path,
        HirExpression::Variable(var) => var.into(),
        _ => unreachable!("get_accessor only returns a path or and ident"),
    };
    let parameters = function_call.next().unwrap();
    let parameters_span = ctx.span(parameters.as_span());
    let parameters: Result<_> = parameters
        .into_inner()
        .map(|expr| get_expression(ctx, expr))
        .collect();

    Ok(HirFunctionCall {
        span: ctx.span(span),
        accessor,
        parameters: parameters?,
        parameters_span,
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

fn get_type_pattern(ctx: &HirContext, pair: Pair<Rule>) -> Result<HirTypePattern> {
    let pair = pair.into_inner().next().unwrap();
    Ok(match pair.as_rule() {
        Rule::accessor => HirTypePattern::Path(get_identifier_path(ctx, pair.into_inner())?),
        Rule::fn_pattern => {
            let span = ctx.span(pair.as_span());
            let mut inner = pair.into_inner();
            let parameter_pair = inner.next().unwrap();
            let return_type = inner.next();

            let parameters = parameter_pair
                .into_inner()
                .map(|pat| get_type_pattern(ctx, pat))
                .collect::<Result<_>>()?;
            let return_type = return_type
                .map(|pat| get_type_pattern(ctx, pat))
                .transpose()?;
            HirTypePattern::Function {
                parameters,
                return_type: return_type.map(Box::new),
                span,
            }
        }
        other => unreachable!("Invalid type pattern rule: {:?}", other),
    })
}

fn get_operator(file_offset: usize, pair: Pair<Rule>) -> HirInfix {
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
        span: HirContext::normalize_pest_span(pair.as_span(), file_offset),
    }
}

fn get_unary_operator(ctx: &HirContext, pair: Pair<Rule>) -> HirPrefix {
    let prefix = pair.into_inner().next().unwrap();
    let span = prefix.as_span();
    let operator = match prefix.as_rule() {
        Rule::prefix_minus => HirPrefixOperator::Minus,
        Rule::prefix_not => HirPrefixOperator::Not,
        other => unreachable!("Invalid unary operator: {:?}", other),
    };
    HirPrefix {
        span: ctx.span(span),
        operator,
    }
}
