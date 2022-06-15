//! Converts the high-level representation from the pest parser.
use std::rc::Rc;

use crate::{
    hir_nodes::{
        Attribute, HirComparisonOperator, HirConditionalBranch, HirConstValue, HirControlFlow,
        HirControlKind, HirDeclarationMode, HirExpression, HirFormatStringMember, HirFunction,
        HirFunctionCall, HirFunctionName, HirImport, HirInfiniteLoop, HirInfix, HirInfixOperator,
        HirModule, HirObject, HirParameterDeclaration, HirPrefix, HirPrefixOperator,
        HirPropertyDeclaration, HirStatement, HirStruct, HirStructInitialization,
        HirTupleInitialization, HirTypePattern, HirVariableInitialization, HirVariablePattern,
        HirVariableUpdate,
    },
    IdentifierPath, SpannedIdentifier,
};
use debris_common::{CodeId, CodeRef, CompileContext};
use debris_error::{ParseError, SingleCompileError};
use debris_parser::{
    ast::{self, Ast, AstToken, FormatStringComponent},
    error::ExpectedItem,
    parser::parse,
    token::PrefixOperator,
};

use super::{
    hir_nodes::{HirBlock, HirItem},
    HirContext, ImportDependencies,
};

/// This struct stores the high-level intermediate representation of a single file.
/// A [`HirFile`] is very similar to a [`super::hir_nodes::HirModule`], but it can store a
/// list of imports
#[derive(Debug)]
pub struct HirFile {
    pub main_function: HirBlock,
    pub code_id: CodeId,
}

impl HirFile {
    /// Creates a [`HirFile`] from code.
    pub fn from_code(
        input: CodeRef,
        compile_context: &CompileContext,
        dependencies: &mut ImportDependencies,
    ) -> Result<Self, Vec<SingleCompileError>> {
        let mut context = HirContext::new(input, compile_context, dependencies);

        let syntax_tree = Rc::new(parse(&input.get_code().source));
        // println!("{}", syntax_tree.debug_fmt(&input.get_code().source));
        if !syntax_tree.errors.is_empty() {
            return Err(syntax_tree
                .errors
                .iter()
                .map(|error| context.handle_parse_error(error).into())
                .collect());
        }

        let ast = Ast::from(syntax_tree);
        let program = ast.program;

        let mut objects = Vec::new();
        let mut statements = Vec::new();
        for item in program.statements() {
            let item = context.handle_item(&item);
            match item {
                HirItem::Statement(stmt) => statements.push(stmt),
                HirItem::Object(obj) => objects.push(obj),
            }
        }

        if !context.errors.is_empty() {
            return Err(context.errors.into_iter().map(Into::into).collect());
        }

        let span = context.item_span(&program);
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

impl HirContext<'_, '_> {
    /// Creates an expression that accesses `lhs` on `rhs` and tries to
    /// avoid creating `BinaryExpression`s
    fn build_accessor(lhs: HirExpression, rhs: &HirExpression) -> HirExpression {
        let rhs_opt = match rhs {
            HirExpression::Variable(ident) => *ident,
            _ => unreachable!("rhs must always be an ident"),
        };

        match (lhs, rhs_opt) {
            (HirExpression::Variable(var), rhs) => {
                HirExpression::Path(IdentifierPath::new(vec![var, rhs]))
            }
            (HirExpression::Path(path), rhs) => {
                let mut prev_parts = path.into_inner();
                prev_parts.push(rhs);
                HirExpression::Path(IdentifierPath::new(prev_parts))
            }
            (lhs, rhs) => HirExpression::PropertyAccess {
                lhs: Box::new(lhs),
                rhs,
            },
        }
    }

    fn handle_assignment(&mut self, assignment: &ast::Assignment) -> HirVariableInitialization {
        let span = self.item_span(assignment);
        let pattern = self.handle_pattern(&assignment.pattern());
        let value = self.handle_expression(&assignment.value());
        let value = match &pattern {
            HirVariablePattern::Path(path) if path.single_ident().is_some() => Box::new(
                Self::maybe_add_name_to_expression(value, *path.single_ident().unwrap()),
            ),
            _ => Box::new(value),
        };

        let mode = match assignment.assign_mode() {
            ast::AssignMode::Let(_) => HirDeclarationMode::Let,
            ast::AssignMode::Comptime(_) => HirDeclarationMode::Comptime,
        };
        HirVariableInitialization {
            span,
            pattern,
            value,
            mode,
        }
    }

    fn handle_attribute_list(&mut self, list: &ast::AttributeList) -> Vec<Attribute> {
        list.attributes()
            .map(|attr| Attribute {
                expression: self.handle_expression(&attr),
            })
            .collect()
    }

    fn handle_block(&mut self, block: &ast::Block) -> HirBlock {
        let span = self.item_span(block);

        let (mut statements, mut objects) = (Vec::new(), Vec::new());
        for item in block.statements() {
            let item = self.handle_item(&item);
            match item {
                HirItem::Object(obj) => objects.push(obj),
                HirItem::Statement(stmt) => statements.push(stmt),
            }
        }

        let return_value = block
            .last_expr()
            .map(|expr| self.handle_expression(&expr))
            .map(Box::new);
        HirBlock {
            span,
            statements,
            return_value,
            objects,
        }
    }

    fn handle_branch(&mut self, branch: &ast::Branch) -> HirConditionalBranch {
        let span = self.item_span(branch);
        let is_comptime = branch.comptime_token().is_some();
        let condition = Box::new(self.handle_expression(&branch.condition()));
        let block_positive = Box::new(self.handle_block(&branch.block()));
        let block_negative = match branch.else_branch() {
            Some(ast::BranchElse::Block(block)) => Some(Box::new(self.handle_block(&block))),
            Some(ast::BranchElse::Branch(else_branch)) => {
                let span = self.item_span(&else_branch);
                let else_branch = self.handle_branch(&else_branch);
                let block = HirBlock {
                    span,
                    objects: Vec::new(),
                    statements: Vec::new(),
                    return_value: Some(Box::new(HirExpression::ConditionalBranch(else_branch))),
                };
                Some(Box::new(block))
            }
            None => None,
        };

        HirConditionalBranch {
            span,
            is_comptime,
            condition,
            block_positive,
            block_negative,
        }
    }

    fn handle_control_flow(&mut self, control_flow: &ast::ControlFlowOperation) -> HirControlFlow {
        let span = self.item_span(control_flow);
        let op = control_flow.op().to_token();
        let kind = match op.kind.control_flow_operator().unwrap() {
            debris_parser::token::ControlFlowOperator::Break => HirControlKind::Break,
            debris_parser::token::ControlFlowOperator::Continue => HirControlKind::Continue,
            debris_parser::token::ControlFlowOperator::Return => HirControlKind::Return,
        };
        let expression = control_flow
            .expr()
            .map(|expr| self.handle_expression(&expr))
            .map(Box::new);
        HirControlFlow {
            span,
            kind,
            expression,
        }
    }

    fn handle_expression(&mut self, expression: &ast::Expression) -> HirExpression {
        match expression {
            ast::Expression::InfixOp(op) => self.handle_infix_op(op),
            ast::Expression::PrefixOp(op) => self.handle_prefix_op(op),
            ast::Expression::PostfixOp(op) => self.handle_postfix_op(op),
            ast::Expression::Value(value) => self.handle_value(value),
        }
    }

    fn handle_function(&mut self, function: &ast::Function) -> HirFunction {
        let span = self.item_span(function);
        let attributes = function
            .attributes()
            .map(|attr| self.handle_attribute_list(&attr))
            .unwrap_or_default();
        let is_comptime = function.comptime_token().is_some();
        let block = self.handle_block(&function.block());
        let ident = match function.ident() {
            Some(ident) => HirFunctionName::Ident(self.span(ident.to_token()).into()),
            None => HirFunctionName::Unnamed {
                file: self.input_file.file,
                pos: span,
            },
        };

        let param_list = function.param_declaration_list();
        let parameter_span = self.item_span(&param_list);
        let parameters = param_list
            .declarations()
            .map(|decl| {
                let span = self.item_span(&decl);
                let ident = self.item_span(&decl.lhs()).into();
                let typ = self.handle_type_pattern(&decl.rhs());

                HirParameterDeclaration { span, ident, typ }
            })
            .collect();

        let ret_decl = function.ret_declaration();
        let signature_span = ret_decl.as_ref().map_or(parameter_span, |ret_decl| {
            parameter_span.until(self.item_span(ret_decl))
        });
        let return_type = ret_decl.map(|typ| self.handle_type_pattern(&typ));

        HirFunction {
            span,
            signature_span,
            parameter_span,
            is_comptime,
            attributes,
            ident,
            block,
            parameters,
            return_type,
        }
    }

    fn handle_import(&mut self, import: &ast::Import) -> HirImport {
        let span = self.item_span(import);
        let ident = self.item_span(&import.ident()).into();
        let id = self.add_import_file(ident);
        HirImport { span, ident, id }
    }

    fn handle_infinite_loop(&mut self, inf_loop: &ast::InfLoop) -> HirInfiniteLoop {
        let span = self.item_span(inf_loop);
        let block = Box::new(self.handle_block(&inf_loop.block()));
        HirInfiniteLoop { span, block }
    }

    fn handle_infix_op(&mut self, op: &ast::InfixOp) -> HirExpression {
        let left = self.handle_expression(&op.left());

        if let Some(right) = op.right() {
            let right = self.handle_expression(&right);

            let op_span = self.item_span(op);
            let op = op
                .operator()
                .expect("Cannot be missing operator with rhs existing")
                .to_token()
                .kind
                .infix_operator()
                .unwrap();
            let op = match op {
                debris_parser::token::InfixOperator::Dot => {
                    return Self::build_accessor(left, &right)
                }
                debris_parser::token::InfixOperator::Plus => HirInfixOperator::Plus,
                debris_parser::token::InfixOperator::Minus => HirInfixOperator::Minus,
                debris_parser::token::InfixOperator::Times => HirInfixOperator::Times,
                debris_parser::token::InfixOperator::Divide => HirInfixOperator::Divide,
                debris_parser::token::InfixOperator::Modulo => HirInfixOperator::Modulo,
                debris_parser::token::InfixOperator::Equal => {
                    HirInfixOperator::Comparison(HirComparisonOperator::Eq)
                }
                debris_parser::token::InfixOperator::NotEqual => {
                    HirInfixOperator::Comparison(HirComparisonOperator::Ne)
                }
                debris_parser::token::InfixOperator::GreaterOrEqual => {
                    HirInfixOperator::Comparison(HirComparisonOperator::Ge)
                }
                debris_parser::token::InfixOperator::Greater => {
                    HirInfixOperator::Comparison(HirComparisonOperator::Gt)
                }
                debris_parser::token::InfixOperator::LessOrEqual => {
                    HirInfixOperator::Comparison(HirComparisonOperator::Le)
                }
                debris_parser::token::InfixOperator::Less => {
                    HirInfixOperator::Comparison(HirComparisonOperator::Lt)
                }
                debris_parser::token::InfixOperator::And => HirInfixOperator::And,
                debris_parser::token::InfixOperator::Or => HirInfixOperator::Or,
            };
            let operation = HirInfix {
                operator: op,
                span: op_span,
            };

            HirExpression::BinaryOperation {
                operation,
                lhs: Box::new(left),
                rhs: Box::new(right),
            }
        } else {
            left
        }
    }

    fn handle_item(&mut self, statement: &ast::Statement) -> HirItem {
        use ast::Statement::*;
        match statement {
            Assignment(assignment) => HirItem::Statement(HirStatement::VariableDecl(
                self.handle_assignment(assignment),
            )),
            Block(block) => HirItem::Statement(HirStatement::Block(self.handle_block(block))),
            Branch(branch) => {
                HirItem::Statement(HirStatement::ConditionalBranch(self.handle_branch(branch)))
            }
            Expression(expr) => {
                HirItem::Statement(HirStatement::Expression(self.handle_expression(expr)))
            }
            Function(function) => {
                HirItem::Object(HirObject::Function(self.handle_function(function)))
            }
            Import(import) => HirItem::Object(HirObject::Import(self.handle_import(import))),
            InfLoop(inf_loop) => HirItem::Statement(HirStatement::InfiniteLoop(
                self.handle_infinite_loop(inf_loop),
            )),
            Module(module) => HirItem::Object(HirObject::Module(self.handle_module(module))),
            Update(update) => {
                HirItem::Statement(HirStatement::VariableUpdate(self.handle_update(update)))
            }
            Struct(strukt) => HirItem::Object(HirObject::Struct(self.handle_struct(strukt))),
            WhileLoop(while_loop) => HirItem::Statement(HirStatement::InfiniteLoop(
                self.handle_while_loop(while_loop),
            )),
        }
    }

    fn handle_module(&mut self, module: &ast::Module) -> HirModule {
        let span = self.item_span(module);
        let ident: SpannedIdentifier = self.span(module.ident().to_token()).into();
        let block = self.handle_block(&module.block());
        HirModule {
            span,
            attributes: Vec::new(),
            ident,
            block,
        }
    }

    fn handle_path(&self, path: &ast::Path) -> IdentifierPath {
        let idents = path
            .segments()
            .map(|segment| self.span(segment.to_token()))
            .map(|span| SpannedIdentifier { span })
            .collect();
        IdentifierPath::new(idents)
    }

    fn handle_parse_error(&mut self, error: &debris_parser::error::ParseErrorKind) -> ParseError {
        match error {
            debris_parser::error::ParseErrorKind::LeftOverInput(ident) => {
                ParseError::LeftoverInput {
                    span: self.span(*ident),
                }
            }
            debris_parser::error::ParseErrorKind::UnexpectedComma(ident) => {
                ParseError::UnexpectedComma {
                    span: self.span(*ident),
                }
            }
            debris_parser::error::ParseErrorKind::UnexpectedFunctionIdent(ident) => {
                ParseError::UnexpectedFunctionIdent {
                    span: self.span(*ident),
                }
            }
            debris_parser::error::ParseErrorKind::UnexpectedPath(span) => {
                ParseError::UnexpectedPath {
                    span: self.normalize_local_span(*span),
                }
            }
            debris_parser::error::ParseErrorKind::UnexpectedToken { got, expected } => {
                ParseError::UnexpectedToken {
                    span: self.span(*got),
                    expected: expected
                        .iter()
                        .map(|kind| match kind {
                            ExpectedItem::ControlFlowOperator => {
                                "control flow operator".to_string()
                            }
                            ExpectedItem::FormatString => "format string".to_string(),
                            ExpectedItem::TokenKind(kind) => kind.to_string(),
                            ExpectedItem::Statement => "statement".to_string(),
                            ExpectedItem::Value => "value".to_string(),
                        })
                        .collect(),
                }
            }
        }
    }

    fn handle_pattern(&self, pattern: &ast::Pattern) -> HirVariablePattern {
        match pattern {
            ast::Pattern::Function(_) => unreachable!(),
            ast::Pattern::Path(path) => HirVariablePattern::Path(self.handle_path(path)),
            ast::Pattern::Tuple(tuple) => HirVariablePattern::Tuple(
                tuple
                    .sub_patterns()
                    .map(|pat| self.handle_pattern(&pat))
                    .collect(),
            ),
        }
    }

    fn handle_postfix_op(&mut self, op: &ast::PostfixOp) -> HirExpression {
        let span = self.item_span(op);
        let value = self.handle_expression(&op.value());
        match op.op() {
            ast::PostfixOperator::ParamList(params) => {
                let parameters_span = self.item_span(&params);
                let mut parameters: Vec<HirExpression> = params
                    .arguments()
                    .map(|arg| self.handle_expression(&arg))
                    .collect();

                // syntax: ```function() { block(); };```
                // The block gets converted into a lambda function and added as the last parameter
                if let Some(lambda_block) = params.lambda() {
                    let block_span = self.item_span(&lambda_block);
                    let function = HirFunction {
                        attributes: Vec::new(),
                        is_comptime: false,
                        block: self.handle_block(&lambda_block),
                        ident: HirFunctionName::Unnamed {
                            file: self.input_file.file,
                            pos: block_span,
                        },
                        parameter_span: block_span.at_start(),
                        parameters: Vec::new(),
                        return_type: None,
                        signature_span: block_span.at_start(),
                        span: block_span,
                    };
                    parameters.push(HirExpression::Function(function));
                }
                HirExpression::FunctionCall(HirFunctionCall {
                    span,
                    value: Box::new(value),
                    parameters,
                    parameters_span,
                })
            }
            ast::PostfixOperator::StructLiteral(struct_literal) => {
                HirExpression::StructInitialization(
                    self.handle_struct_literal(value, &struct_literal),
                )
            }
        }
    }

    fn handle_prefix_op(&mut self, op: &ast::PrefixOp) -> HirExpression {
        let value = self.handle_expression(&op.expr());

        let token = op.op().to_token();
        let prefix_op = token.kind.prefix_operator().unwrap();
        let hir_operator = match prefix_op {
            PrefixOperator::Minus => HirPrefixOperator::Minus,
            PrefixOperator::Not => HirPrefixOperator::Not,
        };
        HirExpression::UnaryOperation {
            operation: HirPrefix {
                operator: hir_operator,
                span: self.span(token),
            },
            value: Box::new(value),
        }
    }

    fn handle_struct(&mut self, strukt: &ast::Struct) -> HirStruct {
        let span = self.item_span(strukt);
        let ident = self.span(strukt.ident().unwrap().to_token()).into();

        let properties = strukt.variables().map_or_else(Vec::new, |variables| {
            variables
                .vars()
                .map(|property| {
                    let span = self.item_span(&property);
                    let ident = self.span(property.ident().to_token()).into();
                    let datatype = HirTypePattern::Path(IdentifierPath::new(
                        property
                            .typ()
                            .segments()
                            .map(|segment| self.span(segment.to_token()).into())
                            .collect(),
                    ));
                    HirPropertyDeclaration {
                        span,
                        ident,
                        datatype,
                    }
                })
                .collect()
        });

        let objects = strukt
            .items()
            .map(|item| HirObject::Function(self.handle_function(&item)))
            .collect();

        HirStruct {
            span,
            ident,
            attributes: Vec::new(),
            properties,
            objects,
        }
    }

    fn handle_struct_literal(
        &mut self,
        base: HirExpression,
        struct_lit: &ast::StructLiteral,
    ) -> HirStructInitialization {
        let span = self.item_span(struct_lit);
        let values = struct_lit
            .items()
            .map(|item| {
                (
                    self.span(item.ident().to_token()).into(),
                    self.handle_expression(&item.expr()),
                )
            })
            .collect();

        HirStructInitialization {
            span,
            base: Box::new(base),
            values,
        }
    }

    fn handle_type_pattern(&mut self, pattern: &ast::Pattern) -> HirTypePattern {
        let span = self.item_span(pattern);
        match pattern {
            ast::Pattern::Function(func) => {
                let parameters = func
                    .parameters()
                    .items()
                    .map(|pattern| self.handle_type_pattern(&pattern))
                    .collect();
                let return_type = func
                    .ret()
                    .map(|pattern| self.handle_type_pattern(&pattern))
                    .map(Box::new);
                HirTypePattern::Function {
                    span,
                    parameters,
                    return_type,
                }
            }
            ast::Pattern::Path(path) => HirTypePattern::Path(IdentifierPath::new(
                path.segments()
                    .map(|ident| self.span(ident.to_token()).into())
                    .collect(),
            )),
            ast::Pattern::Tuple(tuple) => {
                let span = self.item_span(tuple);
                let values = tuple
                    .sub_patterns()
                    .map(|pattern| self.handle_type_pattern(&pattern))
                    .collect();
                HirTypePattern::Tuple { span, values }
            }
        }
    }

    fn handle_update(&mut self, update: &ast::Update) -> HirVariableUpdate {
        let span = self.item_span(update);
        let pattern = self.handle_pattern(&update.pattern());
        let value = Box::new(self.handle_expression(&update.expr()));

        let op_token = update.op().to_token();

        // Emit a normal variable update on normal assignment,
        // otherwise desugar `var <op>= value` to `var = var <op> value`
        let operator = match op_token.kind.assign_operator().unwrap() {
            debris_parser::token::AssignOperator::Assign => {
                return HirVariableUpdate {
                    span,
                    pattern,
                    value,
                }
            }
            debris_parser::token::AssignOperator::AssignPlus => HirInfixOperator::Plus,
            debris_parser::token::AssignOperator::AssignMinus => HirInfixOperator::Minus,
            debris_parser::token::AssignOperator::AssignTimes => HirInfixOperator::Times,
            debris_parser::token::AssignOperator::AssignDivide => HirInfixOperator::Divide,
            debris_parser::token::AssignOperator::AssignModulo => HirInfixOperator::Modulo,
        };

        let lhs = Box::new(self.convert_pattern_to_value(&pattern));
        let operation = HirInfix {
            operator,
            span: self.span(op_token),
        };
        let full_value = Box::new(HirExpression::BinaryOperation {
            operation,
            lhs,
            rhs: value,
        });

        HirVariableUpdate {
            span,
            pattern,
            value: full_value,
        }
    }

    fn convert_pattern_to_value(&self, pattern: &HirVariablePattern) -> HirExpression {
        match pattern {
            HirVariablePattern::Path(path) => HirExpression::Path(path.clone()),
            HirVariablePattern::Tuple(tuple) => {
                HirExpression::TupleInitialization(HirTupleInitialization {
                    span: pattern.span(),
                    values: tuple
                        .iter()
                        .map(|pat| self.convert_pattern_to_value(pat))
                        .collect(),
                })
            }
        }
    }

    fn handle_value(&mut self, value: &ast::Value) -> HirExpression {
        match value {
            ast::Value::Block(block) => HirExpression::Block(self.handle_block(block)),
            ast::Value::Bool(bool) => {
                let span = self.span(bool.to_token());
                let value = match bool {
                    ast::Bool::True(_) => true,
                    ast::Bool::False(_) => false,
                };
                HirExpression::Value(HirConstValue::Bool { span, value })
            }
            ast::Value::Branch(branch) => {
                HirExpression::ConditionalBranch(self.handle_branch(branch))
            }
            ast::Value::ControlFlow(control_flow) => {
                HirExpression::ControlFlow(self.handle_control_flow(control_flow))
            }
            ast::Value::FormatString(fmt_string) => {
                let span = self.item_span(fmt_string);
                let value = fmt_string
                    .components()
                    .map(|component| match component {
                        FormatStringComponent::EscapedChar(char) => {
                            let span = self.span(char);
                            let mut chars =
                                self.compile_context.input_files.get_span_str(span).chars();
                            chars.next();
                            HirFormatStringMember::String(chars.as_str().into())
                        }
                        FormatStringComponent::StringInner(string) => {
                            let span = self.span(string);
                            let value = self.compile_context.input_files.get_span_str(span);
                            HirFormatStringMember::String(value.into())
                        }
                        FormatStringComponent::Path(path) => {
                            let expr = self.handle_path(&path);
                            HirFormatStringMember::Variable(Box::new(HirExpression::Path(expr)))
                        }
                    })
                    .collect();
                HirExpression::Value(HirConstValue::FormatString { span, value })
            }
            ast::Value::Function(function) => {
                HirExpression::Function(self.handle_function(function))
            }
            ast::Value::Ident(ident) => {
                let span = self.span(ident.to_token());
                HirExpression::Variable(SpannedIdentifier::new(span))
            }
            ast::Value::Int(int) => {
                let span = self.span(int.to_token());
                let int_str = self.compile_context.input_files.get_span_str(span);
                let value = match int_str.parse() {
                    Ok(value) => value,
                    Err(error) => {
                        self.errors
                            .push(ParseError::InvalidIntLiteral { span, error });
                        0
                    }
                };
                HirExpression::Value(HirConstValue::Integer { span, value })
            }
            ast::Value::InfLoop(inf_loop) => {
                HirExpression::InfiniteLoop(self.handle_infinite_loop(inf_loop))
            }
            ast::Value::ParenthesisValue(value) => self.handle_expression(&value.expr()),
            ast::Value::String(string) => {
                let span = self.span(string.to_token());
                let value = self.compile_context.input_files.get_span_str(span);
                // Remove the surrounding "" characters
                let value = {
                    let mut chars = value.chars();
                    chars.next();
                    chars.next_back();
                    chars.as_str().into()
                };
                HirExpression::Value(HirConstValue::String { span, value })
            }
            ast::Value::Tuple(tuple) => {
                let span = self.item_span(tuple);
                let values = tuple
                    .items()
                    .map(|item| self.handle_expression(&item))
                    .collect();
                HirExpression::TupleInitialization(HirTupleInitialization { span, values })
            }
            ast::Value::WhileLoop(while_loop) => {
                HirExpression::InfiniteLoop(self.handle_while_loop(while_loop))
            }
        }
    }

    /// Handles while loops by desugaring them into normal loops with break:
    /// ```debris
    /// while condition { block };
    /// ```
    /// Gets turned into
    /// ```debris
    /// loop {
    ///     if condition { block }
    ///     else { break; }
    /// }
    /// ```
    fn handle_while_loop(&mut self, while_loop: &ast::WhileLoop) -> HirInfiniteLoop {
        let span = self.item_span(while_loop);
        let while_loop_block = self.handle_block(&while_loop.block());

        let condition_statement = HirStatement::ConditionalBranch(HirConditionalBranch {
            span,
            is_comptime: false, // TODO: maybe add comptime while
            condition: Box::new(self.handle_expression(&while_loop.condition())),
            block_positive: Box::new(while_loop_block),
            block_negative: Some(Box::new(HirBlock {
                span,
                objects: Vec::new(),
                statements: vec![HirStatement::ControlFlow(HirControlFlow {
                    expression: None,
                    kind: HirControlKind::Break,
                    span: self.item_span(&while_loop.condition()),
                })],
                return_value: None,
            })),
        });

        let block = Box::new(HirBlock {
            objects: Vec::new(),
            statements: vec![condition_statement],
            span,
            return_value: None,
        });
        HirInfiniteLoop { span, block }
    }

    fn maybe_add_name_to_expression(
        mut expr: HirExpression,
        ident: SpannedIdentifier,
    ) -> HirExpression {
        match &mut expr {
            HirExpression::Function(function) if function.ident.is_unknown() => {
                function.ident = HirFunctionName::Ident(ident);
            }
            _ => {}
        }
        expr
    }
}
