use std::ops::ControlFlow;

use crate::ast;

pub trait AstVisitor {
    #[must_use]
    fn visit_assignment(&self, _assignment: &ast::Assignment) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_expression(&self, _expression: &ast::Expression) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_ident(&self, _ident: &ast::Ident) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_infix_op(&self, _infix_op: &ast::InfixOp) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_int(&self, _int: &ast::Int) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_operator(&self, _operator: &ast::Operator) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_pattern(&self, _pattern: &ast::Pattern) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_parenthesis_value(&self, _value: &ast::ParenthesisValue) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_program(&self, _program: &ast::Program) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_statement(&self, _statement: &ast::Statement) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_value(&self, _value: &ast::Value) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
}

// pub fn visit_ast(visitor: &AstVisitor, ast: &Ast) {
//     visit_program(visitor, &ast.program);
// }

// fn visit_program(visitor: &AstVisitor, program: &Program) {
//     for statement in program.statements() {
//         visit_statement(visitor, &statement)
//     }
// }

// fn visit_statement(visitor: &AstVisitor, statement: &Statement) {
//     match statement {
//         Statement::Assignment(assignment) => visit_assignment(assignment),
//     }
// }

// fn visit_assignment(visitor: &AstVisitor, assignment: &Assignment) {
//     let ident = assignment.ident();
//     visit_ident(&ident);

//     let value = assignment.value();
//     visit_expression(&value);
// }

// fn visit_expression(visitor: &AstVisitor, expression: &Expression) {
//     match expression {
//         Expression::InfixOp(infix_op) => visit_infix_op(infix_op),
//         Expression::Value(value) => visit_value(value),
//     }
// }

// fn visit_infix_op(visitor: &AstVisitor, infix_op: &InfixOp) {
//     let lhs = infix_op.left();
//     visit_expression(&lhs);

//     let rhs = infix_op.right();
//     if let Some(rhs) = rhs {
//         visit_expression(&rhs);
//     }

//     let op = infix_op.operator();
//     if let Some(op) = op {
//         visit_operator(&op);
//     }
// }

// fn visit_value(visitor: &AstVisitor, value: &Value) {
//     match value {
//         Value::Ident(ident) => visit_ident(ident),
//         Value::Int(int) => visit_int(int),
//     }
// }

// fn visit_ident(visitor: &AstVisitor, ident: &Ident) {
//     let _ = ident;
// }

// fn visit_int(visitor: &AstVisitor, int: &Int) {
//     let _ = int;
// }

// fn visit_operator(visitor: &AstVisitor, op: &Operator) {
//     let _ = op;
// }
