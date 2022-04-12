use std::ops::ControlFlow;

use crate::ast;

pub type AstFlow = ControlFlow<()>;

pub trait AstVisitor {
    #[must_use]
    fn visit_assignment(&mut self, #[allow(unused)] assignment: &ast::Assignment) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_bool(&mut self, #[allow(unused)] bool: &ast::Bool) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_expression(&mut self, #[allow(unused)] expression: &ast::Expression) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_format_string(&mut self, #[allow(unused)] fmt_string: &ast::FormatString) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_ident(&mut self, #[allow(unused)] ident: &ast::Ident) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_infix_op(&mut self, #[allow(unused)] infix_op: &ast::InfixOp) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_int(&mut self, #[allow(unused)] int: &ast::Int) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_infix_operator(&mut self, #[allow(unused)] operator: &ast::InfixOperator) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_param_list(&mut self, #[allow(unused)] param_list: &ast::ParamList) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_pattern(&mut self, #[allow(unused)] pattern: &ast::Pattern) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_parens_value(&mut self, #[allow(unused)] value: &ast::ParensValue) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_postfix_op(&mut self, #[allow(unused)] op: &ast::PostfixOp) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_postfix_operator(&mut self, #[allow(unused)] value: &ast::PostfixOperator) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_prefix_op(&mut self, #[allow(unused)] op: &ast::PrefixOp) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_prefix_operator(&mut self, #[allow(unused)] value: &ast::PrefixOperator) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_program(&mut self, #[allow(unused)] program: &ast::Program) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_statement(&mut self, #[allow(unused)] statement: &ast::Statement) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_string(&mut self, #[allow(unused)] string: &ast::String) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_tuple(&mut self, #[allow(unused)] tuple: &ast::Tuple) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_value(&mut self, #[allow(unused)] value: &ast::Value) -> AstFlow {
        ControlFlow::Continue(())
    }
}
