use std::ops::ControlFlow;

use crate::ast;

pub type AstFlow = ControlFlow<()>;

pub trait AstVisitor {
    #[must_use]
    fn visit_assignment(&mut self, #[allow(unused)] assignment: &ast::Assignment) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_assign_mode(&mut self, #[allow(unused)] mode: &ast::AssignMode) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_assign_operator(&mut self, #[allow(unused)] op: &ast::AssignOperator) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_attribute_list(&mut self, #[allow(unused)] list: &ast::AttributeList) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_block(&mut self, #[allow(unused)] block: &ast::Block) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_bool(&mut self, #[allow(unused)] bool: &ast::Bool) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_control_flow(&mut self, #[allow(unused)] op: &ast::ControlFlowOperation) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_control_flow_op(&mut self, #[allow(unused)] op: &ast::ControlFlowOperator) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_expression(&mut self, #[allow(unused)] expression: &ast::Expression) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_function_pattern(&mut self, #[allow(unused)] pat: &ast::FunctionPattern) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_inf_loop(&mut self, #[allow(unused)] inf_loop: &ast::InfLoop) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_format_string(&mut self, #[allow(unused)] fmt_string: &ast::FormatString) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_function(&mut self, #[allow(unused)] function: &ast::Function) -> AstFlow {
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
    fn visit_path(&mut self, #[allow(unused)] path: &ast::Path) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_pattern(&mut self, #[allow(unused)] pattern: &ast::Pattern) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_param_decl(&mut self, #[allow(unused)] decl: &ast::ParamDecl) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_param_list_decl(&mut self, #[allow(unused)] decl: &ast::ParamListDecl) -> AstFlow {
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
    fn visit_tuple_pattern(&mut self, #[allow(unused)] pat: &ast::TuplePattern) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_update(&mut self, #[allow(unused)] tuple: &ast::Update) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_value(&mut self, #[allow(unused)] value: &ast::Value) -> AstFlow {
        ControlFlow::Continue(())
    }
    #[must_use]
    fn visit_while_loop(&mut self, #[allow(unused)] while_loop: &ast::WhileLoop) -> AstFlow {
        ControlFlow::Continue(())
    }
}
