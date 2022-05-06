use std::rc::Rc;

use debris_parser::{ast::Ast, ast_visitor::AstVisitor, parser::parse};

struct Visitor;
impl AstVisitor for Visitor {}

fn main() {
    let input = "[a, b, c]fn foo(A: B) {}";
    let st = parse(input);

    {
        let tree = st.debug_fmt(input);
        println!("{tree}");
    }

    println!("Errors: {}", st.errors.len());

    let ast = Ast::from(Rc::new(st));
    ast.visit(&mut Visitor);
}
