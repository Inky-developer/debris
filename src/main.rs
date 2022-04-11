//! Experimental parser for debris
//! TODO:
//!     Mention rust-analyzer in lib.rs
//!     Add environment variable to ci for much more proptests

use std::rc::Rc;

use debris_parser::{ast::Ast, ast_visitor::AstVisitor, parser::parse};

struct Visitor;

impl AstVisitor for Visitor {}

fn main() {
    let input = "let a = true;";
    let result = parse(input);
    println!("{}", result.debug_fmt(input));
    println!("{}", result.to_string(input));

    let ast = Ast::from(Rc::new(result));
    ast.visit(&mut Visitor);
}
