//! Experimental parser for debris
//! TODO:
//!     Mention rust-analyzer in lib.rs
//!     Add environment variable to ci for much more proptests

use std::rc::Rc;

use debris_parser::{parser::parse, ast_visitor::AstVisitor, ast::Ast};

struct Visitor;

impl AstVisitor for Visitor {}

fn main() {
    let input = "let (a, b) = 1 + 2 + 3;";
    let result = parse(input);
    println!("{}", result.debug_fmt(input));
    println!("{}", result.to_string(input));

    let ast = Ast::from(Rc::new(result));
    ast.visit(&Visitor);
}
