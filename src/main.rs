//! Experimental parser for debris
//! TODO:
//!     Mention rust-analyzer in lib.rs
//!     Add environment variable to ci for much more proptests

use debris_parser::parser::parse;

fn main() {
    let input = "let 1+b;let b = 2;";
    let result = parse(input);
    println!("{}", result.debug_fmt(input));
    println!("{}", result.to_string(input));
}
