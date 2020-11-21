//! High-level intermediate representation
//!
//! Parses debris code into a hir.
//! This intermediate representation is very similar to a typical abstract syntax tree,
//! but the some desugaring gets applied.

use debris_common::LocalSpan;
use pest::Span;

mod hir_impl;
pub mod hir_nodes;

mod identifier;
pub use identifier::{IdentifierPath, SpannedIdentifier};

pub use hir_impl::Hir;

pub fn get_span(span: Span) -> LocalSpan {
    LocalSpan::new(span.start(), span.end() - span.start())
}

/// The pest parser which can parse the grammar file
#[derive(Parser)]
#[grammar = "hir/grammar.pest"]
pub struct ArithmeticParser;

#[cfg(test)]
mod tests {
    use super::{ArithmeticParser, Rule};
    use pest::Parser;

    #[test]
    fn test_parses() {
        let test_cases = [
            "let a = 1;",
            "let a = 100;",
            "let a = 001;",
            "let a = -5;",
            "let _a = 0;",
            "let a_1 = 0;",
            r#"let a = "Hello World";"#,
            // operations
            "let a = 1 + 4 * e / (z % -8);",
            // functions
            "function();",
            "function(1, 2, 3);",
            "function (1,  2,   3,);",
            "module.function();",
            // execute
            r#"let a = execute "kill @e";"#,
        ];

        for test_case in test_cases.iter() {
            assert!(
                ArithmeticParser::parse(Rule::program, test_case).is_ok(),
                format!("Could not parse: '{}'", test_case)
            )
        }
    }

    #[test]
    fn test_not_parses() {
        let test_cases = [
            "let a = ;",
            "let a = -;",
            "let 1 = 0;",
            // operations
            "let a = a -;",
            "let a = 1 + 2 +;",
            "let a = ((a+2);",
            // functions
            "function(;",
            // execute
            "execute;",
            r#"execute "Hallo, Welt";"#,
        ];

        for test_case in test_cases.iter() {
            assert!(
                ArithmeticParser::parse(Rule::program, test_case).is_err(),
                format!("Parsed invalid syntax: '{}'", test_case)
            )
        }
    }

    #[test]
    fn test_parses_int() {
        let test_cases = ["1", "500", "005", "-5"];

        for test in test_cases.iter() {
            assert!(
                {
                    let result = ArithmeticParser::parse(Rule::expression, test);
                    result.is_ok()
                        && result
                            .unwrap()
                            .next()
                            .unwrap()
                            .into_inner()
                            .next()
                            .unwrap()
                            .as_span()
                            .end()
                            == test.len()
                },
                "Did not parse integer"
            );
        }
    }

    #[test]
    fn test_parses_string() {
        let test_cases = [r#""""#, r#""Contents""#];

        for test in test_cases.iter() {
            assert!(
                {
                    let result = ArithmeticParser::parse(Rule::expression, test);
                    result.is_ok()
                        && result
                            .unwrap()
                            .next()
                            .unwrap()
                            .into_inner()
                            .next()
                            .unwrap()
                            .as_span()
                            .end()
                            == test.len()
                },
                "Did not parse string"
            )
        }
    }
}
