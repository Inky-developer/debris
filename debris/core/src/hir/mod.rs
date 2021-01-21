//! High-level intermediate representation
//!
//! Parses debris code into a hir.
//! This intermediate representation is very similar to a typical abstract syntax tree,
//! but the some desugaring gets applied.

mod hir_impl;
pub mod hir_nodes;

mod hir_context;
pub use hir_context::HirContext;

mod hir_visitor;
pub use hir_visitor::HirVisitor;

mod identifier;
pub use identifier::{IdentifierPath, SpannedIdentifier};

pub use hir_impl::Hir;

/// The pest parser which can parse the grammar file
#[derive(Parser)]
#[grammar = "hir/grammar.pest"]
pub struct DebrisParser;

#[cfg(test)]
mod tests {
    use super::{DebrisParser, Rule};
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
            // blocks
            "let a = {1};",
            "let a = {print(1); 2};",
            // functions
            "fn a() {}",
            "fn askdlfjlk(param: x,) -> y {}",
            "fn baz(a: b, c: d) -> e.f {}",
            "fn a() -> fn(a) -> b {}",
            "fn a() -> fn() {}",
            "fn a() -> fn(fn(a) -> b) -> c {}",
            "[my.attribute] fn a() {}",
            "[my.attribute, my.second.attribute]fn a() {}",
            // modules
            "mod my_module {}",
            // imports
            "import my_module;",
            // branches
            "let y = if a {stuff();};",
            "let y = if a {b} else {c};",
            "let a = if a { print(0) } else if b { print(2) } else { print(3) };",
        ];

        for test_case in test_cases.iter() {
            assert!(
                DebrisParser::parse(Rule::program, test_case).is_ok(),
                format!("Could not parse: '{}'", test_case)
            )
        }
    }

    #[test]
    fn test_not_parses() {
        let test_cases = [
            "let a = ;",
            "let a = -;",
            "leta = 1;",
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
            // blocks
            "1",
            // functions
            "fn f()",
            "fntest() {}",
            "fn a.b() {}",
            "fn a(a: 1) {}",
            "fn a() -> {}",
            "fn ghgh(a: b) -> baz() {}",
            "fn a() -> fn(a: b) -> () {}",
            "fn a() -> fn(a) -> ()",
            "[function.call()]fn a() {}",
            // modules
            "mod my_module {};",
            "modmy_module {}",
            "mod {}",
            // import
            "import;",
            "import ;",
            "importstuff;",
        ];

        for test_case in test_cases.iter() {
            assert!(
                DebrisParser::parse(Rule::program, test_case).is_err(),
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
                    let result = DebrisParser::parse(Rule::expression, test);
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
                    let result = DebrisParser::parse(Rule::expression, test);
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
