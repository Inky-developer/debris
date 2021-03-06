//! High-level intermediate representation
//!
//! Parses debris code into a hir.
//! This intermediate representation is very similar to a typical abstract syntax tree,
//! but the some desugaring gets applied.

// Oof. Seems like the only way to not get clippy complaining about the generated rules enum.
#![allow(clippy::upper_case_acronyms)]

mod hir_impl;
pub mod hir_nodes;

mod hir_context;
use debris_common::{CodeId, Ident, Span};
pub use hir_context::HirContext;

use hir_nodes::{HirBlock, HirModule};

mod identifier;
pub use identifier::{IdentifierPath, SpannedIdentifier};

pub use hir_impl::HirFile;
use indexmap::IndexSet;

/// The pest parser which can parse the grammar file
#[derive(Parser)]
#[grammar = "hir/grammar.pest"]
pub struct DebrisParser;

/// The hir representation of an input file and all of its dependencies
#[derive(Debug)]
pub struct Hir {
    pub main_function: HirBlock,
    pub code_id: CodeId,
    pub imported_modules: Vec<HirModule>,
}

/// Keeps track of all imported modules, uses indexes as keys
#[derive(Debug, Default)]
pub struct ImportDependencies {
    modules: IndexSet<Ident>,
    /// The spans that correspond to the modules.
    /// Access via the index of the module
    spans: Vec<Span>,
}

impl ImportDependencies {
    /// Inserts a dependency and the code span and returns its index
    pub fn insert(&mut self, value: Ident, span: Span) -> usize {
        let (index, inserted) = self.modules.insert_full(value);

        // If the module is already listed,
        // ignore the span of the second import
        if inserted {
            self.spans.push(span)
        }

        index
    }

    pub fn len(&self) -> usize {
        self.modules.len()
    }

    pub fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }

    pub fn get(&self, index: usize) -> (&Ident, Span) {
        (&self.modules[index], self.spans[index])
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Ident, Span)> {
        self.modules
            .iter()
            .enumerate()
            .map(move |(index, ident)| (ident, self.spans[index]))
    }
}

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
            "let a = (5);",
            r#"let a = "Hello World";"#,
            "let (a, b) = c;",
            "let (a, (b, (c, d))) = (1, (2, (3, 4)));",
            "(a, b, c) = (c, b, a);",
            "a = a * 2;",
            "a.b.c.f = 8;",
            // operations
            "let a = 1 + 4 * e / (z % -8);",
            // functions
            "function();",
            "function(1, 2, 3);",
            "function (1,  2,   3,);",
            "module.function();",
            "(-5).abs();",
            // blocks
            "let a = {1};",
            "let a = {print(1); 2};",
            "{let a = {1};}",
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
            // structs
            "struct Foo {}",
            "struct Foo {Foo: Bar, Baz: Whatever}",
            "struct Foo{Bar: TrailingComma,}",
            "let a = MyStruct{};",
            // imports
            "import my_module;",
            // branches
            "if a {stuff();}",
            "let y = if a {b} else {c};",
            "let a = if a { print(0) } else if b { print(2) } else { print(3) };",
            // Control flow
            "return;",
            "return {5};",
            "break;",
            "continue;",
            // loops
            "loop {}",
            "loop {break;}",
        ];

        for test_case in test_cases.iter() {
            assert!(
                DebrisParser::parse(Rule::program, test_case).is_ok(),
                "Could not parse: '{}'",
                test_case
            )
        }
    }

    #[test]
    fn test_not_parses() {
        let test_cases = [
            "ö",
            "let mod = keyword",
            "let a = ;",
            "let a = -;",
            "let 1 = 0;",
            // "let a.b = c;",
            // operations
            "let a = a -;",
            "let a = 1 + 2 +;",
            "let a = ((a+2);",
            // functions
            "function(;",
            // blocks
            "1",
            "{};",
            "let a = {1}",
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
            // structs
            "structFoo{}",
            "struct Foo {bar:}",
            "struct Foo {:baz}",
            "struct Foo {foo:bar,,}",
            // imports
            "import;",
            "import ;",
            "importstuff;",
            // branches
            "if1+1{};",
            "if trueand false {};",
            // Control flow
            "return",
            "returntrue;",
        ];

        for test_case in test_cases.iter() {
            assert!(
                DebrisParser::parse(Rule::program, test_case).is_err(),
                "Parsed invalid syntax: '{}'",
                test_case
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
