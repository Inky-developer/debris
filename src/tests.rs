use std::{
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

use expect_test::expect_file;
use pretty_assertions::assert_eq;

use crate::{
    ast::Ast,
    ast_visitor::AstVisitor,
    parser::{
        parse_assignment, parse_bin_exp, parse_pattern, parse_root, parse_statement, parse_with,
        ParseResult, Parser,
    },
};

enum SyntaxKind {
    Assignment,
    Expression,
    Pattern,
    Root,
    Statement,
}

impl SyntaxKind {
    fn get_parse_fn(&self) -> &'static dyn Fn(&mut Parser) -> ParseResult<()> {
        match self {
            SyntaxKind::Assignment => &parse_assignment,
            SyntaxKind::Expression => &|parser| parse_bin_exp(parser, 0),
            SyntaxKind::Pattern => &parse_pattern,
            SyntaxKind::Root => &parse_root,
            SyntaxKind::Statement => &parse_statement,
        }
    }
}

impl FromStr for SyntaxKind {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let kind = match value {
            "assignment" => SyntaxKind::Assignment,
            "expression" => SyntaxKind::Expression,
            "pattern" => SyntaxKind::Pattern,
            "parse" => SyntaxKind::Root,
            "statement" => SyntaxKind::Statement,
            _ => return Err(()),
        };
        Ok(kind)
    }
}

struct TestCase {
    name: String,
    syntax_kind: SyntaxKind,
    de_file: PathBuf,
    ast_file: PathBuf,
}

impl TestCase {
    fn de_and_ast(&self) -> (String, String) {
        let de = std::fs::read_to_string(&self.de_file).unwrap();
        let ast = std::fs::read_to_string(&self.ast_file).unwrap();
        (de, ast)
    }
}

fn list_cases(path: impl AsRef<Path>) -> Vec<TestCase> {
    let mut cases = Vec::new();

    for entry in std::fs::read_dir(path).unwrap() {
        let entry = entry.unwrap();
        if entry.metadata().unwrap().is_file() && entry.path().extension().unwrap() == "de" {
            let name = entry
                .path()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .split_once(".")
                .unwrap()
                .0
                .to_string();
            let mut ast_file = entry.path();
            ast_file.set_extension("ast");
            let syntax_kind = name.split_once("_").unwrap().0.parse().unwrap();
            cases.push(TestCase {
                name,
                ast_file,
                de_file: entry.path(),
                syntax_kind,
            });
        }
    }

    cases
}

fn test_dir(path: impl AsRef<Path>, should_error: bool) {
    struct Visitor;
    impl AstVisitor for Visitor {}

    for case in list_cases(path) {
        eprint!("Testing {}... ", &case.name);
        let (de, _) = case.de_and_ast();
        let parse_fn = case.syntax_kind.get_parse_fn();
        let syntax_tree = parse_with(&de, parse_fn);

        let ast_str = syntax_tree.debug_fmt(&de).to_string();
        let expected = expect_file![case.ast_file.canonicalize().unwrap()];
        expected.assert_eq(&ast_str);

        let roundtrip_str = syntax_tree.to_string(&de);
        assert_eq!(
            de,
            roundtrip_str.as_ref(),
            "Ast to string conversion not lossless"
        );

        assert_eq!(!syntax_tree.errors.is_empty(), should_error, "Unexpected error/no-error");

        // Test that the higher level api can be used to visit the entire syntax tree without panicking
        if syntax_tree.errors.is_empty() && matches!(case.syntax_kind, SyntaxKind::Root) {
            let ast = Ast::from(Rc::new(syntax_tree));
            ast.visit(&Visitor);
        }

        eprintln!("Ok!");
    }
}

#[test]
fn test_valid() {
    test_dir("testcases/valid", false);
}

#[test]
fn test_unit_tests() {
    test_dir("testcases/unit_tests", false);
}

#[test]
fn test_err() {
    test_dir("testcases/err", true);
}
