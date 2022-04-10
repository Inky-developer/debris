use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

use expect_test::expect_file;
use pretty_assertions::assert_eq;

use crate::parser::{
    parse_bin_exp, parse_pattern, parse_root, parse_statement, parse_with, ParseResult, Parser, parse_assignment,
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

fn test_dir(path: impl AsRef<Path>) {
    for case in list_cases(path) {
        eprint!("Testing {}... ", &case.name);
        let (de, _) = case.de_and_ast();
        let parse_fn = case.syntax_kind.get_parse_fn();
        let parsed_ast = parse_with(&de, parse_fn);

        let ast_str = parsed_ast.debug_fmt(&de);
        let expected = expect_file![case.ast_file.canonicalize().unwrap()];
        expected.assert_eq(&ast_str.to_string());

        let roundtrip_str = parsed_ast.to_string(&de);
        assert_eq!(
            de,
            roundtrip_str.as_ref(),
            "Ast to string conversion not lossless"
        );

        eprintln!("Ok!");
    }
}

#[test]
fn test_valid() {
    test_dir("testcases/valid");
}

#[test]
fn test_err() {
    test_dir("testcases/err");
}
