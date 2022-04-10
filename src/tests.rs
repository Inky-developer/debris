use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

use pretty_assertions::assert_eq;

use crate::parser::{
    parse_bin_exp, parse_pattern, parse_statement, parse_with, ParseResult, Parser,
};

enum SyntaxKind {
    Expression,
    Pattern,
    Statement,
}

impl SyntaxKind {
    fn get_parse_fn(&self) -> &'static dyn Fn(&mut Parser) -> ParseResult<()> {
        match self {
            SyntaxKind::Expression => &|parser| parse_bin_exp(parser, 0),
            SyntaxKind::Pattern => &parse_pattern,
            SyntaxKind::Statement => &parse_statement,
        }
    }
}

impl FromStr for SyntaxKind {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let kind = match value {
            "expression" => SyntaxKind::Expression,
            "pattern" => SyntaxKind::Pattern,
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

#[test]
fn test_valid() {
    for case in list_cases("testcases/valid") {
        eprint!("Testing {}... ", &case.name);
        let (de, ast) = case.de_and_ast();
        let parse_fn = case.syntax_kind.get_parse_fn();
        let parsed_ast = parse_with(&de, parse_fn);

        let ast_str = parsed_ast.debug_fmt(&de);
        assert_eq!(ast, ast_str.to_string(), "Ast representation is not equal");

        let roundtrip_str = parsed_ast.to_string(&de);
        assert_eq!(
            de,
            roundtrip_str.as_ref(),
            "Ast to string conversion not lossless"
        );

        eprintln!("Ok!");
    }
}
