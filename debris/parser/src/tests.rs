use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

use expect_test::expect_file;

use crate::parser::{
    parse, parse_assignment, parse_block, parse_branch, parse_expr, parse_module, parse_pattern,
    parse_root, parse_statement, parse_struct_def, parse_with, ParseResult, Parser,
};

enum SyntaxKind {
    Assignment,
    Block,
    Branch,
    Expression,
    Module,
    Pattern,
    Root,
    Statement,
    Struct,
}

impl SyntaxKind {
    fn get_parse_fn(&self) -> &'static dyn Fn(&mut Parser) -> ParseResult<()> {
        match self {
            SyntaxKind::Assignment => &parse_assignment,
            SyntaxKind::Block => &parse_block,
            SyntaxKind::Branch => &parse_branch,
            SyntaxKind::Expression => &|parser| parse_expr(parser, 0, Default::default()),
            SyntaxKind::Module => &parse_module,
            SyntaxKind::Pattern => &|parser| parse_pattern(parser, true),
            SyntaxKind::Root => &parse_root,
            SyntaxKind::Statement => &|parser| {
                parse_statement(parser, false)?;
                Ok(())
            },
            SyntaxKind::Struct => &parse_struct_def,
        }
    }
}

impl FromStr for SyntaxKind {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let kind = match value {
            "assignment" => SyntaxKind::Assignment,
            "block" => SyntaxKind::Block,
            "branch" => SyntaxKind::Branch,
            "expression" => SyntaxKind::Expression,
            "module" => SyntaxKind::Module,
            "pattern" => SyntaxKind::Pattern,
            "parse" => SyntaxKind::Root,
            "statement" => SyntaxKind::Statement,
            "struct" => SyntaxKind::Struct,
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
                .split_once('.')
                .unwrap()
                .0
                .to_string();
            let mut ast_file = entry.path();
            ast_file.set_extension("ast");
            let syntax_kind = name.split_once('_').unwrap().0.parse().unwrap();
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

        assert_eq!(
            !syntax_tree.errors.is_empty(),
            should_error,
            "Unexpected error/no-error"
        );

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

#[test]
fn legacy_test_parses() {
    let test_cases = [
        "let a = 1;",
        "let a = 100;",
        "let a = 001;",
        "let a = -5;",
        "let _a = 0;",
        "let a_1 = 0;",
        "let a = (5);",
        r#"let a = "Hello World";"#,
        "let a = `The variable is: $some.path.to.a.variable`;",
        "let (a, b) = c;",
        "let (a, (b, (c, d))) = (1, (2, (3, 4)));",
        "(a, b, c) = (c, b, a);",
        "a = a * 2;",
        "a *= 2;",
        "a.b.c.f = 8;",
        // operations
        "let a = 1 + 4 * e / (z % -8);",
        // functions
        "function();",
        "function(1, 2, 3);",
        "function (1,  2,   3,);",
        "module.function();",
        "-5.abs();",
        "(1 - 8).abs();",
        "foo.bar().baz().ok();",
        // "a.b() {};",
        "let my_func = fn() {};",
        "comptime my_func = fn () {1};",
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
        "[function.call()]fn a() {}",
        "[1]fn a() {}",
        "[a.b().c]fn a() {}",
        "[aa()]fn a() {}",
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
        "comptime if a {}",
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
        "let a = loop {};",
        "while some_expression() {}",
        "while true { do_stuff(); }",
        "let a = while false {};",
    ];

    for test_case in test_cases {
        print!("Parsing {test_case}... ");
        let syntax_tree = parse(test_case);
        assert!(syntax_tree.errors.is_empty(), "Could not parse input");

        let roundtrip_str = syntax_tree.to_string(test_case);
        assert_eq!(
            test_case,
            roundtrip_str.as_ref(),
            "Ast to string conversion not lossless"
        );
        println!("Ok!");
    }
}

#[test]
fn legacy_test_not_parses() {
    let test_cases = [
        "ö",
        // "let mod = keyword;",
        "let a = ;",
        "let a = -;",
        "let 1 = 0;",
        // "let a = `${1 + 1}`;", TODO: enable
        "let a.b = c;",
        // operations
        "let a = a -;",
        "let a = 1 + 2 +;",
        "let a = ((a+2);",
        "let a += 2;",
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
        "fn () {}",
        "comptime foo = fn {};",
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
        // branches
        "comptimeif a {}",
        "if trueand false {}",
        // loops
        "while Foo {a: 1} { do_stuff(); }",
    ];

    for test_case in test_cases {
        print!("Parsing {test_case}... ");
        let syntax_tree = parse(test_case);
        assert!(!syntax_tree.errors.is_empty(), "parsed invalid input");

        let roundtrip_str = syntax_tree.to_string(test_case);
        assert_eq!(
            test_case,
            roundtrip_str.as_ref(),
            "Ast to string conversion not lossless"
        );
        println!("Ok!");
    }
}
