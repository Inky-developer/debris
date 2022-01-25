use std::{
    fs::{self, File},
    io::Read,
    path::{Path, PathBuf},
};

use debris_error::{AsAnnotationSnippet, CompileError, LangErrorKind};

mod common;
pub use common::*;

macro_rules! expect_error {
    ($file:literal, $error:pat) => {{
        println!("testing {}", $file);
        let panic_result = std::panic::catch_unwind(|| {
            compile_file(
                $file.into(),
                Path::new("tests/compile_test_fail").to_path_buf(),
            )
        });
        let (result, config) = match panic_result {
            Ok(result) => result,
            Err(_) => panic!("ICE occurred. See message above"),
        };
        match result {
            Ok(_) => panic!("Expected {} but compiled successfully", stringify!($error)),
            Err(CompileError::LangError(lang_err)) => {
                assert!(
                    matches!(lang_err.kind, $error),
                    "Expected {} but got:\n{}",
                    stringify!($error),
                    AsAnnotationSnippet::to_string(&lang_err, &config.compile_context)
                );
            }
            Err(other) => unreachable!("{}", other.format(&config.compile_context)),
        }
    }};
}

#[test]
fn test_compile_fails() {
    expect_error!(
        "builtin_too_many_parameters.de",
        LangErrorKind::UnexpectedOverload { .. }
    );

    expect_error!(
        "unexpected_path_assignment.de",
        LangErrorKind::UnexpectedProperty { .. }
    );

    // Type attributes are temporarily ignored
    // expect_error!(
    //     "unexpected_type_attribute_a.de",
    //     LangErrorKind::UnexpectedType { .. }
    // );
    // expect_error!(
    //     "unexpected_type_attribute_b.de",
    //     LangErrorKind::UnexpectedType { .. }
    // );

    expect_error!(
        "unexpected_type_function_b.de",
        LangErrorKind::UnexpectedType { .. }
    );
    expect_error!(
        "unexpected_type_return_type.de",
        LangErrorKind::UnexpectedType { .. }
    );
    expect_error!(
        "unexpected_type_condition.de",
        LangErrorKind::UnexpectedType { .. }
    );
    // // Unused because comptime if statements can only evaluate one branch right now.
    // expect_error!(
    //     "unexpected_type_else_a.de",
    //     LangErrorKind::UnexpectedType { .. }
    // );
    expect_error!(
        "unexpected_type_else_b.de",
        LangErrorKind::UnexpectedType { .. }
    );
    expect_error!(
        "unexpected_type_no_else.de",
        LangErrorKind::UnexpectedType { .. }
    );
    expect_error!(
        "unexpected_type_parameter_type.de",
        LangErrorKind::UnexpectedType { .. }
    );
    expect_error!(
        "unexpected_type_struct_initialization.de",
        LangErrorKind::UnexpectedType { .. }
    );
    expect_error!(
        "unexpected_type_if.de",
        LangErrorKind::ComptimeUpdate { .. }
    );

    expect_error!(
        "struct_instantiation_a.de",
        LangErrorKind::UnexpectedType { .. }
    );
    expect_error!(
        "struct_instantiation_b.de",
        LangErrorKind::UnexpectedType { .. }
    );
    expect_error!(
        "struct_instantiation_incomplete_a.de",
        LangErrorKind::UnexpectedStructInitializer { .. }
    );
    expect_error!(
        "struct_instantiation_incomplete_b.de",
        LangErrorKind::MissingStructInitializer { .. }
    );

    expect_error!(
        "unexpected_pattern.de",
        LangErrorKind::MissingVariable { .. }
    );
    expect_error!(
        "unexpected_return_value_branch.de",
        LangErrorKind::UnexpectedType { .. }
    );
    expect_error!(
        "unexpected_overload_builtin.de",
        LangErrorKind::UnexpectedOverload { .. }
    );
    expect_error!(
        "unexpected_overload_struct.de",
        LangErrorKind::UnexpectedOverload { .. }
    );
    expect_error!(
        "unexpected_overload_native.de",
        LangErrorKind::UnexpectedOverload { .. }
    );

    expect_error!("missing_variable.de", LangErrorKind::MissingVariable { .. });

    expect_error!("module_return.de", LangErrorKind::UnexpectedType { .. });

    expect_error!(
        "missing_variable_update.de",
        LangErrorKind::MissingVariable { .. }
    );

    expect_error!(
        "missing_property.de",
        LangErrorKind::UnexpectedProperty { .. }
    );

    expect_error!(
        "unexpected_operator.de",
        LangErrorKind::UnexpectedProperty { .. }
    );

    expect_error!("missing_module.de", LangErrorKind::MissingModule { .. });

    expect_error!("circular_import_a.de", LangErrorKind::CircularImport { .. });

    // ToDo: Decide whether this should actually be an error
    // expect_error!("const_variable.de", LangErrorKind::ConstVariable { .. });

    expect_error!(
        "function_attribute.de",
        LangErrorKind::MissingVariable { .. }
    );

    expect_error!(
        "fuzz_promote_type.de",
        LangErrorKind::UnexpectedOverload { .. }
    );

    expect_error!(
        "invalid_code_in_unreachable_branch.de",
        LangErrorKind::UnexpectedOverload { .. }
    );

    expect_error!(
        "invalid_comptime_branch.de",
        LangErrorKind::InvalidComptimeBranch { .. }
    );
    expect_error!("comptime_variable.de", LangErrorKind::ComptimeUpdate { .. });

    expect_error!(
        "invalid_export_a.de",
        LangErrorKind::InvalidExternItemPath { .. }
    );
    expect_error!(
        "invalid_export_b.de",
        LangErrorKind::InvalidExternItemPath { .. }
    );
    expect_error!(
        "invalid_export_c.de",
        LangErrorKind::InvalidExternItemPath { .. }
    );
    expect_error!(
        "invalid_export_multiple.de",
        LangErrorKind::FunctionAlreadyExported
    );

    expect_error!(
        "non_comptime_declaration.de",
        LangErrorKind::NonComptimeVariable { .. }
    );

    expect_error!(
        "not_yet_implemented_recursive_call.de",
        LangErrorKind::NotYetImplemented { .. }
    );

    expect_error!("pattern_mismatch_a.de", LangErrorKind::TupleMismatch { .. });
    expect_error!("pattern_mismatch_b.de", LangErrorKind::TupleMismatch { .. });

    expect_error!(
        "register_ticking_function_a.de",
        LangErrorKind::UnexpectedOverload { .. }
    );

    expect_error!(
        "register_ticking_function_b.de",
        LangErrorKind::UnexpectedOverload { .. }
    );

    expect_error!(
        "invalid_return.de",
        LangErrorKind::InvalidControlFlow { .. }
    );

    expect_error!(
        "loop_continue_value.de",
        LangErrorKind::ContinueWithValue { .. }
    );

    expect_error!("missing_function.de", LangErrorKind::MissingVariable { .. });

    expect_error!("unreachable_code_a.de", LangErrorKind::UnreachableCode);
}

fn compile(path: PathBuf) {
    let (result, config) = compile_file(path, ".".into());
    if let Err(err) = result {
        panic!(
            "Test did not compile:\n{}",
            err.format(&config.compile_context)
        );
    }
}

// TODO: Fix skip tests
#[test]
fn test_compile_succeeds() {
    for file in fs::read_dir("tests/compile_test_succeed").unwrap() {
        let file = file.unwrap();
        if file.file_type().unwrap().is_file() {
            let mut buf = String::new();
            File::open(file.path())
                .unwrap()
                .read_to_string(&mut buf)
                .unwrap();
            if buf.starts_with("!!! SKIP") {
                println!("Skipping {}", file.path().canonicalize().unwrap().display());
                continue;
            }
            println!("Testing {}", file.path().canonicalize().unwrap().display());
            compile(file.path());
        }
    }
}
