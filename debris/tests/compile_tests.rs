use std::{
    fs,
    path::{Path, PathBuf},
};

use debris_core::{
    error::{AsAnnotationSnippet, CompileError, LangErrorKind},
    llir::Llir,
};

use debris_core::error::Result;
use debris_lang::CompileConfig;

fn get_llir(config: &mut CompileConfig) -> Result<Llir> {
    let hir = config.compute_hir(0)?;
    let mir = config.compute_mir(&hir)?;
    config.compute_llir(&mir)
}

fn get_llir_and_config(file: PathBuf, root: PathBuf) -> (Result<Llir>, CompileConfig) {
    let mut config = CompileConfig::new(debris_std::load_all, root);
    config.add_relative_file(file);
    (get_llir(&mut config), config)
}

macro_rules! expect_error {
    ($file:literal, $error:pat) => {{
        println!("testing {}", $file);
        let (result, config) = get_llir_and_config(
            $file.into(),
            Path::new("tests/compile_test_fail").to_path_buf(),
        );
        match result {
            Ok(_) => panic!("Expected {} but compiled successfully", stringify!($error)),
            Err(CompileError::LangError(lang_err)) => {
                if !matches!(lang_err.kind, $error) {
                    panic!(
                        "Expected {} but got:\n{}",
                        stringify!($error),
                        AsAnnotationSnippet::to_string(&lang_err, &config.compile_context)
                    );
                }
            }
            Err(other) => unreachable!("{}", other.format(&config.compile_context)),
        }
    }};
}

#[test]
fn test_compile_fails() {
    expect_error!(
        "var_already_defined_a.de",
        LangErrorKind::VariableAlreadyDefined { .. }
    );
    expect_error!(
        "var_already_defined_b.de",
        LangErrorKind::VariableAlreadyDefined { .. }
    );

    expect_error!(
        "unexpected_path_assignment.de",
        LangErrorKind::UnexpectedPathAssignment { .. }
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
        LangErrorKind::ExpectedBoolean { .. }
    );
    expect_error!(
        "unexpected_type_else_a.de",
        LangErrorKind::UnexpectedType { .. }
    );
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

    expect_error!(
        "missing_variable_update.de",
        LangErrorKind::MissingVariable { .. }
    );

    expect_error!("missing_property.de", LangErrorKind::MissingProperty { .. });

    expect_error!(
        "unexpected_operator.de",
        LangErrorKind::UnexpectedOperator { .. }
    );

    expect_error!(
        "unpromotable_type.de",
        LangErrorKind::UnpromotableType { .. }
    );

    expect_error!("missing_module.de", LangErrorKind::MissingModule { .. });

    expect_error!("circular_import_a.de", LangErrorKind::CircularImport { .. });

    expect_error!("const_variable.de", LangErrorKind::ConstVariable { .. });

    expect_error!(
        "comptime_variable.de",
        LangErrorKind::ComptimeVariable { .. }
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
        LangErrorKind::UnexpectedType { .. }
    );

    expect_error!(
        "invalid_return.de",
        LangErrorKind::InvalidControlFlow { .. }
    );

    expect_error!(
        "loop_continue_value.de",
        LangErrorKind::UnexpectedType { .. }
    );

    expect_error!("unreachable_code_a.de", LangErrorKind::UnreachableCode);
}

fn compile(path: PathBuf) {
    let (result, config) = get_llir_and_config(path, ".".into());
    if let Err(err) = result {
        panic!(
            "Test did not compile:\n{}",
            err.format(&config.compile_context)
        );
    }
}

#[test]
fn test_compile_succeeds() {
    for file in fs::read_dir("tests/compile_test_succeed").unwrap() {
        let file = file.unwrap();
        if file.file_type().unwrap().is_file() {
            println!("Testing {}", file.path().canonicalize().unwrap().display());
            compile(file.path());
        }
    }
}
