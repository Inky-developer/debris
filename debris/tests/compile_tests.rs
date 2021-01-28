use std::path::{Path, PathBuf};

use debris_core::{
    error::{AsAnnotationSnippet, CompileError, LangErrorKind},
    llir::Llir,
};

use debris_core::error::Result;
use debris_lang::{get_std_module, CompileConfig};

fn get_llir(config: &mut CompileConfig) -> Result<Llir> {
    let hir = config.get_hir(0)?;
    let mut mir = config.get_mir(&hir)?;
    config.get_llir(&mir.contexts, &mut mir.namespaces)
}

fn get_llir_and_config(file: PathBuf, root: PathBuf) -> (Result<Llir>, CompileConfig) {
    let mut config = CompileConfig::new(file, get_std_module().into(), root);
    (get_llir(&mut config), config)
}

macro_rules! expect_error {
    ($file:literal, $error:pat) => {{
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
    expect_error!("var_already_defined_a.de", LangErrorKind::VariableAlreadyDefined {..});
    expect_error!("var_already_defined_b.de", LangErrorKind::VariableAlreadyDefined {..});

    expect_error!("unexpected_type_function.de", LangErrorKind::UnexpectedType {..});
    expect_error!("unexpected_type_return_type.de", LangErrorKind::UnexpectedType {..});
    expect_error!("unexpected_type_condition.de", LangErrorKind::UnexpectedType {..});
    expect_error!("unexpected_type_else_a.de", LangErrorKind::UnexpectedType {..});
    expect_error!("unexpected_type_else_b.de", LangErrorKind::UnexpectedType {..});
    expect_error!("unexpected_type_no_else.de", LangErrorKind::UnexpectedType {..});

    expect_error!("unexpected_pattern.de", LangErrorKind::UnexpectedPattern {..});

    expect_error!("unexpected_overload_builtin.de", LangErrorKind::UnexpectedOverload {..});
    expect_error!("unexpected_overload_native.de", LangErrorKind::UnexpectedOverload {..});

    expect_error!("missing_variable.de", LangErrorKind::MissingVariable {..});

    expect_error!("missing_property.de", LangErrorKind::MissingProperty {..});

    expect_error!("unexpected_operator.de", LangErrorKind::UnexpectedOperator {..});

    expect_error!("unpromotable_type.de", LangErrorKind::UnpromotableType {..});

    expect_error!("missing_module.de", LangErrorKind::MissingModule {..});

    expect_error!("circular_import_a.de", LangErrorKind::CircularImport {..});

    expect_error!("not_yet_implemented_type_path.de", LangErrorKind::NotYetImplemented {..});
    expect_error!("not_yet_implemented_recursive_call.de", LangErrorKind::NotYetImplemented {..});
}
