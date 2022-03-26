use debris_common::OptMode;
use debris_llir::Runtime;

mod common;
pub use common::*;

const TICKING_PROGRAM: &str = r#"
[on_tick]
fn tick() { }
"#;

const EXPORTED_FUNCTION_PROGRAM: &str = r#"
[export("foo/bar/baz")]
fn stuff() { }
"#;

#[test]
fn test_empty_runtime() {
    let (result, _config) = compile_string("".into(), "".into(), OptMode::None);
    let llir = result.unwrap();
    let Runtime {
        scheduled_blocks,
        load_blocks,
        extern_blocks,
    } = llir.runtime;
    assert!(scheduled_blocks.is_empty());
    assert_eq!(load_blocks.len(), 1);
    assert!(extern_blocks.is_empty());
}

#[test]
fn test_ticking_function_runtime() {
    let (result, _config) = compile_string(TICKING_PROGRAM.into(), "".into(), OptMode::None);
    let llir = result.unwrap();
    assert_eq!(llir.runtime.scheduled_blocks.len(), 1);
}

#[test]
fn test_exported_function_runtime() {
    let (result, _config) =
        compile_string(EXPORTED_FUNCTION_PROGRAM.into(), "".into(), OptMode::None);
    let llir = result.unwrap();
    assert_eq!(llir.runtime.extern_blocks.len(), 1);
    assert_eq!(
        llir.runtime.extern_blocks.values().next().unwrap().as_ref(),
        "foo/bar/baz"
    );
}
