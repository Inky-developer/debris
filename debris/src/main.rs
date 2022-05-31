//! The Debris compiler.
//!
//! Currently, the compiler is in a very early state.
//! Look at [`debug_run`] to see how to compile a script.
//!
//! [`debris_std`] contains the standard library, which is implicitly imported into every script.
//!
//! The compiler is mostly backend agnostic and only generates a [low-level intermediate representation](debris_llir).
//! The crate [`debris_backends`] crate contains backend implementations that can convert this ir into files.
//! Right now, the only backend implementation that exists converts the llir code into datapacks.
//! Backends that can create command blocks or even executables might be added in the future.

use std::{env, fs::read_to_string, path::Path, process, time::Instant};

use debris_backends::{Backend, DatapackBackend};
use debris_common::{file_provider::FsFileProvider, BuildMode};
use debris_error::CompileErrors;
use debris_lang::CompileConfig;
use debris_llir::Llir;

/// Compiles the file `test.txt` into llir
// This main function is only used for debugging
#[allow(clippy::use_debug)]
pub fn debug_run(compiler: &mut CompileConfig) -> Result<Llir, CompileErrors> {
    let start_time = Instant::now();
    let high_ir = compiler.compute_hir(0)?;
    println!("Got hir in {:?}", start_time.elapsed());
    // println!("{high_ir:#?}");

    let compile_time = Instant::now();
    let mid_ir = compiler.compute_mir(&high_ir)?;
    // println!("{mid_ir:?}");
    println!("mir took {:?}", compile_time.elapsed());

    let llir = compiler.compute_llir(&mid_ir, debris_std::load_all)?;
    println!("{llir}");
    println!(
        "Compilation without backend took {:?}",
        compile_time.elapsed()
    );

    Ok(llir)
}

// This main function is only used for debugging
#[allow(clippy::use_debug)]
fn main() {
    let mut compile_config = init();

    process::exit(match debug_run(&mut compile_config) {
        Ok(llir) => {
            let backend_time = Instant::now();
            let backend = DatapackBackend;
            let result = backend.generate(&llir, &compile_config.compile_context);
            println!("Backend took another {:?}", backend_time.elapsed());

            // This file should contain one line with the path to the output directory
            let config_file = read_to_string("debug.config")
                .expect("debug.config file is missing at the directory root!");
            let dir = config_file.lines().next().expect("Invalid config file");

            result
                .persist("temp_pack", Path::new(dir))
                .expect("Could not persist");

            #[cfg(feature = "interpret")]
            {
                println!("Interpreting pack...");
                run_pack(&result);
            }

            0
        }
        Err(err) => {
            println!("{}", err.format(&compile_config.compile_context));
            1
        }
    })
}

fn init() -> CompileConfig {
    let mut compile_config = CompileConfig::new(Box::new(FsFileProvider::new("examples/".into())));
    let mut args = env::args();
    let build_mode = args
        .nth(1)
        .map_or(BuildMode::default(), |arg| match arg.as_str() {
            "release" => BuildMode::Release,
            _ => BuildMode::default(),
        });

    let file = args.next().unwrap_or_else(|| "test.de".to_string());

    compile_config
        .compile_context
        .config
        .update_build_mode(build_mode);
    // compile_config.compile_context.config.opt_mode = debris_common::OptMode::None;
    compile_config.add_file(&file);

    compile_config
}

#[cfg(feature = "interpret")]
fn run_pack(dir: &datapack_common::vfs::Directory) {
    let functions = datapack_common::functions::get_functions(dir).unwrap();

    let main_function_path = format!("{}main", DatapackBackend::FUNCTION_INTERNAL_PATH);
    let idx = functions
        .iter()
        .enumerate()
        .find(|(_, f)| f.id.path == main_function_path)
        .unwrap_or_else(|| {
            panic!("Failed to find main");
        })
        .0;

    let mut i = datapack_vm::Interpreter::new(functions, idx);

    i.run_to_end().unwrap();

    println!("{}", i.output.join("\n"));
}
