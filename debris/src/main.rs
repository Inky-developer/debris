//! The Debris compiler.
//!
//! Currently, the compiler is in a very early state.
//! Look at [`debug_run`] to see how to compile a script.
//!
//! Most of the logic of the compiler is in [debris_core].
//!
//! [debris_std] contains the standard library, which is implicitely imported into every script.
//!
//! [debris_type] contains logic for types and resolving types.
//!
//! The compiler is mostly backend agnostic and only generates a [low-level intermediate representation](debris_core::llir).
//! The crate [debris_backends] crate contains backend implementations that can convert this ir into files.
//! Right now, the only backend implementation that exists converts the llir code into datapacks.
//! Backends that can create command blocks or even executables might be added in the future.

use std::{env, fs::read_to_string, path::Path, process, time::Instant};

use debris_backends::{Backend, DatapackBackend};

use debris_core::{error::Result, llir::Llir, mir::Mir, BuildMode};
use debris_lang::{get_std_module, CompileConfig};
// use mc_utils::rcon::McRcon;

/// Compiles the file `test.txt` into llir
pub fn debug_run(compiler: &mut CompileConfig) -> Result<Llir> {
    let start_time = Instant::now();
    let ast = compiler.get_hir(0)?;
    println!("Got hir in {:?}", start_time.elapsed());
    // println!("{:?}", ast);
    // println!("---------\n\n");

    let compile_time = Instant::now();
    let Mir {
        contexts,
        mut namespaces,
    } = compiler.get_mir(&ast)?;
    // println!("{}", contexts);
    // println!("mir took {:?}", compile_time.elapsed());

    let llir = compiler.get_llir(&contexts, &mut namespaces)?;

    // println!("{}", llir);
    // println!();
    println!(
        "Compilation without backend took {:?}",
        compile_time.elapsed()
    );

    Ok(llir)
}

fn main() {
    let mut compile_config = init();

    process::exit(match debug_run(&mut compile_config).as_ref() {
        Ok(llir) => {
            let backend_time = Instant::now();
            let backend = DatapackBackend;
            let result = backend.generate(&llir, &compile_config.compile_context);
            println!("Backend took another {:?}", backend_time.elapsed());
            // println!("{:#?}", result);

            // This file should contain one line with the path to the output directory
            let config_file = read_to_string("debug.config")
                .expect("debug.config file is missing at the directory root!");

            result
                .persist("temp_pack", Path::new(config_file.trim()))
                .expect("Could not persist");

            // let rcon = McRcon::new(("localhost", 25575), "1234".to_string());

            // match rcon {
            //     Ok(mut rcon) => {
            //         rcon.command("reload").expect("Could not reload");
            //         println!("Reloaded!");
            //     }
            //     Err(err) => println!("Could not connect to the server: {}", err),
            // }

            0
        }
        Err(err) => {
            println!("{}", err.format(&compile_config.compile_context));
            1
        }
    })
}

fn init() -> CompileConfig {
    let mut compile_config = CompileConfig::new(get_std_module().into(), "examples".into());
    let mut args = env::args();
    let build_mode = args.nth(1).map_or(Default::default(), |arg| {
        if arg.eq("release") {
            BuildMode::Release
        } else {
            BuildMode::default()
        }
    });

    let file = args.next().unwrap_or_else(|| "test.de".to_string());

    compile_config
        .compile_context
        .config
        .update_build_mode(build_mode);
    compile_config.add_relative_file(file);

    compile_config
}
