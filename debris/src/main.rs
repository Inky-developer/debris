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

use std::{fs::read_to_string, path::Path, process};

use debris_backends::{Backend, DatapackBackend};

use debris_core::{error::Result, llir::Llir, mir::Mir, objects::ModuleFactory};
use debris_lang::CompileConfig;
use mc_utils::rcon::McRcon;

/// Loads the extern modules (for now only std)
fn get_extern_modules() -> [ModuleFactory; 1] {
    [ModuleFactory::new(&debris_std::load, true)]
}

/// Compiles the file `test.txt` into llir
pub fn debug_run(compiler: &CompileConfig) -> Result<Llir> {
    let ast = compiler.get_hir()?;
    println!("{:?}", ast);
    println!("---------\n\n");

    let Mir {
        contexts,
        mut namespaces,
    } = compiler.get_mir(&ast)?;
    // for value in mir.iter() {
    //     println!("{:?}", value.contexts[0].values);
    // }
    println!("{:?}", contexts);
    println!("---------\n\n");

    let llir = compiler.get_llir(&contexts, &mut namespaces)?;
    println!("{:#?}", llir);
    println!();

    Ok(llir)
}

fn main() {
    let compiler = CompileConfig::new("test.txt", get_extern_modules().into());
    process::exit(match debug_run(&compiler).as_ref() {
        Ok(llir) => {
            let result = DatapackBackend::generate(&llir, &compiler.compile_context);
            // println!("{:#?}", result);

            // This file should contains one line with the path to the output directory
            let config_file = read_to_string("debug.config")
                .expect("debug.config file is missing at the directory root!");

            result
                .persist("temp_pack", Path::new(config_file.trim()))
                .expect("Could not persist");

            let rcon = McRcon::new(("localhost", 25575), "1234".to_string());

            match rcon {
                Ok(mut rcon) => {
                    rcon.command("reload").expect("Could not reload");
                    println!("Reloaded!");
                }
                Err(err) => println!("Could not connect to the server: {}", err),
            }

            0
        }
        Err(err) => {
            println!("{}", err.format(&compiler.compile_context));
            1
        }
    })
}
