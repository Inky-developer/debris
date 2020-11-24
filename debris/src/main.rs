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

use std::{path::Path, process};

use debris_backends::{Backend, DatapackBackend};

use debris_core::{error::Result, llir::Llir, objects::ModuleFactory};
use debris_lang::CompileConfig;

/// Loads the extern modules (for now only std)
fn get_extern_modules() -> [ModuleFactory; 1] {
    [(&debris_std::load).into()]
}

/// Compiles the file `test.txt` into llir
pub fn debug_run() -> Result<Llir> {
    let compiler = CompileConfig::new("test.txt", get_extern_modules().into());

    let ast = compiler.get_hir()?;
    println!("{:?}", ast);
    println!("---------\n\n");

    let mir = compiler.get_mir(&ast)?;
    // for value in mir.iter() {
    //     println!("{:?}", value.contexts[0].values);
    // }
    println!("{:?}", mir);
    println!("---------\n\n");

    let llir = compiler.get_llir(&mir)?;
    println!("{:?}", llir);
    println!();

    Ok(llir)
}

fn main() {
    process::exit(match debug_run().as_ref() {
        Ok(llir) => {
            let result = DatapackBackend::generate(&llir);
            // println!("{:#?}", result);

            result
                .persist("temp_pack", Path::new(""))
                .expect("Could not persist");
            0
        }
        Err(err) => {
            err.with_display_list(|dl| println!("{}", dl));
            1
        }
    })
}
