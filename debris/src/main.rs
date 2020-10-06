use std::{path::Path, process, rc::Rc};

use debris_backends::{Backend, DatapackBackend};
use debris_common::InputFile;
use debris_core::{
    error::Result, hir::HirParser, llir::LLIRParser, llir::LLIR, mir::MirParser,
    objects::ModuleFactory, CompileContext, DatabaseStruct, Inputs,
};

/// Loads the extern modules (for now only std)
fn get_extern_modules() -> Vec<ModuleFactory> {
    vec![(&debris_std::load).into()]
}

fn debug_run() -> Rc<Result<LLIR>> {
    let mut db = DatabaseStruct::default();

    let compile_context = Rc::new(CompileContext::default());

    db.set_input_file(InputFile::Main, "test.txt".into());
    db.set_compile_context(compile_context.clone());
    db.set_extern_modules(Rc::new(get_extern_modules()));

    let ast = db.parse(InputFile::Main);
    println!("{:?}", ast);
    println!("---------\n\n");

    let mir = db.parse_hir_global(InputFile::Main);
    // for value in mir.iter() {
    //     println!("{:?}", value.contexts[0].values);
    // }
    println!("{:?}", mir);
    println!("---------\n\n");

    let llir = db.parse_mir(InputFile::Main);
    println!("{:?}", llir);
    println!();

    llir
}

fn main() {
    process::exit(match debug_run().as_ref() {
        Ok(llir) => {
            let result = DatapackBackend::generate(&llir);
            println!("{:#?}", result);

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
