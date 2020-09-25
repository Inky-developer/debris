use std::{path::Path, process, rc::Rc};

use debris_backends::{Backend, DatapackBackend};
use debris_common::InputFile;
use debris_core::{
    error::Result, hir::HirParser, llir::LLIRParser, llir::LLIR, mir::MirParser, DatabaseStruct,
    Inputs,
};

fn debug_run() -> Rc<Result<LLIR>> {
    let mut db = DatabaseStruct::default();

    db.set_input_file(InputFile::Main, "test.txt".into());

    let ast = db.parse(InputFile::Main);
    println!("{:?}", ast);
    println!("---------\n\n");

    let mir = db.parse_hir(InputFile::Main);
    println!("{:?}", mir);
    println!("---------\n\n");

    let llir = db.parse_mir(InputFile::Main);
    println!("{:#?}", llir);
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
