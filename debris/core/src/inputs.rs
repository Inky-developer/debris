use debris_common::{Code, CodeRef, InputFile};
use std::fs;
use std::rc::Rc;

use crate::{objects::ModuleFactory, CompileContext, Config};

#[salsa::query_group(InputsStorage)]
pub trait Inputs: salsa::Database {
    #[salsa::input]
    fn input_file(&self, key: InputFile) -> String;

    #[salsa::input]
    fn extern_modules(&self) -> Rc<Vec<ModuleFactory>>;

    #[salsa::input]
    fn compile_context(&self) -> Rc<CompileContext>;

    fn input_text(&self, key: InputFile) -> CodeRef;

    fn config(&self, key: InputFile) -> Rc<Config>;
}

fn input_text(db: &dyn Inputs, key: InputFile) -> CodeRef {
    let file_name = db.input_file(key);

    CodeRef::new(Code {
        source: fs::read_to_string(&file_name).unwrap(),
        path: Some(file_name),
    })
}

fn config(_db: &dyn Inputs, _key: InputFile) -> Rc<Config> {
    Rc::new(Config::default())
}
