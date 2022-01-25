use std::{fs, path::PathBuf};

use datapack_common::{
    functions::command_components::{Objective, ScoreHolder},
    vfs::Directory,
};
use datapack_vm::Interpreter;

use debris_backends::{Backend, DatapackBackend};
use debris_common::{CompileContext, OptMode};
use debris_error::CompileError;

mod common;
pub use common::*;

pub trait OrFail<T> {
    fn or_fail(self, ctx: &CompileContext) -> T;
}

impl<T> OrFail<T> for Result<T, CompileError> {
    fn or_fail(self, ctx: &CompileContext) -> T {
        match self {
            Ok(val) => val,
            Err(err) => {
                println!("{}", err.format(ctx));
                panic!("Program failed. See above for output.")
            }
        }
    }
}

fn compile_test_file(input_file: PathBuf, opt_mode: OptMode) -> Directory {
    println!("Testing '{}'...", input_file.display());
    let file = fs::read_to_string(&input_file)
        .unwrap_or_else(|_| panic!("Could not read test file {}", input_file.display()));

    // This solution is just temporary, so it is okay that this is a hack..
    let source = format!(
        "fn __test() -> Bool {{{file}}} 
         execute(\"scoreboard objectives add debris_test dummy\");
         let __result = __test();
         execute(`scoreboard players operation test_result debris_test = $__result`);",
    );

    let (result, config) = compile_string(source, ".".into(), opt_mode);
    let llir = result.or_fail(&config.compile_context);

    DatapackBackend.generate(&llir, &config.compile_context)
}

fn run_pack(dir: &Directory) -> Option<i32> {
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

    let mut i = Interpreter::new(functions, idx);

    i.run_to_end().unwrap();

    let name = ScoreHolder::new("test_result".to_string()).unwrap();
    let obj = Objective::new("debris_test".to_string()).unwrap();

    i.scoreboard.get(&name, &obj)
}

#[test]
fn test_compiled_datapacks_interpreted() {
    let test_files = fs::read_dir("tests/datapack_test_snippets")
        .unwrap()
        .filter_map(|entry| {
            let path = entry.unwrap().path();
            if path.is_file() {
                Some(path)
            } else {
                None
            }
        });

    println!("Running tests..");
    for file in test_files {
        for opt_mode in [OptMode::Debug, OptMode::Full] {
            let pack = compile_test_file(file.clone(), opt_mode);

            let result_code = run_pack(&pack).unwrap_or(0);

            println!(
                "test {}({opt_mode}) returned with {result_code}",
                file.display(),
            );
            if result_code != 1 {
                panic!("Program failed! Output: {result_code}")
            }
        }
    }
}

#[cfg(feature = "test_vanilla_server")]
#[test]
fn test_compiled_datapacks() {
    use mc_utils::{
        rcon,
        server::{self, ServerInstance},
    };
    use std::{env::temp_dir, thread::sleep, time::Duration};

    struct Tempdir(PathBuf);

    impl Drop for Tempdir {
        fn drop(&mut self) {
            // Honestly I don't know why that is needed. Maybe minecraft is still accessing
            // the direction after the server got killed?
            for i in 0..15 {
                match fs::remove_dir_all(&self.0) {
                    Ok(()) => return,
                    Err(e) => eprintln!("Try {i}: Could not remove temp dir: {e}"),
                }
                sleep(Duration::from_millis(1000));
            }
            panic!("Could not remove tempdir!");
        }
    }

    // This is in the outer scope to ensure that all other file handles are dropped when this dir
    // gets dropped
    let test_dir = Tempdir(temp_dir().join(".debris_test"));
    // new scope so that `test_dir` is dropped at last
    fs::create_dir(&test_dir.0).unwrap_or_else(|err| {
        panic!(
            "Could not create a temp dir at {}: {err}",
            test_dir.0.display(),
        )
    });
    println!("Tempdir at {}", test_dir.0.display());

    let test_files = fs::read_dir("tests/datapack_test_snippets")
        .unwrap()
        .filter_map(|entry| {
            let path = entry.unwrap().path();
            if path.is_file() {
                Some(path)
            } else {
                None
            }
        });
    let datapacks = test_dir.0.join("world/datapacks/");
    let version_manifest = server::VersionManifest::default();
    let latest_version = version_manifest
        .find_version(version_manifest.latest_release())
        .expect("Could not find latest release");

    println!("Downloading server");
    server::download_file(
        &latest_version
            .jar_url()
            .expect("Could not detect server jar url"),
        test_dir.0.join("server.jar"),
    )
    .expect("Could not download the server");

    println!("Installing server");
    // The server needs to live until the end of the function
    let _server = ServerInstance::builder(&test_dir.0)
        .property("rcon.port", "25575")
        .property("rcon.password", "1234")
        .property("enable-rcon", "true")
        .property("level-type", "flat")
        .build()
        .expect("Could not create server");

    let mut rcon = {
        let mut tries = 0;
        loop {
            if tries > 15 {
                panic!("Could not create rcon: Max tries exceeded!");
            }
            match rcon::McRcon::new(("localhost", 25575), "1234".to_string()) {
                Ok(rcon) => break rcon,
                Err(e) => {
                    eprintln!("Try {tries}: Could not create rcon: {e}");
                    sleep(Duration::from_millis(1000));
                }
            }
            tries += 1;
        }
    };

    println!("Running tests..");
    for file in test_files {
        println!("Compiling {}", file.display());
        for &opt_mode in &[OptMode::Debug, OptMode::Full] {
            let pack = compile_test_file(file.clone(), opt_mode);
            pack.persist("debris_test", &datapacks)
                .expect("Could not write the generated datapack");
            rcon.command("reload").unwrap();
            let result = rcon
                .command("scoreboard players get test_result debris_test")
                .unwrap();
            let result_code: i32 = result
                .payload
                .split_once("test_result has ")
                .unwrap_or_else(|| panic!("Bad server response: {}", result.payload))
                .1
                .trim_end_matches(" [debris_test]")
                .parse()
                .expect("Could not parse score");

            println!(
                "test {}({opt_mode}) returned with {result_code}",
                file.display(),
            );
            if result_code != 1 {
                panic!("Program failed! Output:\n{pack:?}")
            }

            fs::remove_dir_all(datapacks.join("debris_test"))
                .expect("Could not remove previous datapack");
        }
    }
}
