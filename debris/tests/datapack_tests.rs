use std::{env::temp_dir, fs, path::PathBuf, thread::sleep, time::Duration};

use debris_backends::{Backend, DatapackBackend};
use debris_common::Code;
use debris_core::{error::CompileError, CompileContext};
use debris_lang::{get_std_module, CompileConfig};

use mc_utils::{
    rcon,
    server::{self, ServerInstance},
};
use vfs::Directory;

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

fn compile_test_file(input_file: PathBuf) -> Directory {
    let file = fs::read_to_string(&input_file)
        .unwrap_or_else(|_| panic!("Could not read test file {}", input_file.display()));
    let mut config = CompileConfig::new(get_std_module().into(), ".".into());
    config.add_relative_file(input_file);

    // This solution is just temporary, so it is okay that this is a hack..
    let test_file = config.compile_context.input_files.add_input(Code {
        path: None,
        source: format!(
            "fn __test() -> Bool {{{}}} 
             execute(\"scoreboard objectives add debris_test dummy\");
             set_score(\"test_result\", \"debris_test\", dyn_int(__test()));",
            file
        ),
    });

    let hir = config.get_hir(test_file).or_fail(&config.compile_context);

    let mut mir = config.get_mir(&hir).or_fail(&config.compile_context);

    let llir = config
        .get_llir(&mir.contexts, &mut mir.namespaces)
        .or_fail(&config.compile_context);

    DatapackBackend::generate(&llir, &config.compile_context)
}

#[test]
fn test_compiled_datapacks() {
    // This is in the outer scope to ensure that all other file handles are dropped when this dir
    // gets dropped
    let test_dir = temp_dir().join(".debris_test");
    fs::create_dir(&test_dir).expect("Could not create a temp dir");

    println!("Tempdir at {}", test_dir.display());

    {
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
        let datapacks = test_dir.join("world/datapacks/");
        let version_manifest = server::VersionManifest::default();
        let latest_version = version_manifest
            .find_version(version_manifest.latest_release())
            .expect("Could not find latest release");

        println!("Downloading server");
        server::download_file(
            latest_version
                .jar_url()
                .expect("Could not detect server jar url"),
            test_dir.join("server.jar"),
        )
        .expect("Could not download the server");

        println!("Installing server");
        // The server needs to live until the end of the function
        let server = ServerInstance::new(&test_dir)
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
                        eprintln!("Try {}: Could not create rcon: {}", tries, e);
                        sleep(Duration::from_millis(1000));
                    }
                }
                tries += 1;
            }
        };

        println!("Running tests..");
        for file in test_files {
            let pack = compile_test_file(file.clone());
            pack.persist("debris_test", &datapacks)
                .expect("Could not write the generated datapack");
            rcon.command("reload").unwrap();
            let result = rcon
                .command("scoreboard players get test_result debris_test")
                .unwrap();

            let result_code: i32 = result
                .payload
                .split("test_result has ")
                .nth(1)
                .expect("Bad server response")
                .trim_end_matches(" [debris_test]")
                .parse()
                .expect("Could not parse score");

            println!("test {} returned with {}", file.display(), result_code);
            if result_code != 1 {
                panic!("Program failed! Output:\n{:?}", pack)
            }

            fs::remove_dir_all(datapacks.join("debris_test"))
                .expect("Could not remove previous datapack")
        }

        // No need to gracefully shut it down since it will be deleted anyways
        server.kill();
    }

    // Honestly I don't know why that is needed. Maybe minecraft is still accessing
    // the direction after the server got killed?
    for i in 0..15 {
        match fs::remove_dir_all(&test_dir) {
            Ok(()) => return,
            Err(e) => eprintln!("Try {}: Could not remove temp dir: {}", i, e),
        }
        sleep(Duration::from_millis(1000));
    }
    panic!("Failed to remove tempdir!");
}
