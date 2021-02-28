//! A Backend that can comppile to minecraft datapacks

use crate::datapack::templates::{template_load_json, template_pack_mcmeta, template_tick_json};
use debris_core::Config;
use vfs::{directories, Directory};

mod generator;

mod backend;
pub use backend::DatapackBackend;

mod stringify;
mod templates;

mod scoreboard_constants;

mod function_context;

mod scoreboard_context;

pub mod json_formatter;

/// Represents an in-memory datapack
#[derive(Debug, Default)]
struct Datapack {
    /// The virtual file structure
    dir: Directory,
    /// The name of the main directory
    main_dir: String,
}

impl Datapack {
    /// Creates a new `Datapack` from a [Config]
    ///
    /// Looks like the vfs implementation is really bad
    fn new(config: &Config) -> Self {
        let main_dir = config.project_name.to_ascii_lowercase();
        let dir = directories! {
            "pack.mcmeta" => File(template_pack_mcmeta(config.into())),
            data => directories! {
                main_dir.clone() => directories! {
                    functions => directories!()
                },
                minecraft => directories! {
                    tags => directories! {
                        functions => directories! {
                            "tick.json" => File(template_tick_json(config.into())),
                            "load.json" => File(template_load_json(config.into()))
                        }
                    }
                }
            }
        };
        Datapack { dir, main_dir }
    }

    /// Returns the functions directory of this pack
    fn functions(&mut self) -> &mut Directory {
        match self
            .dir
            .resolve_path(&[
                "data".to_string(),
                self.main_dir.clone(),
                "functions".to_string(),
            ])
            .unwrap()
        {
            vfs::FsElement::Directoy(dir) => dir,
            vfs::FsElement::File(_) => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use debris_core::Config;
    use tempfile::tempdir;

    use super::Datapack;

    #[test]
    fn test_default_pack() {
        let config = Config::default();
        let datapack = Datapack::new(&config);

        let temp_dir = tempdir().expect("Could not get a temporary directory");

        datapack
            .dir
            .persist("test_pack", temp_dir.path())
            .expect("Failed to persist");
    }
}
