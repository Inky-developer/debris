//! A Backend that can comppile to minecraft datapacks

use crate::datapack::templates::{template_load_json, template_pack_mcmeta, template_tick_json};
use datapack_common::{directories, vfs::Directory};
use debris_core::Config;

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
    const FUNCTION_TAGS_PATH: [&'static str; 4] = ["data", "minecraft", "tags", "functions"];

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
                        functions => directories! {}
                    }
                }
            }
        };
        Datapack { dir, main_dir }
    }

    fn add_tick_json(&mut self, config: &Config) {
        let file = self
            .dir
            .resolve_path(&Self::FUNCTION_TAGS_PATH)
            .unwrap()
            .dir()
            .unwrap()
            .file("tick.json".into());
        file.contents = template_tick_json(config.into())
    }

    fn add_load_json(&mut self, config: &Config) {
        let file = self
            .dir
            .resolve_path(&Self::FUNCTION_TAGS_PATH)
            .unwrap()
            .dir()
            .unwrap()
            .file("load.json".into());
        file.contents = template_load_json(config.into())
    }

    /// Returns the functions directory of this pack
    fn functions(&mut self) -> &mut Directory {
        match self
            .dir
            .resolve_path(&["data", &self.main_dir, "functions"])
            .unwrap()
        {
            datapack_common::vfs::FsElement::Directoy(dir) => dir,
            datapack_common::vfs::FsElement::File(_) => unreachable!(),
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
