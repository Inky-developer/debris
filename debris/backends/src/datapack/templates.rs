//! Implements some functions which can static files
//! which never really change except for some parameters, like
//! 'pack.mcmeta', 'tick.json' and so on

use debris_common::Config;

use crate::DatapackBackend;

#[derive(Debug, Clone, Copy)]
pub struct TemplateData<'a> {
    project: &'a str,
    project_description: &'a str,
    main_function_dir: &'a str,
    // default_scoreboard: &'a str,
    // build_mode: BuildMode,
}

impl<'a> From<&'a Config> for TemplateData<'a> {
    fn from(config: &'a Config) -> Self {
        TemplateData {
            project: &config.project_name,
            project_description: &config.project_description,
            main_function_dir: DatapackBackend::FUNCTION_INTERNAL_PATH,
            // default_scoreboard: &config.default_scoreboard_name,
            // build_mode: config.build_mode,
        }
    }
}

pub fn template_load_json(data: TemplateData) -> String {
    format!(
        include_str!("res/load.json.tp"),
        project = data.project,
        main_function_dir = data.main_function_dir
    )
}

pub fn template_tick_json(data: TemplateData) -> String {
    format!(
        include_str!("res/tick.json.tp"),
        project = data.project,
        main_function_dir = data.main_function_dir
    )
}

pub fn template_pack_mcmeta(data: TemplateData) -> String {
    format!(
        include_str!("res/pack.mcmeta.tp"),
        project_description = data.project_description
    )
}
