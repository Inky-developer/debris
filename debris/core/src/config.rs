/// The build mode for this compilation
///
/// Specifies how to optimize the code
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BuildMode {
    /// Debug build: produces slower code, but contains various safety checks
    /// and can be easier to debug
    Debug,
    /// Release build: optimized the code, drops most safety checks
    Release,
}

/// Holds data about the user specified configuration
#[derive(Debug, Eq, PartialEq)]
pub struct Config {
    pub project_name: String,
    pub project_description: String,
    pub default_scoreboard_name: String,
    pub build_mode: BuildMode,
}

impl BuildMode {
    /// Whether the current build mode is release
    pub fn is_release(&self) -> bool {
        matches!(self, BuildMode::Release)
    }

    /// Whether the current build mode is debug
    pub fn is_debug(&self) -> bool {
        matches!(self, BuildMode::Debug)
    }
}

impl Default for Config {
    fn default() -> Self {
        Config {
            project_name: "debris_project".to_string(),
            project_description: "A project generated by debris".to_string(),
            default_scoreboard_name: "debris".to_string(),
            build_mode: BuildMode::Debug,
        }
    }
}
