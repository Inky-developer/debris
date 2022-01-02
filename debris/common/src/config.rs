use std::fmt;

/// The build mode for this compilation
///
/// Specifies how to optimize the code
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BuildMode {
    /// Debug build: produces slower code, but contains various safety checks
    /// and can be easier to debug
    Debug,
    /// drops most safety checks, lower readability
    Release,
}

/// How to optimize the code
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OptMode {
    /// No optimizations
    None,
    /// Some optimizations, but no aggressive inlining (default)
    Debug,
    /// Full optimizations
    Full,
}

impl OptMode {
    pub fn disable_optimization(&self) -> bool {
        matches!(self, OptMode::None)
    }

    /// Returns whether the optimizer should perform aggressive function inlining
    pub fn aggressive_function_inlining(&self) -> bool {
        matches!(self, OptMode::Full)
    }
}

impl fmt::Display for OptMode {
    #[allow(clippy::use_debug)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Holds data about the user specified configuration
#[derive(Debug, Eq, PartialEq)]
pub struct Config {
    pub project_name: String,
    pub project_description: String,
    pub default_scoreboard_name: String,
    pub build_mode: BuildMode,
    pub opt_mode: OptMode,
}

impl Config {
    /// Replaces the build mode and changes the opt mode to the new default
    pub fn update_build_mode(&mut self, build_mode: BuildMode) {
        self.build_mode = build_mode;
        self.opt_mode = build_mode.opt_mode();
    }
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

    /// Returns the default optimization mode for this build mode
    pub fn opt_mode(&self) -> OptMode {
        match self {
            BuildMode::Debug => OptMode::Debug,
            BuildMode::Release => OptMode::Full,
        }
    }
}

impl Default for BuildMode {
    fn default() -> Self {
        BuildMode::Debug
    }
}

impl Default for Config {
    fn default() -> Self {
        Config {
            project_name: "debris_project".to_string(),
            project_description: "A project generated by debris".to_string(),
            default_scoreboard_name: "debris".to_string(),
            build_mode: BuildMode::default(),
            opt_mode: BuildMode::default().opt_mode(),
        }
    }
}
