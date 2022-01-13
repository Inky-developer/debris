use std::fmt;

/// Represents the path of an extern item, like extern fn
/// This path only supports a very limited set of characters for now.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ExternItemPath(String);

impl ExternItemPath {
    /// A Regex of valid characters (See [Minecraft Wiki](https://minecraft.fandom.com/wiki/Resource_location#Legal_characters))
    /// Currently excludes "." to avoid confusion
    pub const VALID_CHARACTERS: &'static str = "[a-z0-9_-/]";

    pub fn new(path: String) -> Result<Self, ItemPathError> {
        let path = Self::verify(path)?;
        Ok(ExternItemPath(path))
    }

    pub fn verify(path: String) -> Result<String, ItemPathError> {
        if path.starts_with("__") {
            return Err(ItemPathError::InvalidDoubleUnderscore(path));
        }

        for chr in path.chars() {
            match chr {
                'a'..='z' | '0'..='9' | '_' | '-' | '/' => {}
                _ => return Err(ItemPathError::InvalidCharacter(path, chr)),
            }
        }
        Ok(path)
    }
}

impl AsRef<str> for ExternItemPath {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

pub enum ItemPathError {
    InvalidCharacter(String, char),
    InvalidDoubleUnderscore(String),
}

impl ItemPathError {
    pub fn path(self) -> String {
        match self {
            ItemPathError::InvalidCharacter(path, _)
            | ItemPathError::InvalidDoubleUnderscore(path) => path,
        }
    }
}

impl fmt::Display for ItemPathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ItemPathError::InvalidCharacter(_, char) => write!(
                f,
                "Invalid character: '{char}' (Valid characters are {})",
                ExternItemPath::VALID_CHARACTERS
            ),
            ItemPathError::InvalidDoubleUnderscore(_) => write!(
                f,
                "Extern item paths are not allowed to start with double underscores"
            ),
        }
    }
}
