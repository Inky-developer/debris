/// Escapes a string for minecraft:
/// Newlines are replaced with \n and \ are replaced with \\
pub fn escape_minecraft(string: &str) -> impl Iterator<Item = char> + '_ {
    string.chars().flat_map(|chr| match chr {
        '\n' => StringEscape::Str("\\n"),
        '\\' => StringEscape::Str("\\\\"),
        c => StringEscape::Char(c),
    })
}

/// This implements iterator to allow for the flat_map operation
enum StringEscape {
    Str(&'static str),
    Char(char),
    None,
}

impl Iterator for StringEscape {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            StringEscape::Str(str) => {
                let mut chars = str.chars();
                let char = chars.next();
                if char.is_some() {
                    *str = chars.as_str();
                } else {
                    *self = StringEscape::None
                }
                char
            }
            StringEscape::Char(chr) => {
                let chr = *chr;
                *self = StringEscape::None;
                Some(chr)
            }
            StringEscape::None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::common::string_escape::escape_minecraft;

    #[test]
    fn test_escape_minecraft() {
        assert_eq!(
            escape_minecraft("This\ncontains\nsome\nnewlines").collect::<String>(),
            "This\\ncontains\\nsome\\nnewlines"
        );
        assert_eq!(
            escape_minecraft("Now \\ some \\ backslashes").collect::<String>(),
            "Now \\\\ some \\\\ backslashes"
        );
    }
}
