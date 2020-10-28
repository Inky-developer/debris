/// Naive function that converts a snake_case string to a CamelCase string
pub(crate) fn camelcase(string: &str) -> String {
    let mut out = String::with_capacity(string.len());

    let mut chars = string.chars();
    let mut prev_char = match chars.next() {
        Some(char) => char,
        None => return out,
    };
    out.push(if prev_char.is_ascii_alphabetic() {
        prev_char.to_ascii_uppercase()
    } else {
        prev_char
    });

    for char in chars {
        if prev_char.is_ascii_punctuation() && char.is_ascii_alphabetic() {
            out.push(char.to_ascii_uppercase())
        } else if !char.is_ascii_punctuation() {
            out.push(char)
        }

        prev_char = char;
    }

    out
}
