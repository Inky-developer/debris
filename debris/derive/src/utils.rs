pub(crate) fn titlecase(string: &str) -> String {
    let mut chars = string.chars();

    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
    }
}
