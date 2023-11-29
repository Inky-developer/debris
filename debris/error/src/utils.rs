use std::fmt::Display;

use itertools::Itertools;

pub fn display_expected_of_any<T: Display>(collection: &[T]) -> String {
    display_expected_of(collection, "Expected one of")
}

pub fn display_expected_of_all<T: Display>(collection: &[T]) -> String {
    display_expected_of(collection, "Expected all of")
}

/// Displays a string which say that the elements of `collection`
/// were expected.
pub fn display_expected_of<T: Display>(collection: &[T], expected_of: &str) -> String {
    match collection {
        [] => "Did not expect anything".to_string(),
        [single] => format!("Expected {single}"),
        many => format!(
            "{} ({})",
            expected_of,
            many.iter().map(|val| format!("{val}")).join(", ")
        ),
    }
}
