#[macro_export]
macro_rules! status_parts {
    () => { Vec::new() };

    ($($part:expr),+ $(,)?) => {
        vec![$(StatusPart::from($part)),+]
    };
}
pub use status_parts;

/// A convenience macro to join any number of variables that can
/// be turned into `String`s.
///
/// # Examples
///
/// ```
/// # use parsec_core::widgets::status_line::join;
/// let my_text = join!["num: ", 21, ", floating: ", 3.14];
/// assert!(my_text == String::from("number: 21, floating: 3.14"));
/// ```
#[macro_export]
macro_rules! join {
    () => { String::from("") };

    ($($var:expr),+ $(,)?) => {
        [$($var.to_string()),+].join("")
    }
}
pub use join;
