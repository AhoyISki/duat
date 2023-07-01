#[macro_export]
macro_rules! status_parts {
    () => { Vec::new() };

    ($($part:expr),+ $(,)?) => {
        {
            use $crate::widgets::StatusPart;
            vec![$(StatusPart::from($part)),+]
        }
    };
}

pub use status_parts;
