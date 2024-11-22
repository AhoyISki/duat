// THE ENTIRE PARAMETER SYSTEM WILL BE REDONE
use std::{
    any::TypeId,
    iter::Peekable,
    str::{FromStr, SplitWhitespace},
};

use crate::text::{Text, err};

#[derive(Clone)]
pub struct Args<'a> {
    count: usize,
    expected: Option<usize>,
    args: Peekable<SplitWhitespace<'a>>,
}

impl<'a> Args<'a> {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> std::result::Result<&str, Text> {
        match self.args.next() {
            Some(arg) => {
                self.count += 1;
                Ok(arg)
            }
            None => Err({
                let args = match self.expected.unwrap_or(self.count + 1) {
                    1 => " argument",
                    _ => " arguments",
                };
                let expected = match self.expected {
                    Some(expected) => err!([*a] expected),
                    None => err!("at least " [*a] { self.count + 1 }),
                };
                let received = match self.count {
                    0 => err!([*a] "none"),
                    1 => err!([*a] 1),
                    count => err!([*a] count),
                };

                err!("Expected " expected args ", received " received ".")
            }),
        }
    }

    pub fn next_as<F: FromStr>(&mut self) -> std::result::Result<F, Text> {
        let arg = self.next()?;
        arg.parse().map_err(|_| {
            err!(
                "Couldn't convert " [*a] arg []
                " to " [*a] { std::any::type_name::<F>() } [] "."
            )
        })
    }

    pub fn next_else<T>(&mut self, to_text: T) -> std::result::Result<&str, Text>
    where
        T: Into<Text>,
    {
        match self.args.next() {
            Some(arg) => {
                self.count += 1;
                Ok(arg)
            }
            None => Err(to_text.into()),
        }
    }

    pub fn ended(&mut self) -> std::result::Result<(), Text> {
        match self.args.clone().count() {
            0 => Ok(()),
            count => Err({
                let args = match self.count == 1 {
                    true => " argument",
                    false => " arguments",
                };
                err!(
                    "Expected " [*a] { self.count } [] args
                    ", received " [*a] { self.count + count } [] " instead."
                )
            }),
        }
    }

    pub fn collect<B: FromIterator<&'a str> + 'static>(&mut self) -> B {
        let args: Vec<&str> = (&mut self.args).collect();

        if TypeId::of::<B>() == TypeId::of::<String>() {
            B::from_iter(args.into_iter().intersperse(" "))
        } else {
            B::from_iter(args)
        }
    }

    pub fn set_expected(&mut self, expected: usize) {
        self.expected = Some(expected);
    }
}

#[derive(Clone, Copy)]
pub struct Flags<'a, 'b>(&'a InnerFlags<'b>);

impl<'a, 'b> Flags<'a, 'b> {
    pub fn new(inner: &'a InnerFlags<'b>) -> Self {
        Self(inner)
    }

    pub fn blob(&self, blob: impl AsRef<str>) -> bool {
        self.0.blob(blob)
    }

    pub fn word(&self, flag: impl AsRef<str>) -> bool {
        self.0.word(flag)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

pub struct InnerFlags<'a> {
    blob: String,
    word: Vec<&'a str>,
}

impl InnerFlags<'_> {
    /// Checks if all of the [`char`]s in the `blob` passed.
    pub fn blob(&self, blob: impl AsRef<str>) -> bool {
        let mut all_chars = true;
        for char in blob.as_ref().chars() {
            all_chars &= self.blob.contains(char);
        }
        all_chars
    }

    /// Returns `true` if the `word` flag was passed.
    pub fn word(&self, flag: impl AsRef<str>) -> bool {
        self.word.contains(&flag.as_ref())
    }

    /// Returns `true` if no flags have been passed.
    pub fn is_empty(&self) -> bool {
        self.blob.is_empty() && self.word.is_empty()
    }
}

/// Takes the [`Flags`] from an [`Iterator`] of `args`.
pub fn split_flags_and_args(command: &str) -> (InnerFlags<'_>, Args<'_>) {
    let mut blob = String::new();
    let mut word = Vec::new();

    let mut args = command.split_whitespace().peekable();

    args.next();

    while let Some(arg) = args.peek() {
        if let Some(word_arg) = arg.strip_prefix("--") {
            if !word_arg.is_empty() {
                args.next();
                if !word.contains(&word_arg) {
                    word.push(word_arg)
                }
            } else {
                args.next();
                break;
            }
        } else if let Some(blob_arg) = arg.strip_prefix('-') {
            args.next();
            for char in blob_arg.chars() {
                if !blob.contains(char) {
                    blob.push(char)
                }
            }
        } else {
            break;
        }
    }

    (InnerFlags { blob, word }, Args {
        count: 0,
        expected: None,
        args,
    })
}
