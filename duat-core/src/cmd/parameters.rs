// THE ENTIRE PARAMETER SYSTEM WILL BE REDONE
use std::{any::TypeId, iter::Peekable, ops::Range};

pub use args_iter::split_flags_and_args;

use super::Parameter;
use crate::text::{Text, err};

#[derive(Clone)]
pub struct Args<'a> {
    args: Peekable<args_iter::ArgsIter<'a>>,
    range: Range<u32>,
    param_range: Range<u32>,
    started_recording_range: bool,
    is_recording_range: bool,
}

impl<'a> Args<'a> {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<&'a str, Text> {
        match self.args.next() {
            Some((arg, range)) => {
                self.param_range = range.clone();
                if self.started_recording_range {
                    self.range = range;
                    self.started_recording_range = false;
                    self.is_recording_range = true;
                } else {
                    self.range.end = range.end
                }
                Ok(arg)
            }
            None => Err(err!("Wrong argument count")),
        }
    }

    pub fn next_as<P: Parameter<'a>>(&mut self) -> Result<P::Returns, Text> {
        P::new(self)
    }

    pub fn next_else<T: Into<Text>>(&mut self, to_text: T) -> Result<&'a str, Text> {
        match self.args.next() {
            Some((arg, _)) => Ok(arg),
            None => Err(to_text.into()),
        }
    }

    pub fn collect<B: FromIterator<&'a str> + 'static>(self) -> B {
        let args: Vec<&str> = (self.args).map(|(str, _)| str).collect();

        if TypeId::of::<B>() == TypeId::of::<String>() {
            B::from_iter(args.into_iter().intersperse(" "))
        } else {
            B::from_iter(args)
        }
    }

    pub(crate) fn next_as_with_range<P: Parameter<'a>>(
        &mut self,
    ) -> Result<(P::Returns, Range<u32>), (Text, Range<u32>)> {
        self.started_recording_range = true;
        let ret = P::new(self)
            .map(|p| (p, self.range.clone()))
            .map_err(|e| (e, self.param_range.clone()));
        self.is_recording_range = false;
        ret
    }

    pub(super) fn is_recording_range(&self) -> bool {
        self.is_recording_range
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

mod args_iter {
    /// Takes the [`Flags`] from an [`Iterator`] of `args`.
    pub fn split_flags_and_args(command: &str) -> (super::InnerFlags<'_>, super::Args<'_>) {
        let mut blob = String::new();
        let mut word = Vec::new();

        let mut chars = command.char_indices();
        let mut start = None;
        let args: ArgsIter = std::iter::from_fn(move || {
            while let Some((b, char)) = chars.next() {
                if let Some(s) = start
                    && char.is_whitespace()
                {
                    start = None;
                    unsafe {
                        return Some((
                            core::str::from_utf8_unchecked(&command.as_bytes()[s..b]),
                            s as u32..b as u32,
                        ));
                    }
                } else if !char.is_whitespace() && start.is_none() {
                    start = Some(b);
                }
            }

            start.map(|s| unsafe {
                (
                    core::str::from_utf8_unchecked(&command.as_bytes()[s..]),
                    s as u32..command.len() as u32,
                )
            })
        });
        let mut args = args.peekable();
        let mut byte = 0;
        while let Some((arg, range)) = args.peek() {
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
                byte = range.start;
                break;
            }
        }

        (super::InnerFlags { blob, word }, super::Args {
            args,
            range: byte..byte,
            param_range: byte..byte,
            started_recording_range: false,
            is_recording_range: false,
        })
    }

    pub type ArgsIter<'a> = impl Iterator<Item = (&'a str, std::ops::Range<u32>)> + Clone;
}
