//! Defines the processing of parameters in commands
//!
//! This processing first separates the [`Flags`] and [`Args`] of the
//! call, and then transforms the list of arguments into a list of
//! [`Parameter`]s, as defined by the command. Each [`Parameter`] may
//! take multiple words, which makes this structure very flexible for
//! multiple branching paths on how to read the arguments, all from
//! the same command.
use std::{iter::Peekable, ops::Range, path::PathBuf};

use crossterm::style::Color;

use crate::text::{Text, err};

pub trait Parameter<'a>: Sized {
    type Returns;
    /// Tries to consume arguments until forming a parameter
    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text>;
}

impl<'a, P: Parameter<'a>> Parameter<'a> for Option<P> {
    type Returns = Option<P::Returns>;

    /// Will match either [`Parameter`] given, or nothing
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        match args.next_as::<P>() {
            Ok(arg) => Ok(Some(arg)),
            Err(err) if args.is_forming_param => Err(err),
            Err(_) => Ok(None),
        }
    }
}

impl<'a, P: Parameter<'a>> Parameter<'a> for Vec<P> {
    type Returns = Vec<P::Returns>;

    /// Will match a list of [`Parameter`]s
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        let mut returns = Vec::new();

        loop {
            match args.next_as::<P>() {
                Ok(ret) => returns.push(ret),
                Err(err) if args.is_forming_param => return Err(err),
                Err(_) => break Ok(returns),
            }
        }
    }
}

impl<'a, const N: usize, P: Parameter<'a>> Parameter<'a> for [P; N] {
    type Returns = [P::Returns; N];

    /// Will match either the argument given, or nothing
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        use std::mem::MaybeUninit;
        let mut returns = [const { MaybeUninit::uninit() }; N];

        for r in returns.iter_mut() {
            match args.next_as::<P>() {
                Ok(ret) => *r = MaybeUninit::new(ret),
                Err(err) => return Err(err),
            }
        }

        Ok(returns.map(|ret| unsafe { ret.assume_init() }))
    }
}

/// Command [`Parameter`]: A list of between `MIN` and `MAX` items
///
/// This, like other lists, _has_ to be the final argument in the
/// [`Parameter`] list, as it will either match correcly, finish
/// matching, or match incorrectly in order to give accurate
/// feedback.
pub struct Between<const MIN: usize, const MAX: usize, P>(std::marker::PhantomData<P>);

impl<'a, const MIN: usize, const MAX: usize, P: Parameter<'a>> Parameter<'a>
    for Between<MIN, MAX, P>
{
    type Returns = Vec<P::Returns>;

    /// Will match between `MIN` and `MAX` [`Parameter`]s
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        let mut returns = Vec::new();

        for _ in 0..MAX {
            match args.next_as::<P>() {
                Ok(ret) => returns.push(ret),
                Err(err) if args.is_forming_param => return Err(err),
                Err(_) if returns.len() >= MIN => return Ok(returns),
                Err(err) => return Err(err),
            }
        }

        if returns.len() >= MIN {
            Ok(returns)
        } else {
            Err(err!(
                "List needed at least [a]{MIN}[] elements, got only [a]{}",
                returns.len()
            ))
        }
    }
}

impl<'a> Parameter<'a> for &'a str {
    type Returns = &'a str;

    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        args.next()
    }
}

impl Parameter<'_> for String {
    type Returns = String;

    fn new(args: &mut Args) -> Result<Self::Returns, Text> {
        Ok(args.next()?.to_string())
    }
}

/// Command [`Parameter`]: The remaining arguments, divided by a space
///
/// Fails if the [`String`] would be empty.
pub struct Remainder;

impl Parameter<'_> for Remainder {
    type Returns = String;

    fn new(args: &mut Args) -> Result<Self::Returns, Text> {
        let remainder: String = std::iter::from_fn(|| args.next().ok())
            .collect::<Vec<&str>>()
            .join(" ");
        if remainder.is_empty() {
            Err(err!("There are no more arguments"))
        } else {
            Ok(remainder)
        }
    }
}

/// Command [`Parameter`]: An existing [`ColorScheme`]'s name
///
/// [`ColorScheme`]: crate::form::ColorScheme
pub struct ColorSchemeArg;

impl<'a> Parameter<'a> for ColorSchemeArg {
    type Returns = &'a str;

    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        let scheme = args.next()?;
        if crate::form::colorscheme_exists(scheme) {
            Ok(scheme)
        } else {
            Err(err!("The colorscheme [a]{scheme}[] was not found"))
        }
    }
}

/// Command [`Parameter`]: An open [`File`]'s name
///
/// [`File`]: crate::widgets::File
pub struct FileBuffer<U>(std::marker::PhantomData<U>);

impl<'a, U: crate::ui::Ui> Parameter<'a> for FileBuffer<U> {
    type Returns = &'a str;

    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        let buffer = args.next()?;
        let windows = crate::context::windows::<U>().borrow();
        if windows
            .iter()
            .flat_map(|w| w.file_names())
            .any(|f| f == buffer)
        {
            Ok(buffer)
        } else {
            Err(err!("No buffer called [a]{buffer}[] open"))
        }
    }
}

/// Command [`Parameter`]: An open [`File`]'s name, except the current
///
/// [`File`]: crate::widgets::File
pub struct OtherFileBuffer<U>(std::marker::PhantomData<U>);

impl<'a, U: crate::ui::Ui> Parameter<'a> for OtherFileBuffer<U> {
    type Returns = &'a str;

    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        let buffer = args.next_as::<FileBuffer<U>>()?;
        let mut ff = crate::context::fixed_file::<U>().unwrap();
        if buffer == ff.read(|file, _| file.name()) {
            Err(err!("Argument can't be the current file"))
        } else {
            Ok(buffer)
        }
    }
}

/// Command [`Parameter`]: A [`File`] whose parent is real
///
/// [`File`]: crate::widgets::File
pub struct PossibleFile;

impl Parameter<'_> for PossibleFile {
    type Returns = PathBuf;

    fn new(args: &mut Args<'_>) -> Result<Self::Returns, Text> {
        let path = args.next_as::<PathBuf>()?;

        let canon_path = path.canonicalize();
        if let Ok(path) = &canon_path {
            if !path.is_file() {
                return Err(err!("Path is not a file"));
            }
            Ok(path.clone())
        } else if canon_path.is_err()
            && let Ok(canon_path) = path.with_file_name(".").canonicalize()
        {
            Ok(canon_path.join(
                path.file_name()
                    .ok_or_else(|| err!("Path has no file name"))?,
            ))
        } else {
            Err(err!("Path was not found"))
        }
    }
}

/// Command [`Parameter`]: An [`f32`] from a [`u8`] or a percentage
///
/// The percentage is of whole divisions of 100, 100 being equivalent
/// to 255 in [`u8`].
pub struct F32PercentOfU8;

impl Parameter<'_> for F32PercentOfU8 {
    type Returns = f32;

    fn new(args: &mut Args) -> Result<Self::Returns, Text> {
        let arg = args.next()?;
        if let Some(percentage) = arg.strip_suffix("%") {
            let percentage: u8 = percentage
                .parse()
                .map_err(|_| err!("[a]{arg}[] is not a valid percentage"))?;
            if percentage <= 100 {
                Ok(percentage as f32 / 100.0)
            } else {
                Err(err!("[a]{arg}[] is more than [a]100%"))
            }
        } else {
            let byte: u8 = arg
                .parse()
                .map_err(|_| err!("[a]{arg}[] couldn't be parsed"))?;
            Ok(byte as f32 / 255.0)
        }
    }
}

impl<'a> Parameter<'a> for Color {
    type Returns = Color;

    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        const fn hue_to_rgb(p: f32, q: f32, mut t: f32) -> f32 {
            t = if t < 0.0 { t + 1.0 } else { t };
            t = if t > 1.0 { t - 1.0 } else { t };
            if t < 1.0 / 6.0 {
                p + (q - p) * 6.0 * t
            } else if t < 1.0 / 2.0 {
                q
            } else if t < 2.0 / 3.0 {
                p + (q - p) * (2.0 / 3.0 - t) * 6.0
            } else {
                p
            }
        }

        let arg = args.next()?;
        // Expects "#{red:x}{green:x}{blue:x}"
        if let Some(hex) = arg.strip_prefix("#") {
            let total = match u32::from_str_radix(hex, 16) {
                Ok(total) if hex.len() == 6 => total,
                _ => return Err(err!("Hexcode does not contain 6 hex values")),
            };
            let r = (total >> 16) as u8;
            let g = (total >> 8) as u8;
            let b = total as u8;
            Ok(Color::Rgb { r, g, b })
            // Expects "rgb {red} {green} {blue}"
        } else if arg == "rgb" {
            let r = args.next_as::<u8>()?;
            let g = args.next_as::<u8>()?;
            let b = args.next_as::<u8>()?;
            Ok(Color::Rgb { r, g, b })
            // Expects "hsl {hue%?} {saturation%?} {lightness%?}"
        } else if arg == "hsl" {
            let hue = args.next_as::<F32PercentOfU8>()?;
            let sat = args.next_as::<F32PercentOfU8>()?;
            let lit = args.next_as::<F32PercentOfU8>()?;
            let [r, g, b] = if sat == 0.0 {
                [lit.round() as u8; 3]
            } else {
                let q = if lit < 0.5 {
                    lit * (1.0 + sat)
                } else {
                    lit + sat - lit * sat
                };
                let p = 2.0 * lit - q;
                let r = hue_to_rgb(p, q, hue + 1.0 / 3.0);
                let g = hue_to_rgb(p, q, hue);
                let b = hue_to_rgb(p, q, hue - 1.0 / 3.0);
                [r.round() as u8, g.round() as u8, b.round() as u8]
            };
            Ok(Color::Rgb { r, g, b })
        } else {
            return Err(err!("Color format was not recognized"));
        }
    }
}

/// Command [`Parameter`]: A [`set`] [`Form`]'s name
///
/// [`set`]: crate::form::set
/// [`Form`]: crate::form::Form
pub struct FormName;

impl<'a> Parameter<'a> for FormName {
    type Returns = &'a str;

    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        let arg = args.next()?;
        if crate::form::exists(arg) {
            Ok(arg)
        } else {
            Err(err!("The form [a]{arg}[] has not been set"))
        }
    }
}

impl<'a> Parameter<'a> for Flags<'a> {
    type Returns = Flags<'a>;

    fn new(args: &mut Args<'a>) -> Result<Self::Returns, Text> {
        Ok(args.flags.clone())
    }
}

/// The list of arguments passed to a command
///
/// This list excludes [`Flags`], and separates arguments either by
/// whitespace, or by non escaped double quotes.
///
/// ```rust
/// # use duat_core::cmd;
/// //                  cmd │      flags      │         arguments
/// //                 ┌   ┐│┌               ┐│┌  ┐ ┌        ┐ ┌   ┐ ┌   ┐
/// cmd::run_notify(r#"mycmd --flags -moreflag arg1 "more arg" \"arg arg\""#);
/// ```
#[derive(Clone)]
pub struct Args<'a> {
    args: Peekable<ArgsIter<'a>>,
    param_range: Range<usize>,
    has_to_start_param: bool,
    is_forming_param: bool,
    flags: Flags<'a>,
}

impl<'a> Args<'a> {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<&'a str, Text> {
        match self.args.next() {
            Some((arg, range)) => {
                self.param_range = range.clone();
                if self.has_to_start_param {
                    self.has_to_start_param = false;
                    self.is_forming_param = true;
                }
                Ok(arg)
            }
            None => Err(err!("Wrong argument count")),
        }
    }

    pub fn next_as<P: Parameter<'a>>(&mut self) -> Result<P::Returns, Text> {
        self.has_to_start_param = true;
        let ret = P::new(self);
        if ret.is_ok() {
            self.is_forming_param = false;
        }
        ret
    }

    pub fn next_else<T: Into<Text>>(&mut self, to_text: T) -> Result<&'a str, Text> {
        match self.args.next() {
            Some((arg, _)) => Ok(arg),
            None => Err(to_text.into()),
        }
    }

    pub fn next_start(&mut self) -> Option<usize> {
        self.args.peek().map(|(_, r)| r.start)
    }

    pub fn param_range(&self) -> Range<usize> {
        self.param_range.clone()
    }
}

#[derive(Clone)]
pub struct Flags<'a> {
    blob: String,
    word: Vec<&'a str>,
}

impl Flags<'_> {
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

/// Splits a command into [`Args`] and [`Flags`]
///
/// [`Args`]: super::Args
/// [`Flags`]: super::Flags
pub fn get_args(command: &str) -> super::Args<'_> {
    let mut blob = String::new();
    let mut word = Vec::new();

    let args = args_iter(command);
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

    super::Args {
        args,
        param_range: byte..byte,
        has_to_start_param: false,
        is_forming_param: false,
        flags: super::Flags { blob, word },
    }
}

#[define_opaque(ArgsIter)]
pub fn args_iter(command: &str) -> ArgsIter {
    let mut chars = command.char_indices();
    let mut start = None;
    let mut end = None;
    let mut is_quoting = false;
    // Initial value doesn't matter, as long as it's not '\'
    let mut last_char = 'a';
    let mut args: ArgsIter = std::iter::from_fn(move || {
        while let Some((b, char)) = chars.next() {
            let lc = last_char;
            last_char = char;
            if start.is_some() && char.is_whitespace() && !is_quoting {
                end = Some(b);
                break;
            } else if char == '"' && lc != '\\' {
                is_quoting = !is_quoting;
                if !is_quoting {
                    end = Some(b + 1);
                    break;
                } else {
                    start = Some(b);
                }
            } else if !char.is_whitespace() && start.is_none() {
                start = Some(b);
            }
        }

        start.take().map(|s| unsafe {
            let e = end.take().unwrap_or(command.len());
            (
                core::str::from_utf8_unchecked(&command.as_bytes()[s..e]),
                s..e,
            )
        })
    });
    args.next();
    args
}

pub type ArgsIter<'a> = impl Iterator<Item = (&'a str, std::ops::Range<usize>)> + Clone;

parse_impl!(bool);
parse_impl!(u8);
parse_impl!(u16);
parse_impl!(u32);
parse_impl!(u64);
parse_impl!(u128);
parse_impl!(usize);
parse_impl!(i8);
parse_impl!(i16);
parse_impl!(i32);
parse_impl!(i64);
parse_impl!(i128);
parse_impl!(isize);
parse_impl!(f32);
parse_impl!(f64);
parse_impl!(std::path::PathBuf);

macro parse_impl($t:ty) {
    impl Parameter<'_> for $t {
        type Returns = Self;

        fn new(args: &mut Args) -> Result<Self::Returns, Text> {
            let arg = args.next()?;
            arg.parse()
                .map_err(|_| err!("[a]{arg}[] couldn't be parsed as [a]{}[]", stringify!($t)))
        }
    }
}
