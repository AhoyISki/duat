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

use crate::{
    context::Handle,
    data::Pass,
    form::{self, FormId},
    text::{Text, txt},
};

macro_rules! implDeref {
    ($type:ty, $target:ty $(, $($args:tt)+)?) => {
        impl$(<$($args)+>)? std::ops::Deref for $type$(<$($args)+>)? {
            type Target = $target;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl$(<$($args)+>)? std::ops::DerefMut for $type$(<$($args)+>)? {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    }
}

/// A parameter for commands that can be called
///
/// This parameter must be parseable from [`Args`], which come from a
/// `&str`. It can take multiple words, and can be composed of other
/// [`Parameter`]s. An example of this is the [`Form`], which is
/// composed of multiple [`Color`] parameters, which are then composed
/// of some format (rgb, hsl), which is then composed of more
/// parameters, like rgb values, for example.
///
/// Other types of [`Parameter`] are just a "list" of other
/// [`Parameter`]s. For example, [`Vec<P>`] can be used as a
/// [`Parameter`] to capture any number of `P` arguments.
/// Additionally, there is the [`Between<MIN, MAX, P>`], which is
/// _like_ [`Vec<P>`], but takes at least `MIN` `P`s and at most `MAX`
/// `P`s.
///
/// [`Form`]: crate::form::Form
pub trait Parameter: Sized {
    /// Tries to consume arguments until forming a parameter
    ///
    /// Since parameters shouldn't mutate data, pa is just a regular
    /// shared reference.
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text>;
}

impl<P: Parameter> Parameter for Option<P> {
    /// Will match either [`Parameter`] given, or nothing
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        match args.next_as::<P>(pa) {
            Ok(arg) => Ok((Some(arg), None)),
            Err(err) if args.is_forming_param => Err(err),
            Err(_) => Ok((None, None)),
        }
    }
}

impl<P: Parameter> Parameter for Vec<P> {
    /// Will match a list of [`Parameter`]s
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let mut returns = Vec::new();

        loop {
            match args.next_as::<P>(pa) {
                Ok(ret) => returns.push(ret),
                Err(err) if args.is_forming_param => return Err(err),
                Err(_) => break Ok((returns, None)),
            }
        }
    }
}

impl<const N: usize, P: Parameter> Parameter for [P; N] {
    /// Will match either the argument given, or nothing
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        use std::mem::MaybeUninit;
        let mut returns = [const { MaybeUninit::uninit() }; N];

        for r in returns.iter_mut() {
            match args.next_as::<P>(pa) {
                Ok(ret) => *r = MaybeUninit::new(ret),
                Err(err) => return Err(err),
            }
        }

        Ok((returns.map(|ret| unsafe { ret.assume_init() }), None))
    }
}

/// Command [`Parameter`]: A list of between `MIN` and `MAX` items
///
/// This, like other lists, _has_ to be the final argument in the
/// [`Parameter`] list, as it will either match correcly, finish
/// matching, or match incorrectly in order to give accurate
/// feedback.
pub struct Between<const MIN: usize, const MAX: usize, P>(pub Vec<P>);

impl<const MIN: usize, const MAX: usize, P: Parameter> Parameter for Between<MIN, MAX, P> {
    /// Will match between `MIN` and `MAX` [`Parameter`]s
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let mut returns = Vec::new();

        for _ in 0..MAX {
            match args.next_as::<P>(pa) {
                Ok(ret) => returns.push(ret),
                Err(err) if args.is_forming_param => return Err(err),
                Err(_) if returns.len() >= MIN => return Ok((Self(returns), None)),
                Err(err) => return Err(err),
            }
        }

        if returns.len() >= MIN {
            Ok((Self(returns), None))
        } else {
            Err(txt!(
                "List needed at least [a]{MIN}[] elements, got only [a]{}",
                returns.len()
            ))
        }
    }
}

impl<const MIN: usize, const MAX: usize, P> std::ops::Deref for Between<MIN, MAX, P> {
    type Target = Vec<P>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const MIN: usize, const MAX: usize, P> std::ops::DerefMut for Between<MIN, MAX, P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Parameter for String {
    fn new(_: &Pass, args: &mut Args) -> Result<(String, Option<FormId>), Text> {
        Ok((args.next()?.to_string(), None))
    }
}

/// Command [`Parameter`]: The remaining arguments, divided by a space
///
/// Fails if the [`String`] would be empty.
pub struct Remainder(pub String);

impl Parameter for Remainder {
    fn new(_: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let remainder: String = std::iter::from_fn(|| args.next().ok())
            .collect::<Vec<&str>>()
            .join(" ");
        if remainder.is_empty() {
            Err(txt!("There are no more arguments"))
        } else {
            Ok((Self(remainder), None))
        }
    }
}
implDeref!(Remainder, String);

/// Command [`Parameter`]: An existing [`ColorScheme`]'s name
///
/// [`ColorScheme`]: crate::form::ColorScheme
pub struct ColorSchemeArg(pub String);

impl Parameter for ColorSchemeArg {
    fn new(_: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let scheme = args.next()?;
        if crate::form::colorscheme_exists(scheme) {
            Ok((ColorSchemeArg(scheme.to_string()), None))
        } else {
            Err(txt!("The colorscheme [a]{scheme}[] was not found"))
        }
    }
}
implDeref!(ColorSchemeArg, String);

impl Parameter for Handle {
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let buffer_name = args.next()?;
        if let Some(handle) = crate::context::windows()
            .buffers(pa)
            .find(|handle| handle.read(pa).name() == buffer_name)
        {
            Ok((handle, Some(form::id_of!("param.buffer.open"))))
        } else {
            Err(txt!("No buffer called [a]{buffer_name}[] open"))
        }
    }
}

/// Command [`Parameter`]: An open [`Buffer`]'s name, except the
/// current
///
/// [`Buffer`]: crate::buffer::Buffer
pub struct OtherBuffer(pub Handle);

impl Parameter for OtherBuffer {
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let handle = args.next_as::<Handle>(pa)?;
        let cur_handle = crate::context::current_buffer(pa);
        if *cur_handle == handle {
            Err(txt!("Argument can't be the current buffer"))
        } else {
            Ok((Self(handle), Some(form::id_of!("param.buffer.open"))))
        }
    }
}
implDeref!(OtherBuffer, Handle);

/// Command [`Parameter`]: A file that _could_ exist
///
/// This is the case if the file's path has a parent that exists,
/// or if the file itself exists.
///
/// [`Buffer`]: crate::buffer::Buffer
pub struct ValidFilePath(pub PathBuf);

impl Parameter for ValidFilePath {
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let path = args.next_as::<PathBuf>(pa)?;

        let canon_path = path.canonicalize();
        let path = if let Ok(path) = &canon_path {
            if !path.is_file() {
                return Err(txt!("Path is not a buffer"));
            }
            path.clone()
        } else if canon_path.is_err()
            && let Ok(canon_path) = path.with_file_name(".").canonicalize()
        {
            canon_path.join(
                path.file_name()
                    .ok_or_else(|| txt!("Path has no buffer name"))?,
            )
        } else {
            return Err(txt!("Path was not found"));
        };

        if let Some(parent) = path.parent()
            && let Ok(false) | Err(_) = parent.try_exists()
        {
            return Err(txt!("Path's parent doesn't exist"));
        }

        let form = if crate::context::windows()
            .buffers(pa)
            .map(|handle| handle.read(pa).path())
            .any(|p| std::path::Path::new(&p) == path)
        {
            form::id_of!("param.buffer.open")
        } else if let Ok(true) = path.try_exists() {
            form::id_of!("param.buffer.exists")
        } else {
            form::id_of!("param.buffer")
        };

        Ok((Self(path), Some(form)))
    }
}
implDeref!(ValidFilePath, PathBuf);

/// Comand [`Parameter`]: A [`ValidFile`], [`Handle`], `--opts` or
/// `--opts-manifest`
///
/// This is a generalized way of switching to a [`Handle<Buffer`] on
/// the `edit` and `open` commands.
pub(super) enum PathOrBufferOrCfg {
    Path(PathBuf),
    Buffer(Handle),
    Cfg,
    CfgManifest,
}

impl Parameter for PathOrBufferOrCfg {
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        if args.flags.word("cfg") {
            Ok((Self::Cfg, None))
        } else if args.flags.word("cfg-manifest") {
            Ok((Self::CfgManifest, None))
        } else if let Ok((handle, form)) = args.next_as_with_form::<Handle>(pa) {
            Ok((Self::Buffer(handle), form))
        } else {
            let (path, form) = args.next_as_with_form::<ValidFilePath>(pa)?;
            Ok((Self::Path(path.0), form))
        }
    }
}

/// Command [`Parameter`]: An [`f32`] from a [`u8`] or a percentage
///
/// The percentage is of whole divisions of 100, 100 being equivalent
/// to 255 in [`u8`].
pub struct F32PercentOfU8(pub f32);

impl Parameter for F32PercentOfU8 {
    fn new(_: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let arg = args.next()?;
        if let Some(percentage) = arg.strip_suffix("%") {
            let percentage: u8 = percentage
                .parse()
                .map_err(|_| txt!("[a]{arg}[] is not a valid percentage"))?;
            if percentage <= 100 {
                Ok((Self(percentage as f32 / 100.0), None))
            } else {
                Err(txt!("[a]{arg}[] is more than [a]100%"))
            }
        } else {
            let byte: u8 = arg
                .parse()
                .map_err(|_| txt!("[a]{arg}[] couldn't be parsed"))?;
            Ok((Self(byte as f32 / 255.0), None))
        }
    }
}
implDeref!(F32PercentOfU8, f32);

impl Parameter for Color {
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
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
                _ => return Err(txt!("Hexcode does not contain 6 hex values")),
            };
            let r = (total >> 16) as u8;
            let g = (total >> 8) as u8;
            let b = total as u8;
            Ok((Color::Rgb { r, g, b }, None))
            // Expects "rgb {red} {green} {blue}"
        } else if arg == "rgb" {
            let r = args.next_as::<u8>(pa)?;
            let g = args.next_as::<u8>(pa)?;
            let b = args.next_as::<u8>(pa)?;
            Ok((Color::Rgb { r, g, b }, None))
            // Expects "hsl {hue%?} {saturation%?} {lightness%?}"
        } else if arg == "hsl" {
            let hue = args.next_as::<F32PercentOfU8>(pa)?.0;
            let sat = args.next_as::<F32PercentOfU8>(pa)?.0;
            let lit = args.next_as::<F32PercentOfU8>(pa)?.0;
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
            Ok((Color::Rgb { r, g, b }, None))
        } else {
            Err(txt!("Color format was not recognized"))
        }
    }
}

/// Command [`Parameter`]: The name of a [`Form`] that has been [set]
///
/// [set]: crate::form::set
/// [`Form`]: crate::form::Form
pub struct FormName(pub String);

impl Parameter for FormName {
    fn new(_: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let arg = args.next()?;
        if !arg.chars().all(|c| c.is_ascii_alphanumeric() || c == '.') {
            return Err(txt!(
                "Expected identifiers separated by '.'s, found [a]{arg}"
            ));
        }
        if crate::form::exists(arg) {
            Ok((Self(arg.to_string()), Some(form::id_of_non_static(arg))))
        } else {
            Err(txt!("The form [a]{arg}[] has not been set"))
        }
    }
}
implDeref!(FormName, String);

impl Parameter for Flags<'_> {
    fn new(_: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        Ok((args.flags.clone(), None))
    }
}

/// The list of arguments passed to a command
///
/// This list excludes [`Flags`], and separates arguments either by
/// whitespace, or by non escaped double quotes.
///
/// ```rust
/// # use duat_core::cmd;
/// # fn test() {
/// //                    cmd │      flags      │         arguments
/// //                   ┌   ┐│┌               ┐│┌  ┐ ┌        ┐ ┌   ┐ ┌   ┐
/// cmd::queue_notify(r#"mycmd --flags -moreflag arg1 "more arg" \"arg arg\""#);
/// # }
/// ```
pub struct Args<'a> {
    args: Peekable<ArgsIter<'a>>,
    param_range: Range<usize>,
    has_to_start_param: bool,
    is_forming_param: bool,
    flags: Flags<'a>,
}

impl<'a> Args<'a> {
    /// Returns the next word or quoted argument
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
            None => Err(txt!("Wrong argument count")),
        }
    }

    /// Tries to parse the next argument as `P`
    ///
    /// If parsing fails, [`Args`] will be reset as if this function
    /// wasn't called.
    pub fn next_as<P: Parameter>(&mut self, pa: &Pass) -> Result<P, Text> {
        let initial_args = self.args.clone();
        self.has_to_start_param = true;
        let ret = P::new(pa, self);
        if ret.is_ok() {
            self.is_forming_param = false;
        } else {
            self.args = initial_args
        }
        ret.map(|(arg, _)| arg)
    }

    /// Tries to parse the next argument as `P`
    ///
    /// If parsing fails, [`Args`] will be reset as if this function
    /// wasn't called.
    pub fn next_as_with_form<P: Parameter>(
        &mut self,
        pa: &Pass,
    ) -> Result<(P, Option<FormId>), Text> {
        let initial_args = self.args.clone();
        self.has_to_start_param = true;
        let ret = P::new(pa, self);
        if ret.is_ok() {
            self.is_forming_param = false;
        } else {
            self.args = initial_args
        }
        ret
    }

    /// Tries to get the next argument, otherwise returns a [`Text`]
    pub fn next_else<T: Into<Text>>(&mut self, to_text: T) -> Result<&'a str, Text> {
        match self.args.next() {
            Some((arg, _)) => Ok(arg),
            None => Err(to_text.into()),
        }
    }

    /// Returns the char position of the next argument
    ///
    /// Mostly used for error feedback by the [`PromptLine`]
    ///
    /// [`PromptLine`]: docs.rs/duat/latest/duat/widgets/struct.PromptLine.html
    pub fn next_start(&mut self) -> Option<usize> {
        self.args.peek().map(|(_, r)| r.start)
    }

    /// The range of the previous [`Parameter`]
    ///
    /// Mostly used for error feedback by the [`PromptLine`]
    ///
    /// [`PromptLine`]: docs.rs/duat/latest/duat/widgets/struct.PromptLine.html
    pub fn param_range(&self) -> Range<usize> {
        self.param_range.clone()
    }

    /// A private [`Clone`]
    pub(super) fn clone(&self) -> Self {
        Self {
            args: self.args.clone(),
            param_range: self.param_range.clone(),
            has_to_start_param: self.has_to_start_param,
            is_forming_param: self.is_forming_param,
            flags: self.flags.clone(),
        }
    }
}

/// The flags passed to a command
///
/// They work just like flags on regular Linux commands, i.e., you
/// have word flags, like `"--global"`, and glob flags, like `"-aBc"`.
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

    let args = ArgsIter::new(command);
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

/// A iterator over arguments in a `&str`, useful for the [`cmd`]
/// module
///
/// [`cmd`]: super
#[derive(Clone)]
pub struct ArgsIter<'a> {
    command: &'a str,
    chars: std::str::CharIndices<'a>,
    start: Option<usize>,
    end: Option<usize>,
    is_quoting: bool,
    last_char: char,
}

impl<'a> ArgsIter<'a> {
    /// Returns a new iterator over arguments in a `&str`
    pub fn new(command: &'a str) -> Self {
        let mut args_iter = Self {
            command,
            chars: command.char_indices(),
            start: None,
            end: None,
            is_quoting: false,
            // Initial value doesn't matter, as long as it's not '\'
            last_char: 'a',
        };

        args_iter.next();
        args_iter
    }
}

impl<'a> Iterator for ArgsIter<'a> {
    type Item = (&'a str, Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((b, char)) = self.chars.next() {
            let lc = self.last_char;
            self.last_char = char;
            if self.start.is_some() && char.is_whitespace() && !self.is_quoting {
                self.end = Some(b);
                break;
            } else if char == '"' && lc != '\\' {
                self.is_quoting = !self.is_quoting;
                if !self.is_quoting {
                    self.end = Some(b + 1);
                    break;
                } else {
                    self.start = Some(b);
                }
            } else if !char.is_whitespace() && self.start.is_none() {
                self.start = Some(b);
            }
        }

        let e = self.end.take().unwrap_or(self.command.len());
        self.start.take().map(|s| (&self.command[s..e], s..e))
    }
}

macro_rules! parse_impl {
    ($t:ty) => {
        impl Parameter for $t {
            fn new(_: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
                let arg = args.next()?;
                let arg = arg
                    .parse()
                    .map_err(|_| txt!("[a]{arg}[] couldn't be parsed as [a]{}[]", stringify!($t)));
                arg.map(|arg| (arg, None))
            }
        }
    };
}

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
