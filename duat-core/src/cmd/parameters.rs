//! Defines the processing of parameters in commands
//!
//! This processing first separates the [`Args`] of the call and then
//! transforms the list of arguments into a list of [`Parameter`]s, as
//! defined by the command. Each [`Parameter`] may take multiple
//! words, which makes this structure very flexible for
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

/// Command [`Parameter`]: A flag passed to the command
///
/// `Flag`s in duat differ from those of UNIX like operating system
/// commands, since a flag can show up anywhere, not just before some
/// standalone `--` which separates flags and "not flags". Instead,
/// what determines if an argument starting with `--` or `-` is a flag
/// or not is if said argument is _quoted_:
///
/// ```text
/// mycmd --this-is-a-flag "--this-is not a flag" -blobflag -- --flag
/// ```
pub enum Flag<S: AsRef<str> = String> {
    /// A word flag is prefixed by `--` and represents only one thing
    ///
    /// Examples of this are the `--cfg` and `--cfg-manifest`, which
    /// are used by the `edit` and `open` commands to open Duat
    /// configuration files.
    Word(S),
    /// A blob flag is prefixed by `-` and represents one thing per
    /// `char`
    ///
    /// An example, coming from UNIX like operating systems is `rm
    /// -rf`, witch will forcefully (`f`) remove files recursively
    /// (`r`).
    Blob(S),
}

impl Parameter for Flag {
    fn new(_: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let arg = args.next()?;
        if !arg.is_quoted {
            if let Some(word) = arg.strip_prefix("--") {
                Ok((
                    Flag::Word(word.to_string()),
                    Some(form::id_of!("param.flag")),
                ))
            } else if let Some(blob_chars) = arg.strip_prefix("-") {
                let mut blob = String::new();
                for char in blob_chars.chars() {
                    if !blob.chars().any(|c| c == char) {
                        blob.push(char);
                    }
                }

                Ok((Flag::Blob(blob), Some(form::id_of!("param.flag"))))
            } else {
                Err(txt!("[param.info]Flag[]s must start with `-` or `--`"))
            }
        } else {
            Err(txt!("Quoted arguments can't be [param.info]Flag[]s"))
        }
    }
}

impl<S: AsRef<str>> Flag<S> {
    /// Returns `Ok` only if the `Flag` is of type [`Flag::Word`]
    pub fn as_word(self) -> Result<S, Text> {
        match self {
            Flag::Word(word) => Ok(word),
            Flag::Blob(_) => Err(txt!(
                "[param.info]Flag[] is of type [param.info]blob[], not [param.info]word"
            )),
        }
    }

    /// Returns true if this `Flag` is a `Flag::Word(word)`
    pub fn is_word(&self, word: &str) -> bool {
        self.as_str().as_word().ok().is_some_and(|w| w == word)
    }

    /// Returns `Ok` only if the `Flag` is of type [`Flag::Blob`]
    pub fn as_blob(self) -> Result<S, Text> {
        match self {
            Flag::Blob(blob) => Ok(blob),
            Flag::Word(_) => Err(txt!(
                "[param.info]Flag[] is of type [param.info]word[], not [param.info]blob"
            )),
        }
    }

    /// Returns true if this `Flag` is a [`Flag::Blob`] with all
    /// characters in `blob`
    pub fn has_blob(&self, blob: &str) -> bool {
        self.as_str()
            .as_blob()
            .ok()
            .is_some_and(|b| blob.chars().all(|char| b.chars().any(|c| c == char)))
    }

    /// Returns an [`Err`] if the `Flag` is a blob or doesn't belong
    /// on the list
    ///
    /// this is useful for quickly matching against a fixed list of
    /// possible words, while having a "catchall `_ => {}`, which will
    /// never match.
    pub fn word_from_list<const N: usize>(self, list: [&str; N]) -> Result<&str, Text> {
        let word = self.as_word()?;
        if let Some(word) = list.into_iter().find(|w| w == &word.as_ref()) {
            Ok(word)
        } else {
            Err(txt!("Word not in list of valid options"))
        }
    }

    /// Returns a new `Flag<&str>`
    ///
    /// This is particularly useful in pattern matching.
    pub fn as_str(&self) -> Flag<&str> {
        match self {
            Flag::Word(word) => Flag::Word(word.as_ref()),
            Flag::Blob(blob) => Flag::Blob(blob.as_ref()),
        }
    }
}

/// Command [`Parameter`]: A list of [`Flag`]s passed to the command
///
/// This `Parameter` will capture all following arguments, until it
/// finds one that can't be parsed as a `Flag` (i.e. not starting with
/// `--` or `-`, or quoted arguments).
///
/// Unlike [`Vec`], this `Parameter` _can_ be followed up by other
/// ones, and if there are no `Flag`s, then this will have an empty
/// list. As such, this function never actually fails.
pub struct Flags(pub Vec<Flag>);

impl Parameter for Flags {
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let mut list = Vec::new();

        while let Ok((flag, _)) = args.try_next_as(pa) {
            list.push(flag);
        }

        Ok((Self(list), Some(form::id_of!("param.flag"))))
    }
}

impl Flags {
    /// Returns `true` if the `Flags` contains a [`Flag::Word`] with
    /// the given word
    pub fn has_word(&self, word: &str) -> bool {
        self.0
            .iter()
            .any(|flag| flag.as_str().as_word() == Ok(word))
    }

    /// Returns `true` if the `Flags` contains [`Flag::Blob]s with
    /// all `char`s in the given blob
    pub fn has_blob(&self, blob: &str) -> bool {
        blob.chars().all(|char| {
            self.0
                .iter()
                .filter_map(|flag| flag.as_str().as_blob().ok())
                .any(|blob| blob.chars().any(|c| c == char))
        })
    }
}

impl std::ops::Deref for Flags {
    type Target = Vec<Flag>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Flags {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Command [`Parameter`]: Global or local scope for commands
///
/// This struct captures a [`Flag`] if it exists, if it is `--global`,
/// then the scope is global. If the flag is something else, it
/// returns an [`Err`]. If there is no `Flag` then the scope is
/// [`Scope::Local`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Scope {
    /// The scope of this command is meant to be local
    ///
    /// This is a loose definition. Usually, it means it should affect
    /// stuff related only to the current [`Buffer`], but you can
    /// decide that.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    Local,
    /// The scope of this command is meant to be global
    ///
    /// This is a loose definition. Usually, it means it should affect
    /// stuff related to _all_ [`Buffer`]s, but you decide what it
    /// really means.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    Global,
}

impl Parameter for Scope {
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        if let Ok((flag, form)) = args.try_next_as::<Flag>(pa) {
            if flag.is_word("global") {
                Ok((Scope::Global, form))
            } else {
                Err(txt!(
                    "Invalid [param.info]Flag[], it can only be [param.info]--global"
                ))
            }
        } else {
            Ok((Scope::Local, None))
        }
    }
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
                "List needed at least [param.info]{MIN}[] elements, got only [a]{}",
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
        let remainder: String = std::iter::from_fn(|| args.next().ok().map(|arg| arg.value))
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

impl Parameter for Handle {
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let buffer_name = args.next()?.value;
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
        let path = PathBuf::from(
            shellexpand::full(args.next()?.value)
                .map_err(|err| txt!("{err}"))?
                .into_owned(),
        );

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

/////////// Command specific parameters

/// Comand [`Parameter`]: A [`ValidFilePath`], [`Handle`], `--cfg` or
/// `--cfg-manifest`
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
        if let Ok((flag, _)) = args.try_next_as::<Flag>(pa) {
            match flag.as_word()?.as_str() {
                "cfg" => Ok((Self::Cfg, None)),
                "cfg-manifest" => Ok((Self::CfgManifest, None)),
                _ => Err(txt!(
                    "Invalid flag, pick [param.ok]cfg or [param.info]cfg-manifest[]"
                )),
            }
        } else if let Ok((handle, form)) = args.try_next_as::<Handle>(pa) {
            Ok((Self::Buffer(handle), form))
        } else {
            let (path, form) = args.try_next_as::<ValidFilePath>(pa)?;
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

        let arg = args.next()?.value;
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
        let arg = args.next()?.value;
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

/// Command [`Parameter`]: An existing [`ColorScheme`]'s name
///
/// [`ColorScheme`]: crate::form::ColorScheme
pub struct ColorSchemeArg(pub String);

impl Parameter for ColorSchemeArg {
    fn new(_: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let scheme = args.next()?.value;
        if crate::form::colorscheme_exists(scheme) {
            Ok((ColorSchemeArg(scheme.to_string()), None))
        } else {
            Err(txt!("The colorscheme [a]{scheme}[] was not found"))
        }
    }
}
implDeref!(ColorSchemeArg, String);

/// Command [`Parameter`]: Options for reloading
pub(super) struct ReloadOptions {
    /// Wether to clean
    pub clean: bool,
    /// Wether to update
    pub update: bool,
}

impl Parameter for ReloadOptions {
    fn new(pa: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
        let flags = args.next_as::<Flags>(pa)?;

        if flags
            .iter()
            .any(|flag| !flag.is_word("update") && !flag.is_word("clean"))
        {
            Err(txt!("Invalid [a]Flag"))
        } else {
            Ok((
                Self {
                    clean: flags.has_word("clean"),
                    update: flags.has_word("update"),
                },
                Some(form::id_of!("param.flag")),
            ))
        }
    }
}

/// The list of arguments passed to a command
pub struct Args<'a> {
    args: Peekable<ArgsIter<'a>>,
    param_range: Range<usize>,
    has_to_start_param: bool,
    is_forming_param: bool,
}

impl<'arg> Args<'arg> {
    /// Returns a new instance of `Args`
    pub(super) fn new(command: &'arg str) -> Self {
        Self {
            args: ArgsIter::new(command).peekable(),
            param_range: 0..0,
            has_to_start_param: false,
            is_forming_param: false,
        }
    }

    /// Returns the next word or quoted argument
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<Arg<'arg>, Text> {
        match self.args.next() {
            Some((value, range, is_quoted)) => {
                self.param_range = range.clone();
                if self.has_to_start_param {
                    self.has_to_start_param = false;
                    self.is_forming_param = true;
                }
                Ok(Arg { value, is_quoted })
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
    pub fn try_next_as<P: Parameter>(&mut self, pa: &Pass) -> Result<(P, Option<FormId>), Text> {
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
    pub fn next_else<T: Into<Text>>(&mut self, to_text: T) -> Result<&'arg str, Text> {
        match self.args.next() {
            Some((arg, ..)) => Ok(arg),
            None => Err(to_text.into()),
        }
    }

    /// Returns the char position of the next argument
    ///
    /// Mostly used for error feedback by the [`PromptLine`]
    ///
    /// [`PromptLine`]: docs.rs/duat/latest/duat/widgets/struct.PromptLine.html
    pub fn next_start(&mut self) -> Option<usize> {
        self.args.peek().map(|(_, r, _)| r.start)
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
        }
    }
}

/// An arguemnt that was passed to a command
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Arg<'arg> {
    /// The `&str` of that argument
    pub value: &'arg str,
    /// Wether said argument was quoted with `"`s
    pub is_quoted: bool,
}

impl<'arg> std::ops::Deref for Arg<'arg> {
    type Target = &'arg str;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'arg, 'other> PartialEq<&'other str> for Arg<'arg> {
    fn eq(&self, other: &&'other str) -> bool {
        &self.value == other
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
    type Item = (&'a str, Range<usize>, bool);

    fn next(&mut self) -> Option<Self::Item> {
        let mut is_quoted = false;
        while let Some((b, char)) = self.chars.next() {
            let lc = self.last_char;
            self.last_char = char;
            if self.start.is_some() && char.is_whitespace() && !self.is_quoting {
                self.end = Some(b);
                break;
            } else if char == '"' && lc != '\\' {
                self.is_quoting = !self.is_quoting;
                if !self.is_quoting {
                    is_quoted = true;
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
        self.start
            .take()
            .map(|s| (&self.command[s..e], s..e, is_quoted))
    }
}

macro_rules! parse_impl {
    ($t:ty) => {
        impl Parameter for $t {
            fn new(_: &Pass, args: &mut Args) -> Result<(Self, Option<FormId>), Text> {
                let arg = args.next()?;
                let arg = arg.parse().map_err(|_| {
                    txt!(
                        "[a]{arg}[] couldn't be parsed as [param.info]{}[]",
                        stringify!($t)
                    )
                });
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
