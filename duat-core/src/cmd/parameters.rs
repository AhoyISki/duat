//! Defines the processing of parameters in commands
//!
//! This processing first separates the [`Flags`] and [`Args`] of the
//! call, and then transforms the list of arguments into a list of
//! [`Parameter`]s, as defined by the command. Each [`Parameter`] may
//! take multiple words, which makes this structure very flexible for
//! multiple branching paths on how to read the arguments, all from
//! the same command.
use std::{iter::Peekable, marker::PhantomData, ops::Range, path::PathBuf};

use crossterm::style::Color;

use crate::{
    context::{self, Handle},
    data::Pass,
    form::{self, FormId},
    text::{Text, txt},
    ui::{Node, Ui, Widget},
};

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
pub trait Parameter<'a>: Sized {
    /// The type that is returned
    type Returns;
    /// Tries to consume arguments until forming a parameter
    ///
    /// Since parameters shouldn't mutate data, pa is just a regular
    /// shared reference.
    fn new(pa: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text>;
}

impl<'a, P: Parameter<'a>> Parameter<'a> for Option<P> {
    type Returns = Option<P::Returns>;

    /// Will match either [`Parameter`] given, or nothing
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(pa: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
        match args.next_as::<P>(pa) {
            Ok(arg) => Ok((Some(arg), None)),
            Err(err) if args.is_forming_param => Err(err),
            Err(_) => Ok((None, None)),
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
    fn new(pa: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
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

impl<'a, const N: usize, P: Parameter<'a>> Parameter<'a> for [P; N] {
    type Returns = [P::Returns; N];

    /// Will match either the argument given, or nothing
    ///
    /// This, like other lists, _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(pa: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
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
pub struct Between<const MIN: usize, const MAX: usize, P>(PhantomData<P>);

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
    fn new(pa: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
        let mut returns = Vec::new();

        for _ in 0..MAX {
            match args.next_as::<P>(pa) {
                Ok(ret) => returns.push(ret),
                Err(err) if args.is_forming_param => return Err(err),
                Err(_) if returns.len() >= MIN => return Ok((returns, None)),
                Err(err) => return Err(err),
            }
        }

        if returns.len() >= MIN {
            Ok((returns, None))
        } else {
            Err(txt!(
                "List needed at least [a]{MIN}[] elements, got only [a]{}",
                returns.len()
            )
            .build())
        }
    }
}

impl<'a> Parameter<'a> for &'a str {
    type Returns = &'a str;

    fn new(_: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
        args.next().map(|arg| (arg, None))
    }
}

impl Parameter<'_> for String {
    type Returns = String;

    fn new(_: &Pass, args: &mut Args) -> Result<(Self::Returns, Option<FormId>), Text> {
        Ok((args.next()?.to_string(), None))
    }
}

/// Command [`Parameter`]: The remaining arguments, divided by a space
///
/// Fails if the [`String`] would be empty.
pub struct Remainder;

impl Parameter<'_> for Remainder {
    type Returns = String;

    fn new(_: &Pass, args: &mut Args) -> Result<(Self::Returns, Option<FormId>), Text> {
        let remainder: String = std::iter::from_fn(|| args.next().ok())
            .collect::<Vec<&str>>()
            .join(" ");
        if remainder.is_empty() {
            Err(txt!("There are no more arguments").build())
        } else {
            Ok((remainder, None))
        }
    }
}

/// Command [`Parameter`]: An existing [`ColorScheme`]'s name
///
/// [`ColorScheme`]: crate::form::ColorScheme
pub struct ColorSchemeArg;

impl<'a> Parameter<'a> for ColorSchemeArg {
    type Returns = &'a str;

    fn new(_: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
        let scheme = args.next()?;
        if crate::form::colorscheme_exists(scheme) {
            Ok((scheme, None))
        } else {
            Err(txt!("The colorscheme [a]{scheme}[] was not found").build())
        }
    }
}

/// Command [`Parameter`]: An open [`File`]'s name
///
/// [`File`]: crate::file::File
pub struct Buffer<U>(PhantomData<U>);

impl<'a, U: crate::ui::Ui> Parameter<'a> for Buffer<U> {
    type Returns = &'a str;

    fn new(pa: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
        let buffer = args.next()?;
        if crate::context::windows::<U>()
            .file_handles(pa)
            .map(|handle| handle.read(pa).name())
            .any(|f| f == buffer)
        {
            Ok((buffer, Some(form::id_of!("param.file.open"))))
        } else {
            Err(txt!("No buffer called [a]{buffer}[] open").build())
        }
    }
}

/// Command [`Parameter`]: An open [`File`]'s name, except the current
///
/// [`File`]: crate::file::File
pub struct OtherFileBuffer<U>(PhantomData<U>);

impl<'a, U: crate::ui::Ui> Parameter<'a> for OtherFileBuffer<U> {
    type Returns = &'a str;

    fn new(pa: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
        let buffer = args.next_as::<Buffer<U>>(pa)?;
        let handle = crate::context::fixed_file::<U>(pa).unwrap();
        if buffer == handle.read(pa).name() {
            Err(txt!("Argument can't be the current file").build())
        } else {
            Ok((buffer, Some(form::id_of!("param.file.open"))))
        }
    }
}

/// Command [`Parameter`]: A [`File`] whose parent is real
///
/// [`File`]: crate::file::File
pub struct ValidFile<U>(PhantomData<U>);

impl<U: Ui> Parameter<'_> for ValidFile<U> {
    type Returns = PathBuf;

    fn new(pa: &Pass, args: &mut Args) -> Result<(Self::Returns, Option<FormId>), Text> {
        let path = args.next_as::<PathBuf>(pa)?;

        let canon_path = path.canonicalize();
        let path = if let Ok(path) = &canon_path {
            if !path.is_file() {
                return Err(txt!("Path is not a file").build());
            }
            path.clone()
        } else if canon_path.is_err()
            && let Ok(canon_path) = path.with_file_name(".").canonicalize()
        {
            canon_path.join(
                path.file_name()
                    .ok_or_else(|| txt!("Path has no file name"))?,
            )
        } else {
            return Err(txt!("Path was not found").build());
        };

        let form = if crate::context::windows::<U>()
            .file_handles(pa)
            .map(|handle| handle.read(pa).path())
            .any(|p| p == path)
        {
            form::id_of!("param.file.open")
        } else if let Ok(true) = path.try_exists() {
            form::id_of!("param.file.exists")
        } else {
            form::id_of!("param.file")
        };

        Ok((path, Some(form)))
    }
}

/// Command [`Parameter`]: An [`f32`] from a [`u8`] or a percentage
///
/// The percentage is of whole divisions of 100, 100 being equivalent
/// to 255 in [`u8`].
pub struct F32PercentOfU8;

impl Parameter<'_> for F32PercentOfU8 {
    type Returns = f32;

    fn new(_: &Pass, args: &mut Args) -> Result<(Self::Returns, Option<FormId>), Text> {
        let arg = args.next()?;
        if let Some(percentage) = arg.strip_suffix("%") {
            let percentage: u8 = percentage
                .parse()
                .map_err(|_| txt!("[a]{arg}[] is not a valid percentage").build())?;
            if percentage <= 100 {
                Ok((percentage as f32 / 100.0, None))
            } else {
                Err(txt!("[a]{arg}[] is more than [a]100%").build())
            }
        } else {
            let byte: u8 = arg
                .parse()
                .map_err(|_| txt!("[a]{arg}[] couldn't be parsed"))?;
            Ok((byte as f32 / 255.0, None))
        }
    }
}

impl<'a> Parameter<'a> for Color {
    type Returns = Color;

    fn new(pa: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
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
                _ => return Err(txt!("Hexcode does not contain 6 hex values").build()),
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
            let hue = args.next_as::<F32PercentOfU8>(pa)?;
            let sat = args.next_as::<F32PercentOfU8>(pa)?;
            let lit = args.next_as::<F32PercentOfU8>(pa)?;
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
            Err(txt!("Color format was not recognized").build())
        }
    }
}

/// Command [`Parameter`]: The name of a [`Form`] that has been [set]
///
/// [set]: crate::form::set
/// [`Form`]: crate::form::Form
pub struct FormName;

impl<'a> Parameter<'a> for FormName {
    type Returns = &'a str;

    fn new(_: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
        let arg = args.next()?;
        if !arg.chars().all(|c| c.is_ascii_alphanumeric() || c == '.') {
            return Err(txt!("Expected identifiers separated by '.'s, found [a]{arg}").build());
        }
        if crate::form::exists(arg) {
            Ok((arg, Some(form::id_of_non_static(arg))))
        } else {
            Err(txt!("The form [a]{arg}[] has not been set").build())
        }
    }
}

/// Command [`Parameter`]: [`Handle`]s for a given type of [`Widget`]
///
/// This [`Parameter`] lets you act upon [`Handle`]s of a type of
/// [`Widget`] with the following methods:
///
/// - [`on_current`]: Acts on the current, most relevant instance.
/// - [`on_each`]: Acts on every instance, on every window.
/// - [`on_window`]: Acts on all instances on the current window.
/// - [`on_flags`]: Acts based on [`Flags`] passed, `"global"` for
///   [`on_each`], `"window"` for [`on_window`].
///
/// [`on_current`]: Self::on_current
/// [`on_each`]: Self::on_each
/// [`on_window`]: Self::on_window
/// [`on_flags`]: Self::on_flags
pub struct Handles<'a, W: Widget<U>, U: Ui>(Flags<'a>, PhantomData<(W, U)>);

impl<'a, W: Widget<U>, U: Ui> Handles<'a, W, U> {
    /// Acts on a [`Handle`] of the most relevant instance of `W`
    ///
    /// The most relevant instance is determined in this order:
    ///
    /// - A [`Widget`] pushed to the currently active [`File`].
    /// - A [`Widget`] pushed to the currently active window.
    /// - A [`Widget`] that is open anywhere, starting from the next
    ///   windows, ending on the previous.
    ///
    /// Do note that this function will only trigger once, so if there
    /// are multiple instances of the [`Widget`] with the same level
    /// of relevance, the first one that was pushed is the one that
    /// will be picked. Since this is only triggered once, it can also
    /// return a value.
    ///
    /// This function will be called by [`Handles::on_flags`], if no
    /// context choosing [`Flags`] are passed.
    ///
    /// [`File`]: crate::file::File
    pub fn on_current<Ret>(
        &self,
        pa: &mut Pass,
        f: impl FnOnce(&mut Pass, Handle<W, U>) -> Ret,
    ) -> Option<Ret> {
        if let Some(handle) = context::fixed_file::<U>(pa).unwrap().get_related(pa) {
            Some(f(pa, handle))
        } else {
            let w = context::cur_window();

            let node = match context::windows::<U>()
                .widget_entry::<W>(pa, w)
                .map(|(.., node)| node.clone())
            {
                Ok(node) => node,
                Err(err) => {
                    context::error!("{err}");
                    return None;
                }
            };

            Some(f(pa, node.try_downcast().unwrap()))
        }
    }

    /// Acts on a each [`Handle`] of any instance of `W`
    ///
    /// This will trigger in all windows, starting from the first, in
    /// the order that they were pushed.
    ///
    /// Unlike [`Handles::on_current`], this can act on multiple
    /// instances of the same type of [`Widget`] pushed to a single
    /// window.
    ///
    /// This function will be called by [`Handles::on_flags`], if the
    /// `"--global"` or `"-g"` [`Flags`] are passed.
    pub fn on_each(&self, pa: &mut Pass, mut f: impl FnMut(&mut Pass, Handle<W, U>)) {
        let handles: Vec<_> = context::windows::<U>().handles(pa).cloned().collect();
        for handle in handles {
            if let Some(handle) = handle.try_downcast() {
                f(pa, handle)
            }
        }
    }

    /// Acts on each [`Handle`] of `W` on the current window
    ///
    /// Will trigger in the order that they were pushed.
    ///
    /// Unlike [`Handles::on_current`], this can act on multiple
    /// instances of the same type of [`Widget`] pushed to a single
    /// window.
    ///
    /// This function will be called by [`Handles::on_flags`], if the
    /// `"--window"` or `"-w"` [`Flags`] are passed.
    pub fn on_window(&self, pa: &mut Pass, mut f: impl FnMut(&mut Pass, Handle<W, U>)) {
        let cur_win = context::cur_window();
        let nodes: Vec<Node<U>> = context::windows::<U>()
            .entries(pa)
            .filter(|&(win, ..)| win == cur_win)
            .map(|(.., node)| node.clone())
            .collect();

        for handle in nodes.iter().map(Node::handle) {
            if let Some(handle) = handle.try_downcast() {
                f(pa, handle)
            }
        }
    }

    /// Acts on [`Handle`]s of `W`, based on which[`Flags`] were
    /// passed
    ///
    /// If the `"--global"` [`word`] or the `"-g"` [`blob`] flag are
    /// passed, then will call [`Handles::on_each`]. If `"--window"`
    /// or `"w"` are passed, then will call [`Handles::on_window`].
    /// Otherwise, will call [`Handles::on_current`].
    ///
    /// If there are conflicting contexts, e.g. `"--global -w"`, then
    /// nothing will be done, and an error will be notified.
    ///
    /// [`word`]: Flags::word
    /// [`blob`]: Flags::blob
    pub fn on_flags(&self, pa: &mut Pass, f: impl FnMut(&mut Pass, Handle<W, U>)) {
        let is_global = self.0.word("global") || self.0.blob("g");
        let is_window = self.0.word("window") || self.0.blob("w");
        if is_global && !is_window {
            self.on_each(pa, f);
        } else if is_window && !is_global {
            self.on_window(pa, f);
        } else if !is_global && !is_window {
            self.on_current(pa, f);
        } else {
            context::error!(
                "Multiple contexts chosen, either pick a [a]global[] context or a [a]window[] \
                 context"
            )
        }
    }
}

impl<'a, W: Widget<U>, U: Ui> Parameter<'a> for Handles<'a, W, U> {
    type Returns = Self;

    fn new(_: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
        Ok((Self(args.flags.clone(), PhantomData), None))
    }
}

impl<'a> Parameter<'a> for Flags<'a> {
    type Returns = Flags<'a>;

    fn new(_: &Pass, args: &mut Args<'a>) -> Result<(Self::Returns, Option<FormId>), Text> {
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
#[derive(Clone)]
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
            None => Err(txt!("Wrong argument count").build()),
        }
    }

    /// Tries to parse the next argument as `P`
    ///
    /// For now, this will consume arguments even if it fails, but
    /// that may change in the future.
    pub fn next_as<P: Parameter<'a>>(&mut self, pa: &Pass) -> Result<P::Returns, Text> {
        self.has_to_start_param = true;
        let ret = P::new(pa, self);
        if ret.is_ok() {
            self.is_forming_param = false;
        }
        ret.map(|(arg, _)| arg)
    }

    /// Tries to parse the next argument as `P`, otherwise returns a
    /// [`Text`]
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
    /// [`PromptLine`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
    pub fn next_start(&mut self) -> Option<usize> {
        self.args.peek().map(|(_, r)| r.start)
    }

    /// The range of the previous [`Parameter`]
    ///
    /// Mostly used for error feedback by the [`PromptLine`]
    ///
    /// [`PromptLine`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
    pub fn param_range(&self) -> Range<usize> {
        self.param_range.clone()
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

/// The [`Iterator`] over the [`Parameter`]s of the command
#[define_opaque(ArgsIter)]
pub fn args_iter(command: &str) -> ArgsIter<'_> {
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

        let e = end.take().unwrap_or(command.len());
        start.take().map(|s| (&command[s..e], s..e))
    });
    args.next();
    args
}

/// An [`Iterator`] over the arguments in a command call
#[doc(hidden)]
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

        fn new(_: &Pass, args: &mut Args) -> Result<(Self::Returns, Option<FormId>), Text> {
            let arg = args.next()?;
            let arg = arg.parse().map_err(|_| {
                txt!("[a]{arg}[] couldn't be parsed as [a]{}[]", stringify!($t)).build()
            });
            arg.map(|arg| (arg, None))
        }
    }
}
