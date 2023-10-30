use std::{
    any::TypeId,
    iter::Peekable,
    str::{FromStr, SplitWhitespace},
};

use crate::text::{text, Text};

/// The non flag arguments that were passed to the caller.
///
/// The first argument not prefixed with a "`-`" or a "`--`" will turn
/// all remaining arguments into non flag arguments, even if they have
/// those prefixes.
///
/// # Examples
///
/// ```rust
/// # use duat_core::commands::{split_flags_and_args};
/// let call = "command --foo -bar notflag --foo --baz -abfgh";
/// let (flags, mut args) = split_flags_and_args(call);
///
/// assert!(flags.short("bar"));
/// assert!(flags.long("foo"));
/// assert_eq!(args.collect::<Vec<&str>>(), vec![
///     "notflag", "--foo", "--baz", "-abfgh"
/// ]);
/// ```
///
/// You can also make that happen by introducing an empty "`--`"
/// argument:
///
/// ```rust
/// # use duat_core::commands::{split_flags_and_args};
/// let call = "command --foo -bar -- --foo --baz -abfgh";
/// let (flags, mut args) = split_flags_and_args(call);
///
/// assert!(flags.short("bar"));
/// assert!(flags.long("foo"));
/// assert_eq!(args.collect::<Vec<&str>>(), vec![
///     "--foo", "--baz", "-abfgh"
/// ]);
/// ```
#[derive(Clone)]
pub struct Args<'a> {
    count: usize,
    expected: Option<usize>,
    args: Peekable<SplitWhitespace<'a>>,
}

impl<'a> Args<'a> {
    /// Returns the next argument, if there is one.
    ///
    /// Since this method is supposed to be used inside of a command,
    /// it returns an error that can easily be returned by `?`,
    /// exiting the function with an appropriate error message,
    /// formated to be shown somewhere in the editor.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "run away i'll kill you 👹";
    /// let (flags, mut args) = split_flags_and_args(call);
    /// args.next();
    /// args.next();
    /// args.next();
    /// args.next();
    ///
    /// let ogre = args.next();
    /// assert_eq!(ogre, Ok("👹"));
    ///
    /// let error = args.next();
    /// let error_msg = text!(
    ///     "Expected at least " [AccentErr] 6 []
    ///     " arguments, got " [AccentErr] 5 [] "."
    /// );
    /// assert_eq!(error, Err(error_msg));
    /// ```
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> std::result::Result<&str, Text> {
        match self.args.next() {
            Some(arg) => {
                self.count += 1;
                Ok(arg)
            }
            None => Err({
                let expected = match self.expected {
                    Some(expected) => text!([AccentErr] expected),
                    None => text!("at least " [AccentErr] { self.count + 1 }),
                };
                let (args, received) = match self.count {
                    0 => (" arguments", text!([AccentErr] "none")),
                    1 => (" argument", text!([AccentErr] 1)),
                    count => (" arguments", text!([AccentErr] count)),
                };

                text!("Expected " expected [] args ", got " received [] ".")
            }),
        }
    }

    /// Attempts to parse the next argument, if there is one.
    ///
    /// This method will return an [`Err`] in two different ways,
    /// either there is no next argument, in which it defers to the
    /// error message of [`Args::next`], or the parsing fails, then it
    /// returns a custom built error message for that type.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "int-and-float 42 non-float-arg";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// let int = args.next_as::<usize>();
    /// assert_eq!(int, Ok(42));
    ///
    /// let float = args.next_as::<f32>();
    /// let error_msg = text!(
    ///     "Couldn't convert " [AccentErr] "non-float-arg" []
    ///     " to " [AccentErr] "f32" [] "."
    /// );
    /// assert_eq!(float, Err(error_msg));
    /// ```
    ///
    /// [`Args::next`]: Args::next
    pub fn next_as<F: FromStr>(&mut self) -> std::result::Result<F, Text> {
        let arg = self.next()?;
        arg.parse().map_err(|_| {
            text!(
                "Couldn't convert " [AccentErr] arg []
                " to " [AccentErr] { std::any::type_name::<F>() } [] "."
            )
        })
    }

    /// Returns the next argument, if there is one, otherwise, returns
    /// a custom error message.
    ///
    /// This method will replace the usual "not enough arguments"
    /// error message from the [`Args::next`] method by a [`Text`]
    /// provided by the user itself, usually with the [`text`] macro.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "expects-2-and-file arg-1 not-quite";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// let first = args.next();
    /// assert_eq!(first, Ok("arg-1"));
    ///
    /// let second = args.next();
    /// assert_eq!(second, Ok("not-quite"));
    ///
    /// let msg = text!("I expected a " [Wack] "file" [] ", damnit!");
    /// let float = args.next_else(msg.clone());
    /// assert_eq!(float, Err(msg));
    /// ```
    ///
    /// [`Args::next`]: Args::next
    pub fn next_else(&mut self, text: Text) -> std::result::Result<&str, Text> {
        match self.args.next() {
            Some(arg) => {
                self.count += 1;
                Ok(arg)
            }
            None => Err(text),
        }
    }

    /// Optional function to return an error message in case there are
    /// more arguments than expected.
    ///
    /// This is an optional function, in case you want to complain if
    /// the user passes too many arguments. Of course, you could just
    /// ignore them.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "just-1-arg,man arg-1 too-many wayy tooo many";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// let first = args.next();
    /// assert_eq!(first, Ok("arg-1"));
    ///
    /// let error = args.ended();
    /// let msg = text!(
    ///     "Expected " [AccentErr] 1 []
    ///     " argument, received " [AccentErr] 5 [] " instead."
    /// );
    /// assert_eq!(error, Err(msg));
    /// ```
    ///
    /// [`Args::next`]: Args::next
    pub fn ended(&mut self) -> std::result::Result<(), Text> {
        match self.args.clone().count() {
            0 => Ok(()),
            count => Err({
                let args = match self.count == 1 {
                    true => " argument",
                    false => " arguments",
                };
                text!(
                    "Expected " [AccentErr] { self.count } [] args
                    ", received " [AccentErr] { self.count + count } [] " instead."
                )
            }),
        }
    }

    /// Collects the remaining arguments.
    ///
    /// This is similar to any [`Iterator::collect`], but it will
    /// collect differently depending on what struct is being used.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "runner arg1 arg2 arg3 arg4";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// let vector: Vec<&str> = args.clone().collect();
    /// assert_eq!(vector, vec!["arg1", "arg2", "arg3", "arg4"]);
    ///
    /// // In strings, the arguments are joined by a " ".
    /// let string: String = args.collect();
    /// assert_eq!(&string, "arg1 arg2 arg3 arg4");
    /// ```
    ///
    /// [`Args::next`]: Args::next
    pub fn collect<B: FromIterator<&'a str> + 'static>(&mut self) -> B {
        let args: Vec<&str> = (&mut self.args).collect();

        if TypeId::of::<B>() == TypeId::of::<String>() {
            B::from_iter(args.into_iter().intersperse(" "))
        } else {
            B::from_iter(args)
        }
    }

    /// Sets an expected value for the number of arguments.
    ///
    /// This will change the default [`Args::next`] error message, so
    /// that it shows how many arguments were actually expected.
    ///
    /// The reason why this method is here, instead of the command's
    /// creator being able to set a specified number of arguments per
    /// command when creating the given command, is because the number
    /// of arguments to any given command may vary, depending on the
    /// specifics of said command's implementation.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "expects-5 arg1 arg2 ";
    /// let (flags, mut args) = split_flags_and_args(call);
    /// args.set_expected(5);
    /// args.next();
    /// args.next();
    ///
    /// let error = args.next();
    /// let error_msg = text!(
    ///     "Expected " [AccentErr] 5 []
    ///     " arguments, got " [AccentErr] 2 [] "."
    /// );
    /// assert_eq!(error, Err(error_msg));
    /// ```
    pub fn set_expected(&mut self, expected: usize) {
        self.expected = Some(expected);
    }
}

/// A struct representing flags passed down to [`Command`]s when
/// running them.
///
/// There are 2 types of flag, the `short` and `long` flags.
///
/// `short` flags represent singular characters passed after a
/// single `'-'` character, they can show up in multiple
/// places, and should represent an incremental addition of
/// features to a command.
///
/// `long` flags are words that come after any `"--"` sequence,
/// and should represent more verbose, but more readable
/// versions of `short` flags.
///
/// # Examples
///
/// Both `short` and `long` flags can only be counted once, no
/// matter how many times they show up:
///
/// ```rust
/// # use duat_core::commands::{split_flags_and_args};
/// let call = "my-command --foo -abcde --foo --bar -abfgh arg1";
/// let (flags, mut args) = split_flags_and_args(call);
///
/// assert!(flags.short("abcdefgh"));
/// assert!(flags.long("foo") && flags.long("bar"));
/// assert_eq!(args.collect::<Vec<&str>>(), vec!["arg1"]);
/// ```
///
/// If you have any arguments that start with `'-'` or `"--"`, but
/// are not supposed to be flags, you can insert an empty
/// `"--"` after the flags, in order to distinguish them.
///
/// ```rust
/// # use duat_core::commands::{split_flags_and_args};
/// let call = "command --foo --bar -abcde -- --!flag -also-not";
/// let (flags, mut args) = split_flags_and_args(call);
///
/// assert!(flags.short("abcde"));
/// assert!(flags.long("foo") && flags.long("bar"));
/// assert_eq!(args.collect::<String>(), "--!flag -also-not")
/// ```
#[derive(Clone, Copy)]
pub struct Flags<'a, 'b>(&'a InnerFlags<'b>);

impl<'a, 'b> Flags<'a, 'b> {
    pub fn new(inner: &'a InnerFlags<'b>) -> Self {
        Self(inner)
    }

    /// Checks if all of the [`char`]s in the `short` passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::commands::split_flags_and_args;
    /// let call = "run -abcdefgh -ablk args -wz";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// assert!(flags.short("k"));
    /// assert!(!flags.short("w"));
    /// assert_eq!(args.collect::<Vec<&str>>(), vec!["args", "-wz"]);
    /// ```
    pub fn short(&self, short: impl AsRef<str>) -> bool {
        self.0.short(short)
    }

    /// Returns `true` if the `long` flag was passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::commands::split_flags_and_args;
    /// let call = "run --foo --bar args --baz";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// assert!(flags.long("foo"));
    /// assert!(!flags.long("baz"));
    /// assert_eq!(&args.collect::<String>(), "args --baz");
    /// ```
    pub fn long(&self, flag: impl AsRef<str>) -> bool {
        self.0.long(flag)
    }

    /// Returns `true` if no flags have been passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::commands::split_flags_and_args;
    /// let call = "run arg1 --foo --bar arg2 -baz";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// assert!(flags.is_empty());
    /// assert_eq!(args.collect::<Vec<&str>>(), vec![
    ///     "arg1", "--foo", "--bar", "arg2", "-baz"
    /// ]);
    /// ```
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// A struct representing flags passed down to [`Command`]s when
/// running them.
pub struct InnerFlags<'a> {
    short: String,
    long: Vec<&'a str>,
}

impl<'a> InnerFlags<'a> {
    /// Checks if all of the [`char`]s in the `short` passed.
    pub fn short(&self, short: impl AsRef<str>) -> bool {
        let mut all_chars = true;
        for char in short.as_ref().chars() {
            all_chars &= self.short.contains(char);
        }
        all_chars
    }

    /// Returns `true` if the `long` flag was passed.
    pub fn long(&self, flag: impl AsRef<str>) -> bool {
        self.long.contains(&flag.as_ref())
    }

    /// Returns `true` if no flags have been passed.
    pub fn is_empty(&self) -> bool {
        self.short.is_empty() && self.long.is_empty()
    }
}

/// Takes the [`Flags`] from an [`Iterator`] of `args`.
pub fn split_flags_and_args(command: &str) -> (InnerFlags<'_>, Args<'_>) {
    let mut short = String::new();
    let mut long = Vec::new();

    let mut args = command.split_whitespace().peekable();

    args.next();

    while let Some(arg) = args.peek() {
        if let Some(long_arg) = arg.strip_prefix("--") {
            if !long_arg.is_empty() {
                args.next();
                if !long.contains(&long_arg) {
                    long.push(long_arg)
                }
            } else {
                args.next();
                break;
            }
        } else if let Some(short_arg) = arg.strip_prefix('-') {
            args.next();
            for char in short_arg.chars() {
                if !short.contains(char) {
                    short.push(char)
                }
            }
        } else {
            break;
        }
    }

    (InnerFlags { short, long }, Args {
        count: 0,
        expected: None,
        args,
    })
}
