//! General options for Duat.
//!
//! These options apply _globally_ and mostly serve as convenience
//! methods to modify Duat's behavior. If you wish to apply them on a
//! case by case way, you should reach out for [hooks].
//!
//! The main way in which things are configured is through the
//! [`opts::set`] function, which serves as a way to set options for
//! newly opened [`Widget`]s and various other properties of Duat.
//!
//! If you want to remove the default `Buffer` widgets
//! ([`LineNumbers`] and [`VertRule`]), you can add:
//!
//! ```rust
//! # use duat::prelude::*;
//! hook::remove("BufferWidgets");
//! ```
//!
//! If you want to remove the [`FooterWidgets`] ([`StatusLine`],
//! [`PromptLine`] and [`Notifications`]), you can add:
//!
//! ```rust
//! # use duat::prelude::*;
//! hook::remove("FooterWidgets");
//! ```
//!
//! **WARNING**: If you do this, you'll lose access to a
//! [`PromptLine`], so you'll be unable to run commands. You can
//! manually readd it with [`PromptLineBuilder::push_on`]. Remember,
//! you can recompile your config with `duat --reload`.
//!
//! [widgets]: crate::widgets
//! [hooks]: crate::hook
//! [`Widget`]: crate::widgets::Widget
//! [`Buffer`]: crate::widgets::Buffer
//! [`LineNumbers`]: crate::widgets::LineNumbers
//! [`VertRule`]: duat_term::VertRule
//! [`StatusLine`]: crate::widgets::StatusLine
//! [`PromptLine`]: crate::widgets::PromptLine
//! [`Notifications`]: crate::widgets::Notifications
//! [`LogBook`]: crate::widgets::LogBook
//! [`FooterWidgets`]: crate::widgets::FooterWidgets
//! [`WhichKey`]: crate::widgets::WhichKey
//! [`opts::set`]: set
//! [`PromptLineBuilder::push_on`]: crate::widgets::PromptLineBuilder::push_on
use std::{
    any::TypeId,
    sync::{LazyLock, Mutex},
};

use duat_base::widgets::{LineNumbersOpts, LogBookOpts, StatusLineFmt};
#[allow(unused_imports)]
pub use duat_core::opts::*;
use duat_core::{
    data::Pass,
    mode::{Description, KeyCode, KeyEvent, KeyMod, Mode},
    text::Text,
    ui::Orientation,
};
pub use duatmode::TabMode;
use duatmode::opts::DuatModeOpts;

use crate::widgets::NotificationsOpts;

pub(crate) static OPTS: LazyLock<Mutex<StartOpts>> = LazyLock::new(Mutex::default);
pub(crate) static STATUSLINE_FMT: StatusLineFn = Mutex::new(None);

/// General options to set when starting Duat.
pub struct StartOpts {
    /// Highlights the current line.
    ///
    /// The default is `true`
    ///
    /// This makes use of the `current_line` [`Form`]
    ///
    /// [`Form`]: crate::form::Form
    pub highlight_current_line: bool,
    /// Enables wrapping of lines.
    ///
    /// The default is `true`
    pub wrap_lines: bool,
    /// Wrap on word boundaries, rather than on any character.
    ///
    /// The default is `false`.
    pub wrap_on_word: bool,
    /// Where to start wrapping.
    ///
    /// The default is `None`
    ///
    /// If this value is `None` and `opts.wrap_lines == false`, then
    /// wrapping will take place at the right edge of the screen.
    ///
    /// Otherwise, if it is `Some({cap})`, then wrapping will take
    /// place `{cap}` cells from the left edge. This value may or may
    /// not be greater than the width of the area. If it is greater
    /// than it, then wrapping will take place slightly outside the
    /// screen as a concequence.
    pub wrapping_cap: Option<u32>,
    /// Whether to indent wrapped lines or not.
    ///
    /// In [`Buffer`]s, the default is `true`.
    ///
    /// This turns this:
    ///
    /// ```text
    ///     This is a very long line of text, so long that it
    /// wraps around
    /// ```
    ///
    /// Into this:
    ///
    /// ```text
    ///     This is a very long line of text, so long that it
    ///     wraps around
    /// ```
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub indent_wraps: bool,
    /// How long tabs should be on screen.
    ///
    /// In [`Buffer`]s, the default is `4`
    ///
    /// This also affect other things, like if your tabs are converted
    /// into spaces, this will also set how many spaces should be
    /// added.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub tabstop: u8,
    /// How much space to keep between the cursor and edges.
    ///
    /// In [`Buffer`]s, the default is `ScrollOff { x: 3, y: 3 }`
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub scrolloff: ScrollOff,
    /// Whether to limit scrolloff at the end of lines.
    ///
    /// In [`Buffer`]s, the default is `false`
    ///
    /// This makes it so, as you reach the end of a long line of text,
    /// the cursor line will continue scrolling to the left,
    /// maintaining the `scrolloff.x`'s gap.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub force_scrolloff: bool,
    /// Extra characters to be considered part of a word.
    ///
    /// The default is `&[]`.
    ///
    /// Normally, word characters include all of those in the [`\w`]
    /// character set, which most importantly includes `[0-9A-Za-z_]`.
    ///
    /// You can use this setting to add more characters to that list,
    /// usually something like `-`, `$` or `@`, which are useful to
    /// consider as word characters in some circumstances.
    ///
    /// [`\w`]: https://www.unicode.org/reports/tr18/#word
    pub extra_word_chars: &'static [char],
    /// Indent string.
    ///
    /// The default is `Some("│")`.
    ///
    /// The indent lines will be printed with the `replace.indent`
    /// [`Form`].
    ///
    /// A string to replace the indentation at the start of the line.
    /// This string will repeat on every `opts.tabstop` initial spaces
    /// or on every `\t` character, replacing that many characters of
    /// the tab stop with those of the string.
    ///
    /// For example, if `tabstop == 2 && indent_str == Some("│   ")`,
    /// this:
    ///
    /// ```txt
    /// int (int var1, int var2) {
    ///   if (var1 > 2)
    ///     return 42;
    ///   else
    ///     if (var1 <= 50)
    ///       return 20;
    ///     else
    ///       return 10;
    /// }
    /// ```
    ///
    /// Would be displayed like this:
    ///
    /// ```txt
    /// int (int var1, int var2) {
    /// │ if (var1 > 2)
    /// │ │ return 42;
    /// │ else
    /// │ │ if (var1 <= 50)
    /// │ │ │ return 20;
    /// │ │ else
    /// │ │ │ return 10;
    /// }
    /// ```
    ///
    /// That is, it will take `tabstop` characters and print them.
    /// Where the `tabstop == 4`, it would use all 4 characters.
    ///
    /// [`Form`]: crate::form::Form
    pub indent_str: Option<&'static str>,
    /// Wether to copy the indentation string of `opts.indent_str` on
    /// empty lines.
    ///
    /// The default is `true`
    ///
    /// This will always copy whichever line has the smallest ammount
    /// of indentation.
    pub indent_str_on_empty: bool,
    /// An indent string, just like `opts.indent_str`, but only for
    /// `\t`s.
    ///
    /// The default is `None`
    ///
    /// This is useful for languages like python, where the mixup of
    /// tabs and spaces on indentation can cause problems.
    ///
    /// If it is `Some`, the `str` will be shown with the
    /// `replace.indent.tab` form. If this is `None`, then
    /// `opts.indent_str` will be used instead.
    pub indent_tab_str: Option<&'static str>,
    /// A character to be printed in place of the space.
    ///
    /// The default is `None`
    ///
    /// The char will be printed with the `replace.space` [`Form`].
    ///
    /// This character will replace only the space characters that are
    /// not part of the indentation.
    ///
    /// [`Form`]: crate::form::Form
    pub space_char: Option<char>,
    /// A character to be printed on trailing whitespace.
    ///
    /// The default is `None`
    ///
    /// This character will be printed with the
    /// `replace.space.trailing` [`Form`]
    ///
    /// If it is `None`, it will be the same as `opts.space_char`.
    ///
    /// [`Form`]: crate::form::Form
    pub space_char_trailing: Option<char>,
    /// Which `char` should be printed in new lines.
    ///
    /// The default is `' '` (space character)
    ///
    /// This character will be printed with the `replace.new_line`
    /// [`Form`].
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`Form`]: crate::form::Form
    pub new_line_char: char,
    /// A character to be printed on the new line on empty strings.
    ///
    /// The default is `None`
    ///
    /// This character will be printed with the
    /// `replace.new_line.empty` [`Form`].
    ///
    /// If it is `None`, it will be the same as `opts.new_line_char`.
    ///
    /// [`Form`]: crate::form::Form
    pub new_line_char_on_empty: Option<char>,
    /// A character to be printed on trailing new lines.
    ///
    /// The default is `None`
    ///
    /// This character will be printed with the
    /// `replace.new_line.trailing` [`Form`].
    ///
    /// [`Form`]: crate::form::Form
    pub new_line_trailing: Option<char>,
    /// Options concerning the [`duatmode`] [`Mode`]s.
    ///
    /// `duatmode` is the default key arrangement of Duat. These
    /// options tweak the behavior of this arrangement.
    ///
    /// Note that you can have other arrangements for `mode`s, (e.g. a
    /// vim one, or a helix one), and these options may not apply to
    /// those.
    pub duatmode: DuatModeOpts,
    /// Makes the [`FooterWidgets`] take up one line instead of two.
    ///
    /// Normally, the [`StatusLine`] is placed in one line and the
    /// [`PromptLine`] and [`Notifications`] are placed on another.
    /// With this option set to `true`, they will all occupy one
    /// line, the same way Kakoune does it.
    ///
    /// If you don't call [`opts::fmt_status`], this will also
    /// reformat the [`StatusLine`] to this:
    ///
    /// ```rust
    /// # use duat::prelude::*;
    /// let mode = mode_txt();
    /// let new_status = status!("{Spacer}{name_txt} {mode} {sels_txt} {main_txt}");
    /// ```
    ///
    /// This will firmly put all the information on the right side,
    /// like Kakoune does.
    ///
    /// [`FooterWidgets`]: crate::widgets::FooterWidgets
    /// [`StatusLine`]: crate::widgets::StatusLine
    /// [`PromptLine`]: crate::widgets::PromptLine
    /// [`Notifications`]: crate::widgets::Notifications
    /// [`Widget`]: crate::widgets::Widget
    /// [`opts::fmt_status`]: fmt_status
    pub one_line_footer: bool,
    /// Place the [`FooterWidgets`]s on top of the screen.
    ///
    /// Normally, the [`StatusLine`], [`PromptLine`] and
    /// [`Notifications`] [`Widget`]s are placed at the bottom of
    /// the screen, you can use this option to change that.
    ///
    /// [`FooterWidgets`]: crate::widgets::FooterWidgets
    /// [`StatusLine`]: crate::widgets::StatusLine
    /// [`PromptLine`]: crate::widgets::PromptLine
    /// [`Notifications`]: crate::widgets::Notifications
    /// [`Widget`]: crate::ui::Widget
    pub footer_on_top: bool,
    /// A [`KeyEvent`] to show the [`WhichKey`] widget.
    ///
    /// If [`None`] is given, the help key functionality will be
    /// disabled entirely, though the `WhichKey` widget will
    /// continue to show up automatically when appropriate. You
    /// can disable that functionality by [removing] the
    /// `"WhichKey"` hook.
    ///
    /// [`WhichKey`]: crate::widgets::WhichKey
    /// [removing]: crate::hook::remove
    pub help_key: Option<KeyEvent>,
    /// Options for the [`LineNumbers`]s widget.
    ///
    /// Do note that, at the moment, these options only apply to newly
    /// opened `LineNumbers`s, not to those that already exist.
    ///
    /// [`LineNumbers`]: crate::widgets::LineNumbers
    pub line_numbers: LineNumbersOpts,
    /// Options for the [`Notifications`] widget.
    ///
    /// The main purpose of these options is to modify how messages
    /// get displayed in the `Widget`, here's how you can do that:
    ///
    /// ```rust
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     opts::set(|opts| {
    ///         use context::Level::*;
    ///
    ///         opts.notifications.fmt(|rec| {
    ///             let mut builder = Text::builder();
    ///
    ///             match rec.level() {
    ///                 Error => builder.push(txt!("[log_book.error]  ")),
    ///                 Warn => builder.push(txt!("[log_book.warn]  ")),
    ///                 Info => builder.push(txt!("[log_book.info]  ")),
    ///                 Debug => builder.push(txt!("[log_book.debug]  ")),
    ///                 Trace => unreachable!(""),
    ///             };
    ///
    ///             builder.push(rec.text().clone());
    ///
    ///             builder.build()
    ///         });
    ///
    ///         opts.notifications
    ///             .set_allowed_levels([Error, Warn, Info, Debug]);
    ///     });
    /// }
    /// ```
    ///
    /// In the snippet above, I'm reformatting the notifications, so
    /// they show a symbol for identification. I'm also making use
    /// of the `log_book.{}` forms, since those are already set by
    /// the [`LogBook`].
    ///
    /// Do note that, at the moment, these options only apply to newly
    /// opened `Notifications`s, not to those that already exist.
    ///
    /// [`Notifications`]: crate::widgets::Notifications
    /// [`LogBook`]: crate::widgets::LogBook
    pub notifications: NotificationsOpts,
    /// Changes the [`WhichKey`] widget.
    ///
    /// This is the [`Widget`] that shows available bindings, as well
    /// as any possible remappings to those bindings, whenever you
    /// type a multi key sequence (and on other circumstances too).
    ///
    /// [`WhichKey`]: crate::widgets::WhichKey
    /// [`Widget`]: crate::widgets::Widget
    pub whichkey: WhichKeyOpts,
    /// Options for the [`LogBook`] widget.
    ///
    /// You can open the `LogBook` by calling the `"logs"` command,
    /// which will also focus on the `Widget`.
    ///
    /// By default, the `LogBook` will be shown at the bottom of the
    /// screen, and it shows the full log of notifications sent do
    /// Duat, unlike the [`Notifications`] `Widget`, which shows
    /// only the last one.
    ///
    /// You can change how the logs are displayed, here's how:
    ///
    /// ```rust
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     opts::set(|opts| {
    ///         let opts = &mut opts.logs;
    ///
    ///         opts.fmt(|rec| match rec.level() {
    ///             context::Level::Error => Some(txt!("[log_book.error]  {}", rec.text())),
    ///             _ => None,
    ///         });
    ///
    ///         opts.hidden = false;
    ///         opts.close_on_unfocus = false;
    ///         opts.side = ui::Side::Right;
    ///         opts.width = 75.0;
    ///     });
    /// }
    /// ```
    ///
    /// Here, I'm reformatting the notifications. Note that the
    /// closure return [`Some`] only when the notification is of
    /// type [`Level::Error`]. This filters out other types of
    /// notifications, which could be useful for heavy debugging
    /// sessions.
    ///
    /// Also helpful for debugging are the other options set. For
    /// example, if you're debugging a Duat [`Plugin`], it would
    /// be useful to show the [`LogBook`] right as Duat is
    /// reloaded, so you can see diagnostics immediately.
    ///
    /// Do note that, at the moment, these options only apply to newly
    /// opened `LogBook`s, not to those that already exist.
    ///
    /// [`Notifications`]: crate::widgets::Notifications
    /// [`Level::Error`]: crate::context::Level::Error
    /// [`Plugin`]: crate::Plugin
    /// [`LogBook`]: crate::widgets::LogBook
    pub logs: LogBookOpts,
}

impl Default for StartOpts {
    fn default() -> Self {
        Self {
            highlight_current_line: false,
            wrap_lines: true,
            wrap_on_word: false,
            wrapping_cap: None,
            indent_wraps: true,
            tabstop: 4,
            scrolloff: ScrollOff { x: 3, y: 3 },
            force_scrolloff: false,
            extra_word_chars: &[],
            indent_str: Some("▎"),
            indent_str_on_empty: true,
            indent_tab_str: None,
            space_char: None,
            space_char_trailing: None,
            new_line_char: ' ',
            new_line_char_on_empty: None,
            new_line_trailing: Some('↵'),
            duatmode: DuatModeOpts::default(),
            one_line_footer: false,
            footer_on_top: false,
            help_key: Some(KeyEvent::new(KeyCode::Char('h'), KeyMod::CONTROL)),
            line_numbers: LineNumbersOpts::default(),
            notifications: NotificationsOpts::default(),
            whichkey: WhichKeyOpts::default(),
            logs: LogBookOpts::default(),
        }
    }
}

/// Changes global options for Duat.
///
/// Most of these options concern the [`Buffer`] widget, which is the
/// primary [`Widget`] of Duat. But there are also options for the
/// other widgets, as well as options to change the layout and
/// behavior of Duat itself.
///
/// Within the `setup` function, this is how you'd use this function;
///
/// ```rust
/// use duat::prelude::*;
///
/// opts::set(|opts| {
///     opts.tabstop = 2;
///     opts.scrolloff.x = 0;
///     opts.duatmode.indent_on_capital_i = false;
/// });
/// ```
///
/// If you want to set these options on a [`Buffer`] by `Buffer`
/// basis, you should reach out for [hooks]:
///
/// ```rust
/// use duat::prelude::*;
///
/// hook::add::<BufferOpened>(|pa, handle| {
///     let buffer = handle.write(pa);
///
///     match buffer.filetype() {
///         Some("lua" | "c" | "javascript") => {
///             buffer.opts.tabstop = 2;
///         }
///         Some("markdown") => {
///             buffer.opts.extra_word_chars = &['-'];
///             buffer.opts.wrap_on_word = true;
///         }
///         _ => {}
///     }
/// });
/// ```
///
/// Do note that in this case, the only opts available in
/// `buffer.opts` are those that actually concern only the [`Buffer`]
/// struct. For now this is the [`PrintOpts`], which is also used by
/// other `Widget`s, but it will have more dedicated options in the
/// near future.
///
/// More options will come in the future!
///
/// [`Buffer`]: crate::widgets::Buffer
/// [`ScrollOff { x: 3, y: 3 }`]: ScrollOff
/// [hooks]: crate::hook
/// [`Text`]: crate::text::Text
/// [`Widget`]: crate::widgets::Widget
pub fn set(set_fn: impl FnOnce(&mut StartOpts)) {
    let mut opts = std::mem::take(&mut *OPTS.lock().unwrap());
    set_fn(&mut opts);
    duatmode::opts::set(|duatmode| *duatmode = opts.duatmode);
    *OPTS.lock().unwrap() = opts;
}

/// Reformat the [`StatusLine`] using the [`status!`] macro.
///
/// The `status!` macro is very convenient for showing information
/// about Duat, but most importantly to show information about
/// [`Buffer`]s.
///
/// The `status!` macro follows the same syntax as the [`txt!`] macro,
/// although inlined arguments tend to be functions, rather than just
/// variables. Here's the default [`StatusLine`]:
///
/// ```rust
/// # use duat::prelude::*;
/// let mode = mode_txt();
/// status!("{mode}{Spacer}{name_txt} {sels_txt} {main_txt}");
/// ```
///
/// The `mode` has to be explicitely returned, because [`mode_txt`]
/// isn't a `StatusLine` part, but a function that returns a
/// [`DataMap<&str, Text>`], which can be used as a `StatusLine`
/// part.
///
/// The [`Spacer`] here serves to do just that, separate the text into
/// two parts, each on one part of the screen. You can place as many
/// of these as you want, for example, this function will place the
/// `Buffer`'s name in between the two side parts:
///
/// ```rust
/// # use duat::prelude::*;
/// let mode = mode_txt();
/// status!("{mode}{Spacer}{name_txt}{Spacer}{sels_txt} {main_txt}");
/// ```
///
/// # Configuration
///
/// Duat provides a bunch of built-in functions to modify the
/// `StatusLine`:
///
/// - [`name_txt`]: `Text` with the `Buffer`'s name.
/// - [`path_txt`]: `Text` with the `Buffer`'s full path.
/// - [`mode_name`]: Unformatted mode (e.g. `"IncSearch<SearchFwd>"`).
/// - [`mode_txt`]: Formatted mode as `Text` (e.g. "normal").
/// - [`main_byte`]: byte index of the main cursor, 1 indexed.
/// - [`main_char`]: character index of the main cursor, 1 indexed.
/// - [`main_line`]: line index of the main cursor, 1 indexed.
/// - [`main_col`]: column of the main cursor, 1 indexed.
/// - [`main_txt`]: `Text` showing main cursor and line count info.
/// - [`sels_txt`]: `Text` showing the number of cursors.
/// - [`current_sequence_txt`]: `Text` showing the keys being mapped.
/// - [`last_key`]: The last typed key.
///
/// A rule of thumb is that every argument suffixed with `_txt` is a
/// function that returns a [`Text`], usually by making use of the
/// [`txt!`] macro. The [`Spacer`] in there is just one of the many
/// [`Tag`]s that can be placed on `Text`.
///
/// The other functions could return anything that implements the
/// [`Display`] or [`Debug`] traits.
///
/// It is rather easy to make a function that can be slotted into the
/// [`StatusLine`]:
///
/// ```rust
/// # use duat::prelude::*;
/// fn zero_main_txt(buffer: &Buffer, area: &Area) -> Text {
///     txt!(
///         "[coord]{}[separator]|[coord]{}[separator]/[coord]{}",
///         main_col(buffer, area) - 1,
///         main_line(buffer) - 1,
///         buffer.text().len().line()
///     )
/// }
///
/// let mode = mode_txt();
/// status!("{mode}{Spacer}{name_txt} {sels_txt} {zero_main_txt}");
/// ```
///
/// This is just the main cursor ([`Selection`]), but 0 indexed, as
/// opposed to 1 indexed. The `[coord]` and `[spearator]` bits apply
/// the `"coord"` and `"separator"` [`Form`]s, which you can change by
/// calling [`form::set`].
///
/// Of course, you could do this inline as well:
///
/// ```rust
/// # fn test() {
/// # use duat::prelude::*;
/// let mode = mode_txt();
/// status!(
///     "{mode}{Spacer}{name_txt} {sels_txt} [coord]{}[separator]|[coord]{}[separator]/[coord]{}",
///     |buf: &Buffer, area: &Area| main_col(buf, area) - 1,
///     |buf: &Buffer| main_line(buf) - 1,
///     |buf: &Buffer| buf.text().len().line()
/// );
/// # }
/// ```
///
/// . A full list of which types can be used as `StatusLine` parts can
/// be found in the documentation for the [`status!`] macro.
///
/// # Where to place
///
/// Normally, the [`StatusLine`] is included in the [`FooterWidgets`],
/// which is a "bundle" of [`Widget`]s, including the [`PromptLine`]
/// and [`Notifications`]. This means that it will follow
/// them around, so you have a few options for customization of this
/// group, all set in [`opts::set`]:
///
/// - [`StartOpts::footer_on_top`]: Will place the `StatusLine`,
///   `PromptLine`, and `Notifications` `Widget`s on top of the
///   window, rather than at the bottom.
///
/// - [`StartOpts::one_line_footer`]: These widgets will occupy one
///   line, rather than two, Kakoune style.
///
/// # Which [`Buffer`]?
///
/// As you could see above, the [`status!`] macro can take functions
/// that take `Buffer`s. But if there are multiple open `Buffer`s,
/// which one is used as an argument?
///
/// Basically, the [`Buffer`] in question is the "most relevant
/// `Buffer`", which is determined as follows, on a `StatusLine` by
/// `StatusLine` basis:
///
/// - If the `StatusLine` was [pushed onto a `Buffer`], then that
///   `Buffer` is the argument.
/// - If it was [pushed around the window], the [active `Buffer`] is
///   the argument. This is where the [`FooterWidgets`] are placed by
///   default.
///
/// This means that, with multiple [`StatusLine`]s, you could create
/// custom statuses that show global information, as well as
/// information about each [`Buffer`]:
///
/// ```rust
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn buf_percent(text: &Text, main: &Selection) -> Text {
///     // The caret is the part of the cursor that moves, as opposed to the anchor.
///     let caret = main.caret();
///     txt!("[coord]{}%", (100 * caret.line()) / text.len().line())
/// }
///
/// fn setup() {
///     hook::add::<BufferOpened>(|pa, handle| {
///         status!("{name_txt}{Spacer}{buf_percent}")
///             .above()
///             .push_on(pa, handle);
///     });
/// }
/// ```
///
/// With the snipped above, not only will there be the global
/// [`StatusLine`] at the bottom, but each [`Buffer`] will also have a
/// `StatusLine` above, showing the `Buffer`'s name and the line
/// percentage covered by the main cursor.
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [`status!`]: crate::widgets::status
/// [`Buffer`]: crate::widgets::Buffer
/// [`txt!`]: crate::text::txt
/// [`Text`]: crate::text::Text
/// [`Spacer`]: crate::text::Spacer
/// [`Tag`]: crate::text::Tag
/// [`name_txt`]: crate::state::name_txt
/// [`path_txt`]: crate::state::path_txt
/// [`mode_name`]: crate::state::mode_name
/// [`mode_txt`]: crate::state::mode_txt
/// [`main_byte`]: crate::state::main_byte
/// [`main_char`]: crate::state::main_char
/// [`main_line`]: crate::state::main_line
/// [`main_col`]: crate::state::main_col
/// [`main_txt`]: crate::state::main_txt
/// [`sels_txt`]: crate::state::sels_txt
/// [`current_sequence_txt`]: crate::state::current_sequence_txt
/// [`last_key`]: crate::state::last_key
/// [`Display`]: std::fmt::Display
/// [`Debug`]: std::fmt::Debug
/// [`Selection`]: crate::mode::Selection
/// [`Form`]: crate::form::Form
/// [`form::set`]: crate::form::set
/// [`DataMap<&str, Text>`]: crate::data::DataMap
/// [pushed onto a `Buffer`]: crate::context::Handle::push_outer_widget
/// [pushed around the window]: crate::ui::Window::push_outer
/// [active `Buffer`]: crate::context::dynamic_buffer
/// [`Widget`]: crate::ui::Widget
/// [`PromptLine`]: crate::widgets::PromptLine
/// [`Notifications`]: crate::widgets::Notifications
/// [`FooterWidgets`]: crate::widgets::FooterWidgets
/// [`opts::set`]: set
pub fn fmt_status(set_fn: impl FnMut(&mut Pass) -> StatusLineFmt + Send + 'static) {
    *STATUSLINE_FMT.lock().unwrap() = Some(Box::new(set_fn));
}

/// Options for the [`WhichKey`] widget.
///
/// These options concern the formatting and on which [`Mode`]s the
/// help should show up:
///
/// - [`disable_for`]: Disables the automatic showing of `WhichKey` on
///   a `Mode`. It'll still show up with the [help key]. If you want
///   to disable for all `Mode`s, [remove] the `"WhichKey"` hook
///   group.
///
/// [`WhichKey`]: crate::widgets::WhichKey
/// [`disable_for`]: WhichKeyOpts::disable_for
/// [help key]: StartOpts::help_key
/// [remove]: crate::hook::remove
#[allow(clippy::type_complexity)] // ??? where?
pub struct WhichKeyOpts {
    pub(crate) fmt_getter: Option<
        Box<
            dyn Fn() -> Box<dyn FnMut(Description) -> Option<(Text, Text)> + 'static>
                + Send
                + 'static,
        >,
    >,
    pub(crate) disabled_modes: Vec<TypeId>,
    pub(crate) always_shown_modes: Vec<TypeId>,
    /// Where to place the [`Widget`].
    ///
    /// Normally, this is [`Orientation::VerRightBelow`]. Since it's
    /// placed "inside" of the parent `Widget`, this normally places
    /// the widget on the bottom right corner, inside of the active
    /// [`Buffer`].
    ///
    /// [`Widget`]: crate::widgets::Widget
    /// [`Buffer`]: crate::widgets::Buffer
    pub orientation: Orientation,
}

impl Default for WhichKeyOpts {
    fn default() -> Self {
        Self {
            fmt_getter: None,
            disabled_modes: vec![TypeId::of::<duatmode::Insert>()],
            always_shown_modes: vec![TypeId::of::<crate::mode::User>()],
            orientation: Orientation::VerRightBelow,
        }
    }
}

impl WhichKeyOpts {
    /// How to format the `WhichKey` widget.
    ///
    /// This function returns an [`Option<(Text, Text)>`]. The first
    /// [`Text`] is used for the keys, the second `Text` is used for
    /// the description of said keys.
    ///
    /// If it returns [`None`], then that specific entry won't show up
    /// on the list of bindings. This is useful for, for example,
    /// hiding entries that have no description `Text`, which is done
    /// by default.
    pub fn fmt(
        &mut self,
        fmt: impl FnMut(Description) -> Option<(Text, Text)> + Send + Clone + 'static,
    ) {
        self.fmt_getter = Some(Box::new(move || Box::new(fmt.clone())))
    }

    /// Disable hints for the given [`Mode`].
    ///
    /// The hints will still show up if you press the [help key]. By
    /// default, `WhichKey` is disabled for `duatmode`'s [`Insert`]
    /// mode.
    ///
    /// Calling this function will also remove said `Mode` from the
    /// [always shown list].
    ///
    /// [`Mode`]: crate::mode::Mode
    /// [help key]: StartOpts::help_key
    /// [`Insert`]: crate::mode::Insert
    /// [always shown list]: Self::always_show
    pub fn disable_for<M: Mode>(&mut self) {
        self.always_shown_modes
            .retain(|ty| *ty != TypeId::of::<M>());
        self.disabled_modes.push(TypeId::of::<M>());
    }

    /// Makes the [`WhichKey`] permanently visible on this [`Mode`].
    ///
    /// This is useful for `Mode`s where the keys don't form
    /// sequences, but you'd still want them shown to the user. By
    /// default, this is enabled for the [`User`] mode.
    ///
    /// Calling this function will also remove said `Mode` from the
    /// [disabled list].
    ///
    /// [`WhichKey`]: crate::widgets::WhichKey
    /// [`User`]: crate::mode::User
    /// [disabled list]: Self::disable_for
    pub fn always_show<M: Mode>(&mut self) {
        self.disabled_modes.retain(|ty| *ty != TypeId::of::<M>());
        self.always_shown_modes.push(TypeId::of::<M>());
    }

    /// Makes the [`WhichKey`] show up normally on this [`Mode`].
    ///
    /// This function just removes this `Mode` from the [disabled] and
    /// [always shown] lists. By calling it, the [`WhichKey`]s widget
    /// will be displayed when you begin typing a sequence of bound or
    /// mapped keys.
    ///
    /// [`WhichKey`]: crate::widgets::WhichKey
    /// [disabled]: Self::disable_for
    /// [always shown]: Self::always_show
    pub fn show_normally<M: Mode>(&mut self) {
        self.disabled_modes.retain(|ty| *ty != TypeId::of::<M>());
        self.always_shown_modes
            .retain(|ty| *ty != TypeId::of::<M>());
    }
}

type StatusLineFn = Mutex<Option<Box<dyn FnMut(&mut Pass) -> StatusLineFmt + Send>>>;
