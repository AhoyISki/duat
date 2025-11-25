//! General options for Duat
//!
//! These options apply _globally_, and mostly serve as convenience
//! methods to modify Duat's [widgets]. If you wish to apply them on a
//! case by case way, you should reach out for [hooks].
//!
//! This module is primarily concerned with adding configuration for
//! the following [`Widget`]s:
//!
//! - [`Buffer`], through [`opts::set`].
//! - [`LineNumbers`], through [`opts::set_lines`].
//! - [`StatusLine`], through [`opts::set_status`].
//! - [`Notifications`], through [`opts::set_notifs`].
//! - [`LogBook`], through [`opts::set_logs`].
//! - [`WhichKey`], through, [`opts::set_which_key`].
//!
//! Additionally, there are some options pertaining to the group of
//! `Widget`s at the bottom (`StatusLine`, `PromptLine` and
//! `Notifications`):
//!
//! - [`opts::footer_on_top`]: Places them on top of the screen.
//! - [`opts::one_line_footer`]: Makes the footer a one line group.
//!
//! If you want to remove the default `Buffer` widgets (`LineNumbers`
//! and `VertRule`), you can add:
//!
//! ```rust
//! # use duat::prelude::*;
//! hook::remove("BufferWidgets");
//! ```
//!
//! If you want to remove the [`FooterWidgets`] (`StatusLine`,
//! `PromptLine` and `Notifications`), you can add:
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
//! [`StatusLine`]: crate::widgets::StatusLine
//! [`PromptLine`]: crate::widgets::PromptLine
//! [`Notifications`]: crate::widgets::Notifications
//! [`LogBook`]: crate::widgets::LogBook
//! [`FooterWidgets`]: crate::widgets::FooterWidgets
//! [`WhichKey`]: crate::widgets::WhichKey
//! [`opts::set`]: set
//! [`opts::set_lines`]: set_lines
//! [`opts::set_status`]: set_status
//! [`opts::set_notifs`]: set_notifs
//! [`opts::set_logs`]: set_logs
//! [`opts::set_which_key`]: set_which_key
//! [`opts::footer_on_top`]: footer_on_top
//! [`opts::one_line_footer`]: one_line_footer
//! [`PromptLineBuilder::push_on`]: crate::widgets::PromptLineBuilder::push_on
use std::{
    any::TypeId,
    sync::{
        LazyLock, Mutex,
        atomic::{AtomicBool, Ordering},
    },
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
pub use duatmode::opts::*;

use crate::widgets::NotificationsOpts;

/// Options for the [`Buffer`]
///
/// [`Buffer`]: crate::widgets::Buffer
pub(crate) static BUFFER_OPTS: Mutex<PrintOpts> = Mutex::new(PrintOpts::default_for_input());

pub(crate) static LINENUMBERS_OPTS: Mutex<LineNumbersOpts> = Mutex::new(LineNumbersOpts::new());

pub(crate) static STATUSLINE_FMT: StatusLineFn = Mutex::new(None);

pub(crate) static NOTIFICATIONS_FN: LazyLock<NotificationsFn> =
    LazyLock::new(|| Mutex::new(Box::new(|_| {})));

pub(crate) static LOGBOOK_FN: LazyLock<LogBookFn> = LazyLock::new(|| Mutex::new(Box::new(|_| {})));

pub(crate) static HELP_KEY: Mutex<Option<KeyEvent>> =
    Mutex::new(Some(KeyEvent::new(KeyCode::Char('?'), KeyMod::CONTROL)));
pub(crate) static WHICHKEY_OPTS: LazyLock<Mutex<WhichKeyOpts>> = LazyLock::new(Mutex::default);

pub(crate) static FOOTER_ON_TOP: AtomicBool = AtomicBool::new(false);
pub(crate) static ONE_LINE_FOOTER: AtomicBool = AtomicBool::new(false);

/// Change the global [`PrintOpts`] for [`Buffer`]s
///
/// In this function, you can modify the members of `PrintOpts`, which
/// are the following:
///
/// - `opts.wrap_lines: bool` - Enable wrapping of lines
///
///   The default is `false`
///
/// - `opts.wrap_on_word: bool` - Try to wrap at word boundaries
///
///   The default is `false`
///
/// - `opts.wrappint_cap: Option<u32>` - Distance to wrap from. This
///   will ignore the area's width, so you can wrap from outside the
///   screen, for example
///
///   The default is `None`
///
/// - `opts.indent_wrapped`: bool - Whether to indent wrapped lines
///
///   The default is `true`
///
/// - `opts.tabstop: u8` - How long tabs should be on screen
///
///   The default is `4`
///
/// - `opts.print_new_line` - Will show new lines as `' '` characters.
///   Otherwise, they won't get printed, and a cursor on them will
///   look invisible
///
///   The default is `true`
///
/// - `opts.scrolloff: ScrollOff` - How much space to keep between the
///   cursor and edges
///
///   The default is [`ScrollOff { x: 3, y: 3 }`]
///
/// - `opts.force_scrolloff: bool` - Whether to limit scrolloff at the
///   end of lines
///
///   The default is `false`
///
/// - `opts.extra_word_chars: &[char]` - Extra characters to be
///   considered part of a word
///
///   The default is `&[]`
///
/// - `opts.show_ghosts: bool` Whether to show [ghoxt text]
///
///   The default is `true`
///   
/// - `opts.allow_overscroll: bool` Wether to allow the [`Text`] to
///   scroll until only `scrolloff.y` line are on screen
///
///   The default is `true`
///
/// Within the `setup` function, this is how you'd use this function;
///
/// ```rust
/// use duat::prelude::*;
///
/// opts::set(|opts| {
///     opts.tabstop = 2;
///     opts.scrolloff.x = 0;
/// });
/// ```
///
/// If you want to set these options on a [`Buffer`] by `Buffer`
/// basis, you should reach out for [hooks], where the same
///
/// ```rust
/// use duat::prelude::*;
///
/// hook::add::<Buffer>(|pa, handle| {
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
///     Ok(())
/// });
/// ```
///
/// More options will come in the future!
///
/// [`Buffer`]: crate::widgets::Buffer
/// [`ScrollOff { x: 3, y: 3 }`]: ScrollOff
/// [hooks]: crate::hook
/// [`Text`]: crate::text::Text
pub fn set(set_fn: impl FnOnce(&mut PrintOpts)) {
    set_fn(&mut BUFFER_OPTS.lock().unwrap())
}

/// Change the global [`PrintOpts`] for [`LineNumber`]s
///
/// In this function, you can modify the members of `PrintOpts`, which
/// are the following:
///
/// - `relative: bool` - Wether to show relative numbering
///
///   The default is `false`
///
/// - `align: std::fmt::Alignment` Where to align the numbers
///
///   The default is [`std::fmt::Alignment::Left`]
///
/// - `main_align: std::fmt::Alignment` Where to align the main line
///   number
///
///   The default is [`std::fmt::Alignment::Right`]
///
/// - `opts.show_wraps: bool` - Wether to show wrapped line's numbers
///
///   The default is `false`
///
/// - `opts.on_the_right: bool` - Place this [`Widget`] on the right,
///   as opposed to on the left
///
///   The default is `false`
///   
/// Within the `setup` function, this is how you'd use this function;
///
/// ```rust
/// use duat::prelude::*;
///
/// opts::set_lines(|opts| {
///     opts.align = std::fmt::Alignment::Right;
///     opts.relative = true
/// });
/// ```
///
/// If you want to set these options on a [`Buffer`] by `Buffer`
/// basis, you should reach out for [hooks], where the same
///
/// ```rust
/// use duat::prelude::*;
///
/// hook::add::<LineNumbers>(|pa, handle| {
///     let filetype = handle.buffer()?.filetype(pa);
///
///     handle.write(pa).relative = match filetype {
///         Some("cpp" | "rust") => true,
///         _ => false,
///     };
///     Ok(())
/// });
/// ```
///
/// [`Buffer`]: crate::widgets::Buffer
/// [`LineNumber`]: crate::widgets::LineNumbers
/// [hooks]: crate::hook
/// [`Widget`]: crate::widgets::Widget
pub fn set_lines(set_fn: impl FnOnce(&mut LineNumbersOpts)) {
    set_fn(&mut LINENUMBERS_OPTS.lock().unwrap())
}

/// Reformat the [`StatusLine`] using the [`status!`] macro
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
/// - [`cur_map_txt`]: `Text` showing the keys being mapped.
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
/// group:
///
/// - [`opts::footer_on_top`]: Will place the `StatusLine`,
///   `PromptLine`, and `Notifications` `Widget`s on top of the
///   window, rather than at the bottom.
///
/// - [`opts::one_line_footer`]: These widgets will occupy one line,
///   rather than two, Kakoune style.
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
///     hook::add::<Buffer>(|pa, handle| {
///         status!("{name_txt}{Spacer}{buf_percent}")
///             .above()
///             .push_on(pa, handle);
///         Ok(())
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
/// [`cur_map_txt`]: crate::state::cur_map_txt
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
/// [`opts::one_line_footer`]: one_line_footer
/// [`opts::footer_on_top`]: footer_on_top
pub fn set_status(set_fn: impl FnMut(&mut Pass) -> StatusLineFmt + Send + 'static) {
    *STATUSLINE_FMT.lock().unwrap() = Some(Box::new(set_fn));
}

/// Changes the default [`Notifications`]
///
/// The main purpose of calling this function is to modify how
/// messages get displayed in the `Widget`, here's how you can do
/// that:
///
/// ```rust
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     opts::set_notifs(|opts| {
///         use context::Level::*;
///
///         opts.fmt(|rec| {
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
///         opts.set_allowed_levels([Error, Warn, Info, Debug]);
///     });
/// }
/// ```
///
/// In the snippet above, I'm reformatting the notifications, so they
/// show a symbol for identification. I'm also making use of the
/// `log_book.{}` forms, since those are already set by the
/// [`LogBook`].
///
/// Note that I'm also setting which [`Level`]s should be shown, since
/// by default, [`Level::Debug`] is not included in that list, as it
/// is mostly meant for the [`LogBook`]. But if you want to be
/// notified of it, the option's there.
///
/// [`Notifications`]: crate::widgets::Notifications
/// [`LogBook`]: crate::widgets::LogBook
/// [`Level`]: crate::context::Level
/// [`Level::Debug`]: crate::context::Level::Debug
pub fn set_notifs(mut set_fn: impl FnMut(&mut NotificationsOpts) + Send + 'static) {
    let mut notifications_fn = NOTIFICATIONS_FN.lock().unwrap();
    let mut prev = std::mem::replace(&mut *notifications_fn, Box::new(|_| {}));
    *notifications_fn = Box::new(move |opts| {
        prev(opts);
        set_fn(opts)
    });
}

/// Changes the default [`LogBook`]
///
/// You can open the `LogBook` by calling the `"logs"` command, which
/// will also focus on the `Widget`.
///
/// By default, the `LogBook` will be shown at the bottom of the
/// screen, and it shows the full log of notifications sent do Duat,
/// unlike the [`Notifications`] `Widget`, which shows only the last
/// one.
///
/// You can change how the logs are displayed, here's how:
///
/// ```rust
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     opts::set_logs(|opts| {
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
/// Here, I'm reformatting the notifications. Note that the closure
/// return [`Some`] only when the notification is of type
/// [`Level::Error`]. This filters out other types of notifications,
/// which could be useful for heavy debugging sessions.
///
/// Also helpful for debugging are the other options set. For example,
/// if you're debugging a Duat [`Plugin`], it would be useful to show
/// the [`LogBook`] right as Duat is reloaded, so you can see
/// diagnostics immediately.
///
/// [`LogBook`]: crate::widgets::LogBook
/// [`Notifications`]: crate::widgets::Notifications
/// [`Level::Error`]: crate::context::Level::Error
/// [`Plugin`]: crate::Plugin
pub fn set_logs(mut set_fn: impl FnMut(&mut LogBookOpts) + Send + 'static) {
    let mut logbook_fn = LOGBOOK_FN.lock().unwrap();
    let mut prev = std::mem::replace(&mut *logbook_fn, Box::new(|_| {}));
    *logbook_fn = Box::new(move |opts| {
        prev(opts);
        set_fn(opts)
    });
}

/// Changes the [`WhichKey`] widget
///
///
///
/// [`WhichKey`]: crate::widgets::WhichKey
pub fn set_which_key(set_fn: impl FnOnce(&mut WhichKeyOpts)) {
    let mut whichkey_opts = WHICHKEY_OPTS.lock().unwrap();
    set_fn(&mut whichkey_opts);
}

/// Makes the [`FooterWidgets`] take up one line instead of two
///
/// Normally, the [`StatusLine`] is placed in one line and the
/// [`PromptLine`] and [`Notifications`] are placed on another. With
/// this option set to `true`, they will all occupy one line, the same
/// way Kakoune does it.
///
/// If you don't call [`opts::set_status`], this will also reformat
/// the [`StatusLine`] to this:
///
/// ```rust
/// # use duat::prelude::*;
/// let mode = mode_txt();
/// status!("{AlignRight}{name_txt} {mode} {sels_txt} {main_txt}");
/// ```
///
/// This will firmly put all the information on the right side, like
/// Kakoune does.
///
/// [`FooterWidgets`]: crate::widgets::FooterWidgets
/// [`StatusLine`]: crate::widgets::StatusLine
/// [`PromptLine`]: crate::widgets::PromptLine
/// [`Notifications`]: crate::widgets::Notifications
/// [`Widget`]: crate::widgets::Widget
/// [`opts::set_status`]: set_status
pub fn one_line_footer(one_line: bool) {
    ONE_LINE_FOOTER.store(one_line, Ordering::Relaxed);
}

/// Place the [`FooterWidgets`]s on top of the screen
///
/// Normally, the [`StatusLine`], [`PromptLine`] and [`Notifications`]
/// [`Widget`]s are placed at the bottom of the screen, you can use
/// this option to change that.
///
/// [`FooterWidgets`]: crate::widgets::FooterWidgets
/// [`StatusLine`]: crate::widgets::StatusLine
/// [`PromptLine`]: crate::widgets::PromptLine
/// [`Notifications`]: crate::widgets::Notifications
/// [`Widget`]: crate::ui::Widget
pub fn footer_on_top(on_top: bool) {
    FOOTER_ON_TOP.store(on_top, Ordering::Relaxed);
}

/// A [`KeyEvent`] to show the [`WhichKey`] widget
///
/// If [`None`] is given, the help key functionality will be disabled
/// entirely, though the `WhichKey` widget will continue to show up
/// automatically when appropriate. You can disable that functionality
/// by [removing] the `"WhichKey"` hook.
///
/// [`WhichKey`]: crate::widgets::WhichKey
/// [removing]: crate::hook::remove
pub fn set_help_key(key_event: Option<KeyEvent>) {
    *HELP_KEY.lock().unwrap() = key_event;
}

type StatusLineFn = Mutex<Option<Box<dyn FnMut(&mut Pass) -> StatusLineFmt + Send>>>;
type NotificationsFn = Mutex<Box<dyn FnMut(&mut NotificationsOpts) + Send>>;
type LogBookFn = Mutex<Box<dyn FnMut(&mut LogBookOpts) + Send>>;

/// Options for the [`WhichKey`] widget
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
/// [help key]: set_help_key
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
    /// Where to place the [`Widget`]
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
    /// How to format the `WhichKey` widget
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

    /// Disable hints for the given [`Mode`]
    ///
    /// The hints will still show up if you press the [help key]. By
    /// default, `WhichKey` is disabled for `duatmode`'s [`Insert`]
    /// mode.
    ///
    /// Calling this function will also remove said `Mode` from the
    /// [always shown list].
    ///
    /// [`Mode`]: crate::mode::Mode
    /// [help key]: set_help_key
    /// [`Insert`]: crate::mode::Insert
    /// [always shown list]: Self::always_show
    pub fn disable_for<M: Mode>(&mut self) {
        self.always_shown_modes
            .retain(|ty| *ty != TypeId::of::<M>());
        self.disabled_modes.push(TypeId::of::<M>());
    }

    /// Makes the [`WhichKey`] permanently visible on this [`Mode`]
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

    /// Makes the [`WhichKey`] show up normally on this [`Mode`]
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
