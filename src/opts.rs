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
//! - [`PromptLine`], through [`opts::set_prompt`].
//! - [`Notifications`], through [`opts::set_notifs`].
//! - [`LogBook`], through [`opts::set_logs`].
//!
//! Additionally, there are some options pertaining to the group of
//! `Widget`s at the bottom (`StatusLine`, `PromptLine` and
//! `Notifications`):
//!
//! - [`opts::footer_on_top`]: Places them on top of the screen.
//! - [`opts::one_line_footer`]: Makes the footer a one line group.
//!
//! If you want to, you can also add:
//!
//! ```rust
//! # use duat::prelude::*;
//! hook::remove_group("BufferWidgets");
//! ```
//!
//! To completely remove the `Buffer` `Widget`s (`LineNumbers` and
//! `VertRule`). And you can also add:
//!
//! ```rust
//! # use duat::prelude::*;
//! hook::remove_group("FooterWidgets");
//! ```
//!
//! To remove the [`FooterWidgets`]. **WARNING: If you do this, you'll
//! lose access to a [`PromptLine`], so you'll be unable to run
//! commands. Remember, you can recompile your config with `duat
//! --reload`.
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
//! [`opts::set`]: set
//! [`opts::set_lines`]: set_lines
//! [`opts::set_status`]: set_status
//! [`opts::set_prompt`]: set_prompt
//! [`opts::set_notifs`]: set_notifs
//! [`opts::set_logs`]: set_logs
//! [`opts::footer_on_top`]: footer_on_top
//! [`opts::one_line_footer`]: one_line_footer
use std::sync::{
    LazyLock, Mutex,
    atomic::{AtomicBool, Ordering},
};

use duat_core::data::Pass;
#[allow(unused_imports)]
pub use duat_core::opts::word_chars as w_chars;
pub use duat_core::opts::*;
use duat_utils::widgets::{LineNumbersOpts, StatusLineFmt};

/// Options for the [`Buffer`]
///
/// [`Buffer`]: crate::widgets::Buffer
pub(crate) static BUFFER_OPTS: Mutex<PrintOpts> = Mutex::new(PrintOpts::default_for_input());
pub(crate) static LINENUMBERS_OPTS: Mutex<LineNumbersOpts> = Mutex::new(LineNumbersOpts { .. });
pub(crate) static STATUSLINE_FMT: LazyLock<StatusLineFn> =
    LazyLock::new(|| Mutex::new(Box::new(|_| StatusLineFmt::default())));
pub(crate) static FOOTER_ON_TOP: AtomicBool = AtomicBool::new(false);
pub(crate) static ONE_LINE_FOOTER: AtomicBool = AtomicBool::new(false);

/// Change the global [`PrintOpts`] for [`Buffer`]s
///
/// In this function, you can modify the members of `PrintOpts`, which
/// are the following:
///
/// - `opts.wrap_method: WrapMethod` - How to wrap lines in the buffer
///
///   The default is [`WrapMethod::NoWrap`]
///
/// - `opts.indent_wrapped`: bool - Whether to indent wrapped lines
///
///   The default is `true`
///
/// - `opts.tabstop: u8` - How long tabs should be on screen
///
///   The default is `4`
///
/// - `opts.new_line: NewLine` - How to show new lines
///
///   The default is [`NewLine::AlwaysAs(' ')`].
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
/// - `opts.word_chars: WordChars` - Characters to be considered part
///   of a word  The default is [`word_chars!("A-Za-z0-9_-_")`].
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
///             buffer.opts.word_chars = word_chars!("A-Za-z0-9_-_---");
///             buffer.opts.wrap_method = WrapMethod::Word;
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
/// [`WrapMethod::NoWrap`]: WrapMethod
/// [`NewLine::AlwaysAs(' ')`]: NewLine
/// [`ScrollOff { x: 3, y: 3 }`]: ScrollOff
/// [`word_chars!("A-Za-z0-9_-_")`]: word_chars
/// [hooks]: crate::hook
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
/// fn zero_main_txt(buffer: &Buffer, area: &dyn Area) -> Text {
///     txt!(
///         "[coord]{}[separator]|[coord]{}[separator]/[coord]{}",
///         main_col(buffer, area) - 1,
///         main_line(buffer) - 1,
///         buffer.text().len().line()
///     )
///     .build()
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
///     |buf: &Buffer, area: &dyn Area| main_col(buf, area) - 1,
///     |buf: &Buffer| main_line(buf) - 1,
///     |buf: &Buffer| buf.text().len().line()
/// );
/// # }
/// ```
///
/// A full list of which types can be used as `StatusLine` parts can
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
/// fn buf_percent(buf: &Buffer) -> Text {
///     // The caret is the part of the cursor that moves, as opposed to the anchor.
///     let caret = buf.selections().get_main().unwrap().caret();
///     txt!("[coord]{}%", (100 * caret.line()) / buf.text().len().line()).build()
/// }
///
/// fn setup() {
///     hook::add::<Buffer>(|pa, handle| {
///         let status = status!("{name_txt}{Spacer}{buf_percent}");
///         status.above().push_on(pa, handle);
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
/// [pushed around the window]: crate::ui::UiBuilder::push_outer
/// [active `Buffer`]: crate::context::dynamic_buffer
/// [`Widget`]: crate::ui::Widget
/// [`PromptLine`]: crate::widgets::PromptLine
/// [`Notifications`]: crate::widgets::Notifications
/// [`FooterWidgets`]: crate::widgets::FooterWidgets
/// [`opts::one_line_footer`]: one_line_footer
/// [`opts::footer_on_top`]: footer_on_top
pub fn set_status(set_fn: impl FnMut(&mut Pass) -> StatusLineFmt + Send + 'static) {
    *STATUSLINE_FMT.lock().unwrap() = Box::new(set_fn);
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
/// [`Widget`]: crate::ui::Widget
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

type StatusLineFn = Mutex<Box<dyn FnMut(&mut Pass) -> StatusLineFmt + Send>>;
