//! Option definitions for the [`Buffer`] widget
//!
//! These options are defined here, but their actual implementation is
//! left as an exercise to plugins. This is because I want them to be
//! configurable, and if my implementation of them clashes with some
//! plugin, I want that plugin to be able to replace it with a more
//! compatible version.
//!
//! The default implementation of these options is defined in the
//! `duat-base` crate.
//!
//! [`Buffer`]: super::Buffer

use crate::opts::{PrintOpts, ScrollOff};

/// The default suite of options available to [`Buffer`]s
///
/// Unlike most other widget options, these ones are dynamic, that is,
/// if they are changed while duat is still open, the `Buffer` will be
/// updated accordingly.
///
/// # Note
///
/// While these options are defined as a core part of the `Buffer`,
/// they are not implemented natively. The implementation is done in
/// the `duat-base` crate, which means you may replace the
/// implementation with your own version if that suits you.
///
/// [`Buffer`]: super::Buffer
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BufferOpts {
    /// Highlights the current line
    ///
    /// The default is `true`
    ///
    /// This makes use of the `current_line` [`Form`]
    ///
    /// [`Form`]: crate::form::Form
    pub highlight_current_line: bool,
    /// Enables wrapping of lines
    ///
    /// The default is `true`
    pub wrap_lines: bool,
    /// Wrap on word boundaries, rather than on any character
    ///
    /// The default is `false`.
    pub wrap_on_word: bool,
    /// Where to start wrapping
    ///
    /// The default is `None`
    ///
    /// If this value is `None` and `opts.wrap_lines == true`, then
    /// wrapping will take place at the right edge of the screen.
    ///
    /// Otherwise, if it is `Some({cap})`, then wrapping will take
    /// place `{cap}` cells from the left edge. This value may or may
    /// not be greater than the width of the area. If it is greater
    /// than it, then wrapping will take place slightly outside the
    /// screen as a concequence.
    pub wrapping_cap: Option<u32>,
    /// Whether to indent wrapped lines or not
    ///
    /// The default is `true`.
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
    pub indent_wraps: bool,
    /// How much space a tab should occupy
    ///
    /// The default is `4`
    ///
    /// This also affect other things, like if your tabs are converted
    /// into spaces, this will also set how many spaces should be
    /// added.
    pub tabstop: u8,
    /// How much space to keep between the cursor and edges
    ///
    /// The default is `ScrollOff { x: 3, y: 3 }`
    pub scrolloff: ScrollOff,
    /// Whether to limit scrolloff at the end of lines
    ///
    /// The default is `false`
    ///
    /// This makes it so, as you reach the end of a long line of text,
    /// the cursor line will continue scrolling to the left,
    /// maintaining the `scrolloff.x`'s gap.
    pub force_scrolloff: bool,
    /// Extra characters to be considered part of a word
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
    /// Indent string
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
    /// If this is set to true, this str will be printed with the
    /// `replace.indent.empty` form.
    ///
    /// This will always copy whichever line has the smallest ammount
    /// of indentation.
    pub indent_str_on_empty: bool,
    /// An indent string, just like `opts.indent_str`, but only for
    /// `\t`s
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
    /// A character to be printed in place of the space
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
    /// A character to be printed on trailing whitespace
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
    /// Which `char` should be printed in new lines
    ///
    /// The default is `' '` (space character)
    ///
    /// This character will be printed with the `replace.new_line`
    /// [`Form`].
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`Form`]: crate::form::Form
    pub new_line_char: char,
    /// A character to be printed on the new line on empty strings
    ///
    /// The default is `None`
    ///
    /// This character will be printed with the
    /// `replace.new_line.empty` [`Form`].
    ///
    /// If it is `None`, it will be the same as `opts.new_line_char`.
    pub new_line_on_empty: Option<char>,
    /// A character to be printed on trailing new lines
    ///
    /// The default is `None`
    ///
    /// This character will be printed with the
    /// `replace.new_line.trailing` [`Form`].
    ///
    /// [`Form`]: crate::form::Form
    pub new_line_trailing: Option<char>,
}

impl BufferOpts {
    /// Gets [`PrintOpts`] from this [`BufferOpts`]
    pub fn to_print_opts(&self) -> PrintOpts {
        PrintOpts {
            wrap_lines: self.wrap_lines,
            wrap_on_word: self.wrap_on_word,
            wrapping_cap: self.wrapping_cap,
            indent_wraps: self.indent_wraps,
            tabstop: self.tabstop,
            print_new_line: true,
            scrolloff: self.scrolloff,
            force_scrolloff: self.force_scrolloff,
            extra_word_chars: self.extra_word_chars,
            show_ghosts: true,
            allow_overscroll: true,
        }
    }
}

impl Default for BufferOpts {
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
            new_line_on_empty: None,
            new_line_trailing: Some('↵'),
        }
    }
}
