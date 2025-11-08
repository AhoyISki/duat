//! `duatmode` is the default mode for the Duat text editor. It is
//! based on [kakoune]'s keybindings, with some alterations of my own.
//!
//! This plugin is included in Duat by default, as it is considered
//! part of it's identity. Given that, the options aren't set by this
//! plugin's [`Plugin`], but by [`duatmode::opts`], which is exported
//! in duat's [`opts`] module by default
//!
//! # Keymaps
//!
//! On every key, if the action involves selections, unless stated
//! otherwise, it will take place in all selections.
//!
//! ## `Insert` mode
//!
//! Insert mode is the text editing mode of Duat, much like Vim's. It
//! is also entered via various keys in `Normal` mode.
//!
//! On insert mode, keys are sent normally, with the exception of the
//! following:
//!
//! `<Tab>` and `<S-Tab>` will do different things depending on your
//! [tab mode].
//!
//! `<C-n>` and `<C-p>` go to the next and previous completion
//! entries.
//!
//! `<Esc>` exits insert mode, returning to `Normal` mode`.
//!
//! ## `Normal` mode
//!
//! The keys in `normal` mode follow the following patterns:
//!
//! - `word` characters follow Duat's [word chars], which are normally
//!   used to define where lines wrap.
//! - `WORD` characters are just any non-whitespace character.
//!
//! In normal mode, another factor is the `param` value, which is
//! incremented by typing digits.  For example, if you type
//! `10<Right>`, the selections will move 10 times to the right.
//!
//! <details>
//! <summary>
//!
//! ### Object selection
//!
//! </summary>
//!
//! In Duat, there are various types of "objects" for selection. These
//! get used on `Normal` mode key sequences, most notably on `<A-i>`
//! and `<A-a>`. Each of them defines something to be selected:
//!
//! `b`, `(`, `)`\
//! Inside/around parenthesis.
//!
//! `B`, `{`, `}`\
//! Inside/around curly braces.
//!
//! `r`, `[`, `]`\
//! Inside/around brackets.
//!
//! `a`, `<`, `>`\
//! Inside/around angle brackets.
//!
//! `q`, `'`\
//! Inside/around single quotes.
//!
//! `Q`, `"`\
//! Inside/around double quotes.
//!
//! `g`, `` ` ``\
//! Inside/around graves.
//!
//! `w`, `<A-w>`
//! Inside/around `word`s and `WORD`s.
//!
//! `s`\
//! Inside/around sentences.
//!
//! `p`\
//! Inside/around paragraphs.
//!
//! `i`\
//! Inside/around lines of equal or greater indentation.
//!
//! ` `\
//! Inside/around whitespace.
//!
//! </details>
//!
//! <details>
//! <summary>
//!
//! ## Selection keys
//!
//! </summary>
//!
//! `h`, `<Left>`\
//! Move left. Wraps around lines.
//!
//! `j`\
//! Move down
//!
//! `<Down>`\
//! Move down to the next wrapped line (i.c vim's `gj`).
//!
//! `k`\
//! Move up.
//!
//! `<Up>`\
//! Move up to the previous wrapped line (i.e. vim's `gk`).
//!
//! `l`, `<Right>`\
//! Move right. Wraps around lines.
//!
//! `H`, `<S-Left>`, `J`, `<S-Down>`, `K`, `<S-Up>`, `L`, `<S-Right>`\
//! Same as the previous characters, but extends the selection
//!
//! `w`\
//! Selects the `word` and following space ahead of the selection.
//!
//! `b`\
//! Selects the `word` followed by spaces behind the selection.
//!
//! `e`\
//! Selects to the end of the next `word` ahead of the selection.
//!
//! `<(W|B|E)>`\
//! The same as `(w|b|e)`, but extends the selection.
//!
//! `<A-(w|b|e)>`\
//! The same as `(w|b|e)`, but over a `WORD`.
//!
//! `<A-(W|B|E)>`\
//! The same as `<A-(w|b|e)>`, but extends the selection.
//!
//! `f{char}`\
//! Selects to the next occurrence of the `{char}`.
//!
//! `t{char}`\
//! Selects until the next occurrence of the `{char}`.
//!
//! `<(F|T)>{char}`\
//! Same as `(f|t)`, but extends the selection.
//!
//! `<A-(f|t)>{char}`\
//! Same as `(f|t)`, but in the opposite direction.
//!
//! `<A-(F|T)>{char}`\
//! Same as `<A-(f|t)>`, but in extends the selection.
//!
//! `{param}g`\
//! Goes to the `{param}`th line. If param was not set, enters `go to`
//! mode.
//!
//! `{param}G`\
//! Extends to the `{param}`th line. If param was not set, enters `go
//! to` mode, and actions will extend.
//!
//! `x`\
//! Extends selection to encompass full lines.
//!
//! `%`\
//! Selects the whole buffer.
//!
//! `<A-h>`, `<Home>`\
//! Selects to the start of the line.
//!
//! `<A-l>`, `<End>`\
//! Selects until the end of the line.
//!
//! `<A-H>`, `<S-Home>`, `<A-L>`, `<S-End>`\
//! Same as the previous two, but extends the selection.
//!
//! `m`\
//! Selects to the next pair of matching brackets.
//!
//! `<A-m>`\
//! Selects the previous pair of matching brackets.
//!
//! `M`, `<A-M>`\
//! Same as the previous two, but extends the selection.
//!
//! `<A-u>`\
//! Returns to the previous state for the selections.
//!
//! `<A-U>`\
//! Goes to the next state for the selections.
//!
//! `;`\
//! Reduces selections to just the [caret].
//!
//! `<A-;>`\
//! Flips the [caret] and [anchor] of selectionss around.
//!
//! `,`\
//! Removes extra selections.
//!
//! `C`\
//! Creates a selection on the column below the last one.
//!
//! `<A-C>`\
//! Creates a selection on the column above the first one.
//!
//! `<A-:>`\
//! Places the [caret] ahead of the [anchor] in all selections.
//!
//! `<A-s>`\
//! Divides selection into multiple selections, one per line.
//!
//! `<A-S>`\
//! Splits into two selections, one at each end of the selection.
//!
//! `<A-_>`\
//! Merges all adjacent selections.
//!
//! </details>
//!
//! <details>
//! <summary>
//!
//! ### Text modification
//!
//! </summary>
//!
//! `i`\
//! Enter `insert` mode before selections, keys inserted (except for
//! `<Delete>`) will only move the selection.
//!
//! `a`\
//! Enter `insert` mode after selection, keys inserted will extend the
//! selection.
//!
//! `I`\
//! Moves to the beginning of the line (after indent) and enters
//! `insert` mode.
//!
//! `A`\
//! Moves to the end of the line and enters `insert` mode.
//!
//! `y`\
//! Yanks selections.
//!
//! `d`\
//! Deletes and yanks the selections.
//!
//! `c`\
//! Deletes, yanks, and enter `insert` mode.
//!
//! `p`\
//! Pastes after end of each selection (multi line selections are
//! placed on the next line).
//!
//! `P`\
//! Pastes at the start of each selection (multi line pastes are
//! placed on the previous line).
//!
//! `R`\
//! Replaces with the pasted text, without yanking.
//!
//! `<A-d>`\
//! Deletes selections without yanking.
//!
//! `<A-c>`\
//! Deletes selections without yanking, then enters `insert` mode.
//!
//! `o`\
//! Creates a new line below and enters `insert` mode in it.
//!
//! `O`\
//! Creates a new line above and enters `insert` mode in it.
//!
//! `<A-(o|O)>`\
//! Same as `(o|O)`, but just adds the new line without moving.
//!
//! `r{key}`\
//! Replaces each character with `{key}`
//!
//! `u`\
//! [Undoes] the last `moment`
//!
//! `U`\
//! [Redoes] the next `moment`
//!
//! `>`\
//! Adds indentation to the selected lines.
//!
//! `<`\
//! Removes indentation to the selected lines.
//!
//! `<A-j>`\
//! Merges selected lines.
//!
//! `` ` ``\
//! Changes selection to lowercase.
//!
//! `~`\
//! Changes selection to uppercase.
//!
//! ``<A-`>``\
//! Swaps the case of each character.
//!
//! `<A-)>`\
//! Rotates each selection's content forwards.
//!
//! `<A-(>`\
//! Rotates each selection's content backwards.
//!
//! `|`\
//! Changes mode to [`PipeSelections`], letting you pipe each
//! selection to an external program.
//!
//! </details>
//!
//! <details>
//! <summary>
//!
//! ### Search
//!
//! </summary>
//!
//! The searching in this plugin is done through the [`IncSearch`]
//! [`Mode`] from Duat, with some [`IncSearcher`]s defined in this
//! crate. This means that search will be done incrementally over a
//! Regex pattern.
//!
//! `/`\
//! Searches forward for the next pattern.
//!
//! `<A-/>`\
//! Searches backwards for the previous pattern.
//!
//! `?`\
//! Extends forward for the next pattern.
//!
//! `<A-?>`\
//! Extends backwards for the previous pattern.
//!
//! `s`\
//! Selects the pattern from within current selections.
//!
//! `S`\
//! Splits current selections by the pattern.
//!
//! `<A-k>`\
//! Keeps only the selections that match the pattern.
//!
//! `<A-K>`\
//! Keeps only the selections that _don't_ match the pattern.
//!
//! `n`\
//! Go to next match for pattern.
//!
//! `N`\
//! Create a new cursor on the next match for pattern.
//!
//! `<A-n>`\
//! Go to previous match for pattern.
//!
//! `<A-N>`\
//! Create a new cursor on the previous match for pattern.
//!
//! `*`\
//! Makes the main selection the searching pattern.
//!
//! </details>
//!
//! <details>
//! <summary>
//!
//! ## `goto` mode
//!
//! </summary>
//!
//! `goto` mode is entered with the `g` or `G` keys in `normal` mode.
//!
//! On every key that selects, `G` will have the same behavior, but
//! extending the selection instead.
//!
//! `h`\
//! Move to the beginning of the line (before indents, column 0).
//!
//! `l`\
//! Go to the end of the line.
//!
//! `i`\
//! Go to the beginning of the line, after indents.
//!
//! `g`,`k`\
//! Go to the first line.
//!
//! `j`\
//! Go to the last line.
//!
//! `a`\
//! Go to the last buffer. Repeating will return to this buffer
//!
//! `n`\
//! Go to the next buffer (includes other windows).
//!
//! `N`\
//! Go to the previous buffer (includes other windows).
//!
//! </details>
//!
//! <details>
//! <summary>
//!
//! ## User mode
//!
//! </summary>
//!
//! In Duat, [`User`] mode is a "generalized mode", which should be
//! used by [`Plugin`]s for key maps. For example, you could map `l`
//! on `User` mode to do LSP related actions.
//!
//! Other "monolithic modes" (Vim, Helix, Emacs, etc) should make use
//! of this [`User`] mode for the same purpose. Think of it like the
//! leader key in (neo)vim.
//!
//! To enter `User` mode, you type `<Space>` in `Normal` mode.
//!
//! </details>
//!
//! [`User`]: duat_core::mode::User
//! [kakoune]: https://github.com/mawww/kakoune
//! [caret]: duat_core::mode::Cursor::caret
//! [anchor]: duat_core::mode::Cursor::anchor
//! [`Cursor`]: duat_core::mode::Cursor
//! [Undoes]: duat_core::text::Text::undo
//! [Redoes]: duat_core::text::Text::redo
//! [`Cargo.toml`'s `dependencies` section]: https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html
//! [`IncSearch`]: duat_base::modes::IncSearch
//! [`IncSearcher`]: duat_base::modes::IncSearcher
//! [`PipeSelections`]: duat_base::modes::PipeSelections
//! [mapping]: duat_core::mode::map
//! [`duat::mode`]: https://docs.rs/duat/latest/duat/mode
//! [`Mode`]: duat_core::mode::Mode
//! [`duatmode::opts`]: opts
//! [`opts`]: https://docs.rs/duat/latest/duat/opts
//! [word chars]: duat_core::opts::PrintOpts::extra_word_chars
//! [tab mode]: opts::set_very_smart_tabs
use std::{
    collections::HashMap,
    ops::Range,
    sync::{LazyLock, Mutex},
};

use crate::normal::Brackets;
pub use crate::{insert::Insert, normal::Normal};

mod inc_searchers;
mod insert;
mod normal;
mod one_key;

use duat_base::hooks::SearchPerformed;
use duat_core::{
    Plugin, Plugins,
    context::{self, Handle},
    data::Pass,
    form, hook,
    mode::{self, Cursor, KeyEvent, alt, event},
    opts::PrintOpts,
};
pub use parameter::{add_to_param, duat_param, duat_param_txt, take_param};

mod parameter {
    use std::sync::LazyLock;

    use duat_core::{
        context,
        data::{DataMap, Pass, RwData},
        text::{Text, txt},
    };

    static VALUE: LazyLock<RwData<u32>> = LazyLock::new(RwData::default);

    /// Adds a number to Duat's parameter value
    ///
    /// This should multiply the current value by 10 and add this as
    /// the last digit.
    ///
    /// This parameter is a numerical modifier for actions in Duat's
    /// [`Normal`] mode, and is added to by typing digits in `Normal`
    /// mode.
    ///
    /// [`Normal`]: crate::Normal
    pub fn add_to_param(pa: &mut Pass, num: u32) {
        let value = VALUE.read(pa);
        match value.checked_mul(10).and_then(|v| v.checked_add(num)) {
            Some(new_value) => *VALUE.write(pa) = new_value,
            None => context::warn!("Duat's parameter overflowed"),
        };
    }

    /// Takes the current value, leaving it empty
    ///
    /// The first argument is the parameter, the second one is `true`
    /// if a parameter was set in the first place.
    pub fn take_param(pa: &mut Pass) -> (u32, bool) {
        let param = std::mem::take(VALUE.write(pa));
        (param.max(1), param != 0)
    }

    /// [`StatusLine`] part: The parameter for Duat's modes
    ///
    /// This parameter is a numerical modifier for actions in Duat's
    /// [`Normal`] mode, and is added to by typing digits in `Normal`
    /// mode.
    ///
    /// [`Normal`]: crate::Normal
    /// [`StatusLine`]: duat_base::widgets::StatusLine
    pub fn duat_param() -> DataMap<u32, u32> {
        VALUE.map(|value| *value)
    }

    /// [`StatusLine`] part: Formatted version of [`duat_param`]
    ///
    /// This parameter is a numerical modifier for actions in Duat's
    /// [`Normal`] mode, and is added to by typing digits in `Normal`
    /// mode.
    ///
    /// # Formatting
    ///
    /// ```text
    /// [parameter]param={value}
    /// ```
    ///
    /// [`Normal`]: crate::Normal
    /// [`StatusLine`]: duat_base::widgets::StatusLine
    pub fn duat_param_txt() -> DataMap<u32, Text> {
        VALUE.map(|value| {
            if *value > 0 {
                txt!("[parameter]param={value}")
            } else {
                Text::default()
            }
        })
    }
}

pub mod opts {
    use std::sync::{
        Mutex,
        atomic::{AtomicBool, Ordering},
    };

    use crate::insert::TabMode;

    pub(crate) static INSERT_TABS: AtomicBool = AtomicBool::new(false);
    pub(crate) static TABMODE: Mutex<TabMode> = Mutex::new(TabMode::VerySmart);

    /// Makes the `Tab` key insert tabs as opposed to spaces
    ///
    /// Do note that this option interacts with the options for
    /// [normal], [smart], and [very smart] tabs, the default being
    /// very smart tabs.
    ///
    /// [normal]: set_normal_tabs
    /// [smart]: set_smart_tabs
    /// [very smart]: set_very_smart_tabs
    pub fn insert_tabs(insert_tabs: bool) {
        INSERT_TABS.store(insert_tabs, Ordering::Relaxed);
    }

    /// Sets the `Tab` mode to normal
    ///
    /// With this setting, the `Tab` key is just a regular tab key,
    /// and will insert [spaces or a tab], no matter where the caret
    /// is.
    ///
    /// The default is the [very smart mode].
    ///
    /// [very smart mode]: set_very_smart_tabs
    /// [spaces or a tab]: insert_tabs
    pub fn set_normal_tabs() {
        *TABMODE.lock().unwrap() = TabMode::Normal
    }

    /// Sets the `Tab` mode to smart
    ///
    /// With this setting, if you press the `Tab` key at the leading
    /// spaces of a line, then the line will be reindented to the
    /// appropriate indentation, given the treesitter indentation
    /// query.
    ///
    /// If you press it and the indentation is not changed by that,
    /// then [spaces or a tab] will be inserted at the current
    /// position.
    ///
    /// The default is the [very smart mode].
    ///
    /// [very smart mode]: set_very_smart_tabs
    /// [spaces or a tab]: insert_tabs
    pub fn set_smart_tabs() {
        *TABMODE.lock().unwrap() = TabMode::Smart
    }

    /// Sets the `Tab` mode to very smart
    ///
    /// With this setting, if there are multiple selections, this mode
    /// will act exactly like [smart tabs].
    ///
    /// However, if there is just one selection, then it will try to
    /// reindent the line if the caret is on leading whitespace. If
    /// that fails, or the indentation doesn't change, then it will
    /// [scroll to the next completion entry].
    ///
    /// This is the default tab mode.
    ///
    /// [smart tabs]: set_smart_tabs
    /// [scroll to the next completion entry]: duat_base::widgets::Completions::scroll
    pub fn set_very_smart_tabs() {
        *TABMODE.lock().unwrap() = TabMode::VerySmart
    }

    /// The current [`TabMode`]
    pub(crate) fn get_tab_mode() -> TabMode {
        *TABMODE.lock().unwrap()
    }
}

/// The [`Plugin`] for `duatmode`
///
/// This [`Plugin`] will change the default mode to `duatmode`'s
/// [`Normal`].
///
/// It also adds a hook to automatically change the forms of the
/// cursors when the mode changes. This is the pattern that the forms
/// take:
///
/// - On [`Insert`] mode: `"caret.main.Insert"`,
///   `"caret.extra.Insert"`
/// - On [`Normal`] mode: `"caret.main.Normal"`,
///   `"caret.extra.Normal"`
///
/// And so on and so forth.
///
/// [`Form`]: duat_core::form::Form
pub struct DuatMode {
    normal: Normal,
}

impl DuatMode {
    /// Returns a new instance of [`DuatMode`], the plugin for
    /// kakoune-like editing
    pub fn new() -> Self {
        Self { normal: Normal::new() }
    }
}

impl DuatMode {
    /// Changes what is considered a "bracket" in [`Normal`] mode
    ///
    /// More specifically, this will change the behavior of keys like
    /// `m` and the `u` object, which will now consider more
    /// patterns when selecting.
    pub fn with_brackets<'a>(mut self, brackets: impl Iterator<Item = [&'a str; 2]>) -> Self {
        self.normal.set_brackets(brackets);
        self
    }

    /// Makes it so the `I` key no longer indents the line in
    /// [`Normal`] mode
    ///
    /// By default, when you press `I`, the line will be reindented,
    /// in order to send you to the "proper" insertion spot, not just
    /// to the first non whitespace character.
    ///
    /// This function disables that behavior.
    pub fn with_no_indent_on_capital_i(mut self) -> Self {
        self.normal.indent_on_capital_i = false;
        self
    }

    /// Makes the `'f'` and `'t'` keys set the search pattern
    ///
    /// If you type `"fm"`, for example, and then type `'n'`, `'n'`
    /// will search for the next instance of an `'m'` in the
    /// [`Buffer`]
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub fn f_and_t_set_search(mut self) -> Self {
        self.normal.f_and_t_set_search = true;
        self
    }
}

impl Plugin for DuatMode {
    fn plug(self, plugins: &Plugins) {
        plugins.require::<jump_list::JumpList>();
        plugins.require::<treesitter::TreeSitter>();

        mode::set_alt_is_reverse(true);
        mode::set_default::<Normal>(Normal::new());

        hook::add::<SearchPerformed>(|_, search| {
            *SEARCH.lock().unwrap() = search.to_string();
            Ok(())
        });

        form::enable_mask("Insert");
        form::enable_mask("Normal");
        form::enable_mask("OneKey");
    }
}

impl Default for DuatMode {
    fn default() -> Self {
        Self::new()
    }
}

////////// Cursor utility functions

fn edit_or_destroy_all(
    pa: &mut Pass,
    handle: &Handle,
    failed_at_least_once: &mut bool,
    mut f: impl FnMut(&mut Cursor) -> Option<()> + Clone,
) {
    handle.edit_all(pa, move |mut c| {
        let ret: Option<()> = f(&mut c);
        if ret.is_none() {
            c.reset();
            c.destroy();
            *failed_at_least_once = true;
        }
    })
}

fn select_to_end_of_line(set_anchor: bool, mut c: Cursor) {
    set_anchor_if_needed(set_anchor, &mut c);
    c.set_desired_vcol(usize::MAX);
    let pre_nl = match c.char() {
        '\n' => c.chars_rev().take_while(|(_, char)| *char != '\n').next(),
        _ => c.chars_fwd().take_while(|(_, char)| *char != '\n').last(),
    };
    if let Some((p, _)) = pre_nl {
        c.move_to(p);
    }
}

fn set_anchor_if_needed(set_anchor: bool, c: &mut Cursor) {
    if set_anchor {
        if c.anchor().is_none() {
            c.set_anchor();
        }
    } else {
        c.unset_anchor();
    }
}

////////// Object definitions

#[derive(Clone, Copy, PartialEq, Eq)]
enum SelType {
    BeforeEndOfLine,
    ToEndOfLine,
    Reverse,
    Extend,
    ExtendRev,
    Normal,
}

#[derive(PartialEq, Eq)]
enum Category {
    Word,
    Special,
    Space,
}

impl Category {
    fn of(char: char, opts: PrintOpts) -> Self {
        if opts.is_word_char(char) {
            Category::Word
        } else if [' ', '\t', '\n'].contains(&char) {
            Category::Space
        } else {
            Category::Special
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Object<'a> {
    Anchored(&'a str),
    Bounds(&'a str, &'a str),
    Argument(&'a str, &'a str, &'a str),
    Bound(&'a str),
}

impl<'a> Object<'a> {
    fn new(key_event: KeyEvent, opts: PrintOpts, brackets: Brackets) -> Option<Self> {
        static BRACKET_PATS: Memoized<Brackets, ([&str; 2], [&str; 3])> = Memoized::new();
        let m_and_u_patterns = |brackets: Brackets| {
            BRACKET_PATS.get_or_insert_with(brackets, || {
                let (s_pat, e_pat): (String, String) = brackets
                    .iter()
                    .enumerate()
                    .map(|(i, [s_b, e_b])| {
                        if i > 0 {
                            (format!("|{}", *s_b), format!("|{}", *e_b))
                        } else {
                            (s_b.to_string(), e_b.to_string())
                        }
                    })
                    .collect();
                let (s_arg, e_arg) = (format!(r"({s_pat})\s*"), format!(r"\s*({e_pat})"));

                ([s_pat.leak(), e_pat.leak()], [
                    r"(;|,)\s*",
                    s_arg.leak(),
                    e_arg.leak(),
                ])
            })
        };

        match key_event {
            event!('Q') => Some(Self::Bound("\"")),
            event!('q') => Some(Self::Bound("'")),
            event!('g') => Some(Self::Bound("`")),
            event!('|') => Some(Self::Bound(r"\|")),
            event!('$') => Some(Self::Bound(r"\$")),
            event!('^') => Some(Self::Bound(r"\^")),
            event!('s') => Some(Self::Bound(r"[\.;!\?]\s*")),
            event!('p') => Some(Self::Bound("^\n+|\n{2,}|\n$")),
            event!('b' | '(' | ')') => Some(Self::Bounds(r"\(", r"\)")),
            event!('B' | '{' | '}') => Some(Self::Bounds(r"\{", r"\}")),
            event!('r' | '[' | ']') => Some(Self::Bounds(r"\[", r"\]")),
            event!('a' | '<' | '>') => Some(Self::Bounds("<", ">")),
            event!('m' | 'M') | alt!('m' | 'M') => Some({
                let ([s_b, e_b], _) = m_and_u_patterns(brackets);
                Self::Bounds(s_b, e_b)
            }),
            event!('u') => Some({
                let (_, [m_b, s_arg, e_arg]) = m_and_u_patterns(brackets);
                Self::Argument(m_b, s_arg, e_arg)
            }),
            event!('w') => Some(Self::Anchored({
                static WORD_PATS: Memoized<&'static [char], &str> = Memoized::new();
                WORD_PATS.get_or_insert_with(opts.extra_word_chars, || {
                    let cat = opts.word_chars_regex();
                    format!("\\A({cat}+|[^{cat} \t\n]+)\\z").leak()
                })
            })),
            alt!('w') => Some(Self::Anchored("\\A[^ \t\n]+\\z")),
            event!(' ') => Some(Self::Anchored(r"\A\s*\z")),
            event!(mode::KeyCode::Char(char)) if !char.is_alphanumeric() => Some(Self::Bound({
                static BOUNDS: Memoized<char, &str> = Memoized::new();
                BOUNDS.get_or_insert_with(char, || char.to_string().leak())
            })),
            _ => None,
        }
    }

    fn find_ahead(self, c: &mut Cursor, s_count: usize) -> Option<Range<usize>> {
        let mut s_count = s_count as i32;
        match self {
            Object::Anchored(pat) => {
                let pat = pat.strip_suffix(r"\z").unwrap();
                c.search_fwd(pat).next()
            }
            Object::Bounds(s_b, e_b) => {
                let (_, range) = c.search_fwd_excl([s_b, e_b]).find(|&(id, _)| {
                    s_count += (id == 0) as i32 - (id == 1) as i32;
                    s_count <= 0
                })?;
                Some(range)
            }
            Object::Bound(b) => c.search_fwd(b).next(),
            Object::Argument(m_b, s_b, e_b) => {
                context::debug!("{m_b:?}, {s_b:?}, {e_b:?}");
                let caret = c.caret();
                let (_, range) = c.search_fwd([m_b, s_b, e_b]).find(|(id, range)| {
                    s_count += (*id == 1) as i32 - (*id == 2 && range.start != caret.byte()) as i32;
                    s_count == 0 || (s_count == 1 && *id == 0)
                })?;
                Some(range)
            }
        }
    }

    fn find_behind(self, c: &mut Cursor, e_count: usize) -> Option<Range<usize>> {
        let mut e_count = e_count as i32;
        match self {
            Object::Anchored(pat) => {
                let pat = pat.strip_prefix(r"\A").unwrap();
                c.search_rev(pat).next()
            }
            Object::Bounds(s_b, e_b) => {
                let (_, range) = c.search_rev([s_b, e_b]).find(|&(id, _)| {
                    e_count += (id == 1) as i32 - (id == 0) as i32;
                    e_count <= 0
                })?;
                Some(range)
            }
            Object::Bound(b) => c.search_rev(b).next(),
            Object::Argument(m_b, s_b, e_b) => {
                let (_, range) = c.search_rev([m_b, s_b, e_b]).find(|&(id, _)| {
                    e_count += (id == 2) as i32 - (id == 1) as i32;
                    e_count == 0 || (e_count == 1 && id == 0)
                })?;
                Some(range)
            }
        }
    }
}

////////// General utility functions

fn escaped_regex(str: &str) -> &'static str {
    static ESCAPED: Memoized<String, &str> = Memoized::new();
    ESCAPED.get_or_insert_with(str.to_string(), || escaped_str(str).leak())
}

fn escaped_str(str: &str) -> String {
    let mut escaped = String::new();
    for char in str.chars() {
        if let '(' | ')' | '{' | '}' | '[' | ']' | '$' | '^' | '.' | '*' | '+' | '?' | '|' = char {
            escaped.push('\\');
        }
        escaped.push(char);
    }
    escaped
}

struct Memoized<K: std::hash::Hash + std::cmp::Eq, V>(LazyLock<Mutex<HashMap<K, V>>>);

impl<K: std::hash::Hash + std::cmp::Eq, V: Clone + 'static> Memoized<K, V> {
    const fn new() -> Self {
        Self(LazyLock::new(Mutex::default))
    }

    fn get_or_insert_with(&self, k: K, f: impl FnOnce() -> V) -> V {
        self.0.lock().unwrap().entry(k).or_insert_with(f).clone()
    }
}

static SEARCH: Mutex<String> = Mutex::new(String::new());
