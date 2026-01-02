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
//! `<A-a>`\
//! Select around object
//!
//! `[`,`]`\
//! Select around start/end of object
//!
//! `{`,`}`\
//! Extend around start/end of object
//!
//! `<A-i>`\
//! Select inside object
//!
//! `<A-[>`,`<A-]>`\
//! Select inside start/end of object
//!
//! `<A-{>`,`<A-}>`\
//! Extend inside start/end of object
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
pub use crate::{
    insert::{Insert, reindent},
    normal::Normal,
};

mod inc_searchers;
mod insert;
mod normal;
mod one_key;

use duat_base::hooks::SearchPerformed;
use duat_core::{
    Plugin, Plugins,
    buffer::Buffer,
    context::Handle,
    data::Pass,
    form, hook,
    lender::Lender,
    mode::{self, Cursor, KeyEvent, alt, event},
    opts::PrintOpts,
    text::Point,
};
use duat_treesitter::TsHandle;
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
    use std::sync::{Mutex, atomic::AtomicBool};

    use duat_core::{
        mode::{self, KeyCode, KeyEvent},
        text::txt,
    };

    use crate::{Insert, Memoized, escaped_regex, insert::TabMode, normal::Brackets};

    pub(crate) static INSERT_TABS: AtomicBool = AtomicBool::new(false);
    pub(crate) static DUATMODE_OPTS: Mutex<DuatModeOpts> = Mutex::new(DuatModeOpts::new());

    /// Options for the [`Insert`] and [`Normal`] modes of `duatmode`
    ///
    /// [`Normal`]: super::Normal
    #[derive(Clone, Copy)]
    pub struct DuatModeOpts {
        /// Inserts `\t`s instead of an equivalent amount of spaces
        ///
        /// The default is `false`
        pub insert_tabs: bool,
        /// How tabs should be handled in [`Insert`] mode.
        ///
        /// The default is [`TabMode::VerySmart`], which reindents the
        /// line if necessary, otherwise, triggers autocompletion.
        pub tab_mode: TabMode,
        /// Automatically indent new lines
        ///
        /// This makes it so if you press `<Enter>`, the new line will
        /// keep the same indentation as the old line.
        ///
        /// This is only done in [`Buffer`]s that aren't using
        /// `tree-sitter` or if the `<Enter>` key is not a part of
        /// `indent_keys`.
        ///
        /// The default is `true`
        pub auto_indent: bool,
        /// Characters that, upon being typed, reindent the current
        /// line
        ///
        /// The default is all the bracket characters, `\t` and `\n`
        pub indent_chars: &'static [char],
        /// Enables auto reindentation upon typing `I` in [`Normal`]
        /// mode
        ///
        /// The default is `true`.
        ///
        /// [`Normal`]: super::Normal
        pub indent_on_capital_i: bool,
        /// Makes the `'f'` and `'t'` keys set the search pattern
        ///
        /// If you type `"fm"`, for example, and then type `'n'`,
        /// `'n'` will search for the next instance of an
        /// `'m'` in the [`Buffer`]
        ///
        /// The default is `true`
        pub f_and_t_set_search: bool,
        pub(crate) brackets: Brackets,
    }

    impl DuatModeOpts {
        /// The default version of `DuatModeOpts`
        pub const fn new() -> Self {
            const B_PATS: Brackets = Brackets(&[[r"\(", r"\)"], [r"\{", r"\}"], [r"\[", r"\]"]]);
            Self {
                insert_tabs: false,
                tab_mode: TabMode::VerySmart,
                auto_indent: true,
                indent_chars: &['\n', '(', ')', '{', '}', '[', ']'],
                indent_on_capital_i: true,
                f_and_t_set_search: true,
                brackets: B_PATS,
            }
        }

        /// Changes what is considered a "bracket" in [`Normal`] mode
        ///
        /// More specifically, this will change the behavior of keys
        /// like `'m'` and the `'u'` object, which will now
        /// consider more patterns when selecting.
        pub fn set_brackets<'a>(&mut self, brackets: impl Iterator<Item = [&'a str; 2]>) {
            static BRACKETS: Memoized<Vec<[&str; 2]>, Brackets> = Memoized::new();

            let brackets: Vec<[&str; 2]> = brackets.map(|bs| bs.map(escaped_regex)).collect();
            assert!(
                brackets.iter().all(|[s_b, e_b]| s_b != e_b),
                "Brackets are not allowed to look the same"
            );

            self.brackets =
                BRACKETS.get_or_insert_with(brackets.clone(), || Brackets(brackets.leak()));
        }
    }

    impl Default for DuatModeOpts {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Sets options for the [`Insert`] and [`Normal`] modes
    ///
    /// [`Normal`]: super::Normal
    pub fn set(set_fn: impl FnOnce(&mut DuatModeOpts)) {
        let mut opts = *DUATMODE_OPTS.lock().unwrap();
        set_fn(&mut opts);

        mode::change_binding_description::<Insert>(
            &[KeyEvent::from(KeyCode::Tab)],
            match opts.tab_mode {
                TabMode::Normal => txt!("Insert tab"),
                TabMode::Smart => txt!("Reindent or insert tab"),
                TabMode::VerySmart => txt!("Reindent or next completion entry"),
            },
        );

        *DUATMODE_OPTS.lock().unwrap() = opts;
    }

    /// The options currently in effect
    ///
    /// Setting the values of these options will not change the
    /// current value. For that, check out [`opts::set`]
    ///
    /// [`opts::set`]: set
    pub fn get() -> DuatModeOpts {
        *DUATMODE_OPTS.lock().unwrap()
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
#[derive(Default)]
pub struct DuatMode;

impl Plugin for DuatMode {
    fn plug(self, plugins: &Plugins) {
        plugins.require::<duat_jump_list::JumpList>();
        plugins.require::<duat_treesitter::TreeSitter>();

        mode::set_alt_is_reverse(true);
        mode::set_default::<Normal>(Normal::new());

        hook::add::<SearchPerformed>(|_, search| *SEARCH.lock().unwrap() = search.to_string());

        normal::jump_list::add_jump_hook();

        form::enable_mask("Insert");
        form::enable_mask("Normal");
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

/// Regex patterns for searching inside and around objects
#[derive(Clone, Copy, PartialEq, Eq)]
struct Regexes<'o> {
    around: &'o str,
    inside: &'o str,
}

impl<'o> Regexes<'o> {
    fn simple(pat: &'o str) -> Self {
        Self { around: pat, inside: pat }
    }

    fn new(around: &'o str, inside: &'o str) -> Self {
        Self { around, inside }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Object<'o> {
    OneBound(Regexes<'o>),
    TwoBounds {
        ahead: Regexes<'o>,
        behind: Regexes<'o>,
        repeat: bool,
    },
    Argument {
        ahead: Regexes<'o>,
        behind: Regexes<'o>,
    },
    Indent,
}

impl<'o> Object<'o> {
    pub fn two_bounds_simple(behind: &'o str, ahead: &'o str) -> Self {
        Self::TwoBounds {
            ahead: Regexes::simple(ahead),
            behind: Regexes::simple(behind),
            repeat: true,
        }
    }
}

impl<'o> Object<'o> {
    fn new(key_event: KeyEvent, opts: PrintOpts, brackets: Brackets) -> Option<Self> {
        static BRACKET_PATS: Memoized<Brackets, [Regexes; 2]> = Memoized::new();
        let bound_patterns = |brackets: Brackets| {
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

                [
                    Regexes::simple(s_pat.leak() as &'static _),
                    Regexes::simple(e_pat.leak()),
                ]
            })
        };

        match key_event {
            event!('Q') => Some(Self::OneBound(Regexes::simple("\""))),
            event!('q') => Some(Self::OneBound(Regexes::simple("'"))),
            event!('g') => Some(Self::OneBound(Regexes::simple("`"))),
            event!('|') => Some(Self::OneBound(Regexes::simple(r"\|"))),
            event!('$') => Some(Self::OneBound(Regexes::simple(r"\$"))),
            event!('^') => Some(Self::OneBound(Regexes::simple(r"\^"))),
            event!('s') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r"[\.;!\?]\s*"),
                behind: Regexes::new(r"[^\.;!\?]*", r"\s*"),
                repeat: false,
            }),
            event!('p') => Some(Self::TwoBounds {
                ahead: Regexes::new(r"\n{2,}|\z", r"(^\n)*"),
                behind: Regexes::new(r"\n{2,}|\A", r"(^\n)*"),
                repeat: false,
            }),
            event!('b' | '(' | ')') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r"\)"),
                behind: Regexes::simple(r"\("),
                repeat: true,
            }),
            event!('B' | '{' | '}') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r"\}"),
                behind: Regexes::simple(r"\{"),
                repeat: true,
            }),
            event!('r' | '[' | ']') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r"\]"),
                behind: Regexes::simple(r"\["),
                repeat: true,
            }),
            event!('a' | '<' | '>') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r">"),
                behind: Regexes::simple(r"<"),
                repeat: true,
            }),
            event!('m' | 'M') | alt!('m' | 'M') => Some({
                let [behind, ahead] = bound_patterns(brackets);
                Self::TwoBounds { ahead, behind, repeat: true }
            }),
            event!('u') => Some({
                let [behind, ahead] = bound_patterns(brackets);
                Self::Argument { ahead, behind }
            }),
            event!('w') => Some({
                static WORD_PATS: Memoized<&'static [char], [Regexes; 2]> = Memoized::new();
                let [ahead, behind] = WORD_PATS.get_or_insert_with(opts.extra_word_chars, || {
                    let cat = opts.word_chars_regex();
                    [
                        Regexes::new(format!("\\A({cat}+|[^{cat} \t\n]+)\\s*").leak(), r"\s*"),
                        Regexes::new(format!("({cat}*|[^{cat} \t\n]*)\\z").leak(), ""),
                    ]
                });

                Self::TwoBounds { ahead, behind, repeat: false }
            }),
            alt!('w') => Some(Self::TwoBounds {
                ahead: Regexes::new("\\A[^ \t\n]+\\s*", r"\s*"),
                behind: Regexes::new("[^ \t\n]*\\z", ""),
                repeat: false,
            }),
            event!(' ') => Some(Self::TwoBounds {
                ahead: Regexes::new("\\A[ \t]\n*", "\n*"),
                behind: Regexes::new("\n*[ \t]\\z", "\n*"),
                repeat: false,
            }),
            event!('i') => Some(Self::Indent),
            event!(mode::KeyCode::Char(char)) if !char.is_alphanumeric() => Some(Self::OneBound({
                static BOUNDS: Memoized<char, Regexes> = Memoized::new();
                BOUNDS.get_or_insert_with(char, || Regexes::simple(char.to_string().leak()))
            })),
            _ => None,
        }
    }

    fn find_ahead(self, c: &mut Cursor, count: usize, is_inside: bool) -> Option<usize> {
        let mut s_diff = count as i32;
        let (range, inside_pat) = match self {
            Object::OneBound(ahead) => (
                c.search(ahead.around)
                    .from_caret()
                    .nth(count.saturating_sub(1))?,
                ahead.inside,
            ),
            Object::TwoBounds { ahead, repeat: false, .. } => {
                (c.search(ahead.around).from_caret().next()?, ahead.inside)
            }
            Object::TwoBounds { ahead, behind, repeat: true } => {
                let pat = [behind.around, ahead.around];
                let (_, range) = c.search(pat).from_caret().find(|&(id, _)| {
                    s_diff += (id == 0) as i32 - (id == 1) as i32;
                    s_diff <= 0
                })?;
                (range, ahead.inside)
            }
            Object::Argument { ahead, behind } => {
                let pat = [r"\s*([;,]\s*|\z)", behind.around, ahead.around];
                let (id, range) = c.search(pat).from_caret_excl().find(|(id, _)| {
                    s_diff += (*id == 1) as i32 - (*id == 2) as i32;
                    s_diff == 0 || (s_diff == 1 && *id == 0)
                })?;

                return Some(if is_inside {
                    if id == 0 {
                        range.start
                    } else {
                        c.search(r"\s*").range(..range.start).next_back()?.start
                    }
                } else if id == 0 {
                    range.end
                } else {
                    range.start
                });
            }
            Object::Indent => {
                let indent = c.indent();
                let mut point = c.text().point_at_line(c.caret().line());

                while c.indent_on(point) >= indent && point.line() < c.text().len().line() {
                    point = c.text().point_at_line(point.line() + 1)
                }

                return Some(if is_inside {
                    point.byte()
                } else {
                    c.text()
                        .lines(point..)
                        .find(|(_, line)| line.chars().any(|c| !c.is_ascii_whitespace()))
                        .map(|(num, _)| c.text().point_at_line(num))
                        .unwrap_or(c.text().len())
                        .byte()
                });
            }
        };

        Some(if is_inside {
            let pat = inside_pat;
            c.search(pat).range(..range.end).next_back()?.start
        } else {
            range.end
        })
    }

    fn find_behind(self, c: &mut Cursor, count: usize, is_inside: bool) -> Option<usize> {
        let mut e_diff = count as i32;
        let (range, inside_pat) = match self {
            Object::OneBound(behind) => (
                c.search(behind.around)
                    .to_caret()
                    .nth_back(count.saturating_sub(1))?,
                behind.inside,
            ),
            Object::TwoBounds { behind, repeat: false, .. } => (
                c.search(behind.around).to_caret().next_back()?,
                behind.inside,
            ),
            Object::TwoBounds { ahead, behind, repeat: true } => {
                let pat = [behind.around, ahead.around];
                let (_, range) = c.search(pat).to_caret().rev().find(|&(id, _)| {
                    e_diff += (id == 1) as i32 - (id == 0) as i32;
                    e_diff <= 0
                })?;
                (range, behind.inside)
            }
            Object::Argument { ahead, behind } => {
                let pat = [r"(\A|[;,])\s*", behind.around, ahead.around];
                let (id, range) = c.search(pat).to_caret_incl().rev().find(|(id, _)| {
                    e_diff += (*id == 2) as i32 - (*id == 1) as i32;
                    e_diff == 0 || (e_diff == 1 && *id == 0)
                })?;

                return Some(if is_inside {
                    if id == 0 {
                        range.end
                    } else {
                        c.search(r"\s*").range(range.end..).next()?.end
                    }
                } else if id == 0 {
                    range.start + 1
                } else {
                    range.end
                });
            }
            Object::Indent => {
                let indent = c.indent();
                let mut point = c.text().point_at_line(c.caret().line());

                while let Some(prev_line) = point.line().checked_sub(1) {
                    let prev = c.text().point_at_line(prev_line);
                    if c.indent_on(prev) < indent {
                        break;
                    }
                    point = c.text().point_at_line(point.line() - 1)
                }

                return Some(if is_inside {
                    point.byte()
                } else {
                    c.text()
                        .lines(..point)
                        .rev()
                        .take_while(|(_, line)| line.chars().all(|c| c.is_ascii_whitespace()))
                        .last()
                        .map(|(num, _)| c.text().point_at_line(num))
                        .unwrap_or(point)
                        .byte()
                });
            }
        };

        Some(if is_inside {
            let pat = inside_pat;
            c.search(pat).range(range.start..).next()?.end
        } else {
            range.start
        })
    }
}

////////// General utility functions

fn escaped_regex(str: impl ToString) -> &'static str {
    static ESCAPED: Memoized<String, &str> = Memoized::new();
    ESCAPED.get_or_insert_with(str.to_string(), || escaped_str(str).leak())
}

fn escaped_str(str: impl ToString) -> String {
    let str = str.to_string();
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

fn indents(pa: &mut Pass, handle: &Handle) -> (std::vec::IntoIter<usize>, bool) {
    fn prev_non_empty_line_points<S>(c: &mut Cursor<Buffer, S>) -> Option<Range<Point>> {
        let line_start = c.text().point_at_line(c.caret().line());
        let mut lines = c.lines_on(..line_start).rev();
        let prev = lines
            .find_map(|(n, l): (usize, &str)| l.chars().any(|c| !c.is_whitespace()).then_some(n));
        prev.map(|n| c.text().line_range(n))
    }

    if let Some(indents) = handle.ts_get_indentations(pa, ..) {
        (indents.into_iter(), true)
    } else {
        let indents: Vec<_> = handle.edit_iter(pa, |iter| {
            iter.map_into_iter(|mut c| {
                let prev_non_empty = prev_non_empty_line_points(&mut c);
                prev_non_empty
                    .map(|range| c.indent_on(range.start))
                    .unwrap_or(0)
            })
            .collect()
        });

        (indents.into_iter(), false)
    }
}
