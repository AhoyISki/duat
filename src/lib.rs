//! `duat-kak` is the implementation of the [kakoune] editing model
//! for Duat. It's still a work in progress, but it already implements
//! most of the common commands from Kakoune, with some modifications
//! that I thought made sense.
//!
//! The plugin currently has 2 options: `insert_tabs` and
//! `set_cursor_forms`. `insert_tabs` will make the `Tab` key insert a
//! `\t` character, instead of an appropriate amount of spaces.
//! `set_cursor_forms` will create a hook to set the `caret.main.`
//! and `caret.extra` forms to mode specific variants, c.g.
//! `caret.main.Insert`.
//!
//! # Installation
//!
//! Just like other Duat plugins, this one can be installed by calling
//! `cargo add` in the config directory:
//!
//! ```bash
//! cargo add duat-kak@"*" --rename kak
//! ```
//!
//! Or, if you are using a `--git-deps` version of duat, do this:
//!
//! ```bash
//! cargo add --git https://github.com/AhoyISki/duat-kak --rename kak
//! ```
//!
//! # Keymaps
//!
//! This is a list of _currently_ mapped keys, not the ones that
//! appear in Kakoune.
//!
//! When reading keys, they follow Duat's [mapping] rules, that is:
//!
//! - `<A-{key}>` is a chord of `Alt + {key}`, same with `<C-{key}>`
//!   and `Control` and `<S-{key}>` with `Shift` (although that one is
//!   not usually needed).
//! - Special keys are enclosed in `<` `>` pairs (c.g. `<Home>`).
//! - Multiple keys in a row represent a sequence.
//!
//! In any mode, the `<Esc>` key will take you back to `normal` mode.
//!
//! In this plugin, a `moment` contains all of the changes performed
//! by each [`Cursor`], so if you press `u`, multiple changes may be
//! undone.
//!
//! ## `Normal` mode
//!
//! The keys in `normal` mode follow the following patterns:
//!
//! - All actions will be done to all selections.
//! - `word` characters follow Duat's [word chars], which are normally
//!   used to define where lines wrap.
//! - `WORD` characters are just any non-whitespace character.
//! - All keys that say "select", when typed with `<Shift>` will
//!   extend the selection instead (not necessarily growing it).
//! - Yanked selections are always pasted in the order they were
//!   yanked, looping around if there are less yanks than [`Cursor`]s.
//!
//! ### Object selection
//!
//! `h`, `<Left>`\
//! Selects the character to the left. Wraps around lines.
//!
//! `j`\
//! Selects the character below on the next line.
//!
//! `<Down>`\
//! Selects the character below on the next wrapped line (i.c vim's
//! `gj`).
//!
//! `k`\
//! Selects the character above on the previous line.
//!
//! `<Up>`\
//! Selects the character above on the previous wrapped line (i.c.
//! vim's `gk`).
//!
//! `l`, `<Right>`\
//! Selects the character to the right. Wraps around lines.
//!
//! `w`\
//! Selects the `word` and following space to the right of the
//! selection.
//!
//! `b`\
//! Selects the `word` followed by spaces to the left of the
//! selection.
//!
//! `c`\
//! Selects to the end of the next `word` to the right of the
//! selection.
//!
//! `<A-(w|b|c)>`\
//! The same as `(w|b|c)`, but over a `WORD`.
//!
//! `f{key}`\
//! Selects to the next occurrence of the `{key}` character.
//!
//! `t{key}`\
//! Selects until the next occurrence of the `{key}` character.
//!
//! `<A-(f|t)>{key}`\
//! Same as `(f|t)`, but in the opposite direction.
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
//! ### Changing text
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
//! Same as `(o|O)`, but just adds the new line.
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
//! `` ` ``\
//! Changes to lowercase.
//!
//! `~`\
//! Changes to uppercase.
//!
//! `<A->`\
//! Swaps the case of each character.
//!
//! `<A-)>`\
//! Rotates each selection's content forwards.
//!
//! `<A-(>`\
//! Rptates each selection's content backwards.
//!
//! `|`\
//! Changes mode to [`PipeSelections`], letting you pipe each
//! selection to an external program.
//!
//! ### Incremental Search
//!
//! The searching in this plugin is done through the [`IncSearch`]
//! [`Mode`] from Duat, with some [`IncSearcher`]s defined in this
//! crate. This means that search will be done incrementally over a
//! Regex pattern.
//!
//! `/`\
//! Searches forward for the next pattern, on each [`Cursor`].
//!
//! `<A-/>`\
//! Searches backwards for the previous pattern, on each [`Cursor`].
//!
//! `(?|<A-?>)`\
//! Follows the `Shift` pattern described above, so its the same as
//! `(/|<A-/>)`, but extending the selection instead.
//!
//! `s`\
//! Selects the pattern from within current selections.
//!
//! `S`\
//! Splits current selections by the pattern.
//!
//! ### Selection manipulation
//!
//! `;`\
//! Reduces selections to just the [caret].
//!
//! `<A-;>`\
//! Flips the [caret] and [anchor] of [`Cursor`]s around.
//!
//! `<A-:>`\
//! Places the [caret] ahead of the [anchor] in all selections.
//!
//! `<A-s>`\
//! Divides selection into multiple selections, one per line.
//!
//! `<A-S>`\
//! Splits into two [`Cursor`]s, one at each end of the selection.
//!
//! ## `goto` mode
//!
//! `goto` mode is entered with the `g` or `G` keys in `normal` mode.
//! The `G` follows the same `Shift` pattern described above.
//!
//! `h`\
//! Go to the beginning of the line (before indents, column 0).
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
//! Go to the previous [`Buffer`].
//!
//! `n`\
//! Go to the next [`Buffer`] (includes other windows).
//!
//! `N`\
//! Go to the previous [`Buffer`] (includes other windows).
//!
//! [kakoune]: https://github.com/mawww/kakoune
//! [word chars]: duat_core::opts::word_chars
//! [caret]: duat_core::mode::Cursor::caret
//! [anchor]: duat_core::mode::Cursor::anchor
//! [`Cursor`]: duat_core::mode::Cursor
//! [Undoes]: duat_core::text::Text::undo
//! [Redoes]: duat_core::text::Text::redo
//! [`Buffer`]: duat_core::buffer::Buffer
//! [`Cargo.toml`'s `dependencies` section]: https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html
//! [`IncSearch`]: duat_utils::modes::IncSearch
//! [`IncSearcher`]: duat_utils::modes::IncSearcher
//! [`PipeSelections`]: duat_utils::modes::PipeSelections
//! [mapping]: duat_core::mode::map
#![feature(iter_map_windows, if_let_guard, iter_array_chunks, iter_intersperse)]

use std::{
    collections::HashMap,
    ops::Range,
    sync::{LazyLock, Mutex, atomic::Ordering},
};

use duat::{
    mode::{self, Cursor, KeyCode::*},
    opts::PrintOpts,
    prelude::*,
};

use crate::normal::Brackets;
pub use crate::{insert::Insert, normal::Normal};

mod inc_searchers;
mod insert;
mod normal;
mod one_key;

/// The [`Plugin`] for the kakoune [`Mode`]s
///
/// This [`Plugin`] will change the default mode to one based on
/// Kakoune's [`Normal`].
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
/// If you don't want the [`Form`]s to change, see
/// [`Kak::dont_set_cursor_forms`].
///
/// [`Form`]: duat_core::form::Form
pub struct Kak {
    set_cursor_forms: bool,
    insert_tabs: bool,
    normal: Normal,
}

impl Kak {
    /// Returns a new instance of [`Kak`], the plugin for kakoune-like
    /// editing
    pub fn new() -> Self {
        Self {
            set_cursor_forms: true,
            insert_tabs: false,
            normal: Normal::new(),
        }
    }
}

impl Kak {
    /// Stop the automatic setting of cursor [`Form`]s
    ///
    /// [`Form`]: duat_core::form::Form
    pub fn dont_set_cursor_forms(self) -> Self {
        Self { set_cursor_forms: false, ..self }
    }

    /// Makes the tab key insert `\t` instead of spaces
    pub fn insert_tabs(self) -> Self {
        Self { insert_tabs: true, ..self }
    }

    /// Changes what is considered a "bracket" in [`Normal`] mode
    ///
    /// More specifically, this will change the behavior of keys like
    /// `'m'` and the `'u'` object, which will now consider more
    /// patterns when selecting.
    pub fn with_brackets<'a>(self, brackets: impl Iterator<Item = [&'a str; 2]>) -> Self {
        Self {
            normal: self.normal.with_brackets(brackets),
            ..self
        }
    }

    /// Makes it so the `'I'` key no longer indents the line
    ///
    /// By default, when you press `'I'`, the line will be reindented,
    /// in order to send you to the "proper" insertion spot, not just
    /// to the first non whitespace character.
    ///
    /// This function disables that behavior.
    pub fn with_no_indent_on_capital_i(self) -> Self {
        Self {
            normal: self.normal.with_no_indent_on_capital_i(),
            ..self
        }
    }

    /// Makes the `'f'` and `'t'` keys set the search pattern
    ///
    /// If you type `"fm"`, for example, and then type `'n'`, `'n'`
    /// will search for the next instance of an `'m'` in the
    /// [`Buffer`]
    pub fn f_and_t_set_search(self) -> Self {
        Self {
            normal: self.normal.f_and_t_set_search(),
            ..self
        }
    }
}

impl Plugin for Kak {
    fn plug(self, plugins: &Plugins) {
        plugins.require::<jump_list::JumpList>();
        plugins.require::<treesitter::TreeSitter>();

        mode::set_alt_is_reverse(true);
        mode::set_default::<Normal>(Normal::new());
        insert::INSERT_TABS.store(self.insert_tabs, Ordering::Relaxed);

        hook::add::<SearchPerformed>(|_, search| {
            *SEARCH.lock().unwrap() = search.to_string();
            Ok(())
        });

        if self.set_cursor_forms {
            form::enable_mask("Insert");
            form::enable_mask("Normal");
            form::enable_mask("OneKey");
        }
    }
}

impl Default for Kak {
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
    fn new(event: KeyEvent, opts: PrintOpts, brackets: Brackets) -> Option<Self> {
        static BRACKET_PATS: Memoized<Brackets, ([&str; 2], [&str; 3])> = Memoized::new();
        let m_and_u_patterns = |brackets: Brackets| {
            BRACKET_PATS.get_or_insert_with(brackets, || {
                let (s_pat, e_pat): (String, String) = brackets
                    .iter()
                    .map(|[s_b, e_b]| (*s_b, *e_b))
                    .intersperse(("|", "|"))
                    .collect();
                let (s_arg, e_arg) = (format!(r"({s_pat})\s*"), format!(r"\s*({e_pat})"));

                ([s_pat.leak(), e_pat.leak()], [
                    r"(;|,)\s*",
                    s_arg.leak(),
                    e_arg.leak(),
                ])
            })
        };

        match event {
            event!('Q') => Some(Self::Bound("\"")),
            event!('q') => Some(Self::Bound("'")),
            event!('g') => Some(Self::Bound("`")),
            event!('|') => Some(Self::Bound(r"\|")),
            event!('$') => Some(Self::Bound(r"\$")),
            event!('^') => Some(Self::Bound(r"\^")),
            event!('s') => Some(Self::Bound(r"[\.;!\?]")),
            event!('p') => Some(Self::Bound("^\n")),
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
            event!(Char(char)) if !char.is_alphanumeric() => Some(Self::Bound({
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
    ESCAPED.get_or_insert_with(str.to_string(), || {
        let mut escaped = String::new();
        for char in str.chars() {
            if let '(' | ')' | '{' | '}' | '[' | ']' | '$' | '^' | '.' | '*' | '+' | '?' | '|' =
                char
            {
                escaped.push('\\');
            }
            escaped.push(char);
        }
        escaped.leak()
    })
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
