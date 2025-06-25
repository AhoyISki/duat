//! `duat-kak` is the implementation of the [kakoune] editing model
//! for Duat. It's still a work in progress, but it already implements
//! most of the common commands from Kakoune, with some modifications
//! that I thought made sense.
//!
//! The plugin currently has 2 options: `insert_tabs` and
//! `set_cursor_forms`. `insert_tabs` will make the `Tab` key insert a
//! `\t` character, instead of an appropriate amount of spaces.
//! `set_cursor_forms` will create a hook to set the `MainCursor`,
//! `ExtraCursor`, `MainSelection` and `ExtraSelection` forms to mode
//! specific varieties, c.g. `MainCursorInsert`.
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
//! Go to the previous [`File`].
//!
//! `n`\
//! Go to the next [`File`] (includes other windows).
//!
//! `N`\
//! Go to the previous [`File`] (includes other windows).
//!
//! [kakoune]: https://github.com/mawww/kakoune
//! [word chars]: duat_core::cfg::word_chars
//! [caret]: duat_core::mode::Cursor::caret
//! [anchor]: duat_core::mode::Cursor::anchor
//! [`Cursor`]: duat_core::mode::Cursor
//! [Undoes]: duat_core::text::Text::undo
//! [Redoes]: duat_core::text::Text::redo
#![feature(iter_map_windows, if_let_guard, iter_array_chunks, iter_intersperse)]

use std::{
    collections::HashMap,
    marker::PhantomData,
    sync::{
        LazyLock, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use duat_core::{
    cfg::WordChars,
    mode::{Cursor, KeyCode::*, KeyEvent as Event, KeyMod as Mod, Selections},
    prelude::*,
    text::{Point, Searcher},
};
use duat_utils::{
    hooks::SearchPerformed,
    modes::{
        ExtendFwd, ExtendRev, IncSearch, IncSearcher, Pager, PipeSelections, RunCommands,
        SearchFwd, SearchRev,
    },
    widgets::LogBook,
};
use treesitter::TsCursor;

/// The [`Plugin`] for the kakoune [`Mode`]s
///
/// This [`Plugin`] will change the default mode to a facimily of
/// Kakoune's [`Normal`].
///
/// It also adds a hook to automatically change the forms of the
/// cursors when the mode changes. This is the pattern that the forms
/// take:
///
/// - On [`Insert`] mode: `"MainCursor.Insert"`,
///   `"ExtraCursor.Insert"`
/// - On [`Normal`] mode: `"MainCursor.Normal"`,
///   `"ExtraCursor.Normal"`
///
/// And so on and so forth.
///
/// If you don't want the [`Form`]s to change, see
/// [`Kak::dont_set_cursor_forms`].
///
/// [`Form`]: duat_core::Form
pub struct Kak<U> {
    set_cursor_forms: bool,
    insert_tabs: bool,
    _u: PhantomData<U>,
}

impl<U: Ui> Plugin<U> for Kak<U> {
    fn new() -> Self {
        Self {
            set_cursor_forms: true,
            insert_tabs: false,
            _u: PhantomData,
        }
    }

    fn plug(self) {
        mode::set_alt_is_reverse(true);
        duat_core::mode::set_default::<Normal, U>(Normal::new());
        INSERT_TABS.store(self.insert_tabs, Ordering::Relaxed);

        hook::add::<SearchPerformed, U>(|_, search| {
            *SEARCH.lock().unwrap() = search.to_string();
        });


        if self.set_cursor_forms {
            form::enable_mask("Insert");
            form::enable_mask("Normal");
            form::enable_mask("OneKey");
        }
    }
}

impl<U> Kak<U> {
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
}

#[derive(Clone, Copy)]
pub struct Normal {
    sel_type: SelType,
    brackets: Brackets,
    indent_on_capital_i: bool,
    f_and_t_set_search: bool,
}

impl Normal {
    /// Returns an instance of the [`Normal`] mode, inspired by
    /// Kakoune
    pub fn new() -> Self {
        const B_PATS: Brackets = Brackets(&[[r"\(", r"\)"], [r"\{", r"\}"], [r"\[", r"\]"]]);
        Normal {
            sel_type: SelType::Normal,
            brackets: B_PATS,
            indent_on_capital_i: false,
            f_and_t_set_search: false,
        }
    }

    /// [`Normal`] mode with different type of selection
    fn new_with_sel_type(sel_type: SelType) -> Self {
        let mut normal = Self::new();
        normal.sel_type = sel_type;
        normal
    }

    /// Changes what is considered a "bracket" in [`Normal`] mode
    ///
    /// More specifically, this will change the behavior of keys like
    /// `'m'` and the `'u'` object, which will now consider more
    /// patterns when selecting.
    pub fn with_brackets<'a>(self, brackets: impl Iterator<Item = [&'a str; 2]>) -> Self {
        static BRACKETS: Memoized<Vec<[&str; 2]>, Brackets> = Memoized::new();

        let brackets: Vec<[&str; 2]> = brackets.map(|bs| bs.map(escaped_regex)).collect();
        assert!(
            brackets.iter().all(|[s_b, e_b]| s_b != e_b),
            "Brackets are not allowed to look the same"
        );

        let brackets = BRACKETS.get_or_insert_with(brackets.clone(), || Brackets(brackets.leak()));
        Self { brackets, ..self }
    }

    /// Makes it so the `'I'` key no longer indents the line
    ///
    /// By default, when you press `'I'`, the line will be reindented,
    /// in order to send you to the "proper" insertion spot, not just
    /// to the first non whitespace character.
    ///
    /// This function disables that behavior.
    pub fn with_no_indent_on_capital_i(self) -> Self {
        Self { indent_on_capital_i: false, ..self }
    }

    /// Makes the `'f'` and `'t'` keys set the search pattern
    ///
    /// If you type `"fm"`, for example, and then type `'n'`, `'n'`
    /// will search for the next instance of an `'m'` in the [`File`]
    pub fn f_and_t_set_search(self) -> Self {
        Self { f_and_t_set_search: true, ..self }
    }
}

impl<U: Ui> Mode<U> for Normal {
    type Widget = File<U>;

    fn send_key(&mut self, pa: &mut Pass, key: Event, handle: Handle<Self::Widget, U>) {
        let wc = handle.cfg(pa).word_chars;

        match key {
            ////////// Basic movement keys
            key!(Char('h' | 'H') | Left, Mod::NONE | Mod::SHIFT) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor = key.code == Char('H') || key.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(-1);
                });
                self.sel_type = SelType::Normal;
            }
            key!(Down) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers.contains(Mod::SHIFT), &mut c);
                c.move_ver_wrapped(1);
            }),
            key!(Up) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers.contains(Mod::SHIFT), &mut c);
                c.move_ver_wrapped(-1);
            }),
            key!(Down, Mod::ALT) => handle.scroll_ver(pa, 1),
            key!(Up, Mod::ALT) => handle.scroll_ver(pa, -1),
            key!(Char('l' | 'L') | Right, Mod::NONE | Mod::SHIFT) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor = key.code == Char('L') || key.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(1);
                });
                self.sel_type = SelType::Normal;
            }
            key!(Char('j' | 'J')) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.code == Char('J'), &mut c);
                c.move_ver(1);
                let v_caret = c.v_caret();
                if c.char() == '\n'
                    && v_caret.char_col() > 0
                    && self.sel_type != SelType::ToEndOfLine
                {
                    c.move_hor(-1);
                    c.set_desired_vcol(if self.sel_type == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        v_caret.desired_visual_col()
                    });
                }
            }),
            key!(Char('k' | 'K')) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.code == Char('K'), &mut c);
                c.move_ver(-1);
                let v_caret = c.v_caret();
                if c.char() == '\n'
                    && v_caret.char_col() > 0
                    && self.sel_type != SelType::ToEndOfLine
                {
                    c.move_hor(-1);
                    c.set_desired_vcol(if self.sel_type == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        v_caret.desired_visual_col()
                    });
                }
            }),

            ////////// Object selection keys
            key!(Char('w'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = no_nl_windows(c.chars_fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, wc) == Category::of(c1, wc) {
                        c.move_to(p0);
                    } else {
                        c.move_to(p1);
                    }

                    let points = c.search_fwd(word_and_space(alt_word, wc), None).next();
                    if let Some([_, p1]) = points {
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                    }
                };
            }),
            key!(Char('e'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = no_nl_windows(c.chars_fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, wc) == Category::of(c1, wc) {
                        c.move_to(p0);
                    } else {
                        c.move_to(p1);
                    }

                    let points = c.search_fwd(space_and_word(alt_word, wc), None).next();
                    if let Some([_, p1]) = points {
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                    }
                };
            }),
            key!(Char('b'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = {
                    let iter = [(c.caret(), c.char())].into_iter().chain(c.chars_rev());
                    no_nl_windows(iter).next()
                };
                if let Some(((p1, c1), (_, c0))) = init {
                    c.move_to(p1);
                    if Category::of(c0, wc) == Category::of(c1, wc) {
                        c.move_hor(1);
                    }
                    let points = c.search_rev(word_and_space(alt_word, wc), None).next();
                    if let Some([p0, p1]) = points {
                        c.move_to(p0);
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                        c.swap_ends();
                    };
                };
            }),

            key!(Char('W'), Mod::ALT | Mod::NONE) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut c);
                c.move_hor(1);
                let points = c.search_fwd(word_and_space(alt_word, wc), None).next();
                if let Some([_, p1]) = points {
                    c.move_to(p1);
                    c.move_hor(-1);
                }
            }),
            key!(Char('E'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut c);
                c.move_hor(1);
                let points = c.search_fwd(space_and_word(alt_word, wc), None).next();
                if let Some([_, p1]) = points {
                    c.move_to(p1);
                    c.move_hor(-1);
                }
            }),
            key!(Char('B'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut c);
                let points = c.search_rev(word_and_space(alt_word, wc), None).next();
                if let Some([p0, _]) = points {
                    c.move_to(p0);
                }
            }),

            key!(Char('x')) => handle.edit_all(pa, |mut c| {
                self.sel_type = SelType::ToEndOfLine;
                set_anchor_if_needed(true, &mut c);
                c.set_caret_on_start();
                let p0 = c.search_rev("\n", None).next().map(|[_, p0]| p0);
                c.move_to(p0.unwrap_or_default());
                c.swap_ends();

                let p1 = c.search_fwd("\n", None).next().map(|[p1, _]| p1);
                if let Some(p1) = p1.or(c.last_point()) {
                    c.move_to(p1);
                }
                c.set_desired_vcol(usize::MAX);
            }),
            key!(Char('f' | 'F' | 't' | 'T'), Mod::NONE | Mod::ALT) => {
                let mf = key.modifiers;
                let sel_type = match (mf.contains(Mod::SHIFT), mf.contains(Mod::ALT)) {
                    (true, true) => SelType::ExtendRev,
                    (true, false) => SelType::Extend,
                    (false, true) => SelType::Reverse,
                    (false, false) => SelType::Normal,
                };

                mode::set::<U>(if let Char('f' | 'F') = key.code {
                    OneKey::Find(sel_type, self.f_and_t_set_search)
                } else {
                    OneKey::Until(sel_type, self.f_and_t_set_search)
                });
            }
            key!(Char('l' | 'L'), Mod::ALT) | key!(End) => handle.edit_all(pa, |mut c| {
                if key.code == Char('l') {
                    c.unset_anchor();
                }
                select_to_end_of_line(true, c);
                self.sel_type = SelType::BeforeEndOfLine;
            }),
            key!(Char('h' | 'H'), Mod::ALT) | key!(Home) => handle.edit_all(pa, |mut c| {
                if key.code == Char('h') {
                    c.unset_anchor();
                }
                set_anchor_if_needed(true, &mut c);
                c.move_hor(-(c.v_caret().char_col() as i32));
            }),
            key!(Char('a'), Mod::ALT) => mode::set::<U>(OneKey::Around(self.brackets)),
            key!(Char('i'), Mod::ALT) => mode::set::<U>(OneKey::Inside(self.brackets)),
            key!(Char('%')) => {
                handle.edit_main(pa, |mut c| {
                    c.move_to_start();
                    c.set_anchor();
                    c.move_to(c.last_point().unwrap())
                });
            }
            key!(Char('m' | 'M')) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |c| {
                    let object = Object::from_char('m', c.cfg().word_chars, self.brackets).unwrap();
                    let [p2, p3] = object.find_ahead(c, 0, None)?;
                    let prev_caret = c.caret();
                    set_anchor_if_needed(key.code == Char('M'), c);
                    c.move_to(p3);
                    c.move_hor(-1);

                    let [s_b, e_b] = self.brackets.bounds_matching(c.contiguous_in(p2..p3))?;
                    let [p0, _] = Object::Bounds(s_b, e_b).find_behind(c, 1, None)?;
                    if key.code == Char('m') {
                        c.set_anchor();
                    }
                    c.move_to(p0);
                    if prev_caret.char() != p3.char() - 1 {
                        if key.code == Char('m') {
                            c.set_anchor();
                        }
                        c.move_to(p3);
                        c.move_hor(-1);
                    }

                    Some(())
                })
            }
            key!(Char('m' | 'M'), Mod::ALT) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |c| {
                    let object = Object::from_char('m', c.cfg().word_chars, self.brackets).unwrap();
                    let [p0, p1] = object.find_behind(c, 0, None)?;
                    let prev_caret = c.caret();
                    set_anchor_if_needed(key.code == Char('M'), c);
                    c.move_to(p0);

                    let [s_b, e_b] = self.brackets.bounds_matching(c.contiguous_in(p0..p1))?;
                    let [_, p3] = Object::Bounds(s_b, e_b).find_ahead(c, 1, None)?;
                    if key.code == Char('m') {
                        c.set_anchor();
                    }
                    c.move_to(p3);
                    c.move_hor(-1);
                    if prev_caret != p0 {
                        if key.code == Char('m') {
                            c.set_anchor();
                        }
                        c.move_to(p0);
                    }

                    Some(())
                })
            }

            ////////// Insertion mode keys
            key!(Char('i')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_start();
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('I')) => {
                handle.new_moment(pa);
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    if self.indent_on_capital_i {
                        reindent(&mut c, &mut processed_lines);
                    } else {
                        c.unset_anchor();
                        c.move_hor(-(c.v_caret().char_col() as i32));
                        c.set_anchor();
                        let indent = c.indent();
                        c.move_hor(indent as i32);
                    }
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('a')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_end();
                    c.move_hor(1);
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('A')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    c.unset_anchor();
                    let (p, _) = c.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                    c.move_to(p);
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('o' | 'O'), Mod::NONE | Mod::ALT) => {
                handle.new_moment(pa);
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    if key.code == Char('O') {
                        c.set_caret_on_start();
                        let char_col = c.v_caret().char_col();
                        c.move_hor(-(char_col as i32));
                        c.insert("\n");
                        if key.modifiers == Mod::NONE {
                            reindent(&mut c, &mut processed_lines);
                            c.move_hor(c.indent() as i32);
                        } else {
                            c.move_hor(char_col as i32 + 1);
                        }
                    } else {
                        c.set_caret_on_end();
                        let caret = c.caret();
                        let (p, _) = c.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                        c.move_to(p);
                        c.append("\n");
                        if key.modifiers == Mod::NONE {
                            c.move_hor(1);
                            reindent(&mut c, &mut processed_lines);
                            c.move_hor(c.indent() as i32);
                        } else {
                            c.move_to(caret);
                        }
                    }
                });
                if key.modifiers == Mod::NONE {
                    mode::set::<U>(Insert::new());
                }
            }

            ////////// Selection alteration keys
            key!(Char('r')) => {
                handle.new_moment(pa);
                mode::set::<U>(OneKey::Replace)
            }
            key!(Char('`')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    let lower = c
                        .selection()
                        .flat_map(str::chars)
                        .flat_map(char::to_lowercase);
                    c.replace(lower.collect::<String>());
                })
            }
            key!(Char('~')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    let upper = c
                        .selection()
                        .flat_map(str::chars)
                        .flat_map(char::to_uppercase);
                    c.replace(upper.collect::<String>());
                })
            }
            key!(Char('`'), Mod::ALT) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    let inverted = c.selection().flat_map(str::chars).map(|c| {
                        if c.is_uppercase() {
                            c.to_lowercase().collect::<String>()
                        } else {
                            c.to_uppercase().collect()
                        }
                    });
                    c.replace(inverted.collect::<String>());
                })
            }

            ////////// Advanced selection manipulation
            key!(Char(';'), Mod::ALT) => handle.edit_all(pa, |mut c| c.swap_ends()),
            key!(Char(';')) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
            }),
            key!(Char(':'), Mod::ALT) => handle.edit_all(pa, |mut c| {
                c.set_caret_on_end();
            }),
            key!(Char(')')) => handle.write_selections(pa, |s| s.rotate_main(1)),
            key!(Char('(')) => handle.write_selections(pa, |s| s.rotate_main(-1)),
            key!(Char(')'), Mod::ALT) => {
                handle.new_moment(pa);
                let last_sel = handle.edit_iter(pa, |mut iter| {
                    let mut last_sel = iter.next().map(|c| c.selection().to_string());

                    while let Some(mut c) = iter.next() {
                        let selection = c.selection().to_string();
                        c.replace(last_sel.replace(selection).unwrap());
                    }

                    last_sel
                });

                handle.edit_nth(pa, 0, |mut c| c.replace(last_sel.unwrap()));
            }
            key!(Char('('), Mod::ALT) => {
                handle.new_moment(pa);
                let mut selections = Vec::<String>::new();
                handle.edit_all(pa, |c| selections.push(c.selection().collect()));
                let mut s_iter = selections.into_iter().cycle();
                s_iter.next();
                handle.edit_all(pa, |mut c| {
                    if let Some(next) = s_iter.next() {
                        c.replace(next);
                    }
                });
            }
            key!(Char('_'), Mod::ALT) => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_end();
                    c.move_hor(1);
                });
                // In the first iteration, connected Cursors are joined, this one just
                // undoes the movement.
                handle.edit_all(pa, |mut c| {
                    c.move_hor(-1);
                });
            }
            key!(Char('s'), Mod::ALT) => handle.edit_all(pa, |mut c| {
                c.set_caret_on_start();
                let Some(end) = c.anchor() else {
                    return;
                };
                let lines: Vec<[Point; 2]> = c.search_fwd("[^\n]*\n", Some(end)).collect();
                let mut last_p1 = c.caret();
                for [p0, p1] in lines {
                    let mut e_copy = c.copy();
                    e_copy.move_to(p0);
                    e_copy.set_anchor();
                    e_copy.move_to(p1);
                    e_copy.move_hor(-1);

                    last_p1 = p1;
                }
                c.move_to(last_p1);
                c.swap_ends();
            }),
            key!(Char('S'), Mod::ALT) => handle.edit_all(pa, |mut c| {
                if c.anchor().is_some() {
                    let mut e_copy = c.copy();
                    e_copy.swap_ends();
                    e_copy.unset_anchor();
                    c.unset_anchor();
                }
            }),

            ////////// Clipboard keys
            key!(Char('y')) => {
                handle.new_moment(pa);
                copy_selections(pa, &handle)
            }
            key!(Char('d'), Mod::NONE | Mod::ALT) => {
                handle.new_moment(pa);
                if key.modifiers == Mod::NONE {
                    copy_selections(pa, &handle);
                }
                handle.edit_all(pa, |mut c| {
                    c.replace("");
                    c.unset_anchor();
                });
            }
            key!(Char('c'), Mod::NONE | Mod::ALT) => {
                handle.new_moment(pa);
                if key.modifiers == Mod::NONE {
                    copy_selections(pa, &handle);
                }
                handle.edit_all(pa, |mut c| {
                    c.replace("");
                    c.unset_anchor();
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('p' | 'P')) => {
                let pastes = paste_strings();
                if !pastes.is_empty() {
                    handle.new_moment(pa);
                    let mut p_iter = pastes.iter().cycle();
                    handle.edit_all(pa, |mut c| {
                        let paste = p_iter.next().unwrap();
                        // If it ends in a new line, we gotta move to the start of the line.
                        if key.code == Char('p') {
                            c.set_caret_on_end();
                            if paste.ends_with('\n') {
                                let caret = c.caret();
                                let (p, _) =
                                    c.chars_fwd().find(|(_, c)| *c == '\n').unwrap_or_default();
                                c.move_to(p);
                                c.append(paste);
                                c.move_to(caret);
                            } else {
                                c.move_hor(-(c.v_caret().char_col() as i32));
                            }
                        } else if key.code == Char('P') {
                            c.set_caret_on_start();
                            if paste.ends_with('\n') {
                                let char_col = c.v_caret().char_col();
                                c.move_hor(-(char_col as i32));
                                c.insert(paste);
                                c.move_hor(char_col as i32 + 1);
                            } else {
                                c.insert(paste)
                            }
                        }
                    });
                }
            }
            key!(Char('R')) => {
                let pastes = paste_strings();
                if !pastes.is_empty() {
                    handle.new_moment(pa);
                    let mut p_iter = pastes.iter().cycle();
                    handle.edit_all(pa, |mut c| c.replace(p_iter.next().unwrap()));
                }
            }

            ////////// Cursor creation and destruction
            key!(Char(',')) => handle.write(pa, |f, _| {
                f.text_mut().selections_mut().unwrap().remove_extras()
            }),
            key!(Char('C')) => {
                handle.new_moment(pa);
                handle.edit_last(pa, |mut c| {
                    let v_caret = c.v_caret();
                    c.copy();
                    if let Some(v_anchor) = c.v_anchor() {
                        let lines_diff = v_anchor.line() as i32 - c.caret().line() as i32;
                        let len_lines = lines_diff.unsigned_abs() as usize;
                        while c.caret().line() + len_lines < c.len().line() {
                            c.move_ver(len_lines as i32 + 1);
                            c.set_anchor();
                            c.set_desired_vcol(v_anchor.visual_col());
                            c.move_ver(lines_diff);
                            c.swap_ends();
                            if c.v_caret().visual_col() <= v_caret.visual_col()
                                && c.v_anchor().unwrap().visual_col() <= v_anchor.visual_col()
                            {
                                return;
                            }
                            c.swap_ends();
                        }
                    } else {
                        while c.caret().line() < c.len().line() {
                            if c.move_ver(1) == 0 {
                                break;
                            }
                            if c.v_caret().visual_col() == v_caret.visual_col() {
                                return;
                            }
                        }
                    }
                    c.destroy();
                });
            }
            key!(Char('C'), Mod::ALT) => {
                handle.new_moment(pa);
                handle.edit_nth(pa, 0, |mut c| {
                    let v_caret = c.v_caret();
                    c.copy();
                    if let Some(v_anchor) = c.v_anchor() {
                        let lines_diff = v_anchor.line() as i32 - c.caret().line() as i32;
                        let len_lines = lines_diff.unsigned_abs() as usize;
                        while c.caret().line().checked_sub(len_lines + 1).is_some() {
                            c.move_ver(-1 - len_lines as i32);
                            c.set_anchor();
                            c.set_desired_vcol(v_anchor.visual_col());
                            c.move_ver(lines_diff);
                            c.swap_ends();
                            if c.v_caret().visual_col() == v_caret.visual_col()
                                && c.v_anchor().unwrap().visual_col() == v_anchor.visual_col()
                            {
                                return;
                            }
                            c.swap_ends();
                        }
                    } else {
                        while c.caret().line() > 0 {
                            c.move_ver(-1);
                            if c.v_caret().visual_col() == v_caret.visual_col() {
                                return;
                            }
                        }
                    }
                    c.destroy();
                });
            }

            ////////// Search keys
            key!(Char('/')) => mode::set::<U>(IncSearch::new(SearchFwd)),
            key!(Char('/'), Mod::ALT) => mode::set::<U>(IncSearch::new(SearchRev)),
            key!(Char('?')) => mode::set::<U>(IncSearch::new(ExtendFwd)),
            key!(Char('?'), Mod::ALT) => mode::set::<U>(IncSearch::new(ExtendRev)),
            key!(Char('s')) => mode::set::<U>(IncSearch::new(Select)),
            key!(Char('S')) => mode::set::<U>(IncSearch::new(Split)),
            key!(Char('n' | 'N'), Mod::NONE | Mod::ALT) => {
                let search = SEARCH.lock().unwrap();
                if search.is_empty() {
                    context::error!("No search pattern set");
                    return;
                }
                handle.edit_main(pa, |mut c| {
                    if key.code == Char('N') {
                        c.copy();
                    }
                    let caret = c.caret();
                    let next = if key.modifiers == Mod::ALT {
                        c.search_rev(&*search, None).find(|[p, _]| *p != caret)
                    } else {
                        c.search_fwd(&*search, None).find(|[p, _]| *p != caret)
                    };
                    if let Some([p0, p1]) = next {
                        c.move_to(p0);
                        if p1 > p0 {
                            c.set_anchor();
                            c.move_to(p1);
                            c.move_hor(-1);
                        }
                    }
                });
            }

            ////////// Other mode changing keys
            key!(Char(':')) => mode::set::<U>(RunCommands::new()),
            key!(Char('|')) => {
                handle.new_moment(pa);
                mode::set::<U>(PipeSelections::new())
            }
            key!(Char('G')) => mode::set::<U>(OneKey::GoTo(SelType::Extend)),
            key!(Char('g')) => mode::set::<U>(OneKey::GoTo(SelType::Normal)),
            key!(Char(' ')) => mode::set::<U>(mode::User),

            ////////// History manipulation
            key!(Char('u')) => handle.undo(pa),
            key!(Char('U')) => handle.redo(pa),
            _ => {}
        }
    }

    fn on_switch(&mut self, _: &mut Pass, handle: Handle<Self::Widget, U>) {
        handle.set_mask("Normal");
    }
}

impl Default for Normal {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct Insert {
    insert_tabs: bool,
    indent_keys: Vec<char>,
}

impl Insert {
    /// Returns a new instance of Kakoune's [`Insert`]
    pub fn new() -> Self {
        Self {
            insert_tabs: INSERT_TABS.load(Ordering::Relaxed),
            indent_keys: vec!['\n', '\t', '(', ')', '{', '}', '[', ']'],
        }
    }

    /// Returns Kakoune's [`Insert`] mode, inserting tabs
    pub fn with_tabs(self) -> Self {
        Self { insert_tabs: true, ..self }
    }

    /// Returns Kakoune's [`Insert`] mode, not inserting tabs
    pub fn without_tabs(self) -> Self {
        Self { insert_tabs: false, ..self }
    }

    /// Which [`char`]s, when sent, should reindent the line
    ///
    /// By default, this is `'\n'` and the `'('`, `'{'`, `'['` pairs.
    /// Note that you have to include `'\n'` in order for the
    /// [`Enter`] key to reindent.
    ///
    /// Additionally, if you add '\t' to the list of indent keys, upon
    /// pressint [`Tab`] on the first character of the line, it will
    /// automatically be indented by the right amount.
    pub fn with_indent_keys(self, chars: impl Iterator<Item = char>) -> Self {
        Self { indent_keys: chars.collect(), ..self }
    }
}

impl Default for Insert {
    fn default() -> Self {
        Self::new()
    }
}

impl<U: Ui> Mode<U> for Insert {
    type Widget = File<U>;

    fn send_key(&mut self, pa: &mut Pass, key: Event, handle: Handle<Self::Widget, U>) {
        if let key!(Left | Down | Up | Right, mods) = key
            && mods.contains(Mod::SHIFT)
        {
            handle.edit_all(pa, |mut c| {
                if c.anchor().is_none() {
                    c.set_anchor()
                }
            });
        }

        let mut processed_lines = Vec::new();
        match key {
            key!(Tab) => handle.edit_all(pa, |mut c| {
                let char_col = c.v_caret().char_col();
                if self.indent_keys.contains(&'\t') && char_col == 0 {
                    reindent(&mut c, &mut processed_lines);
                    let indent = c.indent();
                    c.move_hor(indent as i32);
                    if c.indent() > 0 {
                        return;
                    }
                }

                if self.insert_tabs {
                    c.insert('\t');
                    c.move_hor(1);
                } else {
                    let tab_len = c.cfg().tab_stops.spaces_at(c.v_caret().visual_col() as u32);
                    c.insert(" ".repeat(tab_len as usize));
                    c.move_hor(tab_len as i32);
                }
            }),
            key!(Char(char)) => handle.edit_all(pa, |mut c| {
                c.insert(char);
                c.move_hor(1);
                if self.indent_keys.contains(&char) && c.indent() == c.v_caret().char_col() - 1 {
                    reindent(&mut c, &mut processed_lines);
                }
            }),
            key!(Enter) => handle.edit_all(pa, |mut c| {
                c.insert('\n');
                c.move_hor(1);
                if self.indent_keys.contains(&'\n') {
                    reindent(&mut c, &mut processed_lines);
                    c.move_hor(c.indent() as i32);
                }
            }),
            key!(Backspace) => handle.edit_all(pa, |mut c| {
                let prev_caret = c.caret();
                let prev_anchor = c.unset_anchor();
                c.move_hor(-1);
                c.replace("");
                if let Some(prev_anchor) = prev_anchor {
                    c.set_anchor();
                    if prev_anchor > prev_caret {
                        c.move_hor((prev_anchor.char() - prev_caret.char()) as i32);
                    } else {
                        c.move_to(prev_anchor);
                    }
                    c.swap_ends();
                }
            }),
            key!(Delete) => handle.edit_all(pa, |mut c| {
                let prev_caret = c.caret();
                let prev_anchor = c.unset_anchor();
                c.replace("");
                if let Some(prev_anchor) = prev_anchor {
                    c.set_anchor();
                    if prev_anchor > prev_caret {
                        c.move_hor((prev_anchor.char() - prev_caret.char()) as i32 - 1);
                    } else {
                        c.move_to(prev_anchor);
                    }
                    c.swap_ends();
                }
            }),
            key!(Left, Mod::NONE | Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut c);
                c.move_hor(-1);
            }),
            key!(Down, Mod::NONE | Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut c);
                if key.modifiers == Mod::NONE {
                    c.unset_anchor();
                    remove_empty_line(&mut c);
                }
                c.move_ver_wrapped(1);
            }),
            key!(Up, Mod::NONE | Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut c);
                if key.modifiers == Mod::NONE {
                    c.unset_anchor();
                    remove_empty_line(&mut c);
                }
                c.move_ver_wrapped(-1)
            }),
            key!(Right) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut c);
                c.move_hor(1);
            }),

            key!(Esc) => {
                handle.new_moment(pa);
                mode::set::<U>(Normal::new());
            }
            _ => {}
        }
    }

    fn on_switch(&mut self, _: &mut Pass, handle: Handle<Self::Widget, U>) {
        handle.set_mask("Insert");
    }
}

#[derive(Clone)]
enum OneKey {
    GoTo(SelType),
    Find(SelType, bool),
    Until(SelType, bool),
    Inside(Brackets),
    Around(Brackets),
    Replace,
}

impl<U: Ui> Mode<U> for OneKey {
    type Widget = File<U>;

    fn send_key(&mut self, pa: &mut Pass, key: Event, handle: Handle<Self::Widget, U>) {
        let sel_type = match *self {
            OneKey::GoTo(st) => match_goto::<(), U>(pa, &handle, key, st),
            OneKey::Find(st, ss) | OneKey::Until(st, ss) if let Some(char) = just_char(key) => {
                match_find_until(pa, handle, char, matches!(*self, OneKey::Until(..)), st);
                if ss {
                    *SEARCH.lock().unwrap() = char.to_string();
                }
                SelType::Normal
            }
            OneKey::Inside(brackets) | OneKey::Around(brackets) => {
                let is_inside = matches!(*self, OneKey::Inside(_));
                match_inside_around(pa, handle, key, brackets, is_inside);
                SelType::Normal
            }
            OneKey::Replace if let Some(char) = just_char(key) => {
                handle.edit_all(pa, |mut c| {
                    let len = c.selection().flat_map(str::chars).count();
                    c.replace(char.to_string().repeat(len));
                });
                SelType::Normal
            }
            _ => SelType::Normal,
        };

        mode::set::<U>(Normal::new_with_sel_type(sel_type));
    }

    fn on_switch(&mut self, _: &mut Pass, handle: Handle<Self::Widget, U>) {
        handle.set_mask("OneKey");
    }
}

fn match_goto<S, U: Ui>(
    pa: &mut Pass,
    handle: &Handle<File<U>, U, S>,
    key: Event,
    mut sel_type: SelType,
) -> SelType {
    static LAST_FILE: LazyLock<Mutex<Option<String>>> = LazyLock::new(Mutex::default);

    match key {
        key!(Char('h')) => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            let p1 = c.search_rev("\n", None).next().map(|[_, p1]| p1);
            c.move_to(p1.unwrap_or_default());
        }),
        key!(Char('j')) => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            c.move_ver(i32::MAX);
        }),
        key!(Char('k')) => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            c.move_to_coords(0, 0)
        }),
        key!(Char('l')) => handle.edit_all(pa, |c| {
            select_to_end_of_line(sel_type == SelType::Extend, c);
            sel_type = SelType::BeforeEndOfLine;
        }),
        key!(Char('i')) => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            let p1 = c.search_rev("(^|\n)[ \t]*", None).next().map(|[_, p1]| p1);
            if let Some(p1) = p1 {
                c.move_to(p1);

                let points = c.search_fwd("[^ \t]", None).next();
                if let Some([p0, _]) = points {
                    c.move_to(p0)
                }
            }
        }),

        ////////// File change keys
        key!(Char('a')) => {
            let cur_name = handle.read(pa, |file, _| file.name());
            let last_file = LAST_FILE.lock().unwrap().clone();
            if let Some(last_file) = last_file {
                cmd::queue_notify_and(format!("b {last_file}"), |res| {
                    if res.is_ok() {
                        *LAST_FILE.lock().unwrap() = Some(cur_name)
                    }
                })
            } else {
                context::error!("There is no previous file");
            }
        }
        key!(Char('n')) => {
            let cur_name = handle.read(pa, |file, _| file.name());
            cmd::queue_notify_and("next-file --global", |res| {
                if res.is_ok() {
                    *LAST_FILE.lock().unwrap() = Some(cur_name)
                }
            })
        }
        key!(Char('N')) => {
            let cur_name = handle.read(pa, |file, _| file.name());
            cmd::queue_notify_and("prev-file --global", |res| {
                if res.is_ok() {
                    *LAST_FILE.lock().unwrap() = Some(cur_name)
                }
            })
        }
        Event { code, .. } => {
            let code = format!("{code:?}");
            context::warn!("Key [a]{code}[] not mapped on [a]go to")
        }
    }

    sel_type
}

fn match_find_until<U: Ui>(
    pa: &mut Pass,
    handle: Handle<File<U>, U, ()>,
    char: char,
    is_t: bool,
    st: SelType,
) {
    use SelType::*;
    handle.edit_all(pa, |mut c| {
        let search = format!("\\x{{{:X}}}", char as u32);
        let cur = c.caret();
        let (points, back) = match st {
            Reverse | ExtendRev => (c.search_rev(search, None).find(|[p1, _]| *p1 != cur), 1),
            Normal | Extend => (c.search_fwd(search, None).find(|[p0, _]| *p0 != cur), -1),
            _ => unreachable!(),
        };

        if let Some([p0, _]) = points
            && p0 != c.caret()
        {
            let is_extension = !matches!(st, Extend | ExtendRev);
            if is_extension || c.anchor().is_none() {
                c.set_anchor();
            }
            c.move_to(p0);
            if is_t {
                c.move_hor(back);
            }
        } else {
            context::warn!("Char [a]{char}[] not found")
        }
    });
}

fn match_inside_around<U: Ui>(
    pa: &mut Pass,
    handle: Handle<File<U>, U, ()>,
    key: Event,
    brackets: Brackets,
    is_inside: bool,
) {
    fn move_to_points<S, U: Ui>(m: &mut Cursor<File<U>, U::Area, S>, [p0, p1]: [Point; 2]) {
        m.move_to(p0);
        m.set_anchor();
        m.move_to(p1);
    }

    let Char(char) = key.code else {
        context::warn!("Key [a]{key.code}[] not mapped on this mode");
        return;
    };

    let wc = handle.cfg(pa).word_chars;
    let initial_cursors_len = handle.read_selections(pa, Selections::len);

    let mut failed = false;

    if let Some(object) = Object::from_char(char, wc, brackets) {
        match char {
            'w' | 'W' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let start = object.find_behind(c, 0, None);
                let [_, p1] = object.find_ahead(c, 0, None)?;
                let p0 = {
                    let p0 = start.map(|[p0, _]| p0).unwrap_or(c.caret());
                    let p0_cat = Category::of(c.char_at(p0).unwrap(), wc);
                    let p1_cat = Category::of(c.char(), wc);
                    let is_same_cat = char == 'W' || p0_cat == p1_cat;
                    if is_same_cat { p0 } else { c.caret() }
                };
                move_to_points(c, [p0, p1]);
                c.move_hor(-1);
                Some(())
            }),
            's' | ' ' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let [_, p0] = object.find_behind(c, 0, None)?;
                let [p1, _] = object.find_ahead(c, 0, None)?;
                move_to_points(c, [p0, p1]);
                if is_inside || char == ' ' && p0 < c.text().len() {
                    c.move_hor(-1);
                }
                Some(())
            }),
            'p' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let end = object.find_ahead(c, 0, None);
                let [p1, _] = end?;
                c.move_to(p1);
                c.set_anchor();
                let [_, p0] = object.find_behind(c, 0, None).unwrap_or_default();
                c.move_to(p0);
                c.swap_ends();
                if is_inside {
                    c.move_hor(-1);
                }
                Some(())
            }),
            'u' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let [p2, _] = object.find_ahead(c, 1, None)?;
                c.move_to(p2);
                let [p0, p1] = object.find_behind(c, 1, None)?;
                if is_inside {
                    move_to_points(c, [p1, p2]);
                    c.move_hor(-1);
                } else {
                    move_to_points(c, [p0, p2]);
                    if !matches!(c.char_at(p2), Some(';' | ',')) {
                        c.move_hor(-1);
                    }
                    if !matches!(c.char_at(p0), Some(';' | ',')) {
                        c.swap_ends();
                        c.move_hor(1);
                        c.swap_ends();
                    }
                }

                Some(())
            }),
            _char => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let [p2, p3] = object.find_ahead(c, 1, None)?;
                let [p0, p1] = object.find_behind(c, 1, None)?;
                let [p0, p1] = if is_inside { [p1, p2] } else { [p0, p3] };
                move_to_points(c, [p0, p1]);
                c.move_hor(-1);
                Some(())
            }),
        }
    } else {
        match char {
            'i' => handle.edit_all(pa, |mut c| {
                let indent = c.indent();
                if indent == 0 {
                    let end = c.len();
                    move_to_points(&mut c, [Point::default(), end]);
                } else {
                    c.set_anchor();
                    c.move_hor(-(c.v_caret().char_col() as i32));

                    while c.indent() >= indent && c.caret().line() > 0 {
                        c.move_ver(-1);
                    }
                    c.move_ver(1);
                    c.swap_ends();

                    while c.indent() >= indent && c.caret().line() + 1 < c.text().len().line() {
                        c.move_ver(1);
                    }
                    c.move_ver(-1);

                    if is_inside {
                        let [_, p1] = c.text().points_of_line(c.caret().line());
                        c.move_to(p1);
                        c.move_hor(-1);
                    } else {
                        let p1 = c.search_fwd("\n+", None).next().map(|[_, p1]| p1).unwrap();
                        c.move_to(p1);
                    }
                }
            }),
            _ => context::warn!("Key [a]{key.code}[] not mapped on this mode"),
        }
    }

    if initial_cursors_len == 1 && failed {
        let rel = if is_inside { "inside" } else { "around" };
        context::warn!("Failed selecting {rel} object");
    }
}

fn edit_or_destroy_all<U: Ui, S>(
    pa: &mut Pass,
    handle: &Handle<File<U>, U, S>,
    failed_at_least_once: &mut bool,
    mut f: impl FnMut(&mut Cursor<File<U>, U::Area, S>) -> Option<()> + Clone,
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

fn no_nl_windows<'a>(
    iter: impl Iterator<Item = (Point, char)> + 'a,
) -> impl Iterator<Item = ((Point, char), (Point, char))> + 'a {
    iter.map_windows(|[first, second]| (*first, *second))
        .skip_while(|((_, c0), (_, c1))| *c0 == '\n' || *c1 == '\n')
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SelType {
    BeforeEndOfLine,
    ToEndOfLine,
    Reverse,
    Extend,
    ExtendRev,
    Normal,
}

fn word_and_space(alt_word: bool, wc: WordChars) -> String {
    if alt_word {
        "[^ \t\n]*[ \t]*".to_string()
    } else {
        let cat = w_char_cat(wc);
        format!("([{cat}]+|[^{cat} \t\n]+)[ \t]*|[ \t]+")
    }
}

fn space_and_word(alt_word: bool, wc: WordChars) -> String {
    if alt_word {
        "[ \t]*[^ \t\n]*".to_string()
    } else {
        let cat = w_char_cat(wc);
        format!("[ \t]*([{cat}]+|[^{cat} \t\n]+)|[ \t]+")
    }
}

fn w_char_cat(wc: WordChars) -> String {
    wc.ranges()
        .iter()
        .map(|r| {
            if r.start() == r.end() {
                format!("{}", r.start())
            } else {
                format!("{}-{}", r.start(), r.end())
            }
        })
        .collect()
}

fn select_to_end_of_line<S, U: Ui>(set_anchor: bool, mut c: Cursor<File<U>, U::Area, S>) {
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

#[derive(PartialEq, Eq)]
enum Category {
    Word,
    Special,
    Space,
}

impl Category {
    fn of(char: char, wc: WordChars) -> Self {
        if wc.contains(char) {
            Category::Word
        } else if [' ', '\t', '\n'].contains(&char) {
            Category::Space
        } else {
            Category::Special
        }
    }
}

#[derive(Clone, Copy)]
struct Select;

impl<U: Ui> IncSearcher<U> for Select {
    fn search(&mut self, pa: &mut Pass, handle: Handle<File<U>, U, Searcher>) {
        handle.edit_all(pa, |mut c| {
            c.set_caret_on_start();
            if let Some(anchor) = c.anchor() {
                let ranges: Vec<[Point; 2]> = c.search_inc_fwd(Some(anchor)).collect();

                for (i, &[p0, p1]) in ranges.iter().enumerate() {
                    c.move_to(p0);
                    if p1.char() > p0.char() + 1 {
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                    } else {
                        c.unset_anchor();
                    }
                    if i < ranges.len() - 1 {
                        c.copy();
                    }
                }
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]select").build()
    }
}

#[derive(Clone, Copy)]
struct Split;

impl<U: Ui> IncSearcher<U> for Split {
    fn search(&mut self, pa: &mut Pass, handle: Handle<File<U>, U, Searcher>) {
        handle.edit_all(pa, |mut c| {
            c.set_caret_on_start();
            if let Some(anchor) = c.anchor() {
                let ranges: Vec<Point> = c.search_inc_fwd(Some(anchor)).flatten().collect();
                let cursors_to_add = ranges.len() / 2 + 1;
                let iter = [c.caret()]
                    .into_iter()
                    .chain(ranges)
                    .chain([anchor])
                    .array_chunks();

                for (i, [p0, p1]) in iter.enumerate() {
                    c.move_to(p0);
                    if p1.char() > p0.char() + 1 {
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                    } else if p1 > p0 {
                        c.unset_anchor();
                    } else {
                        continue;
                    }
                    if i < cursors_to_add {
                        c.copy();
                    }
                }
            }
        })
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]split").build()
    }
}

/// Sets the indentation for every cursor
fn reindent<S, U: Ui>(c: &mut Cursor<File<U>, U::Area, S>, processed_lines: &mut Vec<usize>) {
    let prev_caret = c.caret();
    let prev_anchor = c.unset_anchor();
    if processed_lines.contains(&prev_caret.line()) {
        return;
    }
    c.move_hor(-(c.v_caret().char_col() as i32));
    c.set_anchor();
    c.move_hor(c.indent() as i32);

    let indent = if let Some(indent) = c.ts_indent() {
        indent
    } else {
        let prev_non_empty = prev_non_empty_line_points(c);
        prev_non_empty.map(|[p0, _]| c.indent_on(p0)).unwrap_or(0)
    };

    if c.caret() == c.anchor().unwrap() {
        c.insert(" ".repeat(indent));
    } else {
        c.move_hor(-1);
        c.replace(" ".repeat(indent));
    }
    c.unset_anchor();

    if let Some(prev_anchor) = prev_anchor {
        c.set_anchor();
        if prev_anchor > prev_caret {
            c.move_hor((prev_anchor.char() - prev_caret.char()) as i32);
        } else {
            c.move_to(prev_anchor);
        }
        c.swap_ends();
    }

    if prev_caret < c.caret() {
        c.move_to(prev_caret);
    } else {
        c.move_hor(prev_caret.char() as i32 - c.caret().char() as i32);
    }

    processed_lines.push(c.caret().line());
}

/// removes an empty line
fn remove_empty_line<S, U: Ui>(c: &mut Cursor<File<U>, U::Area, S>) {
    let mut lines = c.lines_on(c.caret()..);
    let (_, line) = lines.next().unwrap();
    if !line.chars().all(char::is_whitespace) || line.len() == 1 {
        return;
    }
    let chars_count = line.chars().count();

    let dvcol = c.v_caret().desired_visual_col();
    c.move_hor(-(c.v_caret().char_col() as i32));
    c.set_anchor();
    c.move_hor(chars_count as i32 - 1);

    c.replace("");
    c.unset_anchor();
    c.set_desired_vcol(dvcol);
}

fn copy_selections<U: Ui>(pa: &mut Pass, handle: &Handle<File<U>, U, ()>) {
    let mut copies: Vec<String> = Vec::new();
    handle.edit_all(pa, |c| copies.push(c.selection().collect()));
    if !copies.iter().all(String::is_empty) {
        if copies.len() == 1 {
            duat_core::clipboard::set_text(copies.first().unwrap());
        }
        *CLIPBOARD.lock().unwrap() = copies
    }
}

fn paste_strings() -> Vec<String> {
    static SYSTEM_CLIPB: Mutex<Option<String>> = Mutex::new(None);

    let paste = duat_core::clipboard::get_text();

    let mut sys_clipb = SYSTEM_CLIPB.lock().unwrap();

    // If there was no previous clipboard, or it has changed, copy the new
    // pasted text
    if let Some(paste) = paste
        && sys_clipb.as_ref().is_none_or(|sc| *sc != paste)
    {
        *CLIPBOARD.lock().unwrap() = vec![paste.clone()];
        *sys_clipb = Some(paste.clone());
        vec![paste]
    } else {
        CLIPBOARD.lock().unwrap().clone()
    }
}

fn set_anchor_if_needed<S, U: Ui>(set_anchor: bool, c: &mut Cursor<File<U>, U::Area, S>) {
    if set_anchor {
        if c.anchor().is_none() {
            c.set_anchor();
        }
    } else {
        c.unset_anchor();
    }
}

fn just_char(key: Event) -> Option<char> {
    if let key!(Char(char)) = key {
        Some(char)
    } else {
        None
    }
}

fn prev_non_empty_line_points<S, U: Ui>(c: &mut Cursor<File<U>, U::Area, S>) -> Option<[Point; 2]> {
    let byte_col = c
        .text()
        .buffers(..c.caret().byte())
        .take_while(|b| *b != b'\n')
        .count();
    let mut lines = c.lines_on(..c.caret().byte() - byte_col);
    let prev =
        lines.find_map(|(n, l): (usize, &str)| l.chars().any(|c| !c.is_whitespace()).then_some(n));
    prev.map(|n| c.text().points_of_line(n))
}

static INSERT_TABS: AtomicBool = AtomicBool::new(false);

#[derive(Clone, Copy, PartialEq, Eq)]
enum Object<'a> {
    Anchored(&'a str),
    Bounds(&'a str, &'a str),
    Argument(&'a str, &'a str, &'a str),
    Bound(&'a str),
}

impl<'a> Object<'a> {
    fn from_char(char: char, wc: WordChars, brackets: Brackets) -> Option<Self> {
        static BRACKET_PATS: Memoized<Brackets, [&'static str; 3]> = Memoized::new();
        match char {
            'Q' => Some(Self::Bound("\"")),
            'q' => Some(Self::Bound("'")),
            'g' => Some(Self::Bound("`")),
            '|' => Some(Self::Bound(r"\|")),
            '$' => Some(Self::Bound(r"\$")),
            '^' => Some(Self::Bound(r"\^")),
            's' => Some(Self::Bound(r"[\.;!\?]")),
            'p' => Some(Self::Bound("^\n")),
            'b' | '(' | ')' => Some(Self::Bounds(r"\(", r"\)")),
            'B' | '{' | '}' => Some(Self::Bounds(r"\{", r"\}")),
            'r' | '[' | ']' => Some(Self::Bounds(r"\[", r"\]")),
            'a' | '<' | '>' => Some(Self::Bounds("<", ">")),
            'm' | 'u' => Some({
                let [m_b, s_b, e_b] = BRACKET_PATS.get_or_insert_with(brackets, || {
                    let (s_pat, e_pat): (String, String) = brackets
                        .iter()
                        .map(|[s_b, e_b]| (*s_b, *e_b))
                        .intersperse(("|", "|"))
                        .collect();
                    [r"(;|,)\s*", s_pat.leak(), e_pat.leak()]
                });

                if char == 'm' {
                    Self::Bounds(s_b, e_b)
                } else {
                    Self::Argument(m_b, s_b, e_b)
                }
            }),
            'w' => Some(Self::Anchored({
                static WORD_PATS: Memoized<WordChars, &str> = Memoized::new();
                WORD_PATS.get_or_insert_with(wc, || {
                    let cat = w_char_cat(wc);
                    format!("\\A([{cat}]+|[^{cat} \t\n]+)\\z").leak()
                })
            })),
            'W' => Some(Self::Anchored("\\A[^ \t\n]+\\z")),
            ' ' => Some(Self::Anchored(r"\A\s*\z")),
            char if !char.is_alphanumeric() => Some(Self::Bound({
                static BOUNDS: Memoized<char, &str> = Memoized::new();
                BOUNDS.get_or_insert_with(char, || char.to_string().leak())
            })),
            _ => None,
        }
    }

    fn find_ahead<S, U: Ui>(
        self,
        c: &mut Cursor<File<U>, U::Area, S>,
        s_count: usize,
        until: Option<Point>,
    ) -> Option<[Point; 2]> {
        let mut s_count = s_count as i32;
        match self {
            Object::Anchored(pat) => {
                let pat = pat.strip_suffix(r"\z").unwrap();
                c.search_fwd(pat, until).next()
            }
            Object::Bounds(s_b, e_b) => {
                let (_, [p0, p1]) = c.search_fwd([s_b, e_b], None).find(|&(id, _)| {
                    s_count += (id == 0) as i32 - (id == 1) as i32;
                    s_count <= 0
                })?;
                Some([p0, p1])
            }
            Object::Bound(b) => c.search_fwd(b, until).next(),
            Object::Argument(m_b, s_b, e_b) => {
                let caret = c.caret();
                let (_, [p0, p1]) = c.search_fwd([m_b, s_b, e_b], None).find(|&(id, [p, _])| {
                    s_count += (id == 1) as i32 - (id == 2 && p != caret) as i32;
                    s_count == 0 || (s_count == 1 && id == 0)
                })?;
                Some([p0, p1])
            }
        }
    }

    fn find_behind<S, U: Ui>(
        self,
        c: &mut Cursor<File<U>, U::Area, S>,
        c_count: usize,
        until: Option<Point>,
    ) -> Option<[Point; 2]> {
        let mut c_count = c_count as i32;
        match self {
            Object::Anchored(pat) => {
                let pat = pat.strip_prefix(r"\A").unwrap();
                c.search_rev(pat, until).next()
            }
            Object::Bounds(s_b, e_b) => {
                let (_, [p0, p1]) = c.search_rev([s_b, e_b], None).find(|&(id, _)| {
                    c_count += (id == 1) as i32 - (id == 0) as i32;
                    c_count <= 0
                })?;
                Some([p0, p1])
            }
            Object::Bound(b) => c.search_rev(b, until).next(),
            Object::Argument(m_b, s_b, e_b) => {
                let (_, [p0, p1]) = c.search_rev([m_b, s_b, e_b], None).find(|&(id, _)| {
                    c_count += (id == 2) as i32 - (id == 1) as i32;
                    c_count == 0 || (c_count == 1 && id == 0)
                })?;
                Some([p0, p1])
            }
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct Brackets(&'static [[&'static str; 2]]);

impl Brackets {
    fn bounds_matching(&self, bound: &str) -> Option<[&'static str; 2]> {
        self.0
            .iter()
            .find(|bs| bs.contains(&escaped_regex(bound)))
            .copied()
    }

    fn iter(&self) -> impl Iterator<Item = &[&'static str; 2]> + '_ {
        self.0.iter()
    }
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

static CLIPBOARD: Mutex<Vec<String>> = Mutex::new(Vec::new());
static SEARCH: Mutex<String> = Mutex::new(String::new());
