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
//! specific varieties, e.g. `MainCursorInsert`.
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
//! - Special keys are enclosed in `<` `>` pairs (e.g. `<Home>`).
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
//! Selects the character below on the next wrapped line (i.e vim's
//! `gj`).
//!
//! `k`\
//! Selects the character above on the previous line.
//!
//! `<Up>`\
//! Selects the character above on the previous wrapped line (i.e.
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
//! `e`\
//! Selects to the end of the next `word` to the right of the
//! selection.
//!
//! `<A-(w|b|e)>`\
//! The same as `(w|b|e)`, but over a `WORD`.
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
//!
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
        LazyLock,
        atomic::{AtomicBool, Ordering},
    },
};

use duat_core::{
    Lender, Mutex, Plugin,
    cfg::WordChars,
    cmd, context, form,
    hooks::{self, ModeSwitched, SearchPerformed},
    mode::{
        self, Cursors, EditHelper, Editor, KeyCode::*, KeyEvent as Event, KeyMod as Mod, Mode, key,
    },
    text::{Point, Searcher, err, text},
    ui::{RawArea, Ui},
    widgets::File,
};
use duat_utils::modes::{
    ExtendFwd, ExtendRev, IncSearch, IncSearcher, PipeSelections, RunCommands, SearchFwd, SearchRev,
};
use treesitter::TsParser;

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
        duat_core::mode::set_default::<Normal, U>(Normal::new());
        INSERT_TABS.store(self.insert_tabs, Ordering::Relaxed);

        hooks::add::<SearchPerformed>(|search| {
            *SEARCH.lock() = search.to_string();
        });

        if self.set_cursor_forms {
            static MODES: &[&str] = &["Insert", "Normal", "GoTo"];
            form::ids_of_non_static(MODES.iter().flat_map(|mode| {
                [
                    format!("MainCursor.{mode}"),
                    format!("ExtraCursor.{mode}"),
                    format!("MainSelection.{mode}"),
                    format!("ExtraSelection.{mode}"),
                ]
            }));

            hooks::add::<ModeSwitched>(|(_, new)| {
                if !MODES.contains(&new) {
                    return;
                }
                form::set("MainCursor", format!("MainCursor.{new}"));
                form::set("ExtraCursor", format!("ExtraCursor.{new}"));
                form::set("MainSelection", format!("MainSelection.{new}"));
                form::set("ExtraSelection", format!("ExtraSelection.{new}"));
            });
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
    type Widget = File;

    fn send_key(&mut self, key: Event, widget: &mut Self::Widget, area: &U::Area) {
        let mut helper = EditHelper::new(widget, area);
        let wc = helper.cfg().word_chars;

        match key {
            ////////// Basic movement keys
            key!(Char('h' | 'H') | Left, Mod::NONE | Mod::SHIFT) => {
                helper.edit_iter().for_each(|mut e| {
                    let set_anchor = key.code == Char('H') || key.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut e);
                    e.move_hor(-1);
                });
                self.sel_type = SelType::Normal;
            }
            key!(Down) => helper.edit_iter().for_each(|mut e| {
                set_anchor_if_needed(key.modifiers.contains(Mod::SHIFT), &mut e);
                e.move_ver_wrapped(1);
            }),
            key!(Up) => helper.edit_iter().for_each(|mut e| {
                set_anchor_if_needed(key.modifiers.contains(Mod::SHIFT), &mut e);
                e.move_ver_wrapped(-1);
            }),
            key!(Char('l' | 'L') | Right, Mod::NONE | Mod::SHIFT) => {
                helper.edit_iter().for_each(|mut e| {
                    let set_anchor = key.code == Char('L') || key.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut e);
                    e.move_hor(1);
                });
                self.sel_type = SelType::Normal;
            }
            key!(Char('j' | 'J')) => helper.edit_iter().for_each(|mut e| {
                set_anchor_if_needed(key.code == Char('J'), &mut e);
                e.move_ver(1);
                let v_caret = e.v_caret();
                if e.char() == '\n'
                    && v_caret.char_col() > 0
                    && self.sel_type != SelType::ToEndOfLine
                {
                    e.move_hor(-1);
                    e.set_desired_vcol(if self.sel_type == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        v_caret.desired_visual_col()
                    });
                }
            }),
            key!(Char('k' | 'K')) => helper.edit_iter().for_each(|mut e| {
                set_anchor_if_needed(key.code == Char('K'), &mut e);
                e.move_ver(-1);
                let v_caret = e.v_caret();
                if e.char() == '\n'
                    && v_caret.char_col() > 0
                    && self.sel_type != SelType::ToEndOfLine
                {
                    e.move_hor(-1);
                    e.set_desired_vcol(if self.sel_type == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        v_caret.desired_visual_col()
                    });
                }
            }),

            ////////// Object selection keys
            key!(Char('w'), Mod::NONE | Mod::ALT) => helper.edit_iter().for_each(|mut e| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = no_nl_windows(e.chars_fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, wc) == Category::of(c1, wc) {
                        e.move_to(p0);
                    } else {
                        e.move_to(p1);
                    }

                    let points = e.search_fwd(word_and_space(alt_word, wc), None).next();
                    if let Some([_, p1]) = points {
                        e.set_anchor();
                        e.move_to(p1);
                        e.move_hor(-1);
                    }
                };
            }),
            key!(Char('e'), Mod::NONE | Mod::ALT) => helper.edit_iter().for_each(|mut e| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = no_nl_windows(e.chars_fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, wc) == Category::of(c1, wc) {
                        e.move_to(p0);
                    } else {
                        e.move_to(p1);
                    }

                    let points = e.search_fwd(space_and_word(alt_word, wc), None).next();
                    if let Some([_, p1]) = points {
                        e.set_anchor();
                        e.move_to(p1);
                        e.move_hor(-1);
                    }
                };
            }),
            key!(Char('b'), Mod::NONE | Mod::ALT) => helper.edit_iter().for_each(|mut e| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = {
                    let iter = [(e.caret(), e.char())].into_iter().chain(e.chars_rev());
                    no_nl_windows(iter).next()
                };
                if let Some(((p1, c1), (_, c0))) = init {
                    e.move_to(p1);
                    if Category::of(c0, wc) == Category::of(c1, wc) {
                        e.move_hor(1);
                    }
                    let points = e.search_rev(word_and_space(alt_word, wc), None).next();
                    if let Some([p0, p1]) = points {
                        e.move_to(p0);
                        e.set_anchor();
                        e.move_to(p1);
                        e.move_hor(-1);
                        e.swap_ends();
                    };
                };
            }),

            key!(Char('W'), Mod::ALT | Mod::NONE) => helper.edit_iter().for_each(|mut e| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut e);
                e.move_hor(1);
                let points = e.search_fwd(word_and_space(alt_word, wc), None).next();
                if let Some([_, p1]) = points {
                    e.move_to(p1);
                    e.move_hor(-1);
                }
            }),
            key!(Char('E'), Mod::NONE | Mod::ALT) => helper.edit_iter().for_each(|mut e| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut e);
                e.move_hor(1);
                let points = e.search_fwd(space_and_word(alt_word, wc), None).next();
                if let Some([_, p1]) = points {
                    e.move_to(p1);
                    e.move_hor(-1);
                }
            }),
            key!(Char('B'), Mod::NONE | Mod::ALT) => helper.edit_iter().for_each(|mut e| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut e);
                let points = e.search_rev(word_and_space(alt_word, wc), None).next();
                if let Some([p0, _]) = points {
                    e.move_to(p0);
                }
            }),

            key!(Char('x')) => helper.edit_iter().for_each(|mut e| {
                self.sel_type = SelType::ToEndOfLine;
                set_anchor_if_needed(true, &mut e);
                e.set_caret_on_start();
                let p0 = e.search_rev("\n", None).next().map(|[_, p0]| p0);
                e.move_to(p0.unwrap_or_default());
                e.swap_ends();

                let p1 = e.search_fwd("\n", None).next().map(|[p1, _]| p1);
                if let Some(p1) = p1.or(e.last_point()) {
                    e.move_to(p1);
                }
                e.set_desired_vcol(usize::MAX);
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
            key!(Char('l' | 'L'), Mod::ALT) | key!(End) => helper.edit_iter().for_each(|mut e| {
                if key.code == Char('l') {
                    e.unset_anchor();
                }
                select_to_end_of_line(true, e);
                self.sel_type = SelType::BeforeEndOfLine;
            }),
            key!(Char('h' | 'H'), Mod::ALT) | key!(Home) => helper.edit_iter().for_each(|mut e| {
                if key.code == Char('h') {
                    e.unset_anchor();
                }
                set_anchor_if_needed(true, &mut e);
                e.move_hor(-(e.v_caret().char_col() as i32));
            }),
            key!(Char('a'), Mod::ALT) => mode::set::<U>(OneKey::Around(self.brackets)),
            key!(Char('i'), Mod::ALT) => mode::set::<U>(OneKey::Inside(self.brackets)),
            key!(Char('%')) => {
                let mut e = helper.edit_main();
                e.move_to(Point::default());
                e.set_anchor();
                e.move_to(e.last_point().unwrap())
            }
            key!(Char('m' | 'M')) => {
                let mut failed = false;
                let failed = &mut failed;
                helper.edit_iter().for_each(do_or_destroy(failed, |e| {
                    let object = Object::from_char('m', e.cfg().word_chars, self.brackets).unwrap();
                    let [p2, p3] = object.find_ahead(e, 0, None)?;
                    let prev_caret = e.caret();
                    set_anchor_if_needed(key.code == Char('M'), e);
                    e.move_to(p3);
                    e.move_hor(-1);

                    let [s_b, e_b] = self.brackets.bounds_matching(e.contiguous_in((p2, p3)))?;
                    let [p0, _] = Object::Bounds(s_b, e_b).find_behind(e, 1, None)?;
                    if key.code == Char('m') {
                        e.set_anchor();
                    }
                    e.move_to(p0);
                    if prev_caret.char() != p3.char() - 1 {
                        if key.code == Char('m') {
                            e.set_anchor();
                        }
                        e.move_to(p3);
                        e.move_hor(-1);
                    }

                    Some(())
                }))
            }
            key!(Char('m' | 'M'), Mod::ALT) => {
                let mut failed = false;
                let failed = &mut failed;
                helper.edit_iter().for_each(do_or_destroy(failed, |e| {
                    let object = Object::from_char('m', e.cfg().word_chars, self.brackets).unwrap();
                    let [p0, p1] = object.find_behind(e, 0, None)?;
                    let prev_caret = e.caret();
                    set_anchor_if_needed(key.code == Char('M'), e);
                    e.move_to(p0);

                    let [s_b, e_b] = self.brackets.bounds_matching(e.contiguous_in((p0, p1)))?;
                    let [_, p3] = Object::Bounds(s_b, e_b).find_ahead(e, 1, None)?;
                    if key.code == Char('m') {
                        e.set_anchor();
                    }
                    e.move_to(p3);
                    e.move_hor(-1);
                    if prev_caret != p0 {
                        if key.code == Char('m') {
                            e.set_anchor();
                        }
                        e.move_to(p0);
                    }

                    Some(())
                }))
            }

            ////////// Insertion mode keys
            key!(Char('i')) => {
                helper.new_moment();
                helper.edit_iter().for_each(|mut e| {
                    e.set_caret_on_start();
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('I')) => {
                helper.new_moment();
                let mut processed_lines = Vec::new();
                helper.edit_iter().for_each(|mut e| {
                    if self.indent_on_capital_i {
                        reindent(&mut e, &mut processed_lines);
                    } else {
                        e.unset_anchor();
                        e.move_hor(-(e.v_caret().char_col() as i32));
                        e.set_anchor();
                        let indent = e.indent();
                        e.move_hor(indent as i32);
                    }
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('a')) => {
                helper.new_moment();
                helper.edit_iter().for_each(|mut e| {
                    e.set_caret_on_end();
                    e.move_hor(1);
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('A')) => {
                helper.new_moment();
                helper.edit_iter().for_each(|mut e| {
                    e.unset_anchor();
                    let (p, _) = e.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                    e.move_to(p);
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('o' | 'O'), Mod::NONE | Mod::ALT) => {
                helper.new_moment();
                let mut processed_lines = Vec::new();
                helper.edit_iter().for_each(|mut e| {
                    if key.code == Char('O') {
                        e.set_caret_on_start();
                        let char_col = e.v_caret().char_col();
                        e.move_hor(-(char_col as i32));
                        e.insert("\n");
                        if key.modifiers == Mod::NONE {
                            reindent(&mut e, &mut processed_lines);
                            e.move_hor(e.indent() as i32);
                        } else {
                            e.move_hor(char_col as i32 + 1);
                        }
                    } else {
                        e.set_caret_on_end();
                        let caret = e.caret();
                        let (p, _) = e.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                        e.move_to(p);
                        e.append("\n");
                        if key.modifiers == Mod::NONE {
                            e.move_hor(1);
                            reindent(&mut e, &mut processed_lines);
                            e.move_hor(e.indent() as i32);
                        } else {
                            e.move_to(caret);
                        }
                    }
                });
                if key.modifiers == Mod::NONE {
                    mode::set::<U>(Insert::new());
                }
            }

            ////////// Selection alteration keys
            key!(Char('r')) => {
                helper.new_moment();
                mode::set::<U>(OneKey::Replace)
            }
            key!(Char('`')) => {
                helper.new_moment();
                helper.edit_iter().for_each(|mut e| {
                    let lower = e
                        .selection()
                        .flat_map(str::chars)
                        .flat_map(char::to_lowercase);
                    e.replace(lower.collect::<String>());
                })
            }
            key!(Char('~')) => {
                helper.new_moment();
                helper.edit_iter().for_each(|mut e| {
                    let upper = e
                        .selection()
                        .flat_map(str::chars)
                        .flat_map(char::to_uppercase);
                    e.replace(upper.collect::<String>());
                })
            }
            key!(Char('`'), Mod::ALT) => {
                helper.new_moment();
                helper.edit_iter().for_each(|mut e| {
                    let inverted = e.selection().flat_map(str::chars).map(|c| {
                        if c.is_uppercase() {
                            c.to_lowercase().collect::<String>()
                        } else {
                            c.to_uppercase().collect()
                        }
                    });
                    e.replace(inverted.collect::<String>());
                })
            }

            ////////// Advanced selection manipulation
            key!(Char(';'), Mod::ALT) => helper.edit_iter().for_each(|mut e| e.swap_ends()),
            key!(Char(';')) => helper.edit_iter().for_each(|mut e| {
                e.unset_anchor();
            }),
            key!(Char(':'), Mod::ALT) => helper.edit_iter().for_each(|mut e| {
                e.set_caret_on_end();
            }),
            key!(Char(')')) => helper.cursors_mut().rotate_main(1),
            key!(Char('(')) => helper.cursors_mut().rotate_main(-1),
            key!(Char(')'), Mod::ALT) => {
                helper.new_moment();
                let mut iter = helper.edit_iter();
                let mut last_sel = iter.next().map(|e| e.selection().to_string());

                while let Some(mut e) = iter.next() {
                    let selection = e.selection().to_string();
                    e.replace(last_sel.replace(selection).unwrap());
                }

                helper.edit_nth(0).replace(last_sel.unwrap());
            }
            key!(Char('('), Mod::ALT) => {
                helper.new_moment();
                let mut selections = Vec::<String>::new();
                helper
                    .edit_iter()
                    .for_each(|e| selections.push(e.selection().collect()));
                let mut s_iter = selections.into_iter().cycle();
                s_iter.next();
                helper.edit_iter().for_each(|mut e| {
                    if let Some(next) = s_iter.next() {
                        e.replace(next);
                    }
                });
            }
            key!(Char('_'), Mod::ALT) => {
                helper.edit_iter().for_each(|mut e| {
                    e.set_caret_on_end();
                    e.move_hor(1);
                });
                // In the first iteration, connected Cursors are joined, this one just
                // undoes the movement.
                helper.edit_iter().for_each(|mut e| {
                    e.move_hor(-1);
                });
            }
            key!(Char('s'), Mod::ALT) => helper.edit_iter().for_each(|mut e| {
                e.set_caret_on_start();
                let Some(end) = e.anchor() else {
                    return;
                };
                let lines: Vec<[Point; 2]> = e.search_fwd("[^\n]*\n", Some(end)).collect();
                let mut last_p1 = e.caret();
                for [p0, p1] in lines {
                    let mut e_copy = e.copy();
                    e_copy.move_to(p0);
                    e_copy.set_anchor();
                    e_copy.move_to(p1);
                    e_copy.move_hor(-1);

                    last_p1 = p1;
                }
                e.move_to(last_p1);
                e.swap_ends();
            }),
            key!(Char('S'), Mod::ALT) => helper.edit_iter().for_each(|mut e| {
                if e.anchor().is_some() {
                    let mut e_copy = e.copy();
                    e_copy.swap_ends();
                    e_copy.unset_anchor();
                    e.unset_anchor();
                }
            }),

            ////////// Clipboard keys
            key!(Char('y')) => {
                helper.new_moment();
                copy_selections(&mut helper)
            }
            key!(Char('d'), Mod::NONE | Mod::ALT) => {
                helper.new_moment();
                if key.modifiers == Mod::NONE {
                    copy_selections(&mut helper);
                }
                helper.edit_iter().for_each(|mut e| {
                    e.replace("");
                    e.unset_anchor();
                });
            }
            key!(Char('c'), Mod::NONE | Mod::ALT) => {
                helper.new_moment();
                if key.modifiers == Mod::NONE {
                    copy_selections(&mut helper);
                }
                helper.edit_iter().for_each(|mut e| {
                    e.replace("");
                    e.unset_anchor();
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('p' | 'P')) => {
                let pastes = paste_strings();
                if !pastes.is_empty() {
                    helper.new_moment();
                    let mut p_iter = pastes.iter().cycle();
                    helper.edit_iter().for_each(|mut e| {
                        let paste = p_iter.next().unwrap();
                        // If it ends in a new line, we gotta move to the start of the line.
                        if key.code == Char('p') {
                            e.set_caret_on_end();
                            if paste.ends_with('\n') {
                                let caret = e.caret();
                                let (p, _) =
                                    e.chars_fwd().find(|(_, c)| *c == '\n').unwrap_or_default();
                                e.move_to(p);
                                e.append(paste);
                                e.move_to(caret);
                            } else {
                                e.move_hor(-(e.v_caret().char_col() as i32));
                            }
                        } else if key.code == Char('P') {
                            e.set_caret_on_start();
                            if paste.ends_with('\n') {
                                let char_col = e.v_caret().char_col();
                                e.move_hor(-(char_col as i32));
                                e.insert(paste);
                                e.move_hor(char_col as i32 + 1);
                            } else {
                                e.insert(paste)
                            }
                        }
                    });
                }
            }
            key!(Char('R')) => {
                let pastes = paste_strings();
                if !pastes.is_empty() {
                    helper.new_moment();
                    let mut p_iter = pastes.iter().cycle();
                    helper
                        .edit_iter()
                        .for_each(|mut e| e.replace(p_iter.next().unwrap()));
                }
            }

            ////////// Cursor creation and destruction
            key!(Char(',')) => helper.cursors_mut().remove_extras(),
            key!(Char('C')) => {
                helper.new_moment();
                let mut e = helper.edit_last();
                let v_caret = e.v_caret();
                e.copy();
                if let Some(v_anchor) = e.v_anchor() {
                    let lines_diff = v_anchor.line() as i32 - e.caret().line() as i32;
                    let len_lines = lines_diff.unsigned_abs() as usize;
                    while e.caret().line() + len_lines < e.len().line() {
                        e.move_ver(len_lines as i32 + 1);
                        e.set_anchor();
                        e.set_desired_vcol(v_anchor.visual_col());
                        e.move_ver(lines_diff);
                        e.swap_ends();
                        if e.v_caret().visual_col() <= v_caret.visual_col()
                            && e.v_anchor().unwrap().visual_col() <= v_anchor.visual_col()
                        {
                            return;
                        }
                        e.swap_ends();
                    }
                } else {
                    while e.caret().line() < e.len().line() {
                        if e.move_ver(1) == 0 {
                            break;
                        }
                        if e.v_caret().visual_col() == v_caret.visual_col() {
                            return;
                        }
                    }
                }
                e.destroy();
            }
            key!(Char('C'), Mod::ALT) => {
                helper.new_moment();
                let mut e = helper.edit_nth(0);
                let v_caret = e.v_caret();
                e.copy();
                if let Some(v_anchor) = e.v_anchor() {
                    let lines_diff = v_anchor.line() as i32 - e.caret().line() as i32;
                    let len_lines = lines_diff.unsigned_abs() as usize;
                    while e.caret().line().checked_sub(len_lines + 1).is_some() {
                        e.move_ver(-1 - len_lines as i32);
                        e.set_anchor();
                        e.set_desired_vcol(v_anchor.visual_col());
                        e.move_ver(lines_diff);
                        e.swap_ends();
                        if e.v_caret().visual_col() == v_caret.visual_col()
                            && e.v_anchor().unwrap().visual_col() == v_anchor.visual_col()
                        {
                            return;
                        }
                        e.swap_ends();
                    }
                } else {
                    while e.caret().line() > 0 {
                        e.move_ver(-1);
                        if e.v_caret().visual_col() == v_caret.visual_col() {
                            return;
                        }
                    }
                }
                e.destroy();
            }

            ////////// Other mode changing keys
            key!(Char(':')) => mode::set::<U>(RunCommands::new()),
            key!(Char('|')) => {
                helper.new_moment();
                mode::set::<U>(PipeSelections::new())
            }
            key!(Char('G')) => mode::set::<U>(OneKey::GoTo(SelType::Extend)),
            key!(Char('g')) => mode::set::<U>(OneKey::GoTo(SelType::Normal)),

            ////////// Search methods
            key!(Char('/')) => mode::set::<U>(IncSearch::new(SearchFwd)),
            key!(Char('/'), Mod::ALT) => mode::set::<U>(IncSearch::new(SearchRev)),
            key!(Char('?')) => mode::set::<U>(IncSearch::new(ExtendFwd)),
            key!(Char('?'), Mod::ALT) => mode::set::<U>(IncSearch::new(ExtendRev)),
            key!(Char('s')) => mode::set::<U>(IncSearch::new(Select)),
            key!(Char('S')) => mode::set::<U>(IncSearch::new(Split)),
            key!(Char('n' | 'N'), Mod::NONE | Mod::ALT) => {
                let search = SEARCH.lock();
                if search.is_empty() {
                    context::notify(err!("No search pattern set"));
                    return;
                }
                let mut e = helper.edit_main();

                if key.code == Char('N') {
                    e.copy();
                }
                let caret = e.caret();
                let next = if key.modifiers == Mod::ALT {
                    e.search_rev(&*search, None).find(|[p, _]| *p != caret)
                } else {
                    e.search_fwd(&*search, None).find(|[p, _]| *p != caret)
                };
                if let Some([p0, p1]) = next {
                    e.move_to(p0);
                    if p1 > p0 {
                        e.set_anchor();
                        e.move_to(p1);
                        e.move_hor(-1);
                    }
                }
            }

            ////////// History manipulation
            key!(Char('u')) => helper.undo(),
            key!(Char('U')) => helper.redo(),
            _ => {}
        }
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
    type Widget = File;

    fn send_key(&mut self, key: Event, widget: &mut Self::Widget, area: &U::Area) {
        let mut helper = EditHelper::new(widget, area);

        if let key!(Left | Down | Up | Right, mods) = key
            && mods.contains(Mod::SHIFT)
        {
            helper.edit_iter().for_each(|mut e| {
                if e.anchor().is_none() {
                    e.set_anchor()
                }
            });
        }

        let mut processed_lines = Vec::new();
        match key {
            key!(Tab) => helper.edit_iter().for_each(|mut e| {
                let char_col = e.v_caret().char_col();
                if self.indent_keys.contains(&'\t') && char_col == 0 {
                    reindent(&mut e, &mut processed_lines);
                    let indent = e.indent();
                    e.move_hor(indent as i32);
                    if e.indent() > 0 {
                        return;
                    }
                }

                if self.insert_tabs {
                    e.insert('\t');
                    e.move_hor(1);
                } else {
                    let tab_len = e.cfg().tab_stops.spaces_at(e.v_caret().visual_col() as u32);
                    e.insert(" ".repeat(tab_len as usize));
                    e.move_hor(tab_len as i32);
                }
            }),
            key!(Char(char)) => helper.edit_iter().for_each(|mut e| {
                e.insert(char);
                e.move_hor(1);
                if self.indent_keys.contains(&char) && e.indent() == e.v_caret().char_col() - 1 {
                    reindent(&mut e, &mut processed_lines);
                }
            }),
            key!(Enter) => helper.edit_iter().for_each(|mut e| {
                e.insert('\n');
                e.move_hor(1);
                if self.indent_keys.contains(&'\n') {
                    reindent(&mut e, &mut processed_lines);
                    e.move_hor(e.indent() as i32);
                }
            }),
            key!(Backspace) => helper.edit_iter().for_each(|mut e| {
                let prev_caret = e.caret();
                let prev_anchor = e.unset_anchor();
                e.move_hor(-1);
                e.replace("");
                if let Some(prev_anchor) = prev_anchor {
                    e.set_anchor();
                    if prev_anchor > prev_caret {
                        e.move_hor((prev_anchor.char() - prev_caret.char()) as i32);
                    } else {
                        e.move_to(prev_anchor);
                    }
                    e.swap_ends();
                }
            }),
            key!(Delete) => helper.edit_iter().for_each(|mut e| {
                let prev_caret = e.caret();
                let prev_anchor = e.unset_anchor();
                e.replace("");
                if let Some(prev_anchor) = prev_anchor {
                    e.set_anchor();
                    if prev_anchor > prev_caret {
                        e.move_hor((prev_anchor.char() - prev_caret.char()) as i32 - 1);
                    } else {
                        e.move_to(prev_anchor);
                    }
                    e.swap_ends();
                }
            }),
            key!(Left, Mod::NONE | Mod::SHIFT) => helper.edit_iter().for_each(|mut e| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut e);
                e.move_hor(-1);
            }),
            key!(Down, Mod::NONE | Mod::SHIFT) => helper.edit_iter().for_each(|mut e| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut e);
                if key.modifiers == Mod::NONE {
                    e.unset_anchor();
                    remove_empty_line(&mut e);
                }
                e.move_ver_wrapped(1);
            }),
            key!(Up, Mod::NONE | Mod::SHIFT) => helper.edit_iter().for_each(|mut e| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut e);
                if key.modifiers == Mod::NONE {
                    e.unset_anchor();
                    remove_empty_line(&mut e);
                }
                e.move_ver_wrapped(-1)
            }),
            key!(Right) => helper.edit_iter().for_each(|mut e| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut e);
                e.move_hor(1);
            }),

            key!(Esc) => {
                helper.new_moment();
                mode::set::<U>(Normal::new());
            }
            _ => {}
        }
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
    type Widget = File;

    fn send_key(&mut self, key: Event, widget: &mut Self::Widget, area: &U::Area) {
        let mut helper = EditHelper::new(widget, area);

        let sel_type = match *self {
            OneKey::GoTo(st) => match_goto::<(), U>(&mut helper, key, st),
            OneKey::Find(st, ss) | OneKey::Until(st, ss) if let Some(char) = just_char(key) => {
                match_find_until(helper, char, matches!(*self, OneKey::Until(..)), st);
                if ss {
                    *SEARCH.lock() = char.to_string();
                }
                SelType::Normal
            }
            OneKey::Inside(brackets) | OneKey::Around(brackets) => {
                let is_inside = matches!(*self, OneKey::Inside(_));
                match_inside_around(helper, key, brackets, is_inside);
                SelType::Normal
            }
            OneKey::Replace if let Some(char) = just_char(key) => {
                helper.edit_iter().for_each(|mut e| {
                    let len = e.selection().flat_map(str::chars).count();
                    e.replace(char.to_string().repeat(len));
                });
                SelType::Normal
            }
            _ => SelType::Normal,
        };

        mode::set::<U>(Normal::new_with_sel_type(sel_type));
    }
}

fn match_goto<S, U: Ui>(
    helper: &mut EditHelper<File, U::Area, S>,
    key: Event,
    mut sel_type: SelType,
) -> SelType {
    static LAST_FILE: LazyLock<Mutex<Option<String>>> = LazyLock::new(Mutex::default);

    match key {
        key!(Char('h')) => helper.edit_iter().for_each(|mut e| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut e);
            let p1 = e.search_rev("\n", None).next().map(|[_, p1]| p1);
            e.move_to(p1.unwrap_or_default());
        }),
        key!(Char('j')) => helper.edit_iter().for_each(|mut e| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut e);
            e.move_ver(i32::MAX);
        }),
        key!(Char('k')) => helper.edit_iter().for_each(|mut e| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut e);
            e.move_to_coords(0, 0)
        }),
        key!(Char('l')) => helper.edit_iter().for_each(|e| {
            select_to_end_of_line(sel_type == SelType::Extend, e);
            sel_type = SelType::BeforeEndOfLine;
        }),
        key!(Char('i')) => helper.edit_iter().for_each(|mut e| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut e);
            let p1 = e.search_rev("(^|\n)[ \t]*", None).next().map(|[_, p1]| p1);
            if let Some(p1) = p1 {
                e.move_to(p1);

                let points = e.search_fwd("[^ \t]", None).next();
                if let Some([p0, _]) = points {
                    e.move_to(p0)
                }
            }
        }),

        ////////// File change keys
        key!(Char('a')) => {
            let cur_name = helper.widget().name();
            let last_file = LAST_FILE.lock().clone();
            if let Some(last_file) = last_file {
                cmd::run_notify(format!("b {last_file}"))
                    .map(|_| *LAST_FILE.lock() = Some(cur_name));
            }
        }
        key!(Char('n')) => {
            let cur_file = helper.widget().name();
            cmd::run_notify("next-file --global").map(|_| *LAST_FILE.lock() = Some(cur_file));
        }
        key!(Char('N')) => {
            let cur_file = helper.widget().name();
            cmd::run_notify("prev-file --global").map(|_| *LAST_FILE.lock() = Some(cur_file));
        }
        Event { code, .. } => {
            let code = format!("{code:?}");
            context::notify(err!("Key [a]{code}[] not mapped on [a]go to"))
        }
    }

    sel_type
}

fn match_find_until(
    mut helper: EditHelper<'_, File, impl RawArea, ()>,
    char: char,
    is_t: bool,
    st: SelType,
) {
    use SelType::*;
    helper.edit_iter().for_each(|mut e| {
        let search = format!("\\x{{{:X}}}", char as u32);
        let cur = e.caret();
        let (points, back) = match st {
            Reverse | ExtendRev => (e.search_rev(search, None).find(|[p1, _]| *p1 != cur), 1),
            Normal | Extend => (e.search_fwd(search, None).find(|[p0, _]| *p0 != cur), -1),
            _ => unreachable!(),
        };

        if let Some([p0, _]) = points
            && p0 != e.caret()
        {
            let is_extension = !matches!(st, Extend | ExtendRev);
            if is_extension || e.anchor().is_none() {
                e.set_anchor();
            }
            e.move_to(p0);
            if is_t {
                e.move_hor(back);
            }
        } else {
            context::notify(err!("Char [a]{char}[] not found"))
        }
    });
}

fn match_inside_around(
    mut helper: EditHelper<'_, File, impl RawArea, ()>,
    key: Event,
    brackets: Brackets,
    is_inside: bool,
) {
    fn move_to_points<S>(m: &mut Editor<File, impl RawArea, S>, [p0, p1]: [Point; 2]) {
        m.move_to(p0);
        m.set_anchor();
        m.move_to(p1);
    }

    let Char(char) = key.code else {
        context::notify(err!("Key [a]{key.code}[] not mapped on this mode"));
        return;
    };

    let wc = helper.cfg().word_chars;
    let initial_cursors_len = helper.cursors().len();

    let mut failed = false;

    if let Some(object) = Object::from_char(char, wc, brackets) {
        match char {
            'w' | 'W' => helper.edit_iter().for_each(do_or_destroy(&mut failed, |e| {
                let start = object.find_behind(e, 0, None);
                let [_, p1] = object.find_ahead(e, 0, None)?;
                let p0 = {
                    let p0 = start.map(|[p0, _]| p0).unwrap_or(e.caret());
                    let p0_cat = Category::of(e.char_at(p0).unwrap(), wc);
                    let p1_cat = Category::of(e.char(), wc);
                    let is_same_cat = char == 'W' || p0_cat == p1_cat;
                    if is_same_cat { p0 } else { e.caret() }
                };
                move_to_points(e, [p0, p1]);
                e.move_hor(-1);
                Some(())
            })),
            's' | ' ' => helper.edit_iter().for_each(do_or_destroy(&mut failed, |e| {
                let [_, p0] = object.find_behind(e, 0, None)?;
                let [p1, _] = object.find_ahead(e, 0, None)?;
                move_to_points(e, [p0, p1]);
                if is_inside || char == ' ' && p0 < e.text().len() {
                    e.move_hor(-1);
                }
                Some(())
            })),
            'p' => helper.edit_iter().for_each(do_or_destroy(&mut failed, |e| {
                let end = object.find_ahead(e, 0, None);
                let [p1, _] = end?;
                e.move_to(p1);
                e.set_anchor();
                let [_, p0] = object.find_behind(e, 0, None).unwrap_or_default();
                e.move_to(p0);
                e.swap_ends();
                if is_inside {
                    e.move_hor(-1);
                }
                Some(())
            })),
            'u' => helper.edit_iter().for_each(do_or_destroy(&mut failed, |e| {
                let [p2, _] = object.find_ahead(e, 1, None)?;
                e.move_to(p2);
                let [p0, p1] = object.find_behind(e, 1, None)?;
                if is_inside {
                    move_to_points(e, [p1, p2]);
                    e.move_hor(-1);
                } else {
                    move_to_points(e, [p0, p2]);
                    if !matches!(e.char_at(p2), Some(';' | ',')) {
                        e.move_hor(-1);
                    }
                    if !matches!(e.char_at(p0), Some(';' | ',')) {
                        e.swap_ends();
                        e.move_hor(1);
                        e.swap_ends();
                    }
                }

                Some(())
            })),
            _char => helper.edit_iter().for_each(do_or_destroy(&mut failed, |e| {
                let [p2, p3] = object.find_ahead(e, 1, None)?;
                let [p0, p1] = object.find_behind(e, 1, None)?;
                let [p0, p1] = if is_inside { [p1, p2] } else { [p0, p3] };
                move_to_points(e, [p0, p1]);
                e.move_hor(-1);
                Some(())
            })),
        }
    } else {
        match char {
            'i' => helper.edit_iter().for_each(|mut e| {
                let indent = e.indent();
                if indent == 0 {
                    let end = e.len();
                    move_to_points(&mut e, [Point::default(), end]);
                } else {
                    e.set_anchor();
                    e.move_hor(-(e.v_caret().char_col() as i32));

                    while e.indent() >= indent && e.caret().line() > 0 {
                        e.move_ver(-1);
                    }
                    e.move_ver(1);
                    e.swap_ends();

                    while e.indent() >= indent && e.caret().line() + 1 < e.text().len().line() {
                        e.move_ver(1);
                    }
                    e.move_ver(-1);

                    if is_inside {
                        let [_, p1] = e.text().points_of_line(e.caret().line());
                        e.move_to(p1);
                        e.move_hor(-1);
                    } else {
                        let p1 = e.search_fwd("\n+", None).next().map(|[_, p1]| p1).unwrap();
                        e.move_to(p1);
                    }
                }
            }),
            _ => context::notify(err!("Key [a]{key.code}[] not mapped on this mode")),
        }
    }

    if initial_cursors_len == 1 && failed {
        let rel = if is_inside { "inside" } else { "around" };
        context::notify(err!("Failed selecting {rel} object"));
    }
}

fn do_or_destroy<A: RawArea, S>(
    failed_at_least_once: &mut bool,
    mut f: impl FnMut(&mut Editor<File, A, S>) -> Option<()> + Clone,
) -> impl FnMut(Editor<File, A, S>) {
    move |mut e: Editor<File, A, S>| {
        let ret: Option<()> = f(&mut e);
        if ret.is_none() {
            e.reset();
            e.destroy();
            *failed_at_least_once = true;
        }
    }
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

fn select_to_end_of_line<S>(set_anchor: bool, mut e: Editor<File, impl RawArea, S>) {
    set_anchor_if_needed(set_anchor, &mut e);
    e.set_desired_vcol(usize::MAX);
    let pre_nl = match e.char() {
        '\n' => e.chars_rev().take_while(|(_, char)| *char != '\n').next(),
        _ => e.chars_fwd().take_while(|(_, char)| *char != '\n').last(),
    };
    if let Some((p, _)) = pre_nl {
        e.move_to(p);
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
    fn search(
        &mut self,
        orig: &(Cursors, <<U as Ui>::Area as RawArea>::PrintInfo),
        file: &mut File,
        area: &<U as Ui>::Area,
        searcher: Searcher,
    ) {
        let (cursors, info) = orig;
        *file.cursors_mut().unwrap() = cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);
        helper.edit_iter().for_each(|mut e| {
            e.set_caret_on_start();
            if let Some(anchor) = e.anchor() {
                let ranges: Vec<[Point; 2]> = e.search_inc_fwd(Some(anchor)).collect();

                for (i, &[p0, p1]) in ranges.iter().enumerate() {
                    e.move_to(p0);
                    if p1.char() > p0.char() + 1 {
                        e.set_anchor();
                        e.move_to(p1);
                        e.move_hor(-1);
                    } else {
                        e.unset_anchor();
                    }
                    if i < ranges.len() - 1 {
                        e.copy();
                    }
                }
            }
        });
    }

    fn prompt(&self) -> duat_core::prelude::Text {
        text!("[Prompt]select[Prompt.colon]:")
    }
}

#[derive(Clone, Copy)]
struct Split;

impl<U: Ui> IncSearcher<U> for Split {
    fn search(
        &mut self,
        orig: &(Cursors, <<U as Ui>::Area as RawArea>::PrintInfo),
        file: &mut File,
        area: &<U as Ui>::Area,
        searcher: Searcher,
    ) {
        let (cursors, info) = orig;
        *file.cursors_mut().unwrap() = cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);
        helper.edit_iter().for_each(|mut e| {
            e.set_caret_on_start();
            if let Some(anchor) = e.anchor() {
                let ranges: Vec<Point> = e.search_inc_fwd(Some(anchor)).flatten().collect();
                let cursors_to_add = ranges.len() / 2 + 1;
                let iter = [e.caret()]
                    .into_iter()
                    .chain(ranges)
                    .chain([anchor])
                    .array_chunks();

                for (i, [p0, p1]) in iter.enumerate() {
                    e.move_to(p0);
                    if p1.char() > p0.char() + 1 {
                        e.set_anchor();
                        e.move_to(p1);
                        e.move_hor(-1);
                    } else if p1 > p0 {
                        e.unset_anchor();
                    } else {
                        continue;
                    }
                    if i < cursors_to_add {
                        e.copy();
                    }
                }
            }
        })
    }

    fn prompt(&self) -> duat_core::prelude::Text {
        text!("[Prompt]split[Prompt.colon]:")
    }
}

/// Sets the indentation for every cursor
fn reindent<S>(e: &mut Editor<File, impl RawArea, S>, processed_lines: &mut Vec<usize>) {
    let prev_caret = e.caret();
    let prev_anchor = e.unset_anchor();
    if processed_lines.contains(&prev_caret.line()) {
        return;
    }
    e.move_hor(-(e.v_caret().char_col() as i32));
    e.set_anchor();
    e.move_hor(e.indent() as i32);

    let cfg = e.cfg();
    let indent = if let Some(mut ts) = e.get_reader::<TsParser>()
        && let Some(indent) = ts.indent_on(prev_caret, cfg)
    {
        indent
    } else {
        let prev_non_empty = prev_non_empty_line_points(e);
        prev_non_empty.map(|[p0, _]| e.indent_on(p0)).unwrap_or(0)
    };

    if e.caret() == e.anchor().unwrap() {
        e.insert(" ".repeat(indent));
    } else {
        e.move_hor(-1);
        e.replace(" ".repeat(indent));
    }
    e.unset_anchor();

    if let Some(prev_anchor) = prev_anchor {
        e.set_anchor();
        if prev_anchor > prev_caret {
            e.move_hor((prev_anchor.char() - prev_caret.char()) as i32);
        } else {
            e.move_to(prev_anchor);
        }
        e.swap_ends();
    }

    if prev_caret < e.caret() {
        e.move_to(prev_caret);
    } else {
        e.move_hor(prev_caret.char() as i32 - e.caret().char() as i32);
    }

    processed_lines.push(e.caret().line());
}

/// removes an empty line
fn remove_empty_line<S>(e: &mut Editor<File, impl RawArea, S>) {
    let (_, line) = e.lines_on(e.caret()).next().unwrap();
    if !line.chars().all(char::is_whitespace) || line.len() == 1 {
        return;
    }
    let chars_count = line.chars().count();

    let dvcol = e.v_caret().desired_visual_col();
    e.move_hor(-(e.v_caret().char_col() as i32));
    e.set_anchor();
    e.move_hor(chars_count as i32 - 1);

    e.replace("");
    e.unset_anchor();
    e.set_desired_vcol(dvcol);
}

fn copy_selections(helper: &mut EditHelper<'_, File, impl RawArea, ()>) {
    let mut copies: Vec<String> = Vec::new();
    helper
        .edit_iter()
        .for_each(|e| copies.push(e.selection().collect()));
    if !copies.iter().all(String::is_empty) {
        if copies.len() == 1 {
            duat_core::clipboard::set_text(copies.first().unwrap());
        }
        *CLIPBOARD.lock() = copies
    }
}

fn paste_strings() -> Vec<String> {
    static SYSTEM_CLIPB: Mutex<Option<String>> = Mutex::new(None);

    let paste = duat_core::clipboard::get_text();

    let mut sys_clipb = SYSTEM_CLIPB.lock();

    // If there was no previous clipboard, or it has changed, copy the new
    // pasted text
    if let Some(paste) = paste
        && sys_clipb.as_ref().is_none_or(|sc| *sc != paste)
    {
        *CLIPBOARD.lock() = vec![paste.clone()];
        *sys_clipb = Some(paste.clone());
        vec![paste]
    } else {
        CLIPBOARD.lock().clone()
    }
}

fn set_anchor_if_needed<S>(set_anchor: bool, e: &mut Editor<File, impl RawArea, S>) {
    if set_anchor {
        if e.anchor().is_none() {
            e.set_anchor();
        }
    } else {
        e.unset_anchor();
    }
}

fn just_char(key: Event) -> Option<char> {
    if let key!(Char(char)) = key {
        Some(char)
    } else {
        None
    }
}

fn prev_non_empty_line_points<S>(e: &mut Editor<File, impl RawArea, S>) -> Option<[Point; 2]> {
    let byte_col = e
        .text()
        .buffers(..e.caret().byte())
        .take_while(|b| *b != b'\n')
        .count();
    let prev = e
        .lines_on(..e.caret().byte() - byte_col)
        .find_map(|(n, l)| l.chars().any(|c| !c.is_whitespace()).then_some(n));
    prev.map(|n| e.text().points_of_line(n))
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

    fn find_ahead<S>(
        self,
        e: &mut Editor<File, impl RawArea, S>,
        s_count: usize,
        until: Option<Point>,
    ) -> Option<[Point; 2]> {
        let mut s_count = s_count as i32;
        match self {
            Object::Anchored(pat) => {
                let pat = pat.strip_suffix(r"\z").unwrap();
                e.search_fwd(pat, until).next()
            }
            Object::Bounds(s_b, e_b) => {
                let (_, [p0, p1]) = e.search_fwd([s_b, e_b], None).find(|&(id, _)| {
                    s_count += (id == 0) as i32 - (id == 1) as i32;
                    s_count <= 0
                })?;
                Some([p0, p1])
            }
            Object::Bound(b) => e.search_fwd(b, until).next(),
            Object::Argument(m_b, s_b, e_b) => {
                let caret = e.caret();
                let (_, [p0, p1]) = e.search_fwd([m_b, s_b, e_b], None).find(|&(id, [p, _])| {
                    s_count += (id == 1) as i32 - (id == 2 && p != caret) as i32;
                    s_count == 0 || (s_count == 1 && id == 0)
                })?;
                Some([p0, p1])
            }
        }
    }

    fn find_behind<S>(
        self,
        e: &mut Editor<File, impl RawArea, S>,
        e_count: usize,
        until: Option<Point>,
    ) -> Option<[Point; 2]> {
        let mut e_count = e_count as i32;
        match self {
            Object::Anchored(pat) => {
                let pat = pat.strip_prefix(r"\A").unwrap();
                e.search_rev(pat, until).next()
            }
            Object::Bounds(s_b, e_b) => {
                let (_, [p0, p1]) = e.search_rev([s_b, e_b], None).find(|&(id, _)| {
                    e_count += (id == 1) as i32 - (id == 0) as i32;
                    e_count <= 0
                })?;
                Some([p0, p1])
            }
            Object::Bound(b) => e.search_rev(b, until).next(),
            Object::Argument(m_b, s_b, e_b) => {
                let (_, [p0, p1]) = e.search_rev([m_b, s_b, e_b], None).find(|&(id, _)| {
                    e_count += (id == 2) as i32 - (id == 1) as i32;
                    e_count == 0 || (e_count == 1 && id == 0)
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
        self.0.lock().entry(k).or_insert_with(f).clone()
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
