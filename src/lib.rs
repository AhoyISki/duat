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
#![feature(let_chains, iter_map_windows, if_let_guard, iter_array_chunks)]

use std::{
    marker::PhantomData,
    ops::RangeInclusive,
    sync::{
        LazyLock,
        atomic::{AtomicBool, Ordering},
    },
};

use duat_core::{
    Mutex, Plugin,
    cfg::WordChars,
    cmd, context, form,
    hooks::{self, ModeSwitched},
    mode::{
        self, EditHelper, Editor, ExtendFwd, ExtendRev, IncSearch, IncSearcher, KeyCode::*,
        KeyEvent as Event, KeyMod as Mod, Mode, Mover, Orig, PipeSelections, RunCommands,
        SearchFwd, SearchRev, key,
    },
    text::{Point, Searcher, err, text},
    ui::{Area, Ui},
    widgets::File,
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
/// - On [`Insert`] mode: `"MainCursorInsert"`, `"ExtraCursorInsert"`
/// - On [`Normal`] mode: `"MainCursorNormal"`, `"ExtraCursorNormal"`
///
/// And so on and so forth.
///
/// If you don't want the [`Form`]s to change, see
/// [`Kak::dont_set_cursor_forms`].
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

#[derive(Clone)]
pub struct Normal(SelType);

impl Normal {
    pub fn new() -> Self {
        Normal(SelType::Normal)
    }
}

impl<U: Ui> Mode<U> for Normal {
    type Widget = File;

    fn send_key(&mut self, key: Event, widget: &mut Self::Widget, area: &U::Area) {
        let mut helper = EditHelper::new(widget, area);
        let w_chars = helper.cfg().word_chars;

        match key {
            ////////// Basic movement keys
            key!(Char('h' | 'H') | Left, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| {
                    let set_anchor = key.code == Char('H') || key.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut m);
                    m.move_hor(-1);
                });
                self.0 = SelType::Normal;
            }
            key!(Down) => helper.move_many(.., |mut m| {
                set_anchor_if_needed(key.modifiers.contains(Mod::SHIFT), &mut m);
                m.move_ver_wrapped(1);
            }),
            key!(Up) => helper.move_many(.., |mut m| {
                set_anchor_if_needed(key.modifiers.contains(Mod::SHIFT), &mut m);
                m.move_ver_wrapped(-1);
            }),
            key!(Char('l' | 'L') | Right, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| {
                    let set_anchor = key.code == Char('L') || key.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut m);
                    m.move_hor(1);
                });
                self.0 = SelType::Normal;
            }
            key!(Char('j' | 'J')) => helper.move_many(.., |mut m| {
                set_anchor_if_needed(key.code == Char('J'), &mut m);
                m.move_ver(1);
                let v_caret = m.v_caret();
                if m.char() == '\n' && v_caret.char_col() > 0 && self.0 != SelType::ToEndOfLine {
                    m.move_hor(-1);
                    m.set_desired_vcol(if self.0 == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        v_caret.desired_visual_col()
                    });
                }
            }),
            key!(Char('k' | 'K')) => helper.move_many(.., |mut m| {
                set_anchor_if_needed(key.code == Char('K'), &mut m);
                m.move_ver(-1);
                let v_caret = m.v_caret();
                if m.char() == '\n' && v_caret.char_col() > 0 && self.0 != SelType::ToEndOfLine {
                    m.move_hor(-1);
                    m.set_desired_vcol(if self.0 == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        v_caret.desired_visual_col()
                    });
                }
            }),

            ////////// Object selection keys.
            key!(Char('w'), Mod::NONE | Mod::ALT) => helper.move_many(.., |mut m| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = no_nl_windows(m.fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, w_chars) == Category::of(c1, w_chars) {
                        m.move_to(p0);
                    } else {
                        m.move_to(p1);
                    }

                    let points = m.search_fwd(word_and_space(alt_word, w_chars), None).next();
                    if let Some([_, p1]) = points {
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                    }
                };
            }),
            key!(Char('e'), Mod::NONE | Mod::ALT) => helper.move_many(.., |mut m| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = no_nl_windows(m.fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, w_chars) == Category::of(c1, w_chars) {
                        m.move_to(p0);
                    } else {
                        m.move_to(p1);
                    }

                    let points = m.search_fwd(space_and_word(alt_word, w_chars), None).next();
                    if let Some([_, p1]) = points {
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                    }
                };
            }),
            key!(Char('b'), Mod::NONE | Mod::ALT) => helper.move_many(.., |mut m| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = {
                    let iter = [(m.caret(), m.char())].into_iter().chain(m.rev());
                    no_nl_windows(iter).next()
                };
                if let Some(((p1, c1), (_, c0))) = init {
                    m.move_to(p1);
                    if Category::of(c0, w_chars) == Category::of(c1, w_chars) {
                        m.move_hor(1);
                    }
                    let points = m.search_rev(word_and_space(alt_word, w_chars), None).next();
                    if let Some([p0, p1]) = points {
                        m.move_to(p0);
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                        m.swap_ends();
                    };
                };
            }),

            key!(Char('W'), Mod::ALT | Mod::NONE) => helper.move_many(.., |mut m| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                if m.anchor().is_none() {
                    m.set_anchor();
                } else {
                    m.move_hor(1);
                }
                let points = m.search_fwd(word_and_space(alt_word, w_chars), None).next();
                if let Some([_, p1]) = points {
                    m.move_to(p1);
                    m.move_hor(-1);
                }
            }),
            key!(Char('E'), Mod::NONE | Mod::ALT) => helper.move_many(.., |mut m| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                if m.anchor().is_none() {
                    m.set_anchor();
                }
                m.move_hor(1);
                let points = m.search_fwd(space_and_word(alt_word, w_chars), None).next();
                if let Some([_, p1]) = points {
                    m.move_to(p1);
                    m.move_hor(-1);
                }
            }),
            key!(Char('B'), Mod::NONE | Mod::ALT) => helper.move_many(.., |mut m| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(key.modifiers.contains(Mod::SHIFT), &mut m);
                let points = m.search_rev(word_and_space(alt_word, w_chars), None).next();
                if let Some([p0, _]) = points {
                    m.move_to(p0);
                }
            }),

            key!(Char('x')) => helper.move_many(.., |mut m| {
                self.0 = SelType::ToEndOfLine;
                set_anchor_if_needed(true, &mut m);
                m.set_caret_on_start();
                let p0 = m.search_rev("\n", None).next().map(|[_, p0]| p0);
                m.move_to(p0.unwrap_or_default());
                m.swap_ends();

                let p1 = m.search_fwd("\n", None).next().map(|[p1, _]| p1);
                if let Some(p1) = p1.or(m.last_point()) {
                    m.move_to(p1);
                }
                m.set_desired_vcol(usize::MAX);
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
                    OneKey::Find(sel_type)
                } else {
                    OneKey::Until(sel_type)
                });
            }
            key!(Char('l' | 'L'), Mod::ALT) | key!(End) => helper.move_many(.., |mut m| {
                if key.code == Char('l') {
                    m.unset_anchor();
                }
                select_to_end_of_line(true, m);
                self.0 = SelType::BeforeEndOfLine;
            }),
            key!(Char('h' | 'H'), Mod::ALT) | key!(Home) => helper.move_many(.., |mut m| {
                if key.code == Char('h') {
                    m.unset_anchor();
                }
                set_anchor_if_needed(true, &mut m);
                m.move_hor(-(m.v_caret().char_col() as i32));
            }),
            key!(Char('a'), Mod::ALT) => mode::set::<U>(OneKey::Around),
            key!(Char('i'), Mod::ALT) => mode::set::<U>(OneKey::Inside),
            key!(Char('%')) => helper.move_main(|mut m| {
                m.move_to(Point::default());
                m.set_anchor();
                m.move_to(m.last_point().unwrap())
            }),

            ////////// Insertion mode keys
            key!(Char('i')) => {
                helper.new_moment();
                helper.move_many(.., |mut m| m.set_caret_on_start());
                mode::set::<U>(Insert::new());
            }
            key!(Char('I')) => {
                helper.new_moment();
                helper.move_many(.., |mut m| {
                    m.unset_anchor();
                    m.move_hor(-(m.v_caret().char_col() as i32));
                    let points = m.search_fwd("[ \t]*[^ \t\n]", None).next();
                    if let Some([_, p1]) = points {
                        m.move_to(p1);
                        m.move_hor(-1);
                    }
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('a')) => {
                helper.new_moment();
                helper.move_many(.., |mut m| {
                    m.set_caret_on_end();
                    m.move_hor(1);
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('A')) => {
                helper.new_moment();
                helper.move_many(.., |mut m| {
                    m.unset_anchor();
                    let (p, _) = m.fwd().find(|(_, c)| *c == '\n').unwrap();
                    m.move_to(p);
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('o' | 'O'), Mod::NONE | Mod::ALT) => {
                helper.new_moment();
                let mut orig_points = Vec::new();
                helper.move_many(.., |mut m| {
                    orig_points.push((m.caret(), m.anchor()));
                    m.unset_anchor();
                    if key.code == Char('O') {
                        m.move_hor(-(m.v_caret().char_col() as i32));
                    } else {
                        let (p, _) = m.fwd().find(|(_, c)| *c == '\n').unwrap();
                        m.move_to(p);
                        m.move_hor(1);
                    }
                });
                helper.edit_many(.., |e| e.insert("\n"));
                if key.modifiers == Mod::ALT {
                    let mut orig_points = orig_points.into_iter();
                    helper.move_many(.., |mut m| {
                        let (caret, anchor) = orig_points.next().unwrap();
                        if let Some(anchor) = anchor {
                            m.move_to(anchor);
                            m.set_anchor();
                            if key.code == Char('O') {
                                m.move_hor(1);
                            }
                        }
                        m.move_to(caret);
                        if key.code == Char('O') {
                            m.move_hor(1);
                        }
                    });
                } else {
                    set_indent(&mut helper);
                    mode::set::<U>(Insert::new());
                }
            }

            ////////// Selection alteration keys.
            key!(Char('r')) => {
                helper.new_moment();
                mode::set::<U>(OneKey::Replace)
            }
            key!(Char('`')) => {
                helper.new_moment();
                helper.edit_many(.., |e| {
                    let lower = e
                        .selection()
                        .flat_map(str::chars)
                        .flat_map(char::to_lowercase);
                    e.replace(lower.collect::<String>());
                })
            }
            key!(Char('~')) => {
                helper.new_moment();
                helper.edit_many(.., |e| {
                    let upper = e
                        .selection()
                        .flat_map(str::chars)
                        .flat_map(char::to_uppercase);
                    e.replace(upper.collect::<String>());
                })
            }
            key!(Char('`'), Mod::ALT) => {
                helper.new_moment();
                helper.edit_many(.., |e| {
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
            key!(Char(';'), Mod::ALT) => helper.move_many(.., |mut m| m.swap_ends()),
            key!(Char(';')) => helper.move_many(.., |mut m| m.unset_anchor()),
            key!(Char(':'), Mod::ALT) => helper.move_many(.., |mut m| m.set_caret_on_end()),
            key!(Char(')')) => helper.cursors_mut().rotate_main(1),
            key!(Char('(')) => helper.cursors_mut().rotate_main(-1),
            key!(Char(')'), Mod::ALT) => {
                helper.new_moment();
                let mut last_sel = None;
                helper.move_many(.., |mut m| m.set_anchor());
                helper.edit_many(.., |e| {
                    if let Some(last) = last_sel.replace(e.selection().collect::<String>()) {
                        e.replace(last);
                    }
                });
                helper.edit_nth(0, |e| {
                    if let Some(last) = last_sel {
                        e.replace(last);
                    }
                });
            }
            key!(Char('('), Mod::ALT) => {
                helper.new_moment();
                let mut selections = Vec::<String>::new();
                helper.move_many(.., |mut m| {
                    m.set_anchor();
                    selections.push(m.selection().collect())
                });
                let mut s_iter = selections.into_iter().cycle();
                s_iter.next();
                helper.edit_many(.., |e| {
                    if let Some(next) = s_iter.next() {
                        e.replace(next);
                    }
                });
            }
            key!(Char('_'), Mod::ALT) => {
                helper.move_many(.., |mut m| {
                    m.set_caret_on_end();
                    m.move_hor(1)
                });
                helper.move_many(.., |mut m| m.move_hor(-1));
            }
            key!(Char('s'), Mod::ALT) => helper.move_many(.., |mut m| {
                m.set_caret_on_start();
                let Some(end) = m.anchor() else {
                    return;
                };
                let lines: Vec<[Point; 2]> = m.search_fwd("[^\n]*\n", Some(end)).collect();
                let mut last_p1 = m.caret();
                for [p0, p1] in lines {
                    m.copy_and(|mut m| {
                        m.move_to(p0);
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                    });
                    last_p1 = p1;
                }
                m.move_to(last_p1);
                m.swap_ends();
            }),
            key!(Char('s'), Mod::ALT) => helper.move_many(.., |mut m| {
                if m.anchor().is_some() {
                    m.copy_and(|mut m| {
                        m.swap_ends();
                        m.unset_anchor();
                    });
                    m.unset_anchor();
                }
            }),

            ////////// Clipboard keys.
            key!(Char('y')) => {
                helper.new_moment();
                copy_selections(&mut helper)
            }
            key!(Char('d'), Mod::NONE | Mod::ALT) => {
                helper.new_moment();
                if key.modifiers == Mod::NONE {
                    copy_selections(&mut helper);
                }
                helper.edit_many(.., |e| e.replace(""));
                helper.move_many(.., |mut m| m.unset_anchor());
            }
            key!(Char('c'), Mod::NONE | Mod::ALT) => {
                helper.new_moment();
                if key.modifiers == Mod::NONE {
                    copy_selections(&mut helper);
                }
                helper.edit_many(.., |e| e.replace(""));
                helper.move_many(.., |mut m| m.unset_anchor());
                mode::set::<U>(Insert::new());
            }
            key!(Char('p' | 'P')) => {
                let pastes = paste_strings();
                if !pastes.is_empty() {
                    helper.new_moment();
                    let mut swap_ends = Vec::new();
                    let mut p_iter = pastes.iter().cycle();
                    helper.move_many(.., |mut m| {
                        swap_ends.push(!m.anchor_is_start());
                        // If it ends in a new line, we gotta move to the start of the line.
                        if p_iter.next().unwrap().ends_with('\n') {
                            if key.code == Char('p') {
                                m.set_caret_on_end();
                                let (p, _) = m.fwd().find(|(_, c)| *c == '\n').unwrap_or_default();
                                m.move_to(p);
                            } else {
                                m.set_caret_on_start();
                                m.move_hor(-(m.v_caret().char_col() as i32))
                            }
                        } else {
                            m.set_caret_on_start();
                            if key.code == Char('p') {
                                m.swap_ends();
                            }
                        }
                    });
                    let mut lens = pastes.iter().map(|str| str.chars().count()).cycle();
                    let mut p_iter = pastes.iter().cycle();
                    helper.edit_many(.., |e| match key.code {
                        Char('p') => e.append(p_iter.next().unwrap()),
                        _ => e.insert(p_iter.next().unwrap()),
                    });
                    let mut swap_ends = swap_ends.into_iter();
                    helper.move_many(.., |mut m| {
                        if key.code == Char('p') {
                            m.move_hor(1);
                        }
                        m.set_anchor();
                        m.move_hor(lens.next().unwrap().saturating_sub(1) as i32);
                        if swap_ends.next().unwrap() {
                            m.swap_ends();
                        }
                    });
                }
            }
            key!(Char('R')) => {
                let pastes = paste_strings();
                if !pastes.is_empty() {
                    helper.new_moment();
                    let mut p_iter = pastes.iter().cycle();
                    helper.edit_many(.., |e| e.replace(p_iter.next().unwrap()));
                }
            }

            ////////// Cursor creation and destruction.
            key!(Char(',')) => helper.cursors_mut().remove_extras(),
            key!(Char('C')) => {
                helper.new_moment();
                helper.move_nth(helper.cursors().len() - 1, |mut m| {
                    let v_caret = m.v_caret();
                    m.copy();
                    if let Some(v_anchor) = m.v_anchor() {
                        let lines_diff = v_anchor.line() as i32 - m.caret().line() as i32;
                        let len_lines = lines_diff.unsigned_abs() as usize;
                        while m.caret().line() + len_lines < m.len().line() {
                            m.move_ver(len_lines as i32 + 1);
                            m.set_anchor();
                            m.set_desired_vcol(v_anchor.visual_col());
                            m.move_ver(lines_diff);
                            m.swap_ends();
                            if m.v_caret().visual_col() == v_caret.visual_col()
                                && m.v_anchor().unwrap().visual_col() == v_anchor.visual_col()
                            {
                                return;
                            }
                            m.swap_ends();
                        }
                    } else {
                        while m.caret().line() < m.len().line() {
                            m.move_ver(1);
                            if m.v_caret().visual_col() == v_caret.visual_col() {
                                return;
                            }
                        }
                    }
                    m.destroy();
                })
            }
            key!(Char('C'), Mod::ALT) => {
                helper.new_moment();
                helper.move_nth(0, |mut m| {
                    let v_caret = m.v_caret();
                    m.copy();
                    if let Some(v_anchor) = m.v_anchor() {
                        let lines_diff = v_anchor.line() as i32 - m.caret().line() as i32;
                        let len_lines = lines_diff.unsigned_abs() as usize;
                        while m.caret().line().checked_sub(len_lines + 1).is_some() {
                            m.move_ver(-1 - len_lines as i32);
                            m.set_anchor();
                            m.set_desired_vcol(v_anchor.visual_col());
                            m.move_ver(lines_diff);
                            m.swap_ends();
                            if m.v_caret().visual_col() == v_caret.visual_col()
                                && m.v_anchor().unwrap().visual_col() == v_anchor.visual_col()
                            {
                                return;
                            }
                            m.swap_ends();
                        }
                    } else {
                        while m.caret().line() > 0 {
                            m.move_ver(-1);
                            if m.v_caret().visual_col() == v_caret.visual_col() {
                                return;
                            }
                        }
                    }
                    m.destroy();
                })
            }

            ////////// Other mode changing keys.
            key!(Char(':')) => mode::set::<U>(RunCommands::new()),
            key!(Char('|')) => {
                helper.new_moment();
                mode::set::<U>(PipeSelections::new())
            }
            key!(Char('G')) => mode::set::<U>(OneKey::GoTo(SelType::Extend)),
            key!(Char('g')) => mode::set::<U>(OneKey::GoTo(SelType::Normal)),

            ////////// Incremental search methods.
            key!(Char('/')) => mode::set::<U>(IncSearch::new(SearchFwd)),
            key!(Char('/'), Mod::ALT) => mode::set::<U>(IncSearch::new(SearchRev)),
            key!(Char('?')) => mode::set::<U>(IncSearch::new(ExtendFwd)),
            key!(Char('?'), Mod::ALT) => mode::set::<U>(IncSearch::new(ExtendRev)),
            key!(Char('s')) => mode::set::<U>(IncSearch::new(Select)),
            key!(Char('S')) => mode::set::<U>(IncSearch::new(Split)),

            ////////// History manipulation.
            key!(Char('u')) => helper.undo(),
            key!(Char('U')) => helper.redo(),
            _ => {}
        }

        // helper.move_many(.., |mut m| m.unset_empty_range());
    }
}

impl Default for Normal {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct Insert(bool);

impl Insert {
    /// Returns a new instance of Kakoune's [`Insert`]
    pub fn new() -> Self {
        Self(INSERT_TABS.load(Ordering::Relaxed))
    }

    /// Returns Kakoune's [`Insert`] mode, inserting tabs
    pub fn with_tabs(self) -> Self {
        Self(true)
    }

    /// Returns Kakoune's [`Insert`] mode, not inserting tabs
    pub fn without_tabs(self) -> Self {
        Self(false)
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

        if let key!(Left | Down | Up | Right, mods) = key {
            if mods.contains(Mod::SHIFT) {
                helper.move_many(.., |mut m| {
                    if m.anchor().is_none() {
                        m.set_anchor()
                    }
                });
            } else {
                let anchors = get_anchors(&mut helper);
                helper.move_many(.., |mut m| m.unset_anchor());
                remove_empty_line(&mut helper);
                restore_anchors(&mut helper, anchors);
            }
        }

        match key {
            key!(Tab) => {
                if self.0 {
                    helper.edit_many(.., |e| e.insert('\t'));
                    helper.move_many(.., |mut m| m.move_hor(1));
                } else {
                    let mut tabs = Vec::new();
                    helper.edit_many(.., |e| {
                        let tab_len = e.cfg().tab_stops.spaces_at(e.v_caret().visual_col() as u32);
                        tabs.push(tab_len);
                        e.insert(" ".repeat(tab_len as usize))
                    });
                    let mut t_iter = tabs.into_iter();
                    helper.move_many(.., |mut m| m.move_hor(t_iter.next().unwrap() as i32))
                }
            }
            key!(Char(char)) => {
                helper.edit_many(.., |e| e.insert(char));
                helper.move_many(.., |mut m| m.move_hor(1));
            }
            key!(Enter, Mod::NONE) => {
                let anchors = get_anchors(&mut helper);
                remove_empty_line(&mut helper);
                helper.edit_many(.., |e| e.insert('\n'));
                helper.move_many(.., |mut m| m.move_hor(1));
                set_indent(&mut helper);
                restore_anchors(&mut helper, anchors);
            }
            key!(Backspace, Mod::NONE) => {
                let mut prev = Vec::with_capacity(helper.cursors().len());
                helper.move_many(.., |mut m| {
                    prev.push((m.caret(), {
                        let c = m.caret();
                        m.unset_anchor().map(|a| match a > c {
                            true => a.char() as i32 - c.char() as i32,
                            false => a.char() as i32 - (c.char() as i32 - 1),
                        })
                    }));
                    m.move_hor(-1);
                });
                let mut prev_iter = prev.iter();
                helper.edit_many(.., |e| {
                    if let Some((caret, _)) = prev_iter.next()
                        && *caret != Point::default()
                    {
                        e.replace("")
                    }
                });
                let mut anchors = prev.into_iter();
                helper.move_many(.., |mut m| {
                    if let Some((_, Some(diff))) = anchors.next() {
                        m.set_anchor();
                        m.move_hor(diff);
                        m.swap_ends()
                    }
                });
            }
            key!(Delete, Mod::NONE) => {
                let mut anchors = Vec::with_capacity(helper.cursors().len());
                helper.move_many(.., |mut m| {
                    let caret = m.caret();
                    anchors.push(m.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    m.set_anchor();
                    m.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                helper.edit_many(.., |editor| editor.replace(""));
                helper.move_many(.., |mut m| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        m.set_anchor();
                        m.move_to(anchor);
                        m.swap_ends()
                    } else {
                        m.unset_anchor();
                    }
                });
            }
            key!(Left) => helper.move_many(.., |mut m| m.move_hor(-1)),
            key!(Down) => helper.move_many(.., |mut m| m.move_ver_wrapped(1)),
            key!(Up) => helper.move_many(.., |mut m| m.move_ver_wrapped(-1)),
            key!(Right) => helper.move_many(.., |mut m| m.move_hor(1)),

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
    Find(SelType),
    Until(SelType),
    Inside,
    Around,
    Replace,
}

impl<U: Ui> Mode<U> for OneKey {
    type Widget = File;

    fn send_key(&mut self, key: Event, widget: &mut Self::Widget, area: &U::Area) {
        let mut helper = EditHelper::new(widget, area);

        let sel_type = match *self {
            OneKey::GoTo(st) => match_goto::<(), U>(&mut helper, key, st),
            OneKey::Find(st) | OneKey::Until(st) if let Some(char) = just_char(key) => {
                match_find_until(helper, char, matches!(*self, OneKey::Until(_)), st);
                SelType::Normal
            }
            OneKey::Inside | OneKey::Around => {
                match_inside_around(helper, key, matches!(*self, OneKey::Inside));
                SelType::Normal
            }
            OneKey::Replace if let Some(char) = just_char(key) => {
                helper.edit_many(.., |e| {
                    let len = e.selection().flat_map(str::chars).count();
                    e.replace(char.to_string().repeat(len));
                });
                helper.move_many(.., |mut m| m.move_hor(-1));
                SelType::Normal
            }
            _ => SelType::Normal,
        };

        mode::set::<U>(Normal(sel_type));
    }
}

fn match_goto<S, U: Ui>(
    helper: &mut EditHelper<File, U::Area, S>,
    key: Event,
    mut sel_type: SelType,
) -> SelType {
    static LAST_FILE: LazyLock<Mutex<Option<String>>> = LazyLock::new(Mutex::default);

    match key {
        key!(Char('h')) => helper.move_many(.., |mut m| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut m);
            let p1 = m.search_rev("\n", None).next().map(|[_, p1]| p1);
            m.move_to(p1.unwrap_or_default());
        }),
        key!(Char('j')) => helper.move_many(.., |mut m| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut m);
            m.move_ver(i32::MAX)
        }),
        key!(Char('k')) => helper.move_many(.., |mut m| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut m);
            m.move_to_coords(0, 0)
        }),
        key!(Char('l')) => helper.move_many(.., |m| {
            select_to_end_of_line(sel_type == SelType::Extend, m);
            sel_type = SelType::BeforeEndOfLine;
        }),
        key!(Char('i')) => helper.move_many(.., |mut m| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut m);
            let p1 = m.search_rev("(^|\n)[ \t]*", None).next().map(|[_, p1]| p1);
            if let Some(p1) = p1 {
                m.move_to(p1);

                let points = m.search_fwd("[^ \t]", None).next();
                if let Some([p0, _]) = points {
                    m.move_to(p0)
                }
            }
        }),

        ////////// File change keys.
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
            context::notify(err!("Key " [*a] code [] " not mapped on " [*a] "go to"))
        }
    }

    sel_type
}

fn match_find_until(
    mut helper: EditHelper<'_, File, impl Area, ()>,
    char: char,
    is_t: bool,
    st: SelType,
) {
    use SelType::*;
    helper.move_many(.., |mut m| {
        let search = format!("\\x{{{:X}}}", char as u32);
        let cur = m.caret();
        let (points, back) = match st {
            Reverse | ExtendRev => (m.search_rev(search, None).find(|[p1, _]| *p1 != cur), 1),
            Normal | Extend => (m.search_fwd(search, None).find(|[p0, _]| *p0 != cur), -1),
            _ => unreachable!(),
        };

        if let Some([p0, _]) = points
            && p0 != m.caret()
        {
            let is_extension = !matches!(st, Extend | ExtendRev);
            if is_extension || m.anchor().is_none() {
                m.set_anchor();
            }
            m.move_to(p0);
            if is_t {
                m.move_hor(back);
            }
        } else {
            context::notify(err!("Char " [*a] {char} [] " not found"))
        }
    });
}

fn match_inside_around(
    mut helper: EditHelper<'_, File, impl Area, ()>,
    key: Event,
    is_inside: bool,
) {
    fn move_inside<S>(mut m: Mover<impl Area, S>, is_inside: bool) {
        if is_inside {
            m.move_hor(-1);
            m.swap_ends();
            m.move_hor(1);
            m.swap_ends();
        }
    }
    fn move_to_points<S>(m: &mut Mover<impl Area, S>, [p0, p1]: [Point; 2]) {
        m.move_to(p0);
        m.set_anchor();
        m.move_to(p1);
    }

    let initial_cursors_len = helper.cursors().len();
    let mut failed_at_least_once = false;
    match key {
        key!(Char(
            'b' | '(' | ')' | 'B' | '{' | '}' | 'r' | '[' | ']' | 'a' | '<' | '>'
        )) => {
            let (s_char, e_char) = match key.code {
                Char('b' | '(' | ')') => ('(', ')'),
                Char('B' | '{' | '}') => ('{', '}'),
                Char('r' | '[' | ']') => ('[', ']'),
                Char('a' | '<' | '>') => ('<', '>'),
                _ => unreachable!(),
            };
            helper.move_many(.., |mut m| {
                let mut e_count = 0;
                let found_s = m.rev().find(|(_, c)| {
                    e_count += (*c == e_char) as i32 - (*c == s_char) as i32;
                    e_count < 0
                });
                let Some((p0, _)) = found_s else {
                    failed_at_least_once = true;
                    m.destroy();
                    return;
                };
                let mut s_count = 0;
                let found_e = m.fwd().find(|(_, c)| {
                    s_count += (*c == s_char) as i32 - (*c == e_char) as i32;
                    s_count < 0
                });
                if let Some((p1, _)) = found_e {
                    move_to_points(&mut m, [p0, p1]);
                    move_inside(m, is_inside);
                } else {
                    failed_at_least_once = true;
                    m.destroy();
                }
            });
        }
        key!(Char('q' | '\'' | 'Q' | '"' | 'g' | '`' | '|')) => {
            let char = match key.code {
                Char('q' | '\'') => '\'',
                Char('Q' | '"') => '"',
                Char('g' | '`') => '`',
                Char('|') => '|',
                _ => unreachable!(),
            };
            helper.move_many(.., |mut m| {
                m.move_hor(1);
                let start = m.rev().find(|(_, c)| *c == char);
                let end = m.fwd().find(|(_, c)| *c == char);
                if let Some((p0, _)) = start
                    && let Some((p1, _)) = end
                {
                    move_to_points(&mut m, [p0, p1]);
                    move_inside(m, is_inside);
                } else {
                    failed_at_least_once = true;
                    m.reset();
                    m.destroy();
                }
            })
        }
        key!(Char('w' | 'W')) => helper.move_many(.., |mut m| {
            let alt_word = key.code == Char('w');
            let w_chars = m.cfg().word_chars;
            let start = m.search_rev(e_anchor_word(alt_word, w_chars), None).next();
            let end = m.search_fwd(s_anchor_word(alt_word, w_chars), None).next();
            if let Some([_, p1]) = end {
                let p0 = {
                    let p0 = start.map(|[p0, _]| p0).unwrap_or(m.caret());
                    let p0_cat = Category::of(m.char_at(p0).unwrap(), w_chars);
                    let p1_cat = Category::of(m.char_at(p1).unwrap(), w_chars);
                    let p0_is_same = alt_word || p0_cat == p1_cat;
                    if p0_is_same { p0 } else { m.caret() }
                };
                move_to_points(&mut m, [p0, p1]);
                m.move_hor(-1);
            } else {
                failed_at_least_once = true;
                m.destroy();
            }
        }),
        key!(Char('s')) => helper.move_many(.., |mut m| {
            let start = m.search_rev(r"[\.;!\?]", None).next();
            let end = m.search_fwd(r"[\.;!\?]", None).next();
            if let Some([_, p0]) = start
                && let Some([p1, _]) = end
            {
                move_to_points(&mut m, [p0, p1]);
                if is_inside {
                    m.move_hor(-1);
                }
            } else {
                failed_at_least_once = true;
                m.destroy();
            }
        }),
        key!(Char('p')) => helper.move_many(.., |mut m| {
            let end = m.search_fwd("^\n", None).next();
            if let Some([p1, _]) = end {
                m.move_to(p1);
                m.set_anchor();
                let [_, p0] = m.search_rev("^\n", None).next().unwrap_or_default();
                m.move_to(p0);
                m.swap_ends();
                if is_inside {
                    m.move_hor(-1);
                }
            } else {
                failed_at_least_once = true;
                m.destroy();
            }
        }),
        key!(Char(' ')) => helper.move_many(.., |mut m| {
            let start = m.search_rev(r"\s*\z", None).next();
            let end = m.search_fwd(r"\A\s+", None).next();
            if let Some([_, p1]) = end {
                let p0 = start.map(|[p0, _]| p0).unwrap_or(m.caret());
                move_to_points(&mut m, [p0, p1]);
                if p0 < m.text().len() {
                    m.move_hor(-1);
                }
            } else {
                failed_at_least_once = true;
                m.destroy();
            }
        }),
        key!(Char('i')) => helper.move_many(.., |mut m| {
            let indent = m.indent();
            if indent == 0 {
                let end = m.len();
                move_to_points(&mut m, [Point::default(), end]);
            } else {
                m.set_anchor();
                m.move_hor(-(m.v_caret().char_col() as i32));

                while m.indent() >= indent && m.caret().line() > 0 {
                    m.move_ver(-1);
                }
                m.move_ver(1);
                m.swap_ends();

                while m.indent() >= indent && m.caret().line() + 1 < m.text().len().line() {
                    m.move_ver(1);
                }
                m.move_ver(-1);

                if is_inside {
                    let [_, p1] = m.text().points_of_line(m.caret().line());
                    m.move_to(p1);
                    m.move_hor(-1);
                } else {
                    let p1 = m.search_fwd("\n+", None).next().map(|[_, p1]| p1).unwrap();
                    m.move_to(p1);
                }
            }
        }),
        Event { code, .. } => {
            let code = format!("{code:?}");
            context::notify(err!("Key " [*a] code [] " not mapped on this mode"))
        }
    }

    if initial_cursors_len == 1 && failed_at_least_once {
        let rel = if is_inside { "inside" } else { "around" };
        context::notify(err!("Failed selecting " rel " object"));
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

fn word_and_space(alt_word: bool, w_chars: WordChars) -> String {
    if alt_word {
        "[^ \t\n]*[ \t]*".to_string()
    } else {
        let cat = w_char_cat(w_chars.ranges());
        format!("([{cat}]+|[^{cat} \t\n]+)[ \t]*|[ \t]+")
    }
}

fn space_and_word(alt_word: bool, w_chars: WordChars) -> String {
    if alt_word {
        "[ \t]*[^ \t\n]*".to_string()
    } else {
        let cat = w_char_cat(w_chars.ranges());
        format!("[ \t]*([{cat}]+|[^{cat} \t\n]+)|[ \t]+")
    }
}

fn s_anchor_word(alt_word: bool, w_chars: WordChars) -> String {
    if alt_word {
        "\\A[^ \t\n]+]".to_string()
    } else {
        let cat = w_char_cat(w_chars.ranges());
        format!("\\A([{cat}]+|[^{cat} \t\n]+)")
    }
}

fn e_anchor_word(is_alt_word: bool, w_chars: WordChars) -> String {
    if is_alt_word {
        "[^ \t\n]+]\\z".to_string()
    } else {
        let cat = w_char_cat(w_chars.ranges());
        format!("([{cat}]+|[^{cat} \t\n]+)\\z")
    }
}

fn w_char_cat(ranges: &'static [RangeInclusive<char>]) -> String {
    ranges
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

fn select_to_end_of_line<S>(set_anchor: bool, mut m: Mover<'_, impl Area, S>) {
    set_anchor_if_needed(set_anchor, &mut m);
    m.set_desired_vcol(usize::MAX);
    let pre_nl = match m.char() {
        '\n' => m.rev().take_while(|(_, char)| *char != '\n').next(),
        _ => m.fwd().take_while(|(_, char)| *char != '\n').last(),
    };
    if let Some((p, _)) = pre_nl {
        m.move_to(p);
    }
}

#[derive(PartialEq, Eq)]
enum Category {
    Word,
    Special,
    Space,
}

impl Category {
    fn of(char: char, w_chars: WordChars) -> Self {
        if w_chars.contains(char) {
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
    fn search(&mut self, orig: &Orig<U>, file: &mut File, area: &U::Area, searcher: Searcher) {
        let (cursors, info) = orig;
        *file.cursors_mut().unwrap() = cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);

        helper.move_many(.., |mut m| {
            m.set_caret_on_start();
            if let Some(anchor) = m.anchor() {
                let ranges: Vec<[Point; 2]> = m.search_inc_fwd(Some(anchor)).collect();

                for (i, &[p0, p1]) in ranges.iter().enumerate() {
                    m.move_to(p0);
                    if p1.char() > p0.char() + 1 {
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                    } else {
                        m.unset_anchor();
                    }
                    if i < ranges.len() - 1 {
                        m.copy();
                    }
                }
            }
        });
    }

    fn prompt(&self) -> duat_core::prelude::Text {
        text!([Prompt] "select" [Prompt.colon] ":")
    }
}

#[derive(Clone, Copy)]
struct Split;

impl<U: Ui> IncSearcher<U> for Split {
    fn search(&mut self, orig: &Orig<U>, file: &mut File, area: &U::Area, searcher: Searcher) {
        let (cursors, info) = orig;
        *file.cursors_mut().unwrap() = cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);

        helper.move_many(.., |mut m| {
            m.set_caret_on_start();
            if let Some(anchor) = m.anchor() {
                let ranges: Vec<Point> = m.search_inc_fwd(Some(anchor)).flatten().collect();
                let cursors_to_add = ranges.len() / 2 + 1;
                let iter = [m.caret()]
                    .into_iter()
                    .chain(ranges)
                    .chain([anchor])
                    .array_chunks();

                for (i, [p0, p1]) in iter.enumerate() {
                    m.move_to(p0);
                    if p1.char() > p0.char() + 1 {
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                    } else if p1 > p0 {
                        m.unset_anchor();
                    } else {
                        continue;
                    }
                    if i < cursors_to_add {
                        m.copy();
                    }
                }
            }
        })
    }

    fn prompt(&self) -> duat_core::prelude::Text {
        text!([Prompt] "split" [Prompt.colon] ":")
    }
}

/// Sets the indentation for every cursor
fn set_indent(helper: &mut EditHelper<'_, File, impl Area, ()>) {
    helper.move_many(.., |mut m| {
        m.move_hor(-(m.v_caret().char_col() as i32));
        if let Some((p1, _)) = m.fwd().take_while(|(_, c)| is_non_nl_space(*c)).last() {
            m.set_anchor();
            m.move_to(p1);
        }
    });
    helper.edit_many(.., |e| {
        let cfg = e.cfg();
        let caret = e.caret();
        let indent = if let Some(mut ts) = e.get_reader::<TsParser>()
            && let Some(indent) = ts.indent_on(caret, cfg)
        {
            indent
        } else {
            let prev_non_empty = prev_non_empty_line_points(e);
            prev_non_empty.map(|[p0, _]| e.indent_on(p0)).unwrap_or(0)
        };
        if e.anchor().is_some() {
            e.replace(" ".repeat(indent));
        } else {
            e.insert(" ".repeat(indent));
        }
    });
    helper.move_many(.., |mut m| {
        m.unset_anchor();
        let indent_end = m.fwd().find(|(_, c)| !is_non_nl_space(*c));
        if let Some((p, _)) = indent_end {
            m.move_to(p);
        }
    });
}

/// removes an empty line
fn remove_empty_line(helper: &mut EditHelper<'_, File, impl Area, ()>) {
    helper.move_many(.., |mut m| {
        m.unset_anchor();
        let indent_start = m.rev().find(|(_, c)| !is_non_nl_space(*c));
        let (s, char) = indent_start.unwrap_or((Point::default(), '\n'));
        if char == '\n' && m.char() == '\n' && s.byte() + 1 < m.caret().byte() {
            m.move_hor(-1);
            m.set_anchor();
            m.move_to(s);
            m.move_hor(1);
        }
    });
    helper.edit_many(.., |e| {
        if e.anchor().is_some() {
            e.replace("")
        }
    });
    helper.move_many(.., |mut m| m.unset_anchor());
}

fn get_anchors(helper: &mut EditHelper<'_, File, impl Area, ()>) -> Vec<Option<Point>> {
    let mut anchors = Vec::new();
    helper.move_many(.., |m| anchors.push(m.anchor()));
    anchors
}

fn restore_anchors(helper: &mut EditHelper<'_, File, impl Area, ()>, anchors: Vec<Option<Point>>) {
    let mut anchors = anchors.into_iter();
    helper.move_many(.., |mut m| {
        if let Some(Some(anchor)) = anchors.next() {
            m.set_anchor();
            match anchor < m.caret() {
                true => m.move_to(anchor),
                false => m.move_hor(anchor.byte() as i32 - m.caret().byte() as i32),
            }
            m.swap_ends();
        } else {
            m.unset_anchor();
        }
    });
}

fn is_non_nl_space(char: char) -> bool {
    char.is_whitespace() && char != '\n'
}

static CLIPBOARD: Mutex<Vec<String>> = Mutex::new(Vec::new());

fn copy_selections(helper: &mut EditHelper<'_, File, impl Area, ()>) {
    let mut copies: Vec<String> = Vec::new();
    helper.move_many(.., |m| copies.push(m.selection().collect()));
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

fn set_anchor_if_needed<S>(set_anchor: bool, m: &mut Mover<impl Area, S>) {
    if set_anchor {
        if m.anchor().is_none() {
            m.set_anchor();
        }
    } else {
        m.unset_anchor();
    }
}

fn just_char(key: Event) -> Option<char> {
    if let key!(Char(char)) = key {
        Some(char)
    } else {
        None
    }
}

fn prev_non_empty_line_points(e: &mut Editor<impl Area, File>) -> Option<[Point; 2]> {
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
