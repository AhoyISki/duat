//! # Duat Kak
//!
//! Duat kak is the implementation of the
//! [kakoune](https://github.com/mawww/kakoune) editing model for Duat.
//! It's still a work in progress, but it already implements most of
//! the common commands from Kakoune.
//!
//! The plugin currently has 2 options: `insert_tabs` and
//! `set_cursor_forms`. `insert_tabs` will make the `Tab` key insert a
//! `\t` character, instead of an appropriate amount of spaces.
//! `set_cursor_forms` will create a hook to set the `MainCursor`,
//! `ExtraCursor`, `MainSelection` and `ExtraSelection` forms to mode
//! specific varieties, e.g. `MainCursorInsert`.
#![feature(let_chains, iter_map_windows, type_alias_impl_trait, if_let_guard)]
use std::{
    marker::PhantomData,
    ops::RangeInclusive,
    sync::{
        LazyLock, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use duat_core::{
    Plugin,
    cfg::WordChars,
    cmd, context,
    data::{RwData, RwLock},
    form,
    hooks::{self, ModeSwitched},
    mode::{
        self, Cursors, EditHelper, ExtendFwd, ExtendRev, Fwd, IncSearcher, KeyCode::*,
        KeyEvent as Event, KeyMod as Mod, Mode, Mover, Rev, key,
    },
    text::{Point, Searcher, err},
    ui::{Area, Ui},
    widgets::{File, IncSearch, PipeSelections, RunCommands},
};

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
            static FORMS: &[&str] = &["Insert", "Normal", "GoTo"];
            for mode in ["Insert", "Normal", "GoTo"] {
                form::set_weak(format!("MainCursor{mode}"), "MainCursor");
                form::set_weak(format!("ExtraCursor{mode}"), "ExtraCursor");
                form::set_weak(format!("MainSelection{mode}"), "MainSelection");
                form::set_weak(format!("ExtraSelection{mode}"), "ExtraSelection");
            }

            hooks::add::<ModeSwitched>(|(_, new)| {
                if !FORMS.contains(&new) {
                    return;
                }
                form::set("MainCursor", format!("MainCursor{new}"));
                form::set("ExtraCursor", format!("ExtraCursor{new}"));
                form::set("MainSelection", format!("MainSelection{new}"));
                form::set("ExtraSelection", format!("ExtraSelection{new}"));
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
        helper.cursors_mut().make_incl();
        let w_chars = helper.cfg().word_chars;

        match key {
            ////////// Basic movement keys
            key!(Char('h' | 'H') | Left, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| {
                    set_anchor_if_needed(&mut m, key.modifiers);
                    m.move_hor(-1);
                });
                self.0 = SelType::Normal;
            }
            key!(Down, Mod::NONE | Mod::SHIFT) => helper.move_many(.., |mut m| {
                set_anchor_if_needed(&mut m, key.modifiers);
                m.move_ver_wrapped(1);
            }),
            key!(Up, Mod::NONE | Mod::SHIFT) => helper.move_many(.., |mut m| {
                set_anchor_if_needed(&mut m, key.modifiers);
                m.move_ver_wrapped(-1);
            }),
            key!(Char('l' | 'L') | Right, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| {
                    set_anchor_if_needed(&mut m, key.modifiers);
                    m.move_hor(1);
                });
                self.0 = SelType::Normal;
            }
            key!(Char('j' | 'J'), Mod::NONE | Mod::SHIFT) => helper.move_many(.., |mut m| {
                set_anchor_if_needed(&mut m, key.modifiers);
                m.move_ver(1);
                if m.char() == '\n' && m.caret_col() > 0 && self.0 != SelType::ToEndOfLine {
                    let vcol = m.desired_caret_vcol();
                    m.move_hor(-1);
                    m.set_desired_v_col(if self.0 == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        vcol
                    });
                }
            }),
            key!(Char('k' | 'K'), Mod::NONE | Mod::SHIFT) => helper.move_many(.., |mut m| {
                set_anchor_if_needed(&mut m, key.modifiers);
                m.move_ver(-1);
                if m.char() == '\n' && m.caret_col() > 0 && self.0 != SelType::ToEndOfLine {
                    let vcol = m.desired_caret_vcol();
                    m.move_hor(-1);
                    m.set_desired_v_col(if self.0 == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        vcol
                    });
                }
            }),

            ////////// Object selection keys.
            key!(Char('w'), Mod::NONE | Mod::ALT) => helper.move_many(.., |mut m| {
                let mf = key.modifiers;
                let init = no_nl_windows(m.fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, w_chars) == Category::of(c1, w_chars) {
                        m.move_to(p0);
                    } else {
                        m.move_to(p1);
                    }

                    let points = m.search_fwd(word_and_space(mf, w_chars), None).next();
                    if let Some((_, p1)) = points {
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                    }
                };
            }),
            key!(Char('e'), Mod::NONE | Mod::ALT) => helper.move_many(.., |mut m| {
                let mf = key.modifiers;
                let init = no_nl_windows(m.fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, w_chars) == Category::of(c1, w_chars) {
                        m.move_to(p0);
                    } else {
                        m.move_to(p1);
                    }

                    let points = m.search_fwd(space_and_word(mf, w_chars), None).next();
                    if let Some((_, p1)) = points {
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                    }
                };
            }),
            key!(Char('b'), Mod::NONE | Mod::ALT) => helper.move_many(.., |mut m| {
                let mf = key.modifiers;
                let init = {
                    let iter = [(m.caret(), m.char())].into_iter().chain(m.rev());
                    no_nl_windows(iter).next()
                };
                if let Some(((_, c1), (_, c0))) = init {
                    if Category::of(c0, w_chars) != Category::of(c1, w_chars) {
                        m.move_hor(-1);
                    }
                    let points = m.search_rev(word_and_space(mf, w_chars), None).next();
                    if let Some((p0, _)) = points {
                        m.set_anchor();
                        m.move_to(p0);
                    }
                };
            }),

            key!(Char('w' | 'W'), Mod::SHIFT | ALTSHIFT) => helper.move_many(.., |mut m| {
                let mf = key.modifiers;
                if m.anchor().is_none() {
                    m.set_anchor();
                } else {
                    m.move_hor(1);
                }
                let points = m.search_fwd(word_and_space(mf, w_chars), None).next();
                if let Some((_, p1)) = points {
                    m.move_to(p1);
                    m.move_hor(-1);
                }
            }),
            key!(Char('e' | 'E'), Mod::SHIFT | ALTSHIFT) => helper.move_many(.., |mut m| {
                let mf = key.modifiers;
                if m.anchor().is_none() {
                    m.set_anchor();
                } else {
                    m.move_hor(1);
                }
                let points = m.search_fwd(space_and_word(mf, w_chars), None).next();
                if let Some((_, p1)) = points {
                    m.move_to(p1);
                    m.move_hor(-1);
                }
            }),
            key!(Char('b' | 'B'), Mod::SHIFT | ALTSHIFT) => helper.move_many(.., |mut m| {
                let mf = key.modifiers;
                set_anchor_if_needed(&mut m, mf);
                let points = m.search_rev(word_and_space(mf, w_chars), None).next();
                if let Some((p0, _)) = points {
                    m.move_to(p0);
                }
            }),

            key!(Char('x')) => helper.move_many(.., |mut m| {
                self.0 = SelType::ToEndOfLine;
                set_anchor_if_needed(&mut m, Mod::SHIFT);
                m.set_caret_on_start();
                let (_, p0) = m.search_rev("\n", None).next().unzip();
                m.move_to(p0.unwrap_or_default());
                m.swap_ends();

                let (p1, _) = m.search_fwd("\n", None).next().unzip();
                if let Some(p1) = p1.or(m.last_point()) {
                    m.move_to(p1);
                }
                m.set_desired_v_col(usize::MAX);
            }),
            key!(
                Char('f' | 'F' | 't' | 'T'),
                Mod::NONE | Mod::SHIFT | Mod::ALT | ALTSHIFT
            ) => {
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
            key!(Char('a'), Mod::ALT) => mode::set::<U>(OneKey::Around),
            key!(Char('i'), Mod::ALT) => mode::set::<U>(OneKey::Inside),
            key!(Char('%')) => helper.move_main(|mut m| {
                m.move_to(Point::default());
                m.set_anchor();
                m.move_to(m.last_point().unwrap())
            }),

            ////////// Insertion mode keys
            key!(Char('i')) => {
                helper.move_many(.., |mut m| m.set_caret_on_start());
                mode::set::<U>(Insert::new());
            }
            key!(Char('I'), Mod::SHIFT) => {
                helper.move_many(.., |mut m| {
                    m.unset_anchor();
                    m.move_hor(-(m.caret_col() as i32))
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('a')) => {
                helper.move_many(.., |mut m| {
                    m.set_caret_on_end();
                    m.move_hor(1);
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('A'), Mod::SHIFT) => {
                helper.move_many(.., |mut m| {
                    m.unset_anchor();
                    let (p, _) = m.fwd().find(|(_, c)| *c == '\n').unwrap();
                    m.move_to(p);
                });
                mode::set::<U>(Insert::new());
            }
            key!(
                Char('o' | 'O'),
                Mod::NONE | Mod::ALT | Mod::SHIFT | ALTSHIFT
            ) => {
                let mut orig_points = Vec::new();
                helper.move_many(.., |mut m| {
                    orig_points.push((m.caret(), m.anchor()));
                    m.unset_anchor();
                    if key.modifiers.contains(Mod::SHIFT) {
                        m.move_hor(-(m.caret_col() as i32));
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
                            if key.modifiers == ALTSHIFT {
                                m.move_hor(1);
                            }
                        }
                        m.move_to(caret);
                        if key.modifiers == ALTSHIFT {
                            m.move_hor(1);
                        }
                    });
                } else {
                    set_indent(&mut helper);
                    mode::set::<U>(Insert::new());
                }
            }

            ////////// Selection alteration keys.
            key!(Char('r')) => mode::set::<U>(OneKey::Replace),
            key!(Char('`'), Mod::ALT) => helper.edit_many(.., |e| {
                let inverted = e.selection().flat_map(str::chars).map(|c| {
                    if c.is_uppercase() {
                        c.to_lowercase().collect::<String>()
                    } else {
                        c.to_uppercase().collect()
                    }
                });
                e.replace(inverted.collect::<String>());
            }),
            key!(Char('`')) => helper.edit_many(.., |e| {
                let lower = e
                    .selection()
                    .flat_map(str::chars)
                    .flat_map(char::to_lowercase);
                e.replace(lower.collect::<String>());
            }),
            key!(Char('~')) => helper.edit_many(.., |e| {
                let upper = e
                    .selection()
                    .flat_map(str::chars)
                    .flat_map(char::to_uppercase);
                e.replace(upper.collect::<String>());
            }),

            ////////// Selection manipulation
            key!(Char(';'), Mod::ALT) => helper.move_many(.., |mut m| m.swap_ends()),
            key!(Char(';')) => helper.move_many(.., |mut m| m.unset_anchor()),
            key!(Char(':'), ALTSHIFT) => helper.move_many(.., |mut m| m.set_caret_on_end()),
            key!(Char(')')) => helper.cursors_mut().rotate_main(1),
            key!(Char('(')) => helper.cursors_mut().rotate_main(-1),
            key!(Char(')'), ALTSHIFT) => {
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
            key!(Char('('), ALTSHIFT) => {
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
            key!(Char('_'), ALTSHIFT) => {
                helper.move_many(.., |mut m| {
                    m.set_caret_on_end();
                    m.move_hor(1)
                });
                helper.move_many(.., |mut m| m.move_hor(-1));
            }

            ////////// Clipboard keys.
            key!(Char('y')) => copy_selections(&mut helper),
            key!(Char('d'), Mod::NONE | Mod::ALT) => {
                if let Mod::NONE = key.modifiers {
                    copy_selections(&mut helper);
                }
                helper.edit_many(.., |e| e.replace(""));
                helper.move_many(.., |mut m| m.unset_anchor());
            }
            key!(Char('c'), Mod::NONE | Mod::ALT) => {
                if let Mod::NONE = key.modifiers {
                    copy_selections(&mut helper);
                }
                helper.edit_many(.., |e| e.replace(""));
                helper.move_many(.., |mut m| m.unset_anchor());
                mode::set::<U>(Insert::new());
            }
            key!(Char('p' | 'P')) => {
                let pastes = paste_strings();
                if !pastes.is_empty() {
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
                                m.move_hor(1);
                            } else {
                                m.set_caret_on_start();
                                m.move_hor(-(m.caret_col() as i32))
                            }
                        } else {
                            m.set_caret_on_start();
                            if key.code == Char('p') {
                                m.swap_ends();
                                m.move_hor(1)
                            }
                        }
                    });
                    let mut lens = pastes.iter().map(|str| str.chars().count()).cycle();
                    let mut p_iter = pastes.iter().cycle();
                    helper.edit_many(.., |e| e.insert(p_iter.next().unwrap()));
                    let mut swap_ends = swap_ends.into_iter();
                    helper.move_many(.., |mut m| {
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
                    let mut p_iter = pastes.iter().cycle();
                    helper.edit_many(.., |e| e.insert_or_replace(p_iter.next().unwrap()));
                }
            }

            ////////// Cursor creation and destruction.
            key!(Char(',')) => helper.cursors_mut().remove_extras(),
            key!(Char('C')) => helper.move_nth(helper.cursors().len() - 1, |mut m| {
                let c_col = m.caret_col();
                m.copy();
                if let Some(anchor) = m.anchor() {
                    let a_col = m.anchor_col().unwrap_or(m.caret_col());
                    let lines_diff = anchor.line() as i32 - m.caret().line() as i32;
                    let len_lines = lines_diff.unsigned_abs() as usize;
                    while m.caret().line() + len_lines < m.len().line() {
                        m.move_ver(len_lines as i32 + 1);
                        m.set_anchor();
                        m.set_desired_v_col(a_col);
                        m.move_ver(lines_diff);
                        m.swap_ends();
                        if m.caret_col() == c_col && m.anchor_col().unwrap() == a_col {
                            return;
                        }
                        m.swap_ends();
                    }
                } else {
                    while m.caret().line() < m.len().line() {
                        m.move_ver(1);
                        if m.caret_col() == c_col {
                            return;
                        }
                    }
                }
                m.destroy();
            }),
            key!(Char('c'), ALTSHIFT) => helper.move_nth(0, |mut m| {
                let c_col = m.caret_col();
                m.copy();
                if let Some(anchor) = m.anchor() {
                    let a_col = m.anchor_col().unwrap_or(m.caret_col());
                    let lines_diff = anchor.line() as i32 - m.caret().line() as i32;
                    let len_lines = lines_diff.unsigned_abs() as usize;
                    while m.caret().line().checked_sub(len_lines + 1).is_some() {
                        m.move_ver(-1 - len_lines as i32);
                        m.set_anchor();
                        m.set_desired_v_col(a_col);
                        m.move_ver(lines_diff);
                        m.swap_ends();
                        if m.caret_col() == c_col && m.anchor_col().unwrap() == a_col {
                            return;
                        }
                        m.swap_ends();
                    }
                } else {
                    while m.caret().line() > 0 {
                        m.move_ver(-1);
                        if m.caret_col() == c_col {
                            return;
                        }
                    }
                }
                m.destroy();
            }),

            ////////// Other mode changing keys.
            key!(Char(':')) => mode::set_cmd::<U>(RunCommands::new()),
            key!(Char('|')) => mode::set_cmd::<U>(PipeSelections::new()),
            key!(Char('G')) => mode::set::<U>(OneKey::GoTo(SelType::Extend)),
            key!(Char('g')) => mode::set::<U>(OneKey::GoTo(SelType::Normal)),

            ////////// Incremental search methods.
            key!(Char('/')) => mode::set_cmd::<U>(IncSearch::new(Fwd::new)),
            key!(Char('/'), Mod::ALT) => mode::set_cmd::<U>(IncSearch::new(Rev::new)),
            key!(Char('?')) => mode::set_cmd::<U>(IncSearch::new(ExtendFwd::new)),
            key!(Char('?'), Mod::ALT) => mode::set_cmd::<U>(IncSearch::new(ExtendRev::new)),
            key!(Char('s')) => mode::set_cmd::<U>(IncSearch::new(Select::new)),

            ////////// History manipulation.
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
        helper.cursors_mut().make_incl();

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
                        let tab_len = e.cfg().tab_stops.spaces_at(e.caret_vcol() as u32);
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
        helper.cursors_mut().make_incl();

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
    static LAST_FILE: LazyLock<RwData<Option<String>>> = LazyLock::new(RwData::default);
    let last_file = LAST_FILE.read().clone();
    let cur_name = helper.widget().name();

    let g_mf = if sel_type == SelType::Extend {
        Mod::SHIFT
    } else {
        Mod::NONE
    };

    match key {
        key!(Char('h')) => helper.move_many(.., |mut m| {
            set_anchor_if_needed(&mut m, g_mf);
            let (_, p1) = m.search_rev("\n", None).next().unzip();
            m.move_to(p1.unwrap_or_default());
        }),
        key!(Char('j')) => helper.move_many(.., |mut m| {
            set_anchor_if_needed(&mut m, g_mf);
            m.move_ver(i32::MAX)
        }),
        key!(Char('k')) => helper.move_many(.., |mut m| {
            set_anchor_if_needed(&mut m, g_mf);
            m.move_to_coords(0, 0)
        }),
        key!(Char('l')) => helper.move_many(.., |mut m| {
            set_anchor_if_needed(&mut m, g_mf);
            sel_type = SelType::BeforeEndOfLine;
            m.set_desired_v_col(usize::MAX);
            let pre_nl = match m.char() {
                '\n' => m.rev().take_while(|(_, char)| *char != '\n').next(),
                _ => m.fwd().take_while(|(_, char)| *char != '\n').last(),
            };
            if let Some((p, _)) = pre_nl {
                m.move_to(p);
            }
        }),
        key!(Char('i')) => helper.move_many(.., |mut m| {
            set_anchor_if_needed(&mut m, g_mf);
            let (_, p1) = m.search_rev("(^|\n)[ \t]*", None).next().unzip();
            if let Some(p1) = p1 {
                m.move_to(p1);

                let points = m.search_fwd("[^ \t]", None).next();
                if let Some((p0, _)) = points {
                    m.move_to(p0)
                }
            }
        }),

        ////////// File change keys.
        key!(Char('a')) => {
            if let Some(file) = last_file {
                cmd::run_notify(format!("b {file}")).map(|_| *LAST_FILE.write() = Some(cur_name));
            }
        }
        key!(Char('n')) => {
            cmd::run_notify("next-file --global").map(|_| *LAST_FILE.write() = Some(cur_name));
        }
        key!(Char('N')) => {
            cmd::run_notify("prev-file --global").map(|_| *LAST_FILE.write() = Some(cur_name));
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
        let cur = m.caret();
        let (points, back) = match st {
            Reverse | ExtendRev => (m.search_rev(char, None).find(|(p, _)| *p != cur), 1),
            Normal | Extend => (m.search_fwd(char, None).find(|(p, _)| *p != cur), -1),
            _ => unreachable!(),
        };

        if let Some((p0, _)) = points
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
                let Some((p1, _)) = found_e else {
                    failed_at_least_once = true;
                    m.destroy();
                    return;
                };
                m.move_to(p0);
                m.set_anchor();
                m.move_to(p1);
                if is_inside {
                    m.move_hor(-1);
                    m.swap_ends();
                    m.move_hor(1);
                    m.swap_ends();
                }
            });
        }
        Event { code, .. } => {
            let code = format!("{code:?}");
            context::notify(err!("Key " [*a] code [] " not mapped on " [*a] "go to"))
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

fn word_and_space(mf: Mod, w_chars: WordChars) -> &'static str {
    const WORD: &str = "[^ \t\n]*[ \t]*";
    static UNWS: RegexStrs = LazyLock::new(RwLock::default);

    let mut unws = UNWS.write();
    if let Some((_, word)) = unws.iter().find(|(r, _)| *r == w_chars.ranges()) {
        if mf.contains(Mod::ALT) { WORD } else { word }
    } else {
        let cat = w_char_cat(w_chars.ranges());
        let word = format!("([{cat}]+|[^{cat} \t\n]+)[ \t]*|[ \t\n]+").leak();

        unws.push((w_chars.ranges(), word));
        if mf.contains(Mod::ALT) { WORD } else { word }
    }
}

fn space_and_word(mf: Mod, w_chars: WordChars) -> &'static str {
    const WORD: &str = "[ \t]*[^ \t\n]*";
    static EOWS: RegexStrs = LazyLock::new(RwLock::default);

    let mut eows = EOWS.write();
    if let Some((_, word)) = eows.iter().find(|(r, _)| *r == w_chars.ranges()) {
        if mf.contains(Mod::ALT) { WORD } else { word }
    } else {
        let cat = w_char_cat(w_chars.ranges());
        let word = format!("[ \t\n]*([{cat}]+|[^{cat} \t\n]+)|[ \t\n]+").leak();

        eows.push((w_chars.ranges(), word));
        if mf.contains(Mod::ALT) { WORD } else { word }
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

type RegexStrs = LazyLock<RwLock<Vec<(&'static [RangeInclusive<char>], &'static str)>>>;

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

struct Select<U: Ui> {
    orig: Cursors,
    info: <U::Area as Area>::PrintInfo,
}

impl<U: Ui> IncSearcher<U> for Select<U> {
    fn new(file: &mut File, area: &U::Area) -> Self {
        Self {
            orig: file.cursors().unwrap().clone(),
            info: area.print_info(),
        }
    }

    fn search(&mut self, file: &mut File, area: &U::Area, searcher: Searcher) {
        *file.cursors_mut().unwrap() = self.orig.clone();
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);

        helper.move_many(.., |mut m| {
            m.set_caret_on_start();
            if let Some(anchor) = m.anchor() {
                let ranges: Vec<(Point, Point)> = m.search_inc_fwd(Some(anchor)).collect();

                for (i, &(p0, p1)) in ranges.iter().enumerate() {
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
}

struct Split<U: Ui> {
    orig: Cursors,
    info: <U::Area as Area>::PrintInfo,
}

impl<U: Ui> IncSearcher<U> for Split<U> {
    fn new(file: &mut File, area: &<U as Ui>::Area) -> Self {
        Self {
            orig: file.cursors().unwrap().clone(),
            info: area.print_info(),
        }
    }

    fn search(&mut self, file: &mut File, area: &U::Area, searcher: Searcher) {
        *file.cursors_mut().unwrap() = self.orig.clone();
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);

        helper.move_many(.., |mut m| {
            m.set_caret_on_start();
            if let Some(anchor) = m.anchor() {
                let ranges: Vec<(Point, Point)> = m.search_inc_fwd(Some(anchor)).collect();

                for (i, &(p0, p1)) in ranges.iter().enumerate() {
                    m.move_to(p0);
                    if p1 > p0 {
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
        })
    }
}

/// Sets the indentation for every cursor
fn set_indent(helper: &mut EditHelper<'_, File, impl Area, ()>) {
    helper.move_many(.., |mut m| {
        let (_, p0) = m.search_rev("\n", None).next().unwrap_or_default();
        m.unset_anchor();
        m.move_to(p0);
        if let Some((p1, _)) = m.fwd().take_while(|(_, c)| is_non_nl_space(*c)).last() {
            m.set_anchor();
            m.move_to(p1);
        }
    });
    helper.edit_many(.., |e| {
        let indent = e.indent_on(e.caret());
        e.insert_or_replace(" ".repeat(indent));
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

fn set_anchor_if_needed<S>(m: &mut Mover<impl Area, S>, mf: Mod) {
    if mf.contains(Mod::SHIFT) {
        if m.anchor().is_none() {
            m.set_anchor();
        }
    } else {
        m.unset_anchor();
    }
}

fn just_char(key: Event) -> Option<char> {
    if let key!(Char(char), Mod::NONE | Mod::SHIFT) = key {
        Some(char)
    } else {
        None
    }
}

const ALTSHIFT: Mod = Mod::ALT.union(Mod::SHIFT);

static INSERT_TABS: AtomicBool = AtomicBool::new(false);
