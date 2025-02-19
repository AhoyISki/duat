#![feature(let_chains, iter_map_windows, type_alias_impl_trait, if_let_guard)]
use std::{
    marker::PhantomData,
    ops::RangeInclusive,
    sync::{LazyLock, Mutex},
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
        KeyEvent as Event, KeyMod as Mod, Mode, Rev, key,
    },
    text::{Point, Searcher, err},
    ui::{Area, Ui},
    widgets::{File, IncSearch, RunCommands},
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
    _u: PhantomData<U>,
}

impl<U: Ui> Plugin<U> for Kak<U> {
    fn new() -> Self {
        Self { set_cursor_forms: true, _u: PhantomData }
    }

    fn plug(self) {
        duat_core::mode::set_default::<Normal, U>(Normal::new());
        if self.set_cursor_forms {
            static FORMS: &[&str] = &["Insert", "Normal", "GoTo"];
            for mode in ["Insert", "Normal", "GoTo"] {
                form::set_weak(format!("MainCursor{mode}"), "MainCursor");
                form::set_weak(format!("ExtraCursor{mode}"), "ExtraCursor");
                form::set_weak(format!("MainSelection{mode}"), "MainSelection");
                form::set_weak(format!("ExtraSelection{mode}"), "ExtraSelection");
            }

            hooks::add::<ModeSwitched>(|&(_, new)| {
                if !FORMS.contains(&new) {
                    form::set("MainCursor", "MainCursor");
                    form::set("ExtraCursor", "ExtraCursor");
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
    pub fn dont_set_cursor_forms(self) -> Self {
        Self { set_cursor_forms: false, ..self }
    }
}

const ALTSHIFT: Mod = Mod::ALT.union(Mod::SHIFT);

#[derive(Clone)]
pub struct Normal(SelType);

impl Normal {
    pub fn new() -> Self {
        Normal(SelType::Normal)
    }
}

impl<U: Ui> Mode<U> for Normal {
    type Widget = File;

    fn send_key(&mut self, key: Event, widget: &RwData<Self::Widget>, area: &U::Area) {
        let mut helper = EditHelper::new(widget, area);
        helper.make_incl();
        let w_chars = helper.cfg().word_chars;

        if let key!(
            Char('H' | 'J' | 'K' | 'L' | 'B') | Left | Down | Up | Right,
            Mod::SHIFT | ALTSHIFT
        ) = key
        {
            helper.move_many(.., |mut m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }
            })
        } else if let key!(
            Char('h' | 'j' | 'k' | 'l' | 'w' | 'b' | 'e') | Left | Down | Up | Right
        ) = key
        {
            helper.move_many(.., |mut m| m.unset_anchor())
        }
        self.0 = if let key!(Char('j' | 'k' | 'J' | 'K'), Mod::NONE | Mod::SHIFT) = key {
            self.0
        } else {
            SelType::Normal
        };

        match key {
            ////////// TEST
            key!(Char('v')) => {
                std::thread::sleep(std::time::Duration::from_micros(100));
            }
            ////////// Basic movement keys
            key!(Char('h' | 'H') | Left, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| m.move_hor(-1))
            }
            key!(Down, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| m.move_ver_wrapped(1))
            }
            key!(Up, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| m.move_ver_wrapped(-1))
            }
            key!(Char('l' | 'L') | Right, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| m.move_hor(1))
            }
            key!(Char('j' | 'J'), Mod::NONE | Mod::SHIFT) => helper.move_many(.., |mut m| {
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

            ////////// Word and WORD selection keys.
            key!(Char('w'), mf) if let Mod::ALT | Mod::NONE = mf => {
                helper.move_many(.., |mut m| {
                    let init = no_nl_windows(m.iter()).next();

                    if let Some(((p0, c0), (p1, c1))) = init {
                        if Category::of(c0, w_chars) == Category::of(c1, w_chars) {
                            m.move_to(p0);
                        } else {
                            m.move_to(p1);
                        }

                        let (_, p1) = m
                            .search_fwd(word_and_space(mf, w_chars), None)
                            .next()
                            .unzip();
                        if let Some(p1) = p1 {
                            m.set_anchor();
                            m.move_to(p1);
                            m.move_hor(-1);
                        }
                    };
                })
            }
            key!(Char('e'), mf) if let Mod::ALT | Mod::NONE = mf => {
                helper.move_many(.., |mut m| {
                    let init = no_nl_windows(m.iter()).next();

                    if let Some(((p0, c0), (p1, c1))) = init {
                        if Category::of(c0, w_chars) == Category::of(c1, w_chars) {
                            m.move_to(p0);
                        } else {
                            m.move_to(p1);
                        }

                        let (_, p1) = m
                            .search_fwd(space_and_word(mf, w_chars), None)
                            .next()
                            .unzip();
                        if let Some(p1) = p1 {
                            m.set_anchor();
                            m.move_to(p1);
                            m.move_hor(-1);
                        }
                    };
                })
            }
            key!(Char('b'), mf) if let Mod::ALT | Mod::NONE = mf => {
                helper.move_many(.., |mut m| {
                    let init = {
                        let iter = [(m.caret(), m.char())].into_iter().chain(m.iter_rev());
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
                })
            }

            key!(Char('w' | 'W'), mf) if let Mod::SHIFT | ALTSHIFT = mf => {
                helper.move_many(.., |mut m| {
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
                })
            }
            key!(Char('e' | 'E'), mf) if let Mod::SHIFT | ALTSHIFT = mf => {
                helper.move_many(.., |mut m| {
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
                })
            }
            key!(Char('b' | 'B'), mf) if let Mod::SHIFT | ALTSHIFT = mf => {
                helper.move_many(.., |mut m| {
                    let points = m.search_rev(word_and_space(mf, w_chars), None).next();
                    if let Some((p0, _)) = points {
                        m.move_to(p0);
                    }
                })
            }

            ////////// Other selection keys.
            key!(Char('x')) => helper.move_many(.., |mut m| {
                self.0 = SelType::ToEndOfLine;
                if m.anchor().is_none() {
                    m.set_anchor();
                } else {
                    m.set_caret_on_start()
                }

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
            key!(Char('%')) => helper.move_main(|mut m| {
                m.move_to(Point::default());
                m.set_anchor();
                m.move_to(m.last_point().unwrap())
            }),
            key!(Char(';'), Mod::ALT) => helper.move_many(.., |mut m| m.swap_ends()),
            key!(Char(';')) => helper.move_many(.., |mut m| m.unset_anchor()),
            key!(Char(')')) => helper.rotate_main(1),
            key!(Char('(')) => helper.rotate_main(-1),

            ////////// Insert mode keys.
            key!(Char('i')) => {
                helper.move_many(.., |mut m| m.set_caret_on_start());
                mode::set::<U>(Insert);
            }
            key!(Char('a')) => {
                helper.move_many(.., |mut m| {
                    m.set_caret_on_end();
                    m.move_hor(1);
                });
                mode::set::<U>(Insert);
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
                        let (p, _) = m.iter().find(|(_, c)| *c == '\n').unwrap();
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
                    mode::set::<U>(Insert);
                }
            }

            ////////// Clipboard keys.
            key!(Char('y')) => copy_selections(&mut helper),
            key!(Char('d'), mods) if let Mod::NONE | Mod::ALT = mods => {
                if let Mod::NONE = mods {
                    copy_selections(&mut helper);
                }
                helper.edit_many(.., |e| e.replace(""));
                helper.move_many(.., |mut m| m.unset_anchor());
            }
            key!(Char('c'), mods) if let Mod::NONE | Mod::ALT = mods => {
                if let Mod::NONE = mods {
                    copy_selections(&mut helper);
                }
                helper.edit_many(.., |e| e.replace(""));
                helper.move_many(.., |mut m| m.unset_anchor());
                mode::set::<U>(Insert);
            }
            key!(Char('p' | 'P'), Mod::NONE | Mod::SHIFT) => {
                let pastes = paste_strings();
                let len = pastes.len();
                if !pastes.is_empty() {
                    let mut swap_ends = Vec::new();
                    let mut p_iter = pastes.iter();
                    helper.move_many(..len, |mut m| {
                        swap_ends.push(!m.anchor_is_start());
                        // If it ends in a new line, we gotta move to the start of the line.
                        if p_iter.next().unwrap().ends_with('\n') {
                            if key.code == Char('p') {
                                m.set_caret_on_end();
                                let (p, _) = m.iter().find(|(_, c)| *c == '\n').unwrap_or_default();
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
                    let mut lens = pastes.iter().map(|str| str.chars().count());
                    let mut p_iter = pastes.iter();
                    helper.edit_many(..len, |e| e.insert(p_iter.next().unwrap()));
                    let mut swap_ends = swap_ends.into_iter();
                    helper.move_many(..len, |mut m| {
                        m.set_anchor();
                        m.move_hor(lens.next().unwrap().saturating_sub(1) as i32);
                        if swap_ends.next().unwrap() {
                            m.swap_ends();
                        }
                    });
                }
            }

            ////////// Cursor creation and destruction.
            key!(Char(',')) => helper.remove_extra_cursors(),
            key!(Char('C'), Mod::SHIFT) => helper.move_nth(helper.cursors_len() - 1, |mut m| {
                m.copy();
                m.move_ver(1);
            }),
            key!(Char('C'), ALTSHIFT) => helper.move_nth(0, |mut m| {
                m.copy();
                m.move_ver(-1);
            }),

            ////////// Other mode changing keys.
            key!(Char(':')) => mode::set_cmd::<U>(RunCommands::new()),
            key!(Char('G'), Mod::SHIFT) => mode::set::<U>(OneKey::GoTo(SelType::Extend)),
            key!(Char('g')) => mode::set::<U>(OneKey::GoTo(SelType::Normal)),

            ////////// Incremental search methods.
            key!(Char('/')) => mode::set_cmd::<U>(IncSearch::new(Fwd::new)),
            key!(Char('/'), Mod::ALT) => mode::set_cmd::<U>(IncSearch::new(Rev::new)),
            key!(Char('?')) => mode::set_cmd::<U>(IncSearch::new(ExtendFwd::new)),
            key!(Char('?'), Mod::ALT) => mode::set_cmd::<U>(IncSearch::new(ExtendRev::new)),
            key!(Char('s')) => mode::set_cmd::<U>(IncSearch::new(Select::new)),

            ////////// History manipulation.
            key!(Char('u')) => helper.undo(),
            key!(Char('U'), Mod::SHIFT) => helper.redo(),
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
pub struct Insert;

impl<U: Ui> Mode<U> for Insert {
    type Widget = File;

    fn send_key(&mut self, key: Event, widget: &RwData<Self::Widget>, area: &<U as Ui>::Area) {
        let mut helper = EditHelper::new(widget, area);
        helper.make_incl();

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
            key!(Char(char)) => {
                helper.edit_many(.., |e| e.insert(char));
                helper.move_many(.., |mut m| m.move_hor(1));
            }
            key!(Char(char), Mod::SHIFT) => {
                helper.edit_many(.., |e| e.insert(char));
                helper.move_many(.., |mut m| m.move_hor(1));
            }
            key!(Enter) => {
                let anchors = get_anchors(&mut helper);
                remove_empty_line(&mut helper);
                helper.edit_many(.., |e| e.insert('\n'));
                helper.move_many(.., |mut m| m.move_hor(1));
                set_indent(&mut helper);
                restore_anchors(&mut helper, anchors);
            }
            key!(Backspace) => {
                let mut prev = Vec::with_capacity(helper.cursors_len());
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
            key!(Delete) => {
                let mut anchors = Vec::with_capacity(helper.cursors_len());
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
            key!(Left, Mod::NONE | Mod::SHIFT) => helper.move_many(.., |mut m| m.move_hor(-1)),
            key!(Down, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| m.move_ver_wrapped(1))
            }
            key!(Up, Mod::NONE | Mod::SHIFT) => {
                helper.move_many(.., |mut m| m.move_ver_wrapped(-1))
            }
            key!(Right, Mod::NONE | Mod::SHIFT) => helper.move_many(.., |mut m| m.move_hor(1)),

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
}

impl OneKey {
    fn match_goto<S, U: Ui>(
        &mut self,
        helper: &mut EditHelper<File, U::Area, S>,
        key: Event,
    ) -> SelType {
        static LAST_FILE: LazyLock<RwData<Option<String>>> = LazyLock::new(RwData::default);
        let last_file = LAST_FILE.read().clone();
        let cur_name = context::cur_file::<U>().unwrap().name();

        let OneKey::GoTo(sel_type) = self else {
            unreachable!();
        };

        if let key!(Char('h' | 'j' | 'k' | 'l' | 'i' | 't' | 'b' | 'c' | '.')) = key
            && let SelType::Normal = sel_type
        {
            helper.move_many(.., |mut m| m.unset_anchor())
        } else {
            helper.move_many(.., |mut m| {
                if m.anchor().is_none() {
                    m.set_anchor()
                }
            })
        }

        match key {
            key!(Char('h')) => helper.move_many(.., |mut m| {
                let (_, p1) = m.search_rev("\n", None).next().unzip();
                m.move_to(p1.unwrap_or_default());
            }),
            key!(Char('j')) => helper.move_many(.., |mut m| m.move_ver(i32::MAX)),
            key!(Char('k')) => helper.move_many(.., |mut m| m.move_to_coords(0, 0)),
            key!(Char('l')) => helper.move_many(.., |mut m| {
                *sel_type = SelType::BeforeEndOfLine;
                m.set_desired_v_col(usize::MAX);
                let pre_nl = match m.char() {
                    '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                    _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                };
                if let Some((p, _)) = pre_nl {
                    m.move_to(p);
                }
            }),
            key!(Char('i')) => helper.move_many(.., |mut m| {
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
                    cmd::run_notify(format!("b {file}"))
                        .map(|_| *LAST_FILE.write() = Some(cur_name));
                }
            }
            key!(Char('n')) => {
                cmd::run_notify("next-file").map(|_| *LAST_FILE.write() = Some(cur_name));
            }
            key!(Char('N'), Mod::SHIFT) => {
                cmd::run_notify("prev-file").map(|_| {
                    *LAST_FILE.write() = Some(cur_name);
                });
            }
            Event { code, .. } => {
                let code = format!("{code:?}");
                context::notify(err!("Key " [*a] code [] " not mapped on " [*a] "go to" [] "."))
            }
        }

        self.sel_type()
    }

    fn sel_type(&self) -> SelType {
        match self {
            OneKey::GoTo(sel_type) => *sel_type,
            OneKey::Find(sel_type) => *sel_type,
            OneKey::Until(sel_type) => *sel_type,
        }
    }
}

impl<U: Ui> Mode<U> for OneKey {
    type Widget = File;

    fn send_key(&mut self, key: Event, widget: &RwData<Self::Widget>, area: &<U as Ui>::Area) {
        let mut helper = EditHelper::new(widget, area);
        helper.make_incl();
        let mut sel_type = self.sel_type();

        sel_type = match self {
            OneKey::GoTo(_) => self.match_goto::<(), U>(&mut helper, key),
            OneKey::Find(_) | OneKey::Until(_)
                if let key!(Char(char), Mod::SHIFT | Mod::NONE) = key =>
            {
                use SelType::*;
                helper.move_many(.., |mut m| {
                    let cur = m.caret();
                    let (points, back) = match sel_type {
                        Reverse | ExtendRev => {
                            (m.search_rev(char, None).find(|(p, _)| *p != cur), 1)
                        }
                        Normal | Extend => (m.search_fwd(char, None).find(|(p, _)| *p != cur), -1),
                        _ => unreachable!(),
                    };

                    if let Some((p0, _)) = points
                        && p0 != m.caret()
                    {
                        let is_extension = !matches!(sel_type, Extend | ExtendRev);
                        if is_extension || m.anchor().is_none() {
                            m.set_anchor();
                        }
                        m.move_to(p0);
                        if let OneKey::Until(_) = self {
                            m.move_hor(back);
                        }
                    } else {
                        context::notify(err!("Char " [*a] {char} [] " not found."))
                    }
                });

                SelType::Normal
            }
            _ => SelType::Normal,
        };

        mode::set::<U>(Normal(sel_type));
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
    fn new(file: &RwData<File>, area: &<U as Ui>::Area) -> Self {
        Self {
            orig: file.read().cursors().unwrap().clone(),
            info: area.print_info(),
        }
    }

    fn search(&mut self, file: &RwData<File>, area: &<U as Ui>::Area, searcher: Searcher) {
        *file.write().cursors_mut().unwrap() = self.orig.clone();
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
    fn new(file: &RwData<File>, area: &<U as Ui>::Area) -> Self {
        Self {
            orig: file.read().cursors().unwrap().clone(),
            info: area.print_info(),
        }
    }

    fn search(&mut self, file: &RwData<File>, area: &<U as Ui>::Area, searcher: Searcher) {
        *file.write().cursors_mut().unwrap() = self.orig.clone();
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
        let (_, p) = m.search_rev("\n", None).next().unwrap_or_default();
        m.unset_anchor();
        m.move_to(p);
        if let Some((p, _)) = m.iter().take_while(|(_, c)| is_non_nl_space(*c)).last() {
            m.set_anchor();
            m.move_to(p);
        }
    });
    helper.edit_many(.., |e| {
        let indent = e.indent_on(e.caret()).unwrap_or(0);
        e.insert_or_replace(" ".repeat(indent));
    });
    helper.move_many(.., |mut m| {
        m.unset_anchor();
        let indent_end = m.iter().find(|(_, c)| !is_non_nl_space(*c));
        if let Some((p, _)) = indent_end {
            m.move_to(p);
        }
    });
}

/// removes an empty line
fn remove_empty_line(helper: &mut EditHelper<'_, File, impl Area, ()>) {
    helper.move_many(.., |mut m| {
        m.unset_anchor();
        let indent_start = m.iter_rev().find(|(_, c)| !is_non_nl_space(*c));
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
