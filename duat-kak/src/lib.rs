#![feature(let_chains, iter_map_windows, type_alias_impl_trait, if_let_guard)]

use std::{ops::RangeInclusive, sync::LazyLock};

use duat_core::{
    cfg::WordChars,
    cmd, context,
    data::{RwData, RwLock},
    mode::{
        self, Cursors, EditHelper, ExtendFwd, ExtendRev, Fwd, IncSearcher, KeyCode::*,
        KeyEvent as Event, KeyMod as Mod, Mode, Rev, key,
    },
    text::{Point, err},
    ui::{Area, Ui},
    widgets::{File, IncSearch, RunCommands},
};

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

    fn send_key(
        &mut self,
        key: Event,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        cursors: &mut Cursors,
    ) {
        cursors.make_incl();
        let mut helper = EditHelper::new(widget, area, cursors);
        let w_chars = helper.cfg().word_chars;

        if let key!(Char('h' | 'j' | 'k' | 'l' | 'w' | 'b' | 'e') | Down | Up) = key {
            helper.move_each(|mut m| m.unset_anchor())
        }

        match key {
            ////////// hjkl and arrow selection keys.
            key!(Char('h')) => helper.move_each(|mut m| m.move_hor(-1)),
            key!(Char('j')) => match self.0 {
                SelType::EndOfNl => helper.move_each(|mut m| {
                    m.move_ver(1);
                    let (p0, _) = m.search_fwd("\n", None).next().unzip();
                    if let Some(point) = p0.or(m.last_point()) {
                        m.move_to(point);
                    }
                }),
                SelType::UntilNL => helper.move_each(|mut m| {
                    m.move_ver(1);
                    let pre_nl = match m.char() {
                        '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                        _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                    };
                    if let Some((p, _)) = pre_nl {
                        m.move_to(p);
                    }
                }),
                SelType::Normal => helper.move_each(|mut m| m.move_ver(1)),
                _ => unreachable!(),
            },
            key!(Char('k')) => match self.0 {
                SelType::EndOfNl => helper.move_each(|mut m| {
                    m.move_ver(-1);
                    let (p0, _) = m.search_fwd("\n", None).next().unzip();
                    if let Some(point) = p0.or(m.last_point()) {
                        m.move_to(point);
                    }
                }),
                SelType::UntilNL => helper.move_each(|mut m| {
                    m.move_ver(-1);
                    let pre_nl = match m.char() {
                        '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                        _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                    };
                    if let Some((p, _)) = pre_nl {
                        m.move_to(p);
                    }
                }),
                SelType::Normal => helper.move_each(|mut m| m.move_ver(-1)),
                _ => unreachable!(),
            },
            key!(Char('l')) => helper.move_each(|mut m| m.move_hor(1)),

            key!(Char('H'), Mod::SHIFT) => select_and_move_each(&mut helper, Side::Left, 1),
            key!(Char('J'), Mod::SHIFT) => match self.0 {
                SelType::EndOfNl => helper.move_each(|mut m| {
                    m.move_ver(1);
                    let (p0, _) = m.search_fwd("\n", None).next().unzip();
                    if let Some(point) = p0.or(m.last_point()) {
                        m.move_to(point);
                    }
                }),
                SelType::UntilNL => helper.move_each(|mut m| {
                    m.move_ver(1);
                    let pre_nl = match m.char() {
                        '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                        _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                    };
                    if let Some((p, _)) = pre_nl {
                        m.move_to(p);
                    }
                }),
                SelType::Normal => select_and_move_each(&mut helper, Side::Bottom, 1),
                _ => unreachable!(),
            },
            key!(Char('K'), Mod::SHIFT) => match self.0 {
                SelType::EndOfNl => helper.move_each(|mut m| {
                    m.move_ver(-1);
                    let (p0, _) = m.search_fwd("\n", None).next().unzip();
                    if let Some(point) = p0.or(m.last_point()) {
                        m.move_to(point);
                    }
                }),
                SelType::UntilNL => helper.move_each(|mut m| {
                    m.move_ver(-1);
                    let pre_nl = match m.char() {
                        '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                        _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                    };
                    if let Some((p, _)) = pre_nl {
                        m.move_to(p);
                    }
                }),
                SelType::Normal => select_and_move_each(&mut helper, Side::Top, 1),
                _ => unreachable!(),
            },
            key!(Char('L'), Mod::SHIFT) => select_and_move_each(&mut helper, Side::Right, 1),

            key!(Left) => helper.move_each(|mut m| m.move_hor(-1)),
            key!(Down) => helper.move_each(|mut m| m.move_ver_wrapped(1)),
            key!(Up) => helper.move_each(|mut m| m.move_ver_wrapped(-1)),
            key!(Right) => helper.move_each(|mut m| m.move_hor(1)),
            key!(Left, Mod::SHIFT) => select_and_move_each_wrapped(&mut helper, Side::Left, 1),
            key!(Down, Mod::SHIFT) => select_and_move_each_wrapped(&mut helper, Side::Bottom, 1),
            key!(Up, Mod::SHIFT) => select_and_move_each_wrapped(&mut helper, Side::Top, 1),
            key!(Right, Mod::SHIFT) => select_and_move_each_wrapped(&mut helper, Side::Right, 1),

            ////////// Word and WORD selection keys.
            key!(Char('w'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each(|mut m| {
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
            }),
            key!(Char('e'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each(|mut m| {
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
            }),
            key!(Char('b'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each(|mut m| {
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
            }),

            key!(Char('W'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each(|mut m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }

                let points = m.search_fwd(word_and_space(mf, w_chars), None).next();
                if let Some((_, p1)) = points {
                    m.move_to(p1);
                    m.move_hor(-1);
                }
            }),
            key!(Char('E'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each(|mut m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }

                let points = m.search_fwd(space_and_word(mf, w_chars), None).next();
                if let Some((_, p1)) = points {
                    m.move_to(p1);
                    m.move_hor(-1);
                }
            }),
            key!(Char('B'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each(|mut m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }

                let points = m.search_rev(word_and_space(mf, w_chars), None).next();
                if let Some((p0, _)) = points {
                    m.move_to(p0);
                }
            }),

            ////////// Other selection keys.
            key!(Char('x')) => {
                self.0 = SelType::EndOfNl;
                helper.move_each(|mut m| {
                    if m.anchor_is_start() {
                        m.swap_ends()
                    }

                    let (_, p0) = m.search_rev("\n", None).next().unzip();
                    m.swap_ends();
                    m.move_to(p0.unwrap_or_default());

                    let (p1, _) = m.search_fwd("\n", None).next().unzip();
                    if let Some(p1) = p1.or(m.last_point()) {
                        m.set_anchor();
                        m.move_to(p1);
                    }
                })
            }
            key!(Char(char), mf)
                if let 'f' | 'F' | 't' | 'T' = char
                    && (mf.intersects(ALTSHIFT) || mf == Mod::NONE) =>
            {
                let sel_type = match (mf.contains(Mod::SHIFT), mf.contains(Mod::ALT)) {
                    (true, true) => SelType::ExtendRev,
                    (true, false) => SelType::Extend,
                    (false, true) => SelType::Reverse,
                    (false, false) => SelType::Normal,
                };

                mode::set::<U>(if let 'f' | 'F' = char {
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
            key!(Char(';'), Mod::ALT) => helper.move_each(|mut m| m.swap_ends()),
            key!(Char(';')) => helper.move_each(|mut m| m.unset_anchor()),
            key!(Char(')')) => helper.rotate_main(1),
            key!(Char('(')) => helper.rotate_main(-1),

            ////////// Text modifying keys.
            key!(Char('i')) => {
                helper.move_each(|mut m| {
                    if m.anchor_is_start() {
                        m.swap_ends();
                    }
                });
                mode::set::<U>(Insert);
            }
            key!(Char('a')) => {
                helper.move_each(|mut m| {
                    if !m.anchor_is_start() {
                        m.swap_ends();
                    }
                    m.move_hor(1);
                });
                mode::set::<U>(Insert);
            }
            key!(Char('c')) => {
                helper.edit_each(|e| e.replace(""));
                helper.move_each(|mut m| m.unset_anchor());
                mode::set::<U>(Insert);
            }
            key!(Char('d')) => {
                helper.edit_each(|e| e.replace(""));
                helper.move_each(|mut m| m.unset_anchor());
            }

            ////////// Cursor creation and destruction.
            key!(Char(',')) => helper.remove_extra_cursors(),
            key!(Char('C'), Mod::SHIFT) => helper.move_nth(helper.cursors().len() - 1, |mut m| {
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

            ////////// Temporary.
            key!(Char('q')) => panic!("Panicked on purpose"),

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

    fn send_key(
        &mut self,
        key: Event,
        widget: &RwData<Self::Widget>,
        area: &<U as Ui>::Area,
        cursors: &mut Cursors,
    ) {
        cursors.make_incl();
        let mut helper = EditHelper::new(widget, area, cursors);

        if let key!(Left | Down | Up | Right) = key {
            helper.move_each(|mut m| m.unset_anchor())
        }

        match key {
            key!(Char(char)) => {
                helper.edit_each(|e| e.insert(char));
                helper.move_each(|mut m| m.move_hor(1));
            }
            key!(Char(char), Mod::SHIFT) => {
                helper.edit_each(|e| e.insert(char));
                helper.move_each(|mut m| m.move_hor(1));
            }
            key!(Enter) => {
                helper.edit_each(|e| e.insert('\n'));
                helper.move_each(|mut m| m.move_hor(1));
            }
            key!(Backspace) => {
                let mut anchors = Vec::with_capacity(helper.cursors().len());
                helper.move_each(|mut m| {
                    anchors.push({
                        let c = m.caret();
                        m.unset_anchor().map(|a| match a > c {
                            true => a.char() as i32 - c.char() as i32,
                            false => a.char() as i32 - (c.char() as i32 - 1),
                        })
                    });
                    m.move_hor(-1);
                });
                let mut anchors = anchors.into_iter().cycle();
                helper.edit_each(|editor| editor.replace(""));
                helper.move_each(|mut m| {
                    if let Some(Some(diff)) = anchors.next() {
                        m.set_anchor();
                        m.move_hor(diff);
                        m.swap_ends()
                    }
                });
            }
            key!(Delete) => {
                let mut anchors = Vec::with_capacity(helper.cursors().len());
                helper.move_each(|mut m| {
                    let caret = m.caret();
                    anchors.push(m.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    m.set_anchor();
                    m.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                helper.edit_each(|editor| {
                    editor.replace("");
                });
                helper.move_each(|mut m| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        m.set_anchor();
                        m.move_to(anchor);
                        m.swap_ends()
                    } else {
                        m.unset_anchor();
                    }
                });
            }
            key!(Left, Mod::SHIFT) => select_and_move_each(&mut helper, Side::Left, 1),
            key!(Right, Mod::SHIFT) => select_and_move_each_wrapped(&mut helper, Side::Right, 1),
            key!(Up, Mod::SHIFT) => select_and_move_each_wrapped(&mut helper, Side::Top, 1),
            key!(Down, Mod::SHIFT) => select_and_move_each(&mut helper, Side::Bottom, 1),

            key!(Left) => helper.move_each(|mut m| m.move_hor(-1)),
            key!(Down) => helper.move_each(|mut m| m.move_ver_wrapped(1)),
            key!(Up) => helper.move_each(|mut m| m.move_ver_wrapped(-1)),
            key!(Right) => helper.move_each(|mut m| m.move_hor(1)),

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
            helper.move_each(|mut m| m.unset_anchor())
        } else {
            helper.move_each(|mut m| {
                if m.anchor().is_none() {
                    m.set_anchor()
                }
            })
        }

        match key {
            key!(Char('h')) => helper.move_each(|mut m| {
                let (_, p1) = m.search_rev("\n", None).next().unzip();
                m.move_to(p1.unwrap_or_default());
            }),
            key!(Char('j')) => helper.move_each(|mut m| m.move_ver(i32::MAX)),
            key!(Char('k')) => helper.move_each(|mut m| m.move_to_coords(0, 0)),
            key!(Char('l')) => helper.move_each(|mut m| {
                *sel_type = SelType::UntilNL;
                let pre_nl = match m.char() {
                    '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                    _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                };
                if let Some((p, _)) = pre_nl {
                    m.move_to(p);
                }
            }),
            key!(Char('i')) => helper.move_each(|mut m| {
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

    fn send_key(
        &mut self,
        key: Event,
        widget: &RwData<Self::Widget>,
        area: &<U as Ui>::Area,
        cursors: &mut Cursors,
    ) {
        cursors.make_incl();
        let mut helper = EditHelper::new(widget, area, cursors);
        let mut sel_type = self.sel_type();

        sel_type = match self {
            OneKey::GoTo(_) => self.match_goto::<(), U>(&mut helper, key),
            OneKey::Find(_) | OneKey::Until(_)
                if let key!(Char(char), Mod::SHIFT | Mod::NONE) = key =>
            {
                use SelType::*;
                helper.move_each(|mut m| {
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

fn select_and_move_each<S>(
    helper: &mut EditHelper<File, impl Area, S>,
    direction: Side,
    amount: usize,
) {
    helper.move_each(|mut m| {
        if m.anchor().is_none() {
            m.set_anchor()
        }
        match direction {
            Side::Top => m.move_ver(-(amount as i32)),
            Side::Bottom => m.move_ver(amount as i32),
            Side::Left => m.move_hor(-(amount as i32)),
            Side::Right => m.move_hor(amount as i32),
        }
    });
}

fn select_and_move_each_wrapped<S>(
    helper: &mut EditHelper<File, impl Area, S>,
    direction: Side,
    amount: usize,
) {
    helper.move_each(|mut m| {
        if m.anchor().is_none() {
            m.set_anchor();
        }
        if let Side::Top = direction {
            m.move_ver_wrapped(-(amount as i32))
        } else {
            m.move_ver_wrapped(amount as i32)
        }
    });
}

fn no_nl_windows<'a>(
    iter: impl Iterator<Item = (Point, char)> + 'a,
) -> impl Iterator<Item = ((Point, char), (Point, char))> + 'a {
    iter.map_windows(|[first, second]| (*first, *second))
        .skip_while(|((_, c0), (_, c1))| *c0 == '\n' || *c1 == '\n')
}

enum Side {
    Left,
    Right,
    Top,
    Bottom,
}

#[derive(Clone, Copy)]
enum SelType {
    UntilNL,
    EndOfNl,
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
    cursors: Cursors,
    info: <U::Area as Area>::PrintInfo,
}

impl<U: Ui> IncSearcher<U> for Select<U> {
    fn new(_: &RwData<File>, area: &<U as Ui>::Area, cursors: &mut Cursors) -> Self {
        Self {
            cursors: cursors.clone(),
            info: area.print_info(),
        }
    }

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &<U as Ui>::Area,
        cursors: &mut Cursors,
        searcher: duat_core::text::Searcher,
    ) {
        *cursors = self.cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, cursors, searcher);

        helper.move_each(|mut m| {
            if m.anchor_is_start() {
                m.swap_ends();
            }
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
    cursors: Cursors,
    info: <U::Area as Area>::PrintInfo,
}

impl<U: Ui> IncSearcher<U> for Split<U> {
    fn new(_: &RwData<File>, area: &<U as Ui>::Area, cursors: &mut Cursors) -> Self {
        Self {
            cursors: cursors.clone(),
            info: area.print_info(),
        }
    }

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &<U as Ui>::Area,
        cursors: &mut Cursors,
        searcher: duat_core::text::Searcher,
    ) {
        *cursors = self.cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, cursors, searcher);

        helper.move_each(|mut m| {
            if m.anchor_is_start() {
                m.swap_ends();
            }
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
