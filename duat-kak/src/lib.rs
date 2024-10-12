#![feature(let_chains, iter_map_windows, type_alias_impl_trait, if_let_guard)]

use std::{collections::HashMap, ops::RangeInclusive, sync::LazyLock};

use duat_core::{
    data::{Context, RwData, RwLock},
    forms::{self, Form},
    hooks::{self, Hookable},
    input::{
        Cursors, EditHelper, InputForFiles, InputMethod, KeyCode::*, KeyEvent as Event,
        KeyMod as Mod, Mover, key,
    },
    text::{Point, Text, err, text},
    ui::{Area, Ui},
    widgets::{File, IncSearch, RunCommands},
};

const ALTSHIFT: Mod = Mod::ALT.union(Mod::SHIFT);

pub struct KeyMap<U: Ui> {
    cursors: Cursors,
    mode: Mode,
    sel_type: SelType,
    searching: Option<(Cursors, <<U as Ui>::Area as Area>::PrintInfo)>,
}

impl<U: Ui> KeyMap<U> {
    pub fn new() -> Self {
        forms::set_weak("Mode", Form::new().green());
        KeyMap {
            cursors: Cursors::new_inclusive(),
            mode: Mode::Normal,
            sel_type: SelType::Normal,
            searching: None,
        }
    }

    pub fn mode(&self) -> &'static str {
        self.mode.generic_name()
    }

    pub fn precise_mode(&self) -> &'static str {
        self.mode.specific_name()
    }

    pub fn mode_fmt(&self) -> Text {
        text!([Mode] { self.mode.generic_name() })
    }

    pub fn precise_mode_fmt(&self) -> Text {
        text!([Mode] { self.mode.specific_name() })
    }

    /// Commands that are available in `Mode::Insert`.
    fn match_insert<S>(&mut self, helper: &mut EditHelper<File, impl Area, S>, event: Event) {
        if let key!(Left | Down | Up | Right) = event {
            helper.move_each(|m| m.unset_anchor())
        }

        match event {
            key!(Char(char)) => {
                helper.edit_on_each(|e| e.insert(char));
                helper.move_each(|m| m.move_hor(1));
            }
            key!(Char(char), Mod::SHIFT) => {
                helper.edit_on_each(|e| e.insert(char));
                helper.move_each(|m| m.move_hor(1));
            }
            key!(Enter) => {
                helper.edit_on_each(|e| e.insert('\n'));
                helper.move_each(|m| m.move_hor(1));
            }
            key!(Backspace) => {
                let mut anchors = Vec::with_capacity(helper.cursors_len());
                helper.move_each(|m| {
                    anchors.push({
                        let c = m.caret();
                        m.unset_anchor().map(|a| match a > c {
                            true => a.char() as isize - c.char() as isize,
                            false => a.char() as isize - (c.char() as isize - 1),
                        })
                    });
                    m.move_hor(-1);
                });
                let mut anchors = anchors.into_iter().cycle();
                helper.edit_on_each(|editor| editor.replace(""));
                helper.move_each(|m| {
                    if let Some(Some(diff)) = anchors.next() {
                        m.set_anchor();
                        m.move_hor(diff);
                        m.swap_ends()
                    }
                });
            }
            key!(Delete) => {
                let mut anchors = Vec::with_capacity(helper.cursors_len());
                helper.move_each(|m| {
                    let caret = m.caret();
                    anchors.push(m.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    m.set_anchor();
                    m.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                helper.edit_on_each(|editor| {
                    editor.replace("");
                });
                helper.move_each(|m| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        m.set_anchor();
                        m.move_to(anchor);
                        m.swap_ends()
                    } else {
                        m.unset_anchor();
                    }
                });
            }
            key!(Left, Mod::SHIFT) => select_and_move_each(helper, Side::Left, 1),
            key!(Right, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Right, 1),
            key!(Up, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Top, 1),
            key!(Down, Mod::SHIFT) => select_and_move_each(helper, Side::Bottom, 1),

            key!(Left) => helper.move_each(|m| m.move_hor(-1)),
            key!(Down) => helper.move_each(|m| m.move_ver_wrapped(1)),
            key!(Up) => helper.move_each(|m| m.move_ver_wrapped(-1)),
            key!(Right) => helper.move_each(|m| m.move_hor(1)),

            key!(Esc) => {
                helper.new_moment();
                self.mode = Mode::Normal;
                hooks::trigger::<OnModeChange>((Mode::Insert, Mode::Normal))
            }
            _ => {}
        }
    }

    /// Commands that are available in `Mode::Normal`.
    fn match_normal<S>(
        &mut self,
        helper: &mut EditHelper<File, U::Area, S>,
        event: Event,
        context: Context<U>,
    ) {
        if let key!(Char('h' | 'j' | 'k' | 'l' | 'w' | 'b' | 'e') | Down | Up) = event {
            helper.move_each(|m| m.unset_anchor())
        }

        match event {
            ////////// hjkl and arrow selection keys.
            key!(Char('h')) => helper.move_each(|m| m.move_hor(-1)),
            key!(Char('j')) => match self.sel_type {
                SelType::EndOfNl => helper.move_each(|m| {
                    m.move_ver(1);
                    let (p0, _) = m.search("\n", None).next().unzip();
                    if let Some(point) = p0.or(m.last_point()) {
                        m.move_to(point);
                    }
                }),
                SelType::UntilNL => helper.move_each(|m| {
                    m.move_ver(1);
                    let pre_nl = match m.char() {
                        '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                        _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                    };
                    if let Some((p, _)) = pre_nl {
                        m.move_to(p);
                    }
                }),
                SelType::Normal => helper.move_each(|m| m.move_ver(1)),
                _ => unreachable!(),
            },
            key!(Char('k')) => match self.sel_type {
                SelType::EndOfNl => helper.move_each(|m| {
                    m.move_ver(-1);
                    let (p0, _) = m.search("\n", None).next().unzip();
                    if let Some(point) = p0.or(m.last_point()) {
                        m.move_to(point);
                    }
                }),
                SelType::UntilNL => helper.move_each(|m| {
                    m.move_ver(-1);
                    let pre_nl = match m.char() {
                        '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                        _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                    };
                    if let Some((p, _)) = pre_nl {
                        m.move_to(p);
                    }
                }),
                SelType::Normal => helper.move_each(|m| m.move_ver(-1)),
                _ => unreachable!(),
            },
            key!(Char('l')) => helper.move_each(|m| m.move_hor(1)),

            key!(Char('H'), Mod::SHIFT) => select_and_move_each(helper, Side::Left, 1),
            key!(Char('J'), Mod::SHIFT) => match self.sel_type {
                SelType::EndOfNl => helper.move_each(|m| {
                    m.move_ver(1);
                    let (p0, _) = m.search("\n", None).next().unzip();
                    if let Some(point) = p0.or(m.last_point()) {
                        m.move_to(point);
                    }
                }),
                SelType::UntilNL => helper.move_each(|m| {
                    m.move_ver(1);
                    let pre_nl = match m.char() {
                        '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                        _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                    };
                    if let Some((p, _)) = pre_nl {
                        m.move_to(p);
                    }
                }),
                SelType::Normal => select_and_move_each(helper, Side::Bottom, 1),
                _ => unreachable!(),
            },
            key!(Char('K'), Mod::SHIFT) => match self.sel_type {
                SelType::EndOfNl => helper.move_each(|m| {
                    m.move_ver(-1);
                    let (p0, _) = m.search("\n", None).next().unzip();
                    if let Some(point) = p0.or(m.last_point()) {
                        m.move_to(point);
                    }
                }),
                SelType::UntilNL => helper.move_each(|m| {
                    m.move_ver(-1);
                    let pre_nl = match m.char() {
                        '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                        _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                    };
                    if let Some((p, _)) = pre_nl {
                        m.move_to(p);
                    }
                }),
                SelType::Normal => select_and_move_each(helper, Side::Top, 1),
                _ => unreachable!(),
            },
            key!(Char('L'), Mod::SHIFT) => select_and_move_each(helper, Side::Right, 1),

            key!(Left) => helper.move_each(|m| m.move_hor(-1)),
            key!(Down) => helper.move_each(|m| m.move_ver_wrapped(1)),
            key!(Up) => helper.move_each(|m| m.move_ver_wrapped(-1)),
            key!(Right) => helper.move_each(|m| m.move_hor(1)),
            key!(Left, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Left, 1),
            key!(Down, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Bottom, 1),
            key!(Up, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Top, 1),
            key!(Right, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Right, 1),

            ////////// Word and WORD selection keys.
            key!(Char('w'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each(|m| {
                let init = no_nl_windows(m.iter()).next();

                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, m) == Category::of(c1, m) {
                        m.move_to(p0);
                    } else {
                        m.move_to(p1);
                    }

                    let (_, p1) = m.search(word_and_space(m, mf), None).next().unzip();
                    if let Some(p1) = p1 {
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                    }
                };
            }),
            key!(Char('e'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each(|m| {
                let init = no_nl_windows(m.iter()).next();

                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, m) == Category::of(c1, m) {
                        m.move_to(p0);
                    } else {
                        m.move_to(p1);
                    }

                    let (_, p1) = m.search(space_and_word(m, mf), None).next().unzip();
                    if let Some(p1) = p1 {
                        m.set_anchor();
                        m.move_to(p1);
                        m.move_hor(-1);
                    }
                };
            }),
            key!(Char('b'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each(|m| {
                let init = {
                    let iter = [(m.caret(), m.char())].into_iter().chain(m.iter_rev());
                    no_nl_windows(iter).next()
                };

                if let Some(((_, c1), (_, c0))) = init {
                    let points = m.search_rev(word_and_space(m, mf), None).next();
                    if let Some((p0, _)) = points {
                        if Category::of(c0, m) != Category::of(c1, m) {
                            m.move_hor(-1);
                        }
                        m.set_anchor();
                        m.move_to(p0);
                    }
                };
            }),

            key!(Char('W'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each(|m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }

                let points = m.search(word_and_space(m, mf), None).next();
                if let Some((_, p1)) = points {
                    m.move_to(p1);
                    m.move_hor(-1);
                }
            }),
            key!(Char('E'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each(|m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }

                let points = m.search(space_and_word(m, mf), None).next();
                if let Some((_, p1)) = points {
                    m.move_to(p1);
                    m.move_hor(-1);
                }
            }),
            key!(Char('B'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each(|m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }

                let points = m.search_rev(word_and_space(m, mf), None).next();
                if let Some((p0, _)) = points {
                    m.move_to(p0);
                }
            }),

            ////////// Other selection keys.
            key!(Char('x')) => {
                self.sel_type = SelType::EndOfNl;
                helper.move_each(|m| {
                    m.set_caret_on_start();

                    let (p0, _) = m.search_rev("\n", None).next().unwrap_or_default();
                    m.set_caret_on_end();
                    m.move_to(p0);

                    let (p1, _) = m.search("\n", None).next().unzip();
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
                self.sel_type = match (mf.contains(Mod::SHIFT), mf.contains(Mod::ALT)) {
                    (true, true) => SelType::ExtendRev,
                    (true, false) => SelType::Extend,
                    (false, true) => SelType::Reverse,
                    (false, false) => SelType::Normal,
                };
                self.mode = Mode::OneKey(if let 'f' | 'F' = char {
                    "find"
                } else {
                    "until"
                });
                hooks::trigger::<OnModeChange>((Mode::Normal, self.mode));
            }
            key!(Char('%')) => helper.move_main(|m| {
                m.move_to(Point::default());
                m.set_anchor();
                m.move_to(m.last_point().unwrap())
            }),
            key!(Char(';'), Mod::ALT) => helper.move_each(|m| m.swap_ends()),
            key!(Char(';')) => helper.move_each(|m| m.unset_anchor()),
            key!(Char(')')) => helper.rotate_main_fwd(),
            key!(Char('(')) => helper.rotate_main_rev(),

            ////////// Text modifying keys.
            key!(Char('i')) => {
                helper.move_each(|m| m.set_caret_on_start());
                self.mode = Mode::Insert;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
            }
            key!(Char('a')) => {
                helper.move_each(|m| m.set_caret_on_end());
                self.mode = Mode::Insert;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
            }
            key!(Char('c')) => {
                helper.edit_on_each(|e| e.replace(""));
                helper.move_each(|m| m.unset_anchor());
                self.mode = Mode::Insert;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
            }
            key!(Char('d')) => {
                helper.edit_on_each(|e| e.replace(""));
                helper.move_each(|m| m.unset_anchor());
            }

            ////////// Other mode changing keys.
            key!(Char(':')) => {
                context.set_cmd_mode::<RunCommands<U>>();
                self.mode = Mode::Other("command");
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Other("command")));
            }
            key!(Char('G'), Mod::SHIFT) => {
                self.mode = Mode::OneKey("go to");
                self.sel_type = SelType::Extend;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::OneKey("go to")));
            }
            key!(Char('g')) => {
                self.mode = Mode::OneKey("go to");
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::OneKey("go to")));
            }
            key!(Char('/')) => {
                context.set_cmd_mode::<IncSearch<U>>();
                self.mode = Mode::Other("search");
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Other("search")));
            }

            ////////// Temporary.
            key!(Char('q')) => panic!("Panicked on purpose"),

            ////////// History manipulation.
            key!(Char('u')) => helper.undo(),
            key!(Char('U'), Mod::SHIFT) => helper.redo(),
            _ => {}
        }
    }

    /// Commands that are available in `Mode::GoTo`.
    fn match_goto<S>(
        &mut self,
        helper: &mut EditHelper<File, U::Area, S>,
        event: Event,
        context: Context<U>,
    ) {
        static LAST_FILE: LazyLock<RwData<Option<String>>> = LazyLock::new(RwData::default);
        let last_file = LAST_FILE.read().clone();
        let cur_name = context.cur_file().unwrap().name();

        if let key!(Char('h' | 'j' | 'k' | 'l' | 'i' | 't' | 'b' | 'c' | '.')) = event
            && let SelType::Normal = self.sel_type
        {
            helper.move_each(|m| m.unset_anchor())
        } else {
            helper.move_each(|m| {
                if m.anchor().is_none() {
                    m.set_anchor()
                }
            })
        }

        match event {
            key!(Char('h')) => helper.move_each(|m| {
                let (_, p1) = m.search_rev("\n", None).next().unzip();
                m.move_to(p1.unwrap_or_default());
            }),
            key!(Char('j')) => helper.move_each(|m| m.move_ver(isize::MAX)),
            key!(Char('k')) => helper.move_each(|m| m.move_to_coords(0, 0)),
            key!(Char('l')) => helper.move_each(|m| {
                self.sel_type = SelType::UntilNL;
                let pre_nl = match m.char() {
                    '\n' => m.iter_rev().take_while(|(_, char)| *char != '\n').next(),
                    _ => m.iter().take_while(|(_, char)| *char != '\n').last(),
                };
                if let Some((p, _)) = pre_nl {
                    m.move_to(p);
                }
            }),
            key!(Char('i')) => helper.move_each(|m| {
                let (_, p1) = m.search_rev("(^|\n)[ \t]*", None).next().unzip();
                if let Some(p1) = p1 {
                    m.move_to(p1);

                    let points = m.search("[^ \t]", None).next();
                    if let Some((p0, _)) = points {
                        m.move_to(p0)
                    }
                }
            }),

            ////////// File change keys.
            key!(Char('a')) => {
                if let Some(file) = last_file
                    && context.run_cmd_notify(format!("b {file}")).is_ok()
                {
                    *LAST_FILE.write() = Some(cur_name);
                } else {
                    context.notify(err!("There is no previous file."))
                }
            }
            key!(Char('n')) => {
                if context.run_cmd_notify("next-file").is_ok() {
                    *LAST_FILE.write() = Some(cur_name);
                }
            }
            key!(Char('N'), Mod::SHIFT) => {
                if context.run_cmd_notify("prev-file").is_ok() {
                    *LAST_FILE.write() = Some(cur_name);
                }
            }
            Event { code, .. } => {
                let code = format!("{code:?}");
                context.notify(err!("Key " [*a] code [] " not mapped on " [*a] "go to" [] "."))
            }
        }
    }
}

impl<U: Ui> Default for KeyMap<U> {
    fn default() -> Self {
        Self::new()
    }
}

impl<U> InputMethod<U> for KeyMap<U>
where
    U: Ui,
{
    type Widget = File;

    fn send_key(
        &mut self,
        event: Event,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        context: Context<U>,
    ) {
        let mut cursors = std::mem::take(&mut self.cursors);
        let mut helper = EditHelper::new(widget, area, &mut cursors);

        match self.mode {
            Mode::Insert => self.match_insert(&mut helper, event),
            Mode::Normal => {
                let (code, mf) = (&event.code, &event.modifiers);
                let end_of_line = [Char('j'), Char('J'), Char('k'), Char('K'), Down, Up];
                if !end_of_line.contains(code) || (*mf != Mod::SHIFT && *mf != Mod::NONE) {
                    self.sel_type = SelType::Normal;
                }
                self.match_normal(&mut helper, event, context);
            }
            Mode::OneKey(one_key) => {
                match one_key {
                    "go to" => self.match_goto(&mut helper, event, context),
                    "find" | "until" if let key!(Char(char), Mod::SHIFT | Mod::NONE) = event => {
                        use SelType::*;
                        helper.move_each(|m| {
                            let cur = m.caret();
                            let (points, back) = match self.sel_type {
                                Reverse | ExtendRev => {
                                    (m.search_rev(char, None).find(|(p, _)| *p != cur), 1)
                                }
                                Normal | Extend => {
                                    (m.search(char, None).find(|(p, _)| *p != cur), -1)
                                }
                                _ => unreachable!(),
                            };

                            if let Some((p0, _)) = points
                                && p0 != m.caret()
                            {
                                let is_extension = !matches!(self.sel_type, Extend | ExtendRev);
                                if is_extension || m.anchor().is_none() {
                                    m.set_anchor();
                                }
                                m.move_to(p0);
                                if one_key == "until" {
                                    m.move_hor(back);
                                }
                            } else {
                                context.notify(err!("Char " [*a] {char} [] " not found."))
                            }
                        })
                    }
                    _ => {}
                }
                self.mode = Mode::Normal;
                if !matches!(self.sel_type, SelType::UntilNL) {
                    self.sel_type = SelType::Normal;
                }
                hooks::trigger::<OnModeChange>((Mode::OneKey(one_key), Mode::Normal));
            }
            Mode::Other(_) => {
                unreachable!("The editor is not supposed to be focused if this is the current mode")
            }
        }

        self.cursors = cursors;
    }

    fn cursors(&self) -> Option<&Cursors> {
        self.searching
            .as_ref()
            .map(|(c, _)| c)
            .or(Some(&self.cursors))
    }

    fn on_focus(&mut self, _area: &U::Area)
    where
        Self: Sized,
    {
        let prev = self.mode;
        self.mode = Mode::Normal;
        hooks::trigger::<OnModeChange>((prev, Mode::Normal));
    }

    fn begin_inc_search(&mut self, _file: &RwData<File>, area: &U::Area, _context: Context<U>) {
        self.searching = Some((self.cursors.clone(), area.get_print_info()));
    }

    fn end_inc_search(&mut self, _file: &RwData<File>, _area: &U::Area, _context: Context<U>) {
        self.cursors = self.searching.take().map(|(c, _)| c).unwrap();
    }

    fn search_inc(
        &mut self,
        file: &RwData<File>,
        area: &<U as Ui>::Area,
        _context: Context<U>,
        searcher: duat_core::text::Searcher,
    ) where
        Self: Sized,
    {
        let mut cursors = self.cursors.clone();
        if searcher.is_empty() {
            let (s_cursors, info) = self.searching.as_mut().unwrap();
            *s_cursors = cursors;
            area.set_print_info(info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, &mut cursors, searcher);

        helper.move_each(|m| {
            let next = m.search_inc(None).next();
            if let Some((p0, p1)) = next {
                m.move_to(p0);
                m.set_anchor();
                m.move_to(p1);
                m.move_hor(-1);
            } else if m.is_main() {
                area.set_print_info(self.searching.as_ref().unwrap().1.clone());
            }
        });

        drop(helper);
        self.searching.as_mut().map(|(c, _)| *c = cursors);
    }
}

impl<U: Ui> InputForFiles<U> for KeyMap<U> {
    fn set_cursors(&mut self, mut cursors: Cursors) {
        cursors.set_inclusive();
        self.cursors = cursors;
    }
}

impl<U: Ui> Clone for KeyMap<U> {
    fn clone(&self) -> Self {
        Self {
            cursors: self.cursors.clone(),
            mode: self.mode.clone(),
            sel_type: self.sel_type.clone(),
            searching: None,
        }
    }
}

fn select_and_move_each<S>(
    helper: &mut EditHelper<File, impl Area, S>,
    direction: Side,
    amount: usize,
) {
    helper.move_each(|m| {
        if m.anchor().is_none() {
            m.set_anchor()
        }
        match direction {
            Side::Top => m.move_ver(-(amount as isize)),
            Side::Bottom => m.move_ver(amount as isize),
            Side::Left => m.move_hor(-(amount as isize)),
            Side::Right => m.move_hor(amount as isize),
        }
    });
}

fn select_and_move_each_wrapped<S>(
    helper: &mut EditHelper<File, impl Area, S>,
    direction: Side,
    amount: usize,
) {
    helper.move_each(|m| {
        if m.anchor().is_none() {
            m.set_anchor();
        }
        if let Side::Top = direction {
            m.move_ver_wrapped(-(amount as isize))
        } else {
            m.move_ver_wrapped(amount as isize)
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

#[derive(Default, Clone, Copy, PartialEq)]
pub enum Mode {
    Insert,
    #[default]
    Normal,
    OneKey(&'static str),
    Other(&'static str),
}

impl Mode {
    fn generic_name(&self) -> &'static str {
        match self {
            Mode::Insert => "insert",
            Mode::Normal => "normal",
            Mode::Other(other) => other,
            Mode::OneKey(_) => "one key",
        }
    }

    fn specific_name(&self) -> &'static str {
        match self {
            Mode::Insert => "insert",
            Mode::Normal => "normal",
            Mode::Other(widget) => widget,
            Mode::OneKey(one_key) => one_key,
        }
    }
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

/// Triggers whenever the mode changes
///
/// Arguments:
/// * The previous [`Mode`]
/// * The current [`Mode`]
pub struct OnModeChange;

impl Hookable for OnModeChange {
    type Args = (Mode, Mode);
}

fn word_and_space<S>(m: &Mover<impl Area, S>, mf: Mod) -> &'static str {
    const WORD: &str = "[^ \t\n]*[ \t]*";
    static UNWS: RegexStrs = LazyLock::new(RwLock::default);

    let mut unws = UNWS.write();
    if let Some(word) = unws.get(m.w_chars().ranges()) {
        if mf.contains(Mod::ALT) { WORD } else { word }
    } else {
        let cat = w_char_cat(m.w_chars().ranges());
        let word = format!("([{cat}]+|[^{cat} \t\n]+)[ \t]*|[ \t\n]+").leak();

        unws.insert(m.w_chars().ranges(), word);
        if mf.contains(Mod::ALT) { WORD } else { word }
    }
}

fn space_and_word<S>(m: &Mover<impl Area, S>, mf: Mod) -> &'static str {
    const WORD: &str = "[ \t]*[^ \t\n]*";
    static EOWS: RegexStrs = LazyLock::new(RwLock::default);

    let mut eows = EOWS.write();
    if let Some(word) = eows.get(m.w_chars().ranges()) {
        if mf.contains(Mod::ALT) { WORD } else { word }
    } else {
        let cat = w_char_cat(m.w_chars().ranges());
        let word = format!("[ \t\n]*([{cat}]+|[^{cat} \t\n]+)|[ \t\n]+").leak();

        eows.insert(m.w_chars().ranges(), word);
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

type RegexStrs = LazyLock<RwLock<HashMap<&'static [RangeInclusive<char>], &'static str>>>;

#[derive(PartialEq, Eq)]
enum Category {
    Word,
    Special,
    Space,
}

impl Category {
    fn of<S>(char: char, m: &Mover<impl Area, S>) -> Self {
        if m.w_chars().contains(char) {
            Category::Word
        } else if [' ', '\t', '\n'].contains(&char) {
            Category::Space
        } else {
            Category::Special
        }
    }
}
