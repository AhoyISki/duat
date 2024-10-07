#![feature(let_chains, iter_map_windows, type_alias_impl_trait, if_let_guard)]

use std::{iter::Peekable, sync::LazyLock};

use duat_core::{
    data::{Context, RwData},
    forms::{self, Form},
    hooks::{self, Hookable},
    input::{
        key, Cursors, EditHelper, InputForFiles, InputMethod, KeyCode::*, KeyEvent as Event,
        KeyMod as Mod,
    },
    text::{err, text, CharSet, Point, Text, WordChars},
    ui::{Area, Ui},
    widgets::File,
};

const ALTSHIFT: Mod = Mod::ALT.union(Mod::SHIFT);

#[derive(Clone)]
pub struct KeyMap {
    cursors: Cursors,
    mode: Mode,
    sel_type: SelType,
}

impl KeyMap {
    pub fn new() -> Self {
        forms::set_weak("Mode", Form::new().green());
        KeyMap {
            cursors: Cursors::new_inclusive(),
            mode: Mode::Normal,
            sel_type: SelType::Normal,
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
    fn match_insert(&mut self, helper: &mut EditHelper<File, impl Area>, event: Event) {
        if let key!(Left | Down | Up | Right) = event {
            helper.move_each(|m| m.unset_anchor())
        }

        match event {
            key!(Char(char)) => {
                helper.edit_on_each(|editor| editor.insert(char));
                helper.move_each(|m| m.move_hor(1));
            }
            key!(Char(char), Mod::SHIFT) => {
                helper.edit_on_each(|editor| editor.insert(char));
                helper.move_each(|m| m.move_hor(1));
            }
            key!(Enter) => {
                helper.edit_on_each(|editor| editor.insert('\n'));
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
    fn match_normal<U: Ui>(
        &mut self,
        helper: &mut EditHelper<File, U::Area>,
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
                    let (p, _) = m.search('\n', None).next().unzip().0.unzip();
                    if let Some(point) = p.or(m.last_point()) {
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
                    let (p, _) = m.search('\n', None).next().unzip().0.unzip();
                    if let Some(point) = p.or(m.last_point()) {
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
                    let (p, _) = m.search('\n', None).next().unzip().0.unzip();
                    if let Some(point) = p.or(m.last_point()) {
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
                    let (p, _) = m.search('\n', None).next().unzip().0.unzip();
                    if let Some(point) = p.or(m.last_point()) {
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
                let mut chars = peekable_no_nl_windows(m.iter());

                if let Some(((p0, c0), (p1, c1))) = chars.next() {
                    let (p0, cat) = word_point_and_cat(c0, c1, p0, p1, m.w_chars(), mf);
                    let (p1, ..) = {
                        let mut chars = chars.map(|(_, second)| second).peekable();
                        let (p1, c1, ..) = end_of_cat(&mut chars, cat, (p1, c1));
                        end_of_cat(&mut chars, [' ', '\t'], (p1, c1))
                    };

                    m.move_to(p0);
                    m.set_anchor();
                    m.move_to(p1);
                };
            }),
            key!(Char('e'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each(|m| {
                let mut chars = peekable_no_nl_windows(m.iter());

                if let Some(&((p0, c0), (p1, c1))) = chars.peek() {
                    let (p0, _) = word_point_and_cat(c0, c1, p0, p1, m.w_chars(), mf);
                    let p1 = {
                        let mut chars = chars.map(|(_, second)| second).peekable();
                        let (p1, c1, miss) = end_of_cat(&mut chars, [' ', '\t'], (p1, c1));
                        miss.map(|char| {
                            end_of_cat(&mut chars, CharCat::of(char, m.w_chars(), mf), (p1, c1)).0
                        })
                        .unwrap_or(p1)
                    };

                    m.move_to(p0);
                    m.set_anchor();
                    m.move_to(p1);
                };
            }),
            key!(Char('b'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each(|m| {
                let mut chars = {
                    let iter = [(m.caret(), m.char())].into_iter().chain(m.iter_rev());
                    peekable_no_nl_windows(iter)
                };

                if let Some(&((p1, c1), (p0, c0))) = chars.peek() {
                    let (p1, _) = word_point_and_cat(c1, c0, p1, p0, m.w_chars(), mf);
                    let p0 = {
                        let mut chars = chars.map(|(_, first)| first).peekable();
                        let (p0, c0, miss) = end_of_cat(&mut chars, [' ', '\t'], (p0, c0));
                        miss.map(|char| {
                            end_of_cat(&mut chars, CharCat::of(char, m.w_chars(), mf), (p0, c0)).0
                        })
                        .unwrap_or(p0)
                    };

                    m.move_to(p1);
                    m.set_anchor();
                    m.move_to(p0);
                };
            }),

            key!(Char('W'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each(|m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }
                let mut chars = m.iter().skip(1).skip_while(|(_, char)| *char == '\n');

                if let Some((p, char)) = chars.next() {
                    let cat = CharCat::of(char, m.w_chars(), mf);
                    let (p, ..) = {
                        let mut chars = chars.peekable();
                        let (p, char, _) = end_of_cat(&mut chars, cat, (p, char));
                        end_of_cat(&mut chars, [' ', '\t'], (p, char))
                    };

                    m.move_to(p);
                };
            }),
            key!(Char('E'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each(|m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }
                let mut chars = m
                    .iter()
                    .skip(1)
                    .skip_while(|(_, char)| [' ', '\t', '\n'].matches(*char));

                if let Some((point, char)) = chars.next() {
                    let cat = CharCat::of(char, m.w_chars(), mf);
                    let (point, ..) = {
                        let mut chars = chars.peekable();
                        end_of_cat(&mut chars, cat, (point, char))
                    };

                    m.move_to(point);
                };
            }),
            key!(Char('B'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each(|m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }
                let mut chars = m
                    .iter_rev()
                    .skip_while(|(_, char)| [' ', '\t', '\n'].matches(*char));

                if let Some((point, char)) = chars.next() {
                    let cat = CharCat::of(char, m.w_chars(), mf);
                    let (point, ..) = {
                        let mut chars = chars.peekable();
                        end_of_cat(&mut chars, cat, (point, char))
                    };

                    m.move_to(point);
                };
            }),

            ////////// Other selection keys.
            key!(Char('x')) => {
                self.sel_type = SelType::EndOfNl;
                helper.move_each(|m| {
                    m.set_caret_on_start();

                    let p0 = {
                        let points = m.search_rev('\n', None).next().unzip().0;
                        points.unzip().0.unwrap_or_default()
                    };
                    m.set_caret_on_end();
                    m.move_to(p0);

                    let (p1, _) = m.search('\n', None).next().unzip().0.unzip();
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
                helper.edit_on_each(|editor| editor.replace(""));
                helper.move_each(|m| m.unset_anchor());
                self.mode = Mode::Insert;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
            }
            key!(Char('d')) => {
                helper.edit_on_each(|editor| editor.replace(""));
                helper.move_each(|m| m.unset_anchor());
            }

            ////////// Other mode changing keys.
            key!(Char(':')) => {
                if context.commands.run("switch-to CommandLine<Ui>").is_ok() {
                    context
                        .commands
                        .run("set-cmd-mode RunCommands<Ui>")
                        .unwrap();
                    self.mode = Mode::Other("command");
                    hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Other("command")));
                }
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

            ////////// Temporary.
            key!(Char('q')) => panic!("Panicked on purpose"),

            ////////// History manipulation.
            key!(Char('u')) => helper.undo(),
            key!(Char('U'), Mod::SHIFT) => helper.redo(),
            _ => {}
        }
    }

    /// Commands that are available in `Mode::GoTo`.
    fn match_goto<U: Ui>(
        &mut self,
        helper: &mut EditHelper<File, U::Area>,
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
                let (_, p1) = m.search('\n', None).next().unzip().0.unzip();
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
                let mut first_char = None;
                m.iter_rev()
                    .inspect(|(p, char)| {
                        if CharCat::Space.not().matches(*char) {
                            first_char = Some(*p)
                        }
                    })
                    .find(|(_, char)| *char == '\n');

                if let Some(point) = first_char {
                    m.move_to(point);
                } else if let Some(((p0, _), _)) =
                    m.search(CharCat::Space.not().or('\n'), None).next()
                {
                    m.move_to(p0);
                }
            }),

            ////////// File change keys.
            key!(Char('a')) => {
                if let Some(file) = last_file
                    && context.commands.run_notify(format!("b {file}")).is_ok()
                {
                    *LAST_FILE.write() = Some(cur_name);
                } else {
                    context.notify(err!("There is no previous file."))
                }
            }
            key!(Char('n')) => {
                if context.commands.run_notify("next-file").is_ok() {
                    *LAST_FILE.write() = Some(cur_name);
                }
            }
            key!(Char('N'), Mod::SHIFT) => {
                if context.commands.run_notify("prev-file").is_ok() {
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

impl Default for KeyMap {
    fn default() -> Self {
        Self::new()
    }
}

impl<U> InputMethod<U> for KeyMap
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
                                    (m.search_rev(char, None).find(|(p, _)| p.0 != cur), 1)
                                }
                                Normal | Extend => {
                                    (m.search(char, None).find(|(p, _)| p.0 != cur), -1)
                                }
                                _ => unreachable!(),
                            };

                            if let Some(((point, _), _)) = points
                                && point != m.caret()
                            {
                                let is_extension = !matches!(self.sel_type, Extend | ExtendRev);
                                if is_extension || m.anchor().is_none() {
                                    m.set_anchor();
                                }
                                m.move_to(point);
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

        drop(helper);
        self.cursors = cursors;
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }

    fn on_focus(&mut self, _area: &U::Area)
    where
        Self: Sized,
    {
        let prev = self.mode;
        self.mode = Mode::Normal;
        hooks::trigger::<OnModeChange>((prev, Mode::Normal));
    }
}

impl<U> InputForFiles<U> for KeyMap
where
    U: Ui,
{
    fn set_cursors(&mut self, mut cursors: Cursors) {
        cursors.set_inclusive();
        self.cursors = cursors;
    }
}

fn select_and_move_each(helper: &mut EditHelper<File, impl Area>, direction: Side, amount: usize) {
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

fn select_and_move_each_wrapped(
    helper: &mut EditHelper<File, impl Area>,
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

fn end_of_cat(
    chars: &mut Peekable<impl Iterator<Item = (Point, char)>>,
    char_set: impl CharSet,
    mut last: (Point, char),
) -> (Point, char, Option<char>) {
    let mut mismatch = None;
    let mut entry = chars.peek();
    while let Some(&(p, char)) = entry.take()
        && char != '\n'
    {
        if char_set.matches(char) {
            chars.next();
            entry = chars.peek();
            last = (p, char);
        } else {
            mismatch = Some(char)
        }
    }
    (last.0, last.1, mismatch)
}

fn peekable_no_nl_windows<'a>(
    iter: impl Iterator<Item = (Point, char)> + 'a,
) -> Peekable<impl Iterator<Item = ((Point, char), (Point, char))> + 'a> {
    iter.map_windows(|[first, second]| (*first, *second))
        .skip_while(|(_, (_, c1))| *c1 == '\n')
        .peekable()
}

/// Returns an appropriate [`Point`] and [`CharCat`] for a "word" like
/// command
///
/// If the categories of the two characters differ, the first point
/// must be shifted forwards once.
fn word_point_and_cat(
    c0: char,
    c1: char,
    p0: Point,
    p1: Point,
    w_chars: &WordChars,
    mf: Mod,
) -> (Point, CharCat) {
    let cat0 = CharCat::of(c0, w_chars, mf);
    let cat1 = CharCat::of(c1, w_chars, mf);
    (if cat0 == cat1 && c0 != '\n' { p0 } else { p1 }, cat1)
}

enum CharCat<'a> {
    Word(&'a WordChars),
    Space,
    Other(&'a WordChars),
    NotSpace,
}

impl<'a> CharCat<'a> {
    fn of(char: char, w_chars: &'a WordChars, mf: Mod) -> Self {
        if [' ', '\t', '\n'].matches(char) {
            Self::Space
        } else if mf.contains(Mod::ALT) {
            Self::NotSpace
        } else if w_chars.matches(char) {
            Self::Word(w_chars)
        } else {
            Self::Other(w_chars)
        }
    }
}

impl CharSet for CharCat<'_> {
    fn matches(&self, char: char) -> bool {
        match self {
            CharCat::Word(w_chars) => w_chars.matches(char),
            CharCat::Space => [' ', '\t', '\n'].matches(char),
            CharCat::Other(w_chars) => w_chars.or([' ', '\t', '\n']).not().matches(char),
            CharCat::NotSpace => [' ', '\t', '\n'].not().matches(char),
        }
    }
}

impl<'a> PartialEq for CharCat<'a> {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (CharCat::Word(_), CharCat::Word(_))
                | (CharCat::Space, CharCat::Space)
                | (CharCat::Other(_), CharCat::Other(_))
        )
    }
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
pub struct OnModeChange {}

impl Hookable for OnModeChange {
    type Args<'args> = (Mode, Mode);
}
