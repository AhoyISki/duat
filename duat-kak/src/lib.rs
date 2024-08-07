#![feature(let_chains, iter_map_windows, type_alias_impl_trait, if_let_guard)]

use std::{fmt::Display, iter::Peekable, sync::LazyLock};

use duat_core::{
    data::{Context, RwData},
    hooks::{self, Hookable},
    input::{
        key::{key, Code::*, Event, Mod},
        Cursors, EditHelper, InputMethod,
    },
    palette::{self, Form},
    text::{err, text, CharSet, Point, Text, WordChars},
    ui::Ui,
    widgets::File,
};

const ALTSHIFT: Mod = Mod::ALT.union(Mod::SHIFT);

#[derive(Clone)]
pub struct KeyMap {
    cursors: Option<Cursors>,
    mode: Mode,
}

impl KeyMap {
    pub fn new() -> Self {
        palette::set_weak_form("Mode", Form::new().green());
        KeyMap {
            cursors: Some(Cursors::new_inclusive()),
            mode: Mode::Normal,
        }
    }

    pub fn mode(&self) -> String {
        self.mode.to_string()
    }

    pub fn mode_fmt(&self) -> Text {
        text!([Mode] { self.mode.to_string() })
    }

    /// Commands that are available in `Mode::Insert`.
    fn match_insert<U: Ui>(&mut self, mut helper: EditHelper<File, U>, key: Event) {
        match key {
            key!(Char(char)) => {
                helper.edit_on_each_cursor(|editor| editor.insert(char));
                helper.move_each_cursor(|m| m.move_hor(1));
            }
            key!(Char(char), Mod::SHIFT) => {
                helper.edit_on_each_cursor(|editor| editor.insert(char));
                helper.move_each_cursor(|m| m.move_hor(1));
            }
            key!(Enter) => {
                helper.edit_on_each_cursor(|editor| editor.insert('\n'));
                helper.move_each_cursor(|m| m.move_hor(1));
            }
            key!(Backspace) => {
                let mut anchors = Vec::with_capacity(helper.len_cursors());
                helper.move_each_cursor(|m| {
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
                helper.edit_on_each_cursor(|editor| editor.replace(""));
                helper.move_each_cursor(|m| {
                    if let Some(Some(diff)) = anchors.next() {
                        m.set_anchor();
                        m.move_hor(diff);
                        m.swap_ends()
                    }
                });
            }
            key!(Delete) => {
                let mut anchors = Vec::with_capacity(helper.len_cursors());
                helper.move_each_cursor(|m| {
                    let caret = m.caret();
                    anchors.push(m.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    m.set_anchor();
                    m.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                helper.edit_on_each_cursor(|editor| {
                    editor.replace("");
                });
                helper.move_each_cursor(|m| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        m.set_anchor();
                        m.move_to(anchor);
                        m.swap_ends()
                    } else {
                        m.unset_anchor();
                    }
                });
            }
            key!(Left, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Left, 1),
            key!(Right, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Right, 1),
            key!(Up, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Top, 1),
            key!(Down, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Bottom, 1),

            key!(Left) => move_each_wrapped(helper, Side::Left, 1),
            key!(Right) => move_each_wrapped(helper, Side::Right, 1),
            key!(Up) => move_each_wrapped(helper, Side::Top, 1),
            key!(Down) => move_each_wrapped(helper, Side::Bottom, 1),

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
        mut helper: EditHelper<File, U>,
        key: Event,
        context: Context<U>,
    ) {
        match key {
            ////////// hjkl and arrow selection keys.
            key!(Char('h')) => move_each(helper, Side::Left, 1),
            key!(Char('j')) => move_each(helper, Side::Bottom, 1),
            key!(Char('k')) => move_each(helper, Side::Top, 1),
            key!(Char('l')) => move_each(helper, Side::Right, 1),
            key!(Char('H'), Mod::SHIFT) => select_and_move_each(helper, Side::Left, 1),
            key!(Char('J'), Mod::SHIFT) => select_and_move_each(helper, Side::Bottom, 1),
            key!(Char('K'), Mod::SHIFT) => select_and_move_each(helper, Side::Top, 1),
            key!(Char('L'), Mod::SHIFT) => select_and_move_each(helper, Side::Right, 1),

            key!(Left) => move_each_wrapped(helper, Side::Left, 1),
            key!(Down) => move_each_wrapped(helper, Side::Bottom, 1),
            key!(Up) => move_each_wrapped(helper, Side::Top, 1),
            key!(Right) => move_each_wrapped(helper, Side::Right, 1),
            key!(Left, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Left, 1),
            key!(Down, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Bottom, 1),
            key!(Up, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Top, 1),
            key!(Right, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Right, 1),

            ////////// Word and WORD selection keys.
            key!(Char('w'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each_cursor(|m| {
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
            key!(Char('e'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each_cursor(|m| {
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
            key!(Char('b'), mf) if let Mod::ALT | Mod::NONE = mf => helper.move_each_cursor(|m| {
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

            key!(Char('W'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each_cursor(|m| {
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
            key!(Char('E'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each_cursor(|m| {
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
            key!(Char('B'), mf) if let Mod::SHIFT | ALTSHIFT = mf => helper.move_each_cursor(|m| {
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

            ////////// Text modifying keys.
            key!(Char('i')) => {
                helper.move_each_cursor(|m| m.swap_ends());
                self.mode = Mode::Insert;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
            }
            key!(Char('a')) => {
                helper.move_each_cursor(|m| m.set_caret_on_end());
                self.mode = Mode::Insert;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
            }
            key!(Char('c')) => {
                helper.edit_on_each_cursor(|editor| editor.replace(""));
                helper.move_each_cursor(|m| m.unset_anchor());
                self.mode = Mode::Insert;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
            }
            key!(Char('d')) => {
                helper.edit_on_each_cursor(|editor| editor.replace(""));
                helper.move_each_cursor(|m| m.unset_anchor());
                self.mode = Mode::Insert;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
            }

            ////////// Other mode changing keys.
            key!(Char(':')) => {
                if context.commands.run("switch-to CommandLine<Ui>").is_ok() {
                    context
                        .commands
                        .run("set-cmd-mode RunCommands<Ui>")
                        .unwrap();
                    self.mode = Mode::Command;
                    hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Command));
                }
            }
            key!(Char('g')) => {
                self.mode = Mode::GoTo;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::GoTo));
            }

            ////////// For now
            key!(Char('q')) => panic!("Quit on purpose"),

            ////////// History manipulation.
            key!(Char('u')) => helper.undo(),
            key!(Char('U'), Mod::SHIFT) => helper.redo(),
            _ => {}
        }
    }

    /// Commands that are available in `Mode::GoTo`.
    fn match_goto<U: Ui>(
        &mut self,
        mut helper: EditHelper<File, U>,
        key: Event,
        context: Context<U>,
    ) {
        static LAST_FILE: LazyLock<RwData<Option<String>>> = LazyLock::new(RwData::default);
        let last_file = LAST_FILE.read().clone();
        let cur_name = context.cur_file().unwrap().name();

        match key {
            key!(Char('a')) => {
                if let Some(last) = last_file
                    && context.commands.run_notify(format!("b {last}")).is_ok()
                {
                    *LAST_FILE.write() = Some(cur_name);
                } else {
                    context.notify(err!("There is no previous file."))
                }
            }
            key!(Char('j')) => helper.move_main(|m| m.move_ver(isize::MAX)),
            key!(Char('k')) => helper.move_main(|m| m.move_to_coords(0, 0)),
            key!(Char('n')) => {
                if context.commands.run_notify("next-file").is_ok() {
                    *LAST_FILE.write() = Some(cur_name);
                }
            }
            key!(Char('n')) => {}
            key!(Char('N'), Mod::SHIFT) => {
                if context.commands.run_notify("prev-file").is_ok() {
                    *LAST_FILE.write() = Some(cur_name);
                }
            }
            _ => {}
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
        key: Event,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        context: Context<U>,
    ) {
        let mut cursors = self.cursors.take().unwrap();
        let helper = EditHelper::new(widget, area, &mut cursors);

        match self.mode {
            Mode::Insert => self.match_insert(helper, key),
            Mode::Normal => self.match_normal(helper, key, context),
            Mode::GoTo => {
                self.match_goto(helper, key, context);
                self.mode = Mode::Normal;
                hooks::trigger::<OnModeChange>((Mode::GoTo, Mode::Normal));
            }
            Mode::View => todo!(),
            Mode::Command => {
                unreachable!("The editor is not supposed to be focused if this is the current mode")
            }
        }

        self.cursors = Some(cursors);
    }

    fn cursors(&self) -> Option<&Cursors> {
        self.cursors.as_ref()
    }

    fn on_focus(&mut self, _area: &U::Area)
    where
        Self: Sized,
    {
        self.mode = Mode::Normal;
        hooks::trigger::<OnModeChange>((Mode::GoTo, Mode::Normal));
    }
}

#[derive(Default, Clone, Copy, PartialEq)]
pub enum Mode {
    Insert,
    #[default]
    Normal,
    GoTo,
    View,
    Command,
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

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Insert => f.write_str("insert"),
            Mode::Normal => f.write_str("normal"),
            Mode::GoTo => f.write_str("goto"),
            Mode::View => f.write_str("view"),
            Mode::Command => f.write_str("command"),
        }
    }
}

fn move_each<U: Ui>(mut helper: EditHelper<File, U>, direction: Side, amount: usize) {
    helper.move_each_cursor(|m| {
        m.unset_anchor();
        match direction {
            Side::Top => m.move_ver(-(amount as isize)),
            Side::Bottom => m.move_ver(amount as isize),
            Side::Left => m.move_hor(-(amount as isize)),
            Side::Right => m.move_hor(amount as isize),
        }
    });
}

fn select_and_move_each<U: Ui>(mut helper: EditHelper<File, U>, direction: Side, amount: usize) {
    helper.move_each_cursor(|m| {
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

fn move_each_wrapped<U: Ui>(mut helper: EditHelper<File, U>, direction: Side, amount: usize) {
    helper.move_each_cursor(|m| {
        m.unset_anchor();
        match direction {
            Side::Top => m.move_ver_wrapped(-(amount as isize)),
            Side::Bottom => m.move_ver_wrapped(amount as isize),
            Side::Left => m.move_hor(-(amount as isize)),
            Side::Right => m.move_hor(amount as isize),
        }
    });
}

fn select_and_move_each_wrapped<U: Ui>(
    mut helper: EditHelper<File, U>,
    direction: Side,
    amount: usize,
) {
    helper.move_each_cursor(|m| {
        if m.anchor().is_none() {
            m.set_anchor();
        }
        match direction {
            Side::Top => m.move_ver_wrapped(-(amount as isize)),
            Side::Bottom => m.move_ver_wrapped(amount as isize),
            Side::Left => m.move_hor(-(amount as isize)),
            Side::Right => m.move_hor(amount as isize),
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
    (if cat0 == cat1 { p0 } else { p1 }, cat1)
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
