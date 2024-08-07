#![feature(let_chains, iter_map_windows, type_alias_impl_trait)]

use std::fmt::Display;

use duat_core::{
    data::{Context, RwData},
    hooks::{self, Hookable},
    input::{
        key::{key, Code::*, Event, Mod},
        Cursors, EditHelper, InputMethod,
    },
    palette::{self, Form},
    text::{text, CharSet, Point, Text, WordChars},
    ui::Ui,
    widgets::File,
};

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

enum Side {
    Left,
    Right,
    Top,
    Bottom,
}

#[derive(Default, Clone)]
pub struct KeyMap {
    cursors: Cursors,
    mode: Mode,
    last_file: String,
}

impl KeyMap {
    pub fn new() -> Self {
        palette::set_weak_form("Mode", Form::new().green());
        Self::default()
    }

    pub fn mode(&self) -> String {
        self.mode.to_string()
    }

    pub fn mode_fmt(&self) -> Text {
        text!([Mode] { self.mode.to_string() })
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
        let cursors = &mut self.cursors;
        let helper = EditHelper::new(widget, area, cursors);

        match self.mode {
            Mode::Insert => match_insert(helper, key, &mut self.mode),
            Mode::Normal => match_normal(helper, key, &mut self.mode, context),
            Mode::GoTo => {
                match_goto(helper, key, &mut self.last_file, context);
                self.mode = Mode::Normal;
                hooks::trigger::<OnModeChange>((Mode::GoTo, Mode::Normal));
            }
            Mode::View => todo!(),
            Mode::Command => {
                unreachable!("The editor is not supposed to be focused if this is the current mode")
            }
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }

    fn on_focus(&mut self, _area: &U::Area)
    where
        Self: Sized,
    {
        self.mode = Mode::Normal;
        hooks::trigger::<OnModeChange>((Mode::GoTo, Mode::Normal));
    }
}

/// Commands that are available in `Mode::Insert`.
fn match_insert<U: Ui>(mut helper: EditHelper<File, U>, key: Event, mode: &mut Mode) {
    match key {
        key!(Char(char)) => {
            helper.edit_on_each_cursor(|editor| editor.insert(char));
            helper.move_each_cursor(|mover| mover.move_hor(1));
        }
        key!(Char(char), Mod::SHIFT) => {
            helper.edit_on_each_cursor(|editor| editor.insert(char));
            helper.move_each_cursor(|mover| mover.move_hor(1));
        }
        key!(Enter) => {
            helper.edit_on_each_cursor(|editor| editor.insert('\n'));
            helper.move_each_cursor(|mover| mover.move_hor(1));
        }
        key!(Backspace) => {
            let mut anchors = Vec::with_capacity(helper.len_cursors());
            helper.move_each_cursor(|mover| {
                let caret = mover.caret();
                anchors.push(mover.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                mover.set_anchor();
                mover.move_hor(-1);
            });
            let mut anchors = anchors.into_iter().cycle();
            helper.edit_on_each_cursor(|editor| {
                editor.replace("");
            });
            helper.move_each_cursor(|mover| {
                if let Some(Some((anchor, _))) = anchors.next() {
                    mover.set_anchor();
                    mover.move_to(anchor);
                    mover.swap_ends()
                } else {
                    mover.unset_anchor();
                }
            });
        }
        key!(Delete) => {
            let mut anchors = Vec::with_capacity(helper.len_cursors());
            helper.move_each_cursor(|mover| {
                let caret = mover.caret();
                anchors.push(mover.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                mover.set_anchor();
                mover.move_hor(1);
            });
            let mut anchors = anchors.into_iter().cycle();
            helper.edit_on_each_cursor(|editor| {
                editor.replace("");
            });
            helper.move_each_cursor(|mover| {
                if let Some(Some((anchor, _))) = anchors.next() {
                    mover.set_anchor();
                    mover.move_to(anchor);
                    mover.swap_ends()
                } else {
                    mover.unset_anchor();
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
            *mode = Mode::Normal;
            hooks::trigger::<OnModeChange>((Mode::Insert, Mode::Normal))
        }
        _ => {}
    }
}

/// Commands that are available in `Mode::Normal`.
fn match_normal<U: Ui>(
    mut helper: EditHelper<File, U>,
    key: Event,
    mode: &mut Mode,
    context: Context<U>,
) {
    match key {
        ////////// Movement keys that retain or create selections.
        key!(Char('H'), Mod::SHIFT) => select_and_move_each(helper, Side::Left, 1),
        key!(Char('J'), Mod::SHIFT) => select_and_move_each(helper, Side::Bottom, 1),
        key!(Char('K'), Mod::SHIFT) => select_and_move_each(helper, Side::Top, 1),
        key!(Char('L'), Mod::SHIFT) => select_and_move_each(helper, Side::Right, 1),

        key!(Left, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Left, 1),
        key!(Down, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Bottom, 1),
        key!(Up, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Top, 1),
        key!(Right, Mod::SHIFT) => select_and_move_each_wrapped(helper, Side::Right, 1),

        ////////// Movement keys that get rid of selections.
        key!(Char('h')) => move_each(helper, Side::Left, 1),
        key!(Char('j')) => move_each(helper, Side::Bottom, 1),
        key!(Char('k')) => move_each(helper, Side::Top, 1),
        key!(Char('l')) => move_each(helper, Side::Right, 1),

        key!(Left) => move_each_wrapped(helper, Side::Left, 1),
        key!(Down) => move_each_wrapped(helper, Side::Bottom, 1),
        key!(Up) => move_each_wrapped(helper, Side::Top, 1),
        key!(Right) => move_each_wrapped(helper, Side::Right, 1),

        ////////// Movement that parses text.
        key!(Char('w')) => {
            helper.move_each_cursor(|m| {
                let mut chars = m
                    .iter()
                    .map_windows(|[first, second]| (*first, *second))
                    .skip_while(|(_, (_, c1))| *c1 == '\n');

                if let Some(((p0, c0), (p1, c1))) = chars.next() {
                    let cat0 = CharCategory::of(c0, m.w_chars());
                    let cat1 = CharCategory::of(c1, m.w_chars());
                    let p0 = if cat0 == cat1 { p0 } else { p1 };

                    let chars = chars.map(|(_, second)| second);
                    let p1 = end_of_cat_and_space(chars, cat1, p1);

                    m.move_to(p0);
                    m.set_anchor();
                    m.move_to(p1);
                };
            });
        }

        ////////// Selection movement that parses text.
        key!(Char('W'), Mod::SHIFT) => {
            helper.move_each_cursor(|m| {
                if m.anchor().is_none() {
                    m.set_anchor();
                }
                let mut chars = m.iter().skip(1).skip_while(|(_, char)| *char == '\n');

                if let Some((point, char)) = chars.next() {
                    let cat = CharCategory::of(char, m.w_chars());
                    let point = end_of_cat_and_space(chars, cat, point);
                    m.move_to(point);
                };
            });
        }

        ////////// Insertion keys.
        key!(Char('i')) => {
            helper.move_each_cursor(|mover| mover.swap_ends());
            *mode = Mode::Insert;
            hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
        }
        key!(Char('a')) => {
            helper.move_each_cursor(|mover| mover.set_caret_on_end());
            *mode = Mode::Insert;
            hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
        }
        key!(Char('c')) => {
            helper.move_each_cursor(|mover| {
                if mover.anchor().is_none() {
                    mover.set_anchor();
                }
                mover.move_hor(1);
            });
            helper.edit_on_each_cursor(|editor| editor.replace(""));
            helper.move_each_cursor(|mover| mover.unset_anchor());
            *mode = Mode::Insert;
            hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
        }

        ////////// Other mode changing keys.
        key!(Char(':')) => {
            if context.commands.run("switch-to CommandLine<Ui>").is_ok() {
                context
                    .commands
                    .run("set-cmd-mode RunCommands<Ui>")
                    .unwrap();
                *mode = Mode::Command;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Command));
            }
        }
        key!(Char('g')) => {
            *mode = Mode::GoTo;
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
    mut helper: EditHelper<File, U>,
    key: Event,
    last_file: &mut String,
    context: Context<U>,
) {
    match key {
        key!(Char('a')) if context.commands.buffer(last_file.clone()).is_ok() => {
            *last_file = context.cur_file().unwrap().name();
        }
        key!(Char('a')) => {}
        key!(Char('j')) => helper.move_main(|mover| mover.move_ver(isize::MAX)),
        key!(Char('k')) => helper.move_main(|mover| mover.move_to_coords(0, 0)),
        key!(Char('n')) if context.commands.next_file().is_ok() => {
            *last_file = context.cur_file().unwrap().name()
        }
        key!(Char('n')) => {}
        key!(Char('N')) if context.commands.prev_file().is_ok() => {
            *last_file = context.cur_file().unwrap().name()
        }
        _ => {}
    }
}

fn move_each<U: Ui>(mut helper: EditHelper<File, U>, direction: Side, amount: usize) {
    helper.move_each_cursor(|mover| {
        mover.unset_anchor();
        match direction {
            Side::Top => mover.move_ver(-(amount as isize)),
            Side::Bottom => mover.move_ver(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}

fn select_and_move_each<U: Ui>(mut helper: EditHelper<File, U>, direction: Side, amount: usize) {
    helper.move_each_cursor(|mover| {
        if mover.anchor().is_none() {
            mover.set_anchor();
        }
        match direction {
            Side::Top => mover.move_ver(-(amount as isize)),
            Side::Bottom => mover.move_ver(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}

fn move_each_wrapped<U: Ui>(mut helper: EditHelper<File, U>, direction: Side, amount: usize) {
    helper.move_each_cursor(|mover| {
        mover.unset_anchor();
        match direction {
            Side::Top => mover.move_ver_wrapped(-(amount as isize)),
            Side::Bottom => mover.move_ver_wrapped(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}

fn select_and_move_each_wrapped<U: Ui>(
    mut helper: EditHelper<File, U>,
    direction: Side,
    amount: usize,
) {
    helper.move_each_cursor(|mover| {
        if mover.anchor().is_none() {
            mover.set_anchor();
        }
        match direction {
            Side::Top => mover.move_ver_wrapped(-(amount as isize)),
            Side::Bottom => mover.move_ver_wrapped(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}

fn end_of_cat_and_space<'a>(
    mut chars: impl Iterator<Item = (Point, char)>,
    cat: CharCategory<'a>,
    mut point: Point,
) -> Point {
    let mut on_cat = true;
    while let Some((p, char)) = chars.next()
        && char != '\n'
    {
        if cat.matches(char) && on_cat {
            point = p;
        } else if [' ', '\t'].matches(char) {
            point = p;
            on_cat = false;
        } else {
            break;
        }
    }

    point
}

enum CharCategory<'a> {
    Word(&'a WordChars),
    Space,
    Other(&'a WordChars),
}

impl<'a> CharCategory<'a> {
    fn of(char: char, w_chars: &'a WordChars) -> Self {
        if w_chars.matches(char) {
            Self::Word(w_chars)
        } else if [' ', '\t', '\n'].matches(char) {
            Self::Space
        } else {
            Self::Other(w_chars)
        }
    }
}

impl CharSet for CharCategory<'_> {
    fn matches(&self, char: char) -> bool {
        match self {
            CharCategory::Word(w_chars) => w_chars.matches(char),
            CharCategory::Space => [' ', '\t', '\n'].matches(char),
            CharCategory::Other(w_chars) => w_chars.or([' ', '\t', '\n']).not().matches(char),
        }
    }
}

impl<'a> PartialEq for CharCategory<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (CharCategory::Word(_), CharCategory::Word(_))
            | (CharCategory::Space, CharCategory::Space)
            | (CharCategory::Other(_), CharCategory::Other(_)) => true,
            _ => false,
        }
    }
}
