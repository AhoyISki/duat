use duat_core::prelude::*;

use crate::Ui;

/// A vertical line on screen, useful, for example, for the separation
/// of a [`File`] and [`LineNumbers`].
///
/// By default, this [`VertRule`] will show the `'│'` character on the
/// whole line, using the `"default"` form. However, with the
/// following options:
///
/// - [`VertRuleCfg::with_main_char`]
/// - [`VertRuleCfg::with_char_above`]
/// - [`VertRuleCfg::with_char_below`]
/// - [`VertRuleCfg::with_char`]
///
/// If the main character is not the same as the other two characters,
/// then the line will be printed with the `"rule.upper"` and
/// `"rule.lower"` forms for the characters above and below.
///
/// If you want them to have the same characer, but printing with
/// these different forms, you can just call [`with_main_char`] and
/// set it to the same character.
///
/// [`File`]: duat_core::file::File
/// [`LineNumbers`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
/// [`with_main_char`]: VertRuleCfg::with_main_char
pub struct VertRule {
    handle: Option<Handle<File<Ui>, Ui>>,
    text: Text,
    sep_char: SepChar,
}

impl Widget<Ui> for VertRule {
    type Cfg = VertRuleCfg;

    fn update(pa: &mut Pass, handle: &Handle<Self, Ui>) {
        let vr = handle.read(pa);
        let text = if let Some(handle) = vr.handle.as_ref()
            && let SepChar::ThreeWay(..) | SepChar::TwoWay(..) = vr.sep_char
        {
            let (upper, middle, lower) = {
                let file = handle.read(pa);

                let lines = file.printed_lines();
                if let Some(main) = file.selections().get_main() {
                    let main = main.line();
                    let upper = lines.iter().filter(|&(line, _)| *line < main).count();
                    let middle = lines.iter().filter(|&(line, _)| *line == main).count();
                    let lower = lines.iter().filter(|&(line, _)| *line > main).count();
                    (upper, middle, lower)
                } else {
                    (0, lines.len(), 0)
                }
            };

            let chars = vr.sep_char.chars();
            txt!(
                "[rule.upper]{}[]{}[rule.lower]{}",
                form_string(chars[0], upper),
                form_string(chars[1], middle),
                form_string(chars[2], lower)
            )
        } else {
            let full_line =
                format!("{}\n", vr.sep_char.chars()[1]).repeat(handle.area(pa).height() as usize);

            txt!("{full_line}")
        };

        handle.write(pa).text = text.build();
    }

    fn needs_update(&self, _: &Pass) -> bool {
        matches!(self.sep_char, SepChar::ThreeWay(..) | SepChar::TwoWay(..))
            && self.handle.as_ref().is_some_and(|fh| fh.has_changed())
    }

    fn cfg() -> Self::Cfg {
        VertRuleCfg::new()
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() -> Result<(), Text> {
        form::set_weak("rule.upper", "default.VertRule");
        form::set_weak("rule.lower", "default.VertRule");
        Ok(())
    }
}

/// The [`char`]s that should be printed above, equal to, and below
/// the main line.
#[derive(Clone)]
enum SepChar {
    Uniform(char),
    /// Order: main line, other lines.
    TwoWay(char, char),
    /// Order: main line, above main line, below main line.
    ThreeWay(char, char, char),
}

impl SepChar {
    /// The [`char`]s above, equal to, and below the main line,
    /// respectively.
    fn chars(&self) -> [char; 3] {
        match self {
            SepChar::Uniform(uniform) => [*uniform, *uniform, *uniform],
            SepChar::TwoWay(main, other) => [*other, *main, *other],
            SepChar::ThreeWay(main, upper, lower) => [*upper, *main, *lower],
        }
    }
}

/// The configurations for the [`VertRule`] widget.
#[derive(Clone)]
pub struct VertRuleCfg {
    sep_char: SepChar,
    specs: PushSpecs,
}

impl VertRuleCfg {
    /// Returns a new instance of [`VertRuleCfg`].
    pub fn new() -> Self {
        Self {
            sep_char: SepChar::Uniform('│'),
            specs: PushSpecs::left().hor_len(1.0),
        }
    }

    /// Puts this [`VertRule`] on the right
    pub fn on_the_right(self) -> Self {
        Self {
            specs: PushSpecs::right().hor_len(1.0),
            ..self
        }
    }

    /// Sets a [`char`] to be displayed, is `'│'` by default
    pub fn with_char(self, char: char) -> Self {
        Self { sep_char: SepChar::Uniform(char), ..self }
    }

    /// Sets a [`char`] to be displayed on the main line only, is
    /// `'│'` by default
    ///
    /// The lower and upper ranges are unaffected by this option.
    pub fn with_main_char(self, main: char) -> Self {
        Self {
            sep_char: match self.sep_char {
                SepChar::Uniform(other) => SepChar::TwoWay(main, other),
                SepChar::TwoWay(_, other) => SepChar::TwoWay(main, other),
                SepChar::ThreeWay(_, above, below) => SepChar::ThreeWay(main, above, below),
            },
            ..self
        }
    }

    /// Sets a [`char`] to be displayed on the main line only, is
    /// `'│'` by default
    ///
    /// The lower and upper ranges are unaffected by this option.
    pub fn with_char_above(self, above: char) -> Self {
        Self {
            sep_char: match self.sep_char {
                SepChar::Uniform(other) => SepChar::ThreeWay(other, above, other),
                SepChar::TwoWay(main, below) => SepChar::ThreeWay(main, above, below),
                SepChar::ThreeWay(main, _, below) => SepChar::ThreeWay(main, above, below),
            },
            ..self
        }
    }

    pub fn with_char_below(self, below: char) -> Self {
        Self {
            sep_char: match self.sep_char {
                SepChar::Uniform(other) => SepChar::ThreeWay(other, other, below),
                SepChar::TwoWay(main, above) => SepChar::ThreeWay(main, above, below),
                SepChar::ThreeWay(main, above, _) => SepChar::ThreeWay(main, above, below),
            },
            ..self
        }
    }
}

impl Default for VertRuleCfg {
    fn default() -> Self {
        Self::new()
    }
}

impl WidgetCfg<Ui> for VertRuleCfg {
    type Widget = VertRule;

    fn pushed(self, pa: &mut Pass, info: BuildInfo<Ui>) -> (Self::Widget, PushSpecs) {
        let widget = VertRule {
            handle: info.file(),
            text: Text::default(),
            sep_char: self.sep_char,
        };

        (widget, self.specs)
    }
}

fn form_string(char: char, count: usize) -> String {
    [char, '\n'].repeat(count).iter().collect()
}
