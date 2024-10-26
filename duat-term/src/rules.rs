use duat_core::{
    context::{self, FileReader},
    forms::{self, Form},
    text::{Text, text},
    ui::{Area as UiArea, PushSpecs},
    widgets::{Widget, WidgetCfg},
};

use crate::{Area, Ui};

/// A vertical line on screen, useful, for example, for the separation
/// of a [`File`] and [`LineNumbers`].
///
/// [`File`]: duat_core::widgets::File
/// [`LineNumbers`]: duat_core::widgets::LineNumbers
pub struct VertRule {
    reader: Option<FileReader<Ui>>,
    text: Text,
    sep_char: SepChar,
}

impl Widget<Ui> for VertRule {
    type Cfg = VertRuleCfg;

    fn cfg() -> Self::Cfg {
        VertRuleCfg::new()
    }

    fn update(&mut self, area: &Area) {
        self.text = if let Some(reader) = self.reader.as_ref()
            && let SepChar::ThreeWay(..) | SepChar::TwoWay(..) = self.sep_char
        {
            reader.inspect(|file, _, cursors| {
                let main_line = cursors.as_ref().unwrap().main().line();
                let lines = file.printed_lines();

                let upper = lines.iter().filter(|&(line, _)| *line < main_line).count();
                let middle = lines.iter().filter(|&(line, _)| *line == main_line).count();
                let lower = lines.iter().filter(|&(line, _)| *line > main_line).count();

                let chars = self.sep_char.chars();

                text!(
                    [UpperVertRule] { form_string(chars[0], upper) }
                    [VertRule] { form_string(chars[1], middle) }
                    [LowerVertRule] { form_string(chars[2], lower) }
                )
            })
        } else {
            let full_line = format!("{}\n", self.sep_char.chars()[1]).repeat(area.height());

            text!([VertRule] full_line)
        }
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() {
        forms::set_weak("VertRule", Form::dark_grey());
        forms::set_weak("UpperVertRule", "VertRule");
        forms::set_weak("LowerVertRule", "VertRule");
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
            specs: PushSpecs::left().with_hor_len(1.0),
        }
    }

    pub fn on_the_right(self) -> Self {
        Self {
            specs: PushSpecs::right().with_ver_len(1.0),
            ..self
        }
    }

    pub fn with_char(self, char: char) -> Self {
        Self { sep_char: SepChar::Uniform(char), ..self }
    }

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

    fn build(self, on_file: bool) -> (Self::Widget, impl Fn() -> bool + 'static, PushSpecs) {
        let reader = on_file.then_some(context::fixed_reader().unwrap());

        let widget = VertRule {
            reader: reader.clone(),
            text: Text::default(),
            sep_char: self.sep_char,
        };

        let checker = if let Some(reader) = reader {
            Box::new(move || reader.has_changed()) as Box<dyn Fn() -> bool>
        } else {
            Box::new(move || false)
        };

        (widget, checker, self.specs)
    }
}

fn form_string(char: char, count: usize) -> String {
    [char, '\n'].repeat(count).iter().collect()
}
