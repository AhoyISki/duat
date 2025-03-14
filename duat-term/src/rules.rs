use duat_core::{
    context::{self, FixedFile},
    form::{self, Form},
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
    ff: Option<FixedFile<Ui>>,
    text: Text,
    sep_char: SepChar,
}

impl Widget<Ui> for VertRule {
    type Cfg = VertRuleCfg;

    fn cfg() -> Self::Cfg {
        VertRuleCfg::new()
    }

    fn update(&mut self, area: &Area) {
        self.text = if let Some(ff) = self.ff.as_mut()
            && let SepChar::ThreeWay(..) | SepChar::TwoWay(..) = self.sep_char
        {
            let (file, _) = ff.read();
            let lines = file.printed_lines();
            let (upper, middle, lower) = if let Some(main) = file.cursors().get_main() {
                let main = main.line();
                let upper = lines.iter().filter(|&(line, _)| *line < main).count();
                let middle = lines.iter().filter(|&(line, _)| *line == main).count();
                let lower = lines.iter().filter(|&(line, _)| *line > main).count();
                (upper, middle, lower)
            } else {
                (0, lines.len(), 0)
            };

            let chars = self.sep_char.chars();

            text!(
                [VertRule.upper] { form_string(chars[0], upper) }
                [VertRule] { form_string(chars[1], middle) }
                [VertRule.lower] { form_string(chars[2], lower) }
            )
        } else {
            let full_line =
                format!("{}\n", self.sep_char.chars()[1]).repeat(area.height() as usize);

            text!([VertRule] full_line)
        }
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() -> Result<(), duat_core::Error<()>> {
        form::set_weak("VertRule", Form::dark_grey());
        form::id_of!("VertRule.upper", "VertRule.lower");
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

    fn build(self, on_file: bool) -> (Self::Widget, impl Fn() -> bool, PushSpecs) {
        let ff = on_file.then_some(context::fixed_file().unwrap());

        let checker = if let Some(ff) = ff.as_ref()
            && let SepChar::TwoWay(..) | SepChar::ThreeWay(..) = self.sep_char
        {
            let checker = ff.checker();
            Box::new(checker) as Box<dyn Fn() -> bool + Send + Sync>
        } else {
            Box::new(move || false)
        };

        let widget = VertRule {
            ff,
            text: Text::default(),
            sep_char: self.sep_char,
        };

        (widget, checker, self.specs)
    }
}

fn form_string(char: char, count: usize) -> String {
    [char, '\n'].repeat(count).iter().collect()
}
