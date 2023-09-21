use parsec_core::{
    data::{FileReader, ReadableData, RoData},
    forms::Form,
    input::InputMethod,
    text::{build, Text},
    ui::{Area, PushSpecs, Ui},
    widgets::{FileWidget, PassiveWidget, Widget},
    Controler, ACTIVE_FILE, PALETTE,
};

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
            specs: PushSpecs::left().with_lenght(1.0),
        }
    }

    pub fn on_the_right(self) -> Self {
        Self {
            specs: PushSpecs::right().with_lenght(1.0),
            ..self
        }
    }

    pub fn with_char(self, char: char) -> Self {
        Self {
            sep_char: SepChar::Uniform(char),
            ..self
        }
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

    pub fn builder<U: Ui>(
        self,
    ) -> impl FnOnce(&Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        move |controler| {
            let reader = ACTIVE_FILE.current();

            PALETTE.try_set_form("VertRule", Form::new().grey());
            PALETTE.set_new_ref("UpperVertRule", "VertRule");
            PALETTE.set_new_ref("LowerVertRule", "VertRule");

            let vert_rule = VertRule {
                reader: reader.clone(),
                text: Text::default(),
                sep_char: self.sep_char,
            };

            let checker = Box::new(move || reader.has_changed());
            let widget = Widget::passive(vert_rule);
            (widget, checker, self.specs)
        }
    }
}

impl Default for VertRuleCfg {
    fn default() -> Self {
        Self::new()
    }
}

/// A vertical line on screen, useful, for example, for the separation
/// of a [`FileWidget<U>`] and
/// [`LineNumbers<U>`][parsec_core::widgets::LineNumbers<U>].
pub struct VertRule {
    reader: FileReader,
    text: Text,
    sep_char: SepChar,
}

impl VertRule {
    pub fn config() -> VertRuleCfg {
        VertRuleCfg::new()
    }
}

impl PassiveWidget for VertRule {
    fn build<U>(controler: &Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
        Self: Sized,
    {
        VertRuleCfg::new().builder()(controler)
    }

    fn update(&mut self, _area: &impl Area) {
        let (file, input) = self.reader.read();

        let main_line = input.cursors().unwrap().main().true_line();
        let lines = file.printed_lines();

        let upper = lines.iter().filter(|&(line, _)| *line < main_line).count();
        let middle = lines.iter().filter(|&(line, _)| *line == main_line).count();
        let lower = lines.iter().filter(|&(line, _)| *line > main_line).count();

        let chars = self.sep_char.chars();

        let builder = build!(
            [UpperVertRule] { form_string(chars[0], upper) }
            [VertRule] { form_string(chars[1], middle) }
            [LowerVertRule] { form_string(chars[2], lower) }
        );

        self.text = builder.finish();
    }

    fn text(&self) -> &Text {
        &self.text
    }
}

fn form_string(char: char, count: usize) -> String {
    [char, '\n'].repeat(count).iter().collect()
}
