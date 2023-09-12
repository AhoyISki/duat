use parsec_core::{
    data::{AsAny, ReadableData, RoData},
    forms::{Form, FormId},
    input::InputMethod,
    text::{BuilderTag, Text, TextBuilder},
    ui::{Area, PushSpecs, Ui},
    widgets::{FileWidget, PassiveWidget, Widget},
    Controler,
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
        |controler| {
            let file = controler.current_file();
            let input = controler.current_input();

            let palette = controler.palette();
            let main = palette.set_new_form("VertRule", Form::new().grey());
            let upper = palette.set_new_ref("UpperVertRule", "VertRule");
            let lower = palette.set_new_ref("LowerVertRule", "VertRule");

            let builder = {
                let input = input.read();
                let file = file.read();
                setup_builder(&file, &*input, &self, (upper, main, lower))
            };

            let vert_rule = VertRule {
                file: file.clone(),
                input: input.clone(),
                builder,
                sep_char: self.sep_char,
                forms: (upper, main, lower),
            };

            let checker = Box::new(move || file.has_changed() || input.has_changed());
            let passive = Widget::passive(vert_rule);
            (passive, checker, self.specs)
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
    file: RoData<FileWidget>,
    input: RoData<dyn InputMethod>,
    builder: TextBuilder,
    sep_char: SepChar,
    forms: (FormId, FormId, FormId),
}

impl VertRule {
    pub fn config() -> VertRuleCfg {
        VertRuleCfg::new()
    }
}

impl AsAny for VertRule {
    fn as_any(&self) -> &dyn std::any::Any {
        self
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
        let main_line = {
            let input = self.input.read();
            input.cursors().unwrap().main().true_line()
        };

        let file = self.file.read();
        let lines = file.printed_lines();
        let builder = &mut self.builder;

        let above = lines.iter().filter(|&(line, _)| *line < main_line).count();
        let equal = lines.iter().filter(|&(line, _)| *line == main_line).count();
        let below = lines.iter().filter(|&(line, _)| *line > main_line).count();

        let chars = self.sep_char.chars();

        builder.swap_tag(0, BuilderTag::PushForm(self.forms.0));
        builder.swap_range(
            0,
            [chars[0], '\n']
                .into_iter()
                .collect::<String>()
                .repeat(above),
        );
        builder.swap_tag(1, BuilderTag::PushForm(self.forms.1));
        builder.swap_range(
            1,
            [chars[1], '\n']
                .into_iter()
                .collect::<String>()
                .repeat(equal),
        );
        builder.swap_tag(2, BuilderTag::PushForm(self.forms.2));
        builder.swap_range(
            2,
            [chars[2], '\n']
                .into_iter()
                .collect::<String>()
                .repeat(below),
        );
    }

    fn text(&self) -> &Text {
        self.builder.text()
    }
}

/// Sets up a new [`TextBuilder<U>`] for the [`VertRule`] widget.
fn setup_builder(
    file: &FileWidget,
    input: &dyn InputMethod,
    cfg: &VertRuleCfg,
    forms: (FormId, FormId, FormId),
) -> TextBuilder {
    let lines = file.printed_lines();

    let main_line = input.cursors().unwrap().main().true_line();

    let mut builder = TextBuilder::default();
    let upper = lines
        .iter()
        .take_while(|&(line, _)| *line != main_line)
        .count();
    let lower = lines
        .iter()
        .skip_while(|&(line, _)| *line <= main_line)
        .count();

    let chars = cfg.sep_char.chars();

    builder.push_tag(BuilderTag::PushForm(forms.0));
    builder.push_swappable(
        [chars[0], '\n']
            .into_iter()
            .collect::<String>()
            .repeat(upper),
    );
    builder.push_tag(BuilderTag::PushForm(forms.1));
    builder.push_swappable([chars[1], '\n'].into_iter().collect::<String>());
    builder.push_tag(BuilderTag::PushForm(forms.2));
    builder.push_swappable(
        [chars[2], '\n']
            .into_iter()
            .collect::<String>()
            .repeat(lower),
    );

    builder
}
