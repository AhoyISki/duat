use parsec_core::{
    data::{AsAny, ReadableData, RoData},
    forms::{FormPalette, DEFAULT, FormId},
    text::{BuilderTag, Text, TextBuilder},
    ui::{Constraint, PushSpecs, Ui},
    widgets::{FileWidget, Widget, WidgetType},
    Controler
};

/// The [`char`]s that should be printed above, equal to, and below
/// the main line.
#[derive(Clone)]
pub enum SepChar {
    Uniform(char),
    /// Order: main line, other lines.
    TwoWay(char, char),
    /// Order: main line, above main line, below main line.
    ThreeWay(char, char, char)
}

impl Default for SepChar {
    fn default() -> Self {
        SepChar::Uniform('│')
    }
}

impl SepChar {
    /// The [`char`]s above, equal to, and below the main line,
    /// respectively.
    fn chars(&self) -> [char; 3] {
        match self {
            SepChar::Uniform(uniform) => [*uniform, *uniform, *uniform],
            SepChar::TwoWay(main, other) => [*other, *main, *other],
            SepChar::ThreeWay(main, upper, lower) => [*upper, *main, *lower]
        }
    }
}

/// The `form_id`s that should be printed above, equal to, and below
/// the main line.
#[derive(Clone)]
pub enum SepForm {
    Uniform(FormId),
    /// Order: main line, other lines.
    TwoWay(FormId, FormId),
    /// Order: main line, above main line, below main line.
    ThreeWay(FormId, FormId, FormId)
}

impl Default for SepForm {
    fn default() -> Self {
        SepForm::Uniform(DEFAULT)
    }
}

impl SepForm {
    /// Returns a new instance of [`SepForm`], with one `form_name`
    /// for all lines.
    pub fn uniform(palette: &FormPalette, name: impl AsRef<str>) -> Self {
        let (_, id) = palette.from_name(name);

        SepForm::Uniform(id)
    }

    /// Returns a new instance of [`SepForm`], with one `form_name`
    /// for the main line and another `form_name` for other lines,
    /// respectively.
    pub fn two_way(
        palette: &FormPalette, main_name: impl AsRef<str>, other_name: impl AsRef<str>
    ) -> Self {
        let (_, main_id) = palette.from_name(main_name);
        let (_, other_id) = palette.from_name(other_name);

        SepForm::TwoWay(main_id, other_id)
    }

    /// Returns a new instance of [`SepForm`], with one `form_name`
    /// for the main line, one for lines above, and one for lines
    /// below, respectively.
    pub fn three_way(
        palette: &FormPalette, main_name: impl AsRef<str>, upper_name: impl AsRef<str>,
        lower_name: impl AsRef<str>
    ) -> Self {
        let (_, main_id) = palette.from_name(main_name);
        let (_, upper_id) = palette.from_name(upper_name);
        let (_, lower_id) = palette.from_name(lower_name);

        SepForm::ThreeWay(main_id, upper_id, lower_id)
    }

    /// The `form_id`s above, equal to, and below the main line,
    /// respectively.
    fn forms(&self) -> [FormId; 3] {
        match self {
            SepForm::Uniform(uniform) => [*uniform, *uniform, *uniform],
            SepForm::TwoWay(main, other) => [*other, *main, *other],
            SepForm::ThreeWay(main, lower, upper) => [*upper, *main, *lower]
        }
    }
}

/// The configurations for the [`VertRule`] widget.
#[derive(Default, Clone)]
pub struct VertRuleCfg {
    pub sep_char: SepChar,
    pub sep_form: SepForm
}

impl VertRuleCfg {
    /// Returns a new instance of [`VertRuleCfg`].
    pub fn new(sep_char: SepChar, sep_form: SepForm) -> Self {
        Self { sep_char, sep_form }
    }
}

/// A vertical line on screen, useful, for example, for the separation
/// of a [`FileWidget<U>`] and
/// [`LineNumbers<U>`][parsec_core::widgets::LineNumbers<U>].
pub struct VertRule<U>
where
    U: Ui
{
    file: RoData<FileWidget<U>>,
    builder: TextBuilder,
    cfg: VertRuleCfg
}

impl<U> VertRule<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of `Box<VerticalRuleConfig>`, taking a
    /// user provided config.
    /// Returns a new instance of `Box<VerticalRuleConfig>`, using the
    /// default config.
    pub fn config_fn(
        cfg: VertRuleCfg
    ) -> impl FnOnce(&Controler<U>) -> (WidgetType<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        move |controler| {
            let file = controler.active_file();
            let builder = file.inspect(|file| setup_builder(file, &cfg));

            let vert_rule = VertRule { file: file.clone(), builder, cfg };

            let checker = Box::new(move || file.has_changed());
            let passive = WidgetType::passive(vert_rule);
            (passive, checker, PushSpecs::left(Constraint::Length(1.0)))
        }
    }

    /// Returns a new instance of `Box<VerticalRuleConfig>`, using the
    /// default config.
    pub fn default_fn()
    -> impl FnOnce(&Controler<U>) -> (WidgetType<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        let cfg = VertRuleCfg::default();
        VertRule::config_fn(cfg)
    }
}

impl<U> AsAny for VertRule<U>
where
    U: Ui + 'static
{
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl<U> Widget<U> for VertRule<U>
where
    U: Ui + 'static
{
    fn update(&mut self, _area: &U::Area) {
        let file = self.file.read();
        let lines = file.printed_lines();
        let builder = &mut self.builder;
        let main_line = file.main_cursor().true_line();
        let above = lines.iter().filter(|&(line, _)| *line < main_line).count();
        let equal = lines.iter().filter(|&(line, _)| *line == main_line).count();
        let below = lines.iter().filter(|&(line, _)| *line > main_line).count();

        let forms = self.cfg.sep_form.forms();
        let chars = self.cfg.sep_char.chars();

        builder.swap_tag(0, BuilderTag::PushForm(forms[0]));
        builder.swap_range(0, [chars[0], '\n'].into_iter().collect::<String>().repeat(above));
        builder.swap_tag(1, BuilderTag::PushForm(forms[1]));
        builder.swap_range(1, [chars[1], '\n'].into_iter().collect::<String>().repeat(equal));
        builder.swap_tag(2, BuilderTag::PushForm(forms[2]));
        builder.swap_range(2, [chars[2], '\n'].into_iter().collect::<String>().repeat(below));
    }

    fn text(&self) -> &Text {
        self.builder.text()
    }
}

/// Sets up a new [`TextBuilder<U>`] for the [`VertRule`] widget.
fn setup_builder<U>(file: &FileWidget<U>, cfg: &VertRuleCfg) -> TextBuilder
where
    U: Ui
{
    let lines = file.printed_lines();
    let main_line = file.main_cursor().true_line();
    let mut builder = TextBuilder::default();
    let upper = lines.iter().take_while(|&(line, _)| *line != main_line).count();
    let lower = lines.iter().skip_while(|&(line, _)| *line <= main_line).count();

    let forms = cfg.sep_form.forms();
    let chars = cfg.sep_char.chars();

    builder.push_tag(BuilderTag::PushForm(forms[0]));
    builder.push_swappable([chars[0], '\n'].into_iter().collect::<String>().repeat(upper));
    builder.push_tag(BuilderTag::PushForm(forms[1]));
    builder.push_swappable([chars[1], '\n'].into_iter().collect::<String>());
    builder.push_tag(BuilderTag::PushForm(forms[2]));
    builder.push_swappable([chars[2], '\n'].into_iter().collect::<String>().repeat(lower));

    builder
}
