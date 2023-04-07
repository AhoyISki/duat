#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use std::{any::Any, sync::Arc};

use parsec_core::{
    config::{Config, DownCastableData, RoData, RwData},
    tags::{form::DEFAULT, Tag},
    text::{Text, TextBuilder},
    ui::{PushSpecs, Ui},
    updaters,
    widgets::{file_widget::FileWidget, NormalWidget, Widget},
    SessionManager,
};

pub enum SepChar {
    Uniform(char),
    TwoWay(char, char),
    ThreeWay(char, char, char),
}

impl Default for SepChar {
    fn default() -> Self {
        SepChar::Uniform('│')
    }
}

impl SepChar {
    fn chars(&self) -> [char; 3] {
        match self {
            SepChar::Uniform(uniform) => [*uniform, *uniform, *uniform],
            SepChar::TwoWay(main, other) => [*other, *main, *other],
            SepChar::ThreeWay(main, upper, lower) => [*upper, *main, *lower],
        }
    }
}

pub enum SepForm {
    Uniform(u16),
    TwoWay(u16, u16),
    ThreeWay(u16, u16, u16),
}

impl Default for SepForm {
    fn default() -> Self {
        SepForm::Uniform(DEFAULT)
    }
}

impl SepForm {
    pub fn uniform(config: &RwData<Config>, name: impl AsRef<str>) -> Self {
        let (_, id) = config.read().palette.from_name(name);

        SepForm::Uniform(id)
    }

    pub fn two_way(
        config: &RwData<Config>,
        main_name: impl AsRef<str>,
        other_name: impl AsRef<str>,
    ) -> Self {
        let config = config.read();
        let palette = &config.palette;
        let (_, main_id) = palette.from_name(main_name);
        let (_, other_id) = palette.from_name(other_name);

        SepForm::TwoWay(main_id, other_id)
    }

    pub fn three_way(
        config: &RwData<Config>,
        main_name: impl AsRef<str>,
        lower_name: impl AsRef<str>,
        upper_name: impl AsRef<str>,
    ) -> Self {
        let config = config.read();
        let palette = &config.palette;
        let (_, main_id) = palette.from_name(main_name);
        let (_, lower_id) = palette.from_name(lower_name);
        let (_, upper_id) = palette.from_name(upper_name);

        SepForm::ThreeWay(main_id, lower_id, upper_id)
    }

    fn forms(&self) -> [u16; 3] {
        match self {
            SepForm::Uniform(uniform) => [*uniform, *uniform, *uniform],
            SepForm::TwoWay(main, other) => [*other, *main, *other],
            SepForm::ThreeWay(main, lower, upper) => [*upper, *main, *lower],
        }
    }
}

#[derive(Default)]
pub struct VertRuleCfg {
    pub sep_char: SepChar,
    pub sep_form: SepForm,
}

pub struct VertRule<U>
where
    U: Ui,
{
    file: RoData<FileWidget<U>>,
    builder: TextBuilder<U>,
    cfg: VertRuleCfg,
}

impl<U> VertRule<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `Box<VerticalRuleConfig>`, taking a
    /// user provided config.
    /// Returns a new instance of `Box<VerticalRuleConfig>`, using the
    /// default config.
    pub fn config_fn(
        file: RoData<FileWidget<U>>,
        cfg: VertRuleCfg,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        Box::new(move |_, _| {
            let file_read = file.read();
            let builder = setup_builder(&file_read, &cfg);

            let updater = file.clone();
            let updaters = updaters![updater];
            drop(file_read);
            let vert_rule = VertRule { file, builder, cfg };

            Widget::normal(Arc::new(RwLock::new(vert_rule)), updaters)
        })
    }

    /// Returns a new instance of `Box<VerticalRuleConfig>`, using the
    /// default config.
    pub fn default_fn(
        file: RoData<FileWidget<U>>,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        Box::new(move |_, _| {
            let cfg = VertRuleCfg::default();
            let file_read = file.read();
            let builder = setup_builder(&file_read, &cfg);

            let updater = file.clone();
            let updaters = updaters![updater];
            drop(file_read);
            let vert_rule = VertRule { file, builder, cfg };

            Widget::normal(Arc::new(RwLock::new(vert_rule)), updaters)
        })
    }
}

impl<U> DownCastableData for VertRule<U>
where
    U: Ui + 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl<U> NormalWidget<U> for VertRule<U>
where
    U: Ui + 'static,
{
    fn identifier(&self) -> &str {
        "vertical_rule"
    }

    fn update(&mut self, _label: &U::Label, _config: &Config) {
        let file = self.file.read();
        let lines = file.printed_lines();
        let builder = &mut self.builder;
        let main_line = file.main_cursor().true_row();
        let upper = lines.iter().take_while(|&(line, _)| *line != main_line).count();
        let lower = lines.iter().skip_while(|&(line, _)| *line <= main_line).count();

        let forms = self.cfg.sep_form.forms();
        let chars = self.cfg.sep_char.chars();

        builder.swap_tag(0, Tag::PushForm(forms[0]));
        builder.swap_range(0, [chars[0], '\n'].into_iter().collect::<String>().repeat(upper));
        builder.swap_tag(1, Tag::PushForm(forms[1]));
        builder.swap_range(1, [chars[1], '\n'].into_iter().collect::<String>());
        builder.swap_tag(2, Tag::PushForm(forms[2]));
        builder.swap_range(2, [chars[2], '\n'].into_iter().collect::<String>().repeat(lower));
        if upper > 0 {
            // panic!("{:#?}, {:#?}", builder.text.tags,
            // builder.text.inner);
        }
    }

    fn needs_update(&self) -> bool {
        self.file.has_changed()
    }

    fn text(&self) -> &Text<U> {
        self.builder.text()
    }
}

fn setup_builder<U>(file: &FileWidget<U>, cfg: &VertRuleCfg) -> TextBuilder<U>
where
    U: Ui,
{
    let lines = file.printed_lines();
    let main_line = file.main_cursor().true_row();
    let mut builder = TextBuilder::<U>::default();
    let upper = lines.iter().take_while(|&(line, _)| *line != main_line).count();
    let lower = lines.iter().skip_while(|&(line, _)| *line <= main_line).count();

    let forms = cfg.sep_form.forms();
    let chars = cfg.sep_char.chars();

    builder.push_tag(Tag::PushForm(forms[0]));
    builder.push_swappable([chars[0], '\n'].into_iter().collect::<String>().repeat(upper));
    builder.push_tag(Tag::PushForm(forms[1]));
    builder.push_swappable([chars[1], '\n'].into_iter().collect::<String>());
    builder.push_tag(Tag::PushForm(forms[2]));
    builder.push_swappable([chars[2], '\n'].into_iter().collect::<String>().repeat(lower));

    builder
}
