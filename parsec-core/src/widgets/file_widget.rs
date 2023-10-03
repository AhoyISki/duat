//! The primary widget of Parsec, used to display files.
//!
//! The [`FileWidget`] is Parsec's way of display text files. It is
//! an [`ActionableWidget`] with [`Text`] containing a
//! [`ropey::Rope`] and a [`any_rope::Rope`] as its backing, unlike
//! most other widgets, that just use [`String`]s and [`Vec`]s.
//!
//! Most extensible features of Parsec have the primary purpose of
//! serving the [`FileWidget`], such as multiple [`Cursor`]s, a
//! [`History`] system, [`PrintInfo`], etc.
//!
//! [`FileWidget`]s can have attached extensions called
//! [`Observer`]s, that can read the [`Text`] within, and are also
//! notified of any [`Change`][crate::history::Change]s made to the
//! file.
//!
//! The [`FileWidget`] also provides a list of printed lines
//! through the [`printed_lines()`][FileWidget::printed_lines()`]
//! method. This method is notably used by the
//! [`LineNumbers`][crate::widgets::LineNumbers] widget, that shows
//! the numbers of the currently printed lines.
use std::{fs, path::PathBuf};

use super::{ActiveWidget, ActiveWidgetCfg, PassiveWidget, Widget};
use crate::{
    data::RwData,
    input::{Editor, InputMethod},
    text::{IterCfg, PrintCfg, Text},
    ui::{Area, PushSpecs, Ui},
};

#[derive(Clone)]
pub struct FileCfg<I>
where
    I: InputMethod<Widget = File>,
{
    path: Option<PathBuf>,
    input: I,
    cfg: PrintCfg,
    specs: PushSpecs,
}

impl FileCfg<Editor> {
    pub fn new() -> Self {
        FileCfg {
            path: None,
            input: Editor::new(),
            cfg: PrintCfg::default_for_files(),
            // Kinda arbitrary.
            specs: PushSpecs::above(),
        }
    }
}

impl Default for FileCfg<Editor> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I> FileCfg<I>
where
    I: InputMethod<Widget = File>,
{
    pub(crate) fn build<U>(self) -> (Widget<U>, Box<dyn Fn() -> bool>)
    where
        U: Ui,
    {
        let contents = self
            .path
            .as_ref()
            .and_then(|path| match std::fs::read_to_string(path) {
                Ok(contents) => Some(contents),
                Err(_) => None,
            });

        let full_path = self.path.map(|path| {
            let file_name = path.file_name().unwrap();
            std::env::current_dir().unwrap().join(file_name)
        });

        let mut text = Text::new(contents.unwrap_or(String::from("\n")));

        #[cfg(feature = "wacky-colors")]
        {
            use crate::{
                forms::Form,
                palette::palette,
                text::{text, Marker, Tag},
            };
            let marker = Marker::new();
            let form1 = palette().set_form("form1lmao", Form::new().red());
            let form2 = palette().set_form("form2lmao", Form::new().on_blue());
            for i in (0..20000).step_by(50) {
                text.insert(i, Tag::GhostText(text!("\n")), marker);
                text.insert(i, Tag::GhostText(text!("\n")), marker);
                text.insert(i, Tag::GhostText(text!("\n")), marker);
                text.insert(i, Tag::GhostText(text!("\n")), marker);
                text.insert(i, Tag::GhostText(text!("\n")), marker);
                text.insert(
                    i,
                    Tag::GhostText(text!(
                    "\nhello everynyan\n" [form1lmao]
                    "How are you?\n" [form2lmao]
                    "Fine. Thank you.\n\n")),
                    marker,
                );
            }
            text.insert(110, Tag::PopForm(form1), marker);
            text.insert(10, Tag::PushForm(form1), marker);
            text.insert(15, Tag::PopForm(form1), marker);
            text.insert(30, Tag::PushForm(form2), marker);
            text.insert(80, Tag::PopForm(form2), marker);
            text.insert(80, Tag::PushForm(form2), marker);
            text.insert(90, Tag::PopForm(form2), marker);
        }

        text.add_cursor_tags(self.input.cursors().unwrap());

        (
            Widget::active(
                File {
                    path: full_path,
                    text,
                    cfg: self.cfg,
                    printed_lines: Vec::new(),
                },
                RwData::new(self.input),
            ),
            Box::new(|| false),
        )
    }

    pub fn open(self, path: PathBuf) -> Self {
        Self {
            path: Some(path),
            ..self
        }
    }

    pub fn with_print_cfg(self, cfg: PrintCfg) -> Self {
        Self { cfg, ..self }
    }
}

impl<I> ActiveWidgetCfg for FileCfg<I>
where
    I: InputMethod<Widget = File> + Clone,
{
    type Widget = File;
    type WithInput<NewI> = FileCfg<NewI> where NewI: InputMethod<Widget = Self::Widget> + Clone;

    fn builder<U: Ui>(self) -> impl FnOnce() -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        let specs = self.specs;
        move || {
            let (widget, checker) = self.build();
            (widget, checker, specs)
        }
    }

    fn with_input<NewI>(self, input: NewI) -> Self::WithInput<NewI>
    where
        NewI: InputMethod<Widget = File> + Clone,
    {
        Self::WithInput {
            input,
            cfg: self.cfg,
            specs: self.specs,
            path: self.path,
        }
    }
}

/// The widget that is used to print and edit files.
pub struct File {
    path: Option<PathBuf>,
    text: Text,
    cfg: PrintCfg,
    printed_lines: Vec<(usize, bool)>,
}

impl File {
    pub fn write(&self) -> Result<usize, String> {
        if let Some(path) = &self.path {
            self.text.write_to(std::io::BufWriter::new(
                fs::File::create(path).map_err(|err| err.to_string())?,
            ))
        } else {
            Err(String::from("No path given to write to"))
        }
    }

    pub fn write_to(&self, path: impl AsRef<str>) -> Result<usize, String> {
        self.text.write_to(std::io::BufWriter::new(
            fs::File::create(path.as_ref()).map_err(|err| err.to_string())?,
        ))
    }

    /// The number of bytes in the file.
    pub fn len_bytes(&self) -> usize {
        self.text.len_bytes()
    }

    /// Returns the currently printed set of lines.
    pub fn printed_lines(&self) -> &[(usize, bool)] {
        &self.printed_lines
    }

    /// The file's name.
    pub fn name(&self) -> Option<String> {
        self.path.as_ref().and_then(|path| {
            path.file_name()
                .map(|file| file.to_string_lossy().to_string())
        })
    }

    /// The full path of the file.
    pub fn full_path(&self) -> String {
        self.path
            .as_ref()
            .map(|path| path.to_string_lossy().to_string())
            .unwrap_or(String::from("scratch file"))
    }

    /// The number of [`char`]s in the file.
    pub fn len_chars(&self) -> usize {
        self.text.len_chars()
    }

    /// The number of lines in the file.
    pub fn len_lines(&self) -> usize {
        self.text.len_lines()
    }

    fn set_printed_lines(&mut self, area: &impl Area) {
        let start = area.first_char();

        let mut last_line_num = area
            .rev_print_iter(self.text.rev_iter_at(start), IterCfg::new(&self.cfg))
            .find_map(|(caret, item)| caret.wrap.then_some(item.line));

        self.printed_lines = area
            .print_iter_from_top(self.text(), IterCfg::new(&self.cfg))
            .filter_map(|(caret, item)| caret.wrap.then_some(item.line))
            .map(|line| {
                let wrapped = last_line_num.is_some_and(|last_line_num| last_line_num == line);
                last_line_num = Some(line);
                (line, wrapped)
            })
            .take(area.height())
            .collect();
    }
}

impl PassiveWidget for File {
    fn build<U>() -> (Widget<U>, Box<dyn Fn() -> bool>, crate::ui::PushSpecs)
    where
        U: Ui,
        Self: Sized,
    {
        Self::cfg().builder()()
    }

    fn update(&mut self, _area: &impl Area) {}

    fn text(&self) -> &Text {
        &self.text
    }

    fn print_cfg(&self) -> &PrintCfg {
        &self.cfg
    }

    fn print(&mut self, area: &impl Area)
    where
        Self: Sized,
    {
        self.set_printed_lines(area);
        area.print(self.text(), self.print_cfg(), &crate::PALETTE)
    }
}

impl ActiveWidget for File {
    type Config = FileCfg<Editor>
    where
        Self: Sized;

    fn cfg() -> FileCfg<Editor> {
        FileCfg::new()
    }

    fn mut_text(&mut self) -> &mut Text {
        &mut self.text
    }
}

unsafe impl Send for File {}
unsafe impl Sync for File {}
