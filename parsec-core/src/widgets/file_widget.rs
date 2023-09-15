//! The primary widget of Parsec, used to display files.
//!
//! The [`FileWidget<U>`] is Parsec's way of display text files. It is
//! an [`ActionableWidget`] with [`Text`] containing a
//! [`ropey::Rope`] and a [`any_rope::Rope`] as its backing, unlike
//! most other widgets, that just use [`String`]s and [`Vec`]s.
//!
//! Most extensible features of Parsec have the primary purpose of
//! serving the [`FileWidget<U>`], such as multiple [`Cursor`]s, a
//! [`History`] system, [`PrintInfo`], etc.
//!
//! [`FileWidget<U>`]s can have attached extensions called
//! [`Observer`]s, that can read the [`Text`] within, and are also
//! notified of any [`Change`][crate::history::Change]s made to the
//! file.
//!
//! The [`FileWidget<U>`] also provides a list of printed lines
//! through the [`printed_lines()`][FileWidget::printed_lines()`]
//! method. This method is notably used by the
//! [`LineNumbers<U>`][crate::widgets::LineNumbers] widget, that shows
//! the numbers of the currently printed lines.
use std::{fs::File, path::PathBuf};

use super::{ActiveWidget, ActiveWidgetCfg, PassiveWidget, Widget};
use crate::{
    data::{AsAny, ReadableData, RwData},
    input::{Editor, InputMethod},
    text::{IterCfg, PrintCfg, Text},
    ui::{Area, PushSpecs, Ui},
    Controler, log_info,
};

#[derive(Clone)]
pub struct FileWidgetCfg<I>
where
    I: InputMethod<Widget = FileWidget>,
{
    path: Option<PathBuf>,
    input: RwData<I>,
    cfg: PrintCfg,
    specs: PushSpecs,
}

impl FileWidgetCfg<Editor> {
    pub fn new() -> Self {
        FileWidgetCfg {
            path: None,
            input: RwData::new(Editor::new()),
            cfg: PrintCfg::default_for_files(),
            // Kinda arbitrary.
            specs: PushSpecs::above(),
        }
    }
}

impl Default for FileWidgetCfg<Editor> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I> FileWidgetCfg<I>
where
    I: InputMethod<Widget = FileWidget>,
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

        let mut text = Text::new_rope(contents.unwrap_or(String::from("\n")));

        #[cfg(feature = "wacky-colors")]
        {
            use crate::text::{Handle, Tag};
            let mut tagger = text.tag_with(Handle::new());
            let mut pushes_pops_you_cant_explain_that = true;
            for index in (20..tagger.len_chars()).step_by(30) {
                if pushes_pops_you_cant_explain_that {
                    tagger.insert(index, Tag::ghost_from("\n\n   Ayy lmao   \n\n"));
                    tagger.insert(index, Tag::PushForm(crate::forms::SEPARATOR));
                    tagger.insert(index + 11, Tag::PopForm(crate::forms::SEPARATOR));
                } else {
                    tagger.insert(
                        index,
                        Tag::ghost_from("   Hello World\n lmao deal with this"),
                    );
                }
                pushes_pops_you_cant_explain_that = !pushes_pops_you_cant_explain_that
            }
        }

        text.add_cursor_tags(self.input.read().cursors().unwrap());

        (
            Widget::active(
                FileWidget {
                    path: full_path,
                    text,
                    cfg: self.cfg,
                    printed_lines: Vec::new(),
                },
                self.input,
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

impl<I> ActiveWidgetCfg for FileWidgetCfg<I>
where
    I: InputMethod<Widget = FileWidget> + Clone,
{
    type Widget = FileWidget;
    type WithInput<NewI> = FileWidgetCfg<NewI> where NewI: InputMethod<Widget = Self::Widget> + Clone;

    fn builder<U>(
        self,
    ) -> impl FnOnce(&Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
    {
        let specs = self.specs;
        move |_| {
            let (widget, checker) = self.build();
            (widget, checker, specs)
        }
    }

    fn with_input<NewI>(self, input: NewI) -> Self::WithInput<NewI>
    where
        NewI: InputMethod<Widget = FileWidget> + Clone,
    {
        Self::WithInput {
            input: RwData::new(input),
            cfg: self.cfg,
            specs: self.specs,
            path: self.path,
        }
    }
}

/// The widget that is used to print and edit files.
pub struct FileWidget {
    path: Option<PathBuf>,
    text: Text,
    cfg: PrintCfg,
    printed_lines: Vec<(usize, bool)>,
}

impl FileWidget {
    pub fn write(&self) -> Result<usize, String> {
        if let Some(path) = &self.path {
            self.text.write_to(std::io::BufWriter::new(
                File::create(path).map_err(|err| err.to_string())?,
            ))
        } else {
            Err(String::from("No path given to write to"))
        }
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

impl PassiveWidget for FileWidget {
    fn build<U>(
        controler: &Controler<U>,
    ) -> (Widget<U>, Box<dyn Fn() -> bool>, crate::ui::PushSpecs)
    where
        U: Ui,
        Self: Sized,
    {
        Self::config().builder()(controler)
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
        log_info!("{:#?}", self.text.tags);
        self.set_printed_lines(area);
        area.print(self.text(), self.print_cfg(), &crate::PALETTE)
    }
}

impl ActiveWidget for FileWidget {
    type Config = FileWidgetCfg<Editor>
    where
        Self: Sized;

    fn config() -> FileWidgetCfg<Editor> {
        FileWidgetCfg::new()
    }

    fn mut_text(&mut self) -> &mut Text {
        &mut self.text
    }
}

impl AsAny for FileWidget {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

unsafe impl Send for FileWidget {}
unsafe impl Sync for FileWidget {}
