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

use super::{ActiveWidget, PassiveWidget, Widget, WidgetCfg};
use crate::{
    data::RwData,
    input::{Editor, InputMethod},
    palette,
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
                global::palette,
                palette::Form,
                text::{text, Marker, Tag},
            };
            let marker = Marker::new();
            let form1 = palette().set_form("form1lmao", Form::new().red());
            let form2 = palette().set_form("form2lmao", Form::new().on_blue());
            for i in (0..100000).step_by(250) {
                text.insert(i, Tag::ConcealStart, marker);
                text.insert(i + 60, Tag::ConcealEnd, marker);
                text.insert(i, Tag::ConcealStart, marker);
                text.insert(i + 30, Tag::ConcealEnd, marker);
                text.insert(
                    i,
                    Tag::GhostText(text!(
                        "《施氏食狮史》\n石室诗士施氏，嗜狮，誓食十狮。\n氏时时适市视狮。\n十时，\
                         适十狮适市。\n是时，适施氏适市。\n氏视是十狮，恃矢势，使是十狮逝世。\\
                         \
                         n氏拾是十狮尸，适石室。\n石室湿，氏使侍拭石室。\n石室拭，氏始试食是十狮。\
                         \n食时，始识是十狮尸，实十石狮尸。\n试释是事。"
                    )),
                    marker,
                );
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
                    path: match full_path {
                        Some(path) => Path::Set(path),
                        None => Path::new_unset(),
                    },
                    text,
                    cfg: self.cfg,
                    printed_lines: Vec::new(),
                    related_widgets: Vec::new(),
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

    pub fn with_input<NewI>(self, input: NewI) -> FileCfg<NewI>
    where
        NewI: InputMethod<Widget = File> + Clone,
    {
        FileCfg {
            input,
            cfg: self.cfg,
            specs: self.specs,
            path: self.path,
        }
    }
}

impl<I> WidgetCfg for FileCfg<I>
where
    I: InputMethod<Widget = File> + Clone,
{
    type Widget = File;

    fn build<U: Ui>(self) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        let specs = self.specs;
        let (widget, checker) = self.build();
        (widget, checker, specs)
    }
}

/// The widget that is used to print and edit files.
pub struct File {
    path: Path,
    text: Text,
    cfg: PrintCfg,
    printed_lines: Vec<(usize, bool)>,
    related_widgets: Vec<(RwData<dyn PassiveWidget>, &'static str, Box<dyn Area>)>,
}

impl File {
    pub fn cfg() -> FileCfg<Editor> {
        FileCfg::new()
    }

    pub fn write(&self) -> Result<usize, String> {
        if let Path::Set(path) = &self.path {
            self.text
                .write_to(std::io::BufWriter::new(
                    fs::File::create(path).map_err(|err| err.to_string())?,
                ))
                .map_err(|err| err.to_string())
        } else {
            Err(String::from(
                "The file has no associated path, and no path was given to write to",
            ))
        }
    }

    pub fn write_to(&self, path: impl AsRef<str>) -> std::io::Result<usize> {
        self.text
            .write_to(std::io::BufWriter::new(fs::File::create(path.as_ref())?))
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
    pub fn name(&self) -> String {
        match &self.path {
            Path::Set(path) => path.file_name().unwrap().to_string_lossy().to_string(),
            Path::UnSet(id) => format!("*scratch file {id}*"),
        }
    }

    pub fn set_name(&self) -> Option<String> {
        match &self.path {
            Path::Set(path) => Some(path.file_name().unwrap().to_string_lossy().to_string()),
            Path::UnSet(_) => None,
        }
    }

    /// The full path of the file.
    pub fn full_path(&self) -> String {
        match &self.path {
            Path::Set(path) => path.to_string_lossy().to_string(),
            Path::UnSet(id) => format!("*scratch file*#{id}"),
        }
    }

    /// The number of [`char`]s in the file.
    pub fn len_chars(&self) -> usize {
        self.text.len_chars()
    }

    /// The number of lines in the file.
    pub fn len_lines(&self) -> usize {
        self.text.len_lines()
    }

    pub(crate) fn add_related_widget(
        &mut self,
        related: (RwData<dyn PassiveWidget>, &'static str, Box<dyn Area>),
    ) {
        self.related_widgets.push(related)
    }

    pub(crate) fn inspect_related<W: 'static, R>(&self, f: impl FnOnce(&W) -> R) -> Option<R> {
        self.related_widgets
            .iter()
            .find(|(widget, ..)| widget.data_is::<W>())
            .and_then(|(widget, ..)| widget.inspect_as::<W, R>(f))
    }

    pub(crate) fn mutate_related_widget<W: 'static, R>(
        &mut self,
        f: impl FnOnce(&mut W, &mut dyn Area) -> R,
    ) -> Option<R> {
        self.related_widgets
            .iter_mut()
            .find(|(widget, ..)| widget.data_is::<W>())
            .and_then(|(widget, _, area)| {
                widget.mutate_as::<W, R>(|widget| f(widget, area.as_mut()))
            })
    }

    pub(crate) fn get_related_widget(&self, type_name: &str) -> Option<RwData<dyn PassiveWidget>> {
        self.related_widgets
            .iter()
            .find(|(_, cmp, _)| *cmp == type_name)
            .map(|(widget, ..)| widget)
            .cloned()
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
    fn build<U>() -> (Widget<U>, impl Fn() -> bool, crate::ui::PushSpecs)
    where
        U: Ui,
        Self: Sized,
    {
        let (widget, checker) = Self::cfg().build();
        (widget, checker, PushSpecs::above())
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
        area.print(self.text(), self.print_cfg(), palette::painter())
    }
}

impl ActiveWidget for File {
    fn mut_text(&mut self) -> &mut Text {
        &mut self.text
    }
}

unsafe impl Send for File {}
unsafe impl Sync for File {}

enum Path {
    Set(PathBuf),
    UnSet(usize),
}

impl Path {
    fn new_unset() -> Path {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static UNSET_COUNT: AtomicUsize = AtomicUsize::new(1);

        Path::UnSet(UNSET_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}
