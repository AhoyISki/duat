use std::{
    cmp::min,
    fmt::Display,
    io::{stdout, Stdout},
    sync::{Arc, Mutex, MutexGuard},
};

use crossterm::{
    cursor::{self, MoveTo, RestorePosition, SavePosition, SetCursorStyle},
    execute, queue,
    style::{ContentStyle, Print, ResetColor, SetStyle},
    terminal,
};
use parsec_core::{
    config::{RoData, RwData},
    tags::{
        form::CursorStyle,
        form::{Form, DEFAULT_ID},
    },
    text::{PrintInfo, Text, TextLine, TextLineBuilder},
    ui::{self, Area as UiArea, Axis, EndNode, Window, Side, Split},
    widgets::{file_widget::FileWidget, NormalWidget, Widget},
};
use unicode_width::UnicodeWidthChar;

static mut PRINTER: Mutex<()> = Mutex::new(());
static mut LOCK: Option<MutexGuard<()>> = None;
static mut SHOW_CURSOR: bool = false;

pub struct Ui {
    initial: bool,
    areas: Vec<Area>,
}

impl Default for Ui {
    fn default() -> Self {
        Ui { initial: true, areas: Vec::new() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Coord {
    x: u16,
    y: u16,
}

impl Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.y, self.x))
    }
}

#[derive(Clone)]
enum Owner {
    Parent { parent: Box<Area>, self_index: usize },
    TlAnchor,
    TrAnchor,
    BlAnchor,
    BrAnchor,
    None,
}

#[derive(Clone)]
struct InnerArea {
    tl: Coord,
    br: Coord,
    split: Split,
}

impl InnerArea {
    fn new(tl: Coord, br: Coord, split: Split) -> Self {
        Self { tl, br, split }
    }

    fn len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis { self.width() } else { self.height() }
    }

    fn width(&self) -> usize {
        (self.br.x - self.tl.x) as usize
    }

    fn height(&self) -> usize {
        (self.br.y - self.tl.y - 1) as usize
    }

    fn add_or_take(&mut self, do_add: bool, remaining: usize, side: Side) -> (Coord, usize) {
        if do_add {
            self.add_to_side(remaining, side)
        } else {
            self.take_from_side(remaining, side)
        }
    }

    fn take_from_side(&mut self, len: usize, side: Side) -> (Coord, usize) {
        let Split::Minimum(min_len) = self.split else {
            (self.coord_from_side(side), len)
        };
        let len_diff = min(self.len(Axis::from(side)) - min_len, len);

        match side {
            Side::Top => self.tl.y -= len_diff,
            Side::Right => self.br.x -= len_diff,
            Side::Bottom => self.br.y -= len_diff,
            Side::Left => self.tl.x -= len_diff,
        }

        (self.coord_from_side(side), len - len_diff)
    }

    fn add_to_side(&mut self, len: usize, side: Side) -> (Coord, usize) {
        let Split::Minimum(min_len) = self.split else {
            (self.coord_from_side(side), len)
        };

        match side {
            Side::Top => self.tl.y += len,
            Side::Right => self.br.x += len,
            Side::Bottom => self.br.y += len,
            Side::Left => self.tl.x += len,
        }

        (self.coord_from_side(side), 0)
    }

    fn coord_from_side(&self, side: Side) -> Coord {
        if let Side::Top | Side::Right = side {
            Coord { x: self.br.x, y: self.tl.y }
        } else {
            Coord { x: self.tl.x, y: self.br.y }
        }
    }

    fn set_tl(&mut self, tl: Coord) {
        let (width, height) = (self.width(), self.height());
        self.tl = tl;
        self.br.x = tl.x + width;
        self.br.y = tl.y + height;
    }

    fn set_br(&mut self, br: Coord) {
        let (width, height) = (self.width(), self.height());
        self.br = br;
        self.tl.x = br.x - width;
        self.tl.y = br.y - height;
    }

    fn set_coords(&mut self, inner: InnerArea) {
        self.tl = inner.tl;
        self.br = inner.br;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Area {
    Container { inner: RwData<InnerArea>, owner: Owner, children: Vec<Area>, axis: Axis },
    Label { inner: RwData<InnerArea>, owner: Owner },
}

impl Area {
    pub fn new(inner: RwData<InnerArea>, owner: Owner) -> Self {
        Self { inner, owner }
    }

    fn set_child_len(&mut self, new_len: usize, index: usize, side: Side) -> Result<(), ()> {
        let Area::Container { inner, owner, children, axis } = self else {
            panic!("Supposed to be unreachable");
        };

        let target = children.get(index).unwrap();
        let (old_len, target_inner) = (target.width(), target.inner().write());
        let mut remaining = old_len.abs_diff(new_len);
        let mut last_corner = parent.tl;
        (last_corner, remaining) = target_inner.add_or_take(old_len < new_len, remaining, side);

        if let Side::Right | Side::Bottom = side {
            let mut children = children.iter_mut().skip(index + 1);

            while let (Some(child), true) = (children.next(), remaining > 0) {
                let inner = child.inner_mut().write();
                inner.set_tl(last_corner);
                inner.add_or_take(old_len > new_len, remaining, side);
            }
        } else {
            let mut children = children.iter_mut().take(index);

            while let (Some(child), true) = (children.next(), remaining > 0) {
                let inner = child.inner_mut().write();
                inner.set_br(last_corner);
                inner.add_or_take(old_len > new_len, remaining, side);
            }
        }

        Ok(())
    }

    fn regulate_children(&mut self, axis: Axis) {
        let Area::Container { inner, owner, children, axis } = self else {
            panic!("Supposed to be unreachable");
        };
        
        let mut last_tl = self.tl;
        let (old_len, new_len) = if let Axis::Horizontal = axis {
            (self.resizable_width(), self.width())
        } else {
            (self.resizable_width(), self.height())
        };

        for child in children.iter_mut().take(self.children.len() - 1) {
            let inner = child.inner_mut().write();
            inner.tl = last_tl;

            (inner.br, last_tl) = if let Axis::Horizontal = axis {
                let ratio = (inner.width() as f32) / (old_len as f32);
                (
                    Coord { x: inner.tl.x + (ratio * (new_len as f32)).floor(), y: self.br.y },
                    Coord { x: inner.tl.x + (ratio * (new_len as f32)).floor(), y: self.tl.y },
                )
            } else {
                let ratio = (inner.height() as f32) / (old_len as f32);
                (
                    Coord { x: self.br.x, y: inner.tl.y + (ratio * (new_len as f32)).floor() },
                    Coord { x: inner.tl.x, y: self.br.y },
                )
            };
        }

        children.last_mut().map(|last| last.inner_mut().write().br = self.br);
    }

    fn resizable_on(&self, axis: Axis) -> bool {
        if let Axis::Horizontal = axis {
            self.inner().read().resizable_width() != 0
        } else {
            self.inner().read().resizable_height() != 0
        }
    }

    fn resizable_width(&self) -> usize {
        let Split::Minimum(_) = self.inner().split else {
            return 0;
        };

        let Area::Container { inner, owner, children, axis } = self else {
            panic!("Supposed to be unreachable");
        };

        if children.is_empty() {
            self.width()
        } else {
            children.iter().map(|child| child.read().modifiable_width()).sum()
        }
    }

    fn resizable_height(&self) -> usize {
        let Split::Minimum(_) = self.split else {
            return 0;
        };

        if self.children.is_empty() {
            self.height
        } else {
            self.children.iter().map(|child| child.read().modifiable_height()).sum()
        }
    }

    fn inner(&self) -> &RwData<InnerArea> {
        match self {
            Area::Container { inner, .. } => inner,
            Area::Label { inner, .. } => inner,
        }
    }

    fn inner_mut(&mut self) -> &mut RwData<InnerArea> {
        match self {
            Area::Container { inner, .. } => inner,
            Area::Label { inner, .. } => inner,
        }
    }
}

impl Display for Area {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("({})({})", self.tl, self.br))
    }
}

impl ui::Area for Area {
    fn width(&self) -> usize {
        self.inner.read().width()
    }

    fn height(&self) -> usize {
        self.inner.read().height()
    }

    fn request_len(&mut self, width: usize, side: Side) -> Result<(), ()> {
        match self.owner {
            Owner::Parent { parent, self_index, axis } => {
                if Axis::from(side) != axis {
                    parent.request_len(width, side)
                } else if parent.resizable_width() < width {
                    let new_parent_width = parent.width() + width - self.width();
                    parent.request_len(new_parent_width, side)
                } else {
                    parent.set_child_len(width, self_index, side)
                }
            }
            Owner::TlAnchor => todo!(),
            Owner::TrAnchor => todo!(),
            Owner::BlAnchor => todo!(),
            Owner::BrAnchor => todo!(),
            Owner::None => Err(()),
        }
    }

    fn request_height(&mut self, height: usize) -> Result<(), ()> {
        todo!()
    }

    fn request_width_to_fit(&mut self, text: &str) -> Result<(), ()> {
        todo!()
    }
}

impl Area {
    fn total() -> Self {
        let size = terminal::size().unwrap();

        Area { inner: RwData::new(Coord { x: 0, y: 0 }), owner: Owner::None }
    }
}

#[derive(Clone)]
pub struct Container {
    area: Area,
    direction: Side,
}

impl ui::Container<Area> for Container {
    fn area(&self) -> &Area {
        &self.area
    }

    fn area_mut(&mut self) -> &mut Area {
        &mut self.area
    }
}

pub struct Label {
    stdout: Stdout,
    area: Area,
    cursor: Coord,
    style_before_cursor: Option<ContentStyle>,
    last_style: ContentStyle,
    is_active: bool,
    wrap_next: bool,
    indent: usize,
}

impl Label {
    fn new(area: Area, direction: Side) -> Self {
        Label {
            stdout: stdout(),
            area,
            cursor: area.tl,
            style_before_cursor: None,
            last_style: ContentStyle::default(),
            is_active: false,
            wrap_next: false,
            indent: 0,
        }
    }

    fn clear_line(&mut self) {
        if self.cursor.x < self.area.br.x {
            self.clear_form();

            // The rest of the line is featureless.
            let padding_count = (self.area.br.x - self.cursor.x) as usize;
            let padding = " ".repeat(padding_count);
            queue!(self.stdout, Print(padding)).unwrap();
        }

        self.cursor.x = self.area.tl.x;
        self.cursor.y += 1;

        queue!(self.stdout, MoveTo(self.cursor.x, self.cursor.y), SetStyle(self.last_style))
            .unwrap();
    }

    fn wrap_line(&mut self) {
        self.clear_line();

        queue!(self.stdout, MoveTo(self.cursor.x, self.cursor.y), Print(" ".repeat(self.indent)))
            .unwrap();

        self.cursor.x += self.indent as u16;
        self.indent = 0;
        self.wrap_next = false;
    }
}

impl Clone for Label {
    fn clone(&self) -> Self {
        Label { stdout: stdout(), ..*self }
    }
}

impl ui::Label<Area> for Label {
    fn area_mut(&mut self) -> &mut Area {
        &mut self.area
    }

    fn area(&self) -> &Area {
        &self.area
    }

    fn set_form(&mut self, form: Form) {
        self.last_style = form.style;
        queue!(self.stdout, ResetColor, SetStyle(self.last_style)).unwrap();
    }

    fn clear_form(&mut self) {
        queue!(self.stdout, ResetColor).unwrap();
    }

    fn place_primary_cursor(&mut self, cursor_style: CursorStyle) {
        if let (Some(caret), true) = (cursor_style.caret, self.is_active) {
            queue!(self.stdout, caret, SavePosition).unwrap();
            unsafe { SHOW_CURSOR = true }
        } else {
            self.style_before_cursor = Some(self.last_style);
            queue!(self.stdout, SetStyle(cursor_style.form.style)).unwrap();
        }
    }

    fn place_secondary_cursor(&mut self, cursor_style: CursorStyle) {
        self.style_before_cursor = Some(self.last_style);
        queue!(self.stdout, SetStyle(cursor_style.form.style)).unwrap();
    }

    fn set_as_active(&mut self) {
        self.is_active = true;
    }

    fn start_printing(&mut self) {
        unsafe {
            LOCK = Some(PRINTER.lock().unwrap());
        }

        self.cursor = self.area.tl;
        queue!(self.stdout, MoveTo(self.area.tl.x, self.area.tl.y)).unwrap();
        execute!(self.stdout, cursor::Hide).unwrap();

        if self.is_active {
            unsafe { SHOW_CURSOR = false }
        }
    }

    fn stop_printing(&mut self) {
        for _ in self.cursor.y..(self.area.br.y.saturating_sub(2)) {
            let _ = self.next_line();
        }

        self.clear_line();

        execute!(self.stdout, RestorePosition).unwrap();
        if unsafe { SHOW_CURSOR } {
            execute!(self.stdout, cursor::Show).unwrap();
        }

        self.clear_form();
        unsafe { LOCK = None };
        self.is_active = false;
    }

    fn print(&mut self, ch: char) {
        let len = self.get_char_len(ch) as u16;
        if self.cursor.x <= self.area.br.x - len {
            self.cursor.x += len;
            queue!(self.stdout, Print(ch)).unwrap();
            if let Some(style) = self.style_before_cursor.take() {
                queue!(self.stdout, ResetColor, SetStyle(style)).unwrap();
            }
        }
        if self.wrap_next {
            self.wrap_line();
        }
    }

    fn next_line(&mut self) -> Result<(), ()> {
        if self.cursor.y == self.area.br.y - 1 {
            Err(())
        } else {
            self.clear_line();
            Ok(())
        }
    }

    fn wrap_next(&mut self, indent: usize) -> Result<(), ()> {
        if self.cursor.y == self.area.br.y - 1 {
            Err(())
        } else {
            self.wrap_next = true;
            self.indent = indent;
            Ok(())
        }
    }

    fn get_char_len(&self, ch: char) -> usize {
        UnicodeWidthChar::width(ch).unwrap_or(0)
    }
}

impl ui::Ui for Ui {
    type Area = Area;
    type Container = Container;
    type Label = Label;

    fn push_label(
        &mut self, label: &mut Self::Label, side: Side, split: Split,
    ) -> (Self::Container, Self::Label) {
        label.area.request_len(split.len(), side);
        let (split_inner, resized_inner) = split_by(&mut label.area, split, side);
        let (split_area, resized_area) =
            form_family(&mut label.area, split_inner, resized_inner, side);

        let new_label = Label::new(split_inner, side);

        (parent_container, new_label)
    }

    fn push_container(
        &mut self, container: &mut Self::Container, side: Side, split: Split,
    ) -> (Self::Container, Self::Label) {
        let parent_container = container.clone();

        let area = split_by(len, &mut container.area, side);

        let new_label = Label::new(area, side);

        (parent_container, new_label)
    }

    fn maximum_label(&mut self) -> Option<Self::Label> {
        if self.initial {
            self.initial = false;
            Some(Label::new(Area::total(), Side::Left))
        } else {
            None
        }
    }

    fn startup(&mut self) {
        // This makes it so that if the application panics, the panic message is printed
        // nicely and the terminal is left in a usable state.
        use std::panic::set_hook;
        set_hook(Box::new(|msg| {
            let mut stdout = stdout();

            terminal::disable_raw_mode().unwrap();

            execute!(stdout, terminal::Clear, terminal::LeaveAlternateScreen).unwrap();

            use std::backtrace::Backtrace;
            println!("{}\n\n{}", msg, Backtrace::force_capture());
        }));

        let mut stdout = stdout();
        terminal::enable_raw_mode().unwrap();
        execute!(stdout, terminal::EnterAlternateScreen).unwrap();
    }

    fn shutdown(&mut self) {
        let mut stdout = stdout();

        execute!(stdout, terminal::Clear, terminal::LeaveAlternateScreen,).unwrap();

        terminal::disable_raw_mode().unwrap();
    }

    fn finish_all_printing(&mut self) {}
}

fn get_split_len(split: Split, area: Area, direction: Side) -> u16 {
    let current_len = match direction {
        Side::Left | Side::Right => area.width(),
        Side::Top | Side::Bottom => area.height(),
    };

    match split {
        Split::Locked(len) | Split::Static(len) => min(len, current_len) as u16,
        Split::Ratio(ratio) => (current_len as f32 * ratio).floor() as u16,
    }
}

fn split_by(area: &mut Area, split: Split, side: Side) -> (InnerArea, InnerArea) {
    let old_inner = area.inner.read();
    let mut resized_inner = old_inner.clone();
    let split_inner = match side {
        Side::Left => {
            resized_inner.tl.x += split.len();
            InnerArea::new(old_inner.tl, Coord { x: resized_inner.tl.x, y: old_inner.br.y }, split)
        }
        Side::Right => {
            resized_inner.br.x -= split.len();
            InnerArea::new(Coord { x: resized_inner.br.x, y: old_inner.tl.y }, old_inner.br, split)
        }
        Side::Top => {
            resized_inner.tl.y += split.len();
            InnerArea::new(old_inner.tl, Coord { x: old_inner.br.x, y: resized_inner.tl.y }, split)
        }
        Side::Bottom => {
            resized_inner.br.y -= split.len();
            InnerArea::new(Coord { x: old_inner.tl.x, y: resized_inner.br.y }, old_inner.br, split)
        }
    };

    (split_inner, resized_inner)
}

fn form_family(
    area: &mut Area, split_inner: InnerArea, mut resized_inner: InnerArea, side: Side,
) -> (Area, Option<Area>) {
    let old_inner = area.inner.write();
    let split_area = Area::new(RwData::new(split_inner), Owner::None);

    #[rustfmt::skip]
    if let Owner::Parent { parent, self_index } = &mut area.owner && let parent_inner = parent.inner.write() && parent_inner.children.unwrap {
        let parent_inner = parent.inner.write();

        let split_index =
            if let Side::Top | Side::Left = side { self_index } else { self_index + 1 };

        for child in parent_inner.children.unwrap().0.iter_mut().skip(split_index) {
            if let Owner::Parent { self_index, .. } = &mut child.onwer {
                *self_index += 1;
            }
        }
        parent_inner.children.unwrap().0.insert(split_index, split_area.clone());

        old_inner.set_coords(resized_inner);
        split_area.owner = Owner::Parent { parent: parent.clone(), self_index: split_index };
        (split_area, None)
    } else {
        resized_inner.children = std::mem::take(&mut old_inner.children);
        let split_index = if let Side::Top | Side::Left = side { 1 } else { 0 };
        let resized_area = Area::new(RwData::new(resized_inner), Owner::Parent {
            parent: area.clone(),
            self_index: (split_index + 1) % 2,
        });

		let mut children = vec![resized_area];
		children.insert(split_index, split_area.clone());
        old_inner.children = Some((children, Axis::from(side)));

        split_area.owner = Owner::Parent { parent: area.clone(), self_index: split_index };
        (split_area, Some(resized_area))
    }
}

pub enum SeparatorChar {
    Uniform(char),
    DifferentOnMain(char, char),
    ThreeWay(char, char, char),
}

impl Default for SeparatorChar {
    fn default() -> Self {
        SeparatorChar::Uniform('│')
    }
}

impl SeparatorChar {
    fn get_char(&self, line_number: usize, main_line: usize) -> char {
        match self {
            SeparatorChar::Uniform(ch) => *ch,
            SeparatorChar::DifferentOnMain(other_ch, main_ch) => {
                if line_number == main_line {
                    *main_ch
                } else {
                    *other_ch
                }
            }
            SeparatorChar::ThreeWay(lower_ch, main_ch, higher_ch) => {
                if line_number < main_line {
                    *lower_ch
                } else if line_number > main_line {
                    *higher_ch
                } else {
                    *main_ch
                }
            }
        }
    }
}

pub enum SeparatorForm {
    Uniform(TextLineBuilder),
    DifferentOnMain(TextLineBuilder, TextLineBuilder),
    ThreeWay(TextLineBuilder, TextLineBuilder, TextLineBuilder),
}

impl Default for SeparatorForm {
    fn default() -> Self {
        SeparatorForm::Uniform(TextLineBuilder::from([DEFAULT_ID, DEFAULT_ID]))
    }
}

impl SeparatorForm {
    pub fn uniform<U>(node: &RwData<EndNode<U>>, name: impl ToString) -> Self
    where
        U: ui::Ui,
    {
        let node = node.read();
        let palette = node.palette().read();
        let (_, id) = palette.get_from_name(name);

        SeparatorForm::Uniform(TextLineBuilder::from([id, DEFAULT_ID]))
    }

    pub fn different_on_main<U, S>(node: &RwData<EndNode<U>>, main_name: S, other_name: S) -> Self
    where
        U: ui::Ui,
        S: ToString,
    {
        let node = node.read();
        let palette = node.palette().read();
        let (_, main_id) = palette.get_from_name(main_name);
        let (_, other_id) = palette.get_from_name(other_name);

        SeparatorForm::DifferentOnMain(
            TextLineBuilder::from([main_id, DEFAULT_ID]),
            TextLineBuilder::from([other_id, DEFAULT_ID]),
        )
    }

    pub fn three_way<U, S>(
        node: &RwData<EndNode<U>>, main_name: S, lower_name: S, higher_name: S,
    ) -> Self
    where
        U: ui::Ui,
        S: ToString,
    {
        let node = node.read();
        let palette = node.palette().read();
        let (_, main_id) = palette.get_from_name(main_name);
        let (_, lower_id) = palette.get_from_name(lower_name);
        let (_, higher_id) = palette.get_from_name(higher_name);

        SeparatorForm::ThreeWay(
            TextLineBuilder::from([main_id, DEFAULT_ID]),
            TextLineBuilder::from([lower_id, DEFAULT_ID]),
            TextLineBuilder::from([higher_id, DEFAULT_ID]),
        )
    }

    fn form_line(&self, line_number: usize, main_line: usize, text: String) -> TextLine {
        match self {
            SeparatorForm::Uniform(builder) => builder.form_text_line(text),
            SeparatorForm::DifferentOnMain(other_builder, main_builder) => {
                if line_number == main_line {
                    main_builder.form_text_line(text)
                } else {
                    other_builder.form_text_line(text)
                }
            }
            SeparatorForm::ThreeWay(lower_builder, main_builder, higher_builder) => {
                if line_number < main_line {
                    lower_builder.form_text_line(text)
                } else if line_number > main_line {
                    higher_builder.form_text_line(text)
                } else {
                    main_builder.form_text_line(text)
                }
            }
        }
    }
}

#[derive(Default)]
pub struct VertRuleConfig {
    pub separator_char: SeparatorChar,
    pub separator_form: SeparatorForm,
    pub print_on_empty: bool,
}

pub struct VertRule<U>
where
    U: ui::Ui,
{
    end_node: RwData<EndNode<U>>,
    file: RoData<FileWidget<U>>,
    text: Text<U>,
    vert_rule_config: VertRuleConfig,
}

impl<U> VertRule<U>
where
    U: ui::Ui + 'static,
{
    /// Returns a new instance of `Box<VerticalRuleConfig>`, taking a user provided config.
    pub fn new(
        end_node: RwData<EndNode<U>>, _: &mut Window<U>, file_widget: RwData<FileWidget<U>>,
        vert_rule_config: VertRuleConfig,
    ) -> Widget<U> {
        let file = RoData::from(&file_widget);

        Widget::Normal(Arc::new(Mutex::new(VertRule {
            end_node,
            file,
            text: Text::default(),
            vert_rule_config,
        })))
    }

    /// Returns a new instance of `Box<VerticalRuleConfig>`, using the default config.
    pub fn default(
        node: RwData<EndNode<U>>, _: &mut Window>, file_widget: RwData<FileWidget<U>>,
    ) -> Widget<U> {
        let file = RoData::from(&file_widget);

        Widget::Normal(Arc::new(Mutex::new(VertRule {
            end_node: node,
            file,
            text: Text::default(),
            vert_rule_config: VertRuleConfig::default(),
        })))
    }
}

unsafe impl<U> Send for VertRule<U> where U: ui::Ui {}

impl<U> NormalWidget<U> for VertRule<U>
where
    U: ui::Ui + 'static,
{
    fn identifier(&self) -> String {
        String::from("vertical_rule")
    }

    fn end_node(&self) -> &RwData<EndNode<U>> {
        &self.end_node
    }

    fn end_node_mut(&mut self) -> &mut RwData<EndNode<U>> {
        &mut self.end_node
    }

    fn update(&mut self) {
        let file = self.file.read();
        let node = self.end_node.read();
        let label = node.label().read();
        let area = label.area();

        self.text.lines.clear();

        let mut iterations = file.printed_lines();
        if self.vert_rule_config.print_on_empty {
            let element_beyond = *iterations.last().unwrap() + 1;
            iterations.extend_from_slice(
                vec![element_beyond; area.height().saturating_sub(iterations.len())].as_slice(),
            );
        }

        let main_line = file.main_cursor().true_row();

        for number in iterations {
            let ch = self.vert_rule_config.separator_char.get_char(number, main_line);

            let line = String::from("[]") + String::from(ch).as_str() + "[]\n";
            let line = self.vert_rule_config.separator_form.form_line(number, main_line, line);

            self.text.lines.push(line);
        }
    }

    fn needs_update(&self) -> bool {
        self.file.has_changed()
    }

    fn text(&self) -> &Text<U> {
        &self.text
    }

    fn members_for_printing(&mut self) -> (&Text<U>, &mut RwData<EndNode<U>>, PrintInfo) {
        (&self.text, &mut self.end_node, PrintInfo::default())
    }
}
