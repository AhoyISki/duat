#![feature(stmt_expr_attributes, is_some_and)]

use std::{
    cmp::{max, min},
    fmt::Display,
    io::{stdout, Stdout},
    sync::{Arc, Mutex, MutexGuard},
};

use crossterm::{
    cursor::{self, MoveTo, RestorePosition, SavePosition},
    execute, queue,
    style::{ContentStyle, Print, ResetColor, SetStyle},
    terminal::{self, ClearType},
};
use parsec_core::{
    config::{RoData, RwData, TabPlaces, WrapMethod},
    tags::{
        form::CursorStyle,
        form::{Form, DEFAULT},
    },
    text::{Text, TextLine, TextLineBuilder},
    ui::{self, Area as UiArea, Axis, EndNode, Label as UiLabel, Side, Split},
    widgets::{file_widget::FileWidget, NormalWidget, Widget},
};
use unicode_width::UnicodeWidthChar;

static mut PRINTER: Mutex<()> = Mutex::new(());
static mut LOCK: Option<MutexGuard<()>> = None;
static mut SHOW_CURSOR: bool = false;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Coord {
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
    Parent { parent: Area, self_index: usize, split: Split },
    TlAnchor,
    TrAnchor,
    BlAnchor,
    BrAnchor,
    None,
}
impl Owner {
    fn split(&self) -> Option<Split> {
        if let Owner::Parent { split, .. } = self { Some(*split) } else { None }
    }

    fn new_parent(parent: Area, self_index: usize, split: Split) -> Self {
        Owner::Parent { parent, self_index, split }
    }

    fn aligns(&mut self, other: Axis) -> Option<&mut Self> {
        if let Owner::Parent { parent, .. } = self {
            if parent.lineage.read().as_ref().is_some_and(|(_, axis)| *axis == other) {
                return Some(self);
            }
        }

        None
    }
}

#[derive(Clone)]
struct InnerArea {
    tl: Coord,
    br: Coord,
    owner: Owner,
}

impl InnerArea {
    fn new(tl: Coord, br: Coord, owner: Owner) -> Self {
        InnerArea { tl, br, owner }
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
        let Some(Split::Minimum(min_len)) = self.owner.split() else {
            return (self.coord_from_side(side), len);
        };
        let len_diff = min(self.len(Axis::from(side)) - min_len, len);

        match side {
            Side::Top => self.tl.y -= len_diff as u16,
            Side::Right => self.br.x -= len_diff as u16,
            Side::Bottom => self.br.y -= len_diff as u16,
            Side::Left => self.tl.x -= len_diff as u16,
        }

        (self.coord_from_side(side), len - len_diff)
    }

    fn add_to_side(&mut self, len: usize, side: Side) -> (Coord, usize) {
        let Some(Split::Minimum(min_len)) = self.owner.split() else {
            return (self.coord_from_side(side), len);
        };

        match side {
            Side::Top => self.tl.y += len as u16,
            Side::Right => self.br.x += len as u16,
            Side::Bottom => self.br.y += len as u16,
            Side::Left => self.tl.x += len as u16,
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
        self.br.x = tl.x + width as u16;
        self.br.y = tl.y + height as u16;
    }

    fn set_br(&mut self, br: Coord) {
        let (width, height) = (self.width(), self.height());
        self.br = br;
        self.tl.x = br.x - width as u16;
        self.tl.y = br.y - height as u16;
    }
}

#[derive(Clone)]
pub struct Area {
    inner: RwData<InnerArea>,
    lineage: RwData<Option<(Vec<Area>, Axis)>>,
}

impl Area {
    fn total() -> Self {
        let (max_x, max_y) = terminal::size().unwrap();
        let tl = Coord { x: 0, y: 0 };
        let br = Coord { x: max_x, y: max_y };

        Area { inner: RwData::new(InnerArea::new(tl, br, Owner::None)), lineage: RwData::new(None) }
    }

    pub fn tl(&self) -> Coord {
        self.inner.read().tl
    }

    pub fn br(&self) -> Coord {
        self.inner.read().br
    }

    fn new(inner: RwData<InnerArea>) -> Self {
        Area { inner, lineage: RwData::new(None) }
    }

    fn set_child_len(&mut self, new_len: usize, index: usize, side: Side) -> Result<(), ()> {
        let Some((children, ..)) = &mut *self.lineage.write() else {
            return Err(());
        };

        let target = children.get_mut(index).unwrap();
        let (old_len, mut target_inner) = (target.width(), target.inner.write());

        let mut remaining = old_len.abs_diff(new_len);
        let mut last_corner;
        (last_corner, remaining) = target_inner.add_or_take(old_len < new_len, remaining, side);
        drop(target_inner);
        drop(target);

        if let Side::Right | Side::Bottom = side {
            let mut children = children.iter_mut().skip(index + 1);

            while let (Some(child), true) = (children.next(), remaining > 0) {
                let mut inner = child.inner.write();
                inner.set_tl(last_corner);
                (last_corner, remaining) = inner.add_or_take(old_len > new_len, remaining, side);
                drop(inner);

                child.regulate_children();
            }
        } else {
            let mut children = children.iter_mut().take(index);

            while let (Some(child), true) = (children.next(), remaining > 0) {
                let mut inner = child.inner.write();
                inner.set_br(last_corner);
                (last_corner, remaining) = inner.add_or_take(old_len > new_len, remaining, side);
                drop(inner);

                child.regulate_children();
            }
        }

        Ok(())
    }

    fn regulate_children(&mut self) {
        let Some((_, axis)) = *self.lineage.write() else {
            return;
        };

        let self_inner = self.inner.read();

        let mut last_tl = self_inner.tl;
        let (old_len, new_len) = if let Axis::Horizontal = axis {
            (self.resizable_width(), self.width())
        } else {
            (self.resizable_width(), self.height())
        };

        let mut lineage = self.lineage.write();
        let (children, axis) = lineage.as_mut().unwrap();
        for child in children.iter_mut() {
            let mut inner = child.inner.write();
            inner.tl = last_tl;

            (inner.br, last_tl) = if let Axis::Horizontal = axis {
                let ratio = (inner.width() as f32) / (old_len as f32);
                let x = inner.tl.x + (ratio * (new_len as f32)).floor() as u16;
                (Coord { x, y: inner.br.y }, Coord { x, y: inner.tl.y })
            } else {
                let ratio = (inner.height() as f32) / (old_len as f32);
                let y = inner.tl.y + (ratio * (new_len as f32)).floor() as u16;
                (Coord { x: inner.br.x, y }, Coord { x: inner.tl.x, y })
            };
        }

        children.last_mut().map(|last| last.inner.write().br = self_inner.br);
    }

    fn resizable_len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis { self.resizable_width() } else { self.resizable_height() }
    }

    fn resizable_width(&self) -> usize {
        let Some(Split::Minimum(_)) = self.inner.read().owner.split() else {
            return 0;
        };

        if let Some((children, ..)) = &*self.lineage.read() {
            if children.is_empty() {
                self.width()
            } else {
                children.iter().map(|child| child.resizable_width()).sum()
            }
        } else {
            self.width()
        }
    }

    fn resizable_height(&self) -> usize {
        let Some(Split::Minimum(_)) = self.inner.read().owner.split() else {
            return 0;
        };

        if let Some((children, ..)) = &*self.lineage.read() {
            if children.is_empty() {
                self.height()
            } else {
                children.iter().map(|child| child.resizable_height()).sum()
            }
        } else {
            self.height()
        }
    }
}

impl Display for Area {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("({})({})", self.inner.read().tl, self.inner.read().br))
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
        let InnerArea { tl, br, owner } = &mut *self.inner.write();

        match owner {
            Owner::Parent { parent, self_index, .. } => {
                let axis = parent.lineage.read().as_ref().map(|(_, axis)| *axis).unwrap();

                if Axis::from(side) != axis {
                    parent.request_len(width, side)
                } else if parent.resizable_width() < width {
                    let new_parent_width = parent.width() + width - (br.x - tl.x) as usize;
                    parent.request_len(new_parent_width, side)
                } else {
                    parent.set_child_len(width, *self_index, side)
                }
            }
            Owner::TlAnchor => todo!(),
            Owner::TrAnchor => todo!(),
            Owner::BlAnchor => todo!(),
            Owner::BrAnchor => todo!(),
            Owner::None => Err(()),
        }
    }

    fn request_width_to_fit(&mut self, _text: &str) -> Result<(), ()> {
        todo!()
    }
}

pub struct Label {
    area: Area,
    cursor: Coord,
    style_before_cursor: Option<ContentStyle>,
    last_style: ContentStyle,
    is_active: bool,
    wrap_next: bool,
    indent: usize,
    stdout: Stdout,
}

impl Label {
    fn new(area: Area) -> Self {
        let cursor = area.inner.read().tl;
        Label {
            stdout: stdout(),
            area,
            cursor,
            style_before_cursor: None,
            last_style: ContentStyle::default(),
            is_active: false,
            wrap_next: false,
            indent: 0,
        }
    }

    fn clear_line(&mut self) {
        if self.cursor.x < self.area.br().x {
            self.clear_form();

            // The rest of the line is featureless.
            let padding_count = (self.area.br().x - self.cursor.x) as usize;
            let padding = " ".repeat(padding_count);
            queue!(self.stdout, Print(padding)).unwrap();
        }

        self.cursor.x = self.area.tl().x;
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
        Label { stdout: stdout(), area: self.area.clone(), ..*self }
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

    fn place_main_cursor(&mut self, cursor_style: CursorStyle) {
        if let (Some(caret), true) = (cursor_style.caret, self.is_active) {
            queue!(self.stdout, caret, SavePosition).unwrap();
            unsafe { SHOW_CURSOR = true }
        } else {
            self.style_before_cursor = Some(self.last_style);
            queue!(self.stdout, SetStyle(cursor_style.form.style)).unwrap();
        }
    }

    fn place_extra_cursor(&mut self, cursor_style: CursorStyle) {
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

        self.cursor = self.area.tl();
        queue!(self.stdout, MoveTo(self.area.tl().x, self.area.tl().y)).unwrap();
        execute!(self.stdout, cursor::Hide).unwrap();

        if self.is_active {
            unsafe { SHOW_CURSOR = false }
        }
    }

    fn stop_printing(&mut self) {
        for _ in self.cursor.y..(self.area.br().y.saturating_sub(2)) {
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
        let len = UnicodeWidthChar::width(ch).map(|width| width as u16).unwrap_or(0);
        if self.cursor.x <= self.area.br().x - len {
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
        if self.cursor.y == self.area.br().y - 1 {
            Err(())
        } else {
            self.clear_line();
            Ok(())
        }
    }

    fn wrap_count(&self, text: &str, wrap_method: WrapMethod, tab_places: &TabPlaces) -> usize {
        match wrap_method {
            WrapMethod::Width => self.get_width(text, tab_places) / self.area.width(),
            WrapMethod::Capped(_) => todo!(),
            WrapMethod::Word => todo!(),
            WrapMethod::NoWrap => todo!(),
        }
    }

    fn get_width(&self, text: &str, tab_places: &TabPlaces) -> usize {
        let mut width = 0;
        for ch in text.chars() {
            width += match ch {
                '\t' => tab_places.spaces_on_col(width),
                ch => UnicodeWidthChar::width(ch).unwrap_or(0),
            }
        }

        width
    }

    fn get_col_at_dist(&self, text: &str, dist: usize, tab_places: &TabPlaces) -> usize {
        text.chars()
            .enumerate()
            .scan((0, false), |(width, end_reached), (index, ch)| {
                *width += match ch {
                    '\t' => tab_places.spaces_on_col(*width),
                    ch => UnicodeWidthChar::width(ch).unwrap_or(0),
                };
                if *end_reached {
                    return None;
                }
                if *width >= dist {
                    *end_reached = true
                }
                Some(index)
            })
            .last()
            .unwrap_or(0)
    }
}

#[derive(Default)]
pub struct Ui {
    layout_has_changed: Mutex<bool>,
    areas: Vec<Area>,
}

impl ui::Ui for Ui {
    type Area = Area;
    type Label = Label;

    fn bisect_area(
        &mut self, area: &mut Self::Area, side: Side, split: Split,
    ) -> (Self::Label, Option<Self::Area>) {
        let (tl, br) = (area.tl(), area.br());
        area.request_len(max(area.resizable_len(Axis::from(side)), split.len()), side).unwrap();

        let split_inner = split_by(area, split, side);
        let (split_area, resized_area) = restructure_tree(area, split_inner, side, split, tl, br);

        let new_label = Label::new(split_area);

        (new_label, resized_area)
    }

    fn maximum_label(&mut self) -> Self::Label {
        *self.layout_has_changed.lock().unwrap() = true;
        Label::new(Area::total())
    }

    fn startup(&mut self) {
        // This makes it so that if the application panics, the panic message is printed
        // nicely and the terminal is left in a usable state.
        use std::panic::set_hook;
        set_hook(Box::new(|msg| {
            let mut stdout = stdout();

            terminal::disable_raw_mode().unwrap();

            execute!(stdout, terminal::Clear(ClearType::All), terminal::LeaveAlternateScreen)
                .unwrap();

            use std::backtrace::Backtrace;
            println!("{}\n\n{}", msg, Backtrace::force_capture());
        }));

        let mut stdout = stdout();
        terminal::enable_raw_mode().unwrap();
        execute!(stdout, terminal::EnterAlternateScreen).unwrap();
    }

    fn shutdown(&mut self) {
        let mut stdout = stdout();

        execute!(stdout, terminal::Clear(ClearType::All), terminal::LeaveAlternateScreen,).unwrap();

        terminal::disable_raw_mode().unwrap();
    }

    fn finish_all_printing(&mut self) {}

    fn layout_has_changed(&self) -> bool {
        let mut layout_has_changed = self.layout_has_changed.lock().unwrap();
        if *layout_has_changed {
            *layout_has_changed = false;
            true
        } else {
            false
        }
    }
}

fn split_by(area: &mut Area, split: Split, side: Side) -> InnerArea {
    let (old_tl, old_br) = (area.tl(), area.br());
    let mut inner = area.inner.write();

    let (tl, br) = match side {
        Side::Left => {
            inner.tl.x += split.len() as u16;
            (old_tl, Coord { x: inner.tl.x, y: old_br.y })
        }
        Side::Right => {
            inner.br.x -= split.len() as u16;
            (Coord { x: inner.br.x, y: old_tl.y }, old_br)
        }
        Side::Top => {
            inner.tl.y += split.len() as u16;
            (old_tl, Coord { x: old_br.x, y: inner.tl.y })
        }
        Side::Bottom => {
            inner.br.y -= split.len() as u16;
            (Coord { x: old_tl.x, y: inner.br.y }, old_br)
        }
    };

    let split_inner = InnerArea::new(tl, br, inner.owner.clone());

    split_inner
}

fn restructure_tree(
    area: &mut Area, split_inner: InnerArea, side: Side, split: Split, tl: Coord, br: Coord,
) -> (Area, Option<Area>) {
    let mut split_area = Area::new(RwData::new(split_inner));

    let mut inner = area.inner.write();

    #[rustfmt::skip]
    if let Some(Owner::Parent { parent, self_index, .. }) = inner.owner.aligns(Axis::from(side)) {
        let mut lineage = parent.lineage.write();
        let (children, _) = lineage.as_mut().unwrap();
        let split_index =
            if let Side::Top | Side::Left = side { *self_index } else { *self_index + 1 };

        for child in children.iter_mut().skip(split_index) {
            if let Owner::Parent { self_index, .. } = &mut child.inner.write().owner {
                *self_index += 1;
            }
        }
        children.insert(split_index, split_area.clone());

		drop(lineage);
        split_area.inner.write().owner = Owner::Parent {
            parent: parent.clone(),
            self_index: split_index,
            split
        };

        (split_area, None)
    } else {
        drop(inner);
        let new_inner = InnerArea::new(tl, br, area.inner.read().owner.clone());
        let mut swapped_child = Area::new(RwData::new(new_inner));
        std::mem::swap(&mut swapped_child.inner, &mut area.inner);
        std::mem::swap(&mut swapped_child.lineage, &mut area.lineage);

        let split_index = if let Side::Top | Side::Left = side { 0 } else { 1 };
        swapped_child.inner.write().owner = Owner::Parent {
            parent: area.clone(),
            self_index: (split_index + 1) % 2,
            split: area.inner.read().owner.split().unwrap_or(Split::Minimum(0))
        };

		let mut children = vec![swapped_child.clone()];
		children.insert((split_index + 1) % 2, split_area.clone());
        *area.lineage.write() = Some((children, Axis::from(side)));

        split_area.inner.write().owner = Owner::new_parent(area.clone(), split_index, split);
        (split_area, Some(swapped_child))
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
        SeparatorForm::Uniform(TextLineBuilder::from([DEFAULT, DEFAULT]))
    }
}

impl SeparatorForm {
    pub fn uniform<U>(node: &RwData<EndNode<U>>, name: impl ToString) -> Self
    where
        U: ui::Ui,
    {
        let node = node.read();
        let (_, id) = node.config().palette.get_from_name(name);

        SeparatorForm::Uniform(TextLineBuilder::from([id, DEFAULT]))
    }

    pub fn different_on_main<U, S>(node: &RwData<EndNode<U>>, main_name: S, other_name: S) -> Self
    where
        U: ui::Ui,
        S: ToString,
    {
        let node = node.read();
        let palette = &node.config().palette;
        let (_, main_id) = palette.get_from_name(main_name);
        let (_, other_id) = palette.get_from_name(other_name);

        SeparatorForm::DifferentOnMain(
            TextLineBuilder::from([main_id, DEFAULT]),
            TextLineBuilder::from([other_id, DEFAULT]),
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
        let palette = &node.config().palette;
        let (_, main_id) = palette.get_from_name(main_name);
        let (_, lower_id) = palette.get_from_name(lower_name);
        let (_, higher_id) = palette.get_from_name(higher_name);

        SeparatorForm::ThreeWay(
            TextLineBuilder::from([main_id, DEFAULT]),
            TextLineBuilder::from([lower_id, DEFAULT]),
            TextLineBuilder::from([higher_id, DEFAULT]),
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
}

pub struct VertRule<U>
where
    U: ui::Ui,
{
    file: RoData<FileWidget<U>>,
    text: Text<U>,
    vert_rule_config: VertRuleConfig,
}

impl<U> VertRule<U>
where
    U: ui::Ui + 'static,
{
    /// Returns a new instance of `Box<VerticalRuleConfig>`, taking a user provided config.
    pub fn new(file_widget: RwData<FileWidget<U>>, vert_rule_config: VertRuleConfig) -> Widget<U> {
        let file = RoData::from(&file_widget);

        Widget::Normal(RwData::new_unsized(Arc::new(Mutex::new(VertRule {
            file,
            text: Text::default(),
            vert_rule_config,
        }))))
    }

    /// Returns a new instance of `Box<VerticalRuleConfig>`, using the default config.
    pub fn default(file_widget: RwData<FileWidget<U>>) -> Widget<U> {
        let file = RoData::from(&file_widget);

        Widget::Normal(RwData::new_unsized(Arc::new(Mutex::new(VertRule {
            file,
            text: Text::default(),
            vert_rule_config: VertRuleConfig::default(),
        }))))
    }
}

unsafe impl<U> Send for VertRule<U> where U: ui::Ui {}

impl<U> NormalWidget<U> for VertRule<U>
where
    U: ui::Ui + 'static,
{
    fn identifier(&self) -> &str {
        "vertical_rule"
    }

    fn update(&mut self, _end_node: &mut EndNode<U>) {
        let file = self.file.read();

        self.text.lines.clear();

        let iterations = file.printed_lines().iter().map(|number| *number);
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
}
