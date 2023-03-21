#![feature(stmt_expr_attributes, is_some_and)]
#[cfg(not(feature = "deadlock-detection"))]
use std::sync::Mutex;
use std::{
    any::Any,
    cmp::{max, min},
    fmt::{Debug, Display},
    io::{stdout, Stdout},
    sync::Arc,
};

use crossterm::{
    cursor::{self, MoveTo, RestorePosition, SavePosition},
    execute, queue,
    style::{ContentStyle, Print, ResetColor, SetStyle},
    terminal::{self, ClearType},
};
#[cfg(feature = "deadlock-detection")]
use no_deadlocks::Mutex;
use parsec_core::{
    config::{Config, DownCastableData},
    log_info,
    tags::Tag,
    text::PrintStatus,
    ui::PushSpecs,
};
use parsec_core::{
    config::{RoData, RwData, TabPlaces, WrapMethod},
    tags::{
        form::CursorStyle,
        form::{Form, DEFAULT},
    },
    text::{Text, TextBuilder},
    ui::{self, Area as UiArea, Axis, EndNode, Label as UiLabel, Side, Split},
    widgets::{file_widget::FileWidget, NormalWidget, Widget},
};
use ropey::RopeSlice;
use unicode_width::UnicodeWidthChar;

static mut PRINTER: std::sync::Mutex<()> = std::sync::Mutex::new(());
static mut LOCK: Option<std::sync::MutexGuard<()>> = None;
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

#[derive(Debug, Clone, Copy)]
pub struct Coords {
    tl: Coord,
    br: Coord,
}

impl Coords {
    fn width(&self) -> usize {
        (self.br.x - self.tl.x) as usize
    }

    fn height(&self) -> usize {
        (self.br.y - self.tl.y - 1) as usize
    }
}

impl Display for Coords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{},{}", self.tl, self.br))
    }
}

#[derive(Clone)]
enum Owner {
    Parent {
        parent: Area,
        self_index: usize,
        split: Split,
    },
    TlAnchor,
    TrAnchor,
    BlAnchor,
    BrAnchor,
    None,
}

impl Debug for Owner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Owner::Parent {
                parent,
                self_index,
                split,
            } => f
                .debug_struct("Parent")
                .field("parent_coords", &parent.inner.read().coords)
                .field("self_index", self_index)
                .field("split", split)
                .finish(),
            Owner::TlAnchor => f.write_str("TlAnchor"),
            Owner::TrAnchor => f.write_str("TrAnchor"),
            Owner::BlAnchor => f.write_str("BlAnchor"),
            Owner::BrAnchor => f.write_str("BrAnchor"),
            Owner::None => f.write_str("None"),
        }
    }
}

impl Owner {
    fn clone_with_split(&self, split: Split) -> Self {
        if let Owner::Parent {
            parent, self_index, ..
        } = self
        {
            Owner::Parent {
                parent: parent.clone(),
                self_index: *self_index,
                split,
            }
        } else {
            self.clone()
        }
    }

    fn split(&self) -> Option<Split> {
        if let Owner::Parent { split, .. } = self {
            Some(*split)
        } else {
            None
        }
    }

    fn aligns(&mut self, other: Axis) -> Option<&mut Self> {
        if let Owner::Parent { parent, .. } = self {
            if parent
                .lineage
                .read()
                .as_ref()
                .is_some_and(|(_, axis)| *axis == other)
            {
                return Some(self);
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
struct InnerArea {
    coords: Coords,
    owner: Owner,
}

//impl Debug for InnerArea {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        f.debug_struct("InnerArea").field("tl", &self.coords).finish()
//    }
//}

impl InnerArea {
    fn new(coords: Coords, owner: Owner) -> Self {
        InnerArea { coords, owner }
    }

    fn len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis {
            self.coords.width()
        } else {
            self.coords.height()
        }
    }

    fn add_or_take(&mut self, do_add: bool, remaining: usize, side: Side) -> (Coord, usize) {
        if do_add {
            self.add_to_side(remaining, side)
        } else {
            self.take_from_side(remaining, side)
        }
    }

    fn take_from_side(&mut self, len: usize, side: Side) -> (Coord, usize) {
        let Some(Split::Min(min_len)) = self.owner.split() else {
            return (self.coord_from_side(side), len);
        };
        let len_diff = min(self.len(Axis::from(side)) - min_len, len);

        match side {
            Side::Top => self.coords.tl.y -= len_diff as u16,
            Side::Right => self.coords.br.x -= len_diff as u16,
            Side::Bottom => self.coords.br.y -= len_diff as u16,
            Side::Left => self.coords.tl.x -= len_diff as u16,
        }

        (self.coord_from_side(side), len - len_diff)
    }

    fn add_to_side(&mut self, len: usize, side: Side) -> (Coord, usize) {
        let Some(Split::Min(min_len)) = self.owner.split() else {
            return (self.coord_from_side(side), len);
        };

        match side {
            Side::Top => self.coords.tl.y += len as u16,
            Side::Right => self.coords.br.x += len as u16,
            Side::Bottom => self.coords.br.y += len as u16,
            Side::Left => self.coords.tl.x += len as u16,
        }

        (self.coord_from_side(side), 0)
    }

    fn coord_from_side(&self, side: Side) -> Coord {
        if let Side::Top | Side::Right = side {
            Coord {
                x: self.coords.br.x,
                y: self.coords.tl.y,
            }
        } else {
            Coord {
                x: self.coords.tl.x,
                y: self.coords.br.y,
            }
        }
    }

    fn set_tl(&mut self, tl: Coord) {
        let (width, height) = (self.coords.width(), self.coords.height());
        self.coords.tl = tl;
        self.coords.br.x = tl.x + width as u16;
        self.coords.br.y = tl.y + height as u16;
    }

    fn set_br(&mut self, br: Coord) {
        let (width, height) = (self.coords.width(), self.coords.height());
        self.coords.br = br;
        self.coords.tl.x = br.x - width as u16;
        self.coords.tl.y = br.y - height as u16;
    }
}

#[derive(Debug, Clone)]
pub struct Area {
    inner: RwData<InnerArea>,
    lineage: RwData<Option<(Vec<Area>, Axis)>>,
}

impl Area {
    fn total() -> Self {
        let (max_x, max_y) = terminal::size().unwrap();
        let coords = Coords {
            tl: Coord { x: 0, y: 0 },
            br: Coord { x: max_x, y: max_y },
        };

        Area {
            inner: RwData::new(InnerArea::new(coords, Owner::None)),
            lineage: RwData::new(None),
        }
    }

    pub fn coords(&self) -> Coords {
        self.inner.read().coords
    }

    pub fn tl(&self) -> Coord {
        self.inner.read().coords.tl
    }

    pub fn br(&self) -> Coord {
        self.inner.read().coords.br
    }

    fn new(inner: InnerArea) -> Self {
        Area {
            inner: RwData::new(inner),
            lineage: RwData::new(None),
        }
    }

    fn set_child_len(&mut self, new_len: usize, index: usize, side: Side) -> Result<(), ()> {
        let mut lineage = self.lineage.write();
        let Some((children, ..)) = &mut *lineage else {
            return Err(());
        };

        let target = children.get_mut(index).unwrap();
        let (old_len, mut target_inner) = (target.width(), target.inner.write());

        let mut remaining = old_len.abs_diff(new_len);
        let (mut last_corner, _) = target_inner.add_or_take(old_len < new_len, remaining, side);
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

        drop(lineage);

        Ok(())
    }

    fn regulate_children(&mut self) {
        let Some((_, axis)) = *self.lineage.write() else {
            return;
        };

        let self_inner = self.inner.read();

        let mut last_tl = self_inner.coords.tl;
        let (old_len, new_len) = if let Axis::Horizontal = axis {
            (self.resizable_width(), self.width())
        } else {
            (self.resizable_width(), self.height())
        };

        let mut lineage = self.lineage.write();
        let (children, axis) = lineage.as_mut().unwrap();
        for child in children.iter_mut() {
            let mut inner = child.inner.write();
            let coords = &mut inner.coords;
            coords.tl = last_tl;

            (coords.br, last_tl) = if let Axis::Horizontal = axis {
                let ratio = (coords.width() as f32) / (old_len as f32);
                let x = coords.tl.x + (ratio * (new_len as f32)).floor() as u16;
                (Coord { x, y: coords.br.y }, Coord { x, y: coords.tl.y })
            } else {
                let ratio = (coords.height() as f32) / (old_len as f32);
                let y = coords.tl.y + (ratio * (new_len as f32)).floor() as u16;
                (Coord { x: coords.br.x, y }, Coord { x: coords.tl.x, y })
            };
        }

        children
            .last_mut()
            .map(|last| last.inner.write().coords.br = self_inner.coords.br);
    }

    fn resizable_len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis {
            self.resizable_width()
        } else {
            self.resizable_height()
        }
    }

    fn len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis {
            self.width()
        } else {
            self.height()
        }
    }

    fn resizable_width(&self) -> usize {
        if let Some(Split::Locked(_)) = self.inner.read().owner.split() {
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
        if let Some(Split::Locked(_)) = self.inner.read().owner.split() {
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
        f.write_fmt(format_args!(
            "{},{}",
            self.inner.read().coords.tl,
            self.inner.read().coords.br
        ))
    }
}

impl ui::Area for Area {
    fn width(&self) -> usize {
        self.inner.read().coords.width()
    }

    fn height(&self) -> usize {
        self.inner.read().coords.height()
    }

    fn request_len(&mut self, len: usize, side: Side) -> Result<(), ()> {
        if self.len(Axis::from(side)) == len {
            return Ok(());
        };

        let inner = self.inner.write();
        let InnerArea { coords, owner } = &mut inner.clone();
        drop(inner);

        match owner {
            Owner::Parent {
                ref mut parent,
                self_index,
                ..
            } => {
                let axis = parent
                    .lineage
                    .read()
                    .as_ref()
                    .map(|(_, axis)| *axis)
                    .unwrap();

                if Axis::from(side) != axis {
                    parent.request_len(len, side)
                } else if parent.resizable_width() < len {
                    let new_parent_width = parent.width() + len - coords.width();
                    parent.request_len(new_parent_width, side)
                } else {
                    parent.set_child_len(len, *self_index, side)
                }
            }
            Owner::TlAnchor => todo!(),
            Owner::TrAnchor => todo!(),
            Owner::BlAnchor => todo!(),
            Owner::BrAnchor => todo!(),
            Owner::None => {
                if len == self.resizable_len(Axis::from(side)) {
                    Ok(())
                } else {
                    Err(())
                }
            }
        }
    }

    fn request_width_to_fit(&mut self, _text: &str) -> Result<(), ()> {
        todo!()
    }
}

#[derive(Debug)]
pub struct Label {
    area: Area,
    cursor: Coord,
    style_before_cursor: Option<ContentStyle>,
    last_style: ContentStyle,
    is_active: bool,
    indent: usize,
    stdout: Stdout,
    tab_places: Option<TabPlaces>,
    wrap_method: Option<WrapMethod>,
}

impl Label {
    fn new(area: Area) -> Self {
        let cursor = area.inner.read().coords.tl;
        Label {
            stdout: stdout(),
            area,
            cursor,
            style_before_cursor: None,
            last_style: ContentStyle::default(),
            is_active: false,
            indent: 0,
            tab_places: None,
            wrap_method: None,
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

        queue!(
            self.stdout,
            ResetColor,
            MoveTo(self.cursor.x, self.cursor.y),
            SetStyle(self.last_style)
        )
        .unwrap();
    }

    fn wrap_line(&mut self) -> PrintStatus {
        self.clear_line();

        queue!(
            self.stdout,
            MoveTo(self.cursor.x, self.cursor.y),
            Print(" ".repeat(self.indent))
        )
        .unwrap();

        self.cursor.x += self.indent as u16;
        self.indent = 0;

        if self.cursor.y > self.area.br().y {
            PrintStatus::Finished
        } else {
            PrintStatus::NextChar
        }
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

    fn start_printing(&mut self, config: &Config) {
        self.wrap_method = Some(config.wrap_method);
        self.tab_places = Some(config.tab_places.clone());

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
        self.wrap_method = None;
        self.tab_places = None;

        while let PrintStatus::NextLine = self.next_line() {}

        queue!(self.stdout, ResetColor, RestorePosition).unwrap();
        if unsafe { SHOW_CURSOR } {
            queue!(self.stdout, cursor::Show).unwrap();
        }

        self.clear_form();
        unsafe { LOCK = None };
        self.is_active = false;
    }

    fn print(&mut self, ch: char, x_shift: usize) -> PrintStatus {
        let tab_places = self.tab_places.as_ref().unwrap();
        let wrap_method = self.wrap_method.as_ref().unwrap();

        let len = match ch {
            '\t' => tab_places.spaces_on_col(x_shift) as u16,
            _ => UnicodeWidthChar::width(ch).unwrap_or(0) as u16,
        };

        if self.cursor.x <= self.area.br().x - len {
            self.cursor.x += len;
            queue!(self.stdout, Print(ch)).unwrap();
            if let Some(style) = self.style_before_cursor.take() {
                queue!(self.stdout, ResetColor, SetStyle(style)).unwrap();
            }
        } else if self.cursor.x <= self.area.br().x {
            let width = self.area.br().x - self.cursor.x;
            queue!(self.stdout, Print(" ".repeat(width as usize))).unwrap();
            if let Some(style) = self.style_before_cursor.take() {
                queue!(self.stdout, ResetColor, SetStyle(style)).unwrap();
            }
        }

        if self.cursor.x == self.area.br().x {
            if let WrapMethod::NoWrap = wrap_method {
                PrintStatus::NextLine
            } else {
                self.wrap_line()
            }
        } else {
            PrintStatus::NextChar
        }
    }

    fn next_line(&mut self) -> PrintStatus {
        if self.cursor.y == self.area.br().y - 1 {
            PrintStatus::Finished
        } else {
            self.clear_line();
            PrintStatus::NextChar
        }
    }

    fn wrap_count(
        &self,
        slice: RopeSlice,
        wrap_method: WrapMethod,
        tab_places: &TabPlaces,
    ) -> usize {
        match wrap_method {
            WrapMethod::Width => self.get_width(slice, tab_places) / self.area.width(),
            WrapMethod::Capped(_) => todo!(),
            WrapMethod::Word => todo!(),
            WrapMethod::NoWrap => 0,
        }
    }

    fn get_width(&self, slice: RopeSlice, tab_places: &TabPlaces) -> usize {
        let mut width = 0;
        for ch in slice.chars() {
            width += match ch {
                '\t' => tab_places.spaces_on_col(width),
                ch => UnicodeWidthChar::width(ch).unwrap_or(0),
            }
        }

        width
    }

    fn col_at_dist(&self, slice: RopeSlice, dist: usize, tab_places: &TabPlaces) -> usize {
        slice
            .chars()
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

    fn col_at_wrap(
        &self,
        slice: RopeSlice,
        wrap: usize,
        wrap_method: WrapMethod,
        tab_places: &TabPlaces,
    ) -> usize {
        match wrap_method {
            WrapMethod::Width => {
                let dist = wrap * self.area.width();
                slice
                    .chars()
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
                    .unwrap()
            }
            WrapMethod::Capped(_) => todo!(),
            WrapMethod::Word => todo!(),
            WrapMethod::NoWrap => 0,
        }
    }
}

pub struct Ui {
    layout_has_changed: Mutex<bool>,
    areas: Vec<Area>,
}

impl Debug for Ui {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ui")
            .field(
                "layout_has_changed",
                &*self.layout_has_changed.lock().unwrap(),
            )
            .field("areas", &self.areas)
            .finish()
    }
}

impl Default for Ui {
    fn default() -> Self {
        Ui {
            layout_has_changed: Mutex::new(true),
            areas: Vec::new(),
        }
    }
}

impl ui::Ui for Ui {
    type Area = Area;
    type Label = Label;

    fn bisect_area(
        &mut self,
        area: &mut Self::Area,
        push_specs: PushSpecs,
    ) -> (Self::Label, Option<Self::Area>) {
        let PushSpecs { side, split, .. } = push_specs;
        let (old_coords, resizable_len) = (area.coords(), area.resizable_len(Axis::from(side)));
        if let Err(_) = area.request_len(max(resizable_len, split.len()), side) {
            panic!(
                "Resize failed:\n    {:?}, {:?}, {:?}",
                old_coords, side, split
            )
        }

        let split_coords = split_by(area, split, side);
        let (split_area, resized_area) =
            restructure_tree(area, old_coords, split_coords, side, split);

        let split_label = Label::new(split_area);

        (split_label, resized_area)
    }

    fn maximum_label(&mut self) -> Self::Label {
        *self.layout_has_changed.lock().unwrap() = true;
        Label::new(Area::total())
    }

    fn startup(&mut self) {
        // This makes it so that if the application panics, the panic message is printed
        // nicely and the terminal is left in a usable state.
        use std::panic;
        let orig_hook = panic::take_hook();
        panic::set_hook(Box::new(move |panic_info| {
            let mut stdout = stdout();

            execute!(
                stdout,
                terminal::Clear(ClearType::All),
                terminal::LeaveAlternateScreen
            )
            .unwrap();
            terminal::disable_raw_mode().unwrap();

            orig_hook(panic_info);
            std::process::exit(1)
        }));

        let mut stdout = stdout();
        execute!(stdout, terminal::EnterAlternateScreen).unwrap();
        terminal::enable_raw_mode().unwrap();
    }

    fn shutdown(&mut self) {
        let mut stdout = stdout();

        execute!(
            stdout,
            terminal::Clear(ClearType::All),
            terminal::LeaveAlternateScreen,
        )
        .unwrap();
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

fn split_by(area: &mut Area, split: Split, side: Side) -> Coords {
    let (old_tl, old_br) = (area.tl(), area.br());
    let mut inner = area.inner.write();
    let coords = &mut inner.coords;

    match side {
        Side::Left => {
            coords.tl.x += split.len() as u16;
            Coords {
                tl: old_tl,
                br: Coord {
                    x: coords.tl.x,
                    y: old_br.y,
                },
            }
        }
        Side::Right => {
            coords.br.x -= split.len() as u16;
            Coords {
                tl: Coord {
                    x: coords.br.x,
                    y: old_tl.y,
                },
                br: old_br,
            }
        }
        Side::Top => {
            coords.tl.y += split.len() as u16;
            Coords {
                tl: old_tl,
                br: Coord {
                    x: old_br.x,
                    y: coords.tl.y,
                },
            }
        }
        Side::Bottom => {
            coords.br.y -= split.len() as u16;
            Coords {
                tl: Coord {
                    x: old_tl.x,
                    y: coords.br.y,
                },
                br: old_br,
            }
        }
    }
}

fn restructure_tree(
    resized_area: &mut Area,
    old_coords: Coords,
    split_coords: Coords,
    side: Side,
    split: Split,
) -> (Area, Option<Area>) {
    let mut inner = resized_area.inner.write();

    if let Some(Owner::Parent {
        parent, self_index, ..
    }) = inner.owner.aligns(Axis::from(side))
    {
        let mut lineage = parent.lineage.write();
        let (children, _) = lineage.as_mut().unwrap();

        (
            new_child_area(split_coords, children, side, split, *self_index),
            None,
        )
    } else {
        let new_parent = Area::new(InnerArea::new(old_coords, inner.owner.clone()));

        if let Owner::Parent {
            parent, self_index, ..
        } = &mut inner.owner
        {
            let mut lineage = parent.lineage.write();
            let lineage = lineage.as_mut().unwrap();
            lineage.0[*self_index] = new_parent.clone();
        }

        let resized_split = inner.owner.split().unwrap_or(Split::Min(0));
        inner.owner = Owner::Parent {
            parent: new_parent.clone(),
            self_index: 0,
            split: resized_split,
        };

        let mut children = vec![resized_area.clone()];
        drop(inner);
        let split_area = new_child_area(split_coords, &mut children, side, split, 0);
        *new_parent.lineage.write() = Some((children, Axis::from(side)));

        (split_area, Some(new_parent))
    }
}

fn new_child_area(
    child_coords: Coords,
    children: &mut Vec<Area>,
    side: Side,
    split: Split,
    pushed_index: usize,
) -> Area {
    let owner = children[pushed_index]
        .inner
        .read()
        .owner
        .clone_with_split(split);
    let new_child = Area::new(InnerArea::new(child_coords, owner));

    let index = if let Side::Top | Side::Left = side {
        pushed_index
    } else {
        pushed_index + 1
    };
    children.insert(index, new_child.clone());

    for child in children.iter_mut().skip(pushed_index + 1) {
        if let Owner::Parent { self_index, .. } = &mut child.inner.write().owner {
            *self_index += 1;
        }
    }

    new_child
}

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
            SepChar::ThreeWay(main, lower, upper) => [*upper, *main, *lower],
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
    pub fn uniform<U>(node: &RwData<EndNode<U>>, name: impl AsRef<str>) -> Self
    where
        U: ui::Ui,
    {
        let node = node.read();
        let (_, id) = node.config().palette.from_name(name);

        SepForm::Uniform(id)
    }

    pub fn two_way<U>(
        node: &RwData<EndNode<U>>,
        main_name: impl AsRef<str>,
        other_name: impl AsRef<str>,
    ) -> Self
    where
        U: ui::Ui,
    {
        let node = node.read();
        let palette = &node.config().palette;
        let (_, main_id) = palette.from_name(main_name);
        let (_, other_id) = palette.from_name(other_name);

        SepForm::TwoWay(main_id, other_id)
    }

    pub fn three_way<U>(
        node: &RwData<EndNode<U>>,
        main_name: impl AsRef<str>,
        lower_name: impl AsRef<str>,
        upper_name: impl AsRef<str>,
    ) -> Self
    where
        U: ui::Ui,
    {
        let node = node.read();
        let palette = &node.config().palette;
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
pub struct VertRuleConfig {
    pub sep_char: SepChar,
    pub sep_form: SepForm,
}

pub struct VertRule<U>
where
    U: ui::Ui,
{
    file: RoData<FileWidget<U>>,
    builder: TextBuilder<U>,
    cfg: VertRuleConfig,
}

impl<U> VertRule<U>
where
    U: ui::Ui + 'static,
{
    /// Returns a new instance of `Box<VerticalRuleConfig>`, taking a user provided config.
    pub fn new(mut file_widget: RoData<FileWidget<U>>, cfg: VertRuleConfig) -> Widget<U> {
        let file = file_widget.read();

        let builder = setup_builder(&file, &cfg);

        drop(file);
        Widget::Normal(RwData::new_unsized(Arc::new(Mutex::new(VertRule {
            file: file_widget,
            builder,
            cfg,
        }))))
    }

    /// Returns a new instance of `Box<VerticalRuleConfig>`, using the default config.
    pub fn default(mut file_widget: RoData<FileWidget<U>>) -> Widget<U> {
        let file = file_widget.read();

        let cfg = VertRuleConfig::default();
        let builder = setup_builder(&file, &cfg);

        drop(file);
        Widget::Normal(RwData::new_unsized(Arc::new(Mutex::new(VertRule {
            file: file_widget,
            builder,
            cfg,
        }))))
    }
}

unsafe impl<U> Send for VertRule<U> where U: ui::Ui {}

impl<U> DownCastableData for VertRule<U>
where
    U: ui::Ui + 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl<U> NormalWidget<U> for VertRule<U>
where
    U: ui::Ui + 'static,
{
    fn identifier(&self) -> &str {
        "vertical_rule"
    }

    fn update(&mut self, _end_node: &mut EndNode<U>) {
        let file = self.file.read();
    }

    fn needs_update(&self) -> bool {
        self.file.has_changed()
    }

    fn text(&self) -> &Text<U> {
        self.builder.text()
    }
}

fn setup_builder<U>(file: &FileWidget<U>, cfg: &VertRuleConfig) -> TextBuilder<U>
where
    U: ui::Ui,
{
    let lines = file.printed_lines();
    let main_line = file.main_cursor().true_row();
    let mut builder = TextBuilder::default_string();
    let upper = lines.iter().take_while(|&line| *line != main_line).count();
    let lower = lines.iter().skip_while(|&line| *line <= main_line).count();

    let forms = cfg.sep_form.forms();
    let chars = cfg.sep_char.chars();

    builder.push_tag(Tag::PushForm(forms[0]));
    builder.push_swappable(chars[0].to_string().repeat(upper));
    builder.push_tag(Tag::PushForm(forms[1]));
    builder.push_swappable(chars[1].to_string());
    builder.push_tag(Tag::PushForm(forms[2]));
    builder.push_swappable(chars[2].to_string().repeat(lower));

    builder
}
