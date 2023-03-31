#![feature(stmt_expr_attributes, is_some_and)]
#[cfg(not(feature = "deadlock-detection"))]
use std::{
    cmp::{max, min},
    fmt::{Debug, Display},
    io::{stdout, Stdout},
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex,
    },
};

use crossterm::{
    cursor::{self, MoveTo, RestorePosition, SavePosition},
    execute, queue,
    style::{ContentStyle, Print, ResetColor, SetStyle},
    terminal::{self, ClearType},
};
#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;
use parsec_core::{config::Config, log_info, text::PrintStatus, ui::PushSpecs};
use parsec_core::{
    config::{RwData, TabPlaces, WrapMethod},
    tags::{form::CursorStyle, form::Form},
    ui::{self, Area as UiArea, Axis, Label as UiLabel, Side, Split},
};
use ropey::RopeSlice;
use unicode_width::UnicodeWidthChar;

mod rules;
pub use rules::SepChar;
pub use rules::SepForm;
pub use rules::VertRule;
pub use rules::VertRuleCfg;

static mut IS_PRINTING: AtomicBool = AtomicBool::new(false);
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
    fn add_to_side(&mut self, side: Side, len_diff: i16) {
        match side {
            Side::Top => self.tl.y = self.tl.y.saturating_add_signed(-len_diff),
            Side::Left => self.tl.y = self.tl.y.saturating_add_signed(-len_diff),
            Side::Bottom => self.br.y = self.br.y.saturating_add_signed(len_diff),
            Side::Right => self.br.x = self.br.x.saturating_add_signed(len_diff),
        }
    }

    fn width(&self) -> usize {
        (self.br.x - self.tl.x) as usize
    }

    fn height(&self) -> usize {
        (self.br.y - self.tl.y) as usize
    }

    fn len(&self, axis: Axis) -> usize {
        match axis {
            Axis::Horizontal => self.width(),
            Axis::Vertical => self.height(),
        }
    }

    fn ortho_corner(&self, axis: Axis) -> Coord {
        match axis {
            Axis::Horizontal => Coord {
                x: self.br.x,
                y: self.tl.y,
            },
            Axis::Vertical => Coord {
                x: self.tl.x,
                y: self.br.y,
            },
        }
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

    fn aligns(&mut self, other: Axis) -> Option<Self> {
        if let Owner::Parent { parent, .. } = self {
            if parent.lineage.read().as_ref().is_some_and(|(_, axis)| *axis == other) {
                return Some(self.clone());
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

// impl Debug for InnerArea {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) ->
// std::fmt::Result {        f.debug_struct("InnerArea").field("tl",
// &self.coords).finish()    }
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
        let axis = Axis::from(side);
        let mut lineage = self.lineage.write();
        let Some((children, ..)) = &mut *lineage else {
            return Err(());
        };

        let target = children.get_mut(index).unwrap();
        let old_len = target.len(axis);
        let mut target_inner = target.inner.write();

        let len_diff = new_len as i16 - old_len as i16;
        target_inner.coords.add_to_side(side, len_diff);
        let target_coords = target_inner.coords;
        drop(target_inner);
        drop(target);

        let len_diff = old_len as i16 - new_len as i16;

        let (children, last_tl) = if let Side::Right | Side::Bottom = side {
            (&mut children[(index + 1)..], target_coords.ortho_corner(axis))
        } else {
            (&mut children[..index], self.tl())
        };

        let new_lens = scale_children(children, len_diff, axis);
        normalize_from_tl(children, new_lens, last_tl, side);

        drop(lineage);

        Ok(())
    }

    fn regulate_children(&mut self, len_diff: i16, side: Side) {
        let mut lineage = self.lineage.write();
        let Some((children, axis)) = &mut *lineage else {
            return;
        };

        if Axis::from(side) == *axis {
            let new_lens = scale_children(children, len_diff, *axis);
            normalize_from_tl(children, new_lens, self.tl(), side);
        } else {
            for child in children {
                let mut inner = child.inner.write();
                log_info!("\n\nprev_reg: {:?}", inner.coords);
                inner.coords.add_to_side(side, len_diff);
                log_info!("\npost_reg: {:?}", inner.coords);
            }
        }
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

fn stretch_br(coords: Coords, len: u16, axis: Axis) -> Coord {
    if let Axis::Horizontal = axis {
        let x = coords.tl.x + len;
        Coord { x, y: coords.br.y }
    } else {
        let y = coords.tl.y + len;
        Coord { x: coords.br.x, y }
    }
}

fn normalize_from_tl(children: &mut [Area], lens: Vec<u16>, tl: Coord, side: Side) {
    let axis = Axis::from(side);
    let mut last_tl = tl;
    let mut new_lens = lens.iter();

    for child in children.iter_mut() {
        let mut inner = child.inner.write();
        log_info!("\n\nprev: {:?}", inner.coords);
        let len = if let Split::Locked(len) = inner.owner.split().unwrap() {
            len as u16
        } else {
            *new_lens.next().unwrap()
        };

        let old_len = inner.len(axis);
        inner.coords.tl = last_tl;
        inner.coords.br = stretch_br(inner.coords, len, axis);

        last_tl = inner.coords.ortho_corner(axis);

        log_info!("\npost: {:?}", inner.coords);
        drop(inner);
        child.regulate_children(len as i16 - old_len as i16, side);
    }
}

fn scale_children(children: &[Area], len_diff: i16, axis: Axis) -> Vec<u16> {
    let mut lens = children
        .iter()
        .filter_map(|child| {
            if let Split::Min(_) = child.inner.read().owner.split().unwrap() {
                Some(child.len(axis) as u16)
            } else {
                None
            }
        })
        .collect::<Vec<u16>>();

    let old_len = lens.iter().sum::<u16>();

    let diff_array = {
        let abs_diff = len_diff.abs() as usize;
        let mut vec = Vec::with_capacity(abs_diff);
        for element in 0..abs_diff {
            let ratio = element as f32 / abs_diff as f32;
            vec.push((ratio * old_len as f32).round() as u16);
        }

        vec
    };

    let mut accum = 0;
    let mut last_diff = 0;
    for len in lens.iter_mut() {
        let next_accum = accum + *len;

        for diff in &diff_array[last_diff..] {
            if (accum..(accum + *len)).contains(diff) {
                *len = len.saturating_add_signed(len_diff.signum());
                last_diff += 1;
            }
        }

        accum = next_accum
    }

    lens
}

impl Display for Area {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{},{}", self.inner.read().coords.tl, self.inner.read().coords.br))
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
        let req_axis = Axis::from(side);
        // If the current len is already equal to the requested len,
        // nothing needs to be done.
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
                let axis = parent.lineage.read().as_ref().map(|(_, axis)| *axis).unwrap();

                if req_axis != axis {
                    parent.request_len(len, side)
                } else if parent.resizable_len(req_axis) < len {
                    let new_parent_width = parent.len(req_axis) + len - coords.len(req_axis);
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
                if len == self.resizable_len(req_axis) {
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

        if self.cursor.y == self.area.br().y - 1 {
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
            while IS_PRINTING
                .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Acquire)
                .is_err()
            {}
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

        while let PrintStatus::NextChar = self.next_line() {}

        execute!(self.stdout, ResetColor, RestorePosition).unwrap();
        if unsafe { SHOW_CURSOR } {
            execute!(self.stdout, cursor::Show).unwrap();
        }

        self.clear_form();
        unsafe { IS_PRINTING.store(false, Ordering::Release) };
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
        if self.area.br().y > self.cursor.y + 1 {
            self.clear_line();
            PrintStatus::NextChar
        } else {
            PrintStatus::Finished
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
            .field("layout_has_changed", &*self.layout_has_changed.lock().unwrap())
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
        let axis = Axis::from(side);
        let (old_coords, resizable_len) = (area.coords(), area.resizable_len(axis));

        if let Err(_) = area.request_len(max(resizable_len, split.len()), side) {
            panic!("Resize failed:\n    {:?}, {:?}, {:?}", old_coords, side, split)
        }

        let old_len = area.len(axis);
        let split_coords = split_by(area, split, side);
        log_info!("\nsplit_coords: {:?}", split_coords);
        let new_len = area.len(axis);
        area.regulate_children(new_len as i16 - old_len as i16, side);
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
        // This makes it so that if the application panics, the panic message
        // is printed nicely and the terminal is left in a usable
        // state.
        use std::panic;
        let orig_hook = panic::take_hook();
        panic::set_hook(Box::new(move |panic_info| {
            let mut stdout = stdout();

            execute!(
                stdout,
                terminal::Clear(ClearType::All),
                terminal::LeaveAlternateScreen,
                cursor::Show
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

fn split_by(area: &mut Area, split: Split, side: Side) -> Coords {
    let (tl, br) = (area.tl(), area.br());
    let mut inner = area.inner.write();
    let coords = &mut inner.coords;

    match side {
        Side::Left => {
            coords.tl.x += split.len() as u16;
            Coords {
                tl,
                br: coords.ortho_corner(Axis::Vertical),
            }
        }
        Side::Right => {
            coords.br.x -= split.len() as u16;
            Coords {
                tl: coords.ortho_corner(Axis::Horizontal),
                br,
            }
        }
        Side::Top => {
            coords.tl.y += split.len() as u16;
            Coords {
                tl,
                br: coords.ortho_corner(Axis::Horizontal),
            }
        }
        Side::Bottom => {
            coords.br.y -= split.len() as u16;
            Coords {
                tl: coords.ortho_corner(Axis::Vertical),
                br,
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

        drop(inner);
        drop(resized_area);
        (new_child_area(split_coords, children, side, split, self_index), None)
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
    let owner = children[pushed_index].inner.read().owner.clone_with_split(split);
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
