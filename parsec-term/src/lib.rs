#![feature(stmt_expr_attributes, option_as_slice)]

use std::{
    fmt::{Debug, Display},
    io::{self, stdout, Stdout, StdoutLock, Write},
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
};

use crossterm::{
    cursor::{self, MoveTo, RestorePosition, SavePosition},
    execute, queue,
    style::{ContentStyle, Print, ResetColor, SetStyle},
    terminal::{self, ClearType},
};

use parsec_core::{config::Config, tags::form, text::PrintStatus, ui::PushSpecs};
use parsec_core::{
    config::{RwData, TabPlaces, WrapMethod},
    tags::{form::CursorStyle, form::Form},
    ui::{self, Area as UiArea, Axis, Side, Split},
};
use ropey::RopeSlice;
use unicode_width::UnicodeWidthChar;

mod rules;
pub use rules::SepChar;
pub use rules::SepForm;
pub use rules::VertRule;
pub use rules::VertRuleCfg;

// Static variables, used solely for printing.
static IS_PRINTING: AtomicBool = AtomicBool::new(false);
static SHOW_CURSOR: AtomicBool = AtomicBool::new(false);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Coord {
    x: u16,
    y: u16,
}

impl Coord {
    pub fn new(x: u16, y: u16) -> Self {
        Self { x, y }
    }
}

impl Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.y, self.x))
    }
}

#[derive(Clone, Copy)]
pub struct Coords {
    tl: Coord,
    br: Coord,
}

impl Debug for Coords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{},{}:{}", self.tl.x, self.tl.y, self.br.x, self.br.y))
    }
}

impl Coords {
    /// Returns a new instance of [`Coords`].
    pub fn new(tl: Coord, br: Coord) -> Self {
        Self { tl, br }
    }

    fn empty_on(&self, side: Side) -> Self {
        let tr = Coord::new(self.br.x, self.tl.y);
        let bl = Coord::new(self.tl.x, self.br.y);

        match side {
            Side::Top => Coords::new(self.tl, tr),
            Side::Right => Coords::new(tr, self.br),
            Side::Bottom => Coords::new(bl, self.br),
            Side::Left => Coords::new(self.tl, bl),
        }
    }

    fn len(&self, axis: Axis) -> usize {
        match axis {
            Axis::Horizontal => self.width(),
            Axis::Vertical => self.height(),
        }
    }

    fn width(&self) -> usize {
        (self.br.x - self.tl.x) as usize
    }

    fn height(&self) -> usize {
        (self.br.y - self.tl.y) as usize
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

impl Coords {
    fn add_to_side(&mut self, side: Side, len_diff: i16) {
        match side {
            Side::Top => self.tl.y = self.tl.y.saturating_add_signed(-len_diff),
            Side::Left => self.tl.x = self.tl.x.saturating_add_signed(-len_diff),
            Side::Bottom => self.br.y = self.br.y.saturating_add_signed(len_diff),
            Side::Right => self.br.x = self.br.x.saturating_add_signed(len_diff),
        }
    }
}

impl Display for Coords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{},{}", self.tl, self.br))
    }
}

/// An area in the window.
///
/// This area represents a rectangular region of the window, defined
/// by a [`Coords`]. It also contains information about its children
/// (if it has any).
///
/// An [`Area`] can be freely cloned and safely shared between threads
/// and will (usually) be found in Parsec's widget tree and as the
/// child of other [`Area`]s.
#[derive(Clone)]
pub struct Area {
    coords: RwData<Coords>,
    window: RwData<InnerWindow>,
    index: usize,
}

impl Area {
    fn new(coords: Coords, index: usize, window: RwData<InnerWindow>) -> Self {
        Self {
            coords: RwData::new(coords),
            window,
            index,
        }
    }

    /// Returns the maximum possible [`Area`], engulfing the entire
    /// window.
    fn total(index: usize, window: RwData<InnerWindow>) -> Self {
        let (max_x, max_y) = terminal::size().unwrap();
        let coords = Coords::new(Coord { x: 0, y: 0 }, Coord { x: max_x, y: max_y });

        Area {
            coords: RwData::new(coords),
            window,
            index,
        }
    }

    /// The [`Coords`] of the [`Area`].
    pub fn coords(&self) -> Coords {
        *self.coords.read()
    }

    /// The top-left [`Coord`] of the [`Area`].
    pub fn tl(&self) -> Coord {
        self.coords.read().tl
    }

    /// The bottom-right [`Coord`] of the [`Area`].
    pub fn br(&self) -> Coord {
        self.coords.read().br
    }

    /// The length of [`self`], in a given [`Axis`].
    fn len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis {
            self.width()
        } else {
            self.height()
        }
    }
}

impl Area {}

fn stretch_br(coords: Coords, len: u16, axis: Axis) -> Coord {
    if let Axis::Horizontal = axis {
        let x = coords.tl.x + len;
        Coord { x, y: coords.br.y }
    } else {
        let y = coords.tl.y + len;
        Coord { x: coords.br.x, y }
    }
}

fn normalize_from_tl(children: &[(Node, Split)], lens: Vec<u16>, tl: Coord, side: Side) {
    let axis = Axis::from(side);
    let mut last_tl = tl;
    let mut new_lens = lens.iter();

    for (node, split) in children.iter() {
        let mut coords = node.area.coords.write();
        let len = if let Split::Locked(len) = split {
            *len as u16
        } else {
            *new_lens.next().unwrap()
        };

        let old_len = coords.len(axis);
        coords.tl = last_tl;
        coords.br = stretch_br(*coords, len, axis);

        last_tl = coords.ortho_corner(axis);

        drop(coords);
        node.normalize_children(len as i16 - old_len as i16, side);
    }
}

fn scale_children(children: &[(Node, Split)], len_diff: i16, axis: Axis) -> Vec<u16> {
    let mut lens = children
        .iter()
        .filter_map(|(node, split)| {
            if let Split::Min(_) = *split {
                Some(node.area.len(axis) as u16)
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

// NOTE: This function is meant to be used when the `axis` is aligned
// with the parent's axis.
fn resizable_len_of(children: &[(Node, Split)], axis: Axis) -> u16 {
    children
        .iter()
        .map(|(node, split)| {
            let inner = node.area.coords.read();
            match *split {
                Split::Min(len) => (inner.len(axis).saturating_sub(len)) as u16,
                Split::Locked(_) => 0,
            }
        })
        .sum::<u16>()
}

impl Display for Area {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{},{}", self.coords.read().tl, self.coords.read().br))
    }
}

impl ui::Area for Area {
    fn width(&self) -> usize {
        self.coords.read().width()
    }

    fn height(&self) -> usize {
        self.coords.read().height()
    }

    fn request_len(&self, len: usize, side: Side) -> Result<(), ()> {
        let req_axis = Axis::from(side);
        let window = self.window.read();
        let (child_index, parent) = window.find_parent(self.index).ok_or(())?;

        let (children, axis) = parent.lineage.as_ref().unwrap();
        let (child, _) = &children[child_index];

        // If the current len is already equal to the requested len,
        // nothing needs to be done.
        if child.resizable_len(req_axis, &window) == len {
            return Ok(());
        };

        if req_axis != *axis {
            let area = parent.area.clone();
            drop(window);
            area.request_len(len, side)
        } else if parent.resizable_len(req_axis, &window) < len {
            let new_parent_width = parent.area.len(req_axis) + len - {
                match req_axis {
                    Axis::Horizontal => child.area.width(),
                    Axis::Vertical => child.area.height(),
                }
            };

            let area = parent.area.clone();
            drop(window);
            area.request_len(new_parent_width, side)
        } else {
            let ret = parent.set_child_len(child_index, len as u16, side);
            drop(window);
            ret
        }
    }

    fn bisect(&mut self, push_specs: PushSpecs) -> (usize, Option<usize>) {
        let window = self.window.read();
        let PushSpecs { side, split, .. } = push_specs;
        let axis = Axis::from(side);

        let node = window.find_node(self.index).unwrap();
        let resizable_len = node.resizable_len(axis, &window);
        drop(node);
        drop(window);

        if resizable_len < split.len() {
            self.request_len(resizable_len.max(split.len()), side).unwrap();
        }

        let mut window = self.window.write();
        let new_area_index = window.next_index.fetch_add(1, Ordering::Relaxed);

        let node = window.find_mut_node(self.index).unwrap();

        let ret = if let Some((children, _)) =
            node.lineage.as_ref().filter(|(_, other)| *other == axis)
        {
            let self_index = match side {
                Side::Bottom | Side::Right => children.len() - 1,
                Side::Top | Side::Left => 0,
            };

            node.insert_new_area(self_index, split, side, new_area_index);

            (new_area_index, None)
        } else if let Some((child_index, parent)) = window
            .find_mut_parent(self.index)
            .filter(|(_, parent)| parent.lineage.as_ref().is_some_and(|(_, other)| *other == axis))
        {
            parent.insert_new_area(child_index, split, side, new_area_index);

            (new_area_index, None)
        } else {
            let new_child_index = window.next_index.fetch_add(1, Ordering::Relaxed);
            // NOTE: ???????
            let area = Area::new(self.coords(), self.index, self.window.clone());
            let new_parent = Node::new(area, None);

            let swapped_parent = window.find_mut_node(self.index).unwrap();
            let mut node = std::mem::replace(swapped_parent, new_parent);
            node.area.index = new_area_index;

            swapped_parent.lineage = Some((vec![(node, Split::default())], axis));

            swapped_parent.insert_new_area(0, split, side, new_child_index);

            (new_child_index, Some(new_area_index))
        };

        ret
    }

    fn request_width_to_fit(&self, _text: &str) -> Result<(), ()> {
        todo!()
    }
}

pub struct Label {
    area: Area,
    stdout_lock: StdoutLock<'static>,

    cursor: Coord,
    is_active: bool,

    last_style: ContentStyle,
    style_before_cursor: Option<ContentStyle>,

    wrap_method: WrapMethod,
    tab_places: TabPlaces,
    indent: usize,
}

impl Label {
    fn new(area: Area) -> Self {
        let cursor = area.tl();
        let stdout_lock = stdout().lock();
        Label {
            area,
            stdout_lock,
            cursor,
            is_active: false,
            last_style: ContentStyle::default(),
            style_before_cursor: None,
            wrap_method: WrapMethod::NoWrap,
            tab_places: TabPlaces::default(),
            indent: 0,
        }
    }

    fn clear_line(&mut self) {
        let _ = queue!(self.stdout_lock, ResetColor);

        if self.cursor.x < self.area.br().x {
            // The rest of the line is featureless.
            let padding_count = (self.area.br().x - self.cursor.x) as usize;
            queue!(self.stdout_lock, Print(" ".repeat(padding_count))).unwrap();
        }

        self.cursor.x = self.area.tl().x;
        self.cursor.y += 1;

        let _ = queue!(
            self.stdout_lock,
            ResetColor,
            MoveTo(self.cursor.x, self.cursor.y),
            SetStyle(self.last_style)
        );
    }

    fn wrap_line(&mut self) -> PrintStatus {
        self.clear_line();

        let _ = queue!(
            self.stdout_lock,
            MoveTo(self.cursor.x, self.cursor.y),
            Print(" ".repeat(self.indent))
        );

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
    fn area(&self) -> &Area {
        &self.area
    }

    fn set_form(&mut self, form: Form) {
        self.last_style = form.style;
        let _ = queue!(self.stdout_lock, ResetColor, SetStyle(form.style));
    }

    fn place_main_cursor(&mut self, cursor_style: CursorStyle) {
        if let (Some(caret), true) = (cursor_style.caret, self.is_active) {
            let _ = queue!(self.stdout_lock, caret, SavePosition);
            SHOW_CURSOR.store(true, Ordering::Relaxed)
        } else {
            self.style_before_cursor = Some(self.last_style);
            let _ = queue!(self.stdout_lock, SetStyle(cursor_style.form.style));
        }
    }

    fn place_extra_cursor(&mut self, cursor_style: CursorStyle) {
        self.style_before_cursor = Some(self.last_style);
        let _ = queue!(self.stdout_lock, SetStyle(cursor_style.form.style));
    }

    fn set_as_active(&mut self) {
        self.is_active = true;
        SHOW_CURSOR.store(false, Ordering::Relaxed)
    }

    fn start_printing(&mut self, config: &Config) {
        self.wrap_method = config.wrap_method;
        self.tab_places = config.tab_places.clone();

        while IS_PRINTING
            .compare_exchange_weak(false, true, Ordering::Relaxed, Ordering::Relaxed)
            .is_err()
        {
            std::thread::sleep(std::time::Duration::from_micros(500));
        }

        let _ = queue!(self.stdout_lock, MoveTo(self.area.tl().x, self.area.tl().y));
        let _ = execute!(self.stdout_lock, cursor::Hide);
    }

    fn stop_printing(&mut self) {
        while let PrintStatus::NextChar = self.next_line() {}

        let _ = execute!(self.stdout_lock, ResetColor);

        if SHOW_CURSOR.load(Ordering::Relaxed) {
            let _ = self.stdout_lock.write_all(b"\x1b[8\x1b[?25h");

            let _ = execute!(self.stdout_lock, RestorePosition, cursor::Show);
        }

        IS_PRINTING.store(false, Ordering::Relaxed);
    }

    fn print(&mut self, ch: char, x_shift: usize) -> PrintStatus {
        let len = match ch {
            '\t' => self.tab_places.spaces_on_col(x_shift) as u16,
            _ => UnicodeWidthChar::width(ch).unwrap_or(0) as u16,
        };

        if self.cursor.x <= self.area.br().x - len {
            self.cursor.x += len;
            let _ = {
                let mut temp = [b'a'; 4];
                self.stdout_lock.write_all(ch.encode_utf8(&mut temp).as_bytes())
            };
            if let Some(style) = self.style_before_cursor.take() {
                let _ = queue!(self.stdout_lock, ResetColor, SetStyle(style));
            }
        } else if self.cursor.x <= self.area.br().x {
            let width = self.area.br().x - self.cursor.x;
            let _ = queue!(self.stdout_lock, Print(" ".repeat(width as usize)));
            if let Some(style) = self.style_before_cursor.take() {
                let _ = queue!(self.stdout_lock, ResetColor, SetStyle(style));
            }
        }

        if self.cursor.x == self.area.br().x {
            if let WrapMethod::NoWrap = self.wrap_method {
                PrintStatus::NextLine
            } else {
                self.wrap_line()
            }
        } else {
            PrintStatus::NextChar
        }
    }

    fn next_line(&mut self) -> PrintStatus {
        if self.cursor.y + 1 < self.area.br().y {
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

unsafe impl Send for Label {}
unsafe impl Sync for Label {}

pub enum Anchor {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

pub struct Node {
    area: Area,
    lineage: Option<(Vec<(Node, Split)>, Axis)>,
}

impl Node {
    pub fn new(area: Area, lineage: Option<(Vec<(Node, Split)>, Axis)>) -> Self {
        Self { area, lineage }
    }

    /// The length of [`self`] that can be resized, in a given
    /// [`Axis`].
    ///
    /// This length consists of the total length that is not contained
    /// within children whose split is [`Split::Locked(_)`].
    fn resizable_len(&self, axis: Axis, window: &InnerWindow) -> usize {
        if let Axis::Horizontal = axis {
            self.resizable_width(window)
        } else {
            self.resizable_height(window)
        }
    }

    /// The width of [`self`] that can be resized.
    ///
    /// This width consists of the total width that is not contained
    /// within children whose split is [`Split::Locked(_)`].
    fn resizable_width(&self, window: &InnerWindow) -> usize {
        if let Some((child_index, parent)) = window.find_parent(self.area.index) {
            let (children, axis) = parent.lineage.as_ref().unwrap();
            if let (Axis::Horizontal, Split::Locked(_)) = (axis, children[child_index].1) {
                return 0;
            }
        };
        drop(window);

        if let Some((children, ..)) = &self.lineage {
            if children.is_empty() {
                self.area.width()
            } else {
                children.iter().map(|(node, _)| node.resizable_width(window)).sum()
            }
        } else {
            self.area.width()
        }
    }

    /// The height of [`self`] that can be resized.
    ///
    /// This height consists of the total height that is not contained
    /// within children whose split is [`Split::Locked(_)`].
    fn resizable_height(&self, window: &InnerWindow) -> usize {
        if let Some((child_index, parent)) = window.find_parent(self.area.index) {
            let (children, axis) = parent.lineage.as_ref().unwrap();
            if let (Axis::Vertical, Split::Locked(_)) = (axis, children[child_index].1) {
                return 0;
            }
        };
        drop(window);

        if let Some((children, ..)) = &self.lineage {
            if children.is_empty() {
                self.area.height()
            } else {
                children.iter().map(|(node, _)| node.resizable_height(window)).sum()
            }
        } else {
            self.area.height()
        }
    }

    fn find_node(&self, area_index: usize) -> Option<&Self> {
        if self.area.index == area_index {
            Some(self)
        } else {
            if let Some((children, _)) = &self.lineage {
                for (child, _) in children {
                    let area = child.find_node(area_index);
                    if area.is_some() {
                        return area;
                    }
                }
            }

            None
        }
    }

    fn find_mut_node(&mut self, area_index: usize) -> Option<&mut Self> {
        if self.area.index == area_index {
            Some(self)
        } else {
            if let Some((children, _)) = &mut self.lineage {
                for (child, _) in children {
                    let node = child.find_mut_node(area_index);
                    if node.is_some() {
                        return node;
                    }
                }
            }

            None
        }
    }

    fn find_parent_index(&self, area_index: usize) -> Option<(usize, usize)> {
        if let Some((children, _)) = &self.lineage {
            match children.iter().position(|(node, _)| node.area.index == area_index) {
                Some(index) => {
                    drop(children);
                    return Some((index, self.area.index));
                }
                None => {
                    for (node, _) in children {
                        if let Some(ret) = node.find_parent_index(area_index) {
                            return Some(ret);
                        }
                    }

                    None
                }
            }
        } else {
            None
        }
    }

    fn find_parent(&self, area_index: usize) -> Option<(usize, &Node)> {
        let parent = if let Some((children, _)) = &self.lineage {
            match children.iter().position(|(node, _)| node.area.index == area_index) {
                Some(index) => Some((index, self.area.index)),
                None => {
                    let mut parent = None;
                    for (node, _) in children {
                        if let Some(ret) = node.find_parent_index(area_index) {
                            parent = Some(ret);
                        }
                    }

                    parent
                }
            }
        } else {
            None
        };

        parent
            .map(|(child_index, parent_index)| (child_index, self.find_node(parent_index).unwrap()))
    }

    fn find_mut_parent(&mut self, area_index: usize) -> Option<(usize, &mut Self)> {
        let parent = if let Some((children, _)) = &mut self.lineage {
            match children.iter().position(|(node, _)| node.area.index == area_index) {
                Some(index) => Some((index, self.area.index)),
                None => {
                    let mut parent = None;
                    for (node, _) in children {
                        if let Some(ret) = node.find_parent_index(area_index) {
                            parent = Some(ret);
                        }
                    }

                    parent
                }
            }
        } else {
            None
        };

        parent.map(|(child_index, parent_index)| {
            (child_index, self.find_mut_node(parent_index).unwrap())
        })
    }

    // NOTE: This function will only be called once it is known that the
    // parent has enough length to accomodate the specific resize.
    /// Tries to set the length of a given `index` to `new_len`,
    /// expanding from [`side`][Side].
    ///
    /// Returns [`Ok(())`] if the child's length was changed
    /// succesfully, and [`Err(())`] if it wasn't.
    fn set_child_len(&self, child_index: usize, new_len: u16, side: Side) -> Result<(), ()> {
        let (children, axis) = self.lineage.as_ref().unwrap();

        let bef_len = resizable_len_of(&children[..child_index], *axis) as i16;
        let aft_len = if children.len() > child_index {
            resizable_len_of(&children[(child_index + 1)..], *axis) as i16
        } else {
            0
        };

        let old_len = children.get(child_index).unwrap().0.area.len(*axis);
        let len_diff = new_len as i16 - old_len as i16;

        let (bef_diff, aft_diff) = if let Side::Right | Side::Bottom = side {
            let aft_diff = aft_len.min(len_diff);
            let bef_diff = len_diff - aft_diff;

            let mut coords = children.get(child_index).unwrap().0.area.coords.write();
            coords.add_to_side(side, aft_diff);
            coords.add_to_side(side.opposite(), bef_diff);
            (-bef_diff, -aft_diff)
        } else {
            let bef_diff = bef_len.min(len_diff);
            let aft_diff = len_diff - bef_diff;

            let mut coords = children.get(child_index).unwrap().0.area.coords.write();
            coords.add_to_side(side, bef_diff);
            coords.add_to_side(side.opposite(), aft_diff);
            (-bef_diff, -aft_diff)
        };

        let bef_lens = scale_children(&children[..child_index], bef_diff, *axis);
        normalize_from_tl(&children[..child_index], bef_lens, self.area.tl(), side);

        let aft_lens = scale_children(&children[(child_index + 1)..], aft_diff, *axis);
        let tl = children.get(child_index).unwrap().0.area.coords().ortho_corner(*axis);
        normalize_from_tl(&children[(child_index + 1)..], aft_lens, tl, side);

        Ok(())
    }

    fn insert_new_area(&mut self, child_index: usize, split: Split, side: Side, node_index: usize) {
        let (children, _) = self.lineage.as_mut().unwrap();

        let coords = children
            .get(child_index)
            .map(|(node, _)| node.area.coords())
            .unwrap_or(self.area.coords())
            .empty_on(side);

        let self_index = match side {
            Side::Bottom | Side::Right => child_index + 1,
            Side::Top | Side::Left => child_index,
        };

        let area = Area::new(coords, node_index, self.area.window.clone());
        children.insert(self_index, (Node::new(area, None), split));

        self.set_child_len(self_index, split.len() as u16, side).unwrap();
    }

    /// Normalizes the children of this [`Area`] to fit the resized
    /// length of their parent.
    ///
    /// When an [`Area`] is resized, its [`Coords`] are changed, but
    /// the [`Coords`] of its children still reflect the old
    /// [`Coords`] from their parent. This function's purpose is
    /// to adjust them to the new [`Coords`] of their parent.
    fn normalize_children(&self, len_diff: i16, side: Side) {
        let Some((children, axis)) = &self.lineage else {
            return;
        };

        if Axis::from(side) == *axis {
            let new_lens = scale_children(children, len_diff, *axis);
            normalize_from_tl(children, new_lens, self.area.tl(), side);
        } else {
            for (node, _) in children {
                let mut coords = node.area.coords.write();
                coords.add_to_side(side, len_diff);
            }
        }
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Area")
            .field("coords", &self.area.coords())
            .field("index", &self.area.index)
            .field("lineage", &self.lineage)
            .finish()
    }
}

struct InnerWindow {
    main_node: Option<Node>,
    floating_nodes: Vec<(Node, Anchor)>,
    next_index: Arc<AtomicUsize>,
}

impl InnerWindow {
    fn find_node(&self, node_index: usize) -> Option<&Node> {
        let floating = self.floating_nodes.iter().map(|(node, _)| node);
        for node in floating.chain(std::iter::once(self.main_node.as_ref().unwrap())) {
            let ret = node.find_node(node_index);
            if ret.is_some() {
                return ret;
            }
        }

        None
    }

    fn find_mut_node(&mut self, node_index: usize) -> Option<&mut Node> {
        let floating = self.floating_nodes.iter_mut().map(|(node, _)| node);
        for node in floating.chain(std::iter::once(self.main_node.as_mut().unwrap())) {
            let ret = node.find_mut_node(node_index);
            if ret.is_some() {
                return ret;
            }
        }

        None
    }

    fn find_parent(&self, area_index: usize) -> Option<(usize, &Node)> {
        let floating = self.floating_nodes.iter().map(|(node, _)| node);
        for node in floating.chain(std::iter::once(self.main_node.as_ref().unwrap())) {
            let ret = node.find_parent(area_index);
            if ret.is_some() {
                return ret;
            }
        }

        None
    }

    fn find_mut_parent(&mut self, node_index: usize) -> Option<(usize, &mut Node)> {
        let floating = self.floating_nodes.iter_mut().map(|(node, _)| node);
        for node in floating.chain(std::iter::once(self.main_node.as_mut().unwrap())) {
            let ret = node.find_mut_parent(node_index);
            if ret.is_some() {
                return ret;
            }
        }

        None
    }
}

#[derive(Clone)]
pub struct Window {
    inner: RwData<InnerWindow>,
}

impl ui::Window for Window {
    type Area = Area;
    type Label = Label;

    fn layout_has_changed(&self) -> bool {
        todo!(); // self.layout_has_changed.swap(false,
                 // Ordering::Relaxed)
    }

    fn get_area(&self, area_index: usize) -> Option<Self::Area> {
        self.inner.read().find_node(area_index).map(|node| node.area.clone())
    }

    fn get_label(&self, area_index: usize) -> Option<Self::Label> {
        self.inner
            .read()
            .find_node(area_index)
            .filter(|node| node.lineage.is_none())
            .map(|node| Label::new(node.area.clone()))
    }
}

pub struct Ui {
    layout_has_changed: AtomicBool,
    windows: Vec<Window>,
    next_index: Arc<AtomicUsize>,
}

impl Default for Ui {
    fn default() -> Self {
        Ui {
            layout_has_changed: AtomicBool::new(true),
            windows: Vec::new(),
            next_index: Arc::new(AtomicUsize::new(0)),
        }
    }
}

impl ui::Ui for Ui {
    type Area = Area;
    type Label = Label;
    type Window = Window;

    fn new_window(&mut self) -> (Self::Window, Self::Label) {
        let new_area_index = self.next_index.fetch_add(1, Ordering::Acquire);
        let inner_window = RwData::new(InnerWindow {
            main_node: None,
            floating_nodes: Vec::new(),
            next_index: self.next_index.clone(),
        });

        let area = Area::total(new_area_index, inner_window.clone());
        let node = Node::new(area.clone(), None);
        inner_window.write().main_node = Some(node);

        let window = Window {
            inner: inner_window,
        };
        self.windows.push(window.clone());

        (window, Label::new(area))
    }

    fn startup(&mut self) {
        // This makes it so that if the application panics, the panic message
        // is printed nicely and the terminal is left in a usable
        // state.
        use std::panic;
        let orig_hook = panic::take_hook();
        panic::set_hook(Box::new(move |panic_info| {
            let _ = execute!(
                io::stdout(),
                terminal::Clear(ClearType::All),
                terminal::LeaveAlternateScreen,
                cursor::Show
            );

            terminal::disable_raw_mode().unwrap();

            orig_hook(panic_info);
            std::process::exit(1)
        }));

        let _ = execute!(io::stdout(), terminal::EnterAlternateScreen);
        terminal::enable_raw_mode().unwrap();
    }

    fn shutdown(&mut self) {
        let _ = execute!(
            io::stdout(),
            terminal::Clear(ClearType::All),
            terminal::LeaveAlternateScreen,
            cursor::Show
        );
        terminal::disable_raw_mode().unwrap();
    }
}
