#![feature(stmt_expr_attributes, is_some_and, option_as_slice)]

use std::{
    fmt::{Debug, Display},
    io::{stdout, Stdout},
    sync::atomic::{AtomicBool, Ordering},
};

use crossterm::{
    cursor::{self, MoveTo, RestorePosition, SavePosition},
    execute, queue,
    style::{ContentStyle, Print, ResetColor, SetStyle},
    terminal::{self, ClearType},
};

use parsec_core::{config::Config, text::PrintStatus, ui::{PushSpecs, Node}, widgets::Widget};
use parsec_core::{
    config::{RwData, TabPlaces, WrapMethod},
    tags::{form::CursorStyle, form::Form},
    ui::{self, Area as UiArea, Axis, Label as UiLabel, Side, Split},
    log_info
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
            Side::Left => self.tl.y = self.tl.y.saturating_add_signed(-len_diff),
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

/// The "owner" of an [`Area`].
///
/// The `Owner` can be one of 2 types:
/// - A [`Parent`][Owner::Parent], which can hold an arbitrary number
///   of children.
/// - An anchor, based on some defined corner. The anchor symbolizes a
///   static position on the screen, and dictates a sort of "window
///   within a window".
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
                .field("parent.coords", &parent.coords())
                .field("self_index", &self_index)
                .field("split", split)
                .finish(),
            _ => todo!(),
        }
    }
}

impl Owner {
    fn split(owner: &Option<Self>) -> Option<Split> {
        if let Some(Owner::Parent { split, .. }) = owner {
            Some(*split)
        } else {
            None
        }
    }

    /// Returns an [`Option<Split>`], which is [`Some`] when `owner`
    /// is [`Some(Owner::Parent)`].
    fn axial_split(owner: &Option<Self>, cmp: Axis) -> Option<Split> {
        if let Some(Owner::Parent { split, parent, .. }) = owner {
            if parent.lineage.read().as_ref().is_some_and(|(_, axis)| *axis == cmp) {
                Some(*split)
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Returns true only if the [`Owner`] is [`Some(Owner::Parent)`],
    /// and if the parent's axis is equal to `other`.
    fn aligns(owner: &Option<Self>, other: Axis) -> Option<Self> {
        if let Some(Owner::Parent { parent, .. }) = owner {
            if parent.lineage.read().as_ref().is_some_and(|(_, axis)| *axis == other) {
                return owner.clone();
            }
        }

        None
    }
}

/// The guts of the [`Area`], containing its [`Coords`] and [`Owner`].
#[derive(Clone)]
struct InnerArea {
    coords: Coords,
    owner: Option<Owner>,
}

impl InnerArea {
    /// Returns a new instance of [`InnerArea`].
    fn new(coords: Coords, owner: Option<Owner>) -> Self {
        InnerArea { coords, owner }
    }

    /// The length of [`self`], given an axis to measure it from.
    fn len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis {
            self.coords.width()
        } else {
            self.coords.height()
        }
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
    inner: RwData<InnerArea>,
    lineage: RwData<Option<(Vec<Area>, Axis)>>,
}

impl Debug for Area {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Area")
            .field("inner.owner", &self.inner.read().owner)
            .field("inner.coords", &self.inner.read().coords)
            .field(
                "lineage",
                &self.lineage.read().as_ref().map(|(areas, axis)| {
                    (areas.iter().map(|area| area.coords()).collect::<Vec<Coords>>(), axis)
                }),
            )
            .finish()
    }
}

impl Area {
    fn new(inner: InnerArea, lineage: Option<(Vec<Area>, Axis)>) -> Self {
        Self {
            inner: RwData::new(inner),
            lineage: RwData::new(lineage),
        }
    }

    /// Returns the maximum possible [`Area`], engulfing the entire
    /// window.
    fn total() -> Self {
        let (max_x, max_y) = terminal::size().unwrap();
        let coords = Coords::new(Coord { x: 0, y: 0 }, Coord { x: max_x, y: max_y });

        Area {
            inner: RwData::new(InnerArea::new(coords, None)),
            lineage: RwData::new(None),
        }
    }

    /// The [`Coords`] of the [`Area`].
    pub fn coords(&self) -> Coords {
        self.inner.read().coords
    }

    /// The top-left [`Coord`] of the [`Area`].
    pub fn tl(&self) -> Coord {
        self.inner.read().coords.tl
    }

    /// The bottom-right [`Coord`] of the [`Area`].
    pub fn br(&self) -> Coord {
        self.inner.read().coords.br
    }

    /// The length of [`self`], in a given [`Axis`].
    fn len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis {
            self.width()
        } else {
            self.height()
        }
    }

    /// The length of [`self`] that can be resized, in a given
    /// [`Axis`].
    ///
    /// This length consists of the total length that is not contained
    /// within children whose split is [`Split::Locked(_)`].
    fn resizable_len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis {
            self.resizable_width()
        } else {
            self.resizable_height()
        }
    }

    /// The width of [`self`] that can be resized.
    ///
    /// This width consists of the total width that is not contained
    /// within children whose split is [`Split::Locked(_)`].
    fn resizable_width(&self) -> usize {
        if let Some(Split::Locked(_)) =
            Owner::axial_split(&self.inner.read().owner, Axis::Horizontal)
        {
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

    /// The height of [`self`] that can be resized.
    ///
    /// This height consists of the total height that is not contained
    /// within children whose split is [`Split::Locked(_)`].
    fn resizable_height(&self) -> usize {
        if let Some(Split::Locked(_)) = Owner::axial_split(&self.inner.read().owner, Axis::Vertical)
        {
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

impl Area {
    /// Normalizes the children of this [`Area`] to fit the resized
    /// length of their parent.
    ///
    /// When an [`Area`] is resized, its [`Coords`] are changed, but
    /// the [`Coords`] of its children still reflect the old
    /// [`Coords`] from their parent. This function's purpose is
    /// to adjust them to the new [`Coords`] of their parent.
    fn normalize_children(&mut self, len_diff: i16, side: Side) {
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
                inner.coords.add_to_side(side, len_diff);
            }
        }
    }

    fn insert_new_area(&mut self, index: usize, split: Split, side: Side) -> Area {
        let mut lineage = self.lineage.write();
        let (children, _) = lineage.as_mut().unwrap();

        let coords = children
            .get(index)
            .map(|area| area.coords())
            .unwrap_or(self.coords())
            .empty_on(side);

        let self_index = match side {
            Side::Bottom | Side::Right => index + 1,
            Side::Top | Side::Left => index,
        };
        let owner = Owner::Parent {
            parent: self.clone(),
            self_index,
            split,
        };
        let area = Area::new(InnerArea::new(coords, Some(owner)), None);

        for child in children.get_mut(self_index..).into_iter().flatten() {
            let mut inner = child.inner.write();
            let Some(Owner::Parent {self_index, ..}) = &mut inner.owner else {
                unreachable!();
            };

            *self_index += 1;
        }

        children.insert(self_index, area.clone());

        drop(children);
        drop(lineage);
        self.set_child_len(self_index, split.len() as u16, side).unwrap();

        area
    }

    // NOTE: This function will only be called once it is known that the
    // parent has enough length to accomodate the specific resize.
    /// Tries to set the length of a given `index` to `new_len`,
    /// expanding from [`side`][Side].
    ///
    /// Returns [`Ok(())`] if the child's length was changed
    /// succesfully, and [`Err(())`] if it wasn't.
    fn set_child_len(&mut self, index: usize, new_len: u16, side: Side) -> Result<(), ()> {
        let mut lineage = self.lineage.write();
        let (children, axis) = lineage.as_mut().unwrap();

        let bef_len = resizable_len_of(&children[..index], *axis) as i16;
        let aft_len = if children.len() > index {
            resizable_len_of(&children[(index + 1)..], *axis) as i16
        } else {
            0
        };

        let old_len = children.get(index).unwrap().len(*axis);
        let len_diff = new_len as i16 - old_len as i16;

        let (bef_diff, aft_diff) = if let Side::Right | Side::Bottom = side {
            let aft_diff = aft_len.min(len_diff);
            let bef_diff = len_diff - aft_diff;

            let mut target_inner = children.get_mut(index).unwrap().inner.write();
            target_inner.coords.add_to_side(side, aft_diff);
            target_inner.coords.add_to_side(side.opposite(), bef_diff);
            (-bef_diff, -aft_diff)
        } else {
            let bef_diff = bef_len.min(len_diff);
            let aft_diff = len_diff - bef_diff;

            let mut target_inner = children.get_mut(index).unwrap().inner.write();
            target_inner.coords.add_to_side(side, bef_diff);
            target_inner.coords.add_to_side(side.opposite(), aft_diff);
            (-bef_diff, -aft_diff)
        };

        let bef_lens = scale_children(&children[..index], bef_diff, *axis);
        normalize_from_tl(&mut children[..index], bef_lens, self.tl(), side);

        let aft_lens = scale_children(&children[(index + 1)..], aft_diff, *axis);
        let tl = children.get(index).unwrap().coords().ortho_corner(*axis);
        normalize_from_tl(&mut children[(index + 1)..], aft_lens, tl, side);

		log_info!("\nchildren: {:#?}, index: {}", children, index);

        Ok(())
    }
}

impl From<InnerArea> for Area {
    fn from(value: InnerArea) -> Self {
        Area {
            inner: RwData::new(value),
            lineage: RwData::new(None),
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

    for (index, child) in children.iter_mut().enumerate() {
        let mut inner = child.inner.write();
        let len = if let Split::Locked(len) = Owner::split(&inner.owner).unwrap() {
            len as u16
        } else {
            *new_lens.next().unwrap()
        };

        let old_len = inner.len(axis);
        inner.coords.tl = last_tl;
        inner.coords.br = stretch_br(inner.coords, len, axis);

        last_tl = inner.coords.ortho_corner(axis);

        drop(inner);
        child.normalize_children(len as i16 - old_len as i16, side);
    }
}

fn scale_children(children: &[Area], len_diff: i16, axis: Axis) -> Vec<u16> {
    let mut lens = children
        .iter()
        .filter_map(|child| {
            if let Some(Split::Min(_)) = Owner::split(&child.inner.read().owner) {
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

// NOTE: This function is meant to be used when the `axis` is aligned
// with the parent's axis.
fn resizable_len_of(children: &[Area], axis: Axis) -> u16 {
    children
        .iter()
        .map(|child| {
            let inner = child.inner.read();
            if let Some(Split::Min(len)) = Owner::split(&inner.owner) {
                (inner.len(axis).saturating_sub(len)) as u16
            } else {
                0 as u16
            }
        })
        .sum::<u16>()
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
        if self.len(req_axis) == len {
            return Ok(());
        };

        let inner = self.inner.write();
        let InnerArea { coords, owner } = &mut inner.clone();
        drop(inner);

        match owner {
            Some(Owner::Parent {
                ref mut parent,
                self_index,
                ..
            }) => {
                let &(_, axis) = parent.lineage.read().as_ref().unwrap();

                let ret = if req_axis != axis {
                    parent.request_len(len, side)
                } else if parent.resizable_len(req_axis) < len {
                    let new_parent_width = parent.len(req_axis) + len - {
                        let ref this = coords;
                        match req_axis {
                            Axis::Horizontal => this.width(),
                            Axis::Vertical => this.height(),
                        }
                    };
                    parent.request_len(new_parent_width, side)
                } else {
                    parent.set_child_len(*self_index, len as u16, side)
                };

                log_info!("\nchildren {:#?}", parent.lineage.read().as_ref().unwrap().0);

                ret
            }
            Some(Owner::TlAnchor) => todo!(),
            Some(Owner::TrAnchor) => todo!(),
            Some(Owner::BlAnchor) => todo!(),
            Some(Owner::BrAnchor) => todo!(),
            None => {
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
    layout_has_changed: AtomicBool,
    nodes: Vec<Node<Ui>>,
    next_node_index: usize
}

impl Ui {
}

impl Default for Ui {
    fn default() -> Self {
        Ui {
            layout_has_changed: AtomicBool::new(true),
            areas: Vec::new(),
            next_node_index: 0
        }
    }
}

impl ui::Ui for Ui {
    type Area = Area;
    type Label = Label;
    type NodeIndex = NodeIndex;
    type Widgets<'a> = Widgets<'a>;

    fn bisect_area(
        &mut self,
        area: &mut Self::Area,
        push_specs: PushSpecs,
    ) -> (Self::Label, Option<Self::Area>) {
        let PushSpecs { side, split, .. } = push_specs;
        let axis = Axis::from(side);

        let resizable_len = area.resizable_len(axis);
        if let Err(_) = area.request_len(resizable_len.max(split.len()), side) {
            panic!();
        }

        let mut lineage = area.lineage.write();
        let mut inner = area.inner.write();
        if let Some((children, _)) = &mut lineage.as_mut().filter(|(_, cmp)| *cmp == axis) {
            let self_index = match side {
                Side::Bottom | Side::Right => children.len() - 1,
                Side::Top | Side::Left => 0,
            };

            drop(children);
            drop(lineage);
            drop(inner);
            (Label::new(area.insert_new_area(self_index, split, side)), None)
        } else if let Some(Owner::Parent {
            mut parent,
            self_index,
            ..
        }) = Owner::aligns(&inner.owner, axis)
        {
            drop(lineage);

            (Label::new(parent.insert_new_area(self_index, split, side)), None)
        } else {
            drop(lineage);

            let lineage = (vec![area.clone()], Axis::from(side));
            let parent_inner = InnerArea::new(inner.coords, inner.owner.clone());
            let mut parent = Area::new(parent_inner, Some(lineage));

            inner.owner = Some(Owner::Parent {
                parent: parent.clone(),
                self_index: 0,
                split: Split::Min(0),
            });

            drop(inner);
            let area = parent.insert_new_area(0, split, side);

            (Label::new(area), Some(parent))
        }
    }

    fn maximum_label(&mut self) -> Self::Label {
        self.layout_has_changed.store(true, Ordering::Relaxed);
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
        self.layout_has_changed.swap(false, Ordering::Relaxed)
    }

    fn widgets(&self) -> Self::Widgets<'_> {
        Widgets { ui: self, cur_node_index: NodeIndex(0) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct NodeIndex(usize);

struct Widgets<'a> {
    ui: &'a Ui,
    cur_node_index: NodeIndex,
}

impl<'a> Iterator for Widgets<'a> {
    type Item = (&'a Widget<Ui>, &'a RwData<EndNode<Ui>>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.ui.main_node.find(self.cur_node_index) {
                Some(Node::EndNode {
                    end_node, widget, ..
                }) => {
                    self.cur_node_index.0 += 1;
                    return Some((widget, end_node));
                }
                None => return None,
                _ => self.cur_node_index.0 += 1,
            };
        }
    }
}
