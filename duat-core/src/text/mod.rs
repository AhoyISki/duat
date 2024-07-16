mod cfg;
mod iter;
pub mod reader;
mod records;
mod tags;
mod types;

use std::{
    fmt::{Display, Write},
    ops::{Range, RangeBounds},
    path::Path,
    str::from_utf8_unchecked,
    sync::LazyLock,
};

use gapbuf::GapBuffer;
use point::TwoPoints;
use records::Records;

use self::tags::{Markers, RawTag, Tags};
pub use self::{
    cfg::*,
    iter::{Item, Iter, RevIter},
    point::Point,
    tags::{Marker, Tag, ToggleId},
    types::Part,
};
use crate::{
    data::{RoData, RwData},
    history::Change,
    input::Cursors,
    palette::{self, FormId},
};

/// The text in a given area.
#[derive(Default, Clone, Eq)]
pub struct Text {
    buf: Box<GapBuffer<u8>>,
    pub tags: Box<Tags>,
    /// This [`Marker`] is used for the addition and removal of cursor
    /// [`Tag`]s.
    marker: Marker,
    pub records: Records<(usize, usize, usize)>,
}

impl Text {
    pub fn new() -> Self {
        Self {
            buf: Box::new(GapBuffer::new()),
            tags: Box::new(Tags::new()),
            marker: Marker::base(),
            records: Records::new(),
        }
    }

    pub fn from_file(path: impl AsRef<Path>) -> Self {
        let file = std::fs::read_to_string(path).expect("File failed to open");
        let buf = Box::new(GapBuffer::from_iter(file.bytes()));
        let tags = Box::new(Tags::with_len(buf.len()));

        Self {
            buf,
            tags,
            marker: Marker::base(),
            records: Records::with_max((file.len(), file.chars().count(), file.lines().count())),
        }
    }

    pub fn builder() -> Builder {
        Builder::new()
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace.
    fn replace_range(&mut self, old: Range<usize>, edit: impl AsRef<str>) {
        let edit = edit.as_ref();

        let old_start = {
            let p = self.point_at(old.start);
            (p.byte(), p.char(), p.line())
        };

        let old_len = unsafe {
            let str = String::from_utf8_unchecked(
                self.buf
                    .splice(old.clone(), edit.as_bytes().iter().cloned())
                    .collect(),
            );

            let lines = str.bytes().filter(|b| *b == b'\n').count();
            (str.len(), str.chars().count(), lines)
        };

        let new_len = {
            let lines = edit.bytes().filter(|b| *b == b'\n').count();
            (edit.len(), edit.chars().count(), lines)
        };

        self.records.transform(old_start, old_len, new_len);

        let new_end = old.start + edit.len();
        self.tags.transform(old, new_end);
    }

    pub(crate) fn insert_str(&mut self, at: usize, str: &str) {
        self.replace_range(at..at, str);
    }

    pub(crate) fn apply_change(&mut self, change: &Change) {
        self.replace_range(change.taken_range(), &change.added_text);
    }

    pub(crate) fn undo_change(&mut self, change: &Change, chars: isize) {
        let start = change.start.saturating_add_signed(chars);
        let end = change.added_end().saturating_add_signed(chars);
        self.replace_range(start..end, &change.taken_text);
    }

    fn clear(&mut self) {
        self.buf = Box::new(GapBuffer::new());
        self.tags.clear();
        self.records.clear();
    }

    pub fn insert_tag(&mut self, at: usize, tag: Tag, marker: Marker) {
        self.tags.insert(at, tag, marker);
    }

    pub fn remove_tags_on(&mut self, b: usize, markers: impl Markers) {
        self.tags.remove_at(b, markers)
    }

    /// Removes the tags for all the cursors, used before they are
    /// expected to move.
    pub(crate) fn add_cursor_tags(&mut self, cursors: &Cursors) {
        for (cursor, is_main) in cursors.iter() {
            let Range { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = cursor_tags(is_main);

            let tags = [
                (start, start_tag),
                (end, end_tag),
                (cursor.caret().byte(), caret_tag),
            ];

            let no_selection = if start == end { 2 } else { 0 };

            for (b, tag) in tags.into_iter().skip(no_selection) {
                let point = self.point_at(b);
                let record = (point.byte(), point.char(), point.line());
                self.records.insert(record);
                self.tags.insert(b, tag, self.marker);
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are
    /// expected to have moved.
    pub(crate) fn remove_cursor_tags(&mut self, cursors: &Cursors) {
        for (cursor, _) in cursors.iter() {
            let Range { start, end } = cursor.range();
            let skip = if start == end { 1 } else { 0 };
            for ch_index in [start, end].into_iter().skip(skip) {
                self.tags.remove_at(ch_index, self.marker);
            }
        }
    }

    pub(crate) fn write_to(&self, mut writer: impl std::io::Write) -> std::io::Result<usize> {
        let (s0, s1) = self.buf.as_slices();
        Ok(writer.write(s0)? + writer.write(s1)?)
    }

    pub fn slices(&self) -> (&'_ str, &'_ str) {
        let (s0, s1) = self.buf.as_slices();
        unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) }
    }

    pub fn strs_in_range(
        &self,
        range: impl RangeBounds<usize> + std::fmt::Debug,
    ) -> (&'_ str, &'_ str) {
        let (s0, s1) = self.buf.as_slices();
        let (start, end) = get_ends(range, self.len_bytes());

        unsafe {
            let r0 = start.min(s0.len())..end.min(s0.len());
            let r1 = start.saturating_sub(s0.len())..end.saturating_sub(s0.len());

            (from_utf8_unchecked(&s0[r0]), from_utf8_unchecked(&s1[r1]))
        }
    }

    pub fn tags(&self) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        self.tags.iter_at(0)
    }

    pub fn tags_at(&self, at: usize) -> impl Iterator<Item = (usize, RawTag)> + Clone + '_ {
        self.tags.iter_at(at)
    }

    pub fn chars(&self) -> impl Iterator<Item = char> + Clone + '_ {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.chars().chain(s1.chars())
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    #[inline(always)]
    pub fn point_at(&self, at: usize) -> Point {
        assert!(
            at <= self.len_bytes(),
            "byte out of bounds: the len is {}, but the byte is {at}",
            self.len_bytes()
        );
        let (b, c, mut l) = self.records.closest_to(at);

        let found = if at >= b {
            let (s0, s1) = self.strs_in_range(b..);

            s0.char_indices()
                .chain(s1.char_indices().map(|(b, char)| (b + s0.len(), char)))
                .enumerate()
                .map(|(i, (this_b, char))| {
                    l += (char == '\n') as usize;
                    (b + this_b, c + i, l - (char == '\n') as usize)
                })
                .take_while(|&(b, ..)| at >= b)
                .last()
        } else {
            let (s0, s1) = self.strs_in_range(..b);
            let s1 = s1.chars().rev();
            let mut c_len = 0;

            s1.chain(s0.chars().rev())
                .enumerate()
                .map(|(i, char)| {
                    l -= (char == '\n') as usize;
                    c_len += char.len_utf8();
                    (b - c_len, c - (i + 1), l)
                })
                .take_while(|&(b, ..)| b >= at)
                .last()
        };

        found
            .map(|(b, c, l)| Point::from_coords(b, c, l))
            .unwrap_or(self.max_point())
    }

    #[inline(always)]
    pub fn ghost_max_points_at(&self, at: usize) -> (Point, Option<Point>) {
        let point = self.point_at(at);
        (point, self.tags.ghosts_total_at(point.byte()))
    }

    /// Points visually after the [`TwoPoints`]
    ///
    /// If the [`TwoPoints`] in question is concealed, treats the
    /// next visible character as the first character, and returns
    /// the points of the next visible character.
    ///
    /// This method is useful if you want to iterater reversibly
    /// right after a certain point, thus including the character
    /// of said point.
    pub fn points_after(&self, tp: impl TwoPoints) -> Option<(Point, Option<Point>)> {
        self.iter_at(tp)
            .filter_map(|item| item.part.as_char().map(|_| item.points()))
            .nth(1)
    }

    pub fn len_bytes(&self) -> usize {
        self.buf.len()
    }

    pub fn len_chars(&self) -> usize {
        self.records.max().1
    }

    pub fn len_lines(&self) -> usize {
        self.records.max().2
    }

    pub fn max_point(&self) -> Point {
        let (b, c, l) = self.records.max();
        Point::from_coords(b, c, l)
    }

    pub fn max_points(&self) -> (Point, Option<Point>) {
        self.ghost_max_points_at(self.max_point().byte())
    }

    /// The visual start of the line
    ///
    /// This point is defined not by where the line actually begins,
    /// but by where the last '\n' was located. For example, if
    /// [`Tag`]s create ghost text or ommit text from multiple
    /// different lines, this point may differ from where in the
    /// [`Text`] the physical line actually begins.
    pub fn visual_line_start(&self, p: impl TwoPoints) -> (Point, Option<Point>) {
        let (real, ghost) = p.to_points();

        // NOTE: 20000 is a magic number, being a guess for what a reasonable
        // limit would be.
        let mut iter = self.rev_iter_at(real).peekable();
        let mut points = (real, ghost);
        while let Some(peek) = iter.peek() {
            match peek.part {
                Part::Char('\n') => return points,
                Part::Char(_) => points = iter.next().unwrap().to_points(),
                _ => drop(iter.next()),
            }
        }

        points
    }

    /// Clones the inner [`GapBuffer`] as a [`String`]
    // NOTE: Inherent because I don't want this to implement Display
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        let (s0, s1) = self.strs_in_range(..);
        s0.to_string() + s1
    }
}

// Iterator methods.
impl Text {
    pub fn iter(&self) -> Iter<'_> {
        Iter::new_at(self, Point::default())
    }

    pub fn iter_at(&self, p: impl TwoPoints) -> Iter<'_> {
        Iter::new_at(self, p)
    }

    pub fn rev_iter(&self) -> RevIter {
        RevIter::new_at(self, self.max_point())
    }

    pub fn rev_iter_at(&self, p: impl TwoPoints) -> RevIter<'_> {
        RevIter::new_at(self, p)
    }

    pub fn iter_bytes_at(&self, b: usize) -> impl Iterator<Item = u8> + '_ {
        let (s0, s1) = self.strs_in_range(b..);
        s0.bytes().chain(s1.bytes())
    }

    pub fn iter_chars_at(&self, c: usize) -> impl Iterator<Item = char> + '_ {
        let (s0, s1) = self.strs_in_range(..);
        s0.chars().chain(s1.chars()).skip(c)
    }
}

impl<S> From<S> for Text
where
    S: ToString,
{
    fn from(value: S) -> Self {
        let value = value.to_string();
        let buf = Box::new(GapBuffer::from_iter(value.bytes()));
        let tags = Box::new(Tags::with_len(buf.len()));

        Self {
            buf,
            tags,
            marker: Marker::new(),
            records: Records::with_max((value.len(), value.chars().count(), value.lines().count())),
        }
    }
}

impl std::fmt::Debug for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Text")
            .field(
                "buf",
                &format!("'{}', '{}'", self.slices().0, self.slices().1),
            )
            .field("tags", &self.tags)
            .field("records", &self.records)
            .finish()
    }
}

impl PartialEq for Text {
    fn eq(&self, other: &Self) -> bool {
        self.buf == other.buf && self.tags == other.tags
    }
}

mod point {
    use super::Item;
    /// A position in a [`Text`].
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Point {
        b: usize,
        c: usize,
        l: usize,
    }

    impl Point {
        /// Returns a new [`Point`], at the first byte.
        pub fn new() -> Self {
            Self::default()
        }

        pub(super) fn from_coords(b: usize, c: usize, l: usize) -> Self {
            Self { b, c, l }
        }

        pub(super) fn fwd(self, char: char) -> Self {
            Self {
                b: self.b + char.len_utf8(),
                c: self.c + 1,
                l: self.l + (char == '\n') as usize,
            }
        }

        pub(super) fn rev(self, char: char) -> Self {
            Self {
                b: self.b - char.len_utf8(),
                c: self.c - 1,
                l: self.l - (char == '\n') as usize,
            }
        }

        /// Returns the byte (relative to the beginning of the file)
        /// of self. Indexed at 0.
        pub fn byte(&self) -> usize {
            self.b
        }

        /// Returns the char index (relative tow the beginning of the
        /// file). Indexed at 0.
        pub fn char(&self) -> usize {
            self.c
        }

        /// Returns the line. Indexed at 0.
        pub fn line(&self) -> usize {
            self.l
        }
    }

    pub trait TwoPoints: std::fmt::Debug + Clone + Copy {
        fn to_points(self) -> (Point, Option<Point>);
    }

    impl TwoPoints for Point {
        fn to_points(self) -> (Point, Option<Point>) {
            (self, None)
        }
    }

    impl TwoPoints for (Point, Point) {
        fn to_points(self) -> (Point, Option<Point>) {
            (self.0, Some(self.1))
        }
    }

    impl TwoPoints for (Point, Option<Point>) {
        fn to_points(self) -> (Point, Option<Point>) {
            self
        }
    }

    impl TwoPoints for Item {
        fn to_points(self) -> (Point, Option<Point>) {
            (self.real, self.ghost)
        }
    }

    impl std::ops::Add for Point {
        type Output = Self;

        fn add(self, rhs: Self) -> Self::Output {
            Self {
                b: self.b + rhs.b,
                c: self.c + rhs.c,
                l: self.l + rhs.l,
            }
        }
    }

    impl std::ops::AddAssign for Point {
        fn add_assign(&mut self, rhs: Self) {
            *self = *self + rhs;
        }
    }

    impl std::ops::Sub for Point {
        type Output = Self;

        fn sub(self, rhs: Self) -> Self::Output {
            Self {
                b: self.b - rhs.b,
                c: self.c - rhs.c,
                l: self.l - rhs.l,
            }
        }
    }

    impl std::ops::SubAssign for Point {
        fn sub_assign(&mut self, rhs: Self) {
            *self = *self - rhs;
        }
    }
}

/// Builds and modifies a [`Text`], based on replacements applied
/// to it.
///
/// The generation of text by the [`TextBuilder`] has a few
/// peculiarities that are convenient in the situations where it is
/// useful:
///
/// - The user cannot insert [`Tag`]s directly, only by appending and
///   modifying existing tags.
/// - All [`Tag`]s that are appended result in an inverse [`Tag`]
///   being placed before the next one, or at the end of the [`Tags`]
///   (e.g. [`Tag::PushForm`] would be followed a [`Tag::PopForm`]).
/// - You can insert swappable text with
///   [`push_swappable()`][Self::push_swappable].
///
/// These properties allow for quick and easy modification of the
/// [`Text`] within, which can then be accessed with
/// [`text`][Self::text].
pub struct Builder {
    text: Text,
    last_form: Option<Tag>,
    last_align: Option<Tag>,
    marker: Marker,
    buffer: String,
}

impl Builder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn finish(mut self) -> Text {
        let len = self.text.len_bytes();

        if let Some(tag) = self.last_form {
            self.text.tags.insert(len, tag, self.marker);
        }

        if let Some(tag) = self.last_align {
            self.text.tags.insert(len, tag, self.marker);
        }

        self.text
    }

    pub fn push_str(&mut self, display: impl Display) {
        self.buffer.clear();
        write!(self.buffer, "{display}").unwrap();
        self.text.insert_str(self.text.len_bytes(), &self.buffer)
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text`].
    pub fn push_tag(&mut self, tag: Tag) -> Option<ToggleId> {
        let len = self.text.len_bytes();
        let last_inverted = match tag {
            Tag::PushForm(id) => self.last_form.replace(Tag::PopForm(id)),
            Tag::StartAlignLeft => self.last_align.replace(Tag::EndAlignLeft),
            Tag::StartAlignCenter => self.last_align.replace(Tag::EndAlignCenter),
            Tag::StartAlignRight => self.last_align.replace(Tag::EndAlignRight),
            _ => None,
        };

        if let Some(tag) = last_inverted {
            self.text.tags.insert(len, tag, self.marker);
        }

        self.text.tags.insert(len, tag, self.marker)
    }

    pub fn push_text(&mut self, text: Text) {
        let end = self.text.len_bytes();

        if let Some(tag) = self.last_form.take() {
            self.text.tags.insert(end, tag, self.marker);
        }

        self.text.buf.splice(end..end, *text.buf);
        self.text.tags.extend(*text.tags);
    }

    pub fn push_part<D: Display>(&mut self, part: BuilderPart<D>) {
        match part {
            BuilderPart::Text(text) => self.push_text(text),
            BuilderPart::Tag(tag) => {
                self.push_tag(tag);
            }
            BuilderPart::ToString(display) => self.push_str(display),
        }
    }

    pub fn clear(&mut self) {
        self.text.clear();
        self.last_form = None;
    }
}

impl Default for Builder {
    fn default() -> Self {
        Builder {
            text: Text::default(),
            last_form: None,
            last_align: None,
            marker: Marker::base(),
            buffer: String::with_capacity(50),
        }
    }
}

fn cursor_tags(is_main: bool) -> (Tag, Tag, Tag) {
    use palette::{EXTRA_SEL, MAIN_SEL};
    use tags::Tag::{ExtraCursor, MainCursor, PopForm, PushForm};

    if is_main {
        (MainCursor, PushForm(MAIN_SEL), PopForm(MAIN_SEL))
    } else {
        (ExtraCursor, PushForm(EXTRA_SEL), PopForm(EXTRA_SEL))
    }
}

pub fn get_ends(range: impl std::ops::RangeBounds<usize>, max: usize) -> (usize, usize) {
    let start = match range.start_bound() {
        std::ops::Bound::Included(start) => *start,
        std::ops::Bound::Excluded(start) => *start + 1,
        std::ops::Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        std::ops::Bound::Included(end) => *end + 1,
        std::ops::Bound::Excluded(end) => *end,
        std::ops::Bound::Unbounded => max,
    };

    (start, end)
}

pub struct AlignCenter;
pub struct AlignLeft;
pub struct AlignRight;
pub struct Ghost(pub Text);

pub enum BuilderPart<D>
where
    D: Display,
{
    Text(Text),
    Tag(Tag),
    ToString(D),
}

impl From<AlignCenter> for BuilderPart<String> {
    fn from(_: AlignCenter) -> Self {
        BuilderPart::Tag(Tag::StartAlignCenter)
    }
}

impl From<AlignLeft> for BuilderPart<String> {
    fn from(_: AlignLeft) -> Self {
        BuilderPart::Tag(Tag::StartAlignLeft)
    }
}

impl From<AlignRight> for BuilderPart<String> {
    fn from(_: AlignRight) -> Self {
        BuilderPart::Tag(Tag::StartAlignRight)
    }
}

impl From<Ghost> for BuilderPart<String> {
    fn from(value: Ghost) -> Self {
        BuilderPart::Tag(Tag::GhostText(value.0))
    }
}

impl From<Tag> for BuilderPart<String> {
    fn from(value: Tag) -> Self {
        BuilderPart::Tag(value)
    }
}

impl From<Text> for BuilderPart<String> {
    fn from(value: Text) -> Self {
        BuilderPart::Text(value)
    }
}

impl<D> From<&RwData<D>> for BuilderPart<String>
where
    D: Display,
{
    fn from(value: &RwData<D>) -> Self {
        BuilderPart::ToString(value.read().to_string())
    }
}

impl<D> From<&RoData<D>> for BuilderPart<String>
where
    D: Display,
{
    fn from(value: &RoData<D>) -> Self {
        BuilderPart::ToString(value.read().to_string())
    }
}

impl<D> From<D> for BuilderPart<D>
where
    D: Display,
{
    fn from(value: D) -> Self {
        BuilderPart::ToString(value)
    }
}

// pub macro text {
//    ($builder:expr, $($parts:tt)+) => {
//        inner_text!("Default", "Accent", $builder, $($parts)+)
//    },
//    ($($parts:tt)+) => {{
//        inner_text!("Default", "Accent", $($parts)+)
//    }},
//}

pub macro text {
    // Forms
    (@push $builder:expr, []) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("Default")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push $builder:expr, [*a]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("Accent")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    (@push $builder:expr, [$form:ident]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            let name = stringify!($form);
            crate::palette::__weakest_id_of_name(name)
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        let part = BuilderPart::from($part);
        $builder.push_part(part)
    },

    (@parse $builder:expr, $part:tt $($parts:tt)*) => {{
        text!(@push $builder, $part);
        text!(@parse $builder, $($parts)*);
    }},
    (@parse $builder:expr,) => {},

    ($builder:expr, $($parts:tt)+) => {{
        let builder: &mut Builder = &mut $builder;
        text!(@parse builder, $($parts)+);
    }},
    ($($parts:tt)+) => {{
        let mut builder = Builder::new();
        text!(builder, $($parts)+);
        builder.finish()
    }},
}

pub macro ok {
    // Forms
    (@push $builder:expr, []) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("DefaultOk")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push $builder:expr, [*a]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("AccentOk")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    (@push $builder:expr, [$form:ident]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            let name = stringify!($form);
            crate::palette::__weakest_id_of_name(name)
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        let part = BuilderPart::from($part);
        $builder.push_part(part)
    },

    (@parse $builder:expr, $part:tt $($parts:tt)*) => {{
        text!(@push $builder, $part);
        text!(@parse $builder, $($parts)*);
    }},
    (@parse $builder:expr,) => {},

    ($builder:expr, $($parts:tt)+) => {{
        let builder: &mut Builder = &mut $builder;
        text!(@parse builder, $($parts)+);
    }},
    ($($parts:tt)+) => {{
        let mut builder = Builder::new();
        text!(builder, $($parts)+);
        builder.finish()
    }},
}

pub macro err {
    // Forms
    (@push $builder:expr, []) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("DefaultErr")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push $builder:expr, [*a]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("AccentErr")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    (@push $builder:expr, [$form:ident]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            let name = stringify!($form);
            crate::palette::__weakest_id_of_name(name)
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        let part = BuilderPart::from($part);
        $builder.push_part(part)
    },

    (@parse $builder:expr, $part:tt $($parts:tt)*) => {{
        text!(@push $builder, $part);
        text!(@parse $builder, $($parts)*);
    }},
    (@parse $builder:expr,) => {},

    ($builder:expr, $($parts:tt)+) => {{
        let builder: &mut Builder = &mut $builder;
        text!(@parse builder, $($parts)+);
    }},
    ($($parts:tt)+) => {{
        let mut builder = Builder::new();
        text!(builder, $($parts)+);
        builder.finish()
    }},
}

pub macro hint {
    // Forms
    (@push $builder:expr, []) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("DefaultHint")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push $builder:expr, [*a]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("AccentHint")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    (@push $builder:expr, [$form:ident]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            let name = stringify!($form);
            crate::palette::__weakest_id_of_name(name)
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        let part = BuilderPart::from($part);
        $builder.push_part(part)
    },

    (@parse $builder:expr, $part:tt $($parts:tt)*) => {{
        text!(@push $builder, $part);
        text!(@parse $builder, $($parts)*);
    }},
    (@parse $builder:expr,) => {},

    ($builder:expr, $($parts:tt)+) => {{
        let builder: &mut Builder = &mut $builder;
        text!(@parse builder, $($parts)+);
    }},
    ($($parts:tt)+) => {{
        let mut builder = Builder::new();
        text!(builder, $($parts)+);
        builder.finish()
    }},
}

#[doc(hidden)]
pub struct __FormIdLock(LazyLock<FormId>);

impl std::ops::Deref for __FormIdLock {
    type Target = FormId;

    fn deref(&self) -> &FormId {
        self.0.deref()
    }
}

impl __FormIdLock {
    #[doc(hidden)]
    pub const fn new(f: fn() -> FormId) -> Self {
        Self(LazyLock::new(f))
    }
}
