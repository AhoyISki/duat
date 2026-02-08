//! Line ranges to keep track of where lines begin and end.
//!
//! They work similarly to how a rope works, in that there are saved
//! positions which keep track of the amount of bytes, characters, and
//! lines (or bytes and tags) that show up between them and the next
//! record.
//!
//! This struct is used by the [`Text`] and the [`InnerTags`] structs.
//!
//! [`Text`]: super::Text
//! [`InnerTags`]: super::InnerTags
use crate::text::{
    Point,
    shift_list::{Shift, ShiftList, Shiftable},
};

/// The records of a [`Text`] or [`InnerTags`].
///
/// [`Text`]: super::Text
/// [`InnerTags`]: super::InnerTags
#[derive(Default, Clone, Debug)]
pub struct LineRanges(Option<ShiftList<[u32; 2]>>);

impl LineRanges {
    /// Creates a new `LineRanges`.
    pub fn new([s0, s1]: [&str; 2]) -> Self {
        if s0.len() + s1.len() >= 500 {
            Self(Some({
                let [mut b, mut c, mut l] = [0; 3];
                let mut line_ranges = ShiftList::new([0; 2]);
                line_ranges.insert(0, [0; 2]);

                for char in s0.chars().chain(s1.chars()) {
                    b += char.len_utf8() as u32;
                    c += 1;
                    if char == '\n' {
                        l += 1;
                        line_ranges.insert(l as usize, [b, c])
                    }
                }
                line_ranges.max = [b as i32, c as i32];

                line_ranges
            }))
        } else {
            Self(None)
        }
    }

    /// Transforms a range in the `LineRanges`.
    pub fn transform(
        &mut self,
        start: [usize; 3],
        old_len: [usize; 3],
        new_len: [usize; 3],
        [s0, s1]: [&str; 2],
    ) {
        let Some(line_ranges) = self.0.as_mut() else {
            if s0.len() + s1.len() >= 500 {
                *self = Self::new([s0, s1]);
            }
            return;
        };

        line_ranges
            .extract_if_while(start[2] + 1..start[2] + 1 + old_len[2], |_, _| Some(true))
            .for_each(|_| {});

        let s1 = &s1
            [start[0].saturating_sub(s0.len())..(start[0] + new_len[0]).saturating_sub(s0.len())];
        let s0 = &s0[start[0].min(s0.len())..(start[0] + new_len[0]).min(s0.len())];

        line_ranges.shift_by(start[2] + 1, [
            new_len[0] as i32 - old_len[0] as i32,
            new_len[1] as i32 - old_len[1] as i32,
        ]);

        let [mut b, mut c, mut l] = start;
        for char in s0.chars().chain(s1.chars()) {
            b += char.len_utf8();
            c += 1;
            if char == '\n' {
                l += 1;
                line_ranges.insert(l, [b as u32, c as u32])
            }
        }
    }

    /// The maximum [`Point`].
    pub fn max(&self, [s0, s1]: [&str; 2]) -> Point {
        match self.0.as_ref() {
            Some(line_ranges) => {
                let [byte, char] = line_ranges.max();
                Point::from_raw(byte as usize, char as usize, line_ranges.len() - 1)
            }
            None => s0
                .chars()
                .chain(s1.chars())
                .fold(Point::default(), |point, char| point.fwd(char)),
        }
    }

    /// Returns the [`Point`] at a given index, either the character
    /// or the byte indices.
    pub fn point_by_key(
        &self,
        key: usize,
        by: fn([u32; 2]) -> u32,
        [s0, s1]: [&str; 2],
    ) -> Option<Point> {
        let key = key as u32;

        let ([mut b, mut c, mut l], [s0, s1]) = if let Some(lines) = self.0.as_ref() {
            match lines.find_by_key(key, by) {
                Ok(l) => {
                    let [byte, char] = lines.get(l).unwrap();
                    return Some(Point::from_raw(byte as usize, char as usize, l));
                }
                Err(l) => {
                    let prev = l.checked_sub(1).and_then(|prev_i| lines.get(prev_i));
                    let next = lines.get(l);

                    let (prev, next) = match (prev, next) {
                        (None, None) => ([0; 2], lines.max().map(|x| x as u32)),
                        (None, Some(next)) => ([0; 2], next),
                        (Some(prev), None) => (prev, lines.max().map(|x| x as u32)),
                        (Some(prev), Some(next)) => (prev, next),
                    };

                    if key - by(prev) > by(next) - key {
                        let s1 = &s1[..(next[0] as usize).saturating_sub(s0.len())];
                        let s0 = &s0[..(next[0] as usize).min(s0.len())];
                        let [mut b, mut c, mut l] = [next[0] as usize, next[1] as usize, l];
                        return s1.chars().rev().chain(s0.chars().rev()).find_map(|char| {
                            b -= char.len_utf8();
                            c -= 1;
                            l -= (char == '\n') as usize;
                            (by([b as u32, c as u32]) == key).then(|| Point::from_raw(b, c, l))
                        });
                    } else {
                        let s1 = &s1[(prev[0] as usize).saturating_sub(s0.len())..];
                        let s0 = &s0[(prev[0] as usize).min(s0.len())..];
                        ([prev[0], prev[1], l.saturating_sub(1) as u32], [s0, s1])
                    }
                }
            }
        } else {
            ([0; 3], [s0, s1])
        };

        s0.chars().chain(s1.chars()).find_map(|char| {
            if by([b, c]) == key {
                Some(Point::from_raw(b as usize, c as usize, l as usize))
            } else {
                b += char.len_utf8() as u32;
                c += 1;
                l += (char == '\n') as u32;
                None
            }
        })
    }

    /// Gets the [`Point`] at a given line and column.
    ///
    /// This is the most efficient way of getting randomly accessed a
    /// [`Point`].
    #[track_caller]
    pub fn point_at_coords(
        &self,
        line: usize,
        column: usize,
        [s0, s1]: [&str; 2],
    ) -> Option<Point> {
        if let Some(line_ranges) = self.0.as_ref() {
            line_ranges.get(line).and_then(|[mut b, mut c]| {
                let end = line_ranges
                    .get(line + 1)
                    .unwrap_or(line_ranges.max().map(|v| v as u32));

                let s1 = &s1[(b as usize).saturating_sub(s0.len())
                    ..(end[0] as usize).saturating_sub(s0.len())];
                let s0 = &s0[(b as usize).min(s0.len())..(end[0] as usize).min(s0.len())];

                s0.chars()
                    .chain(s1.chars())
                    .map(|char| {
                        let old = [b as usize, c as usize, line];
                        b += char.len_utf8() as u32;
                        c += 1;
                        Point::from_raw(old[0], old[1], old[2])
                    })
                    .nth(column)
            })
        } else {
            let mut col = 0;
            let [mut b, mut c, mut l] = [0; 3];
            s0.chars().chain(s1.chars()).find_map(|char| {
                if l == line && col == column {
                    Some(Point::from_raw(b, c, l))
                } else {
                    l += (char == '\n') as usize;
                    b += char.len_utf8();
                    c += 1;
                    col += 1;
                    col *= (char == '\n') as usize;
                    None
                }
            })
        }
    }
}

impl Shiftable for [u32; 2] {
    type Shift = [i32; 2];

    fn shift(self, by: Self::Shift) -> Self {
        let sh = |i: usize| (self[i] as i32 + by[i]) as u32;
        [sh(0), sh(1)]
    }
}

impl Shift for [i32; 2] {
    fn neg(self) -> Self {
        [-self[0], -self[1]]
    }

    fn add(self, other: Self) -> Self {
        [self[0] + other[0], self[1] + other[1]]
    }
}
