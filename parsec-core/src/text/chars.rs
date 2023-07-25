use ropey::Rope;

#[derive(Debug)]
pub enum Chars {
    String(String),
    Rope(Rope)
}

impl Chars {
    pub fn replace(
        &mut self, range: impl std::ops::RangeBounds<usize> + Clone, edit: impl AsRef<str>
    ) {
        match self {
            Chars::String(string) => {
                let (start, end) = get_ends(range, string.chars().count());
                let start = string.chars().take(start).map(|ch| ch.len_utf8()).sum::<usize>();
                let end = string.chars().take(end).map(|ch| ch.len_utf8()).sum::<usize>();
                string.replace_range(start..end, edit.as_ref())
            }
            Chars::Rope(rope) => {
                rope.remove(range.clone());
                let (start, _) = get_ends(range, rope.len_chars());
                rope.insert(start, edit.as_ref());
            }
        }
    }

    pub fn iter_at(&self, width: usize) -> Iter {
        match self {
            Chars::String(string) => Iter::String(string.chars().skip(width)),
            Chars::Rope(rope) => Iter::Rope(rope, width, rope.chars_at(width))
        }
    }

    pub fn char_to_byte(&self, char: usize) -> Option<usize> {
        match self {
            Chars::String(string) => string
                .char_indices()
                .map(|(index, _)| index)
                .chain(std::iter::once(string.len()))
                .nth(char),
            Chars::Rope(rope) => rope.try_char_to_byte(char).ok()
        }
    }

    pub fn char_to_line(&self, ch_index: usize) -> Option<usize> {
        match self {
            Chars::String(string) => string
                .split_inclusive('\n')
                .enumerate()
                .scan(0, |accum, (index, line)| {
                    if *accum <= ch_index {
                        *accum += line.chars().count();
                        Some(index)
                    } else {
                        None
                    }
                })
                .last(),
            Chars::Rope(rope) => rope.try_char_to_line(ch_index).ok()
        }
    }

    pub fn line_to_char(&self, line_index: usize) -> Option<usize> {
        match self {
            Chars::String(string) => string
                .split_inclusive('\n')
                .chain(std::iter::once(""))
                .scan(0, |chars, line| {
                    let old_chars = *chars;
                    *chars += line.chars().count();
                    Some(old_chars)
                })
                .nth(line_index),
            Chars::Rope(rope) => rope.try_line_to_char(line_index).ok()
        }
    }

    pub fn len_bytes(&self) -> usize {
        match self {
            Chars::String(string) => string.len(),
            Chars::Rope(rope) => rope.len_bytes()
        }
    }

    pub fn len_chars(&self) -> usize {
        match self {
            Chars::String(string) => string.chars().count(),
            Chars::Rope(rope) => rope.len_chars()
        }
    }

    pub fn len_lines(&self) -> usize {
        match self {
            Chars::String(string) => string.split_inclusive('\n').count(),
            Chars::Rope(rope) => rope.len_lines() - 1
        }
    }

    pub fn clear(&mut self) {
        match self {
            Chars::String(string) => string.clear(),
            Chars::Rope(rope) => *rope = Rope::default()
        }
    }

    pub fn string(&mut self) -> &mut String {
        match self {
            Chars::String(string) => string,
            Chars::Rope(_) => {
                panic!(
                    "Use of string() in a place where `InnerText` is not guaranteed to be \
                     `String`."
                )
            }
        }
    }

    pub(crate) fn get_char(&self, char_index: usize) -> Option<char> {
        match self {
            Chars::String(string) => string.chars().nth(char_index),
            Chars::Rope(rope) => rope.get_char(char_index)
        }
    }
}

#[derive(Clone)]
pub enum Iter<'a> {
    String(std::iter::Skip<std::str::Chars<'a>>),
    Rope(&'a Rope, usize, ropey::iter::Chars<'a>)
}

impl<'a> Iter<'a> {
    pub fn move_forwards_by(&mut self, mut n: usize) -> usize {
        let mut nl_count = 0;
        match self {
            Iter::String(chars) => {
                while n > 0 && let Some(char) = chars.next() {
                    n -= 1;
                    nl_count += (char == '\n') as usize;
                }
                nl_count
            }
            Iter::Rope(rope, char_idx, chars) => {
                let old_line = rope.char_to_line(*char_idx);
                *char_idx += n;
                if let Some(chars_forward) = rope.get_chars_at(*char_idx) {
                    *char_idx += n;
                    *chars = chars_forward;
                    rope.char_to_line(*char_idx) - old_line
                } else {
                    rope.len_lines() - 1 - old_line
                }
            }
        }
    }
}

impl Iterator for Iter<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::String(chars) => chars.next(),
            Iter::Rope(_, width, chars) => chars.next().inspect(|_| *width += 1)
        }
    }
}

pub fn get_ends(range: impl std::ops::RangeBounds<usize>, max: usize) -> (usize, usize) {
    let start = match range.start_bound() {
        std::ops::Bound::Included(start) => *start,
        std::ops::Bound::Excluded(start) => *start + 1,
        std::ops::Bound::Unbounded => 0
    };
    let end = match range.end_bound() {
        std::ops::Bound::Included(end) => *end + 1,
        std::ops::Bound::Excluded(end) => *end,
        std::ops::Bound::Unbounded => max
    };

    (start, end)
}
