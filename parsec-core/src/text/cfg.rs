use std::ops::RangeInclusive;

/// If and how to wrap lines at the end of the screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum WrapMethod {
    Width,
    Capped(usize),
    Word,
    #[default]
    NoWrap,
}

impl WrapMethod {
    /// Returns `true` if the wrap method is [`NoWrap`].
    ///
    /// [`NoWrap`]: WrapMethod::NoWrap
    #[must_use]
    pub fn is_no_wrap(&self) -> bool {
        matches!(self, Self::NoWrap)
    }

    pub fn wrapping_cap(&self, width: usize) -> usize {
        match self {
            WrapMethod::Capped(cap) => *cap,
            _ => width,
        }
    }
}

/// Where the tabs are placed on screen, can be regular or varied.
#[derive(Debug, Clone, Copy)]
pub struct TabStops(pub usize);

impl TabStops {
    #[inline]
    pub fn spaces_at(&self, x: usize) -> usize {
        self.0 - (x % self.0)
    }
}

impl Default for TabStops {
    fn default() -> Self {
        TabStops(4)
    }
}

/// Wheter to show the new line or not.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum NewLine {
    /// Show the given character on every new line.
    AlwaysAs(char),
    /// Show the given character only when there is whitespace at end
    /// of the line.
    AfterSpaceAs(char),
    /// Don't print anything for a new line character.
    #[default]
    Hidden,
}

impl NewLine {
    #[inline]
    pub fn char(&self, last_char: Option<char>) -> Option<char> {
        match self {
            NewLine::AlwaysAs(char) => Some(*char),
            NewLine::AfterSpaceAs(char) => {
                if last_char.is_some_and(|char| char.is_whitespace() && char != '\n') {
                    Some(*char)
                } else {
                    Some(' ')
                }
            }
            NewLine::Hidden => None,
        }
    }
}

// Pretty much only exists because i wanted one of these with
// usize as its builtin type.
#[derive(Debug, Copy, Clone)]
pub struct ScrollOff {
    pub y_gap: usize,
    pub x_gap: usize,
}

impl Default for ScrollOff {
    fn default() -> Self {
        ScrollOff { y_gap: 3, x_gap: 3 }
    }
}

#[derive(Debug, Clone)]
pub struct WordChars(Vec<std::ops::RangeInclusive<char>>);

impl WordChars {
    pub fn new(ranges: Vec<std::ops::RangeInclusive<char>>) -> Self {
        let word_chars = WordChars(ranges);

        assert!(
            ![' ', '\t', '\n']
                .into_iter()
                .any(|char| word_chars.contains(char)),
            "WordChars cannot contain ' ', '\\n' or '\\t'."
        );

        word_chars
    }

    #[inline]
    pub fn contains(&self, char: char) -> bool {
        self.0.iter().any(|chars| chars.contains(&char))
    }
}

/// Configuration options for printing.
#[derive(Debug, Clone)]
pub struct PrintCfg {
    /// How to wrap the file.
    pub wrap_method: WrapMethod,
    /// Wether to indent wrapped lines or not.
    pub indent_wrap: bool,
    /// Which places are considered a "tab stop".
    pub tab_stops: TabStops,
    /// Wether (and how) to show new lines.
    pub new_line: NewLine,
    /// The horizontal and vertical gaps between the main
    /// cursor and the edges of a [`Label`][crate::ui::Label].
    pub scrolloff: ScrollOff,
    // NOTE: This is relevant for printing with `WrapMethod::Word`.
    /// Characters that are considered to be part of a word.
    pub word_chars: WordChars,
}

impl PrintCfg {
    pub fn new() -> Self {
        Self {
            wrap_method: WrapMethod::default(),
            indent_wrap: true,
            tab_stops: TabStops(4),
            new_line: NewLine::default(),
            scrolloff: ScrollOff::default(),
            word_chars: WordChars::new(vec!['A'..='Z', 'a'..='z', '0'..='9', '_'..='_']),
        }
    }

    pub fn wrapping_with(self, wrap_method: WrapMethod) -> Self {
        Self {
            wrap_method,
            ..self
        }
    }

    pub fn indenting_wrap(self) -> Self {
        Self {
            indent_wrap: true,
            ..self
        }
    }

    pub fn with_tabs_size(self, tab_size: usize) -> Self {
        Self {
            tab_stops: TabStops(tab_size),
            ..self
        }
    }

    pub fn showing_new_line_as(self, new_line: NewLine) -> Self {
        Self { new_line, ..self }
    }

    pub fn with_scrolloff(self, scrolloff: ScrollOff) -> Self {
        Self { scrolloff, ..self }
    }

    pub fn with_word_chars(self, word_chars: impl Iterator<Item = RangeInclusive<char>>) -> Self {
        let word_chars = WordChars::new(word_chars.collect());
        Self { word_chars, ..self }
    }

    /// Same as [`default`], but with a hidden new line.
    ///
    /// [`default`]: PrintCfg::default
    pub(crate) fn default_for_files() -> Self {
        Self {
            wrap_method: WrapMethod::default(),
            indent_wrap: true,
            tab_stops: TabStops(4),
            new_line: NewLine::AlwaysAs(' '),
            scrolloff: ScrollOff::default(),
            word_chars: WordChars::new(vec!['a'..='z', 'A'..='Z', '0'..='9', '_'..='_']),
        }
    }
}

impl Default for PrintCfg {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IterCfg<'a> {
    cfg: &'a PrintCfg,
    iter_lfs: bool,
    force_wrap: Option<WrapMethod>,
    no_indent_wrap: bool,
}

impl<'a> IterCfg<'a> {
    pub fn new(cfg: &'a PrintCfg) -> Self {
        Self {
            cfg,
            iter_lfs: true,
            force_wrap: None,
            no_indent_wrap: false,
        }
    }

    pub fn outsource_lfs(self) -> Self {
        Self {
            iter_lfs: false,
            ..self
        }
    }

    pub fn dont_wrap(self) -> Self {
        Self {
            force_wrap: Some(WrapMethod::NoWrap),
            ..self
        }
    }

    pub fn no_word_wrap(self) -> Self {
        match self.cfg.wrap_method {
            WrapMethod::Word if matches!(self.force_wrap, Some(WrapMethod::NoWrap)) => self,
            WrapMethod::Word => Self {
                force_wrap: Some(WrapMethod::Width),
                ..self
            },
            WrapMethod::Width | WrapMethod::Capped(_) | WrapMethod::NoWrap => self,
        }
    }

    pub fn no_indent_wrap(self) -> Self {
        Self {
            no_indent_wrap: true,
            ..self
        }
    }

    #[inline]
    pub fn show_lf(&self) -> bool {
        self.iter_lfs
    }

    #[inline]
    pub fn wrap_method(&self) -> WrapMethod {
        self.force_wrap.unwrap_or(self.cfg.wrap_method)
    }

    #[inline]
    pub fn indent_wrap(&self) -> bool {
        !self.no_indent_wrap && self.cfg.indent_wrap
    }

    #[inline]
    pub fn tab_stops(&self) -> TabStops {
        self.cfg.tab_stops
    }

    #[inline]
    pub fn new_line(&self) -> NewLine {
        if self.iter_lfs {
            NewLine::Hidden
        } else {
            self.cfg.new_line
        }
    }

    #[inline]
    pub fn scrolloff(&self) -> ScrollOff {
        self.cfg.scrolloff
    }

    #[inline]
    pub fn word_chars(&self) -> &WordChars {
        &self.cfg.word_chars
    }
}
