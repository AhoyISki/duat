/// If and how to wrap lines at the end of the screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum WrapMethod {
    Width,
    Capped(usize),
    Word,
    #[default]
    NoWrap
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
            _ => width
        }
    }
}

/// Where the tabs are placed on screen, can be regular or varied.
#[derive(Debug, Clone)]
pub struct TabStops(pub usize);

impl TabStops {
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
#[derive(Default, Debug, Clone, Copy)]
pub enum NewLine {
    /// Show the given character on every new line.
    AlwaysAs(char),
    /// Show the given character only when there is whitespace at end
    /// of the line.
    AfterSpaceAs(char),
    /// Don't print anything for a new line character.
    #[default]
    Hidden
}

impl NewLine {
    pub fn char(&self, last_char: Option<char>) -> Option<char> {
        match self {
            NewLine::AlwaysAs(char) => Some(*char),
            NewLine::AfterSpaceAs(char) => {
                if last_char.is_some_and(|char| char.is_whitespace()) {
                    Some(*char)
                } else {
                    Some(' ')
                }
            }
            NewLine::Hidden => None
        }
    }
}

// Pretty much only exists because i wanted one of these with
// usize as its builtin type.
#[derive(Debug, Copy, Clone)]
pub struct ScrollOff {
    pub y_gap: usize,
    pub x_gap: usize
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
            ![' ', '\t', '\n'].into_iter().any(|char| word_chars.contains(char)),
            "WordChars cannot contain ' ', '\\n' or '\\t'."
        );

        word_chars
    }

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
    pub word_chars: WordChars
}

impl PrintCfg {
    /// Same as [`default`], but with a hidden new line.
    ///
    /// [`default`]: PrintCfg::default
	pub fn default_for_files() -> Self {
        Self {
            wrap_method: WrapMethod::default(),
            indent_wrap: true,
            tab_stops: TabStops(4),
            new_line: NewLine::AlwaysAs(' '),
            scrolloff: ScrollOff::default(),
            word_chars: WordChars::new(vec!['A'..='Z', 'a'..='z', '0'..='9', '_'..='_'])
        }
	}
}

impl Default for PrintCfg {
    fn default() -> Self {
        Self {
            wrap_method: WrapMethod::default(),
            indent_wrap: true,
            tab_stops: TabStops(4),
            new_line: NewLine::default(),
            scrolloff: ScrollOff::default(),
            word_chars: WordChars::new(vec!['A'..='Z', 'a'..='z', '0'..='9', '_'..='_'])
        }
    }
}
