//! `duatmode` is the default mode for the Duat text editor. It is
//! based on [kakoune] and [helix] keybindings, with some alterations
//! of my own.
//!
//! It goes like this, if helix is meant to be a mixing of kakoune and
//! vim keybindings, then duat is meant to be a mix of kakoune and helix
//! keybindings.
//!
//! I generally agree with the notion that kakoune has you typing too
//! many `alt + shift` keys, which afaik was one of the main reasons
//! helix brought back visual mode.
//!
//! However, I have approached this problem in a different way. Instead
//! of relying on a visual mode, I have simplified some of the keys and
//! made other keys composed, without changing the formula too much.
//!
//! You will still have your occasional `alt + shift`s here and there,
//! but it should be much rarer than it is on kakoune at least. Though
//! the `alt` key still sees moderate use.
//!
//! This plugin is included in Duat by default, as it is considered
//! part of it's identity. Given that, the options aren't set by this
//! plugin's [`Plugin`], but by [`duatmode::opts`], which is exported
//! in duat's [`opts`] module by default
//!
//! Keybindings can be found at the [book].
//!
//! [kakoune]: https://github.com/mawww/kakoune
//! [helix]: https://github.com/helix
//! [`duatmode::opts`]: opts
//! [`Plugin`]: https://docs.rs/crates/duat/latest/struct.Plugin.html
//! [book]: ahoyiski.github.io/duat
use std::{any::TypeId, sync::Mutex};

use crate::normal::Brackets;
pub use crate::{
    insert::{Insert, TabMode, reindent},
    normal::Normal,
};

mod inc_searchers;
mod insert;
pub mod normal;
mod one_key;
mod bindings;

use duat_base::hooks::SearchPerformed;
use duat_core::{
    context::Handle,
    data::Pass,
    form, hook,
    mode::{self, KeyEvent, SelectionMut, alt, event},
    opts::PrintOpts,
    utils::Memoized,
};
use duat_jump_list::JumpList;
use duat_treesitter::{DuatTreeSitter, TsBuffer};
pub use parameter::{add_to_param, duat_param, duat_param_txt, take_param};

mod parameter {
    use std::sync::LazyLock;

    use duat_core::{
        context,
        data::{DataMap, Pass, RwData},
        text::{Text, txt},
    };

    static VALUE: LazyLock<RwData<u32>> = LazyLock::new(RwData::default);

    /// Adds a number to Duat's parameter value
    ///
    /// This should multiply the current value by 10 and add this as
    /// the last digit.
    ///
    /// This parameter is a numerical modifier for actions in Duat's
    /// [`Normal`] mode, and is added to by typing digits in `Normal`
    /// mode.
    ///
    /// [`Normal`]: crate::normal::Normal
    pub fn add_to_param(pa: &mut Pass, num: u32) {
        let value = VALUE.read(pa);
        match value.checked_mul(10).and_then(|v| v.checked_add(num)) {
            Some(new_value) => *VALUE.write(pa) = new_value,
            None => context::warn!("Duat's parameter overflowed"),
        };
    }

    /// Takes the current value, leaving it empty
    ///
    /// The first argument is the parameter, the second one is `true`
    /// if a parameter was set in the first place.
    pub fn take_param(pa: &mut Pass) -> (u32, bool) {
        let param = std::mem::take(VALUE.write(pa));
        (param.max(1), param != 0)
    }

    /// [`StatusLine`] part: The parameter for Duat's modes
    ///
    /// This parameter is a numerical modifier for actions in Duat's
    /// [`Normal`] mode, and is added to by typing digits in `Normal`
    /// mode.
    ///
    /// [`Normal`]: crate::normal::Normal
    /// [`StatusLine`]: duat_base::widgets::StatusLine
    pub fn duat_param() -> DataMap<u32, u32> {
        VALUE.map(|value| *value)
    }

    /// [`StatusLine`] part: Formatted version of [`duat_param`]
    ///
    /// This parameter is a numerical modifier for actions in Duat's
    /// [`Normal`] mode, and is added to by typing digits in `Normal`
    /// mode.
    ///
    /// # Formatting
    ///
    /// ```text
    /// [parameter]param={value}
    /// ```
    ///
    /// [`Normal`]: crate::Normal
    /// [`StatusLine`]: duat_base::widgets::StatusLine
    pub fn duat_param_txt() -> DataMap<u32, Text> {
        VALUE.map(|value| {
            if *value > 0 {
                txt!("[parameter]param={value}")
            } else {
                Text::default()
            }
        })
    }
}

pub mod opts {
    use std::sync::{Mutex, atomic::AtomicBool};

    use duat_core::{
        mode::{self, KeyCode, KeyEvent},
        text::txt,
        utils::Memoized,
    };

    use crate::{Insert, escaped_regex, insert::TabMode, normal::Brackets};

    pub(crate) static INSERT_TABS: AtomicBool = AtomicBool::new(false);
    pub(crate) static DUATMODE_OPTS: Mutex<DuatModeOpts> = Mutex::new(DuatModeOpts::new());

    /// Options for the [`Insert`] and [`Normal`] modes of `duatmode`
    ///
    /// [`Normal`]: super::Normal
    #[derive(Clone, Copy)]
    pub struct DuatModeOpts {
        /// Inserts `\t`s instead of an equivalent amount of spaces
        ///
        /// The default is `false`
        pub insert_tabs: bool,
        /// How tabs should be handled in [`Insert`] mode.
        ///
        /// The default is [`TabMode::VerySmart`], which reindents the
        /// line if necessary, otherwise, triggers autocompletion.
        pub tab_mode: TabMode,
        /// Automatically indent new lines
        ///
        /// This makes it so if you press `<Enter>`, the new line will
        /// keep the same indentation as the old line.
        ///
        /// This is only done in [`Buffer`]s that aren't using
        /// `tree-sitter` or if the `<Enter>` key is not a part of
        /// `indent_keys`.
        ///
        /// The default is `true`
        ///
        /// [`Buffer`]: duat_core::buffer::Buffer
        pub auto_indent: bool,
        /// Characters that, upon being typed, reindent the current
        /// line
        ///
        /// The default is all the bracket characters, `\t` and `\n`
        pub indent_chars: &'static [char],
        /// Enables auto reindentation upon typing `I` in [`Normal`]
        /// mode
        ///
        /// The default is `true`.
        ///
        /// [`Normal`]: super::Normal
        pub indent_on_capital_i: bool,
        /// Makes the `'f'` and `'t'` keys set the search pattern
        ///
        /// If you type `"fm"`, for example, and then type `'n'`,
        /// `'n'` will search for the next instance of an
        /// `'m'` in the [`Buffer`]
        ///
        /// The default is `true`
        ///
        /// [`Buffer`]: duat_core::buffer::Buffer
        pub f_and_t_set_search: bool,
        /// Wether `<a-j>` should remove the indentation of joined
        /// lines.
        ///
        /// The default is `true`
        pub remove_joined_line_indent: bool,
        pub(crate) brackets: Brackets,
    }

    impl DuatModeOpts {
        /// The default version of `DuatModeOpts`
        pub const fn new() -> Self {
            const B_PATS: Brackets = Brackets(&[[r"\(", r"\)"], [r"\{", r"\}"], [r"\[", r"\]"]]);
            Self {
                insert_tabs: false,
                tab_mode: TabMode::VerySmart,
                auto_indent: true,
                indent_chars: &['\n', '(', ')', '{', '}', '[', ']'],
                indent_on_capital_i: true,
                f_and_t_set_search: true,
                remove_joined_line_indent: true,
                brackets: B_PATS,
            }
        }

        /// Changes what is considered a "bracket" in [`Normal`] mode
        ///
        /// More specifically, this will change the behavior of keys
        /// like `'m'` and the `'u'` object, which will now
        /// consider more patterns when selecting.
        ///
        /// [`Normal`]: crate::normal::Normal
        pub fn set_brackets<'a>(&mut self, brackets: impl IntoIterator<Item = [&'a str; 2]>) {
            static BRACKETS: Memoized<Vec<[&str; 2]>, Brackets> = Memoized::new();

            let iter = brackets.into_iter();
            let brackets: Vec<[&str; 2]> = iter.map(|bs| bs.map(escaped_regex)).collect();
            assert!(
                brackets.iter().all(|[s_b, e_b]| s_b != e_b),
                "Brackets are not allowed to look the same"
            );

            self.brackets =
                BRACKETS.get_or_insert_with(&brackets, || Brackets(brackets.clone().leak()));
        }
    }

    impl Default for DuatModeOpts {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Sets options for the [`Insert`] and [`Normal`] modes
    ///
    /// [`Normal`]: super::Normal
    pub fn set(set_fn: impl FnOnce(&mut DuatModeOpts)) {
        let mut opts = *DUATMODE_OPTS.lock().unwrap();
        set_fn(&mut opts);

        mode::change_binding_description::<Insert>(
            &[KeyEvent::from(KeyCode::Tab)],
            match opts.tab_mode {
                TabMode::Normal => txt!("Insert tab"),
                TabMode::Smart => txt!("Reindent or insert tab"),
                TabMode::VerySmart => txt!("Reindent or next completion entry"),
            },
        );

        *DUATMODE_OPTS.lock().unwrap() = opts;
    }

    /// The options currently in effect
    ///
    /// Setting the values of these options will not change the
    /// current value. For that, check out [`opts::set`]
    ///
    /// [`opts::set`]: set
    pub fn get() -> DuatModeOpts {
        *DUATMODE_OPTS.lock().unwrap()
    }
}

/// The [`Plugin`] for `duatmode`
///
/// This [`Plugin`] will change the default mode to `duatmode`'s
/// [`Normal`].
///
/// It also adds a hook to automatically change the forms of the
/// selections when the mode changes. This is the pattern that the
/// forms take:
///
/// - On [`Insert`] mode: `"cursor.main.Insert"`,
///   `"cursor.extra.Insert"`
/// - On [`Normal`] mode: `"cursor.main.Normal"`,
///   `"cursor.extra.Normal"`
///
/// And so on and so forth.
///
/// [`Form`]: duat_core::form::Form
/// [`Plugin`]: https://docs.rs/crates/duat/latest/struct.Plugin.html
#[derive(Default)]
pub struct DuatMode;

impl DuatMode {
    /// Adds the `DuatMode` plugin.
    ///
    /// *DON'T USE THIS DIRECTLY, USE `duat::plug` INSTEAD*.
    #[doc(hidden)]
    #[inline(never)]
    pub fn _plug(self, require: fn(TypeId, fn())) {
        require(TypeId::of::<JumpList>(), || JumpList._plug());
        require(TypeId::of::<DuatTreeSitter>(), || DuatTreeSitter._plug());

        mode::set_alt_is_reverse(true);
        mode::set_default(|_| Normal::new());

        hook::add::<SearchPerformed>(|_, search| *SEARCH.lock().unwrap() = search.to_string());

        normal::jump_list::add_jump_hooks();
        normal::setup_hooks();
        insert::setup_hooks();

        form::enable_mask("Insert", true);
        form::enable_mask("Normal", true);
    }
}

////////// SelectionMut utility functions

fn edit_or_destroy_all(
    pa: &mut Pass,
    handle: &Handle,
    failed_at_least_once: &mut bool,
    mut f: impl FnMut(&mut SelectionMut) -> Option<()> + Clone,
) {
    handle.edit_all(pa, move |mut s| {
        let ret: Option<()> = f(&mut s);
        if ret.is_none() {
            s.reset();
            s.destroy();
            *failed_at_least_once = true;
        }
    })
}

fn select_to_end_of_line(set_anchor: bool, mut s: SelectionMut) {
    set_anchor_if_needed(set_anchor, &mut s);
    s.set_desired_vcol(usize::MAX);
    let pre_nl = match s.char() {
        '\n' => s.text()[..s.cursor()]
            .char_indices()
            .rev()
            .map_while(|(b, char)| (char != '\n').then_some(b))
            .next(),
        _ => s.text()[s.cursor()..]
            .char_indices()
            .map_while(|(b, char)| (char != '\n').then_some(s.cursor().byte() + b))
            .last(),
    };
    if let Some(b) = pre_nl {
        s.move_to(b);
    }
}

fn set_anchor_if_needed(set_anchor: bool, s: &mut SelectionMut) {
    if set_anchor {
        if s.anchor().is_none() {
            s.set_anchor();
        }
    } else {
        s.unset_anchor();
    }
}

////////// Object definitions

#[derive(Clone, Copy, PartialEq, Eq)]
enum SelType {
    BeforeEndOfLine,
    ToEndOfLine,
    Reverse,
    Extend,
    ExtendRev,
    Normal,
}

#[derive(PartialEq, Eq)]
enum Category {
    Word,
    Special,
    Space,
}

impl Category {
    fn of(char: char, opts: PrintOpts) -> Self {
        if opts.is_word_char(char) {
            Category::Word
        } else if [' ', '\t', '\n'].contains(&char) {
            Category::Space
        } else {
            Category::Special
        }
    }
}

/// Regex patterns for searching inside and around objects
#[derive(Clone, Copy, PartialEq, Eq)]
struct Regexes<'o> {
    around: &'o str,
    inside: &'o str,
}

impl<'o> Regexes<'o> {
    fn simple(pat: &'o str) -> Self {
        Self { around: pat, inside: pat }
    }

    fn new(around: &'o str, inside: &'o str) -> Self {
        Self { around, inside }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Object<'o> {
    OneBound(Regexes<'o>),
    TwoBounds {
        ahead: Regexes<'o>,
        behind: Regexes<'o>,
        repeat: bool,
    },
    Argument {
        ahead: Regexes<'o>,
        behind: Regexes<'o>,
    },
    Indent,
}

impl<'o> Object<'o> {
    pub fn two_bounds_simple(behind: &'o str, ahead: &'o str) -> Self {
        Self::TwoBounds {
            ahead: Regexes::simple(ahead),
            behind: Regexes::simple(behind),
            repeat: true,
        }
    }
}

impl<'o> Object<'o> {
    fn new(key_event: KeyEvent, opts: PrintOpts, brackets: Brackets) -> Option<Self> {
        static BRACKET_PATS: Memoized<Brackets, [Regexes; 2]> = Memoized::new();
        let bound_patterns = |brackets: Brackets| {
            BRACKET_PATS.get_or_insert_with(&brackets, || {
                let (s_pat, e_pat): (String, String) = brackets
                    .iter()
                    .enumerate()
                    .map(|(i, [s_b, e_b])| {
                        if i > 0 {
                            (format!("|{}", *s_b), format!("|{}", *e_b))
                        } else {
                            (s_b.to_string(), e_b.to_string())
                        }
                    })
                    .collect();

                [
                    Regexes::simple(s_pat.leak() as &'static _),
                    Regexes::simple(e_pat.leak()),
                ]
            })
        };

        match key_event {
            event!('Q') => Some(Self::OneBound(Regexes::simple("\""))),
            event!('q') => Some(Self::OneBound(Regexes::simple("'"))),
            event!('g') => Some(Self::OneBound(Regexes::simple("`"))),
            event!('|') => Some(Self::OneBound(Regexes::simple(r"\|"))),
            event!('$') => Some(Self::OneBound(Regexes::simple(r"\$"))),
            event!('^') => Some(Self::OneBound(Regexes::simple(r"\^"))),
            event!('s') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r"[\.;!\?]\s*"),
                behind: Regexes::new(r"[^\.;!\?]*", r"\s*"),
                repeat: false,
            }),
            event!('p') => Some(Self::TwoBounds {
                ahead: Regexes::new(r"\n{2,}|\z", r"(^\n)*"),
                behind: Regexes::new(r"\n{2,}|\A", r"(^\n)*"),
                repeat: false,
            }),
            event!('b' | '(' | ')') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r"\)"),
                behind: Regexes::simple(r"\("),
                repeat: true,
            }),
            event!('B' | '{' | '}') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r"\}"),
                behind: Regexes::simple(r"\{"),
                repeat: true,
            }),
            event!('r' | '[' | ']') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r"\]"),
                behind: Regexes::simple(r"\["),
                repeat: true,
            }),
            event!('a' | '<' | '>') => Some(Self::TwoBounds {
                ahead: Regexes::simple(r">"),
                behind: Regexes::simple(r"<"),
                repeat: true,
            }),
            event!('m' | 'M') | alt!('m' | 'M') => Some({
                let [behind, ahead] = bound_patterns(brackets);
                Self::TwoBounds { ahead, behind, repeat: true }
            }),
            event!('u') => Some({
                let [behind, ahead] = bound_patterns(brackets);
                Self::Argument { ahead, behind }
            }),
            event!('w') => Some({
                static WORD_PATS: Memoized<&'static [char], [Regexes; 2]> = Memoized::new();
                let [ahead, behind] = WORD_PATS.get_or_insert_with(&opts.extra_word_chars, || {
                    let cat = opts.word_chars_regex();
                    [
                        Regexes::new(format!("\\A({cat}+|[^{cat} \t\n]+)\\s*").leak(), r"\s*"),
                        Regexes::new(format!("({cat}*|[^{cat} \t\n]*)\\z").leak(), ""),
                    ]
                });

                Self::TwoBounds { ahead, behind, repeat: false }
            }),
            event!('e') => Some(Self::TwoBounds {
                ahead: Regexes::new("\\A[^ \t\n]+\\s*", r"\s*"),
                behind: Regexes::new("[^ \t\n]*\\z", ""),
                repeat: false,
            }),
            event!(' ') => Some(Self::TwoBounds {
                ahead: Regexes::new("\\A[ \t]\n*", "\n*"),
                behind: Regexes::new("\n*[ \t]\\z", "\n*"),
                repeat: false,
            }),
            event!('i') => Some(Self::Indent),
            event!(mode::KeyCode::Char(char)) if !char.is_alphanumeric() => Some(Self::OneBound({
                static BOUNDS: Memoized<char, Regexes> = Memoized::new();
                BOUNDS.get_or_insert_with(&char, || Regexes::simple(char.to_string().leak()))
            })),
            _ => None,
        }
    }

    // Find the forward and backwards matches of this `Object`.
    fn find(self, s: &SelectionMut, count: usize, inside: bool) -> (Option<usize>, Option<usize>) {
        let mut s_diff = count as i32;
        let mut e_diff = count as i32;
        let (behind, ahead) = match self {
            Object::OneBound(one) => (
                s.search(one.around)
                    .to_cursor()
                    .nth_back(count.saturating_sub(1))
                    .zip(Some(one.inside)),
                s.search(one.around)
                    .from_cursor()
                    .nth(count.saturating_sub(1))
                    .zip(Some(one.inside)),
            ),
            Object::TwoBounds { behind, ahead, repeat: false, .. } => (
                s.search(behind.around)
                    .to_cursor()
                    .next_back()
                    .zip(Some(behind.inside)),
                s.search(ahead.around)
                    .from_cursor()
                    .next()
                    .zip(Some(ahead.inside)),
            ),
            Object::TwoBounds { ahead, behind, repeat: true } => {
                let pat = [behind.around, ahead.around];
                let (_, range_ahead) = s
                    .search(pat)
                    .from_cursor()
                    .find(|&(id, _)| {
                        s_diff += (id == 0) as i32 - (id == 1) as i32;
                        s_diff <= 0
                    })
                    .unzip();

                let (_, range_behind) = s
                    .search(pat)
                    .to_cursor()
                    .rev()
                    .find(|&(id, _)| {
                        e_diff += (id == 1) as i32 - (id == 0) as i32;
                        e_diff <= 0
                    })
                    .unzip();

                (
                    range_behind.zip(Some(behind.inside)),
                    range_ahead.zip(Some(ahead.inside)),
                )
            }
            Object::Argument { ahead, behind } => {
                let pat = [r"\s*([;,]\s*|\z)", behind.around, ahead.around];
                let (sid, srange) = s
                    .search(pat)
                    .from_cursor_excl()
                    .find(|(id, _)| {
                        s_diff += (*id == 1) as i32 - (*id == 2) as i32;
                        s_diff == 0 || (e_diff == 1 && *id == 0)
                    })
                    .unwrap();

                let pat = [r"(\A|[;,])\s*", behind.around, ahead.around];
                let (eid, erange) = s
                    .search(pat)
                    .to_cursor_incl()
                    .rev()
                    .find(|(id, _)| {
                        e_diff += (*id == 2) as i32 - (*id == 1) as i32;
                        e_diff == 0 || (e_diff == 1 && *id == 0)
                    })
                    .unwrap();

                let byte_ahead = Some(if inside {
                    if sid == 0 {
                        srange.start
                    } else {
                        s.search(r"\s*")
                            .range(..srange.start)
                            .next_back()
                            .unwrap()
                            .start
                    }
                } else if sid == 0 {
                    srange.end
                } else {
                    srange.start
                });

                let byte_behind = Some(if inside {
                    if eid == 0 {
                        erange.end
                    } else {
                        s.search(r"\s*").range(erange.end..).next().unwrap().end
                    }
                } else if eid == 0 {
                    if sid == 0 {
                        erange.start + 1
                    } else {
                        erange.start
                    }
                } else {
                    erange.end
                });

                return (byte_behind, byte_ahead);
            }
            Object::Indent => {
                let indent = s.indent();
                let mut point = s.text().point_at_coords(s.cursor().line(), 0);

                while s.indent_on(point.line()) >= indent
                    && point.line() < s.text().end_point().line()
                {
                    point = s.text().point_at_coords(point.line() + 1, 0)
                }

                let byte_ahead = Some(if inside {
                    point.byte()
                } else {
                    s.text()[point..]
                        .lines()
                        .find(|line| line.chars().any(|s| !s.is_ascii_whitespace()))
                        .map(|line| line.byte_range().start)
                        .unwrap_or(s.text().len())
                });

                let indent = s.indent();
                let mut point = s.text().point_at_coords(s.cursor().line(), 0);

                while let Some(prev_line) = point.line().checked_sub(1) {
                    let prev = s.text().point_at_coords(prev_line, 0);
                    if s.indent_on(prev.line()) < indent {
                        break;
                    }
                    point = s.text().point_at_coords(point.line() - 1, 0)
                }

                let byte_behind = Some(if inside {
                    point.byte()
                } else {
                    s.text()[..point]
                        .lines()
                        .rev()
                        .take_while(|line| line.chars().all(|s| s.is_ascii_whitespace()))
                        .last()
                        .map(|line| line.byte_range().start)
                        .unwrap_or(point.byte())
                });

                return (byte_behind, byte_ahead);
            }
        };

        (
            behind.and_then(|(range, pat)| {
                Some(if inside {
                    s.search(pat).range(range.start..).next()?.end
                } else {
                    range.start
                })
            }),
            ahead.and_then(|(range, pat)| {
                Some(if inside {
                    s.search(pat).range(..range.end).next_back()?.start
                } else {
                    range.end
                })
            }),
        )
    }
}

////////// General utility functions

fn escaped_regex(str: impl ToString) -> &'static str {
    static ESCAPED: Memoized<String, &str> = Memoized::new();
    ESCAPED.get_or_insert_with(&str.to_string(), || escaped_str(str).leak())
}

fn escaped_str(str: impl ToString) -> String {
    let str = str.to_string();
    let mut escaped = String::new();
    for char in str.chars() {
        if let '(' | ')' | '{' | '}' | '[' | ']' | '$' | '^' | '.' | '*' | '+' | '?' | '|' = char {
            escaped.push('\\');
        }
        escaped.push(char);
    }
    escaped
}

static SEARCH: Mutex<String> = Mutex::new(String::new());

fn indents(pa: &mut Pass, widget: &Handle) -> (std::vec::IntoIter<usize>, bool) {
    fn prev_non_empty_line(s: &mut SelectionMut) -> Option<usize> {
        let line_start = s.text().point_at_coords(s.cursor().line(), 0);

        s.text()[..line_start]
            .lines()
            .rev()
            .enumerate()
            .find(|(_, line)| line.chars().any(|s| !s.is_whitespace()))
            .map(|(i, _)| s.cursor().line() - (i + 1))
    }

    if let Some(buffer) = widget.get_as()
        && let Some(indents) = buffer.ts_get_indentations(pa, ..)
    {
        (indents.into_iter(), true)
    } else {
        let mut indents = Vec::new();
        widget.edit_all(pa, |mut s| {
            indents.push(
                prev_non_empty_line(&mut s)
                    .map(|line| s.indent_on(line))
                    .unwrap_or(0),
            )
        });

        (indents.into_iter(), false)
    }
}
