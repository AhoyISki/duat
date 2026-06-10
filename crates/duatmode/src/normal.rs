use std::sync::{LazyLock, Mutex};

use duat_base::{
    BaseBuffer,
    modes::{ExtendFwd, ExtendRev, IncSearch, PipeSelections, RunCommands, SearchFwd, SearchRev},
    widgets::Picker,
};
use duat_core::{
    Ns,
    buffer::Buffer,
    context::{self, Handle},
    data::Pass,
    hook::{self, ModeSwitched},
    mode::{self, KeyEvent, Mode, alt, ctrl, event, shift},
    opts::PrintOpts,
    text::Strs,
};
use duat_jump_list::BufferJumps;
pub use fns::*;

use crate::{
    SelType, escaped_regex,
    inc_searchers::{KeepMatching, Select, Split},
    one_key::{OneKey, OneKeyOrResult},
};

pub(crate) fn setup_hooks() {
    hook::add::<ModeSwitched>(|_, _| {
        *SEL_TYPE.lock().unwrap() = SelType::Normal;
        *ONE_KEY.lock().unwrap() = None;
    });
}

/// `duatmode`'s `Normal` mode.
///
/// This is the equivalent of Vim/Kakoune/Helix's `Normal` modes,
/// but with its own quirks.
///
/// One way to think about it is that, if Helix is supposed to
/// be an inbetween of Kakoune and Vim, then Duat is supposed to
/// be an inbetween of Kakoune and Helix, that is, it's closer
/// to Kakoune, but with some inspiration and goals taken from Helix.
///
/// The biggest divergence from Helix here is that there is no
/// visual mode. It was a decision that I didn't really agree with
/// and it was my primary reasoning for not using Helix.
///
/// However, it came in for a valid reason, which was that people
/// didn't want to do so much `alt + shift`, which was par for the
/// course in Kakoune.
///
/// Duat chooses a different way to handle that, while trying not
/// to break the great orthogonality that Kakoune has by not having
/// a visual mode.
///
/// It does this by having more keys do simpler things, and by having
/// more keys be "composite". For example, in Kakoune, `)` shifts the
/// main selection, while `<a-)>` shifts the contents. To prevent the
/// `alt + shift`, Duat uses the composite sequences `)s` and `)c`
/// respectively.
///
/// This compositeness is only used for less common and more niche
/// commands, and not all `alt + shift` instances are completely
/// removed, but you should end up typing `alt + shift` a whole lot
/// less by default.
#[derive(Clone)]
pub struct Normal {
    only_one_action: bool,
}

impl Normal {
    /// Returns an instance of the [`Normal`] mode, inspired by
    /// Kakoune
    pub fn new() -> Self {
        Normal { only_one_action: false }
    }

    /// The same as [`Self::new`], but immediately returns to insert
    /// mode
    pub(crate) fn only_one_action(self) -> Self {
        Self { only_one_action: true }
    }
}

impl Default for Normal {
    fn default() -> Self {
        Self::new()
    }
}

impl Mode for Normal {
    fn bindings() -> mode::Bindings {
        // Extracted to another module due to macro slowdowns in
        // rust-analyzer.
        crate::bindings::normal_bindings()
    }

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent) {
        use mode::KeyCode::*;

        let widget = context::current_widget(pa);

        widget.text_mut(pa).new_moment();

        if let Some(one_key) = { ONE_KEY.lock().unwrap().take() } {
            match one_key.send_key(pa, key_event) {
                OneKeyOrResult::OneKey(one_key) => set_onekey(one_key),
                OneKeyOrResult::Result(sel_type, succeeded) => {
                    *SEL_TYPE.lock().unwrap() = sel_type;
                    if self.only_one_action {
                        mode::set(pa, crate::Insert::new(widget.clone()));
                    }

                    if succeeded {
                        match one_key {
                            OneKey::GoTo(..) | OneKey::Replace => {}
                            _ => *ALT_DOT.lock().unwrap() = Some((one_key, key_event)),
                        }
                    }
                }
            }

            return;
        }

        let jump_id = widget
            .get_as()
            .map(|buffer| buffer.record_jump(pa, *U_ALT_U_ID, false));
        let rec = if jump_id.is_some() { 2 } else { 1 };

        let (param, param_was_set) = if let event!(Char(char)) = key_event
            && let Some(digit) = char.to_digit(10)
        {
            crate::parameter::add_to_param(pa, digit);
            return;
        } else {
            let (param, param_was_set) = crate::parameter::take_param(pa);
            (param as usize, param_was_set)
        };

        match key_event {
            ////////// Basic movement keys
            event!(Char('h') | Left) => fns::move_hor(pa, -(param as i32), false),
            event!(Char('H')) | shift!(Left) => fns::move_hor(pa, -(param as i32), true),
            event!(Char('l') | Right) => fns::move_hor(pa, param as i32, false),
            event!(Char('L')) | shift!(Right) => fns::move_hor(pa, param as i32, true),

            event!('j' | 'J') => fns::move_ver(pa, param as i32, key_event.code == Char('J')),
            event!('k' | 'K') => fns::move_ver(pa, -(param as i32), key_event.code == Char('K')),

            event!(Down) => fns::move_ver_wrapped(pa, param as i32, false),
            shift!(Down) => fns::move_ver_wrapped(pa, param as i32, true),
            event!(Up) => fns::move_ver_wrapped(pa, -(param as i32), false),
            shift!(Up) => fns::move_ver_wrapped(pa, -(param as i32), true),

            alt!(Down) => widget.scroll_ver(pa, param as f32),
            alt!(Up) => widget.scroll_ver(pa, -(param as f32)),

            ////////// Object selection keys
            event!(char @ ('w' | 'e')) => fns::move_words(pa, param as i32, char == 'e', false),
            event!(char @ ('W' | 'E')) => fns::move_words(pa, param as i32, char == 'E', true),
            event!(char @ ('b' | 'v')) => fns::move_words(pa, -(param as i32), char == 'v', false),
            event!(char @ ('B' | 'V')) => fns::move_words(pa, -(param as i32), char == 'V', true),

            event!('x') => fns::select_line(pa),

            event!(char @ ('f' | 'F')) => set_onekey(OneKey::ft(param as i32, false, char == 'F')),
            event!(char @ ('t' | 'T')) => set_onekey(OneKey::ft(param as i32, true, char == 'T')),
            alt!(char @ ('f' | 'F')) => set_onekey(OneKey::ft(-(param as i32), false, char == 'F')),
            alt!(char @ ('t' | 'T')) => set_onekey(OneKey::ft(-(param as i32), true, char == 'T')),

            alt!('h') | event!(Home) => fns::select_to_start_of_line(pa, false),
            alt!('H') | shift!(Home) => fns::select_to_start_of_line(pa, true),
            alt!('l') | event!(End) => fns::select_to_end_of_line(pa, false),
            alt!('L') | shift!(End) => fns::select_to_end_of_line(pa, true),

            event!('%') => widget.edit_main(pa, |mut s| s.move_to(..)),

            event!('[') => set_onekey(OneKey::ToPrevious(param, false, true)),
            event!(']') => set_onekey(OneKey::ToNext(param, false, true)),
            event!('{') => set_onekey(OneKey::ToPrevious(param, false, false)),
            event!('}') => set_onekey(OneKey::ToNext(param, false, false)),
            alt!('[') => set_onekey(OneKey::ToPrevious(param, true, true)),
            alt!(']') => set_onekey(OneKey::ToNext(param, true, true)),
            alt!('{') => set_onekey(OneKey::ToPrevious(param, true, false)),
            alt!('}') => set_onekey(OneKey::ToNext(param, true, false)),

            event!('m') | alt!('m') => set_onekey(OneKey::Match(param, false)),
            event!('M') | alt!('M') => set_onekey(OneKey::Match(param, true)),

            alt!('.') => fns::repeat_selection_sequence(pa),

            ////////// Insertion mode keys
            event!('i') => fns::enter_insert_mode(pa, false),
            event!('I') => fns::enter_insert_mode(pa, true),
            event!('a') => fns::enter_append_mode(pa, false),
            event!('A') => fns::enter_append_mode(pa, true),

            event!('o') => fns::open_new_line_below(pa, true),
            alt!('o') => fns::open_new_line_below(pa, false),
            event!('O') => fns::open_new_line_above(pa, true),
            alt!('O') => fns::open_new_line_above(pa, false),

            event!('.') => fns::repeat_last_insert(pa),

            ////////// Selection alteration keys
            event!('r') => set_onekey(OneKey::Replace),
            event!('`') => fns::lowercase_selections(pa),
            event!('~') => fns::uppercase_selections(pa),
            alt!('`') => fns::swap_selections_case(pa),

            ////////// Advanced selection manipulation
            event!(')') => set_onekey(OneKey::Rotate(param, true)),
            event!('(') => set_onekey(OneKey::Rotate(param, false)),

            alt!(';') => widget.edit_all(pa, |mut s| s.swap_ends()),
            event!(';') => widget.edit_all(pa, |mut s| _ = s.unset_anchor()),
            alt!(':') => widget.edit_all(pa, |mut s| _ = s.set_cursor_on_end()),

            alt!('_') => fns::merge_selections(pa),
            event!('X') => fns::split_selections_by_line(pa),
            event!('D') => fns::divide_selection_on_ends(pa),

            ////////// Line alteration keys
            event!('>') => fns::reindent_selections(pa, param as i32),
            event!('<') => fns::reindent_selections(pa, -(param as i32)),
            alt!('j') => fns::merge_lines_below(pa),

            ////////// Clipboard keys
            event!('y') => fns::yank_selections(pa),
            event!(char @ ('d' | 'c')) => fns::delete_selections(pa, true, char == 'c'),
            alt!(char @ ('d' | 'c')) => fns::delete_selections(pa, false, char == 'c'),
            event!(char @ ('p' | 'P')) => fns::paste_on_selections(pa, param, char == 'P'),
            event!('R') => fns::paste_over_selections(pa, param),

            ////////// SelectionMut creation and destruction
            event!(',') => widget.remove_extra_selections(pa),
            event!('C') => fns::copy_selection_ver(pa, false),
            alt!('C') => fns::copy_selection_ver(pa, true),

            ////////// Search keys
            event!('/') => mode::set(pa, IncSearch::new(SearchFwd, widget.clone())),
            alt!('/') => mode::set(pa, IncSearch::new(SearchRev, widget.clone())),
            event!('?') => mode::set(pa, IncSearch::new(ExtendFwd, widget.clone())),
            alt!('?') => mode::set(pa, IncSearch::new(ExtendRev, widget.clone())),
            event!('s') => mode::set(pa, IncSearch::new(Select, widget.clone())),
            event!('S') => mode::set(pa, IncSearch::new(Split, widget.clone())),
            alt!('k') => mode::set(pa, IncSearch::new(KeepMatching(true), widget.clone())),
            alt!('K') => mode::set(pa, IncSearch::new(KeepMatching(false), widget.clone())),

            event!('n') => fns::next_search_match(pa, param as i32, false),
            alt!('n') => fns::next_search_match(pa, -(param as i32), false),
            event!('N') => fns::next_search_match(pa, param as i32, true),
            alt!('N') => fns::next_search_match(pa, -(param as i32), true),

            event!('*') => fns::set_search_to_main_selection(pa),

            ////////// Jumping
            alt!('u') => fns::move_on_selection_changes(pa, -(rec as i32)),
            alt!('U') => fns::move_on_selection_changes(pa, rec as i32),

            ////////// Macro keys
            alt!('q') => fns::play_macro(pa),
            alt!('Q') => fns::record_macro(pa),

            ////////// Jumping around
            ctrl!('o') => fns::move_on_jump_list(pa, -(param as i32)),
            ctrl!('i') | event!(Tab) => fns::move_on_jump_list(pa, param as i32),
            ctrl!('j') => fns::save_on_jump_list(pa),

            event!(Tab) if Picker::is_open(pa) => {
                if Picker::is_on_preview(pa) {
                    Picker::unfocus_preview(pa);
                } else if widget.widget().is::<Picker>() {
                    Picker::focus_preview(pa);
                }
            }
            event!(Enter) if widget.widget().is::<Picker>() => Picker::select_current(pa),

            ////////// Other mode changing keys
            event!(':') => mode::set(pa, RunCommands::new()),
            event!('|') => mode::set(pa, PipeSelections::new()),

            event!('g') if param_was_set => fns::go_to_line(pa, param - 1, false),
            event!('G') if param_was_set => fns::go_to_line(pa, param - 1, true),
            event!('g') => set_onekey(OneKey::GoTo(SelType::Normal)),
            event!('G') => set_onekey(OneKey::GoTo(SelType::Extend)),

            event!(' ') => mode::set(pa, mode::User),
            event!(Esc) if !widget.widget().is::<Buffer>() => mode::reset::<Buffer>(pa),

            ////////// History manipulation
            event!('u') => widget.text_mut(pa).undo(),
            event!('U') => widget.text_mut(pa).redo(),
            ctrl!('r') => _ = duat_core::cmd::call_notify(pa, "reload"),

            ////////// Snippets
            ctrl!('l') if let Some(buffer) = widget.get_as() => _ = buffer.jump_snippets(pa, 1),
            ctrl!('h') if let Some(buffer) = widget.get_as() => _ = buffer.jump_snippets(pa, -1),
            _ => {}
        }

        if ONE_KEY.lock().unwrap().is_none() && self.only_one_action {
            mode::set(pa, crate::Insert::new(widget.clone()));
        }
    }
}

pub mod fns {
    use std::sync::atomic::Ordering::Relaxed;

    use duat_core::{
        buffer::Buffer,
        context,
        data::Pass,
        hook::{self, KeyTyped},
        mode::{self, KeyCode, KeyEvent, VPoint, event},
    };
    use duat_filetype::{AutoPrefix, FileType};
    use duat_jump_list::BufferJumps;

    use crate::{
        Insert, Normal, SEARCH, SelType, escaped_str,
        normal::{
            ALT_DOT, InsertKey, LAST_INSERT_KEY, MACRO, MACRO_NS, SEL_TYPE, U_ALT_U_ID,
            current_parts, no_nl_pair, word_and_space,
        },
        one_key::{OneKey, OneKeyOrResult},
        opts::INSERT_TABS,
        reindent, set_anchor_if_needed,
    };

    /// [`Normal`] command: Move all selections horizontally.
    ///
    /// `count` represents the amount of characters to move. Negative
    /// numbers move to the left, positive move to the right.
    ///
    /// If `extend_selections` is set to `true`, will extend
    /// selections instead of just moving them.
    ///
    /// # Key equivalents:
    ///
    /// - `h`, `Left`, `l`, `Right` for moving.
    /// - `H`, `<s-Left>`, `L`, `<s-Right>` for extending.
    ///
    /// [`Normal`]: super::Normal
    pub fn move_hor(pa: &mut Pass, count: i32, extend_selections: bool) {
        let (widget, _) = current_parts(pa);
        widget.edit_all(pa, |mut s| {
            set_anchor_if_needed(extend_selections, &mut s);
            s.move_hor(count);
        });
        *SEL_TYPE.lock().unwrap() = SelType::Normal;
    }

    /// [`Normal`] command: Move all selections vertically.
    ///
    /// `count` represents the amount of lines to move. Negative
    /// numbers move up, positive move down.
    ///
    /// If `extend_selections` is set to `true`, will extend
    /// selections instead of just moving them.
    ///
    /// # Key equivalents:
    ///
    /// - `j`, `k` for moving.
    /// - `J`, `K` for extending.
    ///
    /// [`Normal`]: super::Normal
    pub fn move_ver(pa: &mut Pass, count: i32, extend_selections: bool) {
        let (widget, _) = current_parts(pa);
        let sel_type = *SEL_TYPE.lock().unwrap();
        widget.edit_all(pa, |mut s| {
            set_anchor_if_needed(extend_selections, &mut s);

            if !s.move_ver(count) {
                s.reset();
                return;
            }

            let v_cursor = s.v_cursor();
            if s.char() == '\n' && v_cursor.char_col() > 0 && sel_type != SelType::ToEndOfLine {
                s.move_hor(-1);
                s.set_desired_vcol(if sel_type == SelType::BeforeEndOfLine {
                    usize::MAX
                } else {
                    v_cursor.desired_visual_col()
                });
            }
        })
    }

    /// [`Normal`] command: Move all selections vertically wrapping.
    ///
    /// Wrapped movement doesnt' move whole lines, but instead goes up
    /// or down depending on how the lines are wrapped.
    ///
    /// `count` represents the amount of lines to move. Negative
    /// numbers move up, positive move down.
    ///
    /// If `extend_selections` is set to `true`, will extend
    /// selections instead of just moving them.
    ///
    /// # Key equivalents:
    ///
    /// - `Down`, `Up` for moving.
    /// - `<s-Down>`, `<s-Up>` for extending.
    ///
    /// [`Normal`]: super::Normal
    pub fn move_ver_wrapped(pa: &mut Pass, count: i32, extend_selections: bool) {
        let (widget, _) = current_parts(pa);
        widget.edit_all(pa, |mut s| {
            set_anchor_if_needed(extend_selections, &mut s);
            s.move_ver_wrapped(count);
        })
    }

    /// [`Normal`] command: Move all selections count word.
    ///
    /// `count` represents the amount of words to move. Negative
    /// numbers move left, positive move right.
    ///
    /// If `extend_selections` is set to `true`, will extend
    /// selections instead of just moving them.
    ///
    /// # Key equivalents:
    ///
    /// - `w`, `e`, `b`, `v` for moving.
    /// - `W`, `E`, `B`, `V` for extending.
    ///
    /// [`Normal`]: super::Normal
    pub fn move_words(pa: &mut Pass, count: i32, alt: bool, extend_selections: bool) {
        let (widget, popts) = current_parts(pa);

        let match_on_spot = |[c0, c1]: [_; 2], alt_word, moved| {
            use crate::Category::{self, *};
            let (cat0, cat1) = (Category::of(c0, popts), Category::of(c1, popts));
            !matches!(
                (cat0, cat1, alt_word, moved),
                (Word, Word, ..)
                    | (Special, Special, ..)
                    | (Space, Space, ..)
                    | (Word | Special, Word | Special, true, _)
                    | (.., true)
            )
        };

        let abs = count.unsigned_abs() as usize;
        if !extend_selections {
            if count > 0 {
                widget.edit_all(pa, |mut s| {
                    let iter = s.text()[s.cursor()..]
                        .char_indices()
                        .map(|(b, char)| (b + s.cursor().byte(), char));
                    if let Some([(b0, c0), (b1, c1)]) = no_nl_pair(iter) {
                        let move_to_match = match_on_spot([c0, c1], alt, b0 != s.cursor().byte());
                        s.move_to(if move_to_match { b1 } else { b0 });

                        let range = s
                            .search(word_and_space(alt, popts))
                            .from_cursor()
                            .nth(abs - 1);
                        if let Some(range) = range {
                            s.move_to(range);
                        }
                    };
                })
            } else if count < 0 {
                widget.edit_all(pa, |mut s| {
                    let init = {
                        let iter = [(s.cursor().byte(), s.char())]
                            .into_iter()
                            .chain(s.text()[..s.cursor()].char_indices().rev());
                        no_nl_pair(iter)
                    };
                    if let Some([(b1, c1), (_, c0)]) = init {
                        let moved = b1 != s.cursor().byte();
                        s.move_to(b1);
                        if !match_on_spot([c1, c0], alt, moved) {
                            s.move_hor(1);
                        }

                        let range = s
                            .search(word_and_space(alt, popts))
                            .to_cursor()
                            .nth_back(abs - 1);
                        if let Some(range) = range {
                            s.move_to(range);
                            s.set_cursor_on_start();
                        };
                    };
                })
            }
        } else {
            if count > 0 {
                widget.edit_all(pa, |mut s| {
                    set_anchor_if_needed(true, &mut s);
                    s.move_hor(1);
                    if let Some(range) = {
                        s.search(word_and_space(alt, popts))
                            .from_cursor()
                            .nth(abs - 1)
                    } {
                        s.move_to(range.end);
                        s.move_hor(-1);
                    }
                })
            } else if count < 0 {
                widget.edit_all(pa, |mut s| {
                    set_anchor_if_needed(true, &mut s);
                    if let Some(range) = {
                        s.search(word_and_space(alt, popts))
                            .to_cursor()
                            .nth_back(abs - 1)
                    } {
                        s.move_to(range.start);
                    }
                })
            }
        }
    }

    /// [`Normal`] command: Extend selections to whole lines.
    ///
    /// # Key equivalents:
    ///
    /// - `x`
    ///
    /// [`Normal`]: super::Normal
    pub fn select_line(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);
        widget.edit_all(pa, |mut s| {
            *SEL_TYPE.lock().unwrap() = SelType::ToEndOfLine;
            set_anchor_if_needed(true, &mut s);
            s.set_cursor_on_start();
            let b0 = s.search("\n").to_cursor().next_back().map(|r| r.end);
            s.move_to(b0.unwrap_or_default());
            s.swap_ends();

            let b1 = s.search("\n").from_cursor().next().map(|r| r.start);
            s.move_to(b1.unwrap_or(s.text().len()));
            s.set_desired_vcol(usize::MAX);
        })
    }

    /// [`Normal`] command: Find next/previous instance of `char`.
    ///
    /// # Key equivalents:
    ///
    /// - `f`, `t` for forward selection.
    /// - `<a-f>`, `<a-t>` for backwards selection.
    /// - `F`, `T` for forward extension.
    /// - `<a-F>`, `<a-T>` for backwards extension.
    ///
    /// [`Normal`]: super::Normal
    pub fn find_char(pa: &mut Pass, char: char, count: i32, until: bool, extend_selections: bool) {
        OneKey::ft(count, until, extend_selections)
            .send_key(pa, KeyEvent::from(KeyCode::Char(char)));
    }

    /// [`Normal`] command: Repeat the selection sequence.
    ///
    /// Selection sequences include things like the `f`, `t` and `m`
    /// keys.
    ///
    /// # Key equivalents:
    ///
    /// - `<a-.>`
    ///
    /// [`Normal`]: super::Normal
    pub fn repeat_selection_sequence(pa: &mut Pass) {
        if let Some((one_key, key_event)) = *ALT_DOT.lock().unwrap() {
            let OneKeyOrResult::Result(sel_type, _) = one_key.send_key(pa, key_event) else {
                unreachable!();
            };
            *SEL_TYPE.lock().unwrap() = sel_type;
        } else {
            context::warn!("No previous 2 key sequence");
        }
    }

    /// [`Normal`] command: Select until end of line
    ///
    /// If `extend_selections` is set to `true`, will extend
    /// selections instead of just moving them.
    ///
    /// # Key equivalents:
    ///
    /// - `<a-l>`, `End` to select.
    /// - `<a-L>`, `<s-End>` to extend.
    ///
    /// [`Normal`]: super::Normal
    pub fn select_to_end_of_line(pa: &mut Pass, extend_selections: bool) {
        let (widget, _) = current_parts(pa);
        widget.edit_all(pa, |mut s| {
            if !extend_selections {
                s.unset_anchor();
            }
            crate::select_to_end_of_line(true, s);
            *SEL_TYPE.lock().unwrap() = SelType::BeforeEndOfLine;
        })
    }

    /// [`Normal`] command: Select until start of line.
    ///
    /// If `extend_selections` is set to `true`, will extend
    /// selections instead of just moving them.
    ///
    /// # Key equivalents:
    ///
    /// - `<a-h>`, `Home` to select.
    /// - `<a-H>`, `<s-Home>` to extend.
    ///
    /// [`Normal`]: super::Normal
    pub fn select_to_start_of_line(pa: &mut Pass, extend_selections: bool) {
        let (widget, _) = current_parts(pa);
        widget.edit_all(pa, |mut s| {
            if !extend_selections {
                s.unset_anchor();
            }
            set_anchor_if_needed(true, &mut s);
            s.move_hor(-(s.v_cursor().char_col() as i32));
        })
    }

    /// [`Normal`] command: Select a pair of brackets.
    ///
    /// This key works similarly to `mm` on Helix or `m` on Kakoune.
    /// However, one major difference from those is that this one will
    /// keep moving forwards/backwards, letting you quickly jump
    /// pairs.
    ///
    /// # Key equivalents:
    ///
    /// - `ml`, `mh` to select ahead/behind.
    /// - `Ml`, `Mh` to extend ahead/behind.
    ///
    /// [`Normal`]: super::Normal
    pub fn select_matching_pair(pa: &mut Pass, count: i32, extend_selections: bool) {
        if count > 0 {
            OneKey::Match(count.unsigned_abs() as usize, extend_selections)
                .send_key(pa, KeyEvent::from(KeyCode::Char('l')));
        } else if count < 0 {
            OneKey::Match(count.unsigned_abs() as usize, extend_selections)
                .send_key(pa, KeyEvent::from(KeyCode::Char('h')));
        }
    }

    /// [`Normal`] command: Enter [`Insert`] mode.
    ///
    /// # Key equivalents:
    ///
    /// - `i`, `I`.
    ///
    /// [`Normal`]: super::Normal
    pub fn enter_insert_mode(pa: &mut Pass, at_start_of_line: bool) {
        let opts = crate::opts::get();
        let (widget, _) = current_parts(pa);
        if !at_start_of_line {
            widget.edit_all(pa, |mut s| _ = s.set_cursor_on_start());
            *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::Insert);
            mode::set(pa, Insert::new(widget.clone()));
        } else {
            if opts.indent_on_capital_i {
                let (mut indents, is_ts_indent) = crate::indents(pa, &widget);
                if is_ts_indent {
                    widget.edit_all(pa, |mut s| {
                        reindent(s.indent(), indents.next().unwrap(), &mut s);
                    });
                }
            }

            widget.edit_all(pa, |mut s| {
                s.unset_anchor();
                s.move_to_col(s.indent());
            });

            *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::InsertStart);
            mode::set(pa, Insert::new(widget.clone()));
        }
    }

    /// [`Normal`] command: Enter [`Insert`] mode by appending.
    ///
    /// # Key equivalents:
    ///
    /// - `a`, `A`.
    ///
    /// [`Normal`]: super::Normal
    pub fn enter_append_mode(pa: &mut Pass, at_end_of_line: bool) {
        let (widget, _) = current_parts(pa);
        if !at_end_of_line {
            widget.edit_all(pa, |mut s| {
                s.set_cursor_on_end();
                s.move_hor(1);
            });
            *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::Append);
            mode::set(pa, crate::Insert::new(widget.clone()));
        } else {
            widget.edit_all(pa, |mut s| {
                s.unset_anchor();
                s.move_to_col(usize::MAX);
            });
            *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::AppendEnd);
            mode::set(pa, crate::Insert::new(widget.clone()));
        }
    }

    /// [`Normal`] command: Open a new line below the selections.
    ///
    /// # Key equivalents:
    ///
    /// - `o`, `<a-o>`.
    ///
    /// [`Normal`]: super::Normal
    pub fn open_new_line_below(pa: &mut Pass, enter_insert_mode: bool) {
        // TODO: Implement parameter
        let (widget, _) = current_parts(pa);

        widget.edit_all(pa, |mut s| {
            s.set_cursor_on_end();
            let cursor = s.cursor();
            s.move_to_col(usize::MAX);
            s.insert("\n");
            if enter_insert_mode {
                s.move_hor(1);
            } else {
                s.move_to(cursor);
            }
        });

        if enter_insert_mode {
            if let Some(buffer) = widget.get_as::<Buffer>()
                && let Some(filetype) = buffer.read(pa).filetype()
            {
                buffer.edit_all(pa, |mut s| {
                    if s.add_comment(filetype) {
                        s.insert(' ');
                        s.move_hor(1);
                    }
                });
            }
            let (mut indents, is_ts_indent) = crate::indents(pa, &widget);
            if is_ts_indent {
                widget.edit_all(pa, |mut s| _ = reindent(0, indents.next().unwrap(), &mut s));
            }

            *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::NewLineBelow);
            mode::set(pa, crate::Insert::new(widget.clone()));
        }
    }

    /// [`Normal`] command: Open a new line above the selections.
    ///
    /// # Key equivalents:
    ///
    /// - `O`, `<a-O>`.
    ///
    /// [`Normal`]: super::Normal
    pub fn open_new_line_above(pa: &mut Pass, enter_insert_mode: bool) {
        // TODO: Implement parameter
        let (widget, _) = current_parts(pa);

        widget.edit_all(pa, |mut s| {
            s.set_cursor_on_start();
            let char_col = s.v_cursor().char_col();
            s.move_to_col(0);
            s.insert("\n");
            if !enter_insert_mode {
                s.move_hor(char_col as i32 + 1);
            }
        });

        if enter_insert_mode {
            if let Some(buffer) = widget.get_as::<Buffer>()
                && let Some(filetype) = buffer.read(pa).filetype()
            {
                buffer.edit_all(pa, |mut s| {
                    if s.add_comment(filetype) {
                        s.insert(' ');
                        s.move_hor(1);
                    }
                });
            }
            let (mut indents, is_ts_indent) = crate::indents(pa, &widget);
            if is_ts_indent {
                widget.edit_all(pa, |mut s| _ = reindent(0, indents.next().unwrap(), &mut s));
            }

            *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::NewLineAbove);
            mode::set(pa, crate::Insert::new(widget.clone()));
        }
    }

    /// [`Normal`] command: Repeats the last [`Insert`] mode sequence.
    ///
    /// # Key equivalents:
    ///
    /// - `.`.
    ///
    /// [`Normal`]: super::Normal
    pub fn repeat_last_insert(pa: &mut Pass) {
        let opts = crate::opts::get();
        let (widget, _) = current_parts(pa);

        match *LAST_INSERT_KEY.lock().unwrap() {
            Some(InsertKey::Insert) => widget.edit_all(pa, |mut s| _ = s.set_cursor_on_start()),
            Some(InsertKey::Append) => widget.edit_all(pa, |mut s| {
                s.set_cursor_on_end();
                s.move_hor(1);
            }),
            Some(InsertKey::Change) => delete_selections(pa, false, false),
            Some(InsertKey::InsertStart) => {
                widget.edit_all(pa, |mut s| {
                    s.unset_anchor();
                    s.move_to_col(s.indent());
                });

                if opts.indent_on_capital_i {
                    let (mut indents, is_ts_indent) = crate::indents(pa, &widget);
                    if is_ts_indent {
                        widget.edit_all(pa, |mut s| {
                            reindent(s.indent(), indents.next().unwrap(), &mut s);
                        })
                    }
                }
            }
            Some(InsertKey::AppendEnd) => widget.edit_all(pa, |mut s| {
                s.unset_anchor();
                s.move_to_col(usize::MAX);
            }),
            Some(InsertKey::NewLineAbove) => open_new_line_above(pa, false),
            Some(InsertKey::NewLineBelow) => open_new_line_below(pa, false),
            None => context::warn!("No previous insertion"),
        }

        crate::insert::repeat_last_insert(pa, &widget)
    }

    /// [`Normal`] command: Lowercases the selections.
    ///
    /// # Key equivalents:
    ///
    /// - `` ` ``.
    ///
    /// [`Normal`]: super::Normal
    pub fn lowercase_selections(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);

        widget.edit_all(pa, |mut s| {
            let lower = s.selection().chars().flat_map(char::to_lowercase);
            s.replace(lower.collect::<String>());
        })
    }

    /// [`Normal`] command: Uppercases the selections.
    ///
    /// # Key equivalents:
    ///
    /// - `~`.
    ///
    /// [`Normal`]: super::Normal
    pub fn uppercase_selections(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);

        widget.edit_all(pa, |mut s| {
            let upper = s.selection().chars().flat_map(char::to_uppercase);
            s.replace(upper.collect::<String>());
        })
    }

    /// [`Normal`] command: Swaps the case of the selections.
    ///
    /// # Key equivalents:
    ///
    /// - `` <a-`> ``.
    ///
    /// [`Normal`]: super::Normal
    pub fn swap_selections_case(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);

        widget.edit_all(pa, |mut s| {
            let inverted = s.selection().chars().map(|s| {
                if s.is_uppercase() {
                    s.to_lowercase().collect::<String>()
                } else {
                    s.to_uppercase().collect()
                }
            });
            s.replace(inverted.collect::<String>());
        })
    }

    /// [`Normal`] command: Merges adjacent selections.
    ///
    /// # Key equivalents:
    ///
    /// - `_`.
    ///
    /// [`Normal`]: super::Normal
    pub fn merge_selections(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);

        widget.edit_all(pa, |mut s| {
            s.set_cursor_on_end();
            s.move_hor(1);
        });
        // In the first iteration, connected SelectionMuts are joined, this
        // one just undoes the movement.
        widget.edit_all(pa, |mut s| {
            s.move_hor(-1);
        });
    }

    /// [`Normal`] command: Splits each selection by its lines.
    ///
    /// # Key equivalents:
    ///
    /// - `X`.
    ///
    /// [`Normal`]: super::Normal
    pub fn split_selections_by_line(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);

        widget.edit_all(pa, |mut s| {
            s.set_cursor_on_start();
            let Some(end) = s.anchor() else {
                return;
            };
            let cursor = s.cursor();
            let lines: Vec<_> = s.search("[^\n]*\n").range(cursor..end).collect();
            let mut last_end = s.cursor().byte();
            for range in lines {
                let mut e_copy = s.copy();
                e_copy.move_to(range.start);
                e_copy.set_anchor();
                e_copy.move_to(range.end);
                e_copy.move_hor(-1);

                last_end = range.end;
            }
            s.move_to(last_end);
            s.swap_ends();
        })
    }

    /// [`Normal`] command: Divide each selection into two ends.
    ///
    /// # Key equivalents:
    ///
    /// - `D`.
    ///
    /// [`Normal`]: super::Normal
    pub fn divide_selection_on_ends(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);

        widget.edit_all(pa, |mut s| {
            if s.anchor().is_some() {
                let mut e_copy = s.copy();
                e_copy.swap_ends();
                e_copy.unset_anchor();
                s.unset_anchor();
            }
        })
    }

    /// [`Normal`] command: Reindent lines in all selections.
    ///
    /// `count` represents the amount of indents to add/remove.
    /// Negative numbers dedent, positive indent.
    ///
    /// If `extend_selections` is set to `true`, will extend
    /// selections instead of just moving them.
    ///
    /// # Key equivalents:
    ///
    /// - `<`, `>`.
    ///
    /// [`Normal`]: super::Normal
    pub fn reindent_selections(pa: &mut Pass, count: i32) {
        let (widget, _) = current_parts(pa);
        let abs = count.unsigned_abs() as usize;

        if count > 0 {
            let mut processed_lines = Vec::new();
            widget.edit_all(pa, |mut s| {
                let range = s.range_excl();
                let old_cursor = s.v_cursor();
                let old_anchor = s.v_anchor();
                s.unset_anchor();

                let insert = if INSERT_TABS.load(Relaxed) {
                    "\t".repeat(abs)
                } else {
                    " ".repeat(s.opts().tabstop_spaces_at(0) as usize * abs)
                };

                for line in range.start.line()..=range.end.line() {
                    if processed_lines.contains(&line) {
                        continue;
                    }
                    s.move_to_coords(line, 0);
                    s.insert(&insert);
                }

                let cols = insert.chars().count();
                if let Some(old_anchor) = old_anchor {
                    s.move_to_coords(old_anchor.line(), old_anchor.char_col() + cols);
                    s.set_anchor();
                }
                s.move_to_coords(old_cursor.line(), old_cursor.char_col() + cols);

                processed_lines.extend(range.start.line()..=range.end.line());
            });
        } else if count < 0 {
            let mut processed_lines = Vec::new();
            widget.edit_all(pa, |mut s| {
                let range = s.range_excl();

                let mut cursor = (s.cursor().line(), s.v_cursor().char_col());
                let mut anchor = s.v_anchor().map(|vp| (vp.line(), vp.char_col()));

                s.unset_anchor();

                let find = format!(
                    "^(\t{{1,{}}}| {{1,{}}})",
                    abs,
                    s.opts().tabstop_spaces_at(0) as usize * abs
                );

                for line in (range.start.line()..=range.end.line()).rev() {
                    if processed_lines.contains(&line) {
                        continue;
                    }
                    let s_range = s.text().line(line).byte_range();
                    s.move_to(s_range.start);
                    let range = s.cursor().byte()..s_range.end;
                    let Some(s_range) = s.search(&find).range(range).next() else {
                        continue;
                    };

                    s.move_to(s_range.clone());
                    s.replace("");
                    if line == cursor.0 {
                        cursor.1 = cursor.1.saturating_sub(s_range.end - s_range.start);
                    }
                    if let Some(anchor) = &mut anchor
                        && line == anchor.0
                    {
                        anchor.1 = anchor.1.saturating_sub(s_range.end - s_range.start);
                    }
                }

                if let Some((line, col)) = anchor {
                    s.move_to_coords(line, col);
                    s.set_anchor();
                } else {
                    s.unset_anchor();
                }
                s.move_to_coords(cursor.0, cursor.1);

                processed_lines.extend(range.start.line()..=range.end.line());
            });
        }
    }

    /// [`Normal`] command: Merge lines below the selections.
    ///
    /// # Key equivalents:
    ///
    /// - `<a-j>`.
    ///
    /// [`Normal`]: super::Normal
    pub fn merge_lines_below(pa: &mut Pass) {
        let opts = crate::opts::get();
        let (widget, _) = current_parts(pa);

        let mut processed_lines = Vec::new();
        let rm_str = if opts.remove_joined_line_indent {
            "\n\\s*"
        } else {
            "\n"
        };

        widget.edit_all(pa, |mut s| {
            let c_range = s.range();

            if c_range.start.line() == c_range.end.line() {
                if !processed_lines.contains(&c_range.start.line())
                    && let Some(range) = { s.search(rm_str).from_cursor().next() }
                {
                    s.move_to(range);
                    s.replace(" ");
                    s.reset();
                    processed_lines.push(c_range.start.line());
                }
            } else {
                let cursor_was_on_end = s.set_cursor_on_start();
                let range = s.cursor()..c_range.end;
                let nls: Vec<_> = s.search(rm_str).range(range).collect();

                let mut lines_joined = 0;
                for (line, range) in nls.into_iter().rev().enumerate() {
                    if processed_lines.contains(&(line + c_range.start.line())) {
                        continue;
                    }

                    s.move_to(range);
                    s.replace(" ");
                    lines_joined += 1;
                    processed_lines.push(c_range.start.line());
                }

                s.unset_anchor();
                s.move_to(c_range.start);
                s.set_anchor();
                s.move_hor((c_range.end.char() - (c_range.start.char() + lines_joined)) as i32);
                if !cursor_was_on_end {
                    s.swap_ends();
                }
            }
        });
    }

    /// [`Normal`] command: Yanks the selections.
    ///
    /// # Key equivalents:
    ///
    /// - `y`.
    ///
    /// [`Normal`]: super::Normal
    pub fn yank_selections(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);

        {
            duat_base::modes::copy_selections(pa, &widget);
            if widget.selections(pa).len() > 1 {
                context::info!("Yanked [a]{}[] selections", widget.selections(pa).len());
            } else {
                context::info!("Yanked [a]1[] selection");
            }
        }
    }

    /// [`Normal`] command: Deletes/changes the selectiosn.
    ///
    /// # Key equivalents:
    ///
    /// - `d`, `c' will yank.
    /// - `<a-d>`, `<a-c>' won't yank.
    ///
    /// [`Normal`]: super::Normal
    pub fn delete_selections(pa: &mut Pass, yank: bool, enter_insert_mode: bool) {
        let (widget, _) = current_parts(pa);

        if yank {
            duat_base::modes::copy_selections(pa, &widget);
        }

        widget.edit_all(pa, |mut s| {
            let prev_char = s.text()[..s.cursor()].chars().next_back();
            if s.range().end == s.len()
                && s.selection() == "\n"
                && let Some('\n') = prev_char
            {
                s.set_anchor();
                s.move_hor(-1);
            }

            s.set_anchor_if_needed();
            s.replace("");
            s.unset_anchor();
        });

        if enter_insert_mode {
            *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::Change);
            mode::set(pa, crate::Insert::new(widget.clone()));
        }
    }

    /// [`Normal`] command: Pastes on each selection.
    ///
    /// # Key equivalents:
    ///
    /// - `p`, `P`.
    ///
    /// [`Normal`]: super::Normal
    pub fn paste_on_selections(pa: &mut Pass, count: usize, behind: bool) {
        let (widget, _) = current_parts(pa);

        let pastes = duat_base::modes::paste_strings();
        if pastes.is_empty() {
            return;
        }
        let mut p_iter = pastes.iter().cycle();
        widget.edit_all(pa, |mut s| {
            let paste = p_iter.next().unwrap().repeat(count);

            let anchor_is_start = s.anchor_is_start();

            // If it ends in a new line, we gotta move to the start of the line.
            let appended = if behind {
                s.set_cursor_on_start();
                if paste.ends_with('\n') {
                    let char_col = s.v_cursor().char_col();
                    s.move_hor(-(char_col as i32));
                    s.insert(&paste);
                } else {
                    s.insert(&paste)
                }
                false
            } else {
                s.set_cursor_on_end();
                if paste.ends_with('\n') {
                    let (b, _) = s.text()[s.cursor()..]
                        .char_indices()
                        .find(|(_, char)| *char == '\n')
                        .unwrap_or_default();
                    s.move_to(s.cursor().byte() + b);
                    s.append(&paste);
                } else {
                    s.append(&paste)
                }
                true
            };

            if !paste.is_empty() {
                s.move_hor(appended as i32);
                s.set_anchor();
                s.move_hor(paste.chars().count() as i32 - 1);
                if !anchor_is_start {
                    s.set_cursor_on_start();
                }
            }
        });
    }

    /// [`Normal`] command: Pastes over the content of selections.
    ///
    /// # Key equivalents:
    ///
    /// - `R`.
    ///
    /// [`Normal`]: super::Normal
    pub fn paste_over_selections(pa: &mut Pass, count: usize) {
        let (widget, _) = current_parts(pa);

        let pastes = duat_base::modes::paste_strings();
        if !pastes.is_empty() {
            let mut p_iter = pastes.iter().cycle();
            widget.edit_all(pa, |mut s| {
                s.set_anchor_if_needed();
                s.replace(p_iter.next().unwrap().repeat(count))
            });
        }
    }

    /// [`Normal`] command: Copies the selections vertically.
    ///
    /// # Key equivalents:
    ///
    /// - `C`, `<a-C>`.
    ///
    /// [`Normal`]: super::Normal
    pub fn copy_selection_ver(pa: &mut Pass, above: bool) {
        let (widget, _) = current_parts(pa);

        fn cols_eq(lhs: (VPoint, Option<VPoint>), rhs: (VPoint, Option<VPoint>)) -> bool {
            lhs.0.visual_col() == rhs.0.visual_col()
                && lhs
                    .1
                    .zip(rhs.1)
                    .is_none_or(|(lhs, rhs)| lhs.visual_col() == rhs.visual_col())
        }

        let (nth, mult) = if above {
            (0, -1)
        } else {
            (widget.selections(pa).len() - 1, 1)
        };

        widget.edit_nth(pa, nth, |mut s| {
            s.copy();
            let (v_cursor, v_anchor) = (s.v_cursor(), s.v_anchor());
            let lines_diff = v_anchor.map(|vp| vp.line().abs_diff(v_cursor.line()) as i32);
            let mut lines = mult * (lines_diff.unwrap_or(0) + 1);

            while s.move_ver(lines) {
                s.move_to_col(v_cursor.visual_col());
                if let Some(v_anchor) = v_anchor {
                    s.swap_ends();
                    s.move_ver(lines);
                    s.move_to_col(v_anchor.visual_col());
                    s.swap_ends();
                }

                if cols_eq((v_cursor, v_anchor), (s.v_cursor(), s.v_anchor())) {
                    return;
                }
                lines = mult;
            }
            s.destroy();
        });

        widget.set_main_selection(pa, nth + (!above) as usize);
    }

    /// [`Normal`] command: Copies the selections vertically.
    ///
    /// `count` represents the amount of matches to move. Negative
    /// numbers go backwards, positive go forwards.
    ///
    /// # Key equivalents:
    ///
    /// - `n`, `<a-n>` for selecting.
    /// - `N`, `<a-N>` for extending.
    ///
    /// [`Normal`]: super::Normal
    pub fn next_search_match(pa: &mut Pass, count: i32, extend_selections: bool) {
        let (widget, _) = current_parts(pa);
        let abs = count.unsigned_abs() as usize;

        if extend_selections {
            let search = SEARCH.lock().unwrap();
            if search.is_empty() {
                context::warn!("No search pattern set");
                return;
            }
            widget.edit_main(pa, |mut s| {
                let mut found = Vec::new();
                if count < 0 {
                    found.extend(s.search(&*search).to_cursor().rev().take(abs));
                    found.extend(
                        s.search(&*search)
                            .from_cursor_excl()
                            .rev()
                            .take(abs - found.len()),
                    );
                } else {
                    found.extend(s.search(&*search).from_cursor_excl().take(abs));
                    found.extend(s.search(&*search).to_cursor().take(abs - found.len()));
                };

                for range in found {
                    s.copy();
                    s.move_to(range.start);
                    if !range.is_empty() {
                        s.set_anchor();
                        s.move_to(range.end);
                        s.move_hor(-1);
                    }
                }
            });
        } else {
            let search = SEARCH.lock().unwrap();
            if search.is_empty() {
                context::warn!("No search pattern set");
                return;
            }
            widget.edit_main(pa, |mut s| {
                let found = if count >= 0 {
                    s.search(&*search)
                        .from_cursor_excl()
                        .chain(s.search(&*search).to_cursor())
                        .nth(abs - 1)
                } else {
                    s.search(&*search)
                        .to_cursor()
                        .rev()
                        .chain(s.search(&*search).from_cursor_excl().rev())
                        .nth(abs - 1)
                };
                if let Some(range) = found {
                    s.move_to(range.start);
                    if !range.is_empty() {
                        s.set_anchor();
                        s.move_to(range.end);
                        s.move_hor(-1);
                    }
                }
            });
        }
    }

    /// [`Normal`] command: Copies the selections vertically.
    ///
    /// # Key equivalents:
    ///
    /// - `*`.
    ///
    /// [`Normal`]: super::Normal
    pub fn set_search_to_main_selection(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);

        widget.edit_main(pa, |s| {
            let pat = s.selection().to_string();

            let is_w = |char: Option<char>| char.is_some_and(|char| char.is_alphanumeric());

            let s_boundary = if is_w(pat.chars().next()) { r"\b" } else { "" };
            let e_boundary = if is_w(pat.chars().last()) { r"\b" } else { "" };

            let pattern = format!(r"{s_boundary}{}{e_boundary}", escaped_str(&pat));
            context::info!("Set search pattern to [a]{pattern}");
            *SEARCH.lock().unwrap() = pattern;
        })
    }

    /// [`Normal`] command: Jumps forward/backwards through selection
    /// changes.
    ///
    /// # Key equivalents:
    ///
    /// - `<a-u>`, <a-U>.
    ///
    /// [`Normal`]: super::Normal
    pub fn move_on_selection_changes(pa: &mut Pass, count: i32) {
        if count == 0 {
            return;
        }
        let (widget, _) = current_parts(pa);

        if let Some(buffer) = widget.get_as()
            && let Some(jump) = buffer.move_jumps_by(pa, *U_ALT_U_ID, count)
        {
            jump.apply(pa, &buffer);
        }
    }

    /// [`Normal`] command: Jumps forward/backwards through jump list.
    ///
    /// # Key equivalent:
    ///
    /// - `<c-o>`, <c-i>.
    ///
    /// [`Normal`]: super::Normal
    pub fn move_on_jump_list(pa: &mut Pass, count: i32) {
        if count == 0 {
            return;
        }

        let (widget, _) = current_parts(pa);
        if let Some(buffer) = widget.get_as() {
            super::jump_list::jump_by(pa, &buffer, count)
        }
    }

    /// [`Normal`] command: Save selections to jump list.
    ///
    /// # Key equivalent:
    ///
    /// - `<c-j>`.
    pub fn save_on_jump_list(pa: &mut Pass) {
        let (widget, _) = current_parts(pa);
        if let Some(buffer) = widget.get_as() {
            super::jump_list::register(pa, &buffer, 5)
        }
    }

    /// [`Normal`] command: Play the recorded macro.
    ///
    /// # Key equivalents:
    ///
    /// - `q`.
    ///
    /// [`Normal`]: super::Normal
    pub fn play_macro(_: &mut Pass) {
        if hook::group_exists(*MACRO_NS) {
            context::warn!("Recursive macro calls are not permitted");
        } else {
            if let Some(key_events) = MACRO.lock().unwrap().clone() {
                duat_core::mode::type_keys(key_events);
            } else {
                context::warn!("No macro recorded");
            }
        }
    }

    /// [`Normal`] command: Start recording macro.
    ///
    /// # Key equivalents:
    ///
    /// - `Q`.
    ///
    /// [`Normal`]: super::Normal
    pub fn record_macro(_: &mut Pass) {
        if hook::group_exists(*MACRO_NS) {
            context::info!("Stopped recording macro");
            hook::remove(*MACRO_NS);
        } else {
            context::info!("Started recording macro");
            *MACRO.lock().unwrap() = None;
            hook::add::<KeyTyped>(|_, key_event| {
                if mode::is::<Normal>()
                    && let event!('Q' | 'q') = key_event
                {
                    return;
                }

                let mut macro_keys = MACRO.lock().unwrap();
                if let Some(key_events) = macro_keys.as_mut() {
                    key_events.push(key_event);
                } else {
                    *macro_keys = Some(vec![key_event]);
                }
            })
            .grouped(*MACRO_NS);
        }
    }

    /// [`Normal`] command: Go to a specific line
    ///
    /// # Key equivalents:
    ///
    /// - `{num}g`, `{num}G`.
    ///
    /// [`Normal`]: super::Normal
    pub fn go_to_line(pa: &mut Pass, line: usize, extend_selection: bool) {
        save_on_jump_list(pa);
        let (widget, _) = current_parts(pa);
        let line = line.min(widget.text(pa).end_point().line());

        widget.remove_extra_selections(pa);
        widget.edit_main(pa, |mut s| {
            if extend_selection {
                s.set_anchor_if_needed();
            } else {
                s.unset_anchor();
            }
            s.move_to_coords(line, 0);
        });

        mode::reset_current_sequence(pa);
        save_on_jump_list(pa);
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct Brackets(pub(crate) &'static [[&'static str; 2]]);

impl Brackets {
    pub(crate) fn bounds_matching(&self, bound: &Strs) -> Option<[&'static str; 2]> {
        self.0
            .iter()
            .find(|bs| bs.contains(&escaped_regex(bound)))
            .copied()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &[&'static str; 2]> + '_ {
        self.0.iter()
    }
}

fn no_nl_pair(iter: impl Iterator<Item = (usize, char)>) -> Option<[(usize, char); 2]> {
    let mut entry0 = None;

    for (point, char) in iter {
        if char == '\n' {
            entry0 = None;
        } else if let Some(entry0) = entry0 {
            return Some([entry0, (point, char)]);
        } else {
            entry0 = Some((point, char));
        }
    }

    None
}

fn word_and_space(alt_word: bool, opts: PrintOpts) -> String {
    if alt_word {
        "[^ \t\n]+[ \t]*|[ \t]+".to_string()
    } else {
        let cat = opts.word_chars_regex();
        format!("([{cat}]+|[^{cat} \t\n]+)[ \t]*|[ \t]+")
    }
}

fn current_parts(pa: &Pass) -> (Handle, PrintOpts) {
    let widget = context::current_widget(pa);
    let popts = widget.opts(pa);
    (widget, popts)
}

fn set_onekey(one_key: OneKey) {
    *ONE_KEY.lock().unwrap() = Some(one_key);
}

#[derive(Clone, Copy)]
enum InsertKey {
    Insert,
    Append,
    Change,
    InsertStart,
    AppendEnd,
    NewLineBelow,
    NewLineAbove,
}

static SEL_TYPE: Mutex<SelType> = Mutex::new(SelType::Normal);
static ONE_KEY: Mutex<Option<OneKey>> = Mutex::new(None);

static LAST_INSERT_KEY: Mutex<Option<InsertKey>> = Mutex::new(None);
static ALT_DOT: Mutex<Option<(OneKey, KeyEvent)>> = Mutex::new(None);

static MACRO: Mutex<Option<Vec<KeyEvent>>> = Mutex::new(None);
static MACRO_NS: LazyLock<Ns> = Ns::new_lazy();

static U_ALT_U_ID: LazyLock<Ns> = Ns::new_lazy();

pub(crate) mod jump_list {
    use std::sync::{LazyLock, Mutex};

    use duat_base::{hooks::PickerEntrySelected, widgets::FilePlace};
    use duat_core::{
        Ns,
        buffer::{Buffer, BufferId},
        context::{self, Handle},
        data::Pass,
        hook::{self, BufferSwitched},
        mode,
    };
    use duat_jump_list::{BufferJumps, JumpId};

    static JUMP_LIST: Mutex<JumpList> = Mutex::new(JumpList::new());
    static NS: LazyLock<Ns> = Ns::new_lazy();
    static JUMPS_NS: LazyLock<Ns> = Ns::new_lazy();

    /// A list for jumping around [`Buffer`]s
    #[derive(Debug)]
    pub struct JumpList {
        list: Vec<(JumpId, BufferId)>,
        cur: usize,
    }

    impl JumpList {
        /// Returns a new `JumpList`
        pub const fn new() -> Self {
            Self { list: Vec::new(), cur: 0 }
        }
    }

    /// Register a new jump, if it would be different
    ///
    /// If an equal jump was found at most `eq_lookback` jumps
    /// back, then don't register.
    pub fn register(pa: &mut Pass, buffer: &Handle<Buffer>, eq_lookback: usize) {
        let mut jl = JUMP_LIST.lock().unwrap();

        let jump_id = buffer.record_or_get_current_jump(pa, *NS);

        for (jump_id, buffer_id) in jl.list[..jl.cur].iter().rev().take(eq_lookback) {
            if *buffer_id == buffer.read(pa).buffer_id()
                && let Some(jump) = buffer.get_jump(pa, *NS, *jump_id)
                && jump.is_eq(buffer.read(pa))
            {
                return;
            }
        }

        let cur = jl.cur;
        jl.list.truncate(cur);
        jl.list.push((jump_id, buffer.read(pa).buffer_id()));
        jl.cur += 1;
    }

    /// Jumps by `by` jumps, which can go across [`Buffer`]s and
    /// stuff.
    pub fn jump_by(pa: &mut Pass, buffer: &Handle<Buffer>, by: i32) {
        let mut jl = JUMP_LIST.lock().unwrap();

        if jl.list.is_empty() || by == 0 {
            return;
        }

        jl.cur = jl.cur.saturating_add_signed(by as isize).min(jl.list.len());

        while let Some((jump_id, buffer_id)) = jl.list.get(jl.cur).cloned() {
            let cur = jl.cur;

            let new_buffer = if buffer_id != buffer.read(pa).buffer_id() {
                if let Some(buffer) = {
                    context::windows()
                        .buffers(pa)
                        .into_iter()
                        .find(|buffer| buffer.read(pa).buffer_id() == buffer_id)
                } {
                    hook::remove(*JUMPS_NS);
                    mode::reset_to(pa, &buffer);
                    add_jump_hooks();

                    buffer.clone()
                } else {
                    jl.list.remove(cur);
                    jl.cur = jl.cur.saturating_sub((by < 0) as usize);
                    continue;
                }
            } else {
                buffer.clone()
            };

            if let Some(jump) = new_buffer.go_to_jump(pa, *NS, jump_id) {
                if !new_buffer.ptr_eq(buffer.widget()) || !jump.is_eq(new_buffer.read(pa)) {
                    jump.apply(pa, &new_buffer);
                } else if jl.cur > 0 {
                    jl.cur -= 1;
                    continue;
                }
                break;
            } else {
                jl.list.remove(cur);
                jl.cur = jl.cur.saturating_sub((by < 0) as usize);
            }
        }
    }

    /// Add the hook for automatic insertion of jumps
    pub fn add_jump_hooks() {
        hook::add::<BufferSwitched>(|pa, (former, current)| {
            if !former.is_closed() {
                register(pa, former, 5);
            }
            register(pa, current, 5);
        })
        .grouped(*JUMPS_NS);

        hook::add::<PickerEntrySelected<FilePlace>>(|pa, _| {
            let buffer = context::current_buffer(pa);
            register(pa, &buffer, 5);
        })
        .lateness(49);

        hook::add::<PickerEntrySelected<FilePlace>>(|pa, _| {
            let buffer = context::current_buffer(pa);
            register(pa, &buffer, 5);
        })
        .lateness(51);
    }
}
