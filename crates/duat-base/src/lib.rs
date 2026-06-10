//! Standard complementary additions to Duat
//!
//! This crate essentially consists of the standard bits that pretty
//! much every `config` crate will want to use, but aren't strictly
//! speaking necessary for Duat to function. This split is mostly to
//! improve compile times, but semantically, if a crate doesn't need
//! all of these extra things, it is nice to separate them out.
//!
//! The crate has the following elements:
//!
//! - 10 [`widgets`]:
//!   - [`LineNumbers`] shows the numbers on a [`Buffer`] (for now),
//!     and you can configure their alignment, relativeness, etc.
//!   - The [`PromptLine`] lets you run commands and do other things,
//!     like incremental search
//!   - The [`StatusLine`] lets you display information that gets
//!     updated automatically, it can show information from
//!     [`RwData`]s, mapping functions, static elements, and every bit
//!     of Duat. It's syntax, in [`status!`] is the same as the
//!     [`txt!`] macro.
//!   - [`Notifications`] shows things that have been logged to the
//!     [`Logs`] of Duat, through the [`error!`], [`warn!`] and
//!     [`info!`] macros.
//!   - [`LogBook`] is a log of everything that has been notified to
//!     Duat. It is usually more admissive than `Notifications`. It
//!     can be focused on by calling the `"logs"` command.
//!   - [`Completions`] is Duat's completion widget, it provides an
//!     extensible completions list, which allows you to format the
//!     entries and add lists via [`Completions::add_list`].
//!   - [`Gutter`] Sits on the side of each `Buffer`, showing
//!     diagnostic information about each line of the `Buffer`.
//!   - [`WhichKey`] shows what each key will do. It shows up
//!     automatically as you are typing and multi key sequences are
//!     expected (e.g. Vim's `s`, `d`, `f` and others).
//!   - [`Info`] just shows static information, resizing itself to
//!     properly show as much of it as possible.
//!   - [`Picker`] shows options for places to jump to, or things to
//!     enact, like code actions. It comes with builtin preview.
//!
//! - 1 [mode](mode::Mode):
//!   - [`Prompt`] is a multitool that can serve many purposes,
//!     through the [`PromptMode`] trait, which allows one to act on
//!     the `PromptLine` while abstracting over less important
//!     elements of the `Widget`.
//!
//! - For the [`PromptLine`], there are 4 [`PromptMode`]s:
//!   - [`RunCommands`] will interpret and run Duat commands, with
//!     syntax highlighting for correctness, defined by the
//!     [`Parameter`] trait.
//!   - [`PipeSelections`] will pipe each selection on the current
//!     `Buffer`, replacing them with the return value from a shell
//!     command.
//!   - [`IncSearch`] is a specialized mode used for incremental
//!     search, which can abstract over what the search actually does
//!     with the [`IncSearcher`] trait.
//!
//! - For [`IncSearch`], there are 4 `IncSearcher`s:
//!   - [`SearchFwd`] will move each [`SelectionMut`] to the next
//!     match.
//!   - [`SearchRev`] will move each `SelectionMut` to the previous
//!     match.
//!   - [`ExtendFwd`] will extend each `SelectionMut`'s selections to
//!     the next match.
//!   - [`ExtendRev`] will extend each `SelectionMut`'s selections to
//!     the previous match.
//!
//! Note that the [`IncSearcher`] trait can be used for many more
//! interesting things, like in [`duatmode`] for example, where its
//! implementors allow for splitting selections, selecting everything
//! within a range, and many more such things in the future.
//!
//! - There are also six [`hooks`]:
//!   - [`SearchUpdated`] for when an `IncSearch` is updated.
//!   - [`SearchPerformed`] for when an `IncSearch` is finished.
//!
//! There is the [`state`] module, which contains a bunch of
//! [`StatusLine`] parts for you to customize the `StatusLine` with.
//!
//! And finally, `duat-base` also adds support for snippets.
//!
//! I would consider this crate essential for all `config`s of Duat
//! out there, since it defines primitives that are not only hard to
//! replace, but might also be very extensible by plugins in the
//! ecosystem.
//!
//! [`LineNumbers`]: widgets::LineNumbers
//! [`Buffer`]: duat_core::buffer::Buffer
//! [`PromptLine`]: widgets::PromptLine
//! [`StatusLine`]: widgets::StatusLine
//! [`RwData`]: duat_core::data::RwData
//! [`status!`]: widgets::status
//! [`txt!`]: duat_core::text::txt
//! [`Notifications`]: widgets::Notifications
//! [`Logs`]: duat_core::context::Logs
//! [`error!`]: duat_core::context::error
//! [`warn!`]: duat_core::context::warn
//! [`info!`]: duat_core::context::info
//! [`Mode`]: duat_core::mode::Mode
//! [`Prompt`]: modes::Prompt
//! [`PromptMode`]: modes::PromptMode
//! [`Widget`]: duat_core::ui::Widget
//! [`RunCommands`]: modes::RunCommands
//! [`Parameter`]: duat_core::cmd::Parameter
//! [`PipeSelections`]: modes::PipeSelections
//! [`IncSearch`]: modes::IncSearch
//! [`IncSearcher`]: modes::IncSearcher
//! [`SearchFwd`]: modes::SearchFwd
//! [`SelectionMut`]: duat_core::mode::SelectionMut
//! [`SearchRev`]: modes::SearchRev
//! [`ExtendFwd`]: modes::ExtendFwd
//! [`ExtendRev`]: modes::ExtendRev
//! [`duat-kak`]: https://docs.rs/duat-kak/latest/duat_kak
//! [`SearchUpdated`]: hooks::SearchUpdated
//! [`SearchPerformed`]: hooks::SearchPerformed
//! [`LogBook`]: widgets::LogBook
//! [`Completions`]: widgets::Completions
//! [`Completions::add_list`]: widgets::Completions::add_list
//! [`WhichKey`]: widgets::WhichKey
//! [`Info`]: widgets::Info
//! [`Gutter`]: widgets::Gutter
//! [`Picker`]: widgets::Picker
//! [`duatmode`]: https://docs.rs/duatmode
use duat_core::{
    Ns,
    buffer::Buffer,
    cmd,
    context::{self, Handle},
    data::Pass,
    form::{self, Form},
    mode,
    text::{Point, Text, TextRange, txt},
};
use regex_syntax::ast::Ast;

use crate::widgets::{GutterEntryId, LogBook};

mod buffer_parser;
pub mod modes;
mod snippets;
pub mod state;
pub mod widgets;

/// The plugin for `duat-base`
///
/// This plugin will setup forms, hooks, completions, and add a
/// default parser for [`BufferOpts`].
///
/// The provided [`Ns`] will be used for the `DefaultOptsParser`.
///
/// [`BufferOpts`]: duat_core::buffer::BufferOpts
#[derive(Default)]
pub struct DuatBase {
    /// Wether to enable the default opts parser.
    pub default_opts_parser: bool,
}

impl DuatBase {
    /// Adds the `DuatBase` plugin.
    ///
    /// *DON'T USE THIS DIRECTLY, USE `duat::plug` INSTEAD*.
    #[doc(hidden)]
    #[inline(never)]
    pub fn _plug(self) {
        widgets::linenumbers_setup();
        widgets::gutter_setup();
        widgets::info_setup();
        widgets::logbook_setup();
        widgets::notifications_setup();
        widgets::promptline_setup();
        widgets::whichkey_setup();
        widgets::completions_setup();
        widgets::picker_setup();

        snippets::add_snippet_hook();

        modes::add_prompt_hook();
        if self.default_opts_parser {
            buffer_parser::enable_parser();
        }

        // Setup for the LineNumbers
        form::set_weak("linenum.main", Form::new().yellow());
        form::set_weak("linenum.wrapped", Form::new().cyan().italic());

        // Setup for the StatusLine
        form::set_weak("buffer", Form::new().yellow().italic());
        form::set_weak("selections", Form::new().dark_blue());
        form::set_weak("coord", Form::mimic("contant"));
        form::set_weak("separator", Form::mimic("punctuation.delimiter"));
        form::set_weak("mode", Form::new().green());
        form::set_weak("default.StatusLine", Form::new().on_dark_grey());

        // Setup for the PromptLine
        form::set_weak("prompt.preview", Form::mimic("comment"));
        form::set_weak("regex.error", Form::mimic("accent.error"));
        form::set_weak("regex.operator", Form::mimic("operator"));
        form::set_weak("regex.class", Form::mimic("constant"));
        form::set_weak("regex.bracket", Form::mimic("punctuation.bracket"));
        form::set_weak("caller.info", Form::mimic("accent.info"));
        form::set_weak("caller.error", Form::mimic("accent.error"));
        form::set_weak("param.info", Form::mimic("default.info"));
        form::set_weak("param.error", Form::mimic("default.error"));

        // Setup for Completions
        form::set_weak("default.Completions", Form::new().on_dark_grey());
        form::set_weak("selected.Completions", Form::new().black().on_grey());
        form::set_weak("completion.word.source", Form::mimic("buffer"));

        // Setup for the Gutter
        form::set_weak("gutter.hint", Form::mimic("default.info"));
        form::set_weak("gutter.warn", Form::mimic("default.warn"));
        form::set_weak("gutter.error", Form::mimic("default.error"));
        form::set_weak("diagnostic.line", Form::mimic("replace"));
        form::set_weak("buffer.hint", Form::new().underline_grey().underlined());
        form::set_weak("buffer.warn", Form::new().underline_yellow().underlined());
        form::set_weak("buffer.error", Form::new().underline_red().underlined());
        form::enable_mask("diagnostic");

        // Setup for WhichKey
        form::set_weak("key", Form::mimic("const"));
        form::set_weak("key.mod", Form::mimic("punctuation.bracket"));
        form::set_weak("key.angle", Form::mimic("punctuation.bracket"));
        form::set_weak("key.special", Form::new().yellow());
        form::set_weak("remap", Form::new().italic());
        form::set_weak(
            "default.WhichKeyDescriptions",
            Form::mimic("default.WhichKey"),
        );

        // Setup for the LogBook
        form::set_weak("default.LogBook", Form::new().on_dark_grey());
        form::set_weak("logbook.error", Form::mimic("default.error"));
        form::set_weak("logbook.warn", Form::mimic("default.warn"));
        form::set_weak("logbook.info", Form::mimic("default.info"));
        form::set_weak("logbook.debug", Form::mimic("default.debug"));
        form::set_weak("logbook.colon", Form::mimic("prompt.colon"));
        form::set_weak("logbook.bracket", Form::mimic("punctuation.bracket"));
        form::set_weak("logbook.target", Form::mimic("module"));

        cmd::add("logs", |pa: &mut _| {
            let Some(logbook) = context::handle_of::<LogBook>(pa) else {
                return Ok(None);
            };

            if logbook.area().width(pa) > 0.0 && logbook.area().height(pa) > 0.0 {
                _ = logbook.area().hide(pa);
            } else {
                _ = logbook.area().reveal(pa);
                mode::reset_to(pa, &logbook);
            }

            Ok(None)
        })
        .doc(txt!("Toggle and focus on the [a]Logs[]"), None);
    }
}

#[allow(private_bounds)]
trait Sealed {}
/// Trait with various methods that are useful for [`Buffer`]s.
#[allow(private_bounds)]
pub trait BaseBuffer: Sealed {
    ////////// Gutter methods

    /// Remove all [`Gutter`] entries from a given [`Ns`].
    ///
    /// [`Gutter`]: widgets::Gutter
    #[track_caller]
    fn remove_gutter_entries(&self, pa: &mut Pass, ns: Ns);

    /// Add a hint to the [`Gutter`] and the [`Buffer`].
    ///
    /// This could just be useful information, like the fact that
    /// something won't be included in compilation because of a `cfg`
    /// attribute.
    ///
    /// Note: This function won't add duplicated entries, instead
    /// returning the [`GutterEntryId`] of the entry that was already
    /// in there.
    ///
    /// [`Gutter`]: widgets::Gutter
    #[track_caller]
    fn add_hint(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId;

    /// Add a warning to the [`Gutter`] and the [`Buffer`].
    ///
    /// This could be improvements that you could do to your code, or
    /// ways in which it is innadequate that don't necessarily hinder
    /// it from working properly.
    ///
    /// Note: This function won't add duplicated entries, instead
    /// returning the [`GutterEntryId`] of the entry that was already
    /// in there.
    ///
    /// [`Gutter`]: widgets::Gutter
    #[track_caller]
    fn add_warning(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text)
    -> GutterEntryId;

    /// Add an error to the [`Gutter`] and the [`Buffer`].
    ///
    /// These are fundamental issues in your code, and either prevent
    /// compilation, or prevent it from working properly.
    ///
    /// Note: This function won't add duplicated entries, instead
    /// returning the [`GutterEntryId`] of the entry that was already
    /// in there.
    ///
    /// [`Gutter`]: widgets::Gutter
    fn add_error(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId;

    /// Wether  this [`Buffer`] has a [`Gutter`] or not.
    ///
    /// This should return `true` everytime, unless you disable this
    /// functionality.
    ///
    /// [`Gutter`]: widgets::Gutter
    fn has_gutter(&self, pa: &Pass) -> bool;

    /// Hovers over the gutter entries in a [`Point`], as if the mouse
    /// had gone over them.
    #[track_caller]
    fn hover_gutter_entries_on(&self, pa: &Pass, point: Point);

    ////////// Snippet methods

    /// Replace a range in the `Buffer`'s [`Text`] with a snippet.
    ///
    /// This snippet may contain cursor positions. These cursor
    /// positions can be quickly used to jump around and replace
    /// parts of the `Text`. For example `"function_call($1, $2)$0"`
    /// can be used to quickly fill in the arguments of the function.
    ///
    /// With `num` being a number, the syntax goes as following:
    ///
    /// - `$num` or `${num} for a jump.
    /// - `${num:placeholder}` for a jump with a placeholder.
    ///
    /// You can have multiple of the same `num`. This would spawn
    /// multiple cursors editing the `Text` at the same time.
    ///
    /// Additionally, the `$0` or `${0:placeholder}` will always be
    /// the _last_ position to be jumped to. If there is no `$0`,
    /// then duat will assume that one should be at the end of the
    /// string.
    #[track_caller]
    fn replace_with_snippet(&self, pa: &mut Pass, range: impl TextRange, snippet: impl ToString);

    /// Jumps over the snippets.
    ///
    /// This will move the selections to the next jump, which could
    /// be composed of multiple selections. If `by == 0`, it will
    /// simply move to the last selected jump, if there was one.
    ///
    /// Returns `true` if anything happened.
    fn jump_snippets(&self, pa: &mut Pass, by: i32) -> bool;
}

impl Sealed for Handle<Buffer> {}
impl BaseBuffer for Handle<Buffer> {
    #[track_caller]
    fn remove_gutter_entries(&self, pa: &mut Pass, ns: Ns) {
        widgets::remove_gutter_entries(self, pa, ns);
    }

    #[track_caller]
    fn add_hint(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId {
        widgets::add_hint(self, pa, ns, range, msg)
    }

    #[track_caller]
    fn add_warning(
        &self,
        pa: &mut Pass,
        ns: Ns,
        range: impl TextRange,
        msg: Text,
    ) -> GutterEntryId {
        widgets::add_warning(self, pa, ns, range, msg)
    }

    #[track_caller]
    fn add_error(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId {
        widgets::add_error(self, pa, ns, range, msg)
    }

    fn has_gutter(&self, pa: &Pass) -> bool {
        widgets::has_gutter(self, pa)
    }

    fn hover_gutter_entries_on(&self, pa: &Pass, point: Point) {
        widgets::hover_gutter_entries_on(self, pa, point);
    }

    #[track_caller]
    fn replace_with_snippet(&self, pa: &mut Pass, range: impl TextRange, snippet: impl ToString) {
        let range = range.to_range(self.text(pa).len());

        snippets::replace_with_snippet(self, pa, range, snippet.to_string());
    }

    fn jump_snippets(&self, pa: &mut Pass, by: i32) -> bool {
        snippets::jump_snippets(self, pa, by)
    }
}

pub mod hooks {
    //! Additional hooks for `duat-base` specific things
    //!
    //! Right now, these are the two [`hook`]s available for use:
    //!
    //!   - [`SearchUpdated`] for when an [`IncSearch`] is updated.
    //!   - [`SearchPerformed`] for when an [`IncSearch`] is finished.
    //!
    //! More may come in the future
    //!
    //! [`hook`]: duat_core::hook
    //! [`IncSearch`]: crate::modes::IncSearch
    use std::{any::Any, marker::PhantomData, ops::Range};

    use duat_core::{data::Pass, hook::Hookable};

    use crate::widgets::{InnerCompletionEntry, PickerPreview};

    /// [`Hookable`]: Triggers when a [search] is updated.
    ///
    /// Will not be triggered if the previous and current patterns are
    /// the same.
    ///
    /// # Arguments
    ///
    /// - The previous regex pattern
    /// - The current regex pattern
    ///
    /// [search]: crate::modes::IncSearch
    pub struct SearchUpdated(pub(crate) (String, String));

    impl Hookable for SearchUpdated {
        type Input<'h> = (&'h str, &'h str);

        fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
            (&self.0.0, &self.0.1)
        }
    }

    /// [`Hookable`]: Triggers when a [search] is performed.
    ///
    /// Will not be triggered on empty searches.
    ///
    /// # Arguments
    ///
    /// - The searched regex pattern
    ///
    /// [search]: crate::modes::IncSearch
    pub struct SearchPerformed(pub(crate) String);

    impl Hookable for SearchPerformed {
        type Input<'h> = &'h str;

        fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
            &self.0
        }
    }

    /// [`Hookable`]: Triggers when a completion entry is selected.
    ///
    /// This happens while you're pressing a key like tab to scroll
    /// through the completion options. For when you actually pick the
    /// completion item, or if the completions close without you
    /// selecting anything, check out [`CompletionSelected`].
    ///
    /// # Arguments
    ///
    /// - A [`CompletionEntry`], which contains the focused
    ///   [`CompletionItem`].
    ///
    /// [`CompletionItem`]: crate::widgets::CompletionItem
    pub struct CompletionFocused<C>(pub(crate) (InnerCompletionEntry, PhantomData<C>));

    impl<C: 'static> Hookable for CompletionFocused<C> {
        type Input<'h> = CompletionEntry<'h, C>;

        fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
            CompletionEntry {
                index: self.0.0.index,
                orig_range: self.0.0.orig_range.clone(),
                orig_typed: &self.0.0.orig_typed,
                replacement: &self.0.0.replacement,
                item: self.0.0.get_as().unwrap(),
            }
        }
    }

    /// [`Hookable`]: Triggers right after finishing the completions.
    ///
    /// This happens when you type a character and a completion entry
    /// is selected. If you want to do something while pressing a key
    /// like tab in order to scroll through the completions, check out
    /// [`CompletionFocused`].
    ///
    /// # Arguments
    ///
    /// - A [`CompletionEntry`], which contains the selected
    ///   [`CompletionItem`].
    ///
    /// [`CompletionItem`]: crate::widgets::CompletionItem
    pub struct CompletionSelected<C>(pub(crate) (InnerCompletionEntry, PhantomData<C>));

    impl<C: 'static> Hookable for CompletionSelected<C> {
        type Input<'h> = CompletionEntry<'h, C>;

        fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
            CompletionEntry {
                index: self.0.0.index,
                orig_range: self.0.0.orig_range.clone(),
                orig_typed: &self.0.0.orig_typed,
                replacement: &self.0.0.replacement,
                item: self.0.0.get_as().unwrap(),
            }
        }
    }

    /// A completion entry.
    ///
    /// This came from some [`CompletionItem`], and is used on
    /// the [`CompletionSelected`] and [`CompletionFocused`]
    /// hooks.
    ///
    /// [`CompletionItem`]: crate::widgets::CompletionItem
    pub struct CompletionEntry<'h, C> {
        /// The actual item.
        pub item: &'h C,
        /// The index on the list where this item came from.
        pub index: usize,
        /// The original byte range of the text being replaced.
        pub orig_range: Range<usize>,
        /// What was typed by the user.
        pub orig_typed: &'h str,
        /// What the text was replaced with.
        pub replacement: &'h str,
    }

    impl<C> std::ops::Deref for CompletionEntry<'_, C> {
        type Target = C;

        fn deref(&self) -> &Self::Target {
            self.item
        }
    }

    /// [`Hookable`]: Triggers when a [`Picker`] entry is focused.
    ///
    /// # Arguments
    ///
    /// - The entry in question.
    /// - A [`PickerPreview`], which lets you set what [`Text`] should
    ///   be previewed on the side panel.
    ///
    /// [`Picker`]: crate::widgets::Picker
    /// [`Text`]: duat_core::text::Text
    pub struct PickerEntryFocused<T>(
        pub(crate) (Box<dyn Any + Send>, PickerPreview, PhantomData<T>),
    );

    impl<T: 'static> Hookable for PickerEntryFocused<T> {
        type Input<'h> = (&'h T, &'h PickerPreview);

        fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
            let entry = self.0.0.downcast_ref().unwrap();
            (entry, &self.0.1)
        }
    }

    /// [`Hookable`]: Triggers when a [`Picker`] entry is selected.
    ///
    /// For the [`FilePlace`] picker entry type, the jump to the new
    /// location will happen with a [lateness] of `50`. This means
    /// that if you add hooks with a lateness smaller than `50`,
    /// these hooks will take place before jumping to the new
    /// location.
    ///
    /// # Arguments
    ///
    /// - The entry in question.
    ///
    /// [`Picker`]: crate::widgets::Picker
    /// [`Text`]: duat_core::text::Text
    /// [`FilePlace`]: crate::widgets::FilePlace
    /// [lateness]: duat_core::hook::HookBuilder::lateness
    pub struct PickerEntrySelected<T>(pub(crate) (Box<dyn Any + Send>, PhantomData<T>));

    impl<T: 'static> Hookable for PickerEntrySelected<T> {
        type Input<'h> = &'h T;

        fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
            self.0.0.downcast_ref().unwrap()
        }
    }
}

fn tag_from_ast(ns: Ns, text: &mut Text, ast: &Ast) {
    use duat_core::form::FormId;
    use regex_syntax::ast::{Ast::*, Span};

    let mut insert_form = |id: FormId, span: Span| {
        text.insert_tag(ns, span.start.offset..span.end.offset, id.to_tag(0));
    };

    match ast {
        Empty(_) => {}
        Flags(set_flags) => {
            let id = form::id_of!("regex.operator.flags");
            insert_form(id, set_flags.span);
        }
        Literal(literal) => {
            let id = form::id_of!("regex.literal");
            insert_form(id, literal.span);
        }
        Dot(span) => {
            let id = form::id_of!("regex.operator.dot");
            insert_form(id, **span);
        }
        Assertion(assertion) => {
            let id = form::id_of!("regex.operator.assertion");
            insert_form(id, assertion.span);
        }
        ClassUnicode(class) => {
            let id = form::id_of!("regex.class.unicode");
            insert_form(id, class.span);
        }
        ClassPerl(class) => {
            let id = form::id_of!("regex.class.perl");
            insert_form(id, class.span);
        }
        ClassBracketed(class) => {
            let class_id = form::id_of!("regex.class.bracketed");
            let bracket_id = form::id_of!("regex.bracket.class");

            insert_form(class_id, *class.kind.span());

            let range = class.span.start.offset..class.span.start.offset + 1;
            text.insert_tag(ns, range, bracket_id.to_tag(0));
            let range = class.span.end.offset - 1..class.span.end.offset;
            text.insert_tag(ns, range, bracket_id.to_tag(0));
        }
        Repetition(repetition) => {
            let id = form::id_of!("regex.operator.repetition");
            insert_form(id, repetition.op.span);
        }
        Group(group) => {
            let group_id = form::id_of!("regex.group");
            let bracket_id = form::id_of!("regex.bracket.group");

            insert_form(group_id, *group.ast.span());

            let range = group.span.start.offset..group.span.start.offset + 1;
            text.insert_tag(ns, range, bracket_id.to_tag(0));
            let range = group.span.end.offset - 1..group.span.end.offset;
            text.insert_tag(ns, range, bracket_id.to_tag(0));

            tag_from_ast(ns, text, &group.ast);
        }
        Alternation(alternation) => {
            let id = form::id_of!("regex.operator.alternation");

            let mut prev_end = None;

            for ast in alternation.asts.iter() {
                tag_from_ast(ns, text, ast);

                if let Some(end) = prev_end {
                    let range = end..ast.span().start.offset;
                    text.insert_tag(ns, range, id.to_tag(0));
                }

                prev_end = Some(ast.span().end.offset);
            }
        }
        Concat(concat) => {
            for ast in concat.asts.iter() {
                tag_from_ast(ns, text, ast);
            }
        }
    }
}

#[doc(hidden)]
pub mod private_exports {
    pub use duat_core::{
        buffer::Buffer, context::Handle, data::Pass, form, text::Builder, ui::PushSpecs,
    };
    pub use format_like::format_like;
}
