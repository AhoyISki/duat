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
//! - 8 [`widgets`]:
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
//!     Duat. It is usually more admissive than `Notifications`, and
//!     is most commonly scrolled by the [`Pager`] [`Mode`].
//!   - [`Completions`] is Duat's completion widget, it provides an
//!     extensible completions list, which allows you to format the
//!     entries and add new providers via the [`CompletionsProvider`]
//!     trait. Right now, the only `CompletionsProvider` is the words
//!     provider.
//!   - [`WhichKey`] shows what each key will do. It shows up
//!     automatically as you are typing and multi key sequences are
//!     expected (e.g. Vim's `c`, `d`, `f` and others).
//!   - [`Info`] just shows static information, resizing itself to
//!     properly show as much as possible of it.
//!
//! - 2 [`modes`]:
//!   - [`Prompt`] is a multitool that can serve many purposes,
//!     through the [`PromptMode`] trait, which allows one to act on
//!     the `PromptLine` while abstracting over less important
//!     elements of the `Widget`.
//!   - [`Pager`] is a simple, read only `Mode`, designed for
//!     scrolling and searching through `Widget`s, most commonly the
//!     `LogBook`.
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
//!   - [`SearchFwd`] will move each [`Cursor`] to the next match.
//!   - [`SearchRev`] will move each `Cursor` to the previous match.
//!   - [`ExtendFwd`] will extend each `Cursor`'s selections to the
//!     next match.
//!   - [`ExtendRev`] will extend each `Cursor`'s selections to the
//!     previous match.
//!
//! Note that the [`IncSearcher`] trait can be used for many more
//! interesting things, like in [`duat-kak`] for example, where its
//! implementors allow for splitting selections, selecting everything
//! within a range, and many more such things in the future.
//!
//! - There are also two [`hooks`]:
//!   - [`SearchUpdated`] for when an `IncSearch` is updated.
//!   - [`SearchPerformed`] for when an `IncSearch` is finished.
//!
//! And finally, there is the [`state`] module, which contains a bunch
//! of [`StatusLine`] parts for you to customize the `StatusLine`
//! with.
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
//! [`Cursor`]: duat_core::mode::Cursor
//! [`SearchRev`]: modes::SearchRev
//! [`ExtendFwd`]: modes::ExtendFwd
//! [`ExtendRev`]: modes::ExtendRev
//! [`duat-kak`]: https://docs.rs/duat-kak/latest/duat_kak
//! [`SearchUpdated`]: hooks::SearchUpdated
//! [`SearchPerformed`]: hooks::SearchPerformed
//! [`Pager`]: modes::Pager
//! [`LogBook`]: widgets::LogBook
//! [`Completions`]: widgets::Completions
//! [`CompletionsProvider`]: widgets::CompletionsProvider
//! [`WhichKey`]: widgets::WhichKey
//! [`Info`]: widgets::Info
use duat_core::{
    Plugin, cmd,
    data::Pass,
    form::{self, Form},
    mode,
    text::{Tagger, Text, txt},
};
use regex_syntax::ast::Ast;

use crate::{modes::Pager, widgets::LogBook};

mod buffer_parser;
pub mod modes;
pub mod state;
pub mod widgets;

/// The plugin for `duat-base`
///
/// This plugin will setup forms, hooks, completions, and add a
/// default parser for [`BufferOpts`].
///
/// [`BufferOpts`]: duat_core::buffer::BufferOpts
#[derive(Default)]
pub struct DuatBase;

impl Plugin for DuatBase {
    fn plug(self, _: &duat_core::Plugins) {
        widgets::setup_completions();
        buffer_parser::enable_parser();

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

        // Setup for the LogBook
        form::set_weak("default.LogBook", Form::new().on_dark_grey());
        form::set_weak("log_book.error", Form::mimic("default.error"));
        form::set_weak("log_book.warn", Form::mimic("default.warn"));
        form::set_weak("log_book.info", Form::mimic("default.info"));
        form::set_weak("log_book.debug", Form::mimic("default.debug"));
        form::set_weak("log_book.colon", Form::mimic("prompt.colon"));
        form::set_weak("log_book.bracket", Form::mimic("punctuation.bracket"));
        form::set_weak("log_book.target", Form::mimic("module"));

        // Setup for the PromptLine
        form::set_weak("prompt.preview", Form::mimic("comment"));

        // Setup for Completions
        form::set_weak("default.Completions", Form::new().on_dark_grey());
        form::set_weak("selected.Completions", Form::new().black().on_grey());

        // Setup for WhichKey
        form::set_weak("key", Form::mimic("const"));
        form::set_weak("key.mod", Form::mimic("punctuation.bracket"));
        form::set_weak("key.angle", Form::mimic("punctuation.bracket"));
        form::set_weak("key.special", Form::new().yellow());
        form::set_weak("remap", Form::new().italic());

        cmd::add("logs", |pa: &mut Pass| {
            mode::set(pa, Pager::<LogBook>::new());
            Ok(None)
        })
        .doc(
            txt!("Open the [a]Logs[] and enter [mode]Pager[] mode"),
            None,
        );
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
    use duat_core::{data::Pass, hook::Hookable};

    /// [`Hookable`]: Triggers when a [search] is updated
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

    /// [`Hookable`]: Triggers when a [search] is performed
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
}

fn tag_from_ast(tagger: Tagger, text: &mut Text, ast: &Ast) {
    use duat_core::form::FormId;
    use regex_syntax::ast::{Ast::*, Span};

    let mut insert_form = |id: FormId, span: Span| {
        text.insert_tag(tagger, span.start.offset..span.end.offset, id.to_tag(0));
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
            text.insert_tag(tagger, range, bracket_id.to_tag(0));
            let range = class.span.end.offset - 1..class.span.end.offset;
            text.insert_tag(tagger, range, bracket_id.to_tag(0));
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
            text.insert_tag(tagger, range, bracket_id.to_tag(0));
            let range = group.span.end.offset - 1..group.span.end.offset;
            text.insert_tag(tagger, range, bracket_id.to_tag(0));

            tag_from_ast(tagger, text, &group.ast);
        }
        Alternation(alternation) => {
            let id = form::id_of!("regex.operator.alternation");

            let mut prev_end = None;

            for ast in alternation.asts.iter() {
                tag_from_ast(tagger, text, ast);

                if let Some(end) = prev_end {
                    let range = end..ast.span().start.offset;
                    text.insert_tag(tagger, range, id.to_tag(0));
                }

                prev_end = Some(ast.span().end.offset);
            }
        }
        Concat(concat) => {
            for ast in concat.asts.iter() {
                tag_from_ast(tagger, text, ast);
            }
        }
    }
}

#[doc(hidden)]
pub mod private_exports {
    pub use duat_core::{context::Handle, data::Pass, form, text::Builder, ui::PushSpecs};
    pub use format_like::format_like;
}
