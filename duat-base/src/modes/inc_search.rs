//! Utilities for incremental search in Duat
//!
//! This specific feature of Duat is kind of split across this crate
//! and [`duat-core`], since some of the low level features (like
//! spawning a bazillion [`Cursor`]s) were only possible with access
//! to private things.
//!
//! [`duat-core`]: duat_core
//! [`Cursor`]: duat_core::mode::Cursor
use std::sync::{LazyLock, Once};

use duat_core::{
    buffer::Buffer,
    context::{self, Handle},
    data::Pass,
    form, hook,
    text::{Tagger, Text, txt},
    ui::{PrintInfo, RwArea},
};

use crate::{
    hooks::{SearchPerformed, SearchUpdated},
    modes::{Prompt, PromptMode},
};

static TAGGER: LazyLock<Tagger> = LazyLock::new(Tagger::new);

/// The [`PromptMode`] that makes use of [`IncSearcher`]s
///
/// In order to make use of incremental search, you'd do something
/// like this:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::modes::{IncSearch, SearchFwd};
/// use duat::prelude::*;
///
/// #[derive(Clone)]
/// struct Emacs;
///
/// impl Mode for Emacs {
///     type Widget = Buffer;
///
///     fn send_key(&mut self, pa: &mut Pass, event: KeyEvent, handle: Handle) {
///         match event {
///             ctrl!('s') => _ = mode::set(pa, IncSearch::new(SearchFwd)),
///             other_keys_oh_god => todo!(),
///         }
///     }
/// }
/// ```
pub struct IncSearch<I: IncSearcher> {
    inc: I,
    orig: Option<(duat_core::mode::Selections, PrintInfo)>,
    prev: String,
}

impl<I: IncSearcher> Clone for IncSearch<I> {
    fn clone(&self) -> Self {
        Self {
            inc: self.inc.clone(),
            orig: self.orig.clone(),
            prev: self.prev.clone(),
        }
    }
}

impl<I: IncSearcher> IncSearch<I> {
    /// Returns a [`Prompt`] with [`IncSearch<I>`] as its
    /// [`PromptMode`]
    #[allow(clippy::new_ret_no_self)]
    pub fn new(inc: I) -> Prompt {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            form::set_weak("regex.error", "accent.error");
            form::set_weak("regex.operator", "operator");
            form::set_weak("regex.class", "constant");
            form::set_weak("regex.bracket", "punctuation.bracket");
        });
        Prompt::new(Self { inc, orig: None, prev: String::new() })
    }
}

impl<I: IncSearcher> PromptMode for IncSearch<I> {
    type ExitWidget = Buffer;

    fn update(&mut self, pa: &mut Pass, mut text: Text, _: &RwArea) -> Text {
        let (orig_selections, orig_print_info) = self.orig.as_ref().unwrap();
        text.remove_tags(*TAGGER, ..);

        let handle = context::current_buffer(pa);

        if text == self.prev {
            return text;
        } else {
            let prev = std::mem::replace(&mut self.prev, text.to_string());
            hook::queue(SearchUpdated((prev, self.prev.clone())));
        }

        let pat = text.to_string();

        match regex_syntax::parse(&pat) {
            Ok(_) => {
                handle.area().set_print_info(pa, orig_print_info.clone());
                let buffer = handle.write(pa);
                *buffer.selections_mut() = orig_selections.clone();

                let ast = regex_syntax::ast::parse::Parser::new()
                    .parse(&text.to_string())
                    .unwrap();

                crate::tag_from_ast(*TAGGER, &mut text, &ast);

                if !text.is_empty() {
                    self.inc.search(pa, &pat, handle);
                }
            }
            Err(err) => {
                let regex_syntax::Error::Parse(err) = err else {
                    unreachable!("As far as I can tell, regex_syntax has goofed up");
                };

                let span = err.span();
                let id = form::id_of!("regex.error");

                text.insert_tag(*TAGGER, span.start.offset..span.end.offset, id.to_tag(0));
            }
        }

        text
    }

    fn on_switch(&mut self, pa: &mut Pass, text: Text, _: &RwArea) -> Text {
        let handle = context::current_buffer(pa);

        self.orig = Some((
            handle.read(pa).selections().clone(),
            handle.area().get_print_info(pa),
        ));

        text
    }

    fn before_exit(&mut self, _: &mut Pass, text: Text, _: &RwArea) {
        if !text.is_empty() {
            let pat = text.to_string();
            if let Err(err) = regex_syntax::parse(&pat) {
                let regex_syntax::Error::Parse(err) = err else {
                    unreachable!("As far as I can tell, regex_syntax has goofed up");
                };

                let range = err.span().start.offset..err.span().end.offset;
                let err = txt!(
                    "[a]{:?}, \"{}\"[prompt.colon]:[] {}",
                    range,
                    text.strs(range).unwrap(),
                    err.kind()
                );

                context::error!("{err}")
            } else {
                hook::queue(SearchPerformed(pat));
            }
        }
    }

    fn prompt(&self) -> Text {
        txt!("{}", self.inc.prompt())
    }
}

/// An abstraction trait used to handle incremental search
///
/// This trait can be used for various ways of interpreting what
/// incremental search should do, right now, these are the
/// implementations of [`IncSearcher`]:
///
/// - [`SearchFwd`]: In each cursor, searches forward for the match
/// - [`SearchRev`]: In each cursor, searches backwards for the match
/// - [`ExtendFwd`]: In each cursor, extends forward for the match
/// - [`ExtendRev`]: In each cursor, extends backwards for the match
///
/// Here is how you can implement this trait yourself:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::modes::IncSearcher;
/// use duat::prelude::*;
///
/// #[derive(Clone, Copy)]
/// struct SearchAround;
///
/// impl IncSearcher for SearchAround {
///     fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
///         handle.edit_all(pa, |mut c| {
///             c.set_caret_on_end();
///             let Some(e_range) = c.search(pat).from_caret().next() else {
///                 return;
///             };
///
///             c.set_caret_on_start();
///             let Some(s_range) = c.search(pat).to_caret().next_back() else {
///                 return;
///             };
///
///             c.move_to(s_range.start..e_range.end)
///         });
///     }
///
///     fn prompt(&self) -> Text {
///         txt!("[prompt]search around")
///     }
/// }
/// ```
///
/// There are more advanced implementations in the [`duat-kak`] crate
///
/// [`duat-kak`]: https://docs.rs/duat-kak
pub trait IncSearcher: Clone + Send + 'static {
    /// Performs an incremental search with a `pat`
    ///
    /// Using this `pat` inside any searching method is guaranteed not
    /// to panic.
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>);

    /// What prompt to show in the [`PromptLine`]
    ///
    /// [`PromptLine`]: crate::widgets::PromptLine
    fn prompt(&self) -> Text;
}

/// Searches forward on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct SearchFwd;

impl IncSearcher for SearchFwd {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        handle.edit_all(pa, |mut c| {
            if let Some(range) = {
                c.search(pat)
                    .from_caret_excl()
                    .next()
                    .or_else(|| c.search(pat).to_caret().next())
            } {
                c.move_to(range)
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]search")
    }
}

/// Searches backwards on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct SearchRev;

impl IncSearcher for SearchRev {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        handle.edit_all(pa, |mut c| {
            if let Some(range) = {
                c.search(pat)
                    .to_caret()
                    .next_back()
                    .or_else(|| c.search(pat).from_caret_excl().next_back())
            } {
                c.move_to(range)
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]rev search")
    }
}

/// Extends forward on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct ExtendFwd;

impl IncSearcher for ExtendFwd {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        handle.edit_all(pa, |mut c| {
            if let Some(range) = {
                c.search(pat)
                    .from_caret_excl()
                    .next()
                    .or_else(|| c.search(pat).to_caret().next())
            } {
                c.set_anchor_if_needed();
                c.move_to(range)
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]search (extend)")
    }
}

/// Extends backwards on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct ExtendRev;

impl IncSearcher for ExtendRev {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        handle.edit_all(pa, |mut c| {
            if let Some(range) = {
                c.search(pat)
                    .to_caret()
                    .next_back()
                    .or_else(|| c.search(pat).from_caret_excl().next_back())
            } {
                c.set_anchor_if_needed();
                c.move_to(range)
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]rev search (extend)")
    }
}
