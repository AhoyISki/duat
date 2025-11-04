//! A simple mode for scrolling through widgets
//!
//! This mode takes in a `W` argument, meaning it can act on any
//! [`Widget`]. In Duat, it is normally used with the [`LogBook`], in
//! order to allow scrolling through the logs.
//!
//! It is also capable of searching through the [`Text`], via a
//! companion [`PagerSearch`] mode.
use std::{
    marker::PhantomData,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    buffer::Buffer,
    context::{self, Handle},
    data::Pass,
    form, hook,
    mode::{self, KeyEvent, Mode, alt, event, shift},
    text::{Searcher, Tagger, Text, txt},
    ui::{PrintInfo, RwArea, Widget},
};

use crate::{
    hooks::{SearchPerformed, SearchUpdated},
    modes::{Prompt, PromptMode, RunCommands},
    widgets::LogBook,
};

static SEARCH: Mutex<String> = Mutex::new(String::new());
static PAGER_TAGGER: LazyLock<Tagger> = LazyLock::new(Tagger::new);

/// A simple mode, meant for scrolling and searching through [`Text`]
pub struct Pager<W: Widget = LogBook>(PhantomData<W>);

impl<W: Widget> Pager<W> {
    /// Returns a new [`Pager`]
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<W: Widget> Mode for Pager<W> {
    type Widget = W;

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget>) {
        use duat_core::mode::KeyCode::*;
        match (key, duat_core::mode::alt_is_reverse()) {
            (event!(Char('j') | Down), _) => handle.scroll_ver(pa, 1),
            (event!(Char('J')) | shift!(Down), _) => handle.scroll_ver(pa, i32::MAX),
            (event!(Char('k') | Up), _) => handle.scroll_ver(pa, -1),
            (event!('K') | shift!(Down), _) => handle.scroll_ver(pa, i32::MIN),
            (event!('/'), _) => mode::set(PagerSearch::new(pa, &handle, true)),
            (alt!('/'), true) | (event!('?'), false) => {
                mode::set(PagerSearch::new(pa, &handle, false));
            }
            (event!('n'), _) => {
                let se = SEARCH.lock().unwrap();

                let point = handle.start_points(pa).real;

                let text = handle.read(pa).text();
                let Some(r) = text.search_fwd(&*se, point..).unwrap().next() else {
                    context::error!("[a]{se}[] was not found");
                    return;
                };

                let point = handle.text(pa).point_at_byte(r.start);
                handle.scroll_to_points(pa, point.to_two_points_after());
            }
            (alt!('n'), true) | (event!('N'), false) => {
                let se = SEARCH.lock().unwrap();

                let point = handle.start_points(pa).real;

                let text = handle.read(pa).text();
                let Some(r) = text.search_rev(&*se, ..point).unwrap().next() else {
                    context::error!("[a]{se}[] was not found");
                    return;
                };

                let point = handle.text(pa).point_at_byte(r.start);
                handle.scroll_to_points(pa, point.to_two_points_after());
            }
            (event!(Esc), _) => mode::reset::<Buffer>(),
            (event!(':'), _) => mode::set(RunCommands::new()),
            _ => {}
        }
    }
}

impl<W: Widget> Clone for Pager<W> {
    fn clone(&self) -> Self {
        Self(PhantomData)
    }
}

impl<W: Widget> Default for Pager<W> {
    fn default() -> Self {
        Self::new()
    }
}

/// The searcher [`PromptMode`] for a [`Pager`]ed [`Widget`]
pub struct PagerSearch<W: Widget> {
    is_fwd: bool,
    prev: String,
    orig: PrintInfo,
    handle: Handle<W>,
}

impl<W: Widget> PagerSearch<W> {
    #[allow(clippy::new_ret_no_self)]
    fn new(pa: &Pass, handle: &Handle<W>, is_fwd: bool) -> Prompt {
        Prompt::new(Self {
            is_fwd,
            prev: String::new(),
            orig: handle.area().get_print_info(pa),
            handle: handle.clone(),
        })
    }
}

impl<W: Widget> PromptMode for PagerSearch<W> {
    type ExitWidget = W;

    fn update(&mut self, pa: &mut Pass, mut text: Text, _: &RwArea) -> Text {
        let tagger = *PAGER_TAGGER;
        text.remove_tags(tagger, ..);

        if text == self.prev.as_str() {
            return text;
        } else {
            let prev = std::mem::replace(&mut self.prev, text.to_string());
            hook::queue(SearchUpdated((prev, self.prev.clone())));
        }

        match Searcher::new(text.to_string()) {
            Ok(mut searcher) => {
                self.handle.area().set_print_info(pa, self.orig.clone());
                self.handle
                    .write(pa)
                    .text_mut()
                    .remove_tags(*PAGER_TAGGER, ..);

                let ast = regex_syntax::ast::parse::Parser::new()
                    .parse(&text.to_string())
                    .unwrap();

                crate::tag_from_ast(*PAGER_TAGGER, &mut text, &ast);

                let mut parts = self.handle.write(pa).text_mut().parts();
                let id = form::id_of!("pager.search");

                for range in searcher.search_fwd(parts.bytes, ..) {
                    parts.tags.insert(*PAGER_TAGGER, range, id.to_tag(0));
                }
            }
            Err(err) => {
                let regex_syntax::Error::Parse(err) = *err else {
                    unreachable!("As far as I can tell, regex_syntax has goofed up");
                };

                let span = err.span();
                let id = form::id_of!("regex.error");

                text.insert_tag(
                    *PAGER_TAGGER,
                    span.start.offset..span.end.offset,
                    id.to_tag(0),
                );
            }
        }

        text
    }

    fn before_exit(&mut self, pa: &mut Pass, text: Text, _: &RwArea) {
        match Searcher::new(text.to_string()) {
            Ok(mut se) => {
                let point = self.handle.start_points(pa).real;
                if self.is_fwd {
                    let Some(range) = se.search_fwd(self.handle.read(pa).text(), point..).next()
                    else {
                        context::error!("[a]{}[] was not found", text.to_string());
                        return;
                    };

                    let start = self.handle.text(pa).point_at_byte(range.start);
                    self.handle
                        .scroll_to_points(pa, start.to_two_points_after());
                } else {
                    let Some(range) = se.search_rev(self.handle.read(pa).text(), ..point).next()
                    else {
                        context::error!("[a]{}[] was not found", text.to_string());
                        return;
                    };

                    let start = self.handle.text(pa).point_at_byte(range.start);
                    self.handle
                        .scroll_to_points(pa, start.to_two_points_after());
                }

                *SEARCH.lock().unwrap() = text.to_string();
                hook::queue(SearchPerformed(text.to_string()));
            }
            Err(err) => {
                let regex_syntax::Error::Parse(err) = *err else {
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
            }
        }
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]pager search")
    }

    fn return_handle(&self) -> Option<Handle<dyn Widget>> {
        Some(self.handle.clone().to_dyn())
    }
}

impl<W: Widget> Clone for PagerSearch<W> {
    fn clone(&self) -> Self {
        Self {
            is_fwd: self.is_fwd,
            prev: self.prev.clone(),
            orig: self.orig.clone(),
            handle: self.handle.clone(),
        }
    }
}
