use std::{
    marker::PhantomData,
    sync::{LazyLock, Mutex},
};

use duat_core::{prelude::*, text::Searcher, ui::PrintInfo};

use crate::{
    hooks::{SearchPerformed, SearchUpdated},
    modes::{Prompt, PromptMode, RunCommands},
};

static SEARCH: Mutex<String> = Mutex::new(String::new());
static PAGER_TAGGER: LazyLock<Tagger> = LazyLock::new(Tagger::new);

/// A simple mode, meant for scrolling and searching through [`Text`]
pub struct Pager<W: Widget>(PhantomData<W>);

impl<W: Widget> Pager<W> {
    /// Returns a new [`Pager`]
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<W: Widget> Mode for Pager<W> {
    type Widget = W;

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget>) {
        use KeyCode::*;
        match (key, duat_core::mode::alt_is_reverse()) {
            (key!(Char('j') | Down), _) => handle.scroll_ver(pa, 1),
            (key!(Char('J')) | key!(Down, KeyMod::SHIFT), _) => handle.scroll_ver(pa, i32::MAX),
            (key!(Char('k') | Up), _) => handle.scroll_ver(pa, -1),
            (key!(Char('K')) | key!(Down, KeyMod::SHIFT), _) => handle.scroll_ver(pa, i32::MIN),
            (key!(Char('/')), _) => mode::set(PagerSearch::new(pa, &handle, true)),
            (key!(Char('/'), KeyMod::ALT), true) | (key!(Char('?')), false) => {
                mode::set(PagerSearch::new(pa, &handle, false));
            }
            (key!(Char('n')), _) => {
                let se = SEARCH.lock().unwrap();

                let (point, _) = handle.start_points(pa);

                let text = handle.read(pa).text();
                let Some([point, _]) = text.search_fwd(&*se, point..).unwrap().next() else {
                    context::error!("[a]{se}[] was not found");
                    return;
                };

                handle.scroll_to_points(pa, point);
            }
            (key!(Char('n'), KeyMod::ALT), true) | (key!(Char('N')), false) => {
                let se = SEARCH.lock().unwrap();

                let (point, _) = handle.start_points(pa);

                let text = handle.read(pa).text();
                let Some([point, _]) = text.search_rev(&*se, ..point).unwrap().next() else {
                    context::error!("[a]{se}[] was not found");
                    return;
                };

                handle.scroll_to_points(pa, point);
            }
            (key!(Esc), _) => mode::reset::<File>(),
            (key!(Char(':')), _) => mode::set(RunCommands::new()),
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

    fn update(&mut self, pa: &mut Pass, mut text: Text, _: &Area) -> Text {
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

                for [start, end] in searcher.search_fwd(parts.bytes, ..) {
                    parts.tags.insert(*PAGER_TAGGER, start..end, id.to_tag(0));
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

    fn before_exit(&mut self, pa: &mut Pass, text: Text, _: &Area) {
        match Searcher::new(text.to_string()) {
            Ok(mut se) => {
                let (point, _) = self.handle.start_points(pa);
                if self.is_fwd {
                    let Some([point, _]) =
                        se.search_fwd(self.handle.read(pa).text(), point..).next()
                    else {
                        context::error!("[a]{}[] was not found", text.to_string());
                        return;
                    };

                    self.handle.scroll_to_points(pa, point);
                } else {
                    let Some([point, _]) =
                        se.search_rev(self.handle.read(pa).text(), ..point).next()
                    else {
                        context::error!("[a]{}[] was not found", text.to_string());
                        return;
                    };

                    self.handle.scroll_to_points(pa, point);
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

                context::error!(target: "pager search", "{err}")
            }
        }
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]pager search").build()
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
