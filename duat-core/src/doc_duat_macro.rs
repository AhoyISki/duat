/// A macro to create a mock version of a `duat` dependency
///
/// You should use this macro in order to add testable documentation
/// to your plugins for `duat`. This is necessary because plugins
/// depend on `duat-core`, and as such, they can't call `use
/// duat::prelude::*` at the start.
///
/// Instead, what you should do is quite simple:
///
/// ```rust
/// duat_core::doc_duat!(duat); // Prepend a # before this line in order to hide it.
/// use duat::prelude::*;
///
/// // ...Rest of your example containing duat specific stuff.
/// ```
///
/// The test will compile as though you had access to `duat`, even
/// when you don't.
#[doc(hidden)]
#[macro_export]
macro_rules! doc_duat {
    ($duat:ident) => { $crate::doc_duat!(@inner $duat, $); };
    (@inner $duat:ident, $dol:tt) => {
        #[allow(unused_imports, dead_code, clippy::extra_unused_type_parameters)]
        mod $duat {
            pub mod prelude {
                use std::process::Output;

                pub use super::{
                    doc_cmd as cmd, doc_cursor as cursor,
                    doc_hook::{
                        self as hook, ColorSchemeSet, ConfigLoaded, ConfigUnloaded, ExitedDuat,
                        BufferWritten, FocusedOn, FormSet, KeysSent, KeysSentTo, ModeCreated,
                        ModeSwitched, SearchPerformed, SearchUpdated, UnfocusedFrom, WidgetCreated,
                        WindowCreated,
                    },
                    doc_mode::{self, Mode, User, alias, map},
                    doc_print as print,
                    doc_state::*,
                    doc_ui::*,
                    doc_widgets::*,
                };
                pub use $crate::{
                    Plugin, clipboard, context,
                    data::{self, Pass, RwData},
                    file,
                    form::{self, CursorShape, Form},
                    text::{
                        self, AlignCenter, AlignLeft, AlignRight, Builder, Conceal, Ghost, Spacer,
                        Tagger, Text, txt,
                    },
                    ui::{self, Area as AreaTrait, Widget, WidgetCfg},
                };

				#[macro_export]
                macro_rules! setup_duat {
                    ($dol setup:ident) => {
                        #[unsafe(no_mangle)]
                        fn run() {
                            $dol setup();
                        }
                    }
                }

                pub fn plug<P: $crate::Plugin<Ui>>(_: P) {}

                pub fn exec(_: impl ToString) -> Option<Output> {
                    None
                }

                #[doc(hidden)]
                pub fn plug_inner(_: impl Plugin<Ui>) {}

                pub type Handle<W> = $crate::context::Handle<W, Ui>;

                #[doc(hidden)]
                pub trait DocBufferType {
                    fn filetype(&self) -> Option<&'static str> {
                        Some("This code is only meant for documentation!")
                    }
                }

                impl DocBufferType for Buffer {}

                impl DocBufferType for $crate::buffer::BufferCfg<Ui> {}

                impl DocBufferType for String {}

                impl DocBufferType for &'_ str {}

                impl DocBufferType for std::path::PathBuf {}

                impl DocBufferType for &'_ std::path::Path {}

                #[doc(hidden)]
                pub trait DocPassBufferType {
                    fn filetype(&self, _: &Pass) -> Option<&'static str> {
                        Some("This code is only meant for documentation!")
                    }
                }

                impl DocPassBufferType for RwData<Buffer> {}

                impl DocPassBufferType for Handle<Buffer> {}

                impl DocPassBufferType for $crate::context::CurBuffer<Ui> {}
            }

            pub mod doc_cmd {
                pub use $crate::cmd::*;
                use super::doc_ui::Ui;

                pub type Handles<'a, W> = $crate::cmd::Handles<'a, W, Ui>;
            }

            pub mod doc_cursor {
                pub use $crate::form::{
                    extra_cursor as get_extra, main_cursor as get_main,
                    set_extra_cursor as set_extra, set_main_cursor as set_main,
                    unset_cursors as unset, unset_extra_cursor as unset_extra,
                    unset_main_cursor as unset_main,
                };
            }

            mod doc_filetype {
                use super::doc_ui::Ui;
                use $crate::{data::Pass, buffer::Buffer};

                pub trait BufferType {
                    fn filetype(&self) -> Option<&'static str> { Some("") }
                }
                impl BufferType for Buffer<Ui> {}

                pub trait HandleBufferType {
                    fn filetype(&self, _: &Pass) -> Option<&'static str> { Some("") }
                }
                impl HandleBufferType for $crate::context::Handle<Buffer<Ui>, Ui> {}
            }

            pub mod doc_hook {
                pub use $crate::hook::*;
                use super::doc_ui::Ui;
                use $crate::{data::Pass};

                pub fn add<H: HookAlias<Ui, impl HookDummy>>(
                    _: impl FnMut(&mut Pass, H::Input<'_>) -> H::Output + 'static,
                ) {
                }

                pub fn add_grouped<H: HookAlias<Ui, impl HookDummy>>(
                    _: impl Into<InnerGroupId>,
                    _: impl FnMut(&mut Pass, H::Input<'_>) -> H::Output + 'static,
                ) {
                }

                pub fn add_once<H: HookAlias<Ui, impl HookDummy>>(
                    _: impl FnMut(&mut Pass, H::Input<'_>) -> H::Output + 'static,
                ) {
                }

                pub type WidgetCreated<W> = $crate::hook::WidgetCreated<W, Ui>;
                pub type WindowCreated = $crate::hook::WindowCreated<Ui>;
                pub type FocusedOn<W> = $crate::hook::FocusedOn<W, Ui>;
                pub type UnfocusedFrom<W> = $crate::hook::UnfocusedFrom<W, Ui>;
                pub type KeySentTo<W> = $crate::hook::KeysSentTo<W, Ui>;
                pub type ModeCreated<M> = $crate::hook::ModeCreated<M, Ui>;

                pub struct SearchUpdated((String, String));
                impl Hookable for SearchUpdated {
                    type Input<'h> = (&'h str, &'h str);
                    fn get_input(&mut self) -> Self::Input<'_> { (&self.0.0, &self.0.1) }
                }
                pub struct SearchPerformed(String);
                impl Hookable for SearchPerformed {
                    type Input<'h> = &'h str;
                    fn get_input(&mut self) -> Self::Input<'_> { &self.0 }
                }
            }

            pub mod doc_mode {
                pub use $crate::{mode::{self, *}, ui::Widget};
                use super::{doc_ui::Ui};

                pub fn set_default(mode: impl Mode<Ui>) { mode::set_default(mode); }
                pub fn set(mode: impl Mode<Ui>) { mode::set(mode); }
                pub fn reset<W: Widget<Ui>>() { mode::reset::<W, Ui>(); }
                pub fn map<M: Mode<Ui>>(_: &str, _: impl AsGives<Ui>) {}
                pub fn alias<M: Mode<Ui>>(_: &str, _: impl AsGives<Ui>) {}
            }

            pub mod doc_print {
                pub fn no_wrapping() {}
                pub fn wrap_on_edge() {}
                pub fn wrap_on_words() {}
                pub fn wrap_on_cap(_: u8) {}
                pub fn indent_wraps() {}
                pub fn tabstop(_: u8) {}
                pub fn new_line(_: char) {}
                pub fn trailing_new_line(_: char) {}
                pub fn scrolloff(_: u8, _: u8) {}
                #[macro_export]
                macro_rules! word_chars {
                    ($dol($dol w_chars:tt)+) => {
                        set_word_chars(w_chars!($dol($dol w_chars)+));
                    }
                }
                pub fn set_word_chars(_: $crate::cfg::WordChars) {}
            }

            mod doc_state {
                use $crate::mode::KeyEvent;
                use $crate::{data::{DataMap, Pass, RwData}, buffer::Buffer, text::Text, ui::Ui};

                pub fn name_txt(_: &Buffer<impl Ui>) -> Text { Text::default() }
                pub fn mode_name(pa: &Pass) -> DataMap<&'static str, &'static str> {
                    RwData::default().map(pa, |_| "")
                }
                pub fn mode_txt(pa: &Pass) -> DataMap<&'static str, Text> {
                    RwData::default().map(pa, |_| Text::new())
                }
                pub fn main_byte(_: &Buffer<impl Ui>) -> usize { 0 }
                pub fn main_char(_: &Buffer<impl Ui>) -> usize { 0 }
                pub fn main_line(_: &Buffer<impl Ui>) -> usize { 0 }
                pub fn main_col<U: Ui>(_: &Buffer<U>, _: &U::Area) -> usize { 0 }
                pub fn main_dwcol<U: Ui>(_: &Buffer<U>, _: &U::Area) -> usize { 0 }
                pub fn main_txt<U: Ui>(_: &Buffer<U>, _: &U::Area) -> Text { Text::default() }
                pub fn selections(_: &Buffer<impl Ui>) -> usize { 0 }
                pub fn sels_txt(_: &Buffer<impl Ui>) -> Text { Text::default() }
                pub fn cur_map_txt(pa: &Pass) -> DataMap<(Vec<KeyEvent>, bool), Text> {
                    RwData::default().map(pa, |_| Text::new())
                }
                pub fn last_key() -> RwData<String> { RwData::default() }
            }

            mod doc_ui {
                use $crate::{
                    cfg::PrintCfg, form::Painter, text::{
                        FwdIter, Item, Point, RevIter, Text, TwoPoints
                    },
                    ui::{Caret, Constraint, MutArea, PushSpecs, SpawnSpecs}
                };

                #[derive(Clone, Default, Debug)]
                pub struct Ui;
                impl $crate::ui::Ui for Ui {
                    type Area = Area;
                    type MetaStatics = ();
                    fn open(_: &'static Self::MetaStatics, _: $crate::session::DuatSender) {}
                    fn close(_: &'static Self::MetaStatics) {}
                    fn new_root(
                        _: &'static Self::MetaStatics,
                        _: <Self::Area as $crate::prelude::Area>::Cache,
                    ) -> Self::Area {
                        Area
                    }
                    fn switch_window(_: &'static Self::MetaStatics, _: usize) {}
                    fn flush_layout(_: &'static Self::MetaStatics) {}
                    fn print(_: &'static Self::MetaStatics) {}
                    fn load(_: &'static Self::MetaStatics) {}
                    fn unload(_: &'static Self::MetaStatics) {}
                    fn remove_window(_: &'static Self::MetaStatics, _: usize) {}
                }

                #[derive(Clone, Copy, Default, PartialEq, Eq)]
                pub struct Area;
                impl $crate::ui::Area for Area {
                    type Cache = ();
                    type PrintInfo = ();
                    type Ui = Ui;
                    fn bisect(
                        _: MutArea<Self>,
                        _: PushSpecs,
                        _: bool,
                        _: bool,
                        _: Self::Cache,
                    ) -> (Self, Option<Self>) {
                        Default::default()
                    }
                    fn delete(_: MutArea<Self>) -> Option<Self> { None }
                    fn swap(_: MutArea<Self>, _: &Self) {}
                    fn spawn_floating(
                        _: MutArea<Self>
                        , _: SpawnSpecs
                   	) -> Result<Self, Text> { Ok(Area) }
                    fn spawn_floating_at(
                        _: MutArea<Self>,
                        _: SpawnSpecs,
                        _: impl TwoPoints,
                        _: &Text,
                        _: PrintCfg,
                    ) -> Result<Self, Text> {
                        Ok(Area)
                    }
                    fn constrain_hor(
                        &self,
                        _: impl IntoIterator<Item = Constraint>
                    ) -> Result<(), Text> {
                        Ok(())
                    }
                    fn constrain_ver(
                        &self,
                        _: impl IntoIterator<Item = Constraint>
                    ) -> Result<(), Text> {
                        Ok(())
                    }
                    fn hide(&self) -> Result<(), Text> { Ok(()) }
                    fn reveal(&self) -> Result<(), Text> { Ok(()) }
                    fn request_width_to_fit(&self, _: &str) -> Result<(), Text> { Ok(()) }
                    fn scroll_ver(&self, _: &Text, _: i32, _: PrintCfg) {}
                    fn scroll_around_point(&self, _: &Text, _: Point, _: PrintCfg) {}
                    fn scroll_to_points(&self, _: &Text, _: impl TwoPoints, _: PrintCfg) {}
                    fn set_as_active(&self) {}
                    fn print(&self, _: &mut Text, _: PrintCfg, _: Painter) {}
                    fn print_with<'a>(
                        &self,
                        _: &mut Text,
                        _: PrintCfg,
                        _: Painter,
                        _: impl FnMut(&Caret, &Item ) + 'a,
                    ) {
                    }
                    fn set_print_info(&self, _: Self::PrintInfo) {}
                    fn print_iter<'a>(
                        &self,
                        _: FwdIter<'a>,
                        _: PrintCfg,
                    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
                        [].into_iter()
                    }
                    fn rev_print_iter<'a>(
                        &self,
                        _: RevIter<'a>,
                        _: PrintCfg,
                    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
                        [].into_iter()
                    }
                    fn has_changed(&self) -> bool { false }
                    fn is_master_of(&self, _: &Self) -> bool { false }
                    fn get_cluster_master(&self) -> Option<Self> { None }
                    fn cache(&self) -> Option<Self::Cache> { None }
                    fn width(&self) -> u32 { 0 }
                    fn height(&self) -> u32 { 0 }
                    fn start_points(&self, _: &Text, _: PrintCfg) -> (Point, Option<Point>) {
                        (Point::default(), None)
                    }
                    fn end_points(&self, _: &Text, _: PrintCfg) -> (Point, Option<Point>) {
                        (Point::default(), None)
                    }
                    fn print_info(&self) -> Self::PrintInfo {}
                    fn is_active(&self) -> bool { false }
                }
            }

            mod doc_widgets {
                use std::marker::PhantomData;
                use $crate::{
                    context::{Handle, Record}, data::Pass, text::Text,
                    ui::{BuildInfo, PushSpecs, Ui, Widget, WidgetCfg}
                };

                pub struct LineNumbers<U: Ui>(Text, std::marker::PhantomData<U>);
                impl<U: Ui> Widget<U> for LineNumbers<U> {
                    type Cfg = LineNumbersOptions<U>;
                    fn update(_: &mut Pass, _: &Handle<Self, U>) {}
                    fn needs_update(&self, _: &Pass) -> bool { false }
                    fn cfg() -> Self::Cfg { LineNumbersOptions(PhantomData) }
                    fn text(&self) -> &Text { &self.0 }
                    fn text_mut(&mut self) -> &mut Text { &mut self.0 }
                    fn once() -> Result<(), Text> { Ok(()) }
                }
                pub struct LineNumbersOptions<U>(std::marker::PhantomData<U>);
                impl<U> LineNumbersOptions<U> {
                    pub fn absolute(self) -> Self { self }
                    pub fn relative(self) -> Self { self }
                    pub fn rel_abs(self) -> Self { self }
                    pub fn align_left(self) -> Self { self }
                    pub fn align_center(self) -> Self { self }
                    pub fn align_right(self) -> Self { self }
                    pub fn align_main_left(self) -> Self { self }
                    pub fn align_main_center(self) -> Self { self }
                    pub fn align_main_right(self) -> Self { self }
                    pub fn show_wraps(self) -> Self { self }
                    pub fn hide_wraps(self) -> Self { self }
                    pub fn on_the_right(self) -> Self { self }
                }
                impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
                    type Widget = LineNumbers<U>;
                    fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
                        (LineNumbers(Text::new(), PhantomData), PushSpecs::left())
                    }
                }

                pub struct StatusLine<U: Ui>(Text, std::marker::PhantomData<U>);
                impl<U: Ui> Widget<U> for StatusLine<U> {
                    type Cfg = StatusLineCfg<U>;
                    fn update(_: &mut Pass, _: &Handle<Self, U>) {}
                    fn needs_update(&self, _: &Pass) -> bool { false }
                    fn cfg() -> Self::Cfg { StatusLineCfg(PhantomData) }
                    fn text(&self) -> &Text { &self.0 }
                    fn text_mut(&mut self) -> &mut Text { &mut self.0 }
                    fn once() -> Result<(), Text> { Ok(()) }
                }
                #[derive(Clone, Copy, Default)]
                pub struct StatusLineCfg<U: Ui>(std::marker::PhantomData<U>);
                impl<U: Ui> StatusLineCfg<U> {
                    pub fn fmt(self, _: Self) -> Self { self }
                    pub fn above(self) -> Self { self }
                    pub fn below(self) -> Self { self }
                    pub fn right_ratioed(self, _: u16, _: u16) -> Self { self }
                    pub fn specs(&self) -> PushSpecs { PushSpecs::below() }
                }
                impl<U: Ui> WidgetCfg<U> for StatusLineCfg<U> {
                    type Widget = StatusLine<U>;
                    fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
                        (StatusLine(Text::new(), PhantomData), PushSpecs::left())
                    }
                }

				#[macro_export]
                macro_rules! status {
                    ($dol text:literal $dol(,$dol args:expr)* $dol(,)?) => {{
                        StatusLineCfg::default()
                    }}
                }

                pub struct PromptLine<U: Ui>(Text, std::marker::PhantomData<U>);
                impl<U: Ui> PromptLine<U> {
                    pub fn prompt_of<M>(&self) -> Option<Text> { None }
                    pub fn set_prompt<M>(&mut self, _: Text) {}
                }
                impl<U: Ui> Widget<U> for PromptLine<U> {
                    type Cfg = PromptLineCfg<U>;
                    fn update(_: &mut Pass, _: &Handle<Self, U>) {}
                    fn needs_update(&self, _: &Pass) -> bool { false }
                    fn cfg() -> Self::Cfg { PromptLineCfg(PhantomData) }
                    fn text(&self) -> &Text { &self.0 }
                    fn text_mut(&mut self) -> &mut Text { &mut self.0 }
                    fn once() -> Result<(), Text> { Ok(()) }
                }
                impl<U: Ui> WidgetCfg<U> for PromptLineCfg<U> {
                    type Widget = PromptLine<U>;
                    fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
                        (PromptLine(Text::new(), PhantomData), PushSpecs::left())
                    }
                }
                #[derive(Default)]
                pub struct PromptLineCfg<U>(PhantomData<U>);
                impl<U: Ui> PromptLineCfg<U> {
                    pub fn set_prompt<M>(self, _: Text) -> Self { self }
                    pub fn above(self) -> Self { self }
                    pub fn below(self) -> Self { self }
                    pub fn hidden(self) -> Self { self }
                    pub fn left_ratioed(self, _: u16, _: u16) -> Self { self }
                }

                pub struct Notifications<U: Ui>(Text, std::marker::PhantomData<U>);
                impl<U: Ui> Widget<U> for Notifications<U> {
                    type Cfg = NotificationsCfg<U>;
                    fn update(_: &mut Pass, _: &Handle<Self, U>) {}
                    fn needs_update(&self, _: &Pass) -> bool { false }
                    fn cfg() -> Self::Cfg { NotificationsCfg(PhantomData) }
                    fn text(&self) -> &Text { &self.0 }
                    fn text_mut(&mut self) -> &mut Text { &mut self.0 }
                    fn once() -> Result<(), Text> { Ok(()) }
                }
                impl<U: Ui> WidgetCfg<U> for NotificationsCfg<U> {
                    type Widget = Notifications<U>;
                    fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
                        (Notifications(Text::new(), PhantomData), PushSpecs::left())
                    }
                }
                #[derive(Default)]
                pub struct NotificationsCfg<U>(PhantomData<U>);
                impl<U: Ui> NotificationsCfg<U> {
                    pub fn fmt<T: Into<Text>>(
                        self,
                        _: impl FnMut(Record) -> Option<T> + 'static
                    ) -> Self {
                        self
                    }
                    pub fn with_mask(self, _: impl FnMut(Record) -> &'static str + 'static) -> Self { self }
                }


                pub struct LogBook<U: Ui>(Text, std::marker::PhantomData<U>);
                impl<U: Ui> Widget<U> for LogBook<U> {
                    type Cfg = LogBookCfg<U>;
                    fn update(_: &mut Pass, _: &Handle<Self, U>) {}
                    fn needs_update(&self, _: &Pass) -> bool { false }
                    fn cfg() -> Self::Cfg { LogBookCfg(PhantomData) }
                    fn text(&self) -> &Text { &self.0 }
                    fn text_mut(&mut self) -> &mut Text { &mut self.0 }
                    fn once() -> Result<(), Text> { Ok(()) }
                }
                impl<U: Ui> WidgetCfg<U> for LogBookCfg<U> {
                    type Widget = LogBook<U>;
                    fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
                        (LogBook(Text::new(), PhantomData), PushSpecs::left())
                    }
                }
                #[derive(Default)]
                pub struct LogBookCfg<U>(PhantomData<U>);
                impl<U: Ui> LogBookCfg<U> {
                    pub fn open_by_default(self) -> Self { self }
                    pub fn keep_open_on_unfocus(self) -> Self { self }
                    pub fn fmt(
                        self,
                        _: impl FnMut(Record) -> Option<Text> + 'static
                    ) -> Self { self }
                    pub fn on_the_right(self) -> Self { self }
                    pub fn on_the_left(self) -> Self { self }
                    pub fn above(self) -> Self { self }
                }

                pub type Buffer = $crate::buffer::Buffer<super::doc_ui::Ui>;
            }
        }
    }
}
