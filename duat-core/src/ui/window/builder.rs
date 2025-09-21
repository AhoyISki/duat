//! Builder helpers for Duat
//!
//! These builders are used primarily to push widgets to either a
//! [`File`] or a window. They offer a convenient way to make massive
//! changes to the layout, in a very intuitive "inner-outer" order,
//! where widgets get pushed to a "main area" which holds all of the
//! widgets that were added to that helper.
//!
//! This pushing to an unnamed, but known area makes the syntax for
//! layout modification fairly minimal, with minimal boilerplate.
//!
//! [`File`]: crate::file::File
use super::{AreaId, GetAreaId, Ui, Widget, WidgetCfg};
use crate::{
    context::{self, Handle},
    data::Pass,
    file::File,
    hook::{self, WidgetCreated},
};

/// A struct for determining what the how the Ui should be constructed
///
/// This struct is used from the [`WidgetCreated`] [hook] every time a
/// new [`Widget`] is created, in order to let you push other
/// [`Widget`]s around that one. This makes the extension of Duat's
/// interface quite friendly.
///
/// It will also be called once per window, from the [`WindowCreated`]
/// [hook], letting you place [`Widget`]s "around" the common [`File`]
/// area, at the edges of the screen.
///
/// # Examples
///
/// Here's some examples involving one of the main ways this will be
/// done, in the [`File`] [`Widget`]:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<File>(|_, (cfg, builder)| {
///         builder.push(LineNumbers::cfg());
///         cfg
///     });
/// }
/// ```
///
/// In the example above, I pushed a [`LineNumbers`] widget to the
/// [`File`]. By default, this widget will go on the left, but you can
/// change that:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<File>(|_, (cfg, builder)| {
///         let line_numbers_cfg = LineNumbers::cfg().relative().on_the_right();
///         builder.push(line_numbers_cfg);
///         cfg
///     });
/// }
/// ```
///
/// Note that I also made another change to the widget, it will now
/// show relative numbers, instead of absolute, like it usually does.
///
/// By default, there already exists a [hook group] that adds widgets
/// to a file, the `"FileWidgets"` group. If you want to get rid of
/// this group in order to create your own widget layout, you should
/// use [`hook::remove`]:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::remove("FileWidgets");
///     hook::add::<File>(|_, (cfg, builder)| {
///         builder.push(LineNumbers::cfg().relative().on_the_right());
///         // Push a StatusLine to the bottom.
///         builder.push(StatusLine::cfg());
///         // Push a PromptLine to the bottom.
///         builder.push(PromptLine::cfg());
///         cfg
///     });
/// }
/// ```
///
/// [`File`]: crate::file::File
/// [`LineNumbers`]: https://crates.io/duat-utils/latest/duat_utils/wigets/struct.LineNumbers.html
/// [hook group]: crate::hook::add_grouped
/// [`hook::remove`]: crate::hook::remove
/// [`WindowCreated`]: crate::hook::WindowCreated
pub struct UiBuilder<U: Ui> {
    win: usize,
    widget_id: Option<AreaId>,
    master_id: Option<AreaId>,
    main: Option<Handle<dyn Widget<U>, U>>,
    builder_fn: Option<BuilderFn<U>>,
}

impl<U: Ui> UiBuilder<U> {
    /// Returns a new [`UiBuilder`], with no main [`Widget`]
    ///
    /// This is used when creating
    pub(super) fn new_main(win: usize, widget_id: AreaId) -> Self {
        Self {
            win,
            widget_id: Some(widget_id),
            master_id: None,
            main: None,
            builder_fn: None,
        }
    }

    /// A [`UiBuilder`] specific to [`Window`] creation, has no
    /// `widget_id`
    ///
    /// [`Window`]: super::Window
    pub(super) fn new_window(win: usize) -> Self {
        Self {
            win,
            widget_id: None,
            master_id: None,
            main: None,
            builder_fn: None,
        }
    }

    /// Returns a new [`UiBuilder`] with a predefined main [`Widget`]
    fn pushed(
        win: usize,
        widget_id: AreaId,
        master_id: AreaId,
        main: Option<Handle<dyn Widget<U>, U>>,
    ) -> Self {
        Self {
            win,
            widget_id: Some(widget_id),
            master_id: Some(master_id),
            main,
            builder_fn: None,
        }
    }

    /// Pushes a widget to the main area of the [`UiBuilder`]
    ///
    /// If this [`Widget`] is being pushed to a [`File`]'s group,
    /// then this [`Widget`] will be included in that [`File`]'s
    /// group. This means that, if that [`File`] is moved around
    /// or deleted, this [`Widget`] (and all others in its group)
    /// will follow suit.
    ///
    /// When you push a [`Widget`], it is placed on an edge of the
    /// area, and a new parent area may be created to hold both
    /// widgets. If created, that new area will be used for pushing
    /// widgets in the future.
    ///
    /// This means that, if you push widget *A* to the left, then you
    /// push widget *B* to the bottom, you will get this layout:
    ///
    /// ```text
    /// ╭───┬──────────╮
    /// │   │          │
    /// │ A │   File   │
    /// │   │          │
    /// ├───┴──────────┤
    /// │      B       │
    /// ╰──────────────╯
    /// ```
    ///
    /// Here's an example of such a layout:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::remove("FileWidgets");
    ///     hook::add::<File>(|_, (cfg, builder)| {
    ///         builder.push(LineNumbers::cfg().rel_abs());
    ///         builder.push(status!("{name_txt} {selections_txt} {main_txt}"));
    ///         cfg
    ///     });
    /// }
    /// ```
    ///
    /// In this case, each file will have [`LineNumbers`] with
    /// relative/absolute numbering, and a [`StatusLine`] showing
    /// the file's name, how many selections are in it, and its main
    /// selection.
    ///
    /// [`File`]: crate::file::File
    /// [`LineNumbers`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
    /// [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
    /// [`WindowCreated`]: crate::hook::WindowCreated
    pub fn push<Cfg: WidgetAlias<U, impl BuilderDummy>>(&mut self, widget: Cfg) -> AreaId {
        widget.push_alias(&mut RawUiBuilder(self))
    }

    /// Pushes a widget to a specific [`AreaId`]
    ///
    /// This method can be used to get some more advanced layouts,
    /// where you have multiple widgets parallel to each other, yet on
    /// the same edge.
    ///
    /// One of the main ways in which this is used is when using a
    /// hidden [`Notifications`] widget alongside the [`PromptLine`]
    /// widget.
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::remove("FileWidgets");
    ///     hook::add::<File>(|_, (cfg, builder)| {
    ///         builder.push(LineNumbers::cfg());
    ///
    ///         let status_cfg = status!("{name_txt} {selections_txt} {main_txt}");
    ///         let child = builder.push(status_cfg);
    ///         let prompt_cfg = PromptLine::cfg().left_ratioed(3, 5);
    ///         let child = builder.push_to(child, prompt_cfg);
    ///         builder.push_to(child, Notifications::cfg());
    ///         cfg
    ///     });
    /// }
    /// ```
    ///
    /// Pushing directly to the [`PromptLine`]'s [`U::Area`] means
    /// that they'll share a parent that holds only them. This is
    /// actually done in a more convenient way through the
    /// [`FooterWidgets`] widget group from [`duat-utils`]
    ///
    /// [`File`]: crate::file::File
    /// [`Notifications`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.Notifications.html
    /// [`PromptLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
    /// [`U::Area`]: Ui::Area
    /// [hook group]: crate::hook::add_grouped
    /// [`FooterWidgets`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.FooterWidgets.html
    /// [`duat-utils`]: https://docs.rs/duat-utils/latest/duat_utils
    pub fn push_to<Cfg: WidgetCfg<U>>(&mut self, area: AreaId, cfg: Cfg) -> AreaId {
        RawUiBuilder(self).push_to(area, cfg)
    }

    /// The [`Handle<File>`] of this [`UiBuilder`], if there is one
    ///
    /// This will be [`Some`] when this [`Widget`] is being pushed
    /// around a [`File`], and [`None`] otherwise.
    ///
    /// [`File`]: crate::file::File
    pub fn file(&self) -> Option<Handle<File<U>, U>> {
        self.main.as_ref().and_then(Handle::try_downcast)
    }

    /// Runs the post [`Widget`] construction [hook]
    ///
    /// This does NOT push the widget into the [`Window`], as that is
    /// supposed to have happened already.
    ///
    /// [`Window`]: super::Window
    pub(super) fn finish_around_widget(
        self,
        pa: &mut Pass,
        parent_id: Option<AreaId>,
        handle: Handle<dyn Widget<U>, U>,
    ) -> AreaId {
        let do_cluster = self.main.is_some();

        let master_id = parent_id
            .filter(|_| do_cluster)
            .or(self.master_id)
            .or(self.main.as_ref().map(|h| h.area_id()))
            .unwrap_or(handle.area_id());

        if let Some(builder_fn) = self.builder_fn {
            builder_fn(pa, master_id, self.main.or(Some(handle)))
        } else {
            master_id
        }
    }

    /// Finishes building around a [`Window`]'s central [`File`]
    /// region
    ///
    /// [`Window`]: super::Window
    pub(super) fn finish_around_window(self, pa: &mut Pass, window_id: AreaId) -> AreaId {
        if let Some(builder_fn) = self.builder_fn {
            builder_fn(pa, window_id, None)
        } else {
            window_id
        }
    }

    ////////// Querying functions

    /// The [`AreaId`] for the [`Widget`] being built
    pub fn widget_id(&self) -> Option<AreaId> {
        self.widget_id
    }
}

/// A struct over the regular [`UiBuilder`], which grants direct
/// access to ui control primitives
pub struct RawUiBuilder<'a, U: Ui>(&'a mut UiBuilder<U>);

impl<U: Ui> RawUiBuilder<'_, U> {
    /// Raw pushing of [`Widget`]s by [`WidgetAlias`]es
    pub fn push<Cfg: WidgetCfg<U>>(&mut self, cfg: Cfg) -> AreaId {
        let prev_build_fn = self.0.builder_fn.take();
        let widget_id = AreaId::new();

        let win = self.0.win;
        self.0.builder_fn = Some(Box::new(move |pa, mut master_id, main| {
            let do_cluster = main.is_some();
            let on_file = main
                .as_ref()
                .is_some_and(|w| w.widget().data_is::<File<U>>());

            if let Some(prev) = prev_build_fn {
                master_id = prev(pa, master_id, main.clone());
            }

            let (cfg, builder) = {
                let builder = UiBuilder::pushed(win, widget_id, master_id, main.clone());
                let wc = WidgetCreated::<Cfg::Widget, U>((Some(cfg), builder));
                let (cfg, builder) = hook::trigger(pa, wc).0;
                (cfg.unwrap(), builder)
            };

            let (widget, specs) = cfg.build(pa, BuildInfo { main, pushed_to: None });

            let (node, parent_id) = context::windows().push(
                pa,
                win,
                (widget, specs),
                (master_id, widget_id),
                (do_cluster, on_file),
            );

            if let Some(main) = builder.main.as_ref() {
                main.related().write(pa).push(node.handle().clone())
            }

            if let Some(builder_fn) = builder.builder_fn {
                builder_fn(pa, widget_id, builder.main.or(Some(node.handle().clone())));
            }

            parent_id.unwrap_or(master_id)
        }));

        widget_id
    }

    /// Same as [`UiBuilder::push_to`]
    pub fn push_to<Cfg: WidgetCfg<U>>(&mut self, to_id: AreaId, cfg: Cfg) -> AreaId {
        let prev_build_fn = self.0.builder_fn.take();
        let widget_id = AreaId::new();

        let win = self.0.win;
        self.0.builder_fn = Some(Box::new(move |pa, mut master_id, main| {
            let do_cluster = main.is_some();
            let on_file = main
                .as_ref()
                .is_some_and(|h| h.widget().data_is::<File<U>>());

            if let Some(prev) = prev_build_fn {
                master_id = prev(pa, master_id, main.clone());
            }

            let (cfg, builder) = {
                let builder = UiBuilder::pushed(win, widget_id, master_id, main);
                let wc = WidgetCreated::<Cfg::Widget, U>((Some(cfg), builder));
                let (cfg, builder) = hook::trigger(pa, wc).0;
                (cfg.unwrap(), builder)
            };

            let (widget, specs) = cfg.build(pa, BuildInfo {
                main: builder.main.clone(),
                pushed_to: None,
            });

            let (node, parent_id) = context::windows().push(
                pa,
                win,
                (widget, specs),
                (to_id, widget_id),
                (do_cluster, on_file),
            );

            if let Some(main) = builder.main.as_ref() {
                main.related().write(pa).push(node.handle().clone())
            }

            if let Some(builder_fn) = builder.builder_fn {
                builder_fn(pa, widget_id, builder.main.or(Some(node.handle().clone())));
            }

            parent_id.unwrap_or(master_id)
        }));

        widget_id
    }
}

/// Information about the construction of the [`Ui`]
pub struct BuildInfo<U: Ui> {
    main: Option<Handle<dyn Widget<U>, U>>,
    pushed_to: Option<Handle<dyn Widget<U>, U>>,
}

impl<U: Ui> BuildInfo<U> {
    pub(super) fn for_main() -> Self {
        Self { main: None, pushed_to: None }
    }

    /// Returns the [`File`]'s [`Handle`], if this [`Widget`] was
    /// pushed to one
    pub fn file(&self) -> Option<Handle<File<U>, U>> {
        self.main.as_ref().and_then(Handle::try_downcast)
    }

    /// The [`Handle`] of the main [`Widget`] from this group
    ///
    /// Will return [`None`] when the [`Widget`] being built _is_ the
    /// main [`Widget`], which could happen if it was a spawned
    /// floating [`Widget`], for example.
    ///
    /// This differs from [`pushed_to`], since this will return the
    /// [`Widget`] at the "top level of construction". For example, if
    /// a [`File`] is added, a [`VertRule`] is pushed, and a
    /// [`LineNumbers`] is pushed to the [`VertRule`], when calling
    /// [`LineNumbersOptions::build`], [`BuildInfo::main`] will
    /// return the [`File`], while [`BuildInfo::pushed_to`] will
    /// return the [`VertRule`].
    ///
    /// [`pushed_to`]: Self::pushed_to
    /// [`VertRule`]: https://docs.rs/duat/latest/duat/prelude/struct.VertRule.html
    /// [`LineNumbers`]: https://docs.rs/duat/latest/duat/prelude/struct.LineNumbers.html
    /// [`LineNumbersOptions::build`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbersOptions.html
    pub fn main(&self) -> Option<Handle<dyn Widget<U>, U>> {
        self.main.clone()
    }

    /// The [`Handle`] of the [`Widget`] this one was pushed to
    ///
    /// Will return [`None`] when this [`Widget`] wasn't pushed onto
    /// any other, which could happen if it was a spawned floating
    /// [`Widget`], for example.
    ///
    /// This differs from [`main`], since that one will return the
    /// [`Widget`] at the "top level of construction". For example, if
    /// a [`File`] is added, a [`VertRule`] is pushed, and a
    /// [`LineNumbers`] is pushed to the [`VertRule`], when calling
    /// [`LineNumbersOptions::build`], [`BuildInfo::main`] will
    /// return the [`File`], while [`BuildInfo::pushed_to`] will
    /// return the [`VertRule`].
    ///
    /// [`main`]: Self::main
    /// [`VertRule`]: https://docs.rs/duat/latest/duat/prelude/struct.VertRule.html
    /// [`LineNumbers`]: https://docs.rs/duat/latest/duat/prelude/struct.LineNumbers.html
    /// [`LineNumbersOptions::build`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbersOptions.html
    pub fn pushed_to(&self) -> Option<Handle<dyn Widget<U>, U>> {
        self.pushed_to.clone()
    }
}

/// An alias to allow generic, yet consistent utilization of
/// [`UiBuilder`]s
///
/// This trait lets you do any available actions with [`UiBuilder`]s,
/// like pushing multiple [`Widget`]s, for example.
///
/// The reason for using this, rather than have your type just take a
/// [`UiBuilder`] parameter in its creation function, is mostly for
/// consistency's sake, since [`Ui`] building is done by pushing
/// [`Widget`]s into the builder, this lets you push multiple
/// [`Widget`]s, without breaking that same consistency.
pub trait WidgetAlias<U: Ui, D: BuilderDummy = WidgetCfgDummy> {
    /// "Pushes [`Widget`]s to a [`UiBuilder`], in a specific
    /// [`U::Area`]
    ///
    /// [`U::Area`]: Ui::Area
    fn push_alias(self, builder: &mut RawUiBuilder<U>) -> AreaId;
}

impl<Cfg: WidgetCfg<U>, U: Ui> WidgetAlias<U> for Cfg {
    fn push_alias(self, builder: &mut RawUiBuilder<U>) -> AreaId {
        builder.push(self)
    }
}

/// A dummy trait, meant for specialization
pub trait BuilderDummy {}

#[doc(hidden)]
pub struct WidgetCfgDummy;

impl BuilderDummy for WidgetCfgDummy {}

type BuilderFn<U> = Box<dyn FnOnce(&mut Pass, AreaId, Option<Handle<dyn Widget<U>, U>>) -> AreaId>;
