use std::sync::LazyLock;

use super::{Area, Ui, Window};
use crate::{
    data::{Context, RwData},
    widgets::{PassiveWidget, WidgetCfg},
};

/// A constructor helper for [`Widget<U>`]s.
///
/// When pushing [`Widget<U>`]s to the layout, this struct can be used
/// to further actions to be taken. It is used in contexts where a
/// widget has just been inserted to the screen, inside closures.
///
/// Here, [`LineNumbers<U>`][crate::widgets::LineNumbers<U>] is pushed
/// to the left of a widget (which in this case is a [`FileWidget<U>`]
///
/// ```rust
/// # use duat_core::{
/// #     data::RoData,
/// #     ui::{ModNode, PushSpecs, Constraint, Ui},
/// #     widgets::{FileWidget, LineNumbers}
/// # };
/// fn file_fn<U>(mut mod_node: ModNode<U>, file: RoData<FileWidget<U>>)
/// where
///     U: Ui,
/// {
///     let specs = PushSpecs::left(Constraint::Length(1.0));
///     mod_node.push_specd(LineNumbers::default_fn());
/// }
/// ```
///
/// By using the `file_fn()` function as the `constructor_hook`
/// argument for [`Session::new()`][crate::Session::new()], every file
/// that is opened will have a
/// [`LineNumbers<U>`][crate::widgets::LineNumbers] widget attached to
/// it.
pub struct FileBuilder<'a, U>
where
    U: Ui,
{
    window: &'a mut Window<U>,
    mod_area: U::Area,
    context: Context<U>,
}

impl<'a, U> FileBuilder<'a, U>
where
    U: Ui,
{
    /// Creates a new [`FileBuilder<U>`].
    pub fn new(window: &'a mut Window<U>, mod_area: U::Area, context: Context<U>) -> Self {
        Self {
            window,
            mod_area,
            context,
        }
    }

    /// Pushes a [`Widget<U>`] to [`self`], given [`PushSpecs`] and a
    /// constructor function.
    ///
    /// Do note that this function will should change the index of
    /// [`self`], such that subsequent pushes are targeted at the
    /// parent.
    ///
    /// # Returns
    ///
    /// The first element is the `area_index` of the newly created
    /// [`Widget<U>`], you can use it to push new [`Widget<U>`]s.
    /// The second element, of type [`Option<usize>`] is
    /// [`Some(..)`] only when a new parent was created to
    /// accomodate the new [`Widget<U>`], and represents the new
    /// `area_index` of the old [`Widget<U>`], which has now
    /// become a child.
    ///
    /// # Examples
    ///
    /// Pushing on [`Side::Left`], when [`self`] has an index of `0`:
    ///
    /// ```text
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │                 │     │╭──2───╮╭───1───╮│
    /// │                 │ --> ││      ││       ││
    /// │                 │     ││      ││       ││
    /// │                 │     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// So a subsequent use of [`push_widget`][Self::push_widget] on
    /// [`Side::Bottom`] would push to the bottom of "both 1 and 2":
    ///
    /// ```text
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭──2───╮╭───1───╮│
    /// ││      ││       ││ --> │╰──────╯╰───────╯│
    /// ││      ││       ││     │╭───────3───────╮│
    /// │╰──────╯╰───────╯│     │╰───────────────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    pub fn push<W: PassiveWidget<U>>(&mut self) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = W::build(self.context, true);

        let related = widget.as_passive().clone();

        let (child, parent) = {
            let (child, parent) = self
                .window
                .push(widget, &self.mod_area, checker, specs, true);

            self.context.cur_file().unwrap().add_related_widget((
                related,
                child.clone(),
                W::name(),
            ));

            if let Some(parent) = &parent {
                if parent.is_senior_of(&self.window.files_region) {
                    self.window.files_region = parent.clone();
                }
            }

            (child, parent)
        };

        if let Some(parent) = &parent {
            self.mod_area = parent.clone();
        }

        (child, parent)
    }

    /// Pushes a [`Widget<U>`] to [`self`], given [`PushSpecs`] and a
    /// constructor function.
    ///
    /// Do note that this function will should change the index of
    /// [`self`], such that subsequent pushes are targeted at the
    /// parent.
    ///
    /// # Returns
    ///
    /// The first element is the `area_index` of the newly created
    /// [`Widget<U>`], you can use it to push new [`Widget<U>`]s.
    /// The second element, of type [`Option<usize>`] is
    /// [`Some(..)`] only when a new parent was created to
    /// accomodate the new [`Widget<U>`], and represents the new
    /// `area_index` of the old [`Widget<U>`], which has now
    /// become a child.
    ///
    /// # Examples
    ///
    /// Pushing on [`Side::Left`], when [`self`] has an index of `0`:
    ///
    /// ```text
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │                 │     │╭──2───╮╭───1───╮│
    /// │                 │ --> ││      ││       ││
    /// │                 │     ││      ││       ││
    /// │                 │     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// So a subsequent use of [`push_widget`][Self::push_widget] on
    /// [`Side::Bottom`] would push to the bottom of "both 1 and 2":
    ///
    /// ```text
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭──2───╮╭───1───╮│
    /// ││      ││       ││ --> │╰──────╯╰───────╯│
    /// ││      ││       ││     │╭───────3───────╮│
    /// │╰──────╯╰───────╯│     │╰───────────────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    pub fn push_cfg<W: PassiveWidget<U>>(
        &mut self,
        cfg: impl WidgetCfg<U, Widget = W>,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = cfg.build(self.context, true);

        let related = widget.as_passive().clone();

        let (child, parent) = {
            let (child, parent) = self
                .window
                .push(widget, &self.mod_area, checker, specs, true);

            self.context.cur_file().unwrap().add_related_widget((
                related,
                child.clone(),
                W::name(),
            ));

            if let Some(parent) = &parent {
                if parent.is_senior_of(&self.window.files_region) {
                    self.window.files_region = parent.clone();
                }
            }

            (child, parent)
        };

        if let Some(parent) = &parent {
            self.mod_area = parent.clone();
        }

        (child, parent)
    }

    /// Pushes a [`Widget<U>`] to a specific `area`, given
    /// [`PushSpecs`] and a constructor function.
    ///
    /// # Examples
    ///
    /// Given that [`self`] has an index of `0`, and other widgets
    /// have already been pushed, one can push to a specific
    /// [`Widget<U>`], given an area index.
    ///
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭──2───╮╭───1───╮│
    /// ││      ││       ││ --> ││      │╰───────╯│
    /// ││      ││       ││     ││      │╭───3───╮│
    /// │╰──────╯╰───────╯│     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    pub fn push_to<W: PassiveWidget<U>>(&mut self, area: U::Area) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = W::build(self.context, true);

        let related = widget.as_passive().clone();

        let (child, parent) = self.window.push(widget, &area, checker, specs, true);
        self.context
            .cur_file()
            .unwrap()
            .add_related_widget((related, child.clone(), W::name()));
        (child, parent)
    }

    /// Pushes a [`Widget<U>`] to a specific `area`, given
    /// [`PushSpecs`] and a constructor function.
    ///
    /// # Examples
    ///
    /// Given that [`self`] has an index of `0`, and other widgets
    /// have already been pushed, one can push to a specific
    /// [`Widget<U>`], given an area index.
    ///
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭──2───╮╭───1───╮│
    /// ││      ││       ││ --> ││      │╰───────╯│
    /// ││      ││       ││     ││      │╭───3───╮│
    /// │╰──────╯╰───────╯│     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    pub fn push_cfg_to<W: PassiveWidget<U>>(
        &mut self,
        cfg: impl WidgetCfg<U, Widget = W>,
        area: U::Area,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = cfg.build(self.context, true);

        let related = widget.as_passive().clone();

        let (child, parent) = self.window.push(widget, &area, checker, specs, true);
        self.context
            .cur_file().unwrap()
            .add_related_widget((related, child.clone(), W::name()));
        (child, parent)
    }
}

pub struct WindowBuilder<'a, U>
where
    U: Ui,
{
    window: &'a mut Window<U>,
    area: U::Area,
    context: Context<U>,
}

impl<'a, U> WindowBuilder<'a, U>
where
    U: Ui,
{
    /// Creates a new [`FileBuilder<U>`].
    pub fn new(window: &'a mut Window<U>, context: Context<U>) -> Self {
        let area = window.files_region().clone();
        Self {
            window,
            area,
            context,
        }
    }

    /// Pushes a [`Widget<U>`] to the file's area, given a
    /// [`Widget<U>`] builder function.
    ///
    /// In Duat, windows have two parts: the central area and the
    /// periphery. The central part is the "file's region", it
    /// contains all [`FileWidget<U>`]s, as well as all directly
    /// related [`Widget<U>`]s ([`LineNumbers<U>`]s,
    /// [`StatusLine<U>`]s, etc.). These widgets are all
    /// "clustered" to their main file, that is, moving the file will
    /// move the widget with it.
    ///
    /// The periphery contains all widgets that are _not_ directly
    /// related to any file in particular. One example of this
    /// would be a file explorer, or a global status line, that
    /// switches to display information about the currently active
    /// file. These widgets may be clustered together (not with
    /// any widget in the central area), and be moved in unison. One
    /// could, for example, cluster a [`CommandLine<U>`] with a
    /// [`StatusLine<U>`], to keep them together when moving
    /// either of them around. By default, no widgets
    /// are clustered together, but you can cluster them with the
    /// [`cluster_to`] function.
    ///
    /// # Returns
    ///
    /// The first element is the area occupied by the new widget. You
    /// can use [`push_to`] or [`cluster_to`] methods to push
    /// widgets to this one directly, instead of the parent area.
    ///
    /// The second element is a possible newly created area to house
    /// the previously existing and newly created widgets. It may
    /// not be created, for example, if you push two widgets to
    /// another on the same axis, only one parent is necessary to
    /// house all three of them.
    ///
    /// # Examples
    ///
    /// This method would be used when defining how a new window will
    /// be opened, from Duat's [`Session`]
    ///
    /// ```rust
    /// ```
    ///
    /// [`FileWidget<U>`]: crate::widgets::FileWidget
    /// [`LineNumbers<U>`]: crate::widgets::LineNumbers
    /// [`StatusLine<U>`]: crate::widgets::StatusLine
    /// [`push_to`]: Self::<U>::push_to
    /// [`Session`]: crate::session::Session
    pub fn push<W: PassiveWidget<U>>(&mut self) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = W::build(self.context, false);

        let (child, parent) = self.window.push(widget, &self.area, checker, specs, false);

        if let Some(parent) = &parent {
            self.area = parent.clone();
        }

        (child, parent)
    }

    /// Pushes a [`Widget<U>`] to the file's area, given a
    /// [`Widget<U>`] builder function.
    ///
    /// In Duat, windows have two parts: the central area and the
    /// periphery. The central part is the "file's region", it
    /// contains all [`FileWidget<U>`]s, as well as all directly
    /// related [`Widget<U>`]s ([`LineNumbers<U>`]s,
    /// [`StatusLine<U>`]s, etc.). These widgets are all
    /// "clustered" to their main file, that is, moving the file will
    /// move the widget with it.
    ///
    /// The periphery contains all widgets that are _not_ directly
    /// related to any file in particular. One example of this
    /// would be a file explorer, or a global status line, that
    /// switches to display information about the currently active
    /// file. These widgets may be clustered together (not with
    /// any widget in the central area), and be moved in unison. One
    /// could, for example, cluster a [`CommandLine<U>`] with a
    /// [`StatusLine<U>`], to keep them together when moving
    /// either of them around. By default, no widgets
    /// are clustered together, but you can cluster them with the
    /// [`cluster_to`] function.
    ///
    /// # Returns
    ///
    /// The first element is the area occupied by the new widget. You
    /// can use [`push_to`] or [`cluster_to`] methods to push
    /// widgets to this one directly, instead of the parent area.
    ///
    /// The second element is a possible newly created area to house
    /// the previously existing and newly created widgets. It may
    /// not be created, for example, if you push two widgets to
    /// another on the same axis, only one parent is necessary to
    /// house all three of them.
    ///
    /// # Examples
    ///
    /// This method would be used when defining how a new window will
    /// be opened, from Duat's [`Session`]
    ///
    /// ```rust
    /// ```
    ///
    /// [`FileWidget<U>`]: crate::widgets::FileWidget
    /// [`LineNumbers<U>`]: crate::widgets::LineNumbers
    /// [`StatusLine<U>`]: crate::widgets::StatusLine
    /// [`push_to`]: Self::<U>::push_to
    /// [`Session`]: crate::session::Session
    pub fn push_cfg<W: PassiveWidget<U>>(
        &mut self,
        cfg: impl WidgetCfg<U, Widget = W>,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = cfg.build(self.context, false);

        let (child, parent) = self.window.push(widget, &self.area, checker, specs, false);

        if let Some(parent) = &parent {
            self.area = parent.clone();
        }

        (child, parent)
    }

    /// Pushes a [`Widget<U>`] to a specific `area`, given
    /// [`PushSpecs`] and a constructor function.
    ///
    /// # Examples
    ///
    /// Given that [`self`] has an index of `0`, and other widgets
    /// have already been pushed, one can push to a specific
    /// [`Widget<U>`], given an area index.
    ///
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭──2───╮╭───1───╮│
    /// ││      ││       ││ --> ││      │╰───────╯│
    /// ││      ││       ││     ││      │╭───3───╮│
    /// │╰──────╯╰───────╯│     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    pub fn push_to<W: PassiveWidget<U>>(&mut self, area: U::Area) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = W::build(self.context, true);

        self.window.push(widget, &area, checker, specs, true)
    }

    /// Pushes a [`Widget<U>`] to a specific `area`, given
    /// [`PushSpecs`] and a constructor function.
    ///
    /// # Examples
    ///
    /// Given that [`self`] has an index of `0`, and other widgets
    /// have already been pushed, one can push to a specific
    /// [`Widget<U>`], given an area index.
    ///
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭──2───╮╭───1───╮│
    /// ││      ││       ││ --> ││      │╰───────╯│
    /// ││      ││       ││     ││      │╭───3───╮│
    /// │╰──────╯╰───────╯│     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    pub fn push_cfg_to<W: PassiveWidget<U>>(
        &mut self,
        pushable: impl WidgetCfg<U, Widget = W>,
        area: U::Area,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = pushable.build(self.context, false);

        self.window.push(widget, &area, checker, specs, true)
    }
}

fn run_once<W: PassiveWidget<U>, U: Ui>(context: Context<U>) {
    static ONCE_LIST: LazyLock<RwData<Vec<&'static str>>> =
        LazyLock::new(|| RwData::new(Vec::new()));

    let mut once_list = ONCE_LIST.write();
    if !once_list.contains(&W::name()) {
        W::once(context);
        once_list.push(W::name());
    }
}
