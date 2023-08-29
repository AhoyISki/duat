use std::sync::RwLock;

use super::{Area, PushSpecs, Ui};
use crate::{
    commands::{CommandErr, Commands},
    data::RwData,
    forms::FormPalette,
    widgets::Widget,
    Controler,
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
/// # use parsec_core::{
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
    controler: &'a mut Controler<U>,
    mod_area: RwLock<U::Area>,
}

impl<'a, U> FileBuilder<'a, U>
where
    U: Ui + 'static,
{
    /// Creates a new [`FileBuilder<U>`].
    pub fn new(controler: &'a mut Controler<U>, mod_area: RwLock<U::Area>) -> Self {
        Self {
            controler,
            mod_area,
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
    ///
    /// If you wish to, for example, push on [`Side::Bottom`] of `1`,
    /// checkout [`push_widget_to_area`][Self::push_widget_to_area].
    pub fn push_with_specs<F>(
        &self,
        builder: impl FnOnce(&Controler<U>) -> (Widget<U>, F, PushSpecs),
        specs: PushSpecs,
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        let file_id = *crate::CMD_FILE_ID.lock().unwrap();
        let (widget, checker, _) = builder(self.controler);
        let (child, parent) = self.controler.mutate_active_window(|window| {
            let mod_area = self.mod_area.read().unwrap();
            let (child, parent) = window.push(widget, &*mod_area, checker, specs, file_id, true);

            if let Some(parent) = &parent {
                if parent.is_senior_of(&window.files_region) {
                    window.files_region = parent.clone();
                }
            }

            (child, parent)
        });

        if let Some(parent) = &parent {
            *self.mod_area.write().unwrap() = parent.clone();
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
    pub fn push_with_specs_to<F>(
        &self,
        builder: impl FnOnce(&Controler<U>) -> (Widget<U>, F, PushSpecs),
        area: U::Area,
        specs: PushSpecs,
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        let file_id = *crate::CMD_FILE_ID.lock().unwrap();
        let (widget, checker, _) = builder(self.controler);
        let (child, parent) = self.controler.mutate_active_window(|window| {
            window.push(widget, &area, checker, specs, file_id, true)
        });

        (child, parent)
    }

    pub fn push_specd<F>(
        &self,
        builder: impl FnOnce(&Controler<U>) -> (Widget<U>, F, PushSpecs),
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        let file_id = *crate::CMD_FILE_ID.lock().unwrap();
        let (widget, checker, specs) = builder(self.controler);
        let (child, parent) = self.controler.mutate_active_window(|window| {
            let mod_area = self.mod_area.read().unwrap();
            let (child, parent) = window.push(widget, &*mod_area, checker, specs, file_id, true);

            // If a new parent is created, and it owns the old `files_region`
            // (.i.e all files), then it must become the new files_region.
            if let Some(parent) = &parent {
                if parent.is_senior_of(&window.files_region) {
                    window.files_region = parent.clone();
                }
            }

            (child, parent)
        });

        if let Some(parent) = &parent {
            *self.mod_area.write().unwrap() = parent.clone();
        }

        (child, parent)
    }

    pub fn palette(&self) -> &FormPalette {
        &self.controler.palette
    }

    pub fn commands(&self) -> &RwData<Commands> {
        &self.controler.commands
    }

    pub fn run_cmd(&self, cmd: impl ToString) -> Result<Option<String>, CommandErr> {
        self.controler.run_cmd(cmd)
    }
}
