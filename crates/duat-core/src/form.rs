//! Utilities for stylizing the text of Duat
use std::sync::{Mutex, OnceLock, RwLock, RwLockReadGuard};

use FormKind::*;
use crossterm::style::{Attribute, Attributes, ContentStyle};
pub use crossterm::{cursor::SetCursorStyle as CursorShape, style::Color as CtColor};

pub use self::global::*;
pub(crate) use self::global::{colorscheme_exists, exists};
use crate::{
    context::{self, DuatSender},
    hook::{self, FormSet},
    session::DuatEvent,
    text::FormTag,
};

static COLORS: Mutex<Vec<CtColor>> = Mutex::new(Vec::new());
static SENDER: OnceLock<DuatSender> = OnceLock::new();
static BASE_FORMS: &[(&str, Form)] = &[
    ("default", Form::new()),
    ("accent", Form::new().bold()),
    ("caret.main", Form::new().reverse()),
    ("caret.extra", Form {
        kind: Ref(2, default_style()),
        ..Form::new().reverse()
    }),
    ("selection.main", Form::new().white().on_dark_grey()),
    ("selection.extra", Form::new().white().on_grey()),
    ("cloak", Form::new().grey().on_black()),
    ("character.control", Form::new().grey()),
    ("param.path", Form::new().yellow()),
    ("param.path.exists", Form {
        kind: Ref(8, ContentStyle {
            attributes: Attributes::none().with(Attribute::Underlined),
            ..default_style()
        }),
        ..Form::new().yellow().underlined()
    }),
    ("replace", Form::new().grey()),
];

/// The functions that will be exposed for public use.
mod global {
    use std::{
        any::TypeId,
        collections::HashMap,
        sync::{Arc, LazyLock, Mutex, OnceLock},
    };

    use super::{BASE_FORMS, CtColor, CursorShape, Form, FormId, Painter, Palette};
    #[doc(inline)]
    pub use crate::__id_of__ as id_of;
    use crate::{
        context,
        form::FormKind,
        hook::{self, ColorSchemeSet},
    };

    static PALETTE: OnceLock<&Palette> = OnceLock::new();
    static FORMS: OnceLock<&Mutex<Vec<Arc<str>>>> = OnceLock::new();
    static COLORSCHEMES: LazyLock<Mutex<HashMap<Arc<str>, ColorschemeFn>>> =
        LazyLock::new(Mutex::default);

    /// Sets the [`Form`] by the name of `name`
    ///
    /// This will create a new form or replace one that already
    /// exists, and you can either set it to a [`Form`] directly, or
    /// reference another form by its name:
    ///
    /// ```rust
    /// # let (tx, rx) = duat_core::context::duat_channel();
    /// # duat_core::context::set_sender(tx);
    /// # duat_core::form::set_initial(duat_core::form::get_initial());
    /// # use duat_core::form::{self, Form};
    /// // Creates a regular form
    /// let reg_id = form::set("my_regular_form", Form::new().red());
    /// // Creates a form that references the first
    /// let ref_id = form::set("my_ref_form", Form::mimic("my_regular_form"));
    /// // Sets both "MyRegularForm" and "MyRefForm" to blue
    /// form::set("my_regular_form", Form::new().blue());
    /// ```
    ///
    /// If you are creating a plugin, or another kind of tool for
    /// others using Duat, use [`form::set_weak`] instead of this
    /// function.
    ///
    /// [`form::set_weak`]: set_weak
    pub fn set(name: impl ToString, form: Form) -> FormId {
        let name = name.to_string();
        let cloned_name = name.clone();

        match form.kind {
            FormKind::Normal => PALETTE.get().unwrap().set_form(cloned_name, form),
            FormKind::Ref(refed, style) => {
                PALETTE.get().unwrap().set_ref(cloned_name, refed, style)
            }
            _ => unreachable!(),
        };

        let mut forms = FORMS.get().unwrap().lock().unwrap();
        FormId(position_of_name(&mut forms, name) as u16)
    }

    /// Sets a form, "weakly"
    ///
    /// The difference between this function and [`form::set`] is
    /// that this function will only trigger if the form didn't
    /// already exist/was previously onle set with [`set_weak`].
    ///
    /// This is useful for plugins, since it prioritizes the user's
    /// preferences, no matter in what order this function and
    /// [`form::set`] are called:
    ///
    /// ```rust
    /// # let (tx, rx) = duat_core::context::duat_channel();
    /// # duat_core::context::set_sender(tx);
    /// # duat_core::form::set_initial(duat_core::form::get_initial());
    /// use duat_core::form::{self, Form};
    ///
    /// // Creates a form "weakly"
    /// form::set_weak("weak_form", Form::new().blue().on_white());
    ///
    /// // Sets that form "strongly"
    /// form::set("weak_form", Form::new().red().on_grey());
    ///
    /// // Even if setting the form weakly afterwards, it won't change again.
    /// form::set_weak("weak_form", Form::new().blue().underlined());
    /// ```
    ///
    /// [`form::set`]: set
    pub fn set_weak(name: impl ToString, form: Form) -> FormId {
        let name = name.to_string();
        let cloned_name = name.clone();

        match form.kind {
            FormKind::Normal => PALETTE.get().unwrap().set_weak_form(cloned_name, form),
            FormKind::Ref(refed, style) => {
                PALETTE
                    .get()
                    .unwrap()
                    .set_weak_ref(cloned_name, refed, style)
            }
            _ => unreachable!(),
        };

        let mut forms = FORMS.get().unwrap().lock().unwrap();
        FormId(position_of_name(&mut forms, name) as u16)
    }

    /// Returns a [`Form`], given a [`FormId`].
    pub fn from_id(id: FormId) -> Form {
        PALETTE.get().unwrap().form_from_id(id).unwrap_or_default()
    }

    /// The current main cursor, with the `"caret.main"` [`Form`]
    pub fn main_cursor() -> (Form, Option<CursorShape>) {
        PALETTE.get().unwrap().main_cursor()
    }

    /// The current extra cursor, with the `"caret.extra"` [`Form`]
    pub fn extra_cursor() -> (Form, Option<CursorShape>) {
        PALETTE.get().unwrap().extra_cursor()
    }

    /// Sets the main cursor's [shape]
    ///
    /// Cursors in Duat can either be a distinct [shape], or can be
    /// defined as a [`Form`], just like the rest of the styling.
    ///
    /// This is done because some UIs (like a terminal) lack the
    /// ability to show multiple cursors, so extra cursors are usually
    /// printed as solid blocks with a background color.
    ///
    /// If you want to set the cursor's color, do something like this:
    ///
    /// ```rust
    /// # use duat_core::form::{self, Form, Color};
    /// # let (tx, rx) = duat_core::context::duat_channel();
    /// # duat_core::context::set_sender(tx);
    /// # duat_core::form::set_initial(duat_core::form::get_initial());
    /// let color = Color::new("#456321");
    /// form::set("caret.main", Form::new().black().on(color));
    /// ```
    ///
    /// However, if possible, Duat will still try to use the main
    /// cursor's [shape]. If you don't want that to happen, see
    /// [`form::unset_main_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`form::unset_main_cursor`]: unset_main_cursor
    pub fn set_main_cursor(shape: CursorShape) {
        PALETTE.get().unwrap().set_main_cursor(shape);
    }

    /// Sets extra cursors's [shape]s
    ///
    /// Cursors in Duat can either be a distinct [shape], or can be
    /// defined as a [`Form`], just like the rest of the styling.
    ///
    /// This is done because some UIs (like a terminal) lack the
    /// ability to show multiple cursors, so extra cursors are usually
    /// printed as solid blocks with a background color.
    ///
    /// If you want to set the cursor's color, do something like this:
    ///
    /// ```rust
    /// # use duat_core::form::{self, Form, Color};
    /// # let (tx, rx) = duat_core::context::duat_channel();
    /// # duat_core::context::set_sender(tx);
    /// # duat_core::form::set_initial(duat_core::form::get_initial());
    /// form::set("caret.extra", Form::new().black().on_cyan());
    /// ```
    ///
    /// However, if possible, Duat will still try to use the main
    /// cursor's [shape]. If you don't want that to happen, see
    /// [`form::unset_extra_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`form::unset_extra_cursor`]: unset_extra_cursor
    pub fn set_extra_cursor(shape: CursorShape) {
        PALETTE.get().unwrap().set_extra_cursor(shape);
    }

    /// Removes the main cursor's [shape]
    ///
    /// By doing this, you will force Duat to draw the main cursor by
    /// use of the `"caret.main"` form.
    ///
    /// If you want to set the [shape] instead, see
    /// [`form::set_main_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`form::set_main_cursor`]: set_main_cursor
    pub fn unset_main_cursor() {
        PALETTE.get().unwrap().unset_main_cursor();
    }

    /// Removes extra cursors's [shape]s
    ///
    /// By doing this, you will force Duat to draw the extra cursor by
    /// use of the `"caret.main"` form. Do note however that, in
    /// something like a terminal, extra cursors would never be
    /// printed as a [shape] anyways, since terminals can only
    /// print one cursor at a time.
    ///
    /// If you want to set the [shape] instead, see
    /// [`form::set_extra_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`form::set_extra_cursor`]: set_extra_cursor
    pub fn unset_extra_cursor() {
        PALETTE.get().unwrap().unset_extra_cursor();
    }

    /// Removes all cursors's [shape]s
    ///
    /// Is the equivalent of calling [`unset_main_cursor`] and
    /// [`unset_extra_cursor`].
    ///
    /// [shape]: CursorShape
    pub fn unset_cursors() {
        PALETTE.get().unwrap().unset_main_cursor();
        PALETTE.get().unwrap().unset_extra_cursor();
    }

    /// Creates a [`Painter`] with a mask
    ///
    /// # Warning
    ///
    /// Only [`RawUi`] implementors should ever make use of this
    /// function, since it reads from the [`RwLock`] that is used for
    /// [`Form`] setting. If you try to set `Form`s while holding a
    /// `Painter`, you _will_ deadlock Duat, so be careful with this
    /// function. Getting `Form`s is _also_ going to cause deadlocks,
    /// since you might need to mutably borrow in order to set a
    /// default value for a form.
    ///
    /// [`RawUi`]: crate::ui::traits::RawUi
    /// [`RwLock`]: std::sync::RwLock
    pub fn painter_with_mask(mask: &'static str) -> Painter {
        PALETTE.get().unwrap().painter(super::DEFAULT_ID, mask)
    }

    /// Creates a [`Painter`] with a mask and a widget
    pub(crate) fn painter_with_widget_and_mask<W: ?Sized + 'static>(mask: &'static str) -> Painter {
        PALETTE.get().unwrap().painter(
            default_id(TypeId::of::<W>(), crate::utils::duat_name::<W>()),
            mask,
        )
    }

    /// Enables the use of this mask
    ///
    /// A mask is essentially a remapping of [`Form`]s based on
    /// suffix, this remapping doesn't take place outside of a
    /// [`Painter`] (i.e. [`form::from_id`] won't be altered), here's
    /// how it works:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # let (tx, rx) = duat_core::context::duat_channel();
    /// # duat_core::context::set_sender(tx);
    /// # duat_core::form::set_initial(duat_core::form::get_initial());
    /// use duat::prelude::*;
    ///
    /// let mut text = Text::new();
    ///
    /// // Assume that a Form with the given name exists
    /// form::set("my_form", Form::new().red().on_blue());
    ///
    /// // If I create a second Form like this one, they are separate
    /// form::set("my_form.suffix", Form::new().undercurled());
    ///
    /// text = txt!("[my_form]This text is red on blue[], [my_form.suffix]undercurled");
    ///
    /// // But if I enable the "suffix" mask that's at the end of the second Form
    /// form::enable_mask("suffix");
    ///
    /// // After calling `handle.set_mask("suffix")` on the Handle that owns this
    /// // Text, it will be equivalent to this:
    ///
    /// text = txt!("[my_form.suffix]This text is red on blue[], [my_form.suffix]undercurled");
    /// ```
    ///
    /// Masks can serve a myriad of different purposes, but here's a
    /// few:
    ///
    /// - When you want to temporarily change the [`Form`]s on a
    ///   single [`Widget`]. This is, for example, used in the
    ///   [`Notifications`] [`Widget`], which maps [`Form`]s in order
    ///   to correspond to the [`Level`] of their severity.
    /// - When you want to have [`Widget`]s change [`Form`] based on
    ///   [hooks], so you could have, for example, an `"inactive"`
    ///   mask for your [`Buffer`]s
    /// - If you want to quickly cycle through [`Form`]s in a
    ///   [`Text`], this is the most efficient way of doing that,
    ///   since it relies on static remaps, not on changing the
    ///   [`Form`]s themselves.
    ///
    /// When Duat first starts, the only available masks are
    /// `"error"`, `"warn"` and `"info"`, but you can use this
    /// function to add more of them.
    ///
    /// [`form::from_id`]: from_id
    /// [`Widget`]: crate::ui::Widget
    /// [`Notifications`]: https://docs.rs/duat/latest/duat
    /// [`Level`]: crate::context::Level
    /// [hooks]: crate::hook
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`Text`]: crate::text::Text
    pub fn enable_mask(mask: impl AsRef<str> + Send + Sync + 'static) {
        let mask = mask.as_ref();
        let mut inner = PALETTE.get().unwrap().0.write().unwrap();
        if !inner.masks.iter().any(|(m, _)| *m == mask) {
            let mut remaps: Vec<u16> = (0..inner.forms.len() as u16).collect();

            for (i, (name, ..)) in inner.forms.iter().enumerate() {
                if let Some((pref, suf)) = name.rsplit_once('.')
                    && suf == mask
                    && let Some(j) = inner.forms.iter().position(|(name, ..)| *name == pref)
                {
                    remaps[j] = i as u16;
                }
            }

            inner.masks.push((mask.to_string(), remaps));
        }
    }

    /// Returns the [`FormId`] from the name of a [`Form`]
    ///
    /// If there is no [`Form`] with the given name, a new one is
    /// created, which will behave according to the following
    /// priority:
    ///
    /// - If the name contains a `'.'` character, it will reference
    ///   the [`Form`] whose name is a suffix up to the last `'.'`.
    ///   For example, `"Prefix.Middle.Suffix"` will reference
    ///   `"Prefix.Middle"`;
    /// - If the name does not contain a `'.'`, it will not reference
    ///   anything, having the [default `Form`];
    ///
    /// If a referenced [`Form`] does not exist, it will be added,
    /// following the same rules.
    ///
    /// # Note
    ///
    /// This is a macro because, in order to be as efficient as
    /// possible, it is better to store this value inside of a
    /// static variable, since it is guaranteed to not change. This
    /// way, you only have to figure it out once, and it is much
    /// faster than with a [`HashMap`] (how this is usually done).
    ///
    /// [`HashMap`]: std::collections::HashMap
    /// [default `Form`]: Form::new
    // SAFETY: Since _set_many always resolves to the same value, then the
    // static muts should eventually be set to their correct values, after
    // which no problems can occurr.
    // Before that point, the absolute worst thing that could happen is
    // DEFAULT_ID will be returned instead of the correct id (if the two
    // unsafe setting statements are in the wrong order for some reason),
    // but this should pretty much never happen.
    #[macro_export]
    #[doc(hidden)]
    macro_rules! __id_of__ {
        ($form:expr) => {{
            use $crate::form::{DEFAULT_ID, FormId, set_many};

            static mut WAS_SET: bool = false;
            static mut ID: FormId = DEFAULT_ID;
            if unsafe { WAS_SET } {
                unsafe { ID }
            } else {
                let name = $form.to_string();
                let id = set_many([(name, None)])[0];
                unsafe {
                    ID = id;
                    WAS_SET = true;
                }
                id
            }
        }};
    }

    /// Non static version of [`id_of!`]
    ///
    /// You should only use this if the names of the [`Form`]s in
    /// question are not known at compile time. And if that is the
    /// case, you should try to find a way to memoize around this
    /// issue (usually with something like a [`HashMap`]).
    pub fn id_of_non_static(name: impl ToString) -> FormId {
        let name = name.to_string();
        set_many([(name, None)])[0]
    }

    /// Non static version of [`id_of!`], for many [`Form`]s
    ///
    /// You should only use this if the names of the [`Form`]s in
    /// question are not known at compile time. And if that is the
    /// case, you should try to find a way to memoize around this
    /// issue (usually with something like a [`HashMap`]).
    pub fn ids_of_non_static(names: impl IntoIterator<Item = impl ToString>) -> Vec<FormId> {
        set_many(names.into_iter().map(|n| (n.to_string(), None)))
    }

    /// Sets a bunch of [`Form`]s
    #[doc(hidden)]
    pub fn set_many<S: AsRef<str>>(
        sets: impl IntoIterator<Item = (S, Option<Form>)>,
    ) -> Vec<FormId> {
        let mut ids = Vec::new();
        let mut forms = FORMS.get().unwrap().lock().unwrap();
        let sets: Vec<_> = sets.into_iter().collect();
        for (name, _) in &sets {
            ids.push(FormId(position_of_name(&mut forms, name) as u16));
        }

        PALETTE.get().unwrap().set_many(&sets);

        ids
    }

    /// Adds a colorscheme to the list of colorschemes
    ///
    /// A colorscheme is just a name in the form of a `&'static str`,
    /// and a list of name/[`Form`] pairs.
    ///
    /// This colorscheme can then be applied by calling
    /// [`set_colorscheme`].
    ///
    /// When you call this function, you will replace any previous
    /// colorscheme with the same name.
    pub fn add_colorscheme(
        name: impl ToString,
        pairs: impl FnMut() -> Vec<(String, Form)> + Send + 'static,
    ) {
        let name = name.to_string();
        COLORSCHEMES
            .lock()
            .unwrap()
            .insert(Arc::from(name), Box::new(pairs));
    }

    /// Applies a [`ColorScheme`]
    ///
    /// This [`ColorScheme`] must've first been added via
    /// [`form::add_colorscheme`].
    ///
    /// [`form::add_colorscheme`]: add_colorscheme
    pub fn set_colorscheme(name: &str) {
        let name = name.to_string();
        let mut colorschemes = COLORSCHEMES.lock().unwrap();
        if let Some(pairs) = colorschemes.get_mut(name.as_str()) {
            set_many(pairs().into_iter().map(|(name, form)| (name, Some(form))));
            context::queue(move |pa| _ = hook::trigger(pa, ColorSchemeSet(name.to_string())));
        } else {
            context::error!("The colorscheme [a]{name}[] was not found");
        }
    }

    /// Gets all available colorschemes
    pub fn colorscheme_list() -> Vec<String> {
        COLORSCHEMES
            .lock()
            .unwrap()
            .keys()
            .map(|name| name.to_string())
            .collect()
    }

    /// Wether or not a specific [`Form`] has been set
    pub(crate) fn exists(name: &str) -> bool {
        FORMS
            .get()
            .unwrap()
            .lock()
            .unwrap()
            .iter()
            .any(|fname| fname.as_ref() == name)
    }

    /// Wether or not a specific [`ColorScheme`] was added
    pub(crate) fn colorscheme_exists(name: &str) -> bool {
        COLORSCHEMES.lock().unwrap().contains_key(name)
    }

    /// The name of a form, given a [`FormId`]
    pub(super) fn name_of(id: FormId) -> Arc<str> {
        FORMS.get().unwrap().lock().unwrap()[id.0 as usize].clone()
    }

    fn default_id(type_id: TypeId, type_name: &'static str) -> FormId {
        static IDS: LazyLock<Mutex<HashMap<TypeId, FormId>>> = LazyLock::new(Mutex::default);
        let mut ids = IDS.lock().unwrap();

        if let Some(id) = ids.get(&type_id) {
            *id
        } else {
            let name = format!("default.{type_name}");
            let id = set_many(vec![(name, None)])[0];
            ids.insert(type_id, id);
            id
        }
    }

    fn position_of_name(names: &mut Vec<Arc<str>>, name: impl AsRef<str>) -> usize {
        let name = name.as_ref();
        if let Some((i, _)) = names
            .iter()
            .enumerate()
            .find(|(_, rhs)| rhs.as_ref() == name)
        {
            i
        } else if let Some((refed, _)) = name.rsplit_once('.') {
            position_of_name(names, refed);
            names.push(name.into());
            names.len() - 1
        } else {
            names.push(name.into());
            names.len() - 1
        }
    }

    /// Gets a [`Mutex`] for the initial [`Form`]'s list of Duat
    ///
    /// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
    #[doc(hidden)]
    pub fn get_initial() -> (&'static Mutex<Vec<Arc<str>>>, &'static Palette) {
        let forms = Box::leak(Box::new(Mutex::new(
            BASE_FORMS.iter().map(|(n, ..)| Arc::from(*n)).collect(),
        )));
        let palette = Box::leak(Box::new(Palette::new()));
        (forms, palette)
    }

    /// Sets the [`Mutex`] for the initial [`Form`]'s list of Duat
    ///
    /// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
    #[doc(hidden)]
    pub fn set_initial((forms, palette): (&'static Mutex<Vec<Arc<str>>>, &'static Palette)) {
        FORMS.set(forms).expect("Forms setup ran twice");
        PALETTE.set(palette).expect("Forms setup ran twice");
    }

    /// A color value for use in [`Form`]s
    #[derive(Clone, Copy, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Color(u16);

    impl Color {
        /// Returns a new `Color` for use in [`Form`]s
        ///
        /// This function accepts two color formats:
        ///
        /// - A hexcode, like `"#abcdef"`, capitalization is ignored;
        /// - Three hsl values, like `"hsl {hue} {sat} {lit}"`, where
        ///   {hue}, {sat} and {lit} can either be a number from
        ///   `0..255`, or a percentage, followed by `'%'`, e.g. `"hsl
        ///   234 50% 42"`.
        #[track_caller]
        pub fn new(value: &str) -> Self {
            let mut colors = super::COLORS.lock().unwrap();
            colors.push(match super::str_to_color(value) {
                Ok(color) => color,
                Err(err) => panic!("{}", err),
            });

            Color(colors.len() as u16 - 1)
        }

        /// Creates a new `Color` that has the same value as another
        ///
        /// If you change this one, [`Form`]s making use of the other
        /// one will not be changed.
        pub fn copy(&self) -> Self {
            let mut colors = super::COLORS.lock().unwrap();
            colors.extend_from_within(self.0 as usize..=self.0 as usize);

            Color(colors.len() as u16 - 1)
        }

        /// Sets the value of this `Color` to a new one
        ///
        /// This will change every [`Form`] that makes use of this
        /// color, letting you quickly make adjustments to all `Form`
        /// colors.
        ///
        /// This function accepts two color formats:
        ///
        /// - A hexcode, like `"#abcdef"`, capitalization is ignored;
        /// - Three hsl values, like `"hsl {hue} {sat} {lit}"`, where
        ///   {hue}, {sat} and {lit} can either be a number from
        ///   `0..255`, or a percentage, followed by `'%'`, e.g. `"hsl
        ///   234 50% 42"`.
        #[track_caller]
        pub fn set(&self, value: &str) {
            let color = match super::str_to_color(value) {
                Ok(color) => color,
                Err(err) => panic!("{}", err),
            };

            super::COLORS.lock().unwrap()[self.0 as usize] = color;

            let mut palette = PALETTE.get().unwrap().0.write().unwrap();

            for (_, form) in &mut palette.forms {
                let override_style =
                    if let FormKind::Ref(_, stl) | FormKind::WeakestRef(_, stl) = &mut form.kind {
                        Some(stl)
                    } else {
                        None
                    };

                for style in [Some(&mut form.style), override_style]
                    .into_iter()
                    .flatten()
                {
                    if form.fg == Some(*self) {
                        style.foreground_color = Some(color);
                    }
                    if form.bg == Some(*self) {
                        style.background_color = Some(color);
                    }
                    if form.ul == Some(*self) {
                        style.underline_color = Some(color);
                    }
                }
            }
        }

        /// Sets this `Color`'s value to an interpolation of `self`
        /// and `other`
        ///
        /// It interpolates `(lhs * self + rhs * other) / (lhs + rhs)`
        pub fn interpolate(&self, lhs: u8, rhs: u8, other: Color) {
            let mut colors = super::COLORS.lock().unwrap();

            let CtColor::Rgb { r: r0, g: g0, b: b0 } = colors[self.0 as usize] else {
                unreachable!("You can't set a color to a non rgb value");
            };
            let CtColor::Rgb { r: r1, g: g1, b: b1 } = colors[other.0 as usize] else {
                unreachable!("You can't set a color to a non rgb value");
            };

            colors[self.0 as usize] = CtColor::Rgb {
                r: (lhs * r0 + rhs * r1) / (lhs + rhs),
                g: (lhs * g0 + rhs * g1) / (lhs + rhs),
                b: (lhs * b0 + rhs * b1) / (lhs + rhs),
            };

            let mut palette = PALETTE.get().unwrap().0.write().unwrap();

            for (_, form) in &mut palette.forms {
                if form.fg == Some(*self) {
                    form.style.foreground_color = Some(colors[self.0 as usize]);
                }
                if form.bg == Some(*self) {
                    form.style.background_color = Some(colors[self.0 as usize]);
                }
                if form.ul == Some(*self) {
                    form.style.underline_color = Some(colors[self.0 as usize]);
                }
            }
        }

        /// The underlying value of this `Color`
        pub fn as_u16(&self) -> u16 {
            self.0
        }
    }

    type ColorschemeFn = Box<dyn FnMut() -> Vec<(String, Form)> + Send>;
}

/// An identifier of a [`Form`]
///
/// [`Builder`] part: Applies the [`Form`] destructively
///
/// This struct is always going to point to the same form, since those
/// cannot be destroyed.
///
/// The main use for keeping these things directly is in order to
/// modify a buffer's text in an efficient manner, by adding tags
/// directly, instead of using a macro like [`txt!`]
///
/// [`txt!`]: crate::text::txt
/// [`Builder`]: crate::text::Builder
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct FormId(u16);

impl FormId {
    /// Creates a [`Tag`] out of this [`FormId`]
    ///
    /// In order to push a [`Form`] to the [`Text`], it needs a
    /// priority value, in order to properly sort the [`Form`]s within
    /// the same byte.
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Text`]: crate::text::Text
    pub const fn to_tag(self, prio: u8) -> FormTag {
        FormTag(self, prio)
    }

    /// The internal id of the [`FormId`]
    ///
    /// This may be useful in certain situations.
    pub const fn to_u16(self) -> u16 {
        self.0
    }

    /// The name of this [`FormId`]
    pub fn name(self) -> std::sync::Arc<str> {
        name_of(self)
    }
}

impl std::fmt::Debug for FormId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FormId({}: {})", self.0, name_of(*self))
    }
}

/// Mimics [`ContentStyle`] methods for the [`Form`] type
macro_rules! mimic_method {
    (#[$attr:meta] $method:ident $attrib:expr) => {
        /// New [`Form`] with the
        #[$attr]
        /// attribute.
        pub const fn $method(mut self) -> Form {
            self.style.attributes = self.style.attributes.with($attrib);
            self
        }
    };
    (#[$attr:meta] $fg:ident $bg:ident $ul:ident $color:expr) => {
        /// New [`Form`] with a
        #[$attr]
        /// foreground.
        pub const fn $fg(mut self) -> Form {
            self.style.foreground_color = Some($color);
            self.fg = None;
            self
        }

        /// New [`Form`] with a
        #[$attr]
        /// background.
        pub const fn $bg(mut self) -> Form {
            self.style.background_color = Some($color);
            self.bg = None;
            self
        }

        /// New [`Form`] with a
        #[$attr]
        /// underlining.
        ///
        /// Do note that this feature may not be supported in all `Ui`s,
        /// for example, various terminals don't support this feature,
        /// since it is a part of the kitty protocol, and hasn't been
        /// universally accepted yet.
        ///
        /// `Ui`: crate::ui::traits::RawUi
        pub const fn $ul(mut self) -> Form {
            self.style.underline_color = Some($color);
            self.ul = None;
            self
        }
    };
}

/// A style for text.
#[derive(Default, Clone, Copy)]
pub struct Form {
    /// The actual [style](ContentStyle) that is applied
    pub style: ContentStyle,
    fg: Option<Color>,
    bg: Option<Color>,
    ul: Option<Color>,
    kind: FormKind,
}

#[rustfmt::skip]
impl Form {
    mimic_method!(/**bold*/ bold Attribute::Bold);
    mimic_method!(/**dim*/ dim Attribute::Dim);
    mimic_method!(/**italic*/ italic Attribute::Italic);
    mimic_method!(/**underlined*/ underlined Attribute::Underlined);
    mimic_method!(/**double_underlined*/ double_underlined Attribute::DoubleUnderlined);
    mimic_method!(/**undercurled*/ undercurled Attribute::Undercurled);
    mimic_method!(/**underdashed*/ underdashed Attribute::Underdashed);
    mimic_method!(/**reverse*/ reverse Attribute::Reverse);
    mimic_method!(/**crossed_out*/ crossed_out Attribute::CrossedOut);
    mimic_method!(/**black*/ black on_black underline_black CtColor::Black);
    mimic_method!(/**dark_grey*/ dark_grey on_dark_grey underline_dark_grey CtColor::DarkGrey);
    mimic_method!(/**red*/ red on_red underline_red CtColor::Red);
    mimic_method!(/**dark_red*/ dark_red on_dark_red underline_dark_red CtColor::DarkRed);
    mimic_method!(/**green*/ green on_green underline_green CtColor::Green);
    mimic_method!(
        /**dark_green*/ dark_green on_dark_green underline_dark_green CtColor::DarkGreen
    );
    mimic_method!(/**yellow*/ yellow on_yellow underline_yellow CtColor::Yellow);
    mimic_method!(
        /**dark_yellow*/ dark_yellow on_dark_yellow underline_dark_yellow CtColor::DarkYellow
    );
    mimic_method!(/**blue*/ blue on_blue underline_blue CtColor::Blue);
    mimic_method!(/**dark_blue*/ dark_blue on_dark_blue underline_dark_blue CtColor::DarkBlue);
    mimic_method!(/**magenta*/ magenta on_magenta underline_magenta CtColor::Magenta);
    mimic_method!(
        /**dark_magenta*/ dark_magenta on_dark_magenta underline_dark_magenta CtColor::DarkMagenta
    );
    mimic_method!(/**cyan*/ cyan on_cyan underline_cyan CtColor::Cyan);
    mimic_method!(/**dark_cyan*/ dark_cyan on_dark_cyan underline_dark_cyan CtColor::DarkCyan);
    mimic_method!(/**white*/ white on_white underline_white CtColor::White);
    mimic_method!(/**grey*/ grey on_grey underline_grey CtColor::Grey);
}

impl Form {
    /// Returns a new `Form` with a default style.
    pub const fn new() -> Form {
        Self {
            style: default_style(),
            fg: None,
            bg: None,
            ul: None,
            kind: FormKind::Normal,
        }
    }

    /// A `Form` value, from the name of the form.
    pub fn of(form_name: impl AsRef<str>) -> Form {
        let mut form = from_id(id_of_non_static(form_name.as_ref()));
        form.kind = FormKind::Normal;
        form
    }

    /// A `Form` that mimics another.
    ///
    /// This is useful if you want `Form`s to automatically change if
    /// the mimicked one does.
    ///
    /// Normally, this is done automatically. For example, if you
    /// define a `Form` "foo.bar.baz" through [`form::id_of!`], or
    /// within the [`txt!`] macro, then that `Form` will be set to
    /// "mimic" "foo.bar". That is, if "foo.bar" changes, so will
    /// "foo.bar.baz".
    ///
    /// This function lets you manually do that.
    pub fn mimic(form_name: impl AsRef<str>) -> Form {
        let id = id_of_non_static(form_name.as_ref());
        let mut form = from_id(id);
        form.kind = FormKind::Ref(id.0, default_style());
        form
    }

    /// Sets the [`Reset`] attribute.
    ///
    /// In Duat, the [`Reset`] attribute should remove only other
    /// [`Attribute`]s, not any of the colors in use.
    ///
    /// [`Reset`]: Attribute::Reset
    pub const fn reset(mut self) -> Form {
        self.style.attributes = self.style.attributes.with(Attribute::Reset);

        if let FormKind::Ref(_, style) = &mut self.kind {
            style.attributes = style.attributes.with(Attribute::Reset);
        }

        self
    }

    /// Sets the color of the foreground.
    ///
    /// This color is derived from a [`Color`] struct, which is
    /// created via [`Color::new`], or set via [`Color::set`], letting
    /// you change the color of every `Form` that uses the same color.
    ///
    /// If this `Form` was created via [`Form::mimic`], then the other
    /// attributes will change as the mimicked color does, but the
    /// foreground won't.
    pub fn with(mut self, color: Color) -> Form {
        let color_value = COLORS.lock().unwrap()[color.as_u16() as usize];
        self.style.foreground_color = Some(color_value);
        self.fg = Some(color);

        if let FormKind::Ref(_, style) = &mut self.kind {
            style.foreground_color = Some(color_value);
        }

        self
    }

    /// New `Form` with a colored background.
    ///
    /// This color is derived from a [`Color`] struct, which is
    /// created via [`Color::new`], or set via [`Color::set`], letting
    /// you change the color of every `Form` that uses the same color.
    ///
    /// If this `Form` was created via [`Form::mimic`], then the other
    /// attributes will change as the mimicked color does, but the
    /// background won't.
    pub fn on(mut self, color: Color) -> Form {
        let color_value = COLORS.lock().unwrap()[color.as_u16() as usize];
        self.style.background_color = Some(color_value);
        self.bg = Some(color);

        if let FormKind::Ref(_, style) = &mut self.kind {
            style.background_color = Some(color_value);
        }

        self
    }

    /// New `Form` with a colored underline..
    ///
    /// Note that this doesn't actually make the underline show up, it
    /// merely colors one that is set via a command like
    /// [`Form::underlined`].
    ///
    /// This color is derived from a [`Color`] struct, which is
    /// created via [`Color::new`], or set via [`Color::set`], letting
    /// you change the color of every `Form` that uses the same color.
    ///
    /// If this `Form` was created via [`Form::mimic`], then the other
    /// attributes will change as the mimicked color does, but the
    /// underline color won't.
    pub fn underline(mut self, color: Color) -> Form {
        let color_value = COLORS.lock().unwrap()[color.as_u16() as usize];
        self.style.underline_color = Some(color_value);
        self.ul = Some(color);

        if let FormKind::Ref(_, style) = &mut self.kind {
            style.underline_color = Some(color_value);
        }

        self
    }

    /// The foreground color.
    const fn fg(&self) -> Option<CtColor> {
        self.style.foreground_color
    }

    /// The background color.
    const fn bg(&self) -> Option<CtColor> {
        self.style.background_color
    }

    /// The foreground color.
    const fn ul(&self) -> Option<CtColor> {
        self.style.underline_color
    }

    /// The attributes.
    const fn attrs(&self) -> Attributes {
        self.style.attributes
    }
}

impl PartialEq for Form {
    fn eq(&self, other: &Self) -> bool {
        self.style == other.style
            && self.fg == other.fg
            && self.bg == other.bg
            && self.ul == other.ul
    }
}

impl Eq for Form {}

/// The list of forms to be used when rendering.
///
/// ONLY MEANT TO BE ACCESSED BY DUAT AND DUAT-CORE.
#[derive(Debug)]
#[doc(hidden)]
pub struct Palette(RwLock<InnerPalette>);

impl Palette {
    /// Returns a new instance of [`FormPalette`]
    fn new() -> Self {
        let main_cursor = Some(CursorShape::DefaultUserShape);
        Self(RwLock::new(InnerPalette {
            main_cursor,
            extra_cursor: main_cursor,
            forms: BASE_FORMS
                .iter()
                .map(|(str, form)| (str.to_string(), *form))
                .collect(),
            masks: vec![("".to_string(), (0..BASE_FORMS.len() as u16).collect())],
        }))
    }

    /// Sets a [`Form`]
    fn set_form(&self, name: impl AsRef<str>, form: Form) {
        let name = name.as_ref();
        self.0.write().unwrap().set_form(name, form);
    }

    /// Sets a [`Form`] "weakly"
    fn set_weak_form(&self, name: impl AsRef<str>, form: Form) {
        let name = name.as_ref();
        self.0.write().unwrap().set_weak_form(name, form);
    }

    /// Makes a [`Form`] reference another
    fn set_ref(&self, name: impl AsRef<str>, refed: u16, override_style: ContentStyle) {
        let name = name.as_ref();
        self.0.write().unwrap().set_ref(name, refed, override_style);
    }

    /// Makes a [`Form`] reference another "weakly"
    fn set_weak_ref(&self, name: impl AsRef<str>, refed: u16, override_style: ContentStyle) {
        let name = name.as_ref();
        let mut inner_palette = self.0.write().unwrap();
        inner_palette.set_weak_ref(name, refed, override_style);
    }

    /// Sets many [`Form`]s
    fn set_many<S: AsRef<str>>(&self, sets: &[(S, Option<Form>)]) {
        let mut inner = self.0.write().unwrap();
        for (name, form) in sets {
            let Some(form) = *form else {
                position_and_form(&mut inner.forms, name);
                continue;
            };

            match form.kind {
                FormKind::Normal => inner.set_form(name.as_ref(), form),
                FormKind::Ref(refed, style) => inner.set_ref(name.as_ref(), refed, style),
                FormKind::Weakest => inner.set_weak_form(name.as_ref(), form),
                FormKind::WeakestRef(refed, style) => {
                    inner.set_weak_ref(name.as_ref(), refed, style)
                }
            }
        }
    }

    /// Returns a form, given a [`FormId`].
    fn form_from_id(&self, id: FormId) -> Option<Form> {
        let inner = self.0.read().unwrap();
        inner.forms.get(id.0 as usize).map(|(_, form)| *form)
    }

    /// The [`Form`] and [`CursorShape`] of the main cursor
    fn main_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_from_id(M_CAR_ID).unwrap();
        (form, self.0.read().unwrap().main_cursor)
    }

    /// The [`Form`] and [`CursorShape`] of extra cursors
    fn extra_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_from_id(E_CAR_ID).unwrap();
        (form, self.0.read().unwrap().extra_cursor)
    }

    /// Sets the [`CursorShape`] of the main cursor
    fn set_main_cursor(&self, shape: CursorShape) {
        self.0.write().unwrap().main_cursor = Some(shape);
        if let Some(sender) = SENDER.get() {
            sender.send(DuatEvent::FormChange);
        }
    }

    /// Sets the [`CursorShape`] of extra cursors
    fn set_extra_cursor(&self, shape: CursorShape) {
        self.0.write().unwrap().extra_cursor = Some(shape);
        if let Some(sender) = SENDER.get() {
            sender.send(DuatEvent::FormChange);
        }
    }

    /// Unsets the [`CursorShape`] of the main cursor
    fn unset_main_cursor(&self) {
        self.0.write().unwrap().main_cursor = None;
        if let Some(sender) = SENDER.get() {
            sender.send(DuatEvent::FormChange);
        }
    }

    /// Unsets the [`CursorShape`] of the extra cursors
    fn unset_extra_cursor(&self) {
        self.0.write().unwrap().extra_cursor = None;
        if let Some(sender) = SENDER.get() {
            sender.send(DuatEvent::FormChange);
        }
    }

    /// Returns a [`Painter`]
    fn painter(&'static self, default_id: FormId, mask: &str) -> Painter {
        let inner = self.0.read().unwrap();
        let mask_i = inner
            .masks
            .iter()
            .position(|(m, _)| *m == mask)
            .unwrap_or_default();

        let default = inner
            .forms
            .get(match inner.masks[mask_i].1.get(default_id.0 as usize) {
                Some(i) => *i as usize,
                None => default_id.0 as usize,
            })
            .map(|(_, f)| *f)
            .unwrap_or(Form::new());

        Painter {
            inner,
            mask_i,
            default,
            parts: PainterParts::default(),
            main_parts: None,
        }
    }
}

struct InnerPalette {
    main_cursor: Option<CursorShape>,
    extra_cursor: Option<CursorShape>,
    forms: Vec<(String, Form)>,
    masks: Vec<(String, Vec<u16>)>,
}

impl InnerPalette {
    /// Sets a [`Form`]
    fn set_form(&mut self, name: &str, form: Form) {
        let (i, _) = position_and_form(&mut self.forms, name);

        self.forms[i].1 = form;

        for (referee, override_style) in refs_of(self, i) {
            mimic_form_to_referee(&mut self.forms[referee].1, form, override_style);
        }

        if let Some(sender) = SENDER.get() {
            sender.send(DuatEvent::FormChange);
        }

        mask_form(name, i, self);

        let form_set = FormSet((self.forms[i].0.clone(), FormId(i as u16), form));
        context::queue(move |pa| _ = hook::trigger(pa, form_set));
    }

    /// Sets a [`Form`] "weakly"
    fn set_weak_form(&mut self, name: &str, form: Form) {
        let (i, _) = position_and_form(&mut self.forms, name);

        let (_, f) = &mut self.forms[i];
        if let FormKind::Weakest | FormKind::WeakestRef(..) = f.kind {
            *f = form;
            f.kind = FormKind::Normal;

            if let Some(sender) = SENDER.get() {
                sender.send(DuatEvent::FormChange);
            }
            for (referee, override_style) in refs_of(self, i) {
                mimic_form_to_referee(&mut self.forms[referee].1, form, override_style);
            }

            mask_form(name, i, self);
        }
    }

    /// Makes a [`Form`] reference another
    fn set_ref(&mut self, name: &str, refed: u16, override_style: ContentStyle) {
        let (_, form) = self.forms[refed as usize];
        let (i, _) = position_and_form(&mut self.forms, name);

        self.forms[i].1 = form;
        for (referee, override_style) in refs_of(self, i) {
            mimic_form_to_referee(&mut self.forms[referee].1, form, override_style);
        }

        // If it would be circular, we just don't reference anything.
        if would_be_circular(self, i, refed as usize) {
            self.forms[i].1.kind = FormKind::Normal;
        } else {
            self.forms[i].1.kind = FormKind::Ref(refed, override_style);
        }

        if let Some(sender) = SENDER.get() {
            sender.send(DuatEvent::FormChange);
        }

        mask_form(name, i, self);
        let form_set = FormSet((self.forms[i].0.clone(), FormId(i as u16), form));
        context::queue(move |pa| _ = hook::trigger(pa, form_set));
    }

    /// Makes a [`Form`] reference another "weakly"
    fn set_weak_ref(&mut self, name: &str, refed: u16, override_style: ContentStyle) {
        let (_, form) = self.forms[refed as usize];
        let (i, _) = position_and_form(&mut self.forms, name);

        // For weak refs, no checks are done, since a form is only set if it
        // doesn't exist, and for there to be refs to it, it must exist.
        let (_, f) = &mut self.forms[i];
        if let FormKind::Weakest | FormKind::WeakestRef(..) = f.kind {
            *f = form;
            f.kind = FormKind::WeakestRef(refed, override_style);

            if let Some(sender) = SENDER.get() {
                sender.send(DuatEvent::FormChange);
            }
            for (referee, override_style) in refs_of(self, i) {
                mimic_form_to_referee(&mut self.forms[referee].1, form, override_style);
            }

            mask_form(name, i, self);
        }
    }
}

fn mimic_form_to_referee(referee: &mut Form, form: Form, override_style: ContentStyle) {
    referee.style = form.style;
    referee.style.attributes.extend(override_style.attributes);
    if let Some(color) = override_style.foreground_color {
        referee.style.foreground_color = Some(color);
    }
    if let Some(color) = override_style.background_color {
        referee.style.background_color = Some(color);
    }
    if let Some(color) = override_style.underline_color {
        referee.style.underline_color = Some(color);
    }
}

/// If setting a form with an existing mask suffix, mask its prefix
fn mask_form(name: &str, form_i: usize, inner: &mut InnerPalette) {
    if inner.masks[0].1.len() < inner.forms.len() {
        for (_, remaps) in inner.masks.iter_mut() {
            remaps.extend(remaps.len() as u16..inner.forms.len() as u16);
        }
    }

    if let Some((pref, mask)) = name.rsplit_once(".")
        && let Some((_, remaps)) = inner.masks.iter_mut().find(|(m, _)| *m == mask)
        && let Some(j) = inner.forms.iter().position(|(name, ..)| *name == pref)
    {
        remaps[j] = form_i as u16;
    }
}

/// A struct to create [`Form`]s from [`RawTag`] in a [`Text`]
///
/// This [`Painter`] not only prints the [`Form`]s in the [`Text`],
/// but within it there is also a "mask". This mask will remap
/// [`Form`]s based on suffix, like in the following example:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # let (tx, rx) = duat_core::context::duat_channel();
/// # duat_core::context::set_sender(tx);
/// # duat_core::form::set_initial(duat_core::form::get_initial());
/// use duat::prelude::*;
///
/// let mut text = Text::new();
///
/// // Assume that a Form with the given name exists
/// form::set("my_form", Form::new().red().on_blue());
///
/// // If I create a second Form like this one, they are separate
/// form::set("my_form.suffix", Form::new().undercurled());
///
/// text = txt!("[my_form]This text is red on blue[], [my_form.suffix]undercurled");
///
/// // But if I enable the "suffix" mask that's at the end of the second Form
/// form::enable_mask("suffix");
///
/// // After calling `handle.set_mask("suffix")` on the Handle that owns this
/// // Text, it will be equivalent to this:
///
/// text = txt!("[my_form.suffix]This text is red on blue[], [my_form.suffix]undercurled");
/// ```
///
/// Masks can serve a myriad of different purposes, but here's a
/// few:
///
/// - When you want to temporarily change the [`Form`]s on a single
///   [`Widget`]. This is, for example, used in the [`Notifications`]
///   [`Widget`], which maps [`Form`]s in order to correspond to the
///   [`Level`] of their severity.
/// - When you want to have [`Widget`]s change [`Form`] based on
///   [hooks], so you could have, for example, an `"inactive"` mask
///   for your [`Buffer`]s
/// - If you want to quickly cycle through [`Form`]s in a [`Text`],
///   this is the most efficient way of doing that, since it relies on
///   static remaps, not on changing the [`Form`]s themselves.
///
/// Do note that no suffix, except `"error"`, `"warn"` and
/// `"info"` is a mask when Duat starts. In order to enable more
/// masks, see [`enable_mask`].
///
/// [`Widget`]: crate::ui::Widget
/// [`Notifications`]: https://docs.rs/duat/latest/duat
/// [`Level`]: crate::context::Level
/// [hooks]: crate::hook
/// [`Buffer`]: crate::buffer::Buffer
/// [`Text`]: crate::text::Text
///
/// [`RawTag`]: crate::text::RawTag
/// [`Text`]: crate::text::Text
pub struct Painter {
    inner: RwLockReadGuard<'static, InnerPalette>,
    mask_i: usize,
    default: Form,
    parts: PainterParts,
    main_parts: Option<PainterParts>,
}

impl Painter {
    /// Applies the `Form` with the given `id` and returns the result,
    /// given previous triggers.
    ///
    /// Will return a [`Form`] _relative_ to what the previous
    /// [`Form`] was, that is, if the new [`Form`] doesn't include a
    /// background, its combination with the other [`Form`]s also
    /// won't, since it wasn't changed.
    #[inline(always)]
    pub fn apply(&mut self, id: FormId, prio: u8) {
        let (_, mask) = &self.inner.masks[self.mask_i];
        let id = FormId(mask.get(id.0 as usize).copied().unwrap_or(id.0));

        let forms = &self.inner.forms;
        // SAFETY: When you create a form, it gets indexed, and never becomes
        // unindexed, so this should be fine.
        let form = unsafe { forms.get(id.0 as usize).map(|(_, f)| *f).unwrap_unchecked() };

        let gt = |(.., p): &&(_, _, u8)| *p > prio;
        let i = self.parts.forms.len() - self.parts.forms.iter().rev().take_while(gt).count();
        self.parts.forms.insert(i, (form, id, prio));

        self.parts.set_fg |= form.fg().is_some();
        self.parts.set_bg |= form.bg().is_some();
        self.parts.set_ul |= form.ul().is_some();
        self.parts.reset_attrs |= form.attrs().has(Attribute::Reset);
    }

    /// Removes the [`Form`] with the given `id` and returns the
    /// result, given previous triggers
    #[inline(always)]
    pub fn remove(&mut self, id: FormId) {
        let mask = &self.inner.masks[self.mask_i].1;
        let id = FormId(mask.get(id.0 as usize).copied().unwrap_or(id.0));

        let mut applied_forms = self.parts.forms.iter().enumerate();
        if let Some((i, &(form, ..))) = applied_forms.rfind(|(_, (_, lhs, _))| *lhs == id) {
            self.parts.forms.remove(i);

            self.parts.set_fg |= form.fg().is_some();
            self.parts.set_bg |= form.bg().is_some();
            self.parts.set_ul |= form.ul().is_some();
            self.parts.reset_attrs |= !form.attrs().is_empty();
        };
    }

    /// Removes all [`Form`]s except the default one
    ///
    /// Should be used when a [`ResetState`] part is printed
    ///
    /// [`ResetState`]: crate::text::TextPart::ResetState
    #[inline(always)]
    pub fn reset(&mut self) -> ContentStyle {
        self.parts.forms.clear();
        self.absolute_style()
    }

    /// Generates the absolute [`ContentStyle`] to be set
    ///
    /// This function assumes that all previous styling is not being
    /// carried over, i.e., we're styling from scratch.
    #[inline(always)]
    pub fn absolute_style(&self) -> ContentStyle {
        let mut style = self.default.style;

        for &(form, ..) in &self.parts.forms {
            style.foreground_color = form.fg().or(style.foreground_color);
            style.background_color = form.bg().or(style.background_color);
            style.underline_color = form.ul().or(style.underline_color);
            style.attributes = if form.attrs().has(Attribute::Reset) {
                form.attrs()
            } else {
                form.attrs() | style.attributes
            }
        }

        style
    }

    /// Generates the relative [`ContentStyle`] to be set
    ///
    /// This function assumes that previously printed styles are being
    /// carried over, influencing this one.
    ///
    /// You should strive to use this function more than
    /// [`absolute_style`], since it "theoretically" should be less
    /// work to change just one aspect of the style, rather than
    /// replacing the whole thing.
    ///
    /// [`absolute_style`]: Painter::absolute_style
    #[inline(always)]
    pub fn relative_style(&mut self) -> Option<ContentStyle> {
        let abs_style = self.absolute_style();
        let mut style = abs_style;

        if style.attributes.has(Attribute::Reset) || self.parts.reset_attrs {
            style.attributes.set(Attribute::Reset);
        // Only when we are certain that all forms have been
        // printed, can we cull unnecessary colors for efficiency
        // (this happens most of the time).
        } else {
            style.foreground_color = self
                .parts
                .set_fg
                .then_some(style.foreground_color.unwrap_or(CtColor::Reset))
                .filter(|fg| Some(*fg) != self.parts.prev_style.and_then(|s| s.foreground_color));
            style.background_color = self
                .parts
                .set_bg
                .then_some(style.background_color.unwrap_or(CtColor::Reset))
                .filter(|bg| Some(*bg) != self.parts.prev_style.and_then(|s| s.background_color));
            style.underline_color = self
                .parts
                .set_ul
                .then_some(style.underline_color.unwrap_or(CtColor::Reset))
                .filter(|ul| Some(*ul) != self.parts.prev_style.and_then(|s| s.underline_color));
        }

        self.parts.set_fg = false;
        self.parts.set_bg = false;
        self.parts.set_ul = false;
        self.parts.reset_attrs = false;

        if let Some(prev_style) = self.parts.prev_style.replace(abs_style) {
            (style != prev_style && style != Default::default()).then_some(style)
        } else {
            Some(style)
        }
    }

    /// Makes it so the next call to [`relative_style`] returns the
    /// same thing as a call to [`absolute_style`]
    ///
    /// [`relative_style`]: Self::relative_style
    /// [`absolute_style`]: Self::absolute_style
    pub fn reset_prev_style(&mut self) {
        self.parts.prev_style = None;
        self.parts.set_fg = true;
        self.parts.set_bg = true;
        self.parts.set_ul = true;
        self.parts.reset_attrs = true;
    }

    /// Applies the `"caret.main"` [`Form`]
    #[inline(always)]
    pub fn apply_main_selection(&mut self, is_caret: bool, start_range: bool) {
        if is_caret {
            self.apply(M_CAR_ID, 100);
        }
        if start_range {
            self.apply(M_SEL_ID, 95);
        }
    }

    /// Removes the `"caret.main"` [`Form`]
    #[inline(always)]
    pub fn remove_main_selection(&mut self, is_caret: bool, end_range: bool) {
        if is_caret {
            self.remove(M_CAR_ID);
        }
        if end_range {
            self.remove(M_SEL_ID);
        }
    }

    /// Applies the `"caret.extra"` [`Form`]
    #[inline(always)]
    pub fn apply_extra_selection(&mut self, is_caret: bool, start_range: bool) {
        if is_caret {
            self.apply(E_CAR_ID, 100);
        }
        if start_range {
            self.apply(E_SEL_ID, 95);
        }
    }

    /// Removes the `"caret.extra"` [`Form`]
    #[inline(always)]
    pub fn remove_extra_selection(&mut self, is_caret: bool, end_range: bool) {
        if is_caret {
            self.remove(E_CAR_ID);
        }
        if end_range {
            self.remove(E_SEL_ID);
        }
    }

    /// Prepares this `Painter` to print an [`Inlay`] text
    ///
    /// [`Inlay`]: crate::text::TextPart::Inlay
    pub fn prepare_for_inlay(&mut self) {
        self.main_parts = Some(std::mem::take(&mut self.parts));
    }

    /// Prepares this `Painter` to print an [`Inlay`] text
    ///
    /// [`Inlay`]: crate::text::TextPart::Inlay
    ///
    /// # Panics
    ///
    /// Panics if [`prepare_for_inlay`] wasn't called first.
    ///
    /// [`prepare_for_inlay`]: Self::prepare_for_inlay
    #[track_caller]
    pub fn return_from_inlay(&mut self) {
        self.parts = self.main_parts.take().unwrap();
    }

    /// The [`Form`] "caret.extra", and its shape.
    pub fn main_cursor(&self) -> Option<CursorShape> {
        self.inner.main_cursor
    }

    /// The [`Form`] "caret.extra", and its shape.
    pub fn extra_cursor(&self) -> Option<CursorShape> {
        self.inner.extra_cursor
    }

    /// The `"default"` form's [`Form`]
    pub fn get_default(&self) -> Form {
        self.default
    }
}

struct PainterParts {
    forms: Vec<(Form, FormId, u8)>,
    set_fg: bool,
    set_bg: bool,
    set_ul: bool,
    reset_attrs: bool,
    prev_style: Option<ContentStyle>,
}

impl Default for PainterParts {
    fn default() -> Self {
        Self {
            forms: Vec::new(),
            set_fg: true,
            set_bg: true,
            set_ul: true,
            reset_attrs: true,
            prev_style: None,
        }
    }
}

pub(crate) fn set_sender(sender: DuatSender) {
    SENDER
        .set(sender)
        .unwrap_or_else(|_| panic!("Sender set more than once"));
}

/// An enum that helps in the modification of forms
#[derive(Default, Clone, Copy)]
enum FormKind {
    #[default]
    Normal,
    Ref(u16, ContentStyle),
    Weakest,
    WeakestRef(u16, ContentStyle),
}

/// The position of each form that eventually references the `n`th
fn refs_of(inner: &InnerPalette, refed: usize) -> Vec<(usize, ContentStyle)> {
    let mut refs = Vec::new();
    for (i, (_, form)) in inner.forms.iter().enumerate() {
        if let FormKind::Ref(id, style) | FormKind::WeakestRef(id, style) = form.kind
            && id as usize == refed
        {
            refs.push((i, style));
            refs.extend(refs_of(inner, i));
        }
    }
    refs
}

/// If form references would eventually lead to a loop
fn would_be_circular(inner: &InnerPalette, referee: usize, refed: usize) -> bool {
    if let FormKind::Ref(id, _) | FormKind::WeakestRef(id, _) = inner.forms[refed].1.kind {
        match id as usize == referee {
            true => true,
            false => would_be_circular(inner, referee, id as usize),
        }
    } else {
        false
    }
}

fn position_and_form(forms: &mut Vec<(String, Form)>, name: impl AsRef<str>) -> (usize, Form) {
    let name = name.as_ref();
    if let Some((i, (_, form))) = forms.iter().enumerate().find(|(_, (lhs, _))| *lhs == name) {
        (i, *form)
    } else if let Some((refed, _)) = name.rsplit_once('.') {
        let (i, mut form) = position_and_form(forms, refed);
        form.kind = FormKind::WeakestRef(i as u16, default_style());
        forms.push((name.to_string(), form));
        (forms.len() - 1, form)
    } else {
        let mut form = Form::new();
        form.kind = FormKind::Weakest;
        forms.push((name.to_string(), form));
        (forms.len() - 1, form)
    }
}

/// Converts a string to a color, supporst hex, RGB and HSL
const fn str_to_color(str: &str) -> std::result::Result<CtColor, &'static str> {
    const fn strip_prefix<'a>(prefix: &str, str: &'a str) -> Option<&'a str> {
        let prefix = prefix.as_bytes();

        let mut i = 0;
        while i < prefix.len() {
            if str.as_bytes()[i] != prefix[i] {
                return None;
            }
            i += 1;
        }

        Some(str.split_at(prefix.len()).1)
    }
    const fn strip_suffix<'a>(suffix: &str, str: &'a str) -> Option<&'a str> {
        let prefix = suffix.as_bytes();

        let mut i = str.len() - 1;
        while i >= str.len() - prefix.len() {
            if str.as_bytes()[i] != prefix[i - (str.len() - prefix.len())] {
                return None;
            }
            i += 1;
        }

        Some(str.split_at(str.len() - suffix.len()).0)
    }
    const fn split_space(str: &str) -> Option<(&str, &str)> {
        if str.is_empty() {
            return None;
        }

        let mut i = 0;
        while i < str.len() {
            if str.as_bytes()[i] == b' ' {
                break;
            }
            i += 1;
        }

        let (cut, rest) = str.split_at(i);
        let (_, rest) = rest.split_at(if rest.is_empty() { 0 } else { 1 });
        Some((cut, rest))
    }
    const fn hue_to_rgb(p: f32, q: f32, mut t: f32) -> f32 {
        t = if t < 0.0 { t + 1.0 } else { t };
        t = if t > 1.0 { t - 1.0 } else { t };
        if t < 1.0 / 6.0 {
            p + (q - p) * 6.0 * t
        } else if t < 1.0 / 2.0 {
            q
        } else if t < 2.0 / 3.0 {
            p + (q - p) * (2.0 / 3.0 - t) * 6.0
        } else {
            p
        }
    }

    // Expects "#{red:x}{green:x}{blue:x}"
    if let Some(hex) = strip_prefix("#", str) {
        let total = match u32::from_str_radix(hex, 16) {
            Ok(total) if hex.len() == 6 => total,
            _ => return Err("Hexcode does not contain 6 hex values"),
        };
        let r = (total >> 16) as u8;
        let g = (total >> 8) as u8;
        let b = total as u8;

        Ok(CtColor::Rgb { r, g, b })
    // Expects "hsl {hue%?} {saturation%?} {lightness%?}"
    } else if let Some(mut hsl) = strip_prefix("hsl ", str) {
        let mut values = [0.0, 0.0, 0.0];
        let mut i = 0;
        while i < values.len() {
            if let Some((cut, rest)) = split_space(hsl) {
                hsl = rest;
                let (num, div) = match strip_suffix("%", cut) {
                    Some(perc) => (perc, 100),
                    None => (cut, 255),
                };
                values[i] = match u8::from_str_radix(num, 10) {
                    Ok(value) if value <= div => value as f32 / div as f32,
                    _ => return Err("Hsl format property could not be parsed"),
                }
            } else {
                return Err("Missing value in hsl format");
            }
            i += 1;
        }
        let [hue, sat, lit] = values;

        let (r, g, b) = if sat == 0.0 {
            (lit, lit, lit)
        } else {
            let q = if lit < 0.5 {
                lit * (1.0 + sat)
            } else {
                lit + sat - lit * sat
            };
            let p = 2.0 * lit - q;
            let r = hue_to_rgb(p, q, hue + 1.0 / 3.0);
            let g = hue_to_rgb(p, q, hue);
            let b = hue_to_rgb(p, q, hue - 1.0 / 3.0);
            (r, g, b)
        };

        // + 0.5 because `as` rounds floats down.
        let r = (0.5 + r * 255.0) as u8;
        let g = (0.5 + g * 255.0) as u8;
        let b = (0.5 + b * 255.0) as u8;
        Ok(CtColor::Rgb { r, g, b })
    } else {
        Err("Color format was not recognized")
    }
}

/// Returns the default [`ContentStyle`].
const fn default_style() -> ContentStyle {
    ContentStyle {
        foreground_color: None,
        background_color: None,
        underline_color: None,
        attributes: Attributes::none(),
    }
}

impl std::fmt::Debug for Form {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct DebugColor(Option<CtColor>);
        impl std::fmt::Debug for DebugColor {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.0 {
                    Some(CtColor::Rgb { r, g, b }) => write!(f, "Some(Rgb({r}, {g}, {b}))"),
                    Some(CtColor::AnsiValue(ansi)) => write!(f, "Some(Ansi({ansi}))"),
                    Some(color) => write!(f, "Some({color:?})"),
                    None => f.write_str("None"),
                }
            }
        }

        struct DebugAttributes(Attributes);
        impl std::fmt::Debug for DebugAttributes {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.0.is_empty() {
                    f.write_str("None")
                } else {
                    let mut is_first = true;
                    for attr in Attribute::iterator() {
                        if self.0.has(attr) {
                            if !is_first {
                                f.write_str(" | ")?;
                            }
                            is_first = false;
                            write!(f, "{attr:?}")?;
                        }
                    }
                    Ok(())
                }
            }
        }

        f.debug_struct("Form")
            .field("fg", &DebugColor(self.style.foreground_color))
            .field("bg", &DebugColor(self.style.background_color))
            .field("ul", &DebugColor(self.style.underline_color))
            .field("attr", &DebugAttributes(self.style.attributes))
            .finish()
    }
}

impl std::fmt::Debug for InnerPalette {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct DebugForms<'a>(&'a [(String, Form)]);
        impl std::fmt::Debug for DebugForms<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if f.alternate() {
                    f.write_str("[\n")?;
                    let max = self.0.len().ilog10() as usize + 3;
                    for (n, (name, form)) in self.0.iter().enumerate() {
                        let num = format!("{n}:");
                        writeln!(f, "{num:<max$}({name}, {form:#?})")?;
                    }
                    f.write_str("]")
                } else {
                    write!(f, "{:?}", self.0)
                }
            }
        }

        struct DebugCursorShape(CursorShape);
        impl std::fmt::Debug for DebugCursorShape {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.0 {
                    CursorShape::DefaultUserShape => f.write_str("DefaultUserShape"),
                    CursorShape::BlinkingBlock => f.write_str("BlinkingBlock"),
                    CursorShape::SteadyBlock => f.write_str("SteadyBlock"),
                    CursorShape::BlinkingUnderScore => f.write_str("BlinkingUnderScore"),
                    CursorShape::SteadyUnderScore => f.write_str("SteadyUnderScore"),
                    CursorShape::BlinkingBar => f.write_str("BlinkingBar"),
                    CursorShape::SteadyBar => f.write_str("SteadyBar"),
                }
            }
        }

        f.debug_struct("InnerPalette")
            .field("main_cursor", &self.main_cursor.map(DebugCursorShape))
            .field("extra_cursor", &self.extra_cursor.map(DebugCursorShape))
            .field("forms", &DebugForms(&self.forms))
            .field("masks", &self.masks)
            .finish()
    }
}

impl std::fmt::Debug for FormKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Normal => write!(f, "Normal"),
            Self::Ref(refed, _) => write!(f, "Ref({refed})"),
            Self::Weakest => write!(f, "Weakest"),
            Self::WeakestRef(refed, _) => write!(f, "WeakestRef({refed})"),
        }
    }
}

/// The [`FormId`] of `"default"`
pub const DEFAULT_ID: FormId = FormId(0);
/// The [`FormId`] of `"accent"`
pub const ACCENT_ID: FormId = FormId(1);
/// The [`FormId`] of `"caret.main"`
pub const M_CAR_ID: FormId = FormId(2);
/// The [`FormId`] of `"caret.extra"`
pub const E_CAR_ID: FormId = FormId(3);
/// The [`FormId`] of `"slection.main"`
pub const M_SEL_ID: FormId = FormId(4);
/// The [`FormId`] of `"selection.extra"`
pub const E_SEL_ID: FormId = FormId(5);
/// The [`FormId`] of `"character.control"`
pub const CONTROL_CHAR_ID: FormId = FormId(7);
