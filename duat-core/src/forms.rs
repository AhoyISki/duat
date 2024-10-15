//! Utilities for stylizing the text of Duat
use std::sync::LazyLock;

use crossterm::style::{Attribute, ContentStyle, Stylize};
pub use crossterm::{cursor::SetCursorStyle as CursorShape, style::Color};
use parking_lot::{RwLock, RwLockWriteGuard};

pub use self::global::{
    FormFmt, extra_cursor, from_id, main_cursor, name_from_id, painter, set, set_extra_cursor,
    set_main_cursor, set_weak, to_id, unset_extra_cursor, unset_main_cursor,
};
use crate::data::RwLockReadGuard;

/// The functions that will be exposed for public use.
mod global {
    use std::sync::LazyLock;

    use parking_lot::Mutex;

    use super::{CursorShape, Form, FormId, Painter, Palette};

    static PALETTE: Palette = Palette::new();
    static FORMS: LazyLock<Mutex<Vec<&str>>> = LazyLock::new(|| {
        Mutex::new(vec![
            "Default",
            "Accent",
            "DefaultOk",
            "AccentOk",
            "DefaultErr",
            "AccentErr",
            "DefaultHint",
            "AccentHint",
            "MainCursor",
            "ExtraCursor",
            "MainSelection",
            "ExtraSelection",
            "Inactive",
        ])
    });

    /// Either a [`Form`] or a name of a form
    ///
    /// Note that the referenced form does not need to exist for
    /// [`forms::set`] or [`forms::set_weak`] to work properly.
    ///
    /// [`forms::set`]: set
    /// [`forms::set_weak`]: set_weak
    pub trait FormFmt: InnerFormFmt {}
    impl FormFmt for Form {}
    impl FormFmt for &str {}

    /// Sets the [`Form`] by the name of `name`
    ///
    /// This will create a new form or replace one that already
    /// exists, and you can either set it to a [`Form`] directly, or
    /// reference another form by its name:
    ///
    /// ```rust
    /// # use duat_core::forms::{self, Form};
    /// // Creates a regular form
    /// forms::set("MyRegularForm", Form::red());
    /// // Creates a form that references the first
    /// forms::set("MyRefForm", "MyRegularForm");
    /// // Sets both "MyRegularForm" and "MyRefForm" to blue
    /// forms::set("MyRegularForm", Form::blue());
    /// ```
    ///
    /// If you are creating a plugin, or another kind of tool for
    /// others using Duat, use [`forms::set_weak`] instead of this
    /// function.
    ///
    /// [`forms::set_weak`]: set_weak
    pub fn set(name: impl ToString, form: impl FormFmt) -> FormId {
        let kind = form.kind();
        let name: &'static str = name.to_string().leak();

        match kind {
            Kind::Form(form) => crate::thread::queue(move || PALETTE.set_form(name, form)),
            Kind::Ref(refed) => crate::thread::queue(move || PALETTE.set_ref(name, refed)),
        }

        let mut forms = FORMS.lock();
        if let Kind::Ref(refed) = kind
            && !forms.contains(&refed)
        {
            forms.push(refed);
        }

        if let Some(id) = forms.iter().position(|form| *form == name) {
            FormId(id as u16)
        } else {
            forms.push(name);
            FormId(forms.len() as u16 - 1)
        }
    }

    /// Sets a form, "weakly"
    ///
    /// The difference between this function and [`forms::set`] is
    /// that this function will only trigger if the form didn't
    /// already exist.
    ///
    /// This is useful for plugins, since it prioritizes the user's
    /// preferences, no matter in what order this function and
    /// [`forms::set`] are called:
    ///
    /// ```rust
    /// use duat_core::forms::{self, Form};
    /// // Creates a form
    /// forms::set_weak("WeakForm", Form::blue().on_white());
    /// // Creates a form "strongly"
    /// forms::set("StrongForm", Form::new().bold());
    /// // Does nothing
    /// forms::set_weak("StrongForm", "Default");
    /// ```
    ///
    /// [`forms::set`]: set
    pub fn set_weak(name: impl ToString, form: impl FormFmt) -> FormId {
        let kind = form.kind();
        let name: &'static str = name.to_string().leak();

        match kind {
            Kind::Form(form) => PALETTE.set_weak_form(name, form),
            Kind::Ref(refed) => PALETTE.set_weak_ref(name, refed),
        }

        let mut forms = FORMS.lock();
        if let Kind::Ref(refed) = kind
            && !forms.contains(&refed)
        {
            forms.push(refed);
        }

        if let Some(id) = forms.iter().position(|form| *form == name) {
            FormId(id as u16)
        } else {
            forms.push(name);
            FormId(forms.len() as u16 - 1)
        }
    }

    /// Returns a [`Form`], given a [`FormId`].
    ///
    /// If you are thinking of using this for printing purposes,
    /// consider using [`forms::painter`] instead.
    ///
    /// [`forms::painter`]: painter
    pub fn from_id(id: FormId) -> Form {
        PALETTE.form_from_id(id).unwrap_or_default()
    }

    /// The name of a form, given a [`FormId`]
    pub fn name_from_id(id: FormId) -> &'static str {
        PALETTE.name_from_id(id)
    }

    /// The current main cursor, with the `"MainCursor"` [`Form`]
    pub fn main_cursor() -> (Form, Option<CursorShape>) {
        PALETTE.main_cursor()
    }

    /// The current extra cursor, with the `"ExtraCursor"` [`Form`]
    pub fn extra_cursor() -> (Form, Option<CursorShape>) {
        PALETTE.extra_cursor()
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
    /// # use duat_core::forms::{self, Form, Color};
    /// forms::set(
    ///     "MainCursor",
    ///     Form::black().on(Color::Rgb { r: 240, g: 210, b: 200 }),
    /// )
    /// ```
    ///
    /// However, if possible, Duat will still try to use the main
    /// cursor's [shape]. If you don't want that to happen, see
    /// [`forms::unset_main_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`forms::unset_main_cursor`]: unset_main_cursor
    pub fn set_main_cursor(shape: CursorShape) {
        crate::thread::queue(move || PALETTE.set_main_cursor(shape));
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
    /// # use duat_core::forms::{self, Form, Color};
    /// forms::set(
    ///     "MainCursor",
    ///     Form::black().on(Color::Rgb { r: 240, g: 210, b: 200 }),
    /// )
    /// ```
    ///
    /// However, if possible, Duat will still try to use the main
    /// cursor's [shape]. If you don't want that to happen, see
    /// [`forms::unset_extra_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`forms::unset_extra_cursor`]: unset_extra_cursor
    pub fn set_extra_cursor(shape: CursorShape) {
        crate::thread::queue(move || PALETTE.set_extra_cursor(shape));
    }

    /// Removes the main cursor's [shape]
    ///
    /// By doing this, you will force Duat to draw the main cursor by
    /// use of the `"MainCursor"` form.
    ///
    /// If you want to set the [shape] instead, see
    /// [`forms::set_main_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`forms::set_main_cursor`]: set_main_cursor
    pub fn unset_main_cursor() {
        crate::thread::queue(move || PALETTE.unset_main_cursor());
    }

    /// Removes extra cursors's [shape]s
    ///
    /// By doing this, you will force Duat to draw the extra cursor by
    /// use of the `"MainCursor"` form. Do note however that, in
    /// something like a terminal, extra cursors would never be
    /// printed as a [shape] anyways, since terminals can only
    /// print one cursor at a time.
    ///
    /// If you want to set the [shape] instead, see
    /// [`forms::set_extra_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`forms::set_extra_cursor`]: set_extra_cursor
    pub fn unset_extra_cursor() {
        crate::thread::queue(move || PALETTE.unset_extra_cursor());
    }

    /// A [`Painter`] for coloring text efficiently
    ///
    /// This function will be used primarily when printing widgets to
    /// the screen, which is something that only [`Ui`]s should be
    /// doing.
    ///
    /// One thing to note is that a [`Painter`] will lock the form
    /// palette while it is being used. This means that, if a form is
    /// changed while a widget is in the middle of printing, the
    /// printed form will be the old version, not the new one. Only
    /// after said widget is done printing will the new form come into
    /// effect.
    ///
    /// [`Ui`]: crate::ui::Ui
    pub fn painter() -> Painter {
        PALETTE.painter()
    }

    /// Returns the [`FormId`] from the name of a [`Form`]
    ///
    /// You can also pass multiple names, in order to get a list of
    /// ids.
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
    pub macro to_id {
        // Since this variable will only ever be accessed by one thread, it is perfectly reasonable to use a `static mut`, even if unsafe.
        ($form:expr) => {{
            static ID: LazyLock<FormId> = LazyLock::new(|| inner_to_id($form));
            *ID
        }},
        ($($form:expr),+) => {{
            static IDS: LazyLock<&[FormId]> = LazyLock::new(|| {
    			let mut ids = Vec::new();
    			$(
        			ids.push(inner_to_id($form));
    			)+
    			ids.leak()
			});
			*ID
        }}
    }

    /// Returns the [`FormId`] of the form's name
    pub fn inner_to_id(name: impl ToString) -> FormId {
        let name: &'static str = name.to_string().leak();

        crate::thread::queue(move || PALETTE.id_from_name(name));

        let mut forms = FORMS.lock();
        if let Some(id) = forms.iter().position(|form| *form == name) {
            FormId(id as u16)
        } else {
            forms.push(name);
            FormId(forms.len() as u16 - 1)
        }
    }

    /// A kind of [`Form`]
    #[derive(Debug, Clone, Copy)]
    enum Kind {
        Form(Form),
        Ref(&'static str),
    }

    /// So [`Form`]s and [`impl ToString`]s are arguments for [`set`]
    ///
    /// [`impl ToString`]: ToString
    trait InnerFormFmt {
        /// The kind of [`Form`] that this type represents
        fn kind(self) -> Kind;
    }

    impl InnerFormFmt for Form {
        fn kind(self) -> Kind {
            Kind::Form(self)
        }
    }

    impl InnerFormFmt for &str {
        fn kind(self) -> Kind {
            Kind::Ref(self.to_string().leak())
        }
    }
}

/// An identifier of a [`Form`]
///
/// This struct is always going to point to the same form, since those
/// cannot be destroyed.
///
/// The main use for keeping these things directly is in order to
/// modify a file's text in an efficient manner, by adding tags
/// directly, instead of using a macro like [`text!`]
///
/// [`text!`]: crate::text::text
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct FormId(u16);

/// A style for text.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Form {
    pub style: ContentStyle,
    /// Wether or not the `Form`s colors and attributes should
    /// override any that come after.
    pub is_final: bool,
}

#[rustfmt::skip]
impl Form {
    mimic_method!(on Color);
    mimic_method!(underline Color );
    mimic_method!(attribute Attribute );
    mimic_method!(reset);
    mimic_method!(bold);
    mimic_method!(underlined);
    mimic_method!(double_underlined);
    mimic_method!(undercurled);
    mimic_method!(underdashed);
    mimic_method!(reverse);
    mimic_method!(dim);
    mimic_method!(italic);
    mimic_method!(negative);
    mimic_method!(slow_blink);
    mimic_method!(rapid_blink);
    mimic_method!(hidden);
    mimic_method!(crossed_out);
    mimic_method!(on_black underline_black, black);
    mimic_method!(on_dark_grey underline_dark_grey, dark_grey);
    mimic_method!(on_red underline_red, red);
    mimic_method!(on_dark_red underline_dark_red, dark_red);
    mimic_method!(on_green underline_green, green);
    mimic_method!(on_dark_green underline_dark_green, dark_green);
    mimic_method!(on_yellow underline_yellow, yellow);
    mimic_method!(on_dark_yellow underline_dark_yellow, dark_yellow);
    mimic_method!(on_blue underline_blue, blue);
    mimic_method!(on_dark_blue underline_dark_blue, dark_blue);
    mimic_method!(on_magenta underline_magenta, magenta);
    mimic_method!(on_dark_magenta underline_dark_magenta, dark_magenta);
    mimic_method!(on_cyan underline_cyan, cyan);
    mimic_method!(on_dark_cyan underline_dark_cyan, dark_cyan);
    mimic_method!(on_white underline_white, white);
    mimic_method!(on_grey underline_grey, grey);

    pub fn with(color: Color) -> Self {
        Self { style: ContentStyle::new().with(color), is_final: false }
    }

    pub fn new() -> Self {
        Self { style: ContentStyle::new(), is_final: false }
    }

    pub fn new_final() -> Self {
        Self { style: ContentStyle::default(), is_final: true }
    }

    pub fn as_final(self) -> Self {
        Self { is_final: true, ..self }
    }
}

/// The [`FormId`] of the `"Default"` form
pub const DEFAULT_ID: FormId = FormId(0);
/// The [`FormId`] of the `"MainCursor"` form
pub const M_CUR_ID: FormId = FormId(8);
/// The [`FormId`] of the `"ExtraCursor"` form
pub const E_CUR_ID: FormId = FormId(9);
/// The [`FormId`] of the `"MainSelection"` form
pub const M_SEL_ID: FormId = FormId(10);
/// The [`FormId`] of the `"ExtraSelection"` form
pub const E_SEL_ID: FormId = FormId(11);
/// The [`FormId`] of the `"Inactive"` form
pub const INACTIVE_ID: FormId = FormId(12);

struct InnerPalette {
    main_cursor: Option<CursorShape>,
    extra_cursor: Option<CursorShape>,
    forms: Vec<(&'static str, Form, FormType)>,
}

/// The list of forms to be used when rendering.
struct Palette(LazyLock<RwLock<InnerPalette>>);

impl Palette {
    /// Returns a new instance of [`FormPalette`]
    const fn new() -> Self {
        Self(LazyLock::new(|| {
            use FormType::*;
            let main_cursor = Some(CursorShape::DefaultUserShape);

            let forms = vec![
                ("Default", Form::new(), Normal),
                ("Accent", Form::new().bold(), Normal),
                ("DefaultOk", Form::blue(), Normal),
                ("AccentOk", Form::cyan(), Normal),
                ("DefaultErr", Form::red(), Normal),
                ("AccentErr", Form::red().bold(), Normal),
                ("DefaultHint", Form::grey(), Normal),
                ("AccentHint", Form::grey().bold(), Normal),
                ("MainCursor", Form::new().reverse(), Normal),
                ("ExtraCursor", Form::new().reverse(), Ref(M_CUR_ID)),
                ("MainSelection", Form::new().on_dark_grey(), Normal),
                ("ExtraSelection", Form::new().on_dark_grey(), Ref(M_SEL_ID)),
                ("Inactive", Form::grey(), Normal),
            ];

            RwLock::new(InnerPalette {
                main_cursor,
                extra_cursor: main_cursor,
                forms,
            })
        }))
    }

    /// Sets a [`Form`]
    fn set_form(&self, name: &'static str, form: Form) {
        let form = match name {
            "MainCursor" | "ExtraCursor" => form.as_final(),
            _ => form,
        };

        let mut inner = self.0.write();

        let i = if let Some(i) = inner.forms.iter().position(|(cmp, ..)| *cmp == name) {
            inner.forms[i].1 = form;
            i
        } else {
            inner.forms.push((name, form, FormType::Normal));
            inner.forms.len() - 1
        };

        for refed in refs_of(&inner, i) {
            inner.forms[refed].1 = form;
        }
    }

    /// Sets a [`Form`] "weakly"
    fn set_weak_form(&self, name: &'static str, form: Form) {
        let form = match name {
            "MainCursor" | "ExtraCursor" => form.as_final(),
            _ => form,
        };

        let mut inner = self.0.write();

        if let Some(i) = inner.forms.iter().position(|(cmp, ..)| *cmp == name) {
            let (_, f, ty) = &mut inner.forms[i];
            if let FormType::Weakest = ty {
                *f = form;
                *ty = FormType::Normal
            }
        } else {
            inner.forms.push((name, form, FormType::Normal));
            for refed in refs_of(&inner, inner.forms.len() - 1) {
                inner.forms[refed].1 = form;
            }
        }
    }

    /// Makes a [`Form`] reference another
    fn set_ref(&self, name: &'static str, refed: impl AsRef<str>) {
        let refed = {
            let refed: &'static str = refed.as_ref().to_string().leak();
            self.id_from_name(refed)
        };

        let mut inner = self.0.write();
        let (_, form, _) = inner.forms[refed.0 as usize];

        if let Some(i) = inner.forms.iter().position(|(cmp, ..)| *cmp == name) {
            // If it would be circular, we just don't reference anything.
            if would_be_circular(&inner, i, refed.0 as usize) {
                inner.forms.push((name, form, FormType::Normal));
            } else {
                inner.forms.push((name, form, FormType::Ref(refed)))
            }

            for refed in refs_of(&inner, i) {
                inner.forms[refed].1 = form;
            }
        } else {
            // If the form didn't previously exist, nothing was referencing it, so
            // no checks are done.
            inner.forms.push((name, form, FormType::Ref(refed)));
        }
    }

    /// Makes a [`Form`] reference another "weakly"
    fn set_weak_ref(&self, name: &'static str, refed: impl AsRef<str>) {
        let refed = {
            let refed: &'static str = refed.as_ref().to_string().leak();
            self.id_from_name(refed)
        };

        let mut inner = self.0.write();
        let (_, form, _) = inner.forms[refed.0 as usize];

        // For weak refs, no checks are done, since a form is only set if it
        // doesn't exist, and for there to be refs to it, it must exist.
        if let Some(i) = inner.forms.iter().position(|(cmp, ..)| *cmp == name) {
            let (_, f, ty) = &mut inner.forms[i];
            if let FormType::Weakest = ty {
                *f = form;
                *ty = FormType::Ref(refed);
            }
        } else {
            inner.forms.push((name, form, FormType::Ref(refed)));
        }
    }

    /// Returns the [`FormId`] from a given `name`
    ///
    /// If the named form doesn't exist, create it.
    fn id_from_name(&self, name: &'static str) -> FormId {
        let mut inner = self.0.write();

        if let Some(id) = inner.forms.iter().position(|(cmp, ..)| *cmp == name) {
            FormId(id as u16)
        } else {
            inner.forms.push((name, Form::new(), FormType::Weakest));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Returns a form, given a [`FormId`].
    fn form_from_id(&self, id: FormId) -> Option<Form> {
        let inner = self.0.read_recursive();
        inner.forms.get(id.0 as usize).map(|(_, form, _)| *form)
    }

    /// Returns the name of the [`FormId`]
    fn name_from_id(&self, id: FormId) -> &'static str {
        let inner = self.0.read_recursive();
        let nth = inner.forms.get(id.0 as usize).map(|(name, ..)| name);

        let Some(ret) = nth else {
            unreachable!("Form with id {} not found, this should never happen", id.0);
        };
        ret
    }

    /// The [`Form`] and [`CursorShape`] of the main cursor
    fn main_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_from_id(M_CUR_ID).unwrap();
        (form, self.0.read_recursive().main_cursor)
    }

    /// The [`Form`] and [`CursorShape`] of extra cursors
    fn extra_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_from_id(E_CUR_ID).unwrap();
        (form, self.0.read_recursive().extra_cursor)
    }

    /// Sets the [`CursorShape`] of the main cursor
    fn set_main_cursor(&self, shape: CursorShape) {
        self.0.write().main_cursor = Some(shape);
    }

    /// Sets the [`CursorShape`] of extra cursors
    fn set_extra_cursor(&self, shape: CursorShape) {
        self.0.write().extra_cursor = Some(shape);
    }

    /// Unsets the [`CursorShape`] of the main cursor
    fn unset_main_cursor(&self) {
        self.0.write().main_cursor = None;
    }

    /// Unsets the [`CursorShape`] of the extra cursors
    fn unset_extra_cursor(&self) {
        self.0.write().extra_cursor = None;
    }

    /// Returns a [`Painter`]
    fn painter(&'static self) -> Painter {
        let inner = self.0.read();
        let default = inner.forms[DEFAULT_ID.0 as usize].1;
        Painter {
            inner,
            forms: vec![(default, DEFAULT_ID)],
            cur_sty: default.style,
        }
    }
}

pub struct Painter {
    inner: RwLockReadGuard<'static, InnerPalette>,
    forms: Vec<(Form, FormId)>,
    cur_sty: ContentStyle,
}

impl Painter {
    /// Applies the `Form` with the given `id` and returns the result,
    /// given previous triggers.
    #[inline(always)]
    pub fn apply(&mut self, id: FormId) -> ContentStyle {
        let (_, form, _) = unsafe { self.inner.forms.get_unchecked(id.0 as usize) };

        // So the cursor is always the last form
        self.forms.push((*form, id));
        self.cur_sty = self.make_style();
        self.cur_sty
    }

    /// Removes the [`Form`] with the given `id` and returns the
    /// result, given previous triggers.
    #[inline(always)]
    pub fn remove(&mut self, id: FormId) -> ContentStyle {
        let mut applied_forms = self.forms.iter().enumerate();
        if let Some((index, _)) = applied_forms.rfind(|(_, &(_, i))| i == id) {
            self.forms.remove(index);
            self.cur_sty = self.make_style();
        }
        self.cur_sty
    }

    #[inline(always)]
    pub fn reset(&mut self) -> ContentStyle {
        self.forms.splice(1.., []);
        self.cur_sty = self.make_style();
        self.cur_sty
    }

    /// Generates the form to be printed, given all the previously
    /// pushed forms in the `Form` stack.
    #[inline(always)]
    pub fn make_style(&self) -> ContentStyle {
        let mut form = Form {
            style: ContentStyle::default(),
            is_final: false,
        };

        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false, false);

        for &(Form { style, is_final }, _) in &self.forms {
            if let Some(new_fg) = style.foreground_color
                && (!fg_done || is_final)
            {
                form.style.foreground_color = Some(new_fg);
                fg_done |= is_final;
            }
            if let Some(new_bg) = style.background_color
                && (!bg_done || is_final)
            {
                form.style.background_color = Some(new_bg);
                bg_done |= is_final;
            }
            if let Some(new_ul) = style.underline_color
                && (!ul_done || is_final)
            {
                form.style.underline_color = Some(new_ul);
                ul_done |= is_final;
            }
            if !attr_done || is_final {
                form.style.attributes.extend(style.attributes);
                attr_done |= is_final;
            }
        }

        form.style
    }

    #[inline(always)]
    pub fn apply_main_cursor(&mut self) -> ContentStyle {
        self.apply(M_CUR_ID)
    }

    #[inline(always)]
    pub fn remove_main_cursor(&mut self) -> ContentStyle {
        self.remove(M_CUR_ID)
    }

    #[inline(always)]
    pub fn apply_extra_cursor(&mut self) -> ContentStyle {
        self.apply(E_CUR_ID)
    }

    #[inline(always)]
    pub fn remove_extra_cursor(&mut self) -> ContentStyle {
        self.remove(E_CUR_ID)
    }

    /// The [`Form`] "ExtraCursor", and its shape.
    pub fn main_cursor(&self) -> Option<CursorShape> {
        self.inner.main_cursor
    }

    /// The [`Form`] "ExtraCursor", and its shape.
    pub fn extra_cursor(&self) -> Option<CursorShape> {
        self.inner.extra_cursor
    }

    /// The `"Default"` form's [`Form`]
    pub fn get_default(&self) -> Form {
        self.forms[0].0
    }
}

/// An enum that helps in the modification of forms
enum FormType {
    Normal,
    Ref(FormId),
    Weakest,
}

/// The position of each form that eventually references the `n`th
fn refs_of(inner: &RwLockWriteGuard<InnerPalette>, n: usize) -> Vec<usize> {
    let mut refs = Vec::new();
    for (i, (.., f_ty)) in inner.forms.iter().enumerate() {
        if let FormType::Ref(refed) = f_ty
            && refed.0 as usize == n
        {
            refs.push(i);
            refs.extend(refs_of(inner, i));
        }
    }
    refs
}

/// If form references would eventually lead to a loop
fn would_be_circular(inner: &RwLockWriteGuard<InnerPalette>, referee: usize, refed: usize) -> bool {
    if let (.., FormType::Ref(refed_ref)) = inner.forms[refed] {
        match refed_ref.0 as usize == referee {
            true => true,
            false => would_be_circular(inner, referee, refed_ref.0 as usize),
        }
    } else {
        false
    }
}

/// Mimics [`ContentStyle`] methods for the [`Form`] type
macro mimic_method {
    ($method:ident $type:ty) => {
        pub fn $method(self, val: $type) -> Self {
            let Form { style, is_final } = self;
            Form { style: Stylize::$method(style, val), is_final }
        }
    },

    ($($method:ident)+ $(, $fg:ident)?) => {
        $(
        	pub fn $fg() -> Self {
        	    Form { style: ContentStyle::new().$fg(), is_final: false }
        	}
        )?

        $(
            pub fn $method(self) -> Self {
                let Form { style, is_final } = self;
                Form { style: Stylize::$method(style), is_final }
            }
        )*
    }
}
