//! Utilities for stylizing the text of Duat
use std::sync::{LazyLock, OnceLock};

use crossterm::style::{Attribute, ContentStyle, Stylize};
pub use crossterm::{cursor::SetCursorStyle as CursorShape, style::Color};
use parking_lot::{RwLock, RwLockWriteGuard};

pub use self::global::{
    FormFmt, extra_cursor, from_id, id_of, main_cursor, name_from_id, painter, set,
    set_extra_cursor, set_main_cursor, set_weak, unset_extra_cursor, unset_main_cursor,
};
use crate::{data::RwLockReadGuard, ui::Sender};

static SENDER: OnceLock<Sender> = OnceLock::new();

/// The functions that will be exposed for public use.
mod global {
    use std::sync::LazyLock;

    use parking_lot::Mutex;

    use super::{BuiltForm, CursorShape, Form, FormId, Painter, Palette};

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
    impl FormFmt for BuiltForm {}
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
            Kind::Form(form) => crate::thread::queue(move || PALETTE.set_weak_form(name, form)),
            Kind::Ref(refed) => crate::thread::queue(move || PALETTE.set_weak_ref(name, refed)),
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
        PALETTE.form_from_id(id).unwrap_or(Form::new().0)
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
    /// );
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
    /// forms::set("ExtraCursor", Form::black().on_cyan());
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
    pub macro id_of {
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
    #[derive(Clone, Copy)]
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

    impl InnerFormFmt for BuiltForm {
        fn kind(self) -> Kind {
            Kind::Form(self.0)
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Form {
    pub style: ContentStyle,
    /// Wether or not the `Form`s colors and attributes should
    /// override any that come after.
    pub finished: bool,
}

#[rustfmt::skip]
impl Form {
    mimic_method_new!(/**a colored foreground*/ with Color);
    mimic_method_new!(/**a colored background*/ on Color);
    mimic_method_new!(/**a colored underlining*/ underline Color);
    mimic_method_new!(/**an attribute*/ attribute Attribute);
    mimic_method_new!(/**reset*/ reset);
    mimic_method_new!(/**bold*/ bold);
    mimic_method_new!(/**underlined*/ underlined);
    mimic_method_new!(/**double_underlined*/ double_underlined);
    mimic_method_new!(/**undercurled*/ undercurled);
    mimic_method_new!(/**underdashed*/ underdashed);
    mimic_method_new!(/**reverse*/ reverse);
    mimic_method_new!(/**dim*/ dim);
    mimic_method_new!(/**italic*/ italic);
    mimic_method_new!(/**negative*/ negative);
    mimic_method_new!(/**slow_blink*/ slow_blink);
    mimic_method_new!(/**rapid_blink*/ rapid_blink);
    mimic_method_new!(/**hidden*/ hidden);
    mimic_method_new!(/**crossed_out*/ crossed_out);
    mimic_method_new!(/**black*/ black on_black underline_black);
    mimic_method_new!(/**dark_grey*/ dark_grey on_dark_grey underline_dark_grey);
    mimic_method_new!(/**red*/ red on_red underline_red);
    mimic_method_new!(/**dark_red*/ dark_red on_dark_red underline_dark_red);
    mimic_method_new!(/**green*/ green on_green underline_green);
    mimic_method_new!(/**dark_green*/ dark_green on_dark_green underline_dark_green);
    mimic_method_new!(/**yellow*/ yellow on_yellow underline_yellow);
    mimic_method_new!(/**dark_yellow*/ dark_yellow on_dark_yellow underline_dark_yellow);
    mimic_method_new!(/**blue*/ blue on_blue underline_blue);
    mimic_method_new!(/**dark_blue*/ dark_blue on_dark_blue underline_dark_blue);
    mimic_method_new!(/**magenta*/ magenta on_magenta underline_magenta);
    mimic_method_new!(/**dark_magenta*/ dark_magenta on_dark_magenta underline_dark_magenta);
    mimic_method_new!(/**cyan*/ cyan on_cyan underline_cyan);
    mimic_method_new!(/**dark_cyan*/ dark_cyan on_dark_cyan underline_dark_cyan);
    mimic_method_new!(/**white*/ white on_white underline_white);
    mimic_method_new!(/**grey*/ grey on_grey underline_grey);

	/// Returns a new [`Form`] with a default style
	///
	/// This method actually returns [`BuiltForm`]
	#[allow(clippy::new_ret_no_self)]
    pub fn new() -> BuiltForm {
        BuiltForm(Self { style: ContentStyle::new(), finished: false })
    }

	/// Returns a new [`Form`] with a default _finished_ style
	/// 
    /// A finished style is one that cannot be superseeded. That is,
    /// if this style sets a foreground, while it is active, new
    /// styles may not modify the color of the foreground.
    pub fn finished() -> BuiltForm {
        BuiltForm(Self { style: ContentStyle::default(), finished: true })
    }

	/// Makes `self` finished 
    fn as_finished(self) -> Self {
        Self { style: self.style, finished: true }
    }
}

/// A convenience struct for [`Form`]s
///
/// This struct exists in order to have [`Form`] methods be
/// initializers, while [`BuiltForm`] methods consume and return a
/// [`BuiltForm`]s
///
/// This is their only difference, everywhere else, they are
/// functionally identical.
pub struct BuiltForm(Form);

#[rustfmt::skip]
impl BuiltForm {
    mimic_method_cycle!(/**Colors the foreground of this [`Form`]*/ with Color);
    mimic_method_cycle!(/**Colors the background of this [`Form`]*/ on Color);
    mimic_method_cycle!(/**Colors the underlining of this [`Form`]*/ underline Color );
    mimic_method_cycle!(/**Applies an attribute to this [`Form`]*/ attribute Attribute );
    mimic_method_cycle!(/**reset*/ reset);
    mimic_method_cycle!(/**bold*/ bold);
    mimic_method_cycle!(/**underlined*/ underlined);
    mimic_method_cycle!(/**double_underlined*/ double_underlined);
    mimic_method_cycle!(/**undercurled*/ undercurled);
    mimic_method_cycle!(/**underdashed*/ underdashed);
    mimic_method_cycle!(/**reverse*/ reverse);
    mimic_method_cycle!(/**dim*/ dim);
    mimic_method_cycle!(/**italic*/ italic);
    mimic_method_cycle!(/**negative*/ negative);
    mimic_method_cycle!(/**slow_blink*/ slow_blink);
    mimic_method_cycle!(/**rapid_blink*/ rapid_blink);
    mimic_method_cycle!(/**hidden*/ hidden);
    mimic_method_cycle!(/**crossed_out*/ crossed_out);
    mimic_method_cycle!(/**black*/ black on_black underline_black);
    mimic_method_cycle!(/**dark_grey*/ dark_grey on_dark_grey underline_dark_grey);
    mimic_method_cycle!(/**red*/ red on_red underline_red);
    mimic_method_cycle!(/**dark_red*/ dark_red on_dark_red underline_dark_red);
    mimic_method_cycle!(/**green*/ green on_green underline_green);
    mimic_method_cycle!(/**dark_green*/ dark_green on_dark_green underline_dark_green);
    mimic_method_cycle!(/**yellow*/ yellow on_yellow underline_yellow);
    mimic_method_cycle!(/**dark_yellow*/ dark_yellow on_dark_yellow underline_dark_yellow);
    mimic_method_cycle!(/**blue*/ blue on_blue underline_blue);
    mimic_method_cycle!(/**dark_blue*/ dark_blue on_dark_blue underline_dark_blue);
    mimic_method_cycle!(/**magenta*/ magenta on_magenta underline_magenta);
    mimic_method_cycle!(/**dark_magenta*/ dark_magenta on_dark_magenta underline_dark_magenta);
    mimic_method_cycle!(/**cyan*/ cyan on_cyan underline_cyan);
    mimic_method_cycle!(/**dark_cyan*/ dark_cyan on_dark_cyan underline_dark_cyan);
    mimic_method_cycle!(/**white*/ white on_white underline_white);
    mimic_method_cycle!(/**grey*/ grey on_grey underline_grey);

	/// Makes this [`Form`] finished
	/// 
    /// A finished style is one that cannot be superseeded. That is,
    /// if this style sets a foreground, while it is active, new
    /// styles may not modify the color of the foreground.
    pub fn finished(self) -> Self {
        Self(Form { finished: true, ..self.0 })
    }
}

impl std::ops::Deref for BuiltForm {
    type Target = Form;

    fn deref(&self) -> &Self::Target {
        &self.0
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
                ("Default", Form::new().0, Normal),
                ("Accent", Form::bold().0, Normal),
                ("DefaultOk", Form::blue().0, Normal),
                ("AccentOk", Form::cyan().0, Normal),
                ("DefaultErr", Form::red().0, Normal),
                ("AccentErr", Form::red().bold().0, Normal),
                ("DefaultHint", Form::grey().0, Normal),
                ("AccentHint", Form::grey().bold().0, Normal),
                ("MainCursor", Form::reverse().0, Normal),
                ("ExtraCursor", Form::reverse().0, Ref(M_CUR_ID)),
                ("MainSelection", Form::on_dark_grey().0, Normal),
                ("ExtraSelection", Form::on_dark_grey().0, Ref(M_SEL_ID)),
                ("Inactive", Form::grey().0, Normal),
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
            "MainCursor" | "ExtraCursor" => form.as_finished(),
            _ => form,
        };

        let mut inner = self.0.write();

        if let Some(i) = inner.forms.iter().position(|(cmp, ..)| *cmp == name) {
            inner.forms[i].1 = form;

            for refed in refs_of(&inner, i) {
                inner.forms[refed].1 = form;
            }

            if let Some(sender) = SENDER.get() {
                sender.send_form_changed().unwrap()
            }
        } else {
            inner.forms.push((name, form, FormType::Normal));
        };
    }

    /// Sets a [`Form`] "weakly"
    fn set_weak_form(&self, name: &'static str, form: Form) {
        let form = match name {
            "MainCursor" | "ExtraCursor" => form.as_finished(),
            _ => form,
        };

        let mut inner = self.0.write();

        if let Some(i) = inner.forms.iter().position(|(cmp, ..)| *cmp == name) {
            let (_, f, ty) = &mut inner.forms[i];
            if let FormType::Weakest = ty {
                *f = form;
                *ty = FormType::Normal;

                if let Some(sender) = SENDER.get() {
                    sender.send_form_changed().unwrap()
                }
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

            if let Some(sender) = SENDER.get() {
                sender.send_form_changed().unwrap()
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

                if let Some(sender) = SENDER.get() {
                    sender.send_form_changed().unwrap()
                }
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
            inner.forms.push((name, Form::new().0, FormType::Weakest));
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
        if let Some(sender) = SENDER.get() {
            sender.send_form_changed().unwrap()
        }
    }

    /// Sets the [`CursorShape`] of extra cursors
    fn set_extra_cursor(&self, shape: CursorShape) {
        self.0.write().extra_cursor = Some(shape);
        if let Some(sender) = SENDER.get() {
            sender.send_form_changed().unwrap()
        }
    }

    /// Unsets the [`CursorShape`] of the main cursor
    fn unset_main_cursor(&self) {
        self.0.write().main_cursor = None;
        if let Some(sender) = SENDER.get() {
            sender.send_form_changed().unwrap()
        }
    }

    /// Unsets the [`CursorShape`] of the extra cursors
    fn unset_extra_cursor(&self) {
        self.0.write().extra_cursor = None;
        if let Some(sender) = SENDER.get() {
            sender.send_form_changed().unwrap()
        }
    }

    /// Returns a [`Painter`]
    fn painter(&'static self) -> Painter {
        let inner = self.0.read();
        let default = inner.forms[DEFAULT_ID.0 as usize].1;
        Painter {
            inner,
            cur: vec![(default, DEFAULT_ID)],
            cur_sty: default.style,
        }
    }
}

pub struct Painter {
    inner: RwLockReadGuard<'static, InnerPalette>,
    cur: Vec<(Form, FormId)>,
    cur_sty: ContentStyle,
}

impl Painter {
    /// Applies the `Form` with the given `id` and returns the result,
    /// given previous triggers.
    #[inline(always)]
    pub fn apply(&mut self, id: FormId) -> ContentStyle {
        let i = id.0 as usize;
        let forms = &self.inner.forms;
        let form = forms.get(i).map(|(_, f, _)| *f).unwrap_or(Form::new().0);

        // So the cursor is always the last form
        self.cur.push((form, id));
        self.cur_sty = self.make_style();
        self.cur_sty
    }

    /// Removes the [`Form`] with the given `id` and returns the
    /// result, given previous triggers.
    #[inline(always)]
    pub fn remove(&mut self, id: FormId) -> ContentStyle {
        let mut applied_forms = self.cur.iter().enumerate();
        if let Some((index, _)) = applied_forms.rfind(|(_, &(_, i))| i == id) {
            self.cur.remove(index);
            self.cur_sty = self.make_style();
        }
        self.cur_sty
    }

    #[inline(always)]
    pub fn reset(&mut self) -> ContentStyle {
        self.cur.splice(1.., []);
        self.cur_sty = self.make_style();
        self.cur_sty
    }

    /// Generates the form to be printed, given all the previously
    /// pushed forms in the `Form` stack.
    #[inline(always)]
    pub fn make_style(&self) -> ContentStyle {
        let mut form = Form {
            style: ContentStyle::default(),
            finished: false,
        };

        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false, false);

        for &(Form { style, finished }, _) in &self.cur {
            if let Some(new_fg) = style.foreground_color
                && (!fg_done || finished)
            {
                form.style.foreground_color = Some(new_fg);
                fg_done |= finished;
            }
            if let Some(new_bg) = style.background_color
                && (!bg_done || finished)
            {
                form.style.background_color = Some(new_bg);
                bg_done |= finished;
            }
            if let Some(new_ul) = style.underline_color
                && (!ul_done || finished)
            {
                form.style.underline_color = Some(new_ul);
                ul_done |= finished;
            }
            if !attr_done || finished {
                form.style.attributes.extend(style.attributes);
                attr_done |= finished;
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
        self.cur[0].0
    }
}

pub(crate) fn set_sender(sender: Sender) {
    SENDER
        .set(sender)
        .unwrap_or_else(|_| panic!("Sender set more than once"));
}

/// An enum that helps in the modification of forms
#[derive(Debug)]
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
macro mimic_method_new {
    (#[$attr:meta] $method:ident $type:ty) => {
        /// New [`Form`] with
        #[$attr]
        pub fn $method(val: $type) -> BuiltForm {
            let style = ContentStyle::default();
            BuiltForm(Form { style: Stylize::$method(style, val), finished: false })
        }
    },

    (#[$attr:meta] $method:ident) => {
        /// New [`Form`] with the
        #[$attr]
        /// atribute
        pub fn $method() -> BuiltForm {
            let style = ContentStyle::default();
            BuiltForm(Form { style: Stylize::$method(style), finished: false })
        }
    },

    (#[$attr:meta] $fg:ident $bg:ident $ul:ident) => {
        /// New [`Form`] with a
        #[$attr]
        /// foreground
        pub fn $fg() -> BuiltForm {
            let style = ContentStyle::default();
            BuiltForm(Form { style: Stylize::$fg(style), finished: false })
        }

        /// New [`Form`] with a
        #[$attr]
        /// background
        pub fn $bg() -> BuiltForm {
            let style = ContentStyle::default();
            BuiltForm(Form { style: Stylize::$bg(style), finished: false })
        }

        /// New [`Form`] with a
        #[$attr]
        /// underlining
        ///
        /// Do note that this feature may not be supported in all [`Ui`]s.
        ///
        /// [`Ui`]: crate::ui::Ui
        pub fn $ul() -> BuiltForm {
            let style = ContentStyle::default();
            BuiltForm(Form { style: Stylize::$ul(style), finished: false })
        }
    }
}

macro mimic_method_cycle {
    (#[$attr:meta] $method:ident $type:ty) => {
        #[$attr]
        pub fn $method(self, val: $type) -> BuiltForm {
            let Form { style, finished } = self.0;
            BuiltForm(Form { style: Stylize::$method(style, val), finished })
        }
    },

    (#[$attr:meta] $method:ident) => {
        /// Applies the
        #[$attr]
        /// atribute to this [`Form`]
        pub fn $method(self) -> BuiltForm {
            let Form { style, finished } = self.0;
            BuiltForm(Form { style: Stylize::$method(style), finished })
        }
    },

    (#[$attr:meta] $fg:ident $bg:ident $ul:ident) => {
        /// Turns the foreground of this [`Form`]
        #[$attr]
        pub fn $fg(self) -> BuiltForm {
            let Form { style, finished } = self.0;
            BuiltForm(Form { style: Stylize::$fg(style), finished })
        }

		/// Turns the background of this [`Form`]
        #[$attr]
        pub fn $bg(self) -> BuiltForm {
            let Form { style, finished } = self.0;
            BuiltForm(Form { style: Stylize::$bg(style), finished })
        }

		/// Turns the underlining of this [`Form`]
        #[$attr]
        ///
        /// Do note that this feature may not be supported in all [`Ui`]s.
        ///
        /// [`Ui`]: crate::ui::Ui
        pub fn $ul(self) -> BuiltForm {
            let Form { style, finished } = self.0;
            BuiltForm(Form { style: Stylize::$ul(style), finished })
        }
    }
}
