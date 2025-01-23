//! Utilities for stylizing the text of Duat
use std::sync::{LazyLock, OnceLock};

use FormType::*;
use crossterm::style::{Attribute, Attributes, ContentStyle};
pub use crossterm::{cursor::SetCursorStyle as CursorShape, style::Color};
use parking_lot::{RwLock, RwLockWriteGuard};

pub use self::global::{
    FormFmt, extra_cursor, from_id, id_of, inner_to_id, main_cursor, name_of, painter, set,
    set_extra_cursor, set_main_cursor, set_weak, unset_extra_cursor, unset_main_cursor,
};
use crate::{data::RwLockReadGuard, ui::Sender};

static SENDER: OnceLock<Sender> = OnceLock::new();
static BASE_FORMS: &[(&str, Form, FormType)] = &[
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
    // Tree sitter Forms
    ("type", Form::yellow().0, Normal),
    ("constant", Form::dark_grey().0, Normal),
    ("function", Form::blue().italic().0, Normal),
    ("comment", Form::dark_grey().0, Normal),
    ("comment.documentation", Form::dark_grey().bold().0, Normal),
    ("punctuation.bracket", Form::red().0, Normal),
    ("punctuation.delimiter", Form::cyan().0, Normal),
    ("variable.parameter", Form::italic().0, Normal),
    ("variable.builtin", Form::dark_yellow().0, Normal),
    ("label", Form::green().0, Normal),
    ("keyword", Form::magenta().0, Normal),
    ("string", Form::green().0, Normal),
    ("constant.builtin", Form::dark_yellow().0, Normal),
    ("escape", Form::dark_yellow().0, Normal),
    ("attribute", Form::magenta().0, Normal),
    ("operator", Form::cyan().0, Normal),
    ("constructor", Form::yellow().0, Normal),
    ("module", Form::blue().0, Normal),
    ("interface", Form::white().bold().0, Normal),
];

/// The functions that will be exposed for public use.
mod global {
    use std::sync::LazyLock;

    use parking_lot::Mutex;

    use super::{BASE_FORMS, BuiltForm, CursorShape, Form, FormId, Painter, Palette};

    static PALETTE: Palette = Palette::new();
    static FORMS: LazyLock<Mutex<Vec<&str>>> =
        LazyLock::new(|| Mutex::new(BASE_FORMS.iter().map(|(name, ..)| *name).collect()));

    /// Either a [`Form`] or a name of a form
    ///
    /// Note that the referenced form does not need to exist for
    /// [`form::set`] or [`form::set_weak`] to work properly.
    ///
    /// [`form::set`]: set
    /// [`form::set_weak`]: set_weak
    pub trait FormFmt: InnerFormFmt {}
    impl FormFmt for Form {}
    impl FormFmt for BuiltForm {}
    impl FormFmt for &str {}
    impl FormFmt for &mut str {}

    /// Sets the [`Form`] by the name of `name`
    ///
    /// This will create a new form or replace one that already
    /// exists, and you can either set it to a [`Form`] directly, or
    /// reference another form by its name:
    ///
    /// ```rust
    /// # use duat_core::form::{self, Form};
    /// // Creates a regular form
    /// form::set("MyRegularForm", Form::red());
    /// // Creates a form that references the first
    /// form::set("MyRefForm", "MyRegularForm");
    /// // Sets both "MyRegularForm" and "MyRefForm" to blue
    /// form::set("MyRegularForm", Form::blue());
    /// ```
    ///
    /// If you are creating a plugin, or another kind of tool for
    /// others using Duat, use [`form::set_weak`] instead of this
    /// function.
    ///
    /// [`form::set_weak`]: set_weak
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
    /// The difference between this function and [`form::set`] is
    /// that this function will only trigger if the form didn't
    /// already exist.
    ///
    /// This is useful for plugins, since it prioritizes the user's
    /// preferences, no matter in what order this function and
    /// [`form::set`] are called:
    ///
    /// ```rust
    /// use duat_core::form::{self, Form};
    /// // Creates a form
    /// form::set_weak("WeakForm", Form::blue().on_white());
    /// // Creates a form "strongly"
    /// form::set("StrongForm", Form::new().bold());
    /// // Does nothing
    /// form::set_weak("StrongForm", "Default");
    /// ```
    ///
    /// [`form::set`]: set
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
    /// consider using [`form::painter`] instead.
    ///
    /// [`form::painter`]: painter
    pub fn from_id(id: FormId) -> Form {
        PALETTE.form_from_id(id).unwrap_or(Form::new().0)
    }

    /// The name of a form, given a [`FormId`]
    pub fn name_of(id: FormId) -> &'static str {
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
    /// # use duat_core::form::{self, Form, Color};
    /// form::set(
    ///     "MainCursor",
    ///     Form::black().on(Color::Rgb { r: 240, g: 210, b: 200 }),
    /// );
    /// ```
    ///
    /// However, if possible, Duat will still try to use the main
    /// cursor's [shape]. If you don't want that to happen, see
    /// [`form::unset_main_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`form::unset_main_cursor`]: unset_main_cursor
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
    /// # use duat_core::form::{self, Form, Color};
    /// form::set("ExtraCursor", Form::black().on_cyan());
    /// ```
    ///
    /// However, if possible, Duat will still try to use the main
    /// cursor's [shape]. If you don't want that to happen, see
    /// [`form::unset_extra_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`form::unset_extra_cursor`]: unset_extra_cursor
    pub fn set_extra_cursor(shape: CursorShape) {
        crate::thread::queue(move || PALETTE.set_extra_cursor(shape));
    }

    /// Removes the main cursor's [shape]
    ///
    /// By doing this, you will force Duat to draw the main cursor by
    /// use of the `"MainCursor"` form.
    ///
    /// If you want to set the [shape] instead, see
    /// [`form::set_main_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`form::set_main_cursor`]: set_main_cursor
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
    /// [`form::set_extra_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`form::set_extra_cursor`]: set_extra_cursor
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

    impl InnerFormFmt for &mut str {
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
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct FormId(u16);

impl std::fmt::Debug for FormId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FormId({})", name_of(*self))
    }
}

/// A style for text.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Form {
    pub style: ContentStyle,
    /// Wether or not to reset [`Bold`], [`Italic`] and [`Dim`]
    /// [`Attribute`]s. This can be used alongside these
    /// [`Attribute`]s. So you can have, e.g. non italic bold text
    /// within an italicized form.
    ///
    /// [`Bold`]: Attribute::Bold
    /// [`Italic`]: Attribute::Italic
    /// [`Dim`]: Attribute::Dim
    pub normal: bool,
}

#[rustfmt::skip]
impl Form {
    mimic_method_new!(/**reset*/ reset Attribute::Reset);
    mimic_method_new!(/**bold*/ bold Attribute::Bold);
    mimic_method_new!(/**underlined*/ underlined Attribute::Underlined);
    mimic_method_new!(/**reverse*/ reverse Attribute::Reverse);
    mimic_method_new!(/**dim*/ dim Attribute::Dim);
    mimic_method_new!(/**italic*/ italic Attribute::Italic);
    mimic_method_new!(/**negative*/ negative Attribute::Reverse);
    mimic_method_new!(/**slow_blink*/ slow_blink Attribute::SlowBlink);
    mimic_method_new!(/**rapid_blink*/ rapid_blink Attribute::RapidBlink);
    mimic_method_new!(/**hidden*/ hidden Attribute::Hidden);
    mimic_method_new!(/**crossed_out*/ crossed_out Attribute::CrossedOut);
    mimic_method_new!(/**double_underlined*/ double_underlined Attribute::DoubleUnderlined);
    mimic_method_new!(/**undercurled*/ undercurled Attribute::Undercurled);
    mimic_method_new!(/**underdashed*/ underdashed Attribute::Underdashed);
    mimic_method_new!(/**black*/ black on_black underline_black Color::Black);
    mimic_method_new!(/**dark_grey*/ dark_grey on_dark_grey underline_dark_grey Color::DarkGrey);
    mimic_method_new!(/**red*/ red on_red underline_red Color::Red);
    mimic_method_new!(/**dark_red*/ dark_red on_dark_red underline_dark_red Color::DarkRed);
    mimic_method_new!(/**green*/ green on_green underline_green Color::Green);
    mimic_method_new!(
        /**dark_green*/ dark_green on_dark_green underline_dark_green Color::DarkGreen
    );
    mimic_method_new!(/**yellow*/ yellow on_yellow underline_yellow Color::Yellow);
    mimic_method_new!(
        /**dark_yellow*/ dark_yellow on_dark_yellow underline_dark_yellow Color::DarkYellow
    );
    mimic_method_new!(/**blue*/ blue on_blue underline_blue Color::Blue);
    mimic_method_new!(/**dark_blue*/ dark_blue on_dark_blue underline_dark_blue Color::DarkBlue);
    mimic_method_new!(/**magenta*/ magenta on_magenta underline_magenta Color::Magenta);
    mimic_method_new!(
        /**dark_magenta*/ dark_magenta on_dark_magenta underline_dark_magenta Color::DarkMagenta
    );
    mimic_method_new!(/**cyan*/ cyan on_cyan underline_cyan Color::Cyan);
    mimic_method_new!(/**dark_cyan*/ dark_cyan on_dark_cyan underline_dark_cyan Color::DarkCyan);
    mimic_method_new!(/**white*/ white on_white underline_white Color::White);
    mimic_method_new!(/**grey*/ grey on_grey underline_grey Color::Grey);

    /// Returns a new [`Form`] with a default style
    ///
    /// This method actually returns [`BuiltForm`]
    #[allow(clippy::new_ret_no_self)]
    pub const fn new() -> BuiltForm {
        let style = ContentStyle {
            foreground_color: None,
            background_color: None,
            underline_color: None,
            attributes: Attributes::none(),
        };
        BuiltForm(Self { style, normal: false })
    }

    /// Returns a new [`Form`] with a default _normal_ style
    ///
    /// A normal [`Form`] is one that resets specifically the
    /// [`Bold`], [`Italic`] and [`Dim`] [`Attribute`]s. This can
    /// be used alongside these [`Attribute`]s. So you can have
    /// e.g., non italic bold text within an italicized form.
    ///
    /// [`Bold`]: Attribute::Bold
    /// [`Italic`]: Attribute::Italic
    /// [`Dim`]: Attribute::Dim
    pub const fn normal() -> BuiltForm {
        let mut built = Form::new();
        built.0.normal = true;
        built
    }

    /// New [`Form`] with a colored foreground
    pub const fn with(color: Color) -> BuiltForm {
        let mut built = Form::new();
        built.0.style.foreground_color = Some(color);
        built
    }

    /// New [`Form`] with a colored background
    pub const fn on(color: Color) -> BuiltForm {
        let mut built = Form::new();
        built.0.style.background_color = Some(color);
        built
    }

    /// New [`Form`] with a colored underlining
    pub const fn underline(color: Color) -> BuiltForm {
        let mut built = Form::new();
        built.0.style.underline_color = Some(color);
        built
    }

    /// New [`Form`] with an attribute
    pub const fn attribute(attr: Attribute) -> BuiltForm {
        let mut built = Form::new();
        built.0.style.attributes = built.0.style.attributes.with(attr);
        built
    }

    /// Makes `self` exclusive
    const fn as_normal(self) -> Self {
        Self { style: self.style, normal: true }
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
    mimic_method_mod!(/**reset*/ reset Attribute::Reset);
    mimic_method_mod!(/**bold*/ bold Attribute::Bold);
    mimic_method_mod!(/**underlined*/ underlined Attribute::Underlined);
    mimic_method_mod!(/**reverse*/ reverse Attribute::Reverse);
    mimic_method_mod!(/**dim*/ dim Attribute::Dim);
    mimic_method_mod!(/**italic*/ italic Attribute::Italic);
    mimic_method_mod!(/**negative*/ negative Attribute::Reverse);
    mimic_method_mod!(/**slow_blink*/ slow_blink Attribute::SlowBlink);
    mimic_method_mod!(/**rapid_blink*/ rapid_blink Attribute::RapidBlink);
    mimic_method_mod!(/**hidden*/ hidden Attribute::Hidden);
    mimic_method_mod!(/**crossed_out*/ crossed_out Attribute::CrossedOut);
    mimic_method_mod!(/**double_underlined*/ double_underlined Attribute::DoubleUnderlined);
    mimic_method_mod!(/**undercurled*/ undercurled Attribute::Undercurled);
    mimic_method_mod!(/**underdashed*/ underdashed Attribute::Underdashed);
    mimic_method_mod!(/**black*/ black on_black underline_black Color::Black);
    mimic_method_mod!(/**dark_grey*/ dark_grey on_dark_grey underline_dark_grey Color::DarkGrey);
    mimic_method_mod!(/**red*/ red on_red underline_red Color::Red);
    mimic_method_mod!(/**dark_red*/ dark_red on_dark_red underline_dark_red Color::DarkRed);
    mimic_method_mod!(/**green*/ green on_green underline_green Color::Green);
    mimic_method_mod!(
        /**dark_green*/ dark_green on_dark_green underline_dark_green Color::DarkGreen
    );
    mimic_method_mod!(/**yellow*/ yellow on_yellow underline_yellow Color::Yellow);
    mimic_method_mod!(
        /**dark_yellow*/ dark_yellow on_dark_yellow underline_dark_yellow Color::DarkYellow
    );
    mimic_method_mod!(/**blue*/ blue on_blue underline_blue Color::Blue);
    mimic_method_mod!(/**dark_blue*/ dark_blue on_dark_blue underline_dark_blue Color::DarkBlue);
    mimic_method_mod!(/**magenta*/ magenta on_magenta underline_magenta Color::Magenta);
    mimic_method_mod!(
        /**dark_magenta*/ dark_magenta on_dark_magenta underline_dark_magenta Color::DarkMagenta
    );
    mimic_method_mod!(/**cyan*/ cyan on_cyan underline_cyan Color::Cyan);
    mimic_method_mod!(/**dark_cyan*/ dark_cyan on_dark_cyan underline_dark_cyan Color::DarkCyan);
    mimic_method_mod!(/**white*/ white on_white underline_white Color::White);
    mimic_method_mod!(/**grey*/ grey on_grey underline_grey Color::Grey);

    /// Makes this [`Form`] normal
    ///
    /// A normal [`Form`] is one that resets specifically the
    /// [`Bold`], [`Italic`] and [`Dim`] [`Attribute`]s. This can
    /// be used alongside these [`Attribute`]s. So you can have
    /// e.g., non italic bold text within an italicized form.
    ///
    /// [`Bold`]: Attribute::Bold
    /// [`Italic`]: Attribute::Italic
    /// [`Dim`]: Attribute::Dim
    pub const fn normal(self) -> Self {
        Self(Form { normal: true, ..self.0 })
    }

    /// Colors the foreground of this [`Form`]
    pub const fn with(mut self, color: Color) -> Self {
        self.0.style.foreground_color = Some(color);
        self
    }

    /// Colors the background of this [`Form`]
    pub const fn on(mut self, color: Color) -> Self {
        self.0.style.background_color = Some(color);
        self
    }

    /// Colors the underlining of this [`Form`]
    pub const fn underline(mut self, color: Color) -> Self {
        self.0.style.underline_color = Some(color);
        self
    }

    /// Applies an attribute to this [`Form`]
    pub const fn attribute(mut self, attr: Attribute) -> Self {
        self.0.style.attributes = self.0.style.attributes.with(attr);
        Self(self.0)
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
            let main_cursor = Some(CursorShape::DefaultUserShape);

            RwLock::new(InnerPalette {
                main_cursor,
                extra_cursor: main_cursor,
                forms: BASE_FORMS.to_vec(),
            })
        }))
    }

    /// Sets a [`Form`]
    fn set_form(&self, name: &'static str, form: Form) {
        let form = match name {
            "MainCursor" | "ExtraCursor" => form.as_normal(),
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
            "MainCursor" | "ExtraCursor" => form.as_normal(),
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
        let mut form = Form::new().0;

        for &(Form { style, normal }, _) in &self.cur {
            if normal {
                form.style.attributes.unset(Attribute::Bold);
                form.style.attributes.unset(Attribute::Italic);
                form.style.attributes.unset(Attribute::Dim);
            }

            form.style.foreground_color = style.foreground_color.or(form.style.foreground_color);
            form.style.background_color = style.background_color.or(form.style.background_color);
            form.style.underline_color = style.underline_color.or(form.style.underline_color);
            form.style.attributes.extend(style.attributes);
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
#[derive(Debug, Clone)]
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
    (#[$attr:meta] $method:ident $attrib:expr) => {
        /// New [`Form`] with the
        #[$attr]
        /// attribute
        pub const fn $method() -> BuiltForm {
            let mut built = Form::new();
            built.0.style.attributes = built.0.style.attributes.with($attrib);
            built
        }
    },

    (#[$attr:meta] $fg:ident $bg:ident $ul:ident $color:expr) => {
        /// New [`Form`] with a
        #[$attr]
        /// foreground
        pub const fn $fg() -> BuiltForm {
            let mut built = Form::new();
            built.0.style.foreground_color = Some($color);
            built
        }

        /// New [`Form`] with a
        #[$attr]
        /// background
        pub const fn $bg() -> BuiltForm {
            let mut built = Form::new();
            built.0.style.background_color = Some($color);
            built
        }

        /// New [`Form`] with a
        #[$attr]
        /// underlining
        ///
        /// Do note that this feature may not be supported in all [`Ui`]s.
        ///
        /// [`Ui`]: crate::ui::Ui
        pub fn $ul() -> BuiltForm {
            let mut built = Form::new();
            built.0.style.underline_color = Some($color);
            built
        }
    }
}

macro mimic_method_mod {
    (#[$attr:meta] $method:ident $attrib:expr) => {
        /// Applies the
        #[$attr]
        /// attribute to this [`Form`]
        pub const fn $method(mut self) -> Self {
            self.0.style.attributes = self.0.style.attributes.with($attrib);
            self
        }
    },

    (#[$attr:meta] $fg:ident $bg:ident $ul:ident $color:expr) => {
        /// Turns the foreground of this [`Form`]
        #[$attr]
        pub const fn $fg(mut self) -> Self {
            self.0.style.foreground_color = Some($color);
            self
        }

		/// Turns the background of this [`Form`]
        #[$attr]
        pub const fn $bg(mut self) -> Self {
            self.0.style.background_color = Some($color);
            self
        }

		/// Turns the underlining of this [`Form`]
        #[$attr]
        ///
        /// Do note that this feature may not be supported in all [`Ui`]s.
        ///
        /// [`Ui`]: crate::ui::Ui
        pub const fn $ul(mut self) -> Self {
            self.0.style.underline_color = Some($color);
            self
        }
    }
}
