//! Utilities for stylizing the text of Duat
use std::sync::{LazyLock, OnceLock};

use FormType::*;
use crossterm::style::{Attribute, Attributes, ContentStyle};
pub use crossterm::{cursor::SetCursorStyle as CursorShape, style::Color};
use parking_lot::{RwLock, RwLockReadGuard};

pub use self::global::*;
pub(crate) use self::global::{colorscheme_exists, exists};
use crate::{
    hooks::{self, FormSet},
    ui::Sender,
};

pub trait ColorScheme: Send + Sync + 'static {
    fn apply(&self);

    fn name(&self) -> &'static str;
}

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
    ("ExtraCursor", Form::reverse().0, Ref(M_CUR_ID.0 as usize)),
    ("MainSelection", Form::white().on_dark_grey().0, Normal),
    (
        "ExtraSelection",
        Form::white().on_dark_grey().0,
        Ref(M_SEL_ID.0 as usize),
    ),
    ("Inactive", Form::grey().0, Normal),
];

/// The functions that will be exposed for public use.
mod global {
    use std::{
        any::TypeId,
        collections::HashMap,
        sync::{LazyLock, mpsc},
        time::Duration,
    };

    use parking_lot::Mutex;

    use super::{BASE_FORMS, BuiltForm, ColorScheme, CursorShape, Form, FormId, Painter, Palette};
    use crate::{
        context,
        hooks::{self, ColorSchemeSet},
        text::err,
    };

    static PALETTE: Palette = Palette::new();
    static FORMS: LazyLock<Mutex<Vec<&str>>> =
        LazyLock::new(|| Mutex::new(BASE_FORMS.iter().map(|(name, ..)| *name).collect()));
    static COLORSCHEMES: LazyLock<Mutex<Vec<Box<dyn ColorScheme>>>> = LazyLock::new(Mutex::default);

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
    impl FormFmt for String {}

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
            Kind::Form(form) => queue(move || PALETTE.set_form(name, form)),
            Kind::Ref(refed) => queue(move || PALETTE.set_ref(name, refed)),
        };

        let mut forms = FORMS.lock();
        if let Kind::Ref(refed) = kind {
            position_of_name(&mut forms, refed);
        }
        FormId(position_of_name(&mut forms, name) as u16)
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
            Kind::Form(form) => queue(move || PALETTE.set_weak_form(name, form)),
            Kind::Ref(refed) => queue(move || PALETTE.set_weak_ref(name, refed)),
        };

        let mut forms = FORMS.lock();
        if let Kind::Ref(refed) = kind {
            position_of_name(&mut forms, refed);
        }
        FormId(position_of_name(&mut forms, name) as u16)
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
    /// form::set("MainCursor", Form::black().on("rgb 240 210 200"));
    /// ```
    ///
    /// However, if possible, Duat will still try to use the main
    /// cursor's [shape]. If you don't want that to happen, see
    /// [`form::unset_main_cursor`].
    ///
    /// [shape]: CursorShape
    /// [`form::unset_main_cursor`]: unset_main_cursor
    pub fn set_main_cursor(shape: CursorShape) {
        queue(move || PALETTE.set_main_cursor(shape));
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
        queue(move || PALETTE.set_extra_cursor(shape));
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
        queue(move || PALETTE.unset_main_cursor());
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
        queue(move || PALETTE.unset_extra_cursor());
    }

    /// A [`Painter`] for coloring text efficiently
    ///
    /// The type argument is used to distinguish which `"Default"`
    /// [`Form`] is supposed to be used (e.g. `"Default.File"`,
    /// `"Default.StatusLine"`, etc).
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
    pub fn painter<W: ?Sized + 'static>() -> Painter {
        fn default_id(type_id: TypeId, type_name: &'static str) -> FormId {
            static IDS: LazyLock<Mutex<HashMap<TypeId, FormId>>> = LazyLock::new(Mutex::default);
            let mut ids = IDS.lock();

            if let Some(id) = ids.get(&type_id) {
                *id
            } else {
                let name: &'static str = format!("Default.{type_name}").leak();
                let id = id_from_name(name);
                add_forms(vec![name]);
                ids.insert(type_id, id);
                id
            }
        }
        PALETTE.painter(default_id(TypeId::of::<W>(), crate::duat_name::<W>()))
    }

    /// Returns the [`FormId`] from the name of a [`Form`]
    ///
    /// You can also pass multiple names, in order to get a list of
    /// ids.
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
    pub macro id_of {
        ($form:expr) => {{
            static ID: std::sync::OnceLock<FormId> = std::sync::OnceLock::new();
            *ID.get_or_init(|| {
                let name: &'static str = $form;
                let id = id_from_name(name);
                add_forms(vec![name]);
                id
            })
        }},
        ($($form:expr),+) => {{
            static IDS: std::sync::OnceLock<&[FormId]> = std::sync::OnceLock::new();
            let ids = *IDS.get_or_init(|| {
    			let mut ids = Vec::new();
    			let names = vec![$( $form ),+];
    			for name in names.iter() {
        			ids.push(id_from_name(name));
    			}
    			add_forms(names);
    			ids.leak()
    		});
    		ids
        }}
    }

    /// Non static version of [`id_of!`]
    ///
    /// You should only use this if the names of the [`Form`]s in
    /// question are not known at compile time. And if that is the
    /// case, you should try to find a way to memoize around this
    /// issue (usually with something like a [`HashMap`]).
    pub fn ids_of_non_static(names: impl IntoIterator<Item = impl ToString>) -> Vec<FormId> {
        let names: Vec<&str> = names
            .into_iter()
            .map(|n| {
                let str: &'static str = n.to_string().leak();
                str
            })
            .collect();

        let mut ids = Vec::new();
        let mut forms = FORMS.lock();
        for name in names.iter() {
            ids.push(FormId(position_of_name(&mut forms, name) as u16));
        }
        add_forms(names);
        ids
    }

    /// Returns the [`FormId`] of the form's name
    #[doc(hidden)]
    pub fn id_from_name(name: &'static str) -> FormId {
        let mut forms = FORMS.lock();
        FormId(position_of_name(&mut forms, name) as u16)
    }

    #[doc(hidden)]
    pub fn add_forms(names: Vec<&'static str>) {
        queue(move || PALETTE.set_many(names.as_ref()));
    }

    /// Adds a [`ColorScheme`] to the list of colorschemes
    ///
    /// This [`ColorScheme`] can then be added via
    /// [`form::set_colorscheme`] or through the command
    /// `colorscheme`.
    ///
    /// If a [`ColorScheme`] of the same name was already added, it
    /// will be overritten.
    ///
    /// [`form::set_colorscheme`]: set_colorscheme
    pub fn add_colorscheme(cs: impl ColorScheme) {
        let mut colorschemes = COLORSCHEMES.lock();
        let name = cs.name();
        if let Some(i) = colorschemes.iter().position(|cs| cs.name() == name) {
            colorschemes[i] = Box::new(cs);
        } else {
            colorschemes.push(Box::new(cs));
        }
    }

    /// Applies a [`ColorScheme`]
    ///
    /// This [`ColorScheme`] must've first been added via
    /// [`form::add_colorscheme`].
    ///
    /// [`form::add_colorscheme`]: add_colorscheme
    pub fn set_colorscheme(name: &str) {
        let colorschemes = COLORSCHEMES.lock();
        if let Some(cs) = colorschemes.iter().find(|cs| cs.name() == name) {
            cs.apply();
            hooks::trigger::<ColorSchemeSet>(cs.name());
        } else {
            context::notify(err!("The colorscheme [a]{name}[] was not found"));
        }
    }

    /// Calls [`form::set`] on each tuple in the list
    ///
    /// [`form::set`]: set
    pub macro set_many($(($name:literal, $form:expr)),+ $(,)?) {{
        $(
            set($name, $form);
        )+
    }}

    /// Wether or not a specific [`Form`] has been set
    pub(crate) fn exists(name: &str) -> bool {
        FORMS.lock().contains(&name)
    }

    /// Wether or not a specific [`ColorScheme`] was added
    pub(crate) fn colorscheme_exists(name: &str) -> bool {
        COLORSCHEMES.lock().iter().any(|cs| cs.name() == name)
    }

    /// The name of a form, given a [`FormId`]
    pub(super) fn name_of(id: FormId) -> &'static str {
        FORMS.lock()[id.0 as usize]
    }

    fn position_of_name(names: &mut Vec<&'static str>, name: &'static str) -> usize {
        if let Some((i, _)) = names.iter().enumerate().find(|(_, rhs)| **rhs == name) {
            i
        } else if let Some((refed, _)) = name.rsplit_once('.') {
            position_of_name(names, refed);
            names.push(name);
            names.len() - 1
        } else {
            names.push(name);
            names.len() - 1
        }
    }

    fn queue<R>(f: impl FnOnce() -> R + Send + Sync + 'static) {
        static SENDER: Mutex<Option<mpsc::Sender<Box<dyn FnOnce() + Send + Sync>>>> =
            Mutex::new(None);

        if context::will_reload_or_quit() {
            return;
        }

        let f = Box::new(move || {
            f();
        });

        let mut sender = SENDER.lock();
        let f = if let Some(tx) = sender.as_ref() {
            let Err(err) = tx.send(f) else {
                return;
            };
            err.0
        } else {
            f
        };
        let (tx, rx) = mpsc::channel();
        tx.send(f).unwrap();
        *sender = Some(tx);

        crate::thread::spawn(move || {
            while let Ok(f) = rx.recv_timeout(Duration::from_micros(500)) {
                f();
            }
        });
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

    impl InnerFormFmt for String {
        fn kind(self) -> Kind {
            Kind::Ref(self.leak())
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

impl FormId {
    /// The internal id of the [`FormId`]
    ///
    /// This may be useful in certain situations.
    pub const fn to_u16(self) -> u16 {
        self.0
    }

    /// The name of this [`FormId`]
    pub fn name(self) -> &'static str {
        name_of(self)
    }
}

impl std::fmt::Debug for FormId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FormId({})", name_of(*self))
    }
}

/// A style for text.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Form {
    pub style: ContentStyle,
}

#[rustfmt::skip]
impl Form {
    mimic_method_new!(/**bold*/ bold Attribute::Bold);
    mimic_method_new!(/**dim*/ dim Attribute::Dim);
    mimic_method_new!(/**italic*/ italic Attribute::Italic);
    mimic_method_new!(/**underlined*/ underlined Attribute::Underlined);
    mimic_method_new!(/**double_underlined*/ double_underlined Attribute::DoubleUnderlined);
    mimic_method_new!(/**undercurled*/ undercurled Attribute::Undercurled);
    mimic_method_new!(/**underdashed*/ underdashed Attribute::Underdashed);
    mimic_method_new!(/**reverse*/ reverse Attribute::Reverse);
    mimic_method_new!(/**crossed_out*/ crossed_out Attribute::CrossedOut);
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
}

impl Form {
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
        BuiltForm(Self { style })
    }

    /// Returns a new [`Form`] with the [`Reset`] attribute
    ///
    /// In Duat, the [`Reset`] attribute should remove only other
    /// [`Attribute`]s, not any of the colors in use.
    ///
    /// [`Reset`]: Attribute::Reset
    pub const fn reset() -> BuiltForm {
        let mut built = Form::new();
        built.0.style.attributes = built.0.style.attributes.with(Attribute::Reset);
        built
    }

    /// New [`Form`] with a colored foreground
    ///
    /// This function accepts three color formats:
    ///
    /// - A hexcode, like `"#abcdef"`, capitalization is ignored;
    /// - Three rgb values, like `"rgb 123 456 789"`;
    /// - Three hsl values, like `"hsl {hue} {sat} {lit}"`, where
    ///   {hue}, {sat} and {lit} can either be a number from `0..255`,
    ///   or a percentage, followed by `'%'`, e.g. `"hsl 234 50% 42"`.
    pub const fn with(str: &str) -> BuiltForm {
        let mut built = Form::new();
        built.0.style.foreground_color = match str_to_color(str) {
            Ok(color) => Some(color),
            Err(err) => panic!("{}", err),
        };
        built
    }

    /// New [`Form`] with a colored background
    ///
    /// This function accepts three color formats:
    ///
    /// - A hexcode, like `"#abcdef"`, capitalization is ignored;
    /// - Three rgb values, like `"rgb 123 456 789"`;
    /// - Three hsl values, like `"hsl {hue} {sat} {lit}"`, where
    ///   {hue}, {sat} and {lit} can either be a number from `0..255`,
    ///   or a percentage, followed by `'%'`, e.g. `"hsl 234 50% 42"`.
    pub const fn on(str: &str) -> BuiltForm {
        let mut built = Form::new();
        built.0.style.background_color = match str_to_color(str) {
            Ok(color) => Some(color),
            Err(err) => panic!("{}", err),
        };
        built
    }

    /// New [`Form`] with a colored underlining
    ///
    /// This function accepts three color formats:
    ///
    /// - A hexcode, like `"#abcdef"`, capitalization is ignored;
    /// - Three rgb values, like `"rgb 123 456 789"`;
    /// - Three hsl values, like `"hsl {hue} {sat} {lit}"`, where
    ///   {hue}, {sat} and {lit} can either be a number from `0..255`,
    ///   or a percentage, followed by `'%'`, e.g. `"hsl 234 50% 42"`.
    pub const fn underline(str: &str) -> BuiltForm {
        let mut built = Form::new();
        built.0.style.underline_color = match str_to_color(str) {
            Ok(color) => Some(color),
            Err(err) => panic!("{}", err),
        };
        built
    }

    /// The foreground color
    pub const fn fg(&self) -> Option<Color> {
        self.style.foreground_color
    }

    /// The background color
    pub const fn bg(&self) -> Option<Color> {
        self.style.background_color
    }

    /// The foreground color
    pub const fn ul(&self) -> Option<Color> {
        self.style.underline_color
    }

    /// The attributes
    pub const fn attrs(&self) -> Attributes {
        self.style.attributes
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
    mimic_method_mod!(/**bold*/ bold Attribute::Bold);
    mimic_method_mod!(/**dim*/ dim Attribute::Dim);
    mimic_method_mod!(/**italic*/ italic Attribute::Italic);
    mimic_method_mod!(/**underlined*/ underlined Attribute::Underlined);
    mimic_method_mod!(/**double_underlined*/ double_underlined Attribute::DoubleUnderlined);
    mimic_method_mod!(/**undercurled*/ undercurled Attribute::Undercurled);
    mimic_method_mod!(/**underdashed*/ underdashed Attribute::Underdashed);
    mimic_method_mod!(/**reverse*/ reverse Attribute::Reverse);
    mimic_method_mod!(/**crossed_out*/ crossed_out Attribute::CrossedOut);
    mimic_method_mod!(/**overlined*/ overlined Attribute::OverLined);
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
}

impl BuiltForm {
    /// Adds the [`Reset`] attribute to this [`Form`]
    ///
    /// In Duat, the [`Reset`] attribute should remove only other
    /// [`Attribute`]s, not any of the colors in use.
    ///
    /// [`Reset`]: Attribute::Reset
    pub const fn reset(mut self) -> BuiltForm {
        self.0.style.attributes = self.0.style.attributes.with(Attribute::Reset);
        self
    }

    /// Colors the foreground of this [`Form`]
    ///
    /// This function accepts three color formats:
    ///
    /// - A hexcode, like `"#abcdef"`, capitalization is ignored;
    /// - Three rgb values, like `"rgb 123 456 789"`;
    /// - Three hsl values, like `"hsl {hue} {sat} {lit}"`, where
    ///   {hue}, {sat} and {lit} can either be a number from `0..255`,
    ///   or a percentage, followed by `'%'`, e.g. `"hsl 234 50% 42"`.
    pub const fn with(mut self, str: &str) -> Self {
        self.0.style.foreground_color = match str_to_color(str) {
            Ok(color) => Some(color),
            Err(err) => panic!("{}", err),
        };
        self
    }

    /// Colors the background of this [`Form`]
    ///
    /// This function accepts three color formats:
    ///
    /// - A hexcode, like `"#abcdef"`, capitalization is ignored;
    /// - Three rgb values, like `"rgb 123 456 789"`;
    /// - Three hsl values, like `"hsl {hue} {sat} {lit}"`, where
    ///   {hue}, {sat} and {lit} can either be a number from `0..255`,
    ///   or a percentage, followed by `'%'`, e.g. `"hsl 234 50% 42"`.
    pub const fn on(mut self, str: &str) -> Self {
        self.0.style.background_color = match str_to_color(str) {
            Ok(color) => Some(color),
            Err(err) => panic!("{}", err),
        };
        self
    }

    /// Colors the underlining of this [`Form`]
    ///
    /// This function accepts three color formats:
    ///
    /// - A hexcode, like `"#abcdef"`, capitalization is ignored;
    /// - Three rgb values, like `"rgb 123 456 789"`;
    /// - Three hsl values, like `"hsl {hue} {sat} {lit}"`, where
    ///   {hue}, {sat} and {lit} can either be a number from `0..255`,
    ///   or a percentage, followed by `'%'`, e.g. `"hsl 234 50% 42"`.
    pub const fn underline(mut self, str: &str) -> Self {
        self.0.style.underline_color = match str_to_color(str) {
            Ok(color) => Some(color),
            Err(err) => panic!("{}", err),
        };
        self
    }
}

impl std::ops::Deref for BuiltForm {
    type Target = Form;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for BuiltForm {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
        let mut inner = self.0.write();
        let (i, _) = position_and_form(&mut inner.forms, name);

        inner.forms[i] = (name, form, FormType::Normal);

        for refed in refs_of(&inner, i) {
            inner.forms[refed].1 = form;
        }

        if let Some(sender) = SENDER.get() {
            sender.send_form_changed().unwrap()
        }

        hooks::trigger::<FormSet>((name, FormId(i as u16), form));
    }

    /// Sets a [`Form`] "weakly"
    fn set_weak_form(&self, name: &'static str, form: Form) {
        let mut inner = self.0.write();
        let (i, _) = position_and_form(&mut inner.forms, name);

        let (_, f, f_ty) = &mut inner.forms[i];
        if let FormType::Weakest | FormType::WeakestRef(_) = f_ty {
            *f = form;
            *f_ty = FormType::Normal;

            if let Some(sender) = SENDER.get() {
                sender.send_form_changed().unwrap()
            }
            for refed in refs_of(&inner, i) {
                inner.forms[refed].1 = form;
            }
        }
    }

    /// Makes a [`Form`] reference another
    fn set_ref(&self, name: &'static str, refed: &'static str) {
        let mut inner = self.0.write();
        let (refed, form) = position_and_form(&mut inner.forms, refed);
        let (i, _) = position_and_form(&mut inner.forms, name);

        for refed in refs_of(&inner, i) {
            inner.forms[refed].1 = form;
        }

        // If it would be circular, we just don't reference anything.
        if would_be_circular(&inner, i, refed) {
            inner.forms[i] = (name, form, FormType::Normal);
        } else {
            inner.forms[i] = (name, form, FormType::Ref(refed));
        }

        if let Some(sender) = SENDER.get() {
            sender.send_form_changed().unwrap()
        }
        hooks::trigger::<FormSet>((name, FormId(i as u16), form));
    }

    /// Makes a [`Form`] reference another "weakly"
    fn set_weak_ref(&self, name: &'static str, refed: &'static str) {
        let mut inner = self.0.write();
        let (refed, form) = position_and_form(&mut inner.forms, refed);
        let (i, _) = position_and_form(&mut inner.forms, name);

        // For weak refs, no checks are done, since a form is only set if it
        // doesn't exist, and for there to be refs to it, it must exist.
        let (_, f, f_ty) = &mut inner.forms[i];
        if let FormType::Weakest | FormType::WeakestRef(_) = f_ty {
            *f = form;
            *f_ty = FormType::WeakestRef(refed);

            if let Some(sender) = SENDER.get() {
                sender.send_form_changed().unwrap()
            }
            for refed in refs_of(&inner, i) {
                inner.forms[refed].1 = form;
            }
        }
    }

    /// Sets many [`Form`]s
    fn set_many(&self, names: &[&'static str]) {
        let mut inner = self.0.write();
        for name in names {
            position_and_form(&mut inner.forms, name);
        }
    }

    /// Returns a form, given a [`FormId`].
    fn form_from_id(&self, id: FormId) -> Option<Form> {
        let inner = self.0.read();
        inner.forms.get(id.0 as usize).map(|(_, form, _)| *form)
    }

    /// The [`Form`] and [`CursorShape`] of the main cursor
    fn main_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_from_id(M_CUR_ID).unwrap();
        (form, self.0.read().main_cursor)
    }

    /// The [`Form`] and [`CursorShape`] of extra cursors
    fn extra_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_from_id(E_CUR_ID).unwrap();
        (form, self.0.read().extra_cursor)
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
    fn painter(&'static self, id: FormId) -> Painter {
        let inner = self.0.read();
        let default = inner
            .forms
            .get(id.0 as usize)
            .map(|(_, f, _)| *f)
            .unwrap_or(Form::new().0);
        Painter {
            inner,
            default,
            forms: Vec::new(),
            final_form_start: 0,
            set_fg: true,
            set_bg: true,
            set_ul: true,
            reset_attrs: false,
        }
    }
}

pub struct Painter {
    inner: RwLockReadGuard<'static, InnerPalette>,
    default: Form,
    forms: Vec<(Form, FormId)>,
    final_form_start: usize,
    set_fg: bool,
    set_bg: bool,
    set_ul: bool,
    reset_attrs: bool,
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
    pub fn apply(&mut self, id: FormId) {
        let i = id.0 as usize;
        let forms = &self.inner.forms;
        let form = forms.get(i).map(|(_, f, _)| *f).unwrap_or(Form::new().0);

        self.forms.insert(self.final_form_start, (form, id));
        if id != M_SEL_ID && id != E_SEL_ID {
            self.final_form_start += 1;
        }

        self.set_fg |= form.fg().is_some();
        self.set_bg |= form.bg().is_some();
        self.set_ul |= form.ul().is_some();
        self.reset_attrs |= form.attrs().has(Attribute::Reset);
    }

    /// Removes the [`Form`] with the given `id` and returns the
    /// result, given previous triggers
    #[inline(always)]
    pub fn remove(&mut self, id: FormId) {
        let mut applied_forms = self.forms.iter().enumerate();
        if let Some((i, &(form, _))) = applied_forms.rfind(|(_, (_, lhs))| *lhs == id) {
            self.forms.remove(i);
            if id != M_SEL_ID && id != E_SEL_ID {
                self.final_form_start -= 1;
            }

            self.set_fg |= form.fg().is_some();
            self.set_bg |= form.bg().is_some();
            self.set_ul |= form.ul().is_some();
            self.reset_attrs |= !form.attrs().is_empty();
        };
    }

    /// Removes all [`Form`]s except the default one
    ///
    /// Should be used when a [`ResetState`] part is printed
    ///
    /// [`ResetState`]: crate::text::Part::ResetState
    #[inline(always)]
    pub fn reset(&mut self) -> ContentStyle {
        self.final_form_start = 0;
        self.forms.clear();
        self.absolute_style()
    }

    /// Generates the absolute [`ContentStyle`] to be set
    ///
    /// This function assumes that all previous styling is not being
    /// carried over, i.e., we're styling from scratch.
    #[inline(always)]
    pub fn absolute_style(&self) -> ContentStyle {
        let mut style = self.default.style;

        for &(form, _) in &self.forms {
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
    pub fn relative_style(&mut self) -> ContentStyle {
        let mut style = self.absolute_style();

        if style.attributes.has(Attribute::Reset) || self.reset_attrs {
            style.attributes.set(Attribute::Reset);
        // Only when we are certain that all forms have been
        // printed, can we cull unnecessary colors for efficiency
        // (this happens most of the time).
        } else {
            style.foreground_color = self
                .set_fg
                .then_some(style.foreground_color.unwrap_or(Color::Reset));
            style.background_color = self
                .set_bg
                .then_some(style.background_color.unwrap_or(Color::Reset));
            style.underline_color = self
                .set_ul
                .then_some(style.underline_color.unwrap_or(Color::Reset));
        }

        self.set_fg = false;
        self.set_bg = false;
        self.set_ul = false;
        self.reset_attrs = false;

        style
    }

    #[inline(always)]
    pub fn apply_main_cursor(&mut self) {
        self.apply(M_CUR_ID);
        self.final_form_start -= 1;
    }

    #[inline(always)]
    pub fn remove_main_cursor(&mut self) {
        self.final_form_start += 1;
        self.remove(M_CUR_ID);
    }

    #[inline(always)]
    pub fn apply_extra_cursor(&mut self) {
        self.apply(E_CUR_ID);
        self.final_form_start -= 1;
    }

    #[inline(always)]
    pub fn remove_extra_cursor(&mut self) {
        self.final_form_start += 1;
        self.remove(E_CUR_ID);
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
        self.default
    }
}

pub(crate) fn set_sender(sender: Sender) {
    SENDER
        .set(sender)
        .unwrap_or_else(|_| panic!("Sender set more than once"));
}

/// An enum that helps in the modification of forms
#[derive(Clone)]
enum FormType {
    Normal,
    Ref(usize),
    Weakest,
    WeakestRef(usize),
}

/// The position of each form that eventually references the `n`th
fn refs_of(inner: &InnerPalette, refed: usize) -> Vec<usize> {
    let mut refs = Vec::new();
    for (i, (.., f_ty)) in inner.forms.iter().enumerate() {
        if let FormType::Ref(ref_id) | FormType::WeakestRef(ref_id) = f_ty
            && *ref_id == refed
        {
            refs.push(i);
            refs.extend(refs_of(inner, i));
        }
    }
    refs
}

/// If form references would eventually lead to a loop
fn would_be_circular(inner: &InnerPalette, referee: usize, refed: usize) -> bool {
    if let (.., FormType::Ref(refed_ref) | FormType::WeakestRef(refed_ref)) = inner.forms[refed] {
        match refed_ref == referee {
            true => true,
            false => would_be_circular(inner, referee, refed_ref),
        }
    } else {
        false
    }
}

fn position_and_form(forms: &mut Vec<(&str, Form, FormType)>, name: &'static str) -> (usize, Form) {
    if let Some((i, (_, form, _))) = forms.iter().enumerate().find(|(_, (lhs, ..))| *lhs == name) {
        (i, *form)
    } else if let Some((refed, _)) = name.rsplit_once('.') {
        let (i, form) = position_and_form(forms, refed);
        forms.push((name, form, FormType::WeakestRef(i)));
        (forms.len() - 1, form)
    } else {
        forms.push((name, Form::new().0, FormType::Weakest));
        (forms.len() - 1, Form::new().0)
    }
}

/// Converts a string to a color, supporst hex, RGB and HSL
const fn str_to_color(str: &str) -> std::result::Result<Color, &'static str> {
    use core::str::from_utf8_unchecked as utf8_str;
    const fn strip_prefix<'a>(prefix: &str, str: &'a str) -> Option<&'a str> {
        let prefix = prefix.as_bytes();
        let str = str.as_bytes();

        let mut i = 0;
        while i < prefix.len() {
            if str[i] != prefix[i] {
                return None;
            }
            i += 1;
        }

        let (_, str) = str.split_at(prefix.len());
        Some(unsafe { utf8_str(str) })
    }
    const fn strip_suffix<'a>(suffix: &str, str: &'a str) -> Option<&'a str> {
        let prefix = suffix.as_bytes();
        let str = str.as_bytes();

        let mut i = str.len() - 1;
        while i >= str.len() - prefix.len() {
            if str[i] != prefix[i - (str.len() - prefix.len())] {
                return None;
            }
            i += 1;
        }

        let (str, _) = str.split_at(str.len() - suffix.len());
        Some(unsafe { utf8_str(str) })
    }
    const fn split_space(str: &str) -> Option<(&str, &str)> {
        if str.is_empty() {
            return None;
        }
        let str = str.as_bytes();

        let mut i = 0;
        while i < str.len() {
            if str[i] == b' ' {
                break;
            }
            i += 1;
        }

        let (cut, rest) = str.split_at(i);
        let (_, rest) = rest.split_at(if rest.is_empty() { 0 } else { 1 });
        Some(unsafe { (utf8_str(cut), utf8_str(rest)) })
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

        Ok(Color::Rgb { r, g, b })
        // Expects "rgb {red} {green} {blue}"
    } else if let Some(mut rgb) = strip_prefix("rgb ", str) {
        let mut values = [0, 0, 0];
        let mut i = 0;
        while i < values.len() {
            if let Some((cut, rest)) = split_space(rgb) {
                rgb = rest;
                values[i] = match u8::from_str_radix(cut, 10) {
                    Ok(value) => value,
                    Err(_) => return Err("Rgb format value could not be parsed"),
                }
            } else {
                return Err("Missing value in rgb format");
            }
            i += 1;
        }

        let [r, g, b] = values;
        Ok(Color::Rgb { r, g, b })
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
        Ok(Color::Rgb { r, g, b })
    } else {
        return Err("Color format was not recognized");
    }
}

impl std::fmt::Debug for Form {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct DebugColor(Option<Color>);
        impl std::fmt::Debug for DebugColor {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.0 {
                    Some(Color::Rgb { r, g, b }) => write!(f, "Some(Rgb({r}, {g}, {b}))"),
                    Some(Color::AnsiValue(ansi)) => write!(f, "Some(Ansi({ansi}))"),
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
        struct DebugForms<'a>(&'a [(&'static str, Form, FormType)]);
        impl std::fmt::Debug for DebugForms<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if f.alternate() {
                    f.write_str("[\n")?;
                    let max = self.0.len().ilog10() as usize + 3;
                    for (n, entry) in self.0.iter().enumerate() {
                        let num = format!("{n}:");
                        writeln!(f, "{num:<max$}{entry:#?}")?;
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
            .finish()
    }
}

impl std::fmt::Debug for FormType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Normal => write!(f, "Normal"),
            Self::Ref(refed) => write!(f, "Ref({refed})"),
            Self::Weakest => write!(f, "Weakest"),
            Self::WeakestRef(refed) => write!(f, "WeakestRef({refed})"),
        }
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
        pub const fn $ul() -> BuiltForm {
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
