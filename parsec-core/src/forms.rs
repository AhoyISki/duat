use std::sync::{LazyLock, RwLockReadGuard};

use crossterm::{
    cursor::SetCursorStyle,
    style::{Attribute, Attributes, Color, ContentStyle, Stylize},
};

use crate::data::{RwData};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FormId(usize);

/// A style for text.
#[derive(Default, Debug, Clone, Copy)]
pub struct Form {
    pub style: ContentStyle,
    /// Wether or not the `Form`s colors and attributes should
    /// override any that come after.
    pub is_final: bool,
}

macro_rules! mimic_method {
    ($method:ident $type:ty) => {
        pub fn $method(self, val: $type) -> Self {
            let Form { style, is_final } = self;
            Form { style: Stylize::$method(style, val), is_final }
        }
    };
    ($($method:ident)+) => {
        $(
        pub fn $method(self) -> Self {
            let Form { style, is_final } = self;
            Form { style: Stylize::$method(style), is_final }
        }
        )*
    }
}

#[rustfmt::skip]
impl Form {
    mimic_method!(with Color);
    mimic_method!(on Color);
    mimic_method!(underline Color );
    mimic_method!(attribute Attribute );
    mimic_method!(reset);
    mimic_method!(bold);
    mimic_method!(underlined);
    mimic_method!(reverse);
    mimic_method!(dim);
    mimic_method!(italic);
    mimic_method!(negative);
    mimic_method!(slow_blink);
    mimic_method!(rapid_blink);
    mimic_method!(hidden);
    mimic_method!(crossed_out);
    mimic_method!(black on_black underline_black);
    mimic_method!(dark_grey on_dark_grey underline_dark_grey);
    mimic_method!(red on_red underline_red);
    mimic_method!(dark_red on_dark_red underline_dark_red);
    mimic_method!(green on_green underline_green);
    mimic_method!(dark_green on_dark_green underline_dark_green);
    mimic_method!(yellow on_yellow underline_yellow);
    mimic_method!(dark_yellow on_dark_yellow underline_dark_yellow);
    mimic_method!(blue on_blue underline_blue);
    mimic_method!(dark_blue on_dark_blue underline_dark_blue);
    mimic_method!(magenta on_magenta underline_magenta);
    mimic_method!(dark_magenta on_dark_magenta underline_dark_magenta);
    mimic_method!(cyan on_cyan underline_cyan);
    mimic_method!(dark_cyan on_dark_cyan underline_dark_cyan);
    mimic_method!(white on_white underline_white);
    mimic_method!(grey on_grey underline_grey);

    pub fn new() -> Self {
        Self { style: ContentStyle::new(), is_final: false }
    }

    pub fn new_final() -> Self {
        Self { style: ContentStyle::default(), is_final: true }
    }
}

#[derive(Clone, Copy)]
pub struct CursorStyle {
    /// An optional member when using application specific cursors.
    pub caret: Option<SetCursorStyle>,
    // NOTE: This is obligatory as a fallback for when the application can't render the
    // cursor with `caret`.
    /// To render the cursor as a form, not as an actual cursor.
    pub form: Form,
}

impl CursorStyle {
    pub const fn new(caret: Option<SetCursorStyle>, form: Form) -> Self {
        Self { caret, form }
    }
}

impl std::fmt::Debug for CursorStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.form, f)
    }
}

pub const DEFAULT: FormId = FormId(0);
pub const MAIN_SEL: FormId = FormId(5);
pub const EXTRA_SEL: FormId = FormId(6);

#[derive(Clone, Copy)]
enum Kind {
    Form(Form),
    Ref(&'static str),
}

struct InnerPalette {
    main_cursor: CursorStyle,
    extra_cursor: CursorStyle,
    forms: Vec<(&'static str, Kind)>,
}

impl InnerPalette {
    fn get_from_name(&self, mut name: &'static str) -> Option<(Form, FormId)> {
        let iter = self.forms.iter().enumerate();

        loop {
            match iter.clone().find(|(_, (cmp, _))| *cmp == name) {
                Some((index, (_, Kind::Form(form)))) => break Some((*form, FormId(index))),
                Some((_, (_, Kind::Ref(referenced)))) => name = referenced,
                None => break None,
            }
        }
    }
}

/// The list of forms to be used when rendering.
pub struct FormPalette(LazyLock<RwData<InnerPalette>>);

impl FormPalette {
    pub(crate) const fn new() -> Self {
        Self(LazyLock::new(|| {
            let main_cursor = CursorStyle::new(
                Some(SetCursorStyle::DefaultUserShape),
                Form::new().reverse(),
            );

            let forms = vec![
                ("Default", Kind::Form(Form::default())),
                ("MainSelection", Kind::Form(Form::new().on_dark_grey())),
                ("ExtraSelection", Kind::Ref("MainSelection")),
            ];

            RwData::new(InnerPalette {
                main_cursor,
                extra_cursor: main_cursor,
                forms,
            })
        }))
    }

    /// Sets the `Form` with a given name to a new one.
    pub fn set_form(&self, name: impl AsRef<str>, form: Form) -> FormId {
        let name = name.as_ref().to_string().leak();

        let mut inner = self.0.write();

        if let Some((index, (_, old_form))) = inner
            .forms
            .iter_mut()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
        {
            *old_form = Kind::Form(form);
            FormId(index)
        } else {
            inner.forms.push((name, Kind::Form(form)));
            FormId(inner.forms.len() - 1)
        }
    }

    pub fn try_set_form(&self, name: impl AsRef<str>, form: Form) -> FormId {
        let name = name.as_ref().to_string().leak();

        let mut inner = self.0.write();

        if let Some((_, form_id)) = inner.get_from_name(name) {
            form_id
        } else {
            inner.forms.push((name, Kind::Form(form)));
            FormId(inner.forms.len() - 1)
        }
    }

    /// Sets the `Form` with a given name to a new one.
    pub fn set_ref(&self, name: impl AsRef<str>, referenced: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();
        let referenced: &'static str = referenced.as_ref().to_string().leak();

        let mut inner = self.0.write();

        if let Some((_, old_form)) = inner.forms.iter_mut().find(|(cmp, _)| *cmp == name) {
            *old_form = Kind::Ref(referenced);
            drop(inner);
            self.from_name(referenced).1
        } else {
            inner.forms.push((name, Kind::Ref(referenced)));
            drop(inner);
            self.from_name(referenced).1
        }
    }

    pub fn set_new_ref(&self, name: impl AsRef<str>, referenced: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();
        let referenced: &'static str = referenced.as_ref().to_string().leak();

        let mut inner = self.0.write();

        if let Some((_, form_id)) = inner.get_from_name(name) {
            form_id
        } else {
            inner.forms.push((name, Kind::Ref(referenced)));
            drop(inner);
            self.from_name(referenced).1
        }
    }

    /// Returns the `Form` associated to a given name with the index
    /// for efficient access.
    ///
    /// If a [`Form`] with the given name was not added prior, it will be added
    /// with the same form as the "Default" form.
    pub fn from_name(&self, name: impl AsRef<str>) -> (Form, FormId) {
        let name = name.as_ref().to_string().leak();

        let mut inner = self.0.write();

        if let Some((form, id)) = inner.get_from_name(name) {
            (form, id)
        } else {
            let name = name.to_string().leak();
            inner.forms.push((name, Kind::Ref("Default")));
            drop(inner);
            self.from_name("Default")
        }
    }

    /// Returns a form, given an index.
    pub fn from_id(&self, id: FormId) -> Form {
        let inner = self.0.read();

        let nth = inner.forms.get(id.0).and_then(|(_, kind)| match kind {
            Kind::Form(form) => Some(*form),
            Kind::Ref(name) => inner.get_from_name(name).map(|(form, _)| form),
        });

        let Some(ret) = nth else {
            unreachable!("Form with id {} not found, this should never happen", id.0);
        };
        ret
    }

    pub fn name_from_id(&self, id: FormId) -> &'static str {
        let inner = self.0.read();
        let nth = inner.forms.get(id.0).map(|(name, _)| name);

        let Some(ret) = nth else {
            unreachable!("Form with id {} not found, this should never happen", id.0);
        };
        ret
    }

    pub fn main_cursor(&self) -> CursorStyle {
        self.0.read().main_cursor
    }

    pub fn extra_cursor(&self) -> CursorStyle {
        self.0.read().extra_cursor
    }

    pub fn set_main_cursor(&self, style: CursorStyle) {
        self.0.write().main_cursor = style;
    }

    pub fn set_extra_cursor(&self, style: CursorStyle) {
        self.0.write().extra_cursor = style;
    }

    pub fn form_former(&self) -> FormFormer {
        FormFormer {
            palette: self.0.read(),
            forms: Vec::new(),
        }
    }
}

impl Default for FormPalette {
    fn default() -> Self {
        Self::new()
    }
}

pub struct FormFormer<'a> {
    palette: RwLockReadGuard<'a, InnerPalette>,
    forms: Vec<(Form, FormId)>,
}

impl<'a> FormFormer<'a> {
    /// Applies the `Form` with the given `id` and returns the result,
    /// given previous triggers.
    pub fn apply(&mut self, id: FormId) -> Form {
        let form = match self.palette.forms.get(id.0) {
            Some((_, Kind::Form(form))) => form,
            Some((_, Kind::Ref(referenced))) => panic!("{referenced}"),
            _ => {
                unreachable!("This should not be possible");
            }
        };
        self.forms.push((*form, id));
        self.make_form()
    }

    /// Generates the form to be printed, given all the previously
    /// pushed forms in the `Form` stack.
    pub fn make_form(&self) -> Form {
        let style = ContentStyle {
            foreground_color: Some(Color::Reset),
            background_color: Some(Color::Reset),
            underline_color: Some(Color::Reset),
            attributes: Attributes::default(),
        };

        let mut form = Form {
            style,
            is_final: false,
        };

        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false, false);

        for &(Form { style, is_final }, _) in &self.forms {
            let new_foreground = style.foreground_color;
            set_var(
                &mut fg_done,
                &mut form.style.foreground_color,
                &new_foreground,
                is_final,
            );

            let new_background = style.background_color;
            set_var(
                &mut bg_done,
                &mut form.style.background_color,
                &new_background,
                is_final,
            );

            let new_underline = style.underline_color;
            set_var(
                &mut ul_done,
                &mut form.style.underline_color,
                &new_underline,
                is_final,
            );

            if !attr_done {
                form.style.attributes.extend(style.attributes);
                if is_final {
                    attr_done = true
                }
            }

            if fg_done && bg_done && ul_done && attr_done {
                break;
            }
        }

        form
    }

    /// Removes the [`Form`] with the given `id` and returns the
    /// result, given previous triggers.
    pub fn remove(&mut self, id: FormId) -> Option<Form> {
        let mut applied_forms = self.forms.iter().enumerate();
        if let Some((index, _)) = applied_forms.rfind(|(_, &(_, i))| i == id) {
            self.forms.remove(index);
            Some(self.make_form())
        } else {
            None
        }
    }

    pub fn reset(&mut self) -> Form {
        self.forms.clear();
        self.make_form()
    }

    pub fn main_cursor(&self) -> CursorStyle {
        self.palette.main_cursor
    }

    pub fn extra_cursor(&self) -> CursorStyle {
        self.palette.extra_cursor
    }
}

/// Internal method used only to shorten code in `make_form()`.
fn set_var<T>(is_set: &mut bool, var: &mut Option<T>, maybe_new: &Option<T>, is_final: bool)
where
    T: Clone,
{
    if let (Some(new_var), false) = (maybe_new, &is_set) {
        *var = Some(new_var.clone());
        if is_final {
            *is_set = true
        };
    }
}
