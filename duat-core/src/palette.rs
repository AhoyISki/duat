use std::sync::LazyLock;

pub use crossterm::cursor::SetCursorStyle as CursorShape;
use crossterm::style::{Attribute, Attributes, Color, ContentStyle, Stylize};
pub use global::*;
use parking_lot::RwLockReadGuard;

use crate::data::RwData;

mod global {
    use super::{CursorShape, Form, FormId, FormPalette, Painter};

    static PALETTE: FormPalette = FormPalette::new();

    /// Sets the `Form` with a given name to a new one.
    pub fn set_form(name: impl AsRef<str>, form: Form) -> FormId {
        PALETTE.set_form(name, form)
    }

    pub fn set_weak_form(name: impl AsRef<str>, form: Form) -> FormId {
        PALETTE.set_weak_form(name, form)
    }

    /// Sets the `Form` with a given name to a new one.
    pub fn set_source(name: impl AsRef<str>, referenced: impl AsRef<str>) -> FormId {
        PALETTE.set_ref(name, referenced)
    }

    pub fn set_weak_ref(name: impl AsRef<str>, referenced: impl AsRef<str>) -> FormId {
        PALETTE.set_weak_ref(name, referenced)
    }

    /// Returns the `Form` associated to a given name with the index
    /// for efficient access.
    ///
    /// If a [`Form`] with the given name was not added prior, it will
    /// be added with the same form as the "Default" form.
    pub fn id_of_form(name: impl AsRef<str>) -> FormId {
        PALETTE.id_of_name(name)
    }

    /// Returns a form, given an index.
    pub fn form_of_id(id: FormId) -> Form {
        PALETTE.form_of_id(id)
    }

    pub fn name_of_id(id: FormId) -> &'static str {
        PALETTE.name_of_id(id)
    }

    pub fn main_cursor() -> (Form, Option<CursorShape>) {
        PALETTE.main_cursor()
    }

    pub fn extra_cursor() -> (Form, Option<CursorShape>) {
        PALETTE.extra_cursor()
    }

    pub fn set_main_cursor(shape: CursorShape) {
        PALETTE.set_main_cursor(shape)
    }

    pub fn set_extra_cursor(shape: CursorShape) {
        PALETTE.set_extra_cursor(shape)
    }

    pub fn unset_main_cursor() {
        PALETTE.unset_main_cursor()
    }

    pub fn unset_extra_cursor() {
        PALETTE.unset_extra_cursor()
    }

    pub fn painter() -> Painter {
        PALETTE.painter()
    }

    /// Not meant for external use. Please use [`form_of_name`].
    #[doc(hidden)]
    pub fn __weakest_id_of_name(name: impl AsRef<str>) -> FormId {
        PALETTE.weakest_id_of_name(name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FormId(u16);

/// A style for text.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Form {
    pub style: ContentStyle,
    /// Wether or not the `Form`s colors and attributes should
    /// override any that come after.
    pub is_final: bool,
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

pub(crate) const DEFAULT: FormId = FormId(0);
pub(crate) const MAIN_CURSOR: FormId = FormId(8);
pub(crate) const EXTRA_CURSOR: FormId = FormId(9);
pub(crate) const MAIN_SEL: FormId = FormId(10);
pub(crate) const EXTRA_SEL: FormId = FormId(11);

#[derive(Debug, Clone, Copy)]
enum Kind {
    Form(Form),
    Ref(FormId),
    WeakestRef(FormId),
}

struct InnerPalette {
    main_cursor: Option<CursorShape>,
    extra_cursor: Option<CursorShape>,
    forms: Vec<(&'static str, Kind)>,
}

/// The list of forms to be used when rendering.
struct FormPalette(LazyLock<RwData<InnerPalette>>);

impl FormPalette {
    const fn new() -> Self {
        Self(LazyLock::new(|| {
            let main_cursor = Some(CursorShape::DefaultUserShape);

            let forms = vec![
                ("Default", Kind::Form(Form::new())),
                ("Accent", Kind::Form(Form::new().bold())),
                ("DefaultOk", Kind::Form(Form::new())),
                ("AccentOk", Kind::Form(Form::new().green())),
                ("DefaultErr", Kind::Form(Form::new())),
                ("AccentErr", Kind::Form(Form::new().red())),
                ("DefaultHint", Kind::Form(Form::new())),
                ("AccentHint", Kind::Form(Form::new().bold())),
                ("MainCursor", Kind::Form(Form::new().reverse())),
                ("ExtraCursor", Kind::Ref(MAIN_CURSOR)),
                ("MainSelection", Kind::Form(Form::new().on_dark_grey())),
                ("ExtraSelection", Kind::Ref(MAIN_SEL)),
            ];

            RwData::new(InnerPalette {
                main_cursor,
                extra_cursor: main_cursor,
                forms,
            })
        }))
    }

    /// Sets the `Form` with a given name to a new one.
    fn set_form(&self, name: impl AsRef<str>, form: Form) -> FormId {
        let name = name.as_ref().to_string().leak();

        let mut inner = self.0.write();

        if let Some((index, (_, old_form))) = inner
            .forms
            .iter_mut()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
        {
            *old_form = Kind::Form(form);
            FormId(index as u16)
        } else {
            inner.forms.push((name, Kind::Form(form)));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    fn set_weak_form(&self, name: impl AsRef<str>, form: Form) -> FormId {
        let name = name.as_ref().to_string().leak();

        let mut inner = self.0.write();

        if let Some((index, (_, kind))) = inner
            .forms
            .iter_mut()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
        {
            if let Kind::WeakestRef(_) = *kind {
                *kind = Kind::Form(form);
            }
            FormId(index as u16)
        } else {
            inner.forms.push((name, Kind::Form(form)));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Sets the `Form` with a given name to a new one.
    fn set_ref(&self, name: impl AsRef<str>, referenced: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();
        let referenced: &'static str = referenced.as_ref().to_string().leak();

        let id = self.id_of_name(referenced);

        let mut inner = self.0.write();

        if let Some((index, (_, old_form))) = inner
            .forms
            .iter_mut()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
        {
            *old_form = Kind::Ref(id);
            FormId(index as u16)
        } else {
            inner.forms.push((name, Kind::Ref(id)));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    fn set_weak_ref(&self, name: impl AsRef<str>, referenced: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();
        let referenced: &'static str = referenced.as_ref().to_string().leak();

        let ref_id = self.id_of_name(referenced);

        let mut inner = self.0.write();

        if let Some((index, (_, kind))) = inner
            .forms
            .iter_mut()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
        {
            if let Kind::WeakestRef(_) = *kind {
                *kind = Kind::Ref(ref_id);
            }
            FormId(index as u16)
        } else {
            inner.forms.push((name, Kind::Ref(ref_id)));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Returns the `Form` associated to a given name with the index
    /// for efficient access.
    ///
    /// If a [`Form`] with the given name was not added prior, it will
    /// be added with the same form as the "Default" form.
    fn id_of_name(&self, name: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();

        let mut inner = self.0.write();

        if let Some(index) = inner.forms.iter_mut().position(|(cmp, _)| *cmp == name) {
            FormId(index as u16)
        } else {
            let name = name.to_string().leak();
            inner.forms.push((name, Kind::Ref(FormId(0))));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Returns the `Form` associated to a given name with the index
    /// for efficient access.
    ///
    /// If a [`Form`] with the given name was not added prior, it will
    /// be added with the same form as the "Default" form.
    fn weakest_id_of_name(&self, name: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();

        let mut inner = self.0.write();

        if let Some(index) = inner.forms.iter_mut().position(|(cmp, _)| *cmp == name) {
            FormId(index as u16)
        } else {
            let name = name.to_string().leak();
            inner.forms.push((name, Kind::WeakestRef(FormId(0))));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Returns a form, given an index.
    fn form_of_id(&self, id: FormId) -> Form {
        let inner = self.0.read();

        let nth = inner.forms.get(id.0 as usize).map(|(_, kind)| match kind {
            Kind::Form(form) => *form,
            Kind::Ref(id) => self.form_of_id(*id),
            Kind::WeakestRef(id) => self.form_of_id(*id),
        });

        let Some(ret) = nth else {
            unreachable!("Form with id {} not found, this should never happen", id.0);
        };
        ret
    }

    fn name_of_id(&self, id: FormId) -> &'static str {
        let inner = self.0.read();
        let nth = inner.forms.get(id.0 as usize).map(|(name, _)| name);

        let Some(ret) = nth else {
            unreachable!("Form with id {} not found, this should never happen", id.0);
        };
        ret
    }

    fn main_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_of_id(MAIN_CURSOR);
        (form, self.0.read().main_cursor)
    }

    fn extra_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_of_id(EXTRA_CURSOR);
        (form, self.0.read().extra_cursor)
    }

    fn set_main_cursor(&self, style: CursorShape) {
        self.0.write().main_cursor = Some(style);
    }

    fn set_extra_cursor(&self, style: CursorShape) {
        self.0.write().extra_cursor = Some(style);
    }

    fn unset_main_cursor(&self) {
        self.0.write().main_cursor = None;
    }

    fn unset_extra_cursor(&self) {
        self.0.write().extra_cursor = None;
    }

    fn painter(&'static self) -> Painter {
        Painter {
            palette: self.0.read(),
            forms: vec![(self.form_of_id(DEFAULT), DEFAULT)],
            cur_form: self.form_of_id(DEFAULT),
        }
    }
}

impl Default for FormPalette {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Painter {
    palette: RwLockReadGuard<'static, InnerPalette>,
    forms: Vec<(Form, FormId)>,
    cur_form: Form,
}

impl Painter {
    /// Applies the `Form` with the given `id` and returns the result,
    /// given previous triggers.
    #[inline(always)]
    pub fn apply(&mut self, id: FormId) -> Option<Form> {
        let form = self.get_form(id);

        self.forms.push((form, id));
        let form = self.make_form();
        if form == self.cur_form {
            None
        } else {
            self.cur_form = form;
            Some(form)
        }
    }

    /// Removes the [`Form`] with the given `id` and returns the
    /// result, given previous triggers.
    #[inline(always)]
    pub fn remove(&mut self, id: FormId) -> Option<Form> {
        let mut applied_forms = self.forms.iter().enumerate();
        if let Some((index, _)) = applied_forms.rfind(|(_, &(_, i))| i == id) {
            self.forms.remove(index);
            let form = self.make_form();
            if form == self.cur_form {
                None
            } else {
                self.cur_form = form;
                Some(form)
            }
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn reset(&mut self) -> Option<Form> {
        self.forms.splice(1.., []);
        let form = self.make_form();
        if form == self.cur_form {
            None
        } else {
            self.cur_form = form;
            Some(form)
        }
    }

    /// Generates the form to be printed, given all the previously
    /// pushed forms in the `Form` stack.
    #[inline(always)]
    pub fn make_form(&self) -> Form {
        let style = ContentStyle {
            foreground_color: None,
            background_color: None,
            underline_color: None,
            attributes: Attributes::default(),
        };

        let mut form = Form {
            style,
            is_final: false,
        };

        fn set_var<T: Clone>(
            is_done: &mut bool,
            var: &mut Option<T>,
            maybe_new: &Option<T>,
            is_final: bool,
        ) {
            if let Some(new_var) = maybe_new
                && (!*is_done || is_final)
            {
                *var = Some(new_var.clone());
                *is_done |= is_final;
            }
        }

        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false, false);

        for &(Form { style, is_final }, _) in &self.forms {
            let to_change = &mut form.style.foreground_color;
            let new_foreground = style.foreground_color;
            set_var(&mut fg_done, to_change, &new_foreground, is_final);

            let to_change = &mut form.style.background_color;
            let new_background = style.background_color;
            set_var(&mut bg_done, to_change, &new_background, is_final);

            let to_change = &mut form.style.underline_color;
            let new_underline = style.underline_color;
            set_var(&mut ul_done, to_change, &new_underline, is_final);

            if !attr_done || is_final {
                form.style.attributes.extend(style.attributes);
                attr_done |= is_final;
            }

            if fg_done && bg_done && ul_done && attr_done {
                break;
            }
        }

        form
    }

    /// The [`Form`] "ExtraCursor", and its shape.
    pub fn main_cursor(&self) -> (Form, Option<CursorShape>) {
        (self.get_form(MAIN_CURSOR), self.palette.main_cursor)
    }

    /// The [`Form`] "ExtraCursor", and its shape.
    pub fn extra_cursor(&self) -> (Form, Option<CursorShape>) {
        (self.get_form(EXTRA_CURSOR), self.palette.extra_cursor)
    }

    pub fn get_default(&self) -> Form {
        self.forms[0].0
    }

    /// Gets the [`Form`] from a [`FormId`].
    fn get_form(&self, mut id: FormId) -> Form {
        loop {
            match self.palette.forms.get(id.0 as usize) {
                Some((_, Kind::Form(form))) => break *form,
                Some((_, Kind::Ref(referenced))) => id = *referenced,
                Some((_, Kind::WeakestRef(referenced))) => id = *referenced,
                _ => {
                    unreachable!("This should not be possible")
                }
            }
        }
    }
}

macro mimic_method {
    ($method:ident $type:ty) => {
        pub fn $method(self, val: $type) -> Self {
            let Form { style, is_final } = self;
            Form { style: Stylize::$method(style, val), is_final }
        }
    },

    ($($method:ident)+) => {
        $(
        pub fn $method(self) -> Self {
            let Form { style, is_final } = self;
            Form { style: Stylize::$method(style), is_final }
        }
        )*
    }
}
