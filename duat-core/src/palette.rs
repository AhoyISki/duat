use std::sync::LazyLock;

pub use crossterm::cursor::SetCursorStyle as CursorShape;
use crossterm::style::{Attribute, Color, ContentStyle, Stylize};
pub use global::*;

use crate::data::{RwData, RwLockReadGuard};

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
    pub fn set_ref(name: impl AsRef<str>, referenced: impl AsRef<str>) -> FormId {
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
    pub fn id_from_name(name: impl AsRef<str>) -> FormId {
        PALETTE.id_from_name(name)
    }

    /// Returns a form, given an index.
    pub fn form_from_id(id: FormId) -> Form {
        PALETTE.form_from_id(id)
    }

    pub fn name_from_id(id: FormId) -> &'static str {
        PALETTE.name_from_id(id)
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

    pub fn as_final(self) -> Self {
        Self { is_final: true, ..self }
    }
}

pub const DEFAULT_FORM_ID: FormId = FormId(0);
pub const MAIN_CURSOR_FORM_ID: FormId = FormId(8);
pub const EXTRA_CURSOR_FORM_ID: FormId = FormId(9);
pub const MAIN_SEL_FORM_ID: FormId = FormId(10);
pub const EXTRA_SEL_FORM_ID: FormId = FormId(11);

#[derive(Debug, Clone, Copy)]
enum Kind {
    Form(Form),
    Ref(FormId),
}

struct InnerPalette {
    main_cursor: Option<CursorShape>,
    extra_cursor: Option<CursorShape>,
    forms: Vec<(&'static str, Kind)>,
}

impl InnerPalette {
    fn form_of_id(&self, id: FormId) -> Form {
        let nth = self.forms.get(id.0 as usize).map(|(_, kind)| match kind {
            Kind::Form(form) => *form,
            Kind::Ref(new) => self.form_of_id(*new),
        });

        let Some(ret) = nth else {
            unreachable!("Form with id {} not found, this should never happen", id.0);
        };
        ret
    }
}

/// The list of forms to be used when rendering.
struct FormPalette {
    inner: LazyLock<RwData<InnerPalette>>,
    weak_refs: LazyLock<RwData<Vec<(&'static str, u16)>>>,
}

impl FormPalette {
    /// Returns a new instance of [`FormPalette`]
    const fn new() -> Self {
        Self {
            inner: LazyLock::new(|| {
                let main_cursor = Some(CursorShape::DefaultUserShape);

                let forms = vec![
                    ("Default", Kind::Form(Form::new())),
                    ("Accent", Kind::Form(Form::new().bold())),
                    ("DefaultOk", Kind::Form(Form::new().blue())),
                    ("AccentOk", Kind::Form(Form::new().cyan())),
                    ("DefaultErr", Kind::Form(Form::new().red())),
                    ("AccentErr", Kind::Form(Form::new().red().bold())),
                    ("DefaultHint", Kind::Form(Form::new().grey())),
                    ("AccentHint", Kind::Form(Form::new().grey().bold())),
                    ("MainCursor", Kind::Form(Form::new().reverse())),
                    ("ExtraCursor", Kind::Ref(MAIN_CURSOR_FORM_ID)),
                    ("MainSelection", Kind::Form(Form::new().on_dark_grey())),
                    ("ExtraSelection", Kind::Ref(MAIN_SEL_FORM_ID)),
                ];

                RwData::new(InnerPalette {
                    main_cursor,
                    extra_cursor: main_cursor,
                    forms,
                })
            }),
            weak_refs: LazyLock::new(RwData::default),
        }
    }

    /// Sets a [`Form`]
    fn set_form(&self, name: impl AsRef<str>, form: Form) -> FormId {
        let name: &str = name.as_ref().to_string().leak();
        let form = match name {
            "MainCursor" | "ExtraCursor" => form.as_final(),
            _ => form,
        };

        let mut inner = self.inner.write();

        if let Some((_, id)) = self
            .weak_refs
            .mutate(|weak_refs| weak_refs.extract_if(|(cmp, _)| *cmp == name).next())
        {
            let max = ((id + 1) as usize).max(inner.forms.len());
            inner.forms.resize(max, ("", Kind::Ref(DEFAULT_FORM_ID)));
            inner.forms[id as usize] = (name, Kind::Form(form));
            FormId(id)
        } else if let Some((index, (_, old_form))) = inner
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

    /// Sets a [`Form`] "weakly"
    fn set_weak_form(&self, name: impl AsRef<str>, form: Form) -> FormId {
        let name: &str = name.as_ref().to_string().leak();
        let form = match name {
            "MainCursor" | "ExtraCursor" => form.as_final(),
            _ => form,
        };

        let mut inner = self.inner.write();

        if let Some((_, id)) = self
            .weak_refs
            .mutate(|weak_refs| weak_refs.extract_if(|(n, _)| *n == name).next())
        {
            let max = ((id + 1) as usize).max(inner.forms.len());
            inner.forms.resize(max, ("", Kind::Ref(DEFAULT_FORM_ID)));
            inner.forms[id as usize] = (name, Kind::Form(form));
            FormId(id)
        } else if let Some((index, _)) = inner
            .forms
            .iter()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
        {
            FormId(index as u16)
        } else {
            inner.forms.push((name, Kind::Form(form)));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Sets a [`Form`] to reference another
    ///
    /// Returns `None` if the referenced form doesn't exist.
    fn set_ref(&self, name: impl AsRef<str>, referenced: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();
        let referenced: &'static str = referenced.as_ref().to_string().leak();
        let ref_id = self.create_id_from_name(referenced);

        let mut inner = self.inner.write();

        if let Some((_, id)) = self
            .weak_refs
            .mutate(|weak_refs| weak_refs.extract_if(|(cmp, _)| *cmp == name).next())
        {
            let max = ((id + 1) as usize).max(inner.forms.len());
            inner.forms.resize(max, ("", Kind::Ref(DEFAULT_FORM_ID)));
            inner.forms[id as usize] = (name, Kind::Ref(ref_id));
            FormId(id)
        } else if let Some((index, (_, old_form))) = inner
            .forms
            .iter_mut()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
        {
            *old_form = Kind::Ref(ref_id);
            FormId(index as u16)
        } else {
            inner.forms.push((name, Kind::Ref(ref_id)));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Makes a [`Form`] reference another "weakly"
    fn set_weak_ref(&self, name: impl AsRef<str>, referenced: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();
        let referenced: &'static str = referenced.as_ref().to_string().leak();
        let ref_id = self.create_id_from_name(referenced);

        let mut inner = self.inner.write();

        if let Some((_, id)) = self
            .weak_refs
            .mutate(|weak_refs| weak_refs.extract_if(|(cmp, _)| *cmp == name).next())
        {
            let max = ((id + 1) as usize).max(inner.forms.len());
            inner.forms.resize(max, ("", Kind::Ref(DEFAULT_FORM_ID)));
            inner.forms[id as usize] = (name, Kind::Ref(ref_id));
            FormId(id)
        } else if let Some((index, _)) = inner
            .forms
            .iter_mut()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
        {
            FormId(index as u16)
        } else {
            inner.forms.push((name, Kind::Ref(ref_id)));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Returns the [`FormId`] from a given `name`
    ///
    /// If the named form doesn't exist, create a temporary one.
    fn id_from_name(&self, name: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();

        let inner = self.inner.read();

        if let Some(id) = inner.forms.iter().position(|(cmp, _)| *cmp == name) {
            FormId(id as u16)
        } else {
            let mut weak_refs = self.weak_refs.write();
            let id = inner.forms.len() + weak_refs.len();
            weak_refs.push((name, id as u16));
            FormId(id as u16)
        }
    }

    /// Returns the [`FormId`] from a given `name`
    ///
    /// If the named form doesn't exist, create it.
    fn create_id_from_name(&self, name: impl AsRef<str>) -> FormId {
        let name = name.as_ref().to_string().leak();

        let mut inner = self.inner.write();

        if let Some(id) = inner.forms.iter().position(|(cmp, _)| *cmp == name) {
            FormId(id as u16)
        } else {
            inner.forms.push((name, Kind::Ref(FormId(0))));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Returns a form, given an index.
    fn form_from_id(&self, id: FormId) -> Form {
        let inner = self.inner.read();
        inner.form_of_id(id)
    }

    /// Returns the name of the [`FormId`]
    fn name_from_id(&self, id: FormId) -> &'static str {
        let inner = self.inner.read();
        let nth = inner.forms.get(id.0 as usize).map(|(name, _)| name);

        let Some(ret) = nth else {
            unreachable!("Form with id {} not found, this should never happen", id.0);
        };
        ret
    }

    /// The [`Form`] and [`CursorShape`] of the main cursor
    fn main_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_from_id(MAIN_CURSOR_FORM_ID);
        (form, self.inner.read().main_cursor)
    }

    /// The [`Form`] and [`CursorShape`] of extra cursors
    fn extra_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_from_id(EXTRA_CURSOR_FORM_ID);
        (form, self.inner.read().extra_cursor)
    }

    /// Sets the [`CursorShape`] of the main cursor
    fn set_main_cursor(&self, shape: CursorShape) {
        self.inner.write().main_cursor = Some(shape);
    }

    /// Sets the [`CursorShape`] of extra cursors
    fn set_extra_cursor(&self, shape: CursorShape) {
        self.inner.write().extra_cursor = Some(shape);
    }

    /// Unsets the [`CursorShape`] of the main cursor
    fn unset_main_cursor(&self) {
        self.inner.write().main_cursor = None;
    }

    /// Unsets the [`CursorShape`] of the extra cursors
    fn unset_extra_cursor(&self) {
        self.inner.write().extra_cursor = None;
    }

    /// Returns a [`Painter`]
    fn painter(&'static self) -> Painter {
        let inner = self.inner.read();
        let default_form = inner.form_of_id(DEFAULT_FORM_ID);
        Painter {
            palette: inner,
            forms: vec![(default_form, DEFAULT_FORM_ID)],
            cur_form: default_form,
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
    pub fn apply(&mut self, id: FormId) -> Form {
        let form = self.get_form(id);

        self.forms.push((form, id));
        self.cur_form = self.make_form();
        self.cur_form
    }

    /// Removes the [`Form`] with the given `id` and returns the
    /// result, given previous triggers.
    #[inline(always)]
    pub fn remove(&mut self, id: FormId) -> Form {
        let mut applied_forms = self.forms.iter().enumerate();
        if let Some((index, _)) = applied_forms.rfind(|(_, &(_, i))| i == id) {
            self.forms.remove(index);
            self.cur_form = self.make_form();
        }
        self.cur_form
    }

    #[inline(always)]
    pub fn reset(&mut self) -> Form {
        self.forms.splice(1.., []);
        self.cur_form = self.make_form();
        self.cur_form
    }

    /// Generates the form to be printed, given all the previously
    /// pushed forms in the `Form` stack.
    #[inline(always)]
    pub fn make_form(&self) -> Form {
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

        form
    }

    /// The [`Form`] "ExtraCursor", and its shape.
    pub fn main_cursor(&self) -> (Form, Option<CursorShape>) {
        (self.get_form(MAIN_CURSOR_FORM_ID), self.palette.main_cursor)
    }

    /// The [`Form`] "ExtraCursor", and its shape.
    pub fn extra_cursor(&self) -> (Form, Option<CursorShape>) {
        (
            self.get_form(EXTRA_CURSOR_FORM_ID),
            self.palette.extra_cursor,
        )
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
                None => break self.get_default(),
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
