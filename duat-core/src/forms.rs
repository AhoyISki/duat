use std::sync::LazyLock;

pub use crossterm::cursor::SetCursorStyle as CursorShape;
use crossterm::style::{Attribute, Color, ContentStyle, Stylize};
use parking_lot::{RwLock, RwLockWriteGuard};

pub use self::global::*;
use crate::data::RwLockReadGuard;

mod global {
    use parking_lot::Mutex;

    use super::{CursorShape, Form, FormFmt, FormId, Kind, Painter, Palette};

    static PALETTE: Palette = Palette::new();
    static FORMS: Mutex<Vec<&str>> = Mutex::new(Vec::new());

    /// Sets the `Form` with a given name to a new one.
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

    /// Returns the `Form` associated to a given name with the index
    /// for efficient access.
    ///
    /// If a [`Form`] with the given name was not added prior, it will
    /// be added with the same form as the "Default" form.
    pub fn to_id(name: impl ToString) -> FormId {
        let name: &'static str = name.to_string().leak();

        crate::thread::queue(move || PALETTE.set_weak_form(name, Form::default()));

        let mut forms = FORMS.lock();
        if let Some(id) = forms.iter().position(|form| *form == name) {
            FormId(id as u16)
        } else {
            forms.push(name);
            FormId(forms.len() as u16 - 1)
        }
    }

    /// Returns a form, given an index.
    pub fn from_id(id: FormId) -> Form {
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
        crate::thread::queue(move || PALETTE.set_main_cursor(shape));
    }

    pub fn set_extra_cursor(shape: CursorShape) {
        crate::thread::queue(move || PALETTE.set_extra_cursor(shape));
    }

    pub fn unset_main_cursor() {
        crate::thread::queue(move || PALETTE.unset_main_cursor());
    }

    pub fn unset_extra_cursor() {
        crate::thread::queue(move || PALETTE.unset_extra_cursor());
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

pub const DEFAULT_ID: FormId = FormId(0);
pub const M_CUR_ID: FormId = FormId(8);
pub const E_CUR_ID: FormId = FormId(9);
pub const M_SEL_ID: FormId = FormId(10);
pub const E_SEL_ID: FormId = FormId(11);
pub const INACTIVE_ID: FormId = FormId(12);

#[derive(Debug, Clone, Copy)]
enum Kind {
    Form(Form),
    Ref(&'static str),
}

pub trait FormFmt: InnerFormFmt {}

trait InnerFormFmt {
    fn kind(self) -> Kind;
}

impl InnerFormFmt for Form {
    fn kind(self) -> Kind {
        Kind::Form(self)
    }
}

impl FormFmt for Form {}

impl InnerFormFmt for &str {
    fn kind(self) -> Kind {
        Kind::Ref(self.to_string().leak())
    }
}

impl FormFmt for &str {}

struct InnerPalette {
    main_cursor: Option<CursorShape>,
    extra_cursor: Option<CursorShape>,
    forms: Vec<(&'static str, Form, Option<FormId>)>,
}

/// The list of forms to be used when rendering.
struct Palette(LazyLock<RwLock<InnerPalette>>);

impl Palette {
    /// Returns a new instance of [`FormPalette`]
    const fn new() -> Self {
        Self(LazyLock::new(|| {
            let main_cursor = Some(CursorShape::DefaultUserShape);

            let forms = vec![
                ("Default", Form::new(), None),
                ("Accent", Form::new().bold(), None),
                ("DefaultOk", Form::blue(), None),
                ("AccentOk", Form::cyan(), None),
                ("DefaultErr", Form::red(), None),
                ("AccentErr", Form::red().bold(), None),
                ("DefaultHint", Form::grey(), None),
                ("AccentHint", Form::grey().bold(), None),
                ("MainCursor", Form::new().reverse(), None),
                ("ExtraCursor", Form::new().reverse(), Some(M_CUR_ID)),
                ("MainSelection", Form::new().on_dark_grey(), None),
                ("ExtraSelection", Form::new().on_dark_grey(), Some(M_SEL_ID)),
                ("Inactive", Form::grey(), None),
            ];

            RwLock::new(InnerPalette {
                main_cursor,
                extra_cursor: main_cursor,
                forms,
            })
        }))
    }

    /// Sets a [`Form`]
    fn set_form(&self, name: impl AsRef<str>, form: Form) {
        let name: &str = name.as_ref().to_string().leak();
        let form = match name {
            "MainCursor" | "ExtraCursor" => form.as_final(),
            _ => form,
        };

        let mut inner = self.0.write();

        let i = if let Some(i) = inner.forms.iter().position(|(cmp, ..)| *cmp == name) {
            inner.forms[i].1 = form;
            i
        } else {
            inner.forms.push((name, form, None));
            inner.forms.len() - 1
        };

        for refed in refs_of(&inner, i) {
            inner.forms[refed].1 = form;
        }
    }

    /// Sets a [`Form`] "weakly"
    fn set_weak_form(&self, name: impl AsRef<str>, form: Form) {
        let name: &str = name.as_ref().to_string().leak();
        let form = match name {
            "MainCursor" | "ExtraCursor" => form.as_final(),
            _ => form,
        };

        let mut inner = self.0.write();

        if !inner.forms.iter().any(|(cmp, ..)| *cmp == name) {
            inner.forms.push((name, form, None));
            for refed in refs_of(&inner, inner.forms.len() - 1) {
                inner.forms[refed].1 = form;
            }
        }
    }

    /// Sets a [`Form`] to reference another
    ///
    /// Returns `None` if the referenced form doesn't exist.
    fn set_ref(&self, name: impl AsRef<str>, refed: impl AsRef<str>) {
        let name = name.as_ref().to_string().leak();
        let refed = {
            let refed: &'static str = refed.as_ref().to_string().leak();
            self.id_from_name(refed)
        };

        let mut inner = self.0.write();
        let (_, form, _) = inner.forms[refed.0 as usize];

        if let Some(i) = inner.forms.iter().position(|(cmp, ..)| *cmp == name) {
            // If it would be circular, we just don't reference anything.
            if would_be_circular(&inner, i, refed.0 as usize) {
                inner.forms.push((name, form, None));
            } else {
                inner.forms.push((name, form, Some(refed)))
            }

            for refed in refs_of(&inner, i) {
                inner.forms[refed].1 = form;
            }
        } else {
            // If the form didn't previously exist, nothing was referencing it, so
            // no checks are done.
            inner.forms.push((name, form, Some(refed)));
        }
    }

    /// Makes a [`Form`] reference another "weakly"
    fn set_weak_ref(&self, name: impl AsRef<str>, refed: impl AsRef<str>) {
        let name = name.as_ref().to_string().leak();
        let refed = {
            let refed: &'static str = refed.as_ref().to_string().leak();
            self.id_from_name(refed)
        };

        let mut inner = self.0.write();
        let (_, form, _) = inner.forms[refed.0 as usize];

        // For weak refs, no checks are done, since a form is only set if it
        // doesn't exist, and for there to be refs to it, it must exist.
        if !inner.forms.iter().any(|(cmp, ..)| *cmp == name) {
            inner.forms.push((name, form, Some(refed)));
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
            inner.forms.push((name, Form::new(), None));
            FormId((inner.forms.len() - 1) as u16)
        }
    }

    /// Returns a form, given an index.
    fn form_from_id(&self, id: FormId) -> Form {
        let inner = self.0.read_recursive();
        inner.forms[id.0 as usize].1
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
        let form = self.form_from_id(M_CUR_ID);
        (form, self.0.read_recursive().main_cursor)
    }

    /// The [`Form`] and [`CursorShape`] of extra cursors
    fn extra_cursor(&self) -> (Form, Option<CursorShape>) {
        let form = self.form_from_id(E_CUR_ID);
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
        let cur_form = inner.forms[DEFAULT_ID.0 as usize].1;
        Painter {
            inner,
            forms: vec![(cur_form, DEFAULT_ID)],
            cur_form,
        }
    }
}

pub struct Painter {
    inner: RwLockReadGuard<'static, InnerPalette>,
    forms: Vec<(Form, FormId)>,
    cur_form: Form,
}

impl Painter {
    /// Applies the `Form` with the given `id` and returns the result,
    /// given previous triggers.
    #[inline(always)]
    pub fn apply(&mut self, id: FormId) -> Form {
        let (_, form, _) = unsafe {
            self.inner.forms.get_unchecked(id.0 as usize)
        };

        self.forms.push((*form, id));
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
        let (_, form, _) = unsafe {
            self.inner.forms.get_unchecked(M_CUR_ID.0 as usize)
        };
        (*form, self.inner.main_cursor)
    }

    /// The [`Form`] "ExtraCursor", and its shape.
    pub fn extra_cursor(&self) -> (Form, Option<CursorShape>) {
        let (_, form, _) = unsafe {
            self.inner.forms.get_unchecked(E_CUR_ID.0 as usize)
        };
        (*form, self.inner.extra_cursor)
    }

    pub fn get_default(&self) -> Form {
        self.forms[0].0
    }
}

fn refs_of(inner: &RwLockWriteGuard<InnerPalette>, n: usize) -> Vec<usize> {
    let mut refs = Vec::new();
    for (i, (.., refed)) in inner.forms.iter().enumerate() {
        if let Some(refed) = refed
            && refed.0 as usize == n
        {
            refs.push(i);
            refs.extend(refs_of(inner, i));
        }
    }
    refs
}

fn would_be_circular(inner: &RwLockWriteGuard<InnerPalette>, referee: usize, refed: usize) -> bool {
    if let (.., Some(refed_ref)) = inner.forms[refed] {
        match refed_ref.0 as usize == referee {
            true => true,
            false => would_be_circular(inner, referee, refed_ref.0 as usize),
        }
    } else {
        false
    }
}

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
