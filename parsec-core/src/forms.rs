use crossterm::{
    cursor::SetCursorStyle,
    style::{Attribute, Attributes, Color, ContentStyle, Stylize},
};

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
        Self { style: ContentStyle::default(), is_final: false }
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
    pub fn new(caret: Option<SetCursorStyle>, form: Form) -> Self {
        Self { caret, form }
    }
}

impl std::fmt::Debug for CursorStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.form, f)
    }
}

#[derive(Default, Clone)]
pub struct ExtraForms(Vec<(String, Form)>);

pub const DEFAULT: FormId = FormId(0);
pub const LINE_NUMBERS: FormId = FormId(1);
pub const MAIN_LINE_NUMBER: FormId = FormId(2);
pub const WRAPPED_LINE_NUMBERS: FormId = FormId(3);
pub const WRAPPED_MAIN_LINE_NUMBER: FormId = FormId(4);
pub const MAIN_SEL: FormId = FormId(5);
pub const EXTRA_SEL: FormId = FormId(6);
pub const FILE_NAME: FormId = FormId(7);
pub const SELECTIONS: FormId = FormId(8);
pub const COORDS: FormId = FormId(9);
pub const SEPARATOR: FormId = FormId(10);

#[derive(Clone, Copy)]
enum Named {
    Form(Form),
    Ref(&'static str),
}

impl Default for Named {
    fn default() -> Self {
        Named::Form(Form::default())
    }
}

/// The list of forms to be used when rendering.
#[derive(Clone)]
pub struct FormPalette {
    main_cursor: CursorStyle,
    extra_cursor: CursorStyle,
    forms: Vec<(String, Named)>,
}

impl Default for FormPalette {
    fn default() -> Self {
        Self::new()
    }
}

impl FormPalette {
    pub fn new() -> Self {
        let main_cursor = CursorStyle::new(
            Some(SetCursorStyle::DefaultUserShape),
            Form::new().reverse(),
        );
        let forms = vec![
            (String::from("Default"), Named::default()),
            (String::from("LineNumbers"), Named::default()),
            (String::from("MainLineNumber"), Named::Ref("LineNumbers")),
            (
                String::from("WrappedLineNumbers"),
                Named::Ref("LineNumbers"),
            ),
            (
                String::from("WrappedMainLineNumber"),
                Named::Ref("WrappedLineNumbers"),
            ),
            (
                String::from("MainSelection"),
                Named::Form(Form::new().on_dark_grey()),
            ),
            (String::from("ExtraSelection"), Named::Ref("MainSelection")),
            // Forms for a basic `StatusLine`.
            (
                String::from("FileName"),
                Named::Form(Form::new().dark_yellow().italic()),
            ),
            (
                String::from("Selections"),
                Named::Form(Form::new().dark_blue()),
            ),
            (String::from("Coords"), Named::Form(Form::new().yellow())),
            (String::from("Separator"), Named::Form(Form::new().cyan())),
        ];

        Self {
            main_cursor,
            extra_cursor: main_cursor,
            forms,
        }
    }

    /// Sets the `Form` with a given name to a new one.
    pub fn set_form(&mut self, name: impl ToString, form: Form) {
        let name = name.to_string();

        let name_match = self.forms.iter_mut().find(|(cmp, _)| *cmp == name);
        if let Some((_, old_form)) = name_match {
            *old_form = Named::Form(form)
        } else {
            self.forms.push((name.to_string(), Named::Form(form)));
        }
    }

    /// Returns the `Form` associated to a given name with the index
    /// for efficient access.
    pub fn from_name(&self, name: impl AsRef<str>) -> (Form, FormId) {
        let Some(ret) = self.get_from_name(&name) else {
            panic!("Form with name {} not found.", name.as_ref());
        };
        ret
    }

    /// Non-panicking version of
    /// [`from_name()`][FormPalette::from_name]
    pub fn get_from_name(&self, name: impl AsRef<str>) -> Option<(Form, FormId)> {
        let mut name = name.as_ref();

        while let Some((index, (_, named))) = self
            .forms
            .iter()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
        {
            match named {
                Named::Form(form) => return Some((*form, FormId(index))),
                Named::Ref(ref_name) => name = ref_name,
            }
        }
        None
    }

    /// Returns a form, given an index.
    pub fn from_id(&self, form_id: FormId) -> Form {
        let Some(ret) = self.get_from_id(form_id) else {
            panic!("Form with id {} not found", form_id.0);
        };
        ret
    }

    /// Non-panicking version of [`from_id()`][Self::from_id]
    pub fn get_from_id(&self, form_id: FormId) -> Option<Form> {
        let named = self.forms.get(form_id.0).map(|(_, named)| *named);
        named.map(|named| match named {
            Named::Form(form) => form,
            Named::Ref(name) => self.from_name(name).0,
        })
    }

    pub fn main_cursor(&self) -> &CursorStyle {
        &self.main_cursor
    }

    pub fn secondary_cursor(&self) -> &CursorStyle {
        &self.extra_cursor
    }

    pub fn set_main_cursor(&mut self, style: CursorStyle) {
        self.main_cursor = style;
    }

    pub fn set_secondary_cursor(&mut self, style: CursorStyle) {
        self.extra_cursor = style;
    }

    pub fn form_former(&self) -> FormFormer {
        FormFormer {
            palette: self,
            forms: Vec::new(),
        }
    }
}

pub struct FormFormer<'a> {
    palette: &'a FormPalette,
    forms: Vec<(Form, FormId)>,
}

impl<'a> FormFormer<'a> {
    /// Applies the `Form` with the given `id` and returns the result,
    /// given previous triggers.
    pub fn apply(&mut self, id: FormId) -> Form {
        let form = self.palette.from_id(id);
        self.forms.push((form, id));
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
