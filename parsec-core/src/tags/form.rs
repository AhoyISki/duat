use std::fmt::Debug;

use crossterm::{
    cursor::SetCursorStyle,
    style::{Attribute, Attributes, Color, ContentStyle, Stylize},
};

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

    pub fn new(is_final: bool) -> Self {
        Self { style: ContentStyle::default(), is_final }
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

impl Debug for CursorStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.form, f)
    }
}

#[derive(Default, Clone)]
pub struct ExtraForms(Vec<(String, Form)>);

pub const DEFAULT: u16 = 0;
pub const LINE_NUMBERS: u16 = 1;
pub const MAIN_LINE_NUMBER: u16 = 2;
pub const WRAPPED_LINE_NUMBERS: u16 = 3;
pub const WRAPPED_MAIN_LINE_NUMBER: u16 = 4;
pub const MAIN_SEL: u16 = 5;
pub const EXTRA_SEL: u16 = 6;
pub const FILE_NAME: u16 = 7;
pub const SELECTIONS: u16 = 8;
pub const COORDS: u16 = 9;
pub const SEPARATOR: u16 = 10;

/// The list of forms to be used when rendering.
#[derive(Debug, Clone)]
pub struct FormPalette {
    main_cursor: CursorStyle,
    extra_cursor: CursorStyle,
    forms: Vec<(String, Form)>,
}

impl Default for FormPalette {
    fn default() -> Self {
        let main_cursor =
            CursorStyle::new(Some(SetCursorStyle::DefaultUserShape), Form::new(false).reverse());
        let selection_form = Form::new(false).on_dark_grey();
        let forms = vec![
            (String::from("Default"), Form::default()),
            (String::from("LineNumbers"), Form::default()),
            (String::from("MainLineNumber"), Form::default()),
            (String::from("WrappedLineNumbers"), Form::default()),
            (String::from("WrappedMainLineNumber"), Form::default()),
            (String::from("MainSelection"), selection_form),
            (String::from("ExtraSelection"), selection_form),
            (String::from("FileName"), Form::new(false).dark_yellow().italic()),
            (String::from("Selections"), Form::new(false).dark_blue()),
            (String::from("Coords"), Form::new(false).yellow()),
            (String::from("Separator"), Form::new(false).cyan()),
        ];

        Self {
            main_cursor,
            extra_cursor: main_cursor,
            forms,
        }
    }
}

impl FormPalette {
    /// Adds a new named `Form` to the list of user added `Form`s.
    pub fn add_form(&mut self, name: impl ToString, form: Form) {
        let name = name.to_string();
        if let None = self.forms.iter().find(|(cmp, _)| *cmp == name) {
            self.forms.push((name.to_string(), form));
        } else {
            panic!("The form {} is already in use!", name);
        }
    }

    /// Sets the `Form` with a given name to a new one.
    pub fn set_form(&mut self, name: impl ToString, form: Form) {
        let name = name.to_string();

        let name_match = self.forms.iter_mut().find(|(cmp, _)| *cmp == name);
        if let Some((_, old_form)) = name_match {
            *old_form = form
        } else {
            panic!("The form of name \"{}\" wasn't added!", name);
        }
    }

    /// Returns the `Form` associated to a given name with the index
    /// for efficient access.
    pub fn from_name(&self, name: impl AsRef<str>) -> (Form, u16) {
        let name = name.as_ref();

        let name_match = self.forms.iter().enumerate().find(|(_, (cmp, _))| *cmp == name);
        assert!(name_match.is_some(), "Form with name {} not found", name);
        name_match.map(|(index, &(_, form))| (form, index as u16)).unwrap()
    }

    /// Non-panicking version of
    /// [`from_name()`][FormPalette::from_name]
    pub fn get_from_name(&self, name: impl AsRef<str>) -> Option<(Form, u16)> {
        let name = name.as_ref();

        self.forms
            .iter()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
            .map(|(index, &(_, form))| (form, index as u16))
    }

    /// Returns a form, given an index.
    pub fn from_id(&self, form_id: u16) -> Form {
        let form = self.forms.get(form_id as usize).map(|(_, form)| *form);
        assert!(form.is_some(), "Form with id {} not found", form_id);
        form.unwrap()
    }

    /// Non-panicking version of [`from_id()`][Self::from_id]
    pub fn get_from_id(&self, form_id: u16) -> Option<Form> {
        self.forms.get(form_id as usize).map(|(_, form)| *form)
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

    pub(crate) fn form_former(&self) -> FormFormer {
        FormFormer {
            palette: self,
            forms: Vec::new(),
        }
    }
}

pub(crate) struct FormFormer<'a> {
    palette: &'a FormPalette,
    forms: Vec<(Form, u16)>,
}

impl<'a> FormFormer<'a> {
    /// Applies the `Form` with the given `id` and returns the result,
    /// given previous triggers.
    pub(super) fn apply(&mut self, id: u16) -> Form {
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
            set_var(&mut fg_done, &mut form.style.foreground_color, &new_foreground, is_final);

            let new_background = style.background_color;
            set_var(&mut bg_done, &mut form.style.background_color, &new_background, is_final);

            let new_underline = style.underline_color;
            set_var(&mut ul_done, &mut form.style.underline_color, &new_underline, is_final);

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

    /// Removes the `Form` with the given `id` and returns the result,
    /// given previous triggers.
    pub(super) fn remove(&mut self, id: u16) -> Form {
        let mut applied_forms = self.forms.iter().enumerate();
        if let Some((index, _)) = applied_forms.rfind(|(_, &(_, i))| i == id) {
            self.forms.remove(index);
            self.make_form()
        } else {
            //panic!("The id {} has yet to be pushed.", id);
            Form::default()
        }
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
