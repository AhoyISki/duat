use crossterm::{
    cursor::SetCursorStyle,
    style::{Attributes, Color, ContentStyle, Stylize},
};

#[derive(Default, Debug, Clone, Copy)]
/// A style for text.
pub struct Form {
    pub style: ContentStyle,
    /// Wether or not the `Form`s colors and attributes should override any that come after.
    pub is_final: bool,
}

impl Form {
    pub fn new(style: ContentStyle, is_final: bool) -> Self {
        Self { style, is_final }
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

#[derive(Default, Clone)]
pub struct ExtraForms(Vec<(String, Form)>);

pub const DEFAULT: u16 = 0;
pub const LINE_NUMBERS: u16 = 1;
pub const MAIN_LINE_NUMBER: u16 = 2;
pub const MAIN_SEL_ID: u16 = 3;
pub const EXTRA_SEL_ID: u16 = 4;

/// The list of forms to be used when rendering.
#[derive(Clone)]
pub struct FormPalette {
    main_cursor: CursorStyle,
    extra_cursor: CursorStyle,
    forms: Vec<(String, Form)>,
}

impl Default for FormPalette {
    fn default() -> Self {
        let main_cursor = CursorStyle::new(
            Some(SetCursorStyle::DefaultUserShape),
            Form::new(ContentStyle::new().reverse(), false),
        );
        let selection_form = Form::new(ContentStyle::new().on_dark_grey(), false);
        let forms = vec![
            (String::from("Default"), Form::default()),
            (String::from("LineNumbers"), Form::default()),
            (String::from("MainLineNumber"), Form::default()),
            (String::from("MainSelection"), selection_form),
            (String::from("SecondarySelection"), selection_form),
        ];

        Self { main_cursor, extra_cursor: main_cursor, forms }
    }
}

impl FormPalette {
    /// Adds a new named `Form` to the list of user added `Form`s.
    pub fn add_form<S>(&mut self, name: S, form: Form)
    where
        S: ToString,
    {
        let name = name.to_string();
        if let None = self.forms.iter().find(|(cmp, _)| *cmp == name) {
            self.forms.push((name.to_string(), form));
        } else {
            panic!("The form {} is already in use!", name);
        }
    }

    /// Sets the `Form` with a given name to a new one.
    pub fn set_form<S>(&mut self, name: S, form: Form)
    where
        S: ToString,
    {
        let name = name.to_string();

        let name_match = self.forms.iter_mut().find(|(cmp, _)| *cmp == name);
        if let Some((_, old_form)) = name_match {
            *old_form = form
        } else {
            panic!("The form of name \"{}\" wasn't added!", name);
        }
    }

    /// Returns the `Form` associated to a given name with the index for efficient access.
    pub fn get_from_name<S>(&self, name: S) -> (Form, u16)
    where
        S: ToString,
    {
        let name = name.to_string();

        let name_match = self.forms.iter().enumerate().find(|(_, (cmp, _))| *cmp == name);
        if let Some(tuple) = name_match.map(|(index, &(_, form))| (form, index as u16)) {
            return tuple;
        } else {
            panic!("The form of name \"{}\" wasn't added!", name);
        }
    }

    /// Returns a form, given an index.
    pub fn get(&self, index: u16) -> Form {
        self.forms.get(index as usize).map(|(_, form)| *form).expect("The id is not valid!")
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
        FormFormer { palette: self, forms: Vec::new() }
    }
}

pub(crate) struct FormFormer<'a> {
    palette: &'a FormPalette,
    forms: Vec<(Form, u16)>,
}

impl<'a> FormFormer<'a> {
    /// Applies the `Form` with the given `id` and returns the result, given previous triggers.
    pub(super) fn apply(&mut self, id: u16) -> Form {
        let form = self.palette.get(id);
        self.forms.push((form, id));
        self.make_form()
    }

    /// Generates the form to be printed, given all the previously pushed forms in the `Form`
    /// stack.
    pub fn make_form(&self) -> Form {
        let style = ContentStyle {
            foreground_color: Some(Color::Reset),
            background_color: Some(Color::Reset),
            underline_color: Some(Color::Reset),
            attributes: Attributes::default(),
        };

        let mut form = Form { style, is_final: false };

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

    /// Removes the `Form` with the given `id` and returns the result, given previous triggers.
    pub(super) fn remove(&mut self, id: u16) -> Form {
        let mut applied_forms = self.forms.iter().enumerate();
        if let Some((index, _)) = applied_forms.rfind(|(_, &(_, i))| i == id) {
            self.forms.remove(index);
            self.make_form()
        } else {
            panic!("The id {} has yet to be pushed.", id);
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
