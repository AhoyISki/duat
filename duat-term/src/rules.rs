//! A vertical ruler, usually to separate the [`Buffer`] from other
//! widgets
//!
//! This is just a simple widget, which should only really make sense
//! in a terminal context, since, in GUIs, you'd use styling of the
//! widget areas in order to accomplish the same thing.
//!
//! [`Buffer`]: duat_core::buffer::Buffer
use duat_core::{
    context::Handle,
    data::Pass,
    text::{Text, txt},
    ui::{PushSpecs, PushTarget, Side, Widget},
};

/// A vertical line on screen, useful, for example, for the separation
/// of a [`Buffer`] and [`LineNumbers`].
///
/// By default, this [`VertRule`] will show the `'│'` character on the
/// whole line, using the `"default.VertRule"` form. However, you can
/// change that with the [`VertRule::sep_char`] field.
///
/// This field, of type [`SepChar`], can have up to 3 distinct
/// characters, in order to differentiate between the main line's
/// separator and even the separators above and below the main line.
///
/// If the main character is not the same as the other two characters,
/// then the line will be printed with the `"rule.upper"` and
/// `"rule.lower"` forms for the characters above and below.
///
/// [`Buffer`]: duat_core::buffer::Buffer
/// [`LineNumbers`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
pub struct VertRule {
    handle: Option<Handle>,
    text: Text,
    pub sep_char: SepChar,
}

impl VertRule {
    /// Returns a [`VertRuleBuilder`], letting you push your own
    /// [`VertRule`]s around
    pub fn builder() -> VertRuleBuilder {
        VertRuleBuilder::default()
    }
}

impl Widget for VertRule {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let vr = handle.read(pa);
        
        let text = if let Some(handle) = vr.handle.as_ref()
            && let SepChar::ThreeWay(..) | SepChar::TwoWay(..) = vr.sep_char
        {
            let lines = handle.printed_line_numbers(pa);
            let (upper, middle, lower) = {
                let buffer = handle.read(pa);

                if let Some(main) = buffer.selections().get_main() {
                    let main = main.line();
                    let upper = lines.iter().filter(|&line| line.number < main).count();
                    let middle = lines.iter().filter(|&line| line.number == main).count();
                    let lower = lines.iter().filter(|&line| line.number > main).count();
                    (upper, middle, lower)
                } else {
                    (0, lines.len(), 0)
                }
            };

            let chars = vr.sep_char.chars();
            txt!(
                "[rule.upper]{}[]{}[rule.lower]{}",
                form_string(chars[0], upper),
                form_string(chars[1], middle),
                form_string(chars[2], lower)
            )
        } else {
            let full_line =
                format!("{}\n", vr.sep_char.chars()[1]).repeat(handle.area().height(pa) as usize);

            txt!("{full_line}")
        };

        handle.write(pa).text = text;
    }

    fn needs_update(&self, pa: &Pass) -> bool {
        matches!(self.sep_char, SepChar::ThreeWay(..) | SepChar::TwoWay(..))
            && self.handle.as_ref().is_some_and(|fh| fh.has_changed(pa))
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }
}

/// The configurations for the [`VertRule`] widget.
#[derive(Default, Clone)]
pub struct VertRuleBuilder {
    pub sep_char: SepChar,
    pub on_the_right: bool,
}

impl VertRuleBuilder {
    /// Pushes a [`VertRule`] to a [`PushTarget`]
    pub fn push_on(self, pa: &mut Pass, push_target: &impl PushTarget) -> Handle<VertRule> {
        let vert_rule = VertRule {
            handle: push_target.try_downcast(),
            text: Text::default(),
            sep_char: self.sep_char,
        };

        let specs = PushSpecs {
            side: if self.on_the_right {
                Side::Right
            } else {
                Side::Left
            },
            width: Some(1.0),
            ..Default::default()
        };

        push_target.push_outer(pa, vert_rule, specs)
    }
}

/// The [`char`]s that should be printed above, equal to, and below
/// the main line.
#[derive(Clone)]
pub enum SepChar {
    Uniform(char),
    /// Order: main line, other lines.
    TwoWay(char, char),
    /// Order: main line, above main line, below main line.
    ThreeWay(char, char, char),
}

impl SepChar {
    /// The [`char`]s above, equal to, and below the main line,
    /// respectively.
    fn chars(&self) -> [char; 3] {
        match self {
            SepChar::Uniform(uniform) => [*uniform, *uniform, *uniform],
            SepChar::TwoWay(main, other) => [*other, *main, *other],
            SepChar::ThreeWay(main, upper, lower) => [*upper, *main, *lower],
        }
    }
}

impl Default for SepChar {
    fn default() -> Self {
        Self::Uniform('│')
    }
}

fn form_string(char: char, count: usize) -> String {
    [char, '\n'].repeat(count).iter().collect()
}
