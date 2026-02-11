//! A collection of colorschemes for Duat.
use std::sync::atomic::{AtomicBool, Ordering::Relaxed};

use duat_core::form::{Form, add_colorscheme};

macro_rules! ColorStruct {
    ({
        $(#[$attr:meta])*
        pub struct $Colors:ident;
        $Values:ident {
            $($color:ident: $value:literal,)+
        }
    }) => {
        static COLORS: std::sync::LazyLock<$Colors> = std::sync::LazyLock::new(|| {
            $Colors {
                $($color: Color::new($value)),+
            }
        });

        $(#[$attr])*
        pub fn colors() -> &'static $Colors {
            &COLORS
        }

        $(#[$attr])*
        #[non_exhaustive]
        pub struct $Colors {
            $(pub $color: $crate::form::Color),+
        }

        fn set_values(values: $Values) {
            let colors = &COLORS;
            $(
                colors.$color.set(values.$color);
            )+
        }

		#[derive(Clone, Copy)]
        pub struct $Values {
            $($color: &'static str,)+
        }

        impl $Values {
            pub const fn new() -> Self {
                Self {
                    $($color: $value,)+
                }
            }
        }
    }
}

static WITH_BACKGRUND: AtomicBool = AtomicBool::new(true);

/// Disables setting the background colors.
///
/// When you next change the colorscheme, the default background color
/// will not be applied.
pub fn with_background(with_background: bool) {
    WITH_BACKGRUND.store(with_background, Relaxed);
}

fn has_background() -> bool {
    WITH_BACKGRUND.load(Relaxed)
}

fn pairs_fn<Colors: Copy + Send + 'static>(
    value: Colors,
    pairs: fn(Colors) -> Vec<(String, Form)>,
) -> impl FnMut() -> Vec<(String, Form)> + Send + 'static {
    move || pairs(value)
}

pub(crate) fn add_colorschemes() {
    {
        use catppuccin::*;
        add_colorscheme("catppuccin-latte", pairs_fn(Values::new(), pairs));
        add_colorscheme("catppuccin-frappe", pairs_fn(FRAPPE, pairs));
        add_colorscheme("catppuccin-macchiato", pairs_fn(MACCHIATO, pairs));
        add_colorscheme("catppuccin-mocha", pairs_fn(MOCHA, pairs));
    }
}

mod catppuccin {
    use duat_core::form::{Color, Form};

    pub fn pairs(values: Values) -> Vec<(String, Form)> {
        set_values(values);
        let c = colors();

        let default = if super::has_background() {
            Form::new().with(c.text)
        } else {
            Form::new().with(c.text).on(c.base)
        };

        [
            ("default", default),
            ("accent", Form::new().with(c.rosewater).bold()),
            ("default.error", Form::new().with(c.maroon)),
            ("accent.error", Form::new().with(c.red).bold()),
            ("default.warn", Form::new().with(c.yellow)),
            ("accent.warn", Form::new().with(c.peach).bold()),
            ("default.info", Form::new().with(c.sapphire)),
            ("accent.info", Form::new().with(c.sky).bold()),
            ("default.debug", Form::new().with(c.subtext1)),
            ("accent.debug", Form::new().with(c.lavender).bold()),
            ("caret.main", Form::new().reverse()),
            ("caret.extra", Form::new().reverse()),
            ("selection.main", Form::new().with(c.base).on(c.overlay1)),
            ("selection.extra", Form::new().with(c.base).on(c.overlay0)),
            ("cloak", Form::new().with(c.overlay1).on(c.base)),
            ("character.control", Form::new().with(c.overlay1)),
            ("replace", Form::new().with(c.surface0)),
            (
                "replace.new_line.trailing",
                Form::new().with(c.red).on(c.surface1),
            ),
            // duat-base forms
            ("linenum.main", Form::new().with(c.yellow)),
            ("linenum.wrapped", Form::new().with(c.teal)),
            ("file", Form::new().with(c.yellow)),
            ("selections", Form::new().with(c.blue)),
            ("coord", Form::new().with(c.peach)),
            ("separator", Form::mimic("punctuation.delimiter")),
            ("mode", Form::new().with(c.green)),
            ("terminal.border", Form::new().with(c.surface0).on(c.base)),
            ("terminal.frame", Form::new().with(c.text).on(c.base)),
            ("notifs.colon", Form::new().with(c.subtext0)),
            ("prompt", Form::new().with(c.green)),
            ("prompt.colon", Form::new().with(c.subtext0)),
            ("default.StatusLine", default.on(c.surface0)),
            ("default.LogBook", default.on(c.surface0)),
            ("default.VertRule", default.with(c.surface0)),
            ("default.LineNumbers", default.with(c.overlay0)),
            (
                "matched_pair",
                Form::new().with(c.peach).on(c.surface1).bold(),
            ),
            ("log_book.location", Form::new().with(c.subtext1)),
            ("default.Completions", default.on(c.surface1)),
            (
                "selected.Completions",
                Form::new().with(c.base).on(c.overlay0),
            ),
            ("default.WhichKey", default.with(c.text)),
            ("key", Form::new().with(c.peach)),
            ("key.special", Form::new().with(c.teal)),
            // For duatmode
            ("caret.main.Normal", Form::new().with(c.base).on(c.text)),
            (
                "caret.extra.Normal",
                Form::new().with(c.base).on(c.sapphire),
            ),
            ("caret.main.Insert", Form::new().with(c.base).on(c.mauve)),
            ("caret.extra.Insert", Form::new().with(c.base).on(c.yellow)),
            ("param", Form::new().with(c.lavender)),
            ("param.flag", Form::new().with(c.pink)),
            // Tree sitter Forms
            ("variable", Form::new().with(c.text)),
            ("variable.builtin", Form::new().with(c.peach)),
            ("variable.member", Form::new().with(c.lavender)),
            ("constant", Form::new().with(c.peach)),
            ("constant.builtin", Form::new().with(c.peach)),
            ("module", Form::new().with(c.blue).italic()),
            ("label", Form::new().with(c.green)),
            ("string", Form::new().with(c.green)),
            ("string.escape", Form::new().with(c.peach)),
            ("string.special.path", Form::new().with(c.sky).underlined()),
            ("character", Form::new().with(c.peach)),
            ("boolean", Form::new().with(c.peach)),
            ("number", Form::new().with(c.peach)),
            ("type", Form::new().with(c.yellow).italic()),
            ("type.builtin", Form::new().with(c.yellow).reset()),
            ("attribute", Form::new().with(c.green)),
            ("property", Form::new().with(c.text)),
            ("function", Form::new().with(c.blue).reset()),
            ("function.macro", Form::new().with(c.lavender).italic()),
            ("constructor", Form::new().with(c.peach)),
            ("operator", Form::new().with(c.sapphire)),
            ("keyword", Form::new().with(c.mauve)),
            ("punctuation.bracket", Form::new().with(c.subtext0)),
            ("punctuation.delimiter", Form::new().with(c.subtext0)),
            ("comment", Form::new().with(c.overlay1)),
            ("comment.documentation", Form::new().with(c.overlay1).bold()),
            ("markup", Form::new()),
            ("markup.strong", Form::new().with(c.maroon).bold()),
            ("markup.italic", Form::new().with(c.maroon).italic()),
            ("markup.strikethrough", Form::new().crossed_out()),
            ("markup.underline", Form::new().underlined()),
            ("markup.heading", Form::new().with(c.blue).bold()),
            ("markup.math", Form::new().with(c.yellow)),
            ("markup.quote", Form::new().with(c.maroon).bold()),
            ("markup.environment", Form::new().with(c.pink)),
            ("markup.environment.name", Form::new().with(c.blue)),
            ("markup.link", Form::new().with(c.lavender).underlined()),
            ("markup.raw", Form::new().with(c.teal)),
            ("markup.list", Form::new().with(c.yellow)),
            ("markup.list.checked", Form::new().with(c.green)),
            ("markup.list.unchecked", Form::new().with(c.overlay1)),
            ("diff.plus", Form::new().with(c.green)),
            ("diff.delta", Form::new().with(c.blue)),
            ("diff.delta.renamed", Form::new().with(c.yellow)),
            ("diff.minus", Form::new().with(c.red)),
        ]
        .map(|(name, form)| (name.to_string(), form))
        .to_vec()
    }

    ColorStruct!({
        /// The [catppuccin] color palette
        ///
        /// [catppuccin]: https://github.com/catppuccin
        pub struct CatppuccinColors;
        // The default value is LATTE
        Values {
            rosewater: "#dc8a78",
            flamingo: "#dd7878",
            pink: "#ea76cb",
            mauve: "#8839ef",
            red: "#d20f39",
            maroon: "#e64553",
            peach: "#fe640b",
            yellow: "#df8e1d",
            green: "#40a02b",
            teal: "#179299",
            sky: "#04a5e5",
            sapphire: "#209fb5",
            blue: "#1e66f5",
            lavender: "#7287fd",
            text: "#4c4f69",
            subtext1: "#5c5f77",
            subtext0: "#6c6f85",
            overlay2: "#7c7f93",
            overlay1: "#8c8fa1",
            overlay0: "#9ca0b0",
            surface2: "#acb0be",
            surface1: "#bcc0cc",
            surface0: "#ccd0da",
            base: "#eff1f5",
            mantle: "#e6e9ef",
            crust: "#dce0e8",
        }
    });

    pub const FRAPPE: Values = Values {
        rosewater: "#f2d5cf",
        flamingo: "#eebebe",
        pink: "#f4b8e4",
        mauve: "#ca9ee6",
        red: "#e78284",
        maroon: "#ea999c",
        peach: "#ef9f76",
        yellow: "#e5c890",
        green: "#a6d189",
        teal: "#81c8be",
        sky: "#99d1db",
        sapphire: "#85c1dc",
        blue: "#8caaee",
        lavender: "#babbf1",
        text: "#c6d0f5",
        subtext1: "#b5bfe2",
        subtext0: "#a5adce",
        overlay2: "#949cbb",
        overlay1: "#838ba7",
        overlay0: "#737994",
        surface2: "#626880",
        surface1: "#51576d",
        surface0: "#414559",
        base: "#303446",
        mantle: "#292c3c",
        crust: "#232634",
    };

    pub const MACCHIATO: Values = Values {
        rosewater: "#f4dbd6",
        flamingo: "#f0c6c6",
        pink: "#f5bde6",
        mauve: "#c6a0f6",
        red: "#ed8796",
        maroon: "#ee99a0",
        peach: "#f5a97f",
        yellow: "#eed49f",
        green: "#a6da95",
        teal: "#8bd5ca",
        sky: "#91d7e3",
        sapphire: "#7dc4e4",
        blue: "#8aadf4",
        lavender: "#b7bdf8",
        text: "#cad3f5",
        subtext1: "#b8c0e0",
        subtext0: "#a5adcb",
        overlay2: "#939ab7",
        overlay1: "#8087a2",
        overlay0: "#6e738d",
        surface2: "#5b6078",
        surface1: "#494d64",
        surface0: "#363a4f",
        base: "#24273a",
        mantle: "#1e2030",
        crust: "#181926",
    };

    pub const MOCHA: Values = Values {
        rosewater: "#f5e0dc",
        flamingo: "#f2cdcd",
        pink: "#f5c2e7",
        mauve: "#cba6f7",
        red: "#f38ba8",
        maroon: "#eba0ac",
        peach: "#fab387",
        yellow: "#f9e2af",
        green: "#a6e3a1",
        teal: "#94e2d5",
        sky: "#89dceb",
        sapphire: "#74c7ec",
        blue: "#89b4fa",
        lavender: "#b4befe",
        text: "#cdd6f4",
        subtext1: "#bac2de",
        subtext0: "#a6adc8",
        overlay2: "#9399b2",
        overlay1: "#7f849c",
        overlay0: "#6c7086",
        surface2: "#585b70",
        surface1: "#45475a",
        surface0: "#313244",
        base: "#1e1e2e",
        mantle: "#181825",
        crust: "#11111b",
    };
}
