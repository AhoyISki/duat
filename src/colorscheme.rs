//! A collection of colorschemes for Duat.
use std::sync::atomic::{AtomicBool, Ordering::Relaxed};

pub use duat_core::form::{
    add_colorscheme as add, colorscheme_list as list, set_colorscheme as set,
};

/// Adds one or multiple colorschemes to Duat.
///
/// This is a very convenient macro, which handles all of the
/// inner details, and just lets you declaratively create a new
/// colorscheme.
///
/// Here's how it works:
///
/// - The first item will be an array of variants. Each variant will
///   be a tuple, where the first element will be the name of the
///   colorscheme, and the second element will be an array of tuples,
///   the first element being an identifier for the color, and the
///   second being a color string ([rgb or hsl]).
///
/// Here's an example:
///
/// ```rust
/// fn add_my_colorschemes() {
///     duat::add_colorschemes!(
///         [
///             ("mycolors-light", [
///                 (background, "#ffffff"),
///                 (foreground, "#000000"),
///                 (red, "#ff0000"),
///                 (blue, "#0000ff"),
///                 // ...
///             ]),
///             ("mycolors-dark", [
///                 (background, "#000000"),
///                 (foreground, "#ffffff"),
///                 (red, "#ff0000"),
///                 (blue, "#0000ff"),
///                 // ...
///             ]),
///         ],
///         // todo the rest
///         # |_, _| []
///     );
/// }
/// ```
///
/// Internally, this will create a `Colors` struct, which will have
/// all the fields defined in the list of colors. This struct will be
/// used for all variants, and will be convenient for setting the
/// colors, as we'll see in the second argument.
///
/// The second argument will be a function, which takes as the first
/// argument the `Colors` struct, and as the second argument, a `bool`
/// which tells you wether or not the default background should be
/// set.
///
/// This function should return an array of name-form tuples, which
///
/// ```rust
/// use duat::form::Form;
///
/// fn add_my_colorschemes() {
///     duat::add_colorschemes!(
///         [
///             ("mycolors-light", [
///                 (background, "#ffffff"),
///                 (foreground, "#000000"),
///                 (red, "#ff0000"),
///                 (blue, "#0000ff"),
///                 // ...
///             ]),
///             ("mycolors-dark", [
///                 (background, "#000000"),
///                 (foreground, "#ffffff"),
///                 (red, "#ff0000"),
///                 (blue, "#0000ff"),
///                 // ...
///             ]),
///         ],
///         |c, has_background| {
///             let default = if has_background {
///                 Form::new().with(c.foreground)
///             } else {
///                 Form::new().with(c.foreground).on(c.background)
///             };
///
///             [
///                 ("default", default),
///                 ("accent", Form::new().with(c.foreground).bold()),
///                 ("default.error", Form::new().with(c.red)),
///                 ("accent.error", Form::new().with(c.red).underlined().bold()),
///                 ("function", Form::new().with(c.blue)),
///                 // ...
///             ]
///         }
///     );
/// }
/// ```
///
/// This way, you can use just one function to set the colors of
/// multiple different variants, and the setup is fairly simple to do.
///
/// # Note
///
/// Keep in mind that [`Form`] inheritance by name is an automatic
/// process. For example, if you expect that all `keyword` forms
/// should look the same, you don't have to set `keyword.directive`,
/// `keyword.type`, `keyword.coroutine`, etc., to the same value, if
/// you just set `keyword` to a `Form`, without setting any of the
/// others, they will automatically reference it.
///
/// [rgb or hsl]: crate::form::Form::with
/// [`Form`]: crate::form::Form
#[macro_export]
macro_rules! add_colorschemes {
    (
        [
            $(
                ($variant:literal, [
                    $(($color_name:ident, $color_value:literal $(,)?)),* $(,)?
                ])
            ),+ $(,)?
        ],
        $pairs:expr $(,)?
    ) => {{
        use $crate::form::Form;

        $crate::add_colorschemes!(@Colors Colors, $($variant: { $($color_name),* }),+);

        const VARIANTS: &[&Colors] = &[
            $(&Colors {
                $($color_name: $color_value),*
            }),+
        ];

        fn pairs(variant: usize) -> Vec<(String, Form)> {
            type PairsFn<const N: usize> = fn(&Colors, bool) -> [(&'static str, Form); N];

            let pairs_fn: PairsFn<_> = $pairs;
            let pairs = pairs_fn(VARIANTS[variant], $crate::colorscheme::has_background());

            pairs.map(|(name, form)| (name.to_string(), form)).to_vec()
        }

        #[allow(unused_assignments)]
		let mut variant_num = 0;
		$(
            $crate::colorscheme::add($variant, move || pairs(variant_num));
            variant_num += 1;
		)+
    }};

    (@Colors $struct:ident, $_variant:literal: { $($color_name:ident),* } $($_rest:tt)*
    ) => {
        #[allow(dead_code)]
        #[derive(Clone, Copy)]
        struct $struct {
            $($color_name: &'static str),*
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

/// Wether background setting is
pub fn has_background() -> bool {
    WITH_BACKGRUND.load(Relaxed)
}

pub(crate) fn add_default() {
    add_colorschemes!(
        [
            ("catppuccin-latte", [
                (rosewater, "#dd7878"),
                (flamingo, "#ea76cb"),
                (pink, "#8839ef"),
                (mauve, "#d20f39"),
                (red, "#e64553"),
                (maroon, "#fe640b"),
                (peach, "#df8e1d"),
                (yellow, "#40a02b"),
                (green, "#179299"),
                (teal, "#04a5e5"),
                (sky, "#209fb5"),
                (sapphire, "#1e66f5"),
                (blue, "#7287fd"),
                (lavender, "#4c4f69"),
                (text, "#5c5f77"),
                (subtext1, "#6c6f85"),
                (subtext0, "#7c7f93"),
                (overlay2, "#8c8fa1"),
                (overlay1, "#9ca0b0"),
                (overlay0, "#acb0be"),
                (surface2, "#bcc0cc"),
                (surface1, "#ccd0da"),
                (surface0, "#eff1f5"),
                (base, "#e6e9ef"),
                (mantle, "#dce0e8"),
                (crust, "#dc8a78"),
            ]),
            ("catppuccin-frappe", [
                (rosewater, "#f2d5cf"),
                (flamingo, "#eebebe"),
                (pink, "#f4b8e4"),
                (mauve, "#ca9ee6"),
                (red, "#e78284"),
                (maroon, "#ea999c"),
                (peach, "#ef9f76"),
                (yellow, "#e5c890"),
                (green, "#a6d189"),
                (teal, "#81c8be"),
                (sky, "#99d1db"),
                (sapphire, "#85c1dc"),
                (blue, "#8caaee"),
                (lavender, "#babbf1"),
                (text, "#c6d0f5"),
                (subtext1, "#b5bfe2"),
                (subtext0, "#a5adce"),
                (overlay2, "#949cbb"),
                (overlay1, "#838ba7"),
                (overlay0, "#737994"),
                (surface2, "#626880"),
                (surface1, "#51576d"),
                (surface0, "#414559"),
                (base, "#303446"),
                (mantle, "#292c3c"),
                (crust, "#232634"),
            ]),
            ("catppuccin-macchiato", [
                (rosewater, "#f4dbd6"),
                (flamingo, "#f0c6c6"),
                (pink, "#f5bde6"),
                (mauve, "#c6a0f6"),
                (red, "#ed8796"),
                (maroon, "#ee99a0"),
                (peach, "#f5a97f"),
                (yellow, "#eed49f"),
                (green, "#a6da95"),
                (teal, "#8bd5ca"),
                (sky, "#91d7e3"),
                (sapphire, "#7dc4e4"),
                (blue, "#8aadf4"),
                (lavender, "#b7bdf8"),
                (text, "#cad3f5"),
                (subtext1, "#b8c0e0"),
                (subtext0, "#a5adcb"),
                (overlay2, "#939ab7"),
                (overlay1, "#8087a2"),
                (overlay0, "#6e738d"),
                (surface2, "#5b6078"),
                (surface1, "#494d64"),
                (surface0, "#363a4f"),
                (base, "#24273a"),
                (mantle, "#1e2030"),
                (crust, "#181926"),
            ]),
            ("catppuccin-mocha", [
                (rosewater, "#f5e0dc"),
                (flamingo, "#f2cdcd"),
                (pink, "#f5c2e7"),
                (mauve, "#cba6f7"),
                (red, "#f38ba8"),
                (maroon, "#eba0ac"),
                (peach, "#fab387"),
                (yellow, "#f9e2af"),
                (green, "#a6e3a1"),
                (teal, "#94e2d5"),
                (sky, "#89dceb"),
                (sapphire, "#74c7ec"),
                (blue, "#89b4fa"),
                (lavender, "#b4befe"),
                (text, "#cdd6f4"),
                (subtext1, "#bac2de"),
                (subtext0, "#a6adc8"),
                (overlay2, "#9399b2"),
                (overlay1, "#7f849c"),
                (overlay0, "#6c7086"),
                (surface2, "#585b70"),
                (surface1, "#45475a"),
                (surface0, "#313244"),
                (base, "#1e1e2e"),
                (mantle, "#181825"),
                (crust, "#11111b"),
            ])
        ],
        |c, has_background| {
            let default = if has_background {
                Form::new().with(c.text).on(c.base)
            } else {
                Form::new().with(c.text)
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
        }
    );

    add_colorschemes!(
        [
            ("tokyo-night", [
                (red, "#f7768e"),
                (red1, "#db4b4b"),
                (orange, "#ff9e64"),
                (yellow, "#e0af68"),
                (green, "#9ece6a"),
                (green1, "#73daca"),
                (teal, "#1abc9c"),
                (cyan, "#7dcfff"),
                (blue, "#7aa2f7"),
                (blue0, "#3d59a1"),
                (blue1, "#2ac3de"),
                (blue5, "#89ddff"),
                (magenta, "#bb9af7"),
                (purple, "#9d7cd8"),
                (text, "#c0caf5"),
                (fg_dark, "#a9b1d6"),
                (dark5, "#737aa2"),
                (comment, "#565f89"),
                (dark3, "#545c7e"),
                (terminal_black, "#414868"),
                (fg_gutter, "#3b4261"),
                (bg_highlight, "#292e42"),
                (bg, "#1a1b26"),
                (bg_dark, "#16161e"),
                (black, "#15161e"),
            ]),
            ("tokyo-night-storm", [
                (red, "#f7768e"),
                (red1, "#db4b4b"),
                (orange, "#ff9e64"),
                (yellow, "#e0af68"),
                (green, "#9ece6a"),
                (green1, "#73daca"),
                (teal, "#1abc9c"),
                (cyan, "#7dcfff"),
                (blue, "#7aa2f7"),
                (blue0, "#3d59a1"),
                (blue1, "#2ac3de"),
                (blue5, "#89ddff"),
                (magenta, "#bb9af7"),
                (purple, "#9d7cd8"),
                (text, "#c0caf5"),
                (fg_dark, "#a9b1d6"),
                (dark5, "#737aa2"),
                (comment, "#565f89"),
                (dark3, "#545c7e"),
                (terminal_black, "#414868"),
                (fg_gutter, "#3b4261"),
                (bg_highlight, "#292e42"),
                (bg, "#24283b"),
                (bg_dark, "#1f2335"),
                (black, "#1d202f"),
            ]),
        ],
        |c, has_background| {
            let default = if has_background {
                Form::new().with(c.text).on(c.bg)
            } else {
                Form::new().with(c.text)
            };

            [
                ("default", default),
                ("accent", Form::new().with(c.cyan).bold()),
                ("default.error", Form::new().with(c.red1)),
                ("accent.error", Form::new().with(c.red).bold()),
                ("default.warn", Form::new().with(c.yellow)),
                ("accent.warn", Form::new().with(c.orange).bold()),
                ("default.info", Form::new().with(c.blue1)),
                ("accent.info", Form::new().with(c.cyan).bold()),
                ("default.debug", Form::new().with(c.fg_dark)),
                ("accent.debug", Form::new().with(c.purple).bold()),
                ("caret.main", Form::new().reverse()),
                ("caret.extra", Form::new().reverse()),
                ("selection.main", Form::new().with(c.text).on(c.blue0)),
                ("selection.extra", Form::new().with(c.text).on(c.fg_gutter)),
                ("cloak", Form::new().with(c.dark5).on(c.bg)),
                ("character.control", Form::new().with(c.dark5)),
                ("replace", Form::new().with(c.bg_highlight)),
                (
                    "replace.new_line.trailing",
                    Form::new().with(c.red).on(c.terminal_black),
                ),
                // duat-base forms
                ("linenum.main", Form::new().with(c.yellow)),
                ("linenum.wrapped", Form::new().with(c.teal)),
                ("file", Form::new().with(c.yellow)),
                ("selections", Form::new().with(c.blue)),
                ("coord", Form::new().with(c.orange)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(c.green)),
                ("terminal.border", Form::new().with(c.bg_highlight).on(c.bg)),
                ("terminal.frame", Form::new().with(c.text).on(c.bg)),
                ("notifs.colon", Form::new().with(c.dark3)),
                ("prompt", Form::new().with(c.green)),
                ("prompt.colon", Form::new().with(c.dark3)),
                ("default.StatusLine", default.on(c.bg_highlight)),
                ("default.LogBook", default.on(c.bg_highlight)),
                ("default.VertRule", default.with(c.bg_highlight)),
                ("default.LineNumbers", default.with(c.dark3)),
                (
                    "matched_pair",
                    Form::new().with(c.orange).on(c.terminal_black).bold(),
                ),
                ("log_book.location", Form::new().with(c.fg_dark)),
                ("default.Completions", default.on(c.terminal_black)),
                (
                    "selected.Completions",
                    Form::new().with(c.bg).on(c.dark5),
                ),
                ("default.WhichKey", default.with(c.text)),
                ("key", Form::new().with(c.orange)),
                ("key.special", Form::new().with(c.teal)),
                // For duatmode
                ("caret.main.Normal", Form::new().with(c.bg).on(c.text)),
                (
                    "caret.extra.Normal",
                    Form::new().with(c.bg).on(c.blue1),
                ),
                ("caret.main.Insert", Form::new().with(c.bg).on(c.magenta)),
                ("caret.extra.Insert", Form::new().with(c.bg).on(c.yellow)),
                ("param", Form::new().with(c.purple)),
                ("param.flag", Form::new().with(c.cyan)),
                // Tree sitter Forms
                ("variable", Form::new().with(c.text)),
                ("variable.builtin", Form::new().with(c.orange)),
                ("variable.member", Form::new().with(c.green1)),
                ("constant", Form::new().with(c.orange)),
                ("constant.builtin", Form::new().with(c.orange)),
                ("module", Form::new().with(c.blue).italic()),
                ("label", Form::new().with(c.blue)),
                ("string", Form::new().with(c.green)),
                ("string.escape", Form::new().with(c.orange)),
                ("string.special.path", Form::new().with(c.cyan).underlined()),
                ("character", Form::new().with(c.orange)),
                ("boolean", Form::new().with(c.orange)),
                ("number", Form::new().with(c.orange)),
                ("type", Form::new().with(c.blue1).italic()),
                ("type.builtin", Form::new().with(c.blue1).reset()),
                ("attribute", Form::new().with(c.yellow)),
                ("property", Form::new().with(c.green1)),
                ("function", Form::new().with(c.blue).reset()),
                ("function.macro", Form::new().with(c.blue).italic()),
                ("constructor", Form::new().with(c.orange)),
                ("operator", Form::new().with(c.blue5)),
                ("keyword", Form::new().with(c.magenta)),
                ("punctuation.bracket", Form::new().with(c.dark5)),
                ("punctuation.delimiter", Form::new().with(c.blue5)),
                ("comment", Form::new().with(c.comment)),
                ("comment.documentation", Form::new().with(c.comment).bold()),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(c.red).bold()),
                ("markup.italic", Form::new().with(c.red).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(c.blue).bold()),
                ("markup.math", Form::new().with(c.yellow)),
                ("markup.quote", Form::new().with(c.green).bold()),
                ("markup.environment", Form::new().with(c.magenta)),
                ("markup.environment.name", Form::new().with(c.blue)),
                ("markup.link", Form::new().with(c.cyan).underlined()),
                ("markup.raw", Form::new().with(c.teal)),
                ("markup.list", Form::new().with(c.yellow)),
                ("markup.list.checked", Form::new().with(c.green)),
                ("markup.list.unchecked", Form::new().with(c.dark5)),
                ("diff.plus", Form::new().with(c.green)),
                ("diff.delta", Form::new().with(c.blue)),
                ("diff.delta.renamed", Form::new().with(c.yellow)),
                ("diff.minus", Form::new().with(c.red)),
            ]
        }
    );
}
