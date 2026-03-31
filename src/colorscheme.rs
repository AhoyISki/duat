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
                variant_name: $variant,
                $($color_name: $color_value),*
            }),+
        ];

        fn pairs(variant: usize) -> Vec<(String, Form)> {
            type PairsFn<const N: usize> = fn(&Colors, bool) -> [(&'static str, Form); N];

            let pairs_fn: PairsFn<_> = $pairs;
            let pairs = pairs_fn(VARIANTS[variant], $crate::colorscheme::has_background());

            pairs.map(|(name, form)| (name.to_string(), form)).to_vec()
        }

        #[allow(unused_variables)]
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
            variant_name: &'static str,
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

#[allow(unused_assignments)]
pub(crate) fn add_default() {
    add_colorschemes!(
        [
            ("catppuccin-latte", [
                (rosewater, "#dc8a78"),
                (flamingo, "#dd7878"),
                (pink, "#ea76cb"),
                (mauve, "#8839ef"),
                (red, "#d20f39"),
                (maroon, "#e64553"),
                (peach, "#fe640b"),
                (yellow, "#df8e1d"),
                (green, "#40a02b"),
                (teal, "#179299"),
                (sky, "#04a5e5"),
                (sapphire, "#209fb5"),
                (blue, "#1e66f5"),
                (lavender, "#7287fd"),
                (text, "#4c4f69"),
                (subtext1, "#5c5f77"),
                (subtext0, "#6c6f85"),
                (overlay2, "#7c7f93"),
                (overlay1, "#8c8fa1"),
                (overlay0, "#9ca0b0"),
                (surface2, "#acb0be"),
                (surface1, "#bcc0cc"),
                (surface0, "#ccd0da"),
                (base, "#eff1f5"),
                (mantle, "#e6e9ef"),
                (crust, "#dce0e8"),
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
                // The default form, self explanatory.
                ("default", default),
                // A form to "accent" text, highlighting things.
                ("accent", Form::new().with(c.rosewater).bold()),
                // Variations of the above two for different scenarios.
                ("default.error", Form::new().with(c.maroon)),
                ("accent.error", Form::new().with(c.red).bold()),
                ("default.warning", Form::new().with(c.yellow)),
                ("accent.warning", Form::new().with(c.peach).bold()),
                ("default.info", Form::new().with(c.sapphire)),
                ("accent.info", Form::new().with(c.sky).bold()),
                ("default.debug", Form::new().with(c.subtext1)),
                ("accent.debug", Form::new().with(c.lavender).bold()),
                // In duat, the caret is the blinking bit.
                ("caret.main", Form::new().with(c.base).on(c.rosewater)),
                ("caret.extra", Form::new().with(c.base).on(c.teal)),
                // And the selection is the rest.
                ("selection.main", Form::new().on(c.surface1)),
                ("selection.extra", Form::new().on(c.surface0)),
                // This is to hide selected indent guides.
                ("selection.main.indent", Form::new().with_on(c.surface1)),
                ("selection.extra.indent", Form::new().with_on(c.surface0)),
                // A utility form, mostly used to cover all other forms on screen.
                ("cloak", Form::new().with(c.overlay1).on(c.base)),
                // Form for replacement characters (like indent guides).
                ("replace", Form::new().with(c.surface0)),
                // Same, but only for trailing newlines.
                (
                    "replace.newline.trailing",
                    Form::new().with(c.red).on(c.surface1),
                ),
                // Forms for hovered and clicked Toggles.
                ("toggle.hover", Form::new().on(c.surface0)),
                ("toggle.click", Form::new().on(c.surface1)),
                // Forms for the line numbers.
                ("linenum.main", Form::new().with(c.yellow)),
                ("linenum.wrapped", Form::new().with(c.teal)),
                // Various forms for the StatusLine parts.
                ("file", Form::new().with(c.yellow)),
                ("selections", Form::new().with(c.blue)),
                ("coord", Form::new().with(c.peach)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(c.green)),
                // Borders separate Buffers.
                ("terminal.border", Form::new().with(c.surface0).on(c.base)),
                // Frames surround spawned widgets.
                ("terminal.frame", Form::new().with(c.text).on(c.base)),
                ("notifs.colon", Form::new().with(c.subtext0)),
                // The prompt in a promptline (like `reverse search`).
                ("prompt", Form::new().with(c.green)),
                // The colon that separates the prompt from the input.
                ("prompt.colon", Form::new().with(c.subtext0)),
                // Various default forms for specific Widgets.
                (
                    "default.Buffer.current_line",
                    default.on(c.surface0).interpolate(default, 50),
                ),
                ("default.StatusLine", default.on(c.mantle)),
                ("default.LogBook", default.on(c.mantle)),
                ("default.VertRule", default.with(c.surface0)),
                ("default.LineNumbers", default.with(c.overlay0)),
                ("default.Completions", default.on(c.surface1)),
                // Used when the caret is over parentheses pairs.
                (
                    "matched_pair",
                    Form::new().with(c.peach).on(c.surface1).bold(),
                ),
                // Form used on the location a log message came from.
                ("logbook.location", Form::new().with(c.subtext1)),
                (
                    "selected.Completions",
                    Form::new().with(c.base).on(c.overlay0),
                ),
                // Used in the `mapped_txt` StatusLine part.
                ("key", Form::new().with(c.peach)),
                ("key.special", Form::new().with(c.teal)),
                // Same as before, but on specific Modes.
                ("caret.main.Normal", Form::new().with(c.base).on(c.text)),
                (
                    "caret.extra.Normal",
                    Form::new().with(c.base).on(c.sapphire),
                ),
                ("caret.main.Insert", Form::new().with(c.base).on(c.mauve)),
                ("caret.extra.Insert", Form::new().with(c.base).on(c.yellow)),
                // Used when typing arguments to commands.
                ("param", Form::new().with(c.lavender)),
                ("param.flag", Form::new().with(c.pink)),
                // Tree sitter/LSP Forms
                ("variable", Form::new().with(c.text)),
                ("variable.builtin", Form::new().with(c.peach)),
                ("variable.member", Form::new().with(c.lavender)),
                ("constant", Form::new().with(c.peach).reset()),
                ("constant.builtin", Form::new().with(c.peach).reset()),
                ("static", Form::new().with(c.peach).reset()),
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
                ("type.enum", Form::new().with(c.yellow).reset()),
                ("type.enum.variant", Form::new().with(c.peach).italic()),
                ("interface", Form::new().with(c.text).bold().reset()),
                ("attribute", Form::new().with(c.green)),
                ("property", Form::new().with(c.lavender)),
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
                ("unresolved", Form::new().underlined().underline(c.red)),
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
                ("default.warning", Form::new().with(c.yellow)),
                ("accent.warning", Form::new().with(c.orange).bold()),
                ("default.info", Form::new().with(c.blue1)),
                ("accent.info", Form::new().with(c.cyan).bold()),
                ("default.debug", Form::new().with(c.fg_dark)),
                ("accent.debug", Form::new().with(c.purple).bold()),
                ("caret.main", Form::new().with(c.bg).on(c.text)),
                ("caret.extra", Form::new().with(c.bg).on(c.teal)),
                ("selection.main", Form::new().on(c.blue0)),
                ("selection.extra", Form::new().on(c.fg_gutter)),
                ("selection.main.indent", Form::new().with_on(c.blue0)),
                ("selection.extra.indent", Form::new().with_on(c.fg_gutter)),
                ("cloak", Form::new().with(c.dark5).on(c.bg)),
                ("replace", Form::new().with(c.bg_highlight)),
                (
                    "replace.newline.trailing",
                    Form::new().with(c.red).on(c.terminal_black),
                ),
                ("toggle.hover", Form::new().on(c.bg_highlight)),
                ("toggle.click", Form::new().on(c.terminal_black)),
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
                ("default.StatusLine", default.on(c.bg_dark)),
                ("default.LogBook", default.on(c.bg_dark)),
                ("default.VertRule", default.with(c.bg_dark)),
                ("default.LineNumbers", default.with(c.dark3)),
                (
                    "matched_pair",
                    Form::new().with(c.orange).on(c.terminal_black).bold(),
                ),
                ("logbook.location", Form::new().with(c.fg_dark)),
                (
                    "default.Buffer.current_line",
                    default.on(c.bg_highlight).interpolate(default, 50),
                ),
                ("default.Completions", default.on(c.terminal_black)),
                ("selected.Completions", Form::new().with(c.bg).on(c.dark5)),
                ("default.WhichKey", default.with(c.text)),
                ("key", Form::new().with(c.orange)),
                ("key.special", Form::new().with(c.teal)),
                // For duatmode
                ("caret.main.Normal", Form::new().with(c.bg).on(c.text)),
                ("caret.extra.Normal", Form::new().with(c.bg).on(c.blue1)),
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
                ("static", Form::new().with(c.orange).reset()),
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
                ("type.enum", Form::new().with(c.blue1).reset()),
                ("type.enum.variant", Form::new().with(c.orange).italic()),
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
                ("unresolved", Form::new().underlined().underline(c.red)),
            ]
        }
    );

    add_colorschemes!(
        [
            ("github-dark", [
                (bg, "#0d1117"),
                (bg_overlay, "#161b22"),
                (bg_subtle, "#21262d"),
                (text, "#e6edf3"),
                (text_muted, "#8b949e"),
                (text_subtle, "#6e7681"),
                (red, "#f85149"),
                (orange, "#ffa657"),
                (yellow, "#e3b341"),
                (green, "#3fb950"),
                (teal, "#39c5cf"),
                (blue, "#58a6ff"),
                (purple, "#d2a8ff"),
                (string_color, "#a5d6ff"),
                (number_color, "#79c0ff"),
                (keyword_color, "#ff7b72"),
            ]),
            ("github-light", [
                (bg, "#ffffff"),
                (bg_overlay, "#f6f8fa"),
                (bg_subtle, "#eaeef2"),
                (text, "#24292f"),
                (text_muted, "#57606a"),
                (text_subtle, "#6e7781"),
                (red, "#cf222e"),
                (orange, "#953800"),
                (yellow, "#9a6700"),
                (green, "#1a7f37"),
                (teal, "#0969da"),
                (blue, "#0550ae"),
                (purple, "#8250df"),
                (string_color, "#0a3069"),
                (number_color, "#0550ae"),
                (keyword_color, "#cf222e"),
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
                ("accent", Form::new().with(c.blue).bold()),
                ("default.error", Form::new().with(c.red)),
                ("accent.error", Form::new().with(c.red).bold()),
                ("default.warning", Form::new().with(c.yellow)),
                ("accent.warning", Form::new().with(c.orange).bold()),
                ("default.info", Form::new().with(c.teal)),
                ("accent.info", Form::new().with(c.teal).bold()),
                ("default.debug", Form::new().with(c.text_muted)),
                ("accent.debug", Form::new().with(c.purple).bold()),
                ("caret.main", Form::new().with(c.bg).on(c.text)),
                ("caret.extra", Form::new().with(c.bg).on(c.text_muted)),
                ("selection.main", Form::new().on(c.bg_subtle)),
                ("selection.extra", Form::new().on(c.bg_overlay)),
                ("selection.main.indent", Form::new().with_on(c.bg_subtle)),
                ("selection.extra.indent", Form::new().with_on(c.bg_overlay)),
                ("cloak", Form::new().with(c.text_subtle).on(c.bg)),
                ("replace", Form::new().with(c.bg_subtle)),
                (
                    "replace.newline.trailing",
                    Form::new().with(c.red).on(c.bg_subtle),
                ),
                ("toggle.hover", Form::new().on(c.bg_overlay)),
                ("toggle.click", Form::new().on(c.bg_subtle)),
                // duat-base forms
                ("linenum.main", Form::new().with(c.yellow)),
                ("linenum.wrapped", Form::new().with(c.teal)),
                ("file", Form::new().with(c.yellow)),
                ("selections", Form::new().with(c.blue)),
                ("coord", Form::new().with(c.orange)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(c.green)),
                ("terminal.border", Form::new().with(c.bg_subtle).on(c.bg)),
                ("terminal.frame", Form::new().with(c.text).on(c.bg)),
                ("notifs.colon", Form::new().with(c.text_muted)),
                ("prompt", Form::new().with(c.green)),
                ("prompt.colon", Form::new().with(c.text_muted)),
                ("default.StatusLine", default.on(c.bg_subtle)),
                ("default.LogBook", default.on(c.bg_subtle)),
                ("default.VertRule", default.with(c.bg_subtle)),
                ("default.LineNumbers", default.with(c.text_subtle)),
                (
                    "matched_pair",
                    Form::new().with(c.orange).on(c.bg_subtle).bold(),
                ),
                ("logbook.location", Form::new().with(c.text_muted)),
                (
                    "default.Buffer.current_line",
                    default.on(c.bg_subtle).interpolate(default, 50),
                ),
                ("default.Completions", default.on(c.bg_overlay)),
                ("selected.Completions", Form::new().with(c.bg).on(c.blue)),
                ("default.WhichKey", default.with(c.text)),
                ("key", Form::new().with(c.orange)),
                ("key.special", Form::new().with(c.teal)),
                // For duatmode
                ("caret.main.Normal", Form::new().with(c.bg).on(c.text)),
                ("caret.extra.Normal", Form::new().with(c.bg).on(c.teal)),
                ("caret.main.Insert", Form::new().with(c.bg).on(c.purple)),
                ("caret.extra.Insert", Form::new().with(c.bg).on(c.yellow)),
                ("param", Form::new().with(c.purple)),
                ("param.flag", Form::new().with(c.teal)),
                // Tree sitter Forms
                ("variable", Form::new().with(c.text)),
                ("variable.builtin", Form::new().with(c.orange)),
                ("variable.member", Form::new().with(c.blue)),
                ("constant", Form::new().with(c.number_color)),
                ("constant.builtin", Form::new().with(c.number_color)),
                ("static", Form::new().with(c.number_color).reset()),
                ("module", Form::new().with(c.blue).italic()),
                ("label", Form::new().with(c.green)),
                ("string", Form::new().with(c.string_color)),
                ("string.escape", Form::new().with(c.orange)),
                ("string.special.path", Form::new().with(c.teal).underlined()),
                ("character", Form::new().with(c.number_color)),
                ("boolean", Form::new().with(c.number_color)),
                ("number", Form::new().with(c.number_color)),
                ("type", Form::new().with(c.teal).italic()),
                ("type.builtin", Form::new().with(c.teal).reset()),
                ("type.enum", Form::new().with(c.teal).reset()),
                ("type.enum.variant", Form::new().with(c.orange).italic()),
                ("attribute", Form::new().with(c.yellow)),
                ("property", Form::new().with(c.text_muted)),
                ("function", Form::new().with(c.purple).reset()),
                ("function.macro", Form::new().with(c.purple).italic()),
                ("constructor", Form::new().with(c.orange)),
                ("operator", Form::new().with(c.keyword_color)),
                ("keyword", Form::new().with(c.keyword_color)),
                ("punctuation.bracket", Form::new().with(c.text_subtle)),
                ("punctuation.delimiter", Form::new().with(c.text_subtle)),
                ("comment", Form::new().with(c.text_muted)),
                (
                    "comment.documentation",
                    Form::new().with(c.text_muted).bold(),
                ),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(c.red).bold()),
                ("markup.italic", Form::new().with(c.red).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(c.blue).bold()),
                ("markup.math", Form::new().with(c.yellow)),
                ("markup.quote", Form::new().with(c.green).bold()),
                ("markup.environment", Form::new().with(c.purple)),
                ("markup.environment.name", Form::new().with(c.blue)),
                ("markup.link", Form::new().with(c.teal).underlined()),
                ("markup.raw", Form::new().with(c.string_color)),
                ("markup.list", Form::new().with(c.yellow)),
                ("markup.list.checked", Form::new().with(c.green)),
                ("markup.list.unchecked", Form::new().with(c.text_subtle)),
                ("diff.plus", Form::new().with(c.green)),
                ("diff.delta", Form::new().with(c.blue)),
                ("diff.delta.renamed", Form::new().with(c.yellow)),
                ("diff.minus", Form::new().with(c.red)),
                ("unresolved", Form::new().underlined().underline(c.red)),
            ]
        }
    );

    // Night Owl theme — https://github.com/sdras/night-owl-vscode-theme
    add_colorschemes!(
        [("night-owl", [
            (bg, "#011627"),
            (surface, "#0b253a"),
            (surface_highlight, "#1d3b53"),
            (surface_panel, "#202431"),
            (surface_inactive, "#01111d"),
            (text, "#d6deeb"),
            (text_muted, "#5f7e97"),
            (text_subtle, "#4b6479"),
            (comment, "#637777"),
            (red, "#EF5350"),
            (orange, "#F78C6C"),
            (yellow, "#ecc48d"),
            (sand, "#ffcb8b"),
            (green, "#c5e478"),
            (teal, "#7fdbca"),
            (aqua, "#baebe2"),
            (blue, "#82AAFF"),
            (purple, "#c792ea"),
            (gold, "#faf39f"),
        ]),],
        |c, has_background| {
            let default = if has_background {
                Form::new().with(c.text).on(c.bg)
            } else {
                Form::new().with(c.text)
            };

            [
                ("default", default),
                ("accent", Form::new().with(c.teal).bold()),
                ("default.error", Form::new().with(c.red)),
                ("accent.error", Form::new().with(c.red).bold()),
                ("default.warning", Form::new().with(c.yellow)),
                ("accent.warning", Form::new().with(c.orange).bold()),
                ("default.info", Form::new().with(c.blue)),
                ("accent.info", Form::new().with(c.teal).bold()),
                ("default.debug", Form::new().with(c.text_muted)),
                ("accent.debug", Form::new().with(c.purple).bold()),
                ("caret.main", Form::new().with(c.bg).on(c.text)),
                ("caret.extra", Form::new().with(c.bg).on(c.teal)),
                ("selection.main", Form::new().on(c.surface_highlight)),
                ("selection.extra", Form::new().on(c.surface)),
                (
                    "selection.main.indent",
                    Form::new().with_on(c.surface_highlight),
                ),
                ("selection.extra.indent", Form::new().with_on(c.surface)),
                ("cloak", Form::new().with(c.text_subtle).on(c.bg)),
                ("replace", Form::new().with(c.surface)),
                (
                    "replace.newline.trailing",
                    Form::new().with(c.red).on(c.surface_highlight),
                ),
                ("toggle.hover", Form::new().on(c.surface_inactive)),
                ("toggle.click", Form::new().on(c.surface_highlight)),
                ("linenum.main", Form::new().with(c.yellow)),
                ("linenum.wrapped", Form::new().with(c.teal)),
                ("file", Form::new().with(c.yellow)),
                ("selections", Form::new().with(c.blue)),
                ("coord", Form::new().with(c.orange)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(c.green)),
                ("terminal.border", Form::new().with(c.surface).on(c.bg)),
                ("terminal.frame", Form::new().with(c.text).on(c.bg)),
                ("notifs.colon", Form::new().with(c.text_muted)),
                ("prompt", Form::new().with(c.green)),
                ("prompt.colon", Form::new().with(c.text_muted)),
                ("default.StatusLine", default.on(c.surface_panel)),
                ("default.LogBook", default.on(c.surface_panel)),
                ("default.VertRule", default.with(c.surface)),
                ("default.LineNumbers", default.with(c.text_subtle)),
                (
                    "matched_pair",
                    Form::new().with(c.orange).on(c.surface_highlight).bold(),
                ),
                ("logbook.location", Form::new().with(c.text_muted)),
                (
                    "default.Buffer.current_line",
                    default.on(c.surface_highlight).interpolate(default, 50),
                ),
                ("default.Completions", default.on(c.surface_panel)),
                (
                    "selected.Completions",
                    Form::new().with(c.text).on(c.surface_highlight),
                ),
                ("default.WhichKey", default.with(c.text)),
                ("key", Form::new().with(c.orange)),
                ("key.special", Form::new().with(c.teal)),
                ("caret.main.Normal", Form::new().with(c.bg).on(c.text)),
                ("caret.extra.Normal", Form::new().with(c.bg).on(c.teal)),
                ("caret.main.Insert", Form::new().with(c.bg).on(c.purple)),
                ("caret.extra.Insert", Form::new().with(c.bg).on(c.yellow)),
                ("param", Form::new().with(c.purple)),
                ("param.flag", Form::new().with(c.teal)),
                ("variable", Form::new().with(c.text)),
                ("variable.builtin", Form::new().with(c.teal)),
                ("variable.member", Form::new().with(c.aqua)),
                ("constant", Form::new().with(c.blue)),
                ("constant.builtin", Form::new().with(c.blue)),
                ("static", Form::new().with(c.blue).reset()),
                ("module", Form::new().with(c.blue).italic()),
                ("label", Form::new().with(c.teal)),
                ("string", Form::new().with(c.yellow)),
                ("string.escape", Form::new().with(c.orange)),
                ("string.special.path", Form::new().with(c.blue).underlined()),
                ("character", Form::new().with(c.orange)),
                ("boolean", Form::new().with(c.blue)),
                ("number", Form::new().with(c.orange)),
                ("type", Form::new().with(c.green).italic()),
                ("type.builtin", Form::new().with(c.green).reset()),
                ("type.enum", Form::new().with(c.green).reset()),
                ("type.enum.variant", Form::new().with(c.orange).italic()),
                ("attribute", Form::new().with(c.green).italic()),
                ("property", Form::new().with(c.gold).italic()),
                ("function", Form::new().with(c.blue).reset()),
                ("function.macro", Form::new().with(c.purple).italic()),
                ("constructor", Form::new().with(c.sand)),
                ("operator", Form::new().with(c.teal)),
                ("keyword", Form::new().with(c.purple).italic()),
                ("punctuation.bracket", Form::new().with(c.text_subtle)),
                ("punctuation.delimiter", Form::new().with(c.text_subtle)),
                ("comment", Form::new().with(c.comment).italic()),
                (
                    "comment.documentation",
                    Form::new().with(c.comment).bold().italic(),
                ),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(c.orange).bold()),
                ("markup.italic", Form::new().with(c.yellow).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(c.blue).bold()),
                ("markup.math", Form::new().with(c.teal)),
                ("markup.quote", Form::new().with(c.green).bold()),
                ("markup.environment", Form::new().with(c.purple)),
                ("markup.environment.name", Form::new().with(c.blue)),
                ("markup.link", Form::new().with(c.teal).underlined()),
                ("markup.raw", Form::new().with(c.yellow)),
                ("markup.list", Form::new().with(c.yellow)),
                ("markup.list.checked", Form::new().with(c.green)),
                ("markup.list.unchecked", Form::new().with(c.text_subtle)),
                ("diff.plus", Form::new().with(c.green)),
                ("diff.delta", Form::new().with(c.blue)),
                ("diff.delta.renamed", Form::new().with(c.yellow)),
                ("diff.minus", Form::new().with(c.red)),
                ("unresolved", Form::new().underlined().underline(c.red)),
            ]
        }
    );

    // Dracula theme — https://github.com/dracula/dracula-theme
    add_colorschemes!(
        [
            ("dracula", [
                (bg, "#282a36"),
                (current_line, "#44475a"),
                (selection, "#44475a"),
                (fg, "#f8f8f2"),
                (comment, "#6272a4"),
                (cyan, "#8be9fd"),
                (green, "#50fa7b"),
                (orange, "#ffb86c"),
                (pink, "#ff79c6"),
                (purple, "#bd93f9"),
                (red, "#ff5555"),
                (yellow, "#f1fa8c"),
            ]),
            ("dracula-alucard", [
                (bg, "#fffbeb"),
                (current_line, "#6c664b"),
                (selection, "#cfcfde"),
                (fg, "#1f1f1f"),
                (comment, "#6c664b"),
                (cyan, "#036a96"),
                (green, "#14710a"),
                (orange, "#a34d14"),
                (pink, "#a3144d"),
                (purple, "#644ac9"),
                (red, "#cb3a2a"),
                (yellow, "#846e15"),
            ])
        ],
        |c, has_background| {
            let default = if has_background {
                Form::new().with(c.fg).on(c.bg)
            } else {
                Form::new().with(c.fg)
            };

            [
                ("default", default),
                ("accent", Form::new().with(c.cyan).bold()),
                ("default.error", Form::new().with(c.red)),
                ("accent.error", Form::new().with(c.red).bold()),
                ("default.warning", Form::new().with(c.yellow)),
                ("accent.warning", Form::new().with(c.orange).bold()),
                ("default.info", Form::new().with(c.cyan)),
                ("accent.info", Form::new().with(c.cyan).bold()),
                ("default.debug", Form::new().with(c.comment)),
                ("accent.debug", Form::new().with(c.purple).bold()),
                ("caret.main", Form::new().with(c.bg).on(c.yellow)),
                ("caret.extra", Form::new().with(c.bg).on(c.fg)),
                ("selection.main", Form::new().on(c.selection)),
                (
                    "selection.extra",
                    Form::new().on(c.selection).interpolate(default, 50),
                ),
                ("selection.main.indent", Form::new().with_on(c.selection)),
                (
                    "selection.extra.indent",
                    Form::new().with_on(c.selection).interpolate(default, 50),
                ),
                ("cloak", Form::new().with(c.comment).on(c.bg)),
                ("replace", Form::new().with(c.current_line)),
                (
                    "replace.newline.trailing",
                    Form::new().with(c.red).on(c.current_line),
                ),
                ("toggle.hover", Form::new().on(c.current_line)),
                ("toggle.click", Form::new().on(c.comment)),
                // duat-base forms
                ("linenum.main", Form::new().with(c.current_line)),
                ("linenum.wrapped", Form::new().with(c.cyan)),
                ("file", Form::new().with(c.yellow)),
                ("selections", Form::new().with(c.purple)),
                ("coord", Form::new().with(c.orange)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(c.green)),
                ("terminal.border", Form::new().with(c.current_line).on(c.bg)),
                ("terminal.frame", Form::new().with(c.fg).on(c.bg)),
                ("notifs.colon", Form::new().with(c.comment)),
                ("prompt", Form::new().with(c.green)),
                ("prompt.colon", Form::new().with(c.comment)),
                ("default.StatusLine", default.on(c.current_line)),
                ("default.LogBook", default.on(c.current_line)),
                ("default.VertRule", default.with(c.current_line)),
                ("default.LineNumbers", default.with(c.comment)),
                (
                    "matched_pair",
                    Form::new().with(c.orange).on(c.current_line).bold(),
                ),
                ("logbook.location", Form::new().with(c.comment)),
                (
                    "default.Buffer.current_line",
                    default.on(c.current_line).interpolate(default, 50),
                ),
                ("default.Completions", default.on(c.current_line)),
                ("selected.Completions", Form::new().with(c.bg).on(c.purple)),
                ("default.WhichKey", default.with(c.fg)),
                ("key", Form::new().with(c.orange)),
                ("key.special", Form::new().with(c.cyan)),
                // For duatmode
                ("caret.main.Normal", Form::new().with(c.bg).on(c.fg)),
                ("caret.extra.Normal", Form::new().with(c.bg).on(c.cyan)),
                ("caret.main.Insert", Form::new().with(c.bg).on(c.pink)),
                ("caret.extra.Insert", Form::new().with(c.bg).on(c.yellow)),
                ("param", Form::new().with(c.purple)),
                ("param.flag", Form::new().with(c.pink)),
                // Tree sitter Forms
                ("variable", Form::new().with(c.fg)),
                ("variable.builtin", Form::new().with(c.orange)),
                ("variable.member", Form::new().with(c.fg)),
                ("constant", Form::new().with(c.purple)),
                ("constant.builtin", Form::new().with(c.purple)),
                ("static", Form::new().with(c.purple).reset()),
                ("module", Form::new().with(c.cyan).italic()),
                ("label", Form::new().with(c.cyan)),
                ("string", Form::new().with(c.yellow)),
                ("string.escape", Form::new().with(c.pink)),
                ("string.special.path", Form::new().with(c.cyan).underlined()),
                ("character", Form::new().with(c.pink)),
                ("boolean", Form::new().with(c.purple)),
                ("number", Form::new().with(c.purple)),
                ("type", Form::new().with(c.cyan).italic()),
                ("type.builtin", Form::new().with(c.cyan).reset()),
                ("type.enum", Form::new().with(c.cyan).reset()),
                ("type.enum.variant", Form::new().with(c.purple).italic()),
                ("attribute", Form::new().with(c.green)),
                ("property", Form::new().with(c.fg)),
                ("function", Form::new().with(c.green).reset()),
                ("function.macro", Form::new().with(c.pink).italic()),
                ("constructor", Form::new().with(c.cyan)),
                ("operator", Form::new().with(c.pink)),
                ("keyword", Form::new().with(c.pink)),
                ("punctuation.bracket", Form::new().with(c.fg)),
                ("punctuation.delimiter", Form::new().with(c.fg)),
                ("comment", Form::new().with(c.comment)),
                ("comment.documentation", Form::new().with(c.comment).bold()),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(c.orange).bold()),
                ("markup.italic", Form::new().with(c.yellow).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(c.purple).bold()),
                ("markup.math", Form::new().with(c.cyan)),
                ("markup.quote", Form::new().with(c.yellow).bold()),
                ("markup.environment", Form::new().with(c.pink)),
                ("markup.environment.name", Form::new().with(c.cyan)),
                ("markup.link", Form::new().with(c.cyan).underlined()),
                ("markup.raw", Form::new().with(c.green)),
                ("markup.list", Form::new().with(c.pink)),
                ("markup.list.checked", Form::new().with(c.green)),
                ("markup.list.unchecked", Form::new().with(c.comment)),
                ("diff.plus", Form::new().with(c.green)),
                ("diff.delta", Form::new().with(c.cyan)),
                ("diff.delta.renamed", Form::new().with(c.yellow)),
                ("diff.minus", Form::new().with(c.red)),
                ("unresolved", Form::new().underlined().underline(c.red)),
            ]
        }
    );

    // Nord theme — https://www.nordtheme.com/docs/colors-and-palettes
    add_colorschemes!(
        [("nord", [
            // Polar Night
            (nord0, "#2e3440"),
            (nord1, "#3b4252"),
            (nord2, "#434c5e"),
            (nord3, "#4c566a"),
            // Snow Storm
            (nord4, "#d8dee9"),
            (nord5, "#e5e9f0"),
            (nord6, "#eceff4"),
            // Frost
            (nord7, "#8fbcbb"),
            (nord8, "#88c0d0"),
            (nord9, "#81a1c1"),
            (nord10, "#5e81ac"),
            // Aurora
            (nord11, "#bf616a"),
            (nord12, "#d08770"),
            (nord13, "#ebcb8b"),
            (nord14, "#a3be8c"),
            (nord15, "#b48ead"),
        ]),],
        |c, has_background| {
            let default = if has_background {
                Form::new().with(c.nord4).on(c.nord0)
            } else {
                Form::new().with(c.nord4)
            };

            [
                ("default", default),
                ("accent", Form::new().with(c.nord8).bold()),
                ("default.error", Form::new().with(c.nord11)),
                ("accent.error", Form::new().with(c.nord11).bold()),
                ("default.warning", Form::new().with(c.nord13)),
                ("accent.warning", Form::new().with(c.nord12).bold()),
                ("default.info", Form::new().with(c.nord9)),
                ("accent.info", Form::new().with(c.nord8).bold()),
                ("default.debug", Form::new().with(c.nord3)),
                ("accent.debug", Form::new().with(c.nord15).bold()),
                ("caret.main", Form::new().with(c.nord0).on(c.nord4)),
                ("caret.extra", Form::new().with(c.nord0).on(c.nord7)),
                ("selection.main", Form::new().on(c.nord2)),
                ("selection.extra", Form::new().on(c.nord1)),
                ("selection.main.indent", Form::new().with_on(c.nord2)),
                ("selection.extra.indent", Form::new().with_on(c.nord1)),
                ("cloak", Form::new().with(c.nord3).on(c.nord0)),
                ("replace", Form::new().with(c.nord1)),
                (
                    "replace.newline.trailing",
                    Form::new().with(c.nord11).on(c.nord1),
                ),
                ("toggle.hover", Form::new().on(c.nord1)),
                ("toggle.click", Form::new().on(c.nord2)),
                ("linenum.main", Form::new().with(c.nord13)),
                ("linenum.wrapped", Form::new().with(c.nord7)),
                ("file", Form::new().with(c.nord13)),
                ("selections", Form::new().with(c.nord9)),
                ("coord", Form::new().with(c.nord12)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(c.nord14)),
                ("terminal.border", Form::new().with(c.nord1).on(c.nord0)),
                ("terminal.frame", Form::new().with(c.nord4).on(c.nord0)),
                ("notifs.colon", Form::new().with(c.nord3)),
                ("prompt", Form::new().with(c.nord14)),
                ("prompt.colon", Form::new().with(c.nord3)),
                ("default.StatusLine", default.on(c.nord1)),
                ("default.LogBook", default.on(c.nord1)),
                ("default.VertRule", default.with(c.nord1)),
                ("default.LineNumbers", default.with(c.nord3)),
                (
                    "matched_pair",
                    Form::new().with(c.nord12).on(c.nord2).bold(),
                ),
                ("logbook.location", Form::new().with(c.nord3)),
                (
                    "default.Buffer.current_line",
                    default.on(c.nord1).interpolate(default, 50),
                ),
                ("default.Completions", default.on(c.nord1)),
                (
                    "selected.Completions",
                    Form::new().with(c.nord0).on(c.nord3),
                ),
                ("default.WhichKey", default.with(c.nord4)),
                ("key", Form::new().with(c.nord12)),
                ("key.special", Form::new().with(c.nord7)),
                ("caret.main.Normal", Form::new().with(c.nord0).on(c.nord4)),
                ("caret.extra.Normal", Form::new().with(c.nord0).on(c.nord8)),
                ("caret.main.Insert", Form::new().with(c.nord0).on(c.nord15)),
                ("caret.extra.Insert", Form::new().with(c.nord0).on(c.nord13)),
                ("param", Form::new().with(c.nord15)),
                ("param.flag", Form::new().with(c.nord8)),
                // Tree sitter Forms
                ("variable", Form::new().with(c.nord4)),
                ("variable.builtin", Form::new().with(c.nord12)),
                ("variable.member", Form::new().with(c.nord4)),
                ("constant", Form::new().with(c.nord12)),
                ("constant.builtin", Form::new().with(c.nord12)),
                ("static", Form::new().with(c.nord12).reset()),
                ("module", Form::new().with(c.nord9).italic()),
                ("label", Form::new().with(c.nord7)),
                ("string", Form::new().with(c.nord14)),
                ("string.escape", Form::new().with(c.nord13)),
                (
                    "string.special.path",
                    Form::new().with(c.nord8).underlined(),
                ),
                ("character", Form::new().with(c.nord14)),
                ("boolean", Form::new().with(c.nord12)),
                ("number", Form::new().with(c.nord15)),
                ("type", Form::new().with(c.nord7).italic()),
                ("type.builtin", Form::new().with(c.nord7).reset()),
                ("type.enum", Form::new().with(c.nord7).reset()),
                ("type.enum.variant", Form::new().with(c.nord12).italic()),
                ("attribute", Form::new().with(c.nord13)),
                ("property", Form::new().with(c.nord4)),
                ("function", Form::new().with(c.nord8).reset()),
                ("function.macro", Form::new().with(c.nord8).italic()),
                ("constructor", Form::new().with(c.nord7)),
                ("operator", Form::new().with(c.nord9)),
                ("keyword", Form::new().with(c.nord9)),
                ("punctuation.bracket", Form::new().with(c.nord3)),
                ("punctuation.delimiter", Form::new().with(c.nord4)),
                ("comment", Form::new().with(c.nord3)),
                ("comment.documentation", Form::new().with(c.nord3).bold()),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(c.nord12).bold()),
                ("markup.italic", Form::new().with(c.nord13).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(c.nord9).bold()),
                ("markup.math", Form::new().with(c.nord8)),
                ("markup.quote", Form::new().with(c.nord14).bold()),
                ("markup.environment", Form::new().with(c.nord15)),
                ("markup.environment.name", Form::new().with(c.nord9)),
                ("markup.link", Form::new().with(c.nord8).underlined()),
                ("markup.raw", Form::new().with(c.nord7)),
                ("markup.list", Form::new().with(c.nord9)),
                ("markup.list.checked", Form::new().with(c.nord14)),
                ("markup.list.unchecked", Form::new().with(c.nord3)),
                ("diff.plus", Form::new().with(c.nord14)),
                ("diff.delta", Form::new().with(c.nord8)),
                ("diff.delta.renamed", Form::new().with(c.nord13)),
                ("diff.minus", Form::new().with(c.nord11)),
                ("unresolved", Form::new().underlined().underline(c.nord11)),
            ]
        }
    );
}

#[cfg(test)]
mod tests {
    use super::{add_default, list};

    #[test]
    fn night_owl_is_registered() {
        add_default();
        assert!(list().iter().any(|scheme| scheme == "night-owl"));
    }
}
