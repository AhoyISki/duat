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
///         |s, has_background| {
///             let default = if has_background {
///                 Form::new().with(s.foreground)
///             } else {
///                 Form::new().with(s.foreground).on(s.background)
///             };
///
///             [
///                 ("default", default),
///                 ("accent", Form::new().with(s.foreground).bold()),
///                 ("default.error", Form::new().with(s.red)),
///                 ("accent.error", Form::new().with(s.red).underlined().bold()),
///                 ("function", Form::new().with(s.blue)),
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
#[cfg(feature = "term-ui")]
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
        |s, has_background| {
            let default = if has_background {
                Form::new().with(s.text).on(s.base)
            } else {
                Form::new().with(s.text)
            };

            [
                // The default form, self explanatory.
                ("default", default),
                // A form to "accent" text, highlighting things.
                ("accent", Form::new().with(s.rosewater).bold()),
                // Variations of the above two for different scenarios.
                ("default.error", Form::new().with(s.maroon)),
                ("accent.error", Form::new().with(s.flamingo)),
                ("default.warn", Form::new().with(s.yellow)),
                ("accent.warn", Form::new().with(s.rosewater)),
                ("default.info", Form::new().with(s.sapphire)),
                ("accent.info", Form::new().with(s.text)),
                ("default.debug", Form::new().with(s.subtext1)),
                ("accent.debug", Form::new().with(s.lavender).bold()),
                // In duat, the cursor is the blinking bit.
                ("cursor.main", Form::new().with(s.base).on(s.rosewater)),
                ("cursor.extra", Form::new().with(s.base).on(s.teal)),
                // And the selection is the rest.
                ("selection.main", Form::new().on(s.surface1)),
                ("selection.extra", Form::new().on(s.surface0)),
                // This is to hide selected indent guides.
                ("cursor.main.indent", Form::new().with_on(s.rosewater)),
                ("cursor.extra.indent", Form::new().with_on(s.teal)),
                ("selection.main.indent", Form::new().with_on(s.surface1)),
                ("selection.extra.indent", Form::new().with_on(s.surface0)),
                // A utility form, mostly used to cover all other forms on screen.
                ("cloak", Form::new().reset().with(s.overlay1).on(s.base)),
                // Form for replacement characters (like indent guides).
                ("replace", Form::new().with(s.surface0)),
                // Same, but only for trailing newlines.
                (
                    "replace.newline.trailing",
                    Form::new().with(s.red).on(s.surface1),
                ),
                // Forms for hovered and clicked Toggles.
                ("toggle.hover", Form::new().on(s.surface0)),
                ("toggle.click", Form::new().on(s.surface1)),
                // Forms for the line numbers.
                ("linenum.main", Form::new().with(s.yellow)),
                ("linenum.wrapped", Form::new().with(s.teal)),
                // Various forms for the StatusLine parts.
                ("file", Form::new().with(s.yellow)),
                ("selections", Form::new().with(s.blue)),
                ("coord", Form::new().with(s.peach)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(s.green)),
                // Borders separate Buffers.
                ("terminal.border", Form::new().with(s.surface0).on(s.base)),
                // Frames surround spawned widgets.
                ("terminal.frame", Form::new().with(s.text).on(s.base)),
                ("notifs.colon", Form::new().with(s.subtext0)),
                // The prompt in a promptline (like `reverse search`).
                ("prompt", Form::new().with(s.green)),
                // The colon that separates the prompt from the input.
                ("prompt.colon", Form::new().with(s.subtext0)),
                // Various default forms for specific Widgets.
                (
                    "default.Buffer.current_line",
                    default.on(s.surface0).interpolate(default, 50),
                ),
                ("default.StatusLine", default.on(s.mantle)),
                ("default.LogBook", default.on(s.mantle)),
                ("default.VertRule", default.with(s.surface0)),
                ("default.LineNumbers", default.with(s.overlay0)),
                ("default.Completions", default.on(s.surface1)),
                // Used when the cursor is over parentheses pairs.
                (
                    "matched_pair",
                    Form::new().with(s.peach).on(s.surface1).bold(),
                ),
                // Form used on the location a log message came from.
                ("logbook.location", Form::new().with(s.subtext1)),
                (
                    "selected.Completions",
                    Form::new().with(s.base).on(s.overlay0),
                ),
                // Used in the `mapped_txt` StatusLine part.
                ("key", Form::new().with(s.peach)),
                ("key.special", Form::new().with(s.teal)),
                // Same as before, but on specific Modes.
                ("cursor.main.Insert", Form::new().with(s.base).on(s.mauve)),
                ("cursor.extra.Insert", Form::new().with(s.base).on(s.yellow)),
                // Used when typing arguments to commands.
                ("param", Form::new().with(s.lavender)),
                ("param.flag", Form::new().with(s.pink)),
                // Tree sitter/LSP Forms
                ("variable", Form::new().with(s.text)),
                ("variable.builtin", Form::new().with(s.peach)),
                ("variable.member", Form::new().with(s.lavender)),
                ("constant", Form::new().with(s.peach).reset()),
                ("constant.builtin", Form::new().with(s.peach).reset()),
                ("static", Form::new().with(s.peach).reset()),
                ("module", Form::new().with(s.blue).italic()),
                ("label", Form::new().with(s.green)),
                ("string", Form::new().with(s.green)),
                ("string.escape", Form::new().with(s.peach)),
                ("string.special.path", Form::new().with(s.sky).underlined()),
                ("character", Form::new().with(s.peach)),
                ("boolean", Form::new().with(s.peach)),
                ("number", Form::new().with(s.peach)),
                ("type", Form::new().with(s.yellow).italic()),
                ("type.builtin", Form::new().with(s.yellow).reset()),
                ("type.enum", Form::new().with(s.yellow).reset()),
                ("type.enum.variant", Form::new().with(s.peach).italic()),
                ("interface", Form::new().with(s.text).bold().reset()),
                ("attribute", Form::new().with(s.green)),
                ("property", Form::new().with(s.lavender)),
                ("function", Form::new().with(s.blue).reset()),
                ("function.macro", Form::new().with(s.lavender).italic()),
                ("constructor", Form::new().with(s.peach)),
                ("operator", Form::new().with(s.sapphire)),
                ("keyword", Form::new().with(s.mauve)),
                ("punctuation.bracket", Form::new().with(s.subtext0)),
                ("punctuation.delimiter", Form::new().with(s.subtext0)),
                ("comment", Form::new().with(s.overlay1)),
                ("comment.documentation", Form::new().with(s.overlay1).bold()),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(s.maroon).bold()),
                ("markup.italic", Form::new().with(s.maroon).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(s.blue).bold()),
                ("markup.math", Form::new().with(s.yellow)),
                ("markup.quote", Form::new().with(s.maroon).bold()),
                ("markup.environment", Form::new().with(s.pink)),
                ("markup.environment.name", Form::new().with(s.blue)),
                ("markup.link", Form::new().with(s.lavender).underlined()),
                ("markup.raw", Form::new().with(s.teal)),
                ("markup.list", Form::new().with(s.yellow)),
                ("markup.list.checked", Form::new().with(s.green)),
                ("markup.list.unchecked", Form::new().with(s.overlay1)),
                ("diff.plus", Form::new().with(s.green)),
                ("diff.delta", Form::new().with(s.blue)),
                ("diff.delta.renamed", Form::new().with(s.yellow)),
                ("diff.minus", Form::new().with(s.red)),
                ("unresolved", Form::new().underlined().underline(s.red)),
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
        |s, has_background| {
            let default = if has_background {
                Form::new().with(s.text).on(s.bg)
            } else {
                Form::new().with(s.text)
            };

            [
                ("default", default),
                ("accent", Form::new().with(s.cyan).bold()),
                ("default.error", Form::new().with(s.red1)),
                ("accent.error", Form::new().with(s.red).bold()),
                ("default.warn", Form::new().with(s.yellow)),
                ("accent.warn", Form::new().with(s.orange).bold()),
                ("default.info", Form::new().with(s.blue1)),
                ("accent.info", Form::new().with(s.cyan).bold()),
                ("default.debug", Form::new().with(s.fg_dark)),
                ("accent.debug", Form::new().with(s.purple).bold()),
                ("cursor.main", Form::new().with(s.bg).on(s.text)),
                ("cursor.extra", Form::new().with(s.bg).on(s.teal)),
                ("selection.main", Form::new().on(s.blue0)),
                ("selection.extra", Form::new().on(s.fg_gutter)),
                ("cursor.main.indent", Form::new().with_on(s.text)),
                ("cursor.extra.indent", Form::new().with_on(s.teal)),
                ("selection.main.indent", Form::new().with_on(s.blue0)),
                ("selection.extra.indent", Form::new().with_on(s.fg_gutter)),
                ("cloak", Form::new().reset().with(s.dark5).on(s.bg)),
                ("replace", Form::new().with(s.bg_highlight)),
                (
                    "replace.newline.trailing",
                    Form::new().with(s.red).on(s.terminal_black),
                ),
                ("toggle.hover", Form::new().on(s.bg_highlight)),
                ("toggle.click", Form::new().on(s.terminal_black)),
                // duat-base forms
                ("linenum.main", Form::new().with(s.yellow)),
                ("linenum.wrapped", Form::new().with(s.teal)),
                ("file", Form::new().with(s.yellow)),
                ("selections", Form::new().with(s.blue)),
                ("coord", Form::new().with(s.orange)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(s.green)),
                ("terminal.border", Form::new().with(s.bg_highlight).on(s.bg)),
                ("terminal.frame", Form::new().with(s.text).on(s.bg)),
                ("notifs.colon", Form::new().with(s.dark3)),
                ("prompt", Form::new().with(s.green)),
                ("prompt.colon", Form::new().with(s.dark3)),
                ("default.StatusLine", default.on(s.bg_dark)),
                ("default.LogBook", default.on(s.bg_dark)),
                ("default.VertRule", default.with(s.bg_dark)),
                ("default.LineNumbers", default.with(s.dark3)),
                (
                    "matched_pair",
                    Form::new().with(s.orange).on(s.terminal_black).bold(),
                ),
                ("logbook.location", Form::new().with(s.fg_dark)),
                (
                    "default.Buffer.current_line",
                    default.on(s.bg_highlight).interpolate(default, 50),
                ),
                ("default.Completions", default.on(s.terminal_black)),
                ("selected.Completions", Form::new().with(s.bg).on(s.dark5)),
                ("default.WhichKey", default.with(s.text)),
                ("key", Form::new().with(s.orange)),
                ("key.special", Form::new().with(s.teal)),
                // For duatmode
                ("cursor.main.Insert", Form::new().with(s.bg).on(s.magenta)),
                ("cursor.extra.Insert", Form::new().with(s.bg).on(s.yellow)),
                ("param", Form::new().with(s.purple)),
                ("param.flag", Form::new().with(s.cyan)),
                // Tree sitter Forms
                ("variable", Form::new().with(s.text)),
                ("variable.builtin", Form::new().with(s.orange)),
                ("variable.member", Form::new().with(s.green1)),
                ("constant", Form::new().with(s.orange)),
                ("constant.builtin", Form::new().with(s.orange)),
                ("static", Form::new().with(s.orange).reset()),
                ("module", Form::new().with(s.blue).italic()),
                ("label", Form::new().with(s.blue)),
                ("string", Form::new().with(s.green)),
                ("string.escape", Form::new().with(s.orange)),
                ("string.special.path", Form::new().with(s.cyan).underlined()),
                ("character", Form::new().with(s.orange)),
                ("boolean", Form::new().with(s.orange)),
                ("number", Form::new().with(s.orange)),
                ("type", Form::new().with(s.blue1).italic()),
                ("type.builtin", Form::new().with(s.blue1).reset()),
                ("type.enum", Form::new().with(s.blue1).reset()),
                ("type.enum.variant", Form::new().with(s.orange).italic()),
                ("attribute", Form::new().with(s.yellow)),
                ("property", Form::new().with(s.green1)),
                ("function", Form::new().with(s.blue).reset()),
                ("function.macro", Form::new().with(s.blue).italic()),
                ("constructor", Form::new().with(s.orange)),
                ("operator", Form::new().with(s.blue5)),
                ("keyword", Form::new().with(s.magenta)),
                ("punctuation.bracket", Form::new().with(s.dark5)),
                ("punctuation.delimiter", Form::new().with(s.blue5)),
                ("comment", Form::new().with(s.comment)),
                ("comment.documentation", Form::new().with(s.comment).bold()),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(s.red).bold()),
                ("markup.italic", Form::new().with(s.red).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(s.blue).bold()),
                ("markup.math", Form::new().with(s.yellow)),
                ("markup.quote", Form::new().with(s.green).bold()),
                ("markup.environment", Form::new().with(s.magenta)),
                ("markup.environment.name", Form::new().with(s.blue)),
                ("markup.link", Form::new().with(s.cyan).underlined()),
                ("markup.raw", Form::new().with(s.teal)),
                ("markup.list", Form::new().with(s.yellow)),
                ("markup.list.checked", Form::new().with(s.green)),
                ("markup.list.unchecked", Form::new().with(s.dark5)),
                ("diff.plus", Form::new().with(s.green)),
                ("diff.delta", Form::new().with(s.blue)),
                ("diff.delta.renamed", Form::new().with(s.yellow)),
                ("diff.minus", Form::new().with(s.red)),
                ("unresolved", Form::new().underlined().underline(s.red)),
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
        |s, has_background| {
            let default = if has_background {
                Form::new().with(s.text).on(s.bg)
            } else {
                Form::new().with(s.text)
            };

            [
                ("default", default),
                ("accent", Form::new().with(s.blue).bold()),
                ("default.error", Form::new().with(s.red)),
                ("accent.error", Form::new().with(s.red).bold()),
                ("default.warn", Form::new().with(s.yellow)),
                ("accent.warn", Form::new().with(s.orange).bold()),
                ("default.info", Form::new().with(s.teal)),
                ("accent.info", Form::new().with(s.teal).bold()),
                ("default.debug", Form::new().with(s.text_muted)),
                ("accent.debug", Form::new().with(s.purple).bold()),
                ("cursor.main", Form::new().with(s.bg).on(s.text)),
                ("cursor.extra", Form::new().with(s.bg).on(s.text_muted)),
                ("selection.main", Form::new().on(s.bg_subtle)),
                ("selection.extra", Form::new().on(s.bg_overlay)),
                ("cursor.main.indent", Form::new().with_on(s.text)),
                ("cursor.extra.indent", Form::new().with_on(s.text_muted)),
                ("selection.main.indent", Form::new().with_on(s.bg_subtle)),
                ("selection.extra.indent", Form::new().with_on(s.bg_overlay)),
                ("cloak", Form::new().reset().with(s.text_subtle).on(s.bg)),
                ("replace", Form::new().with(s.bg_subtle)),
                (
                    "replace.newline.trailing",
                    Form::new().with(s.red).on(s.bg_subtle),
                ),
                ("toggle.hover", Form::new().on(s.bg_overlay)),
                ("toggle.click", Form::new().on(s.bg_subtle)),
                // duat-base forms
                ("linenum.main", Form::new().with(s.yellow)),
                ("linenum.wrapped", Form::new().with(s.teal)),
                ("file", Form::new().with(s.yellow)),
                ("selections", Form::new().with(s.blue)),
                ("coord", Form::new().with(s.orange)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(s.green)),
                ("terminal.border", Form::new().with(s.bg_subtle).on(s.bg)),
                ("terminal.frame", Form::new().with(s.text).on(s.bg)),
                ("notifs.colon", Form::new().with(s.text_muted)),
                ("prompt", Form::new().with(s.green)),
                ("prompt.colon", Form::new().with(s.text_muted)),
                ("default.StatusLine", default.on(s.bg_subtle)),
                ("default.LogBook", default.on(s.bg_subtle)),
                ("default.VertRule", default.with(s.bg_subtle)),
                ("default.LineNumbers", default.with(s.text_subtle)),
                (
                    "matched_pair",
                    Form::new().with(s.orange).on(s.bg_subtle).bold(),
                ),
                ("logbook.location", Form::new().with(s.text_muted)),
                (
                    "default.Buffer.current_line",
                    default.on(s.bg_subtle).interpolate(default, 50),
                ),
                ("default.Completions", default.on(s.bg_overlay)),
                ("selected.Completions", Form::new().with(s.bg).on(s.blue)),
                ("default.WhichKey", default.with(s.text)),
                ("key", Form::new().with(s.orange)),
                ("key.special", Form::new().with(s.teal)),
                // For duatmode
                ("cursor.main.Insert", Form::new().with(s.bg).on(s.purple)),
                ("cursor.extra.Insert", Form::new().with(s.bg).on(s.yellow)),
                ("param", Form::new().with(s.purple)),
                ("param.flag", Form::new().with(s.teal)),
                // Tree sitter Forms
                ("variable", Form::new().with(s.text)),
                ("variable.builtin", Form::new().with(s.orange)),
                ("variable.member", Form::new().with(s.blue)),
                ("constant", Form::new().with(s.number_color)),
                ("constant.builtin", Form::new().with(s.number_color)),
                ("static", Form::new().with(s.number_color).reset()),
                ("module", Form::new().with(s.blue).italic()),
                ("label", Form::new().with(s.green)),
                ("string", Form::new().with(s.string_color)),
                ("string.escape", Form::new().with(s.orange)),
                ("string.special.path", Form::new().with(s.teal).underlined()),
                ("character", Form::new().with(s.number_color)),
                ("boolean", Form::new().with(s.number_color)),
                ("number", Form::new().with(s.number_color)),
                ("type", Form::new().with(s.teal).italic()),
                ("type.builtin", Form::new().with(s.teal).reset()),
                ("type.enum", Form::new().with(s.teal).reset()),
                ("type.enum.variant", Form::new().with(s.orange).italic()),
                ("attribute", Form::new().with(s.yellow)),
                ("property", Form::new().with(s.text_muted)),
                ("function", Form::new().with(s.purple).reset()),
                ("function.macro", Form::new().with(s.purple).italic()),
                ("constructor", Form::new().with(s.orange)),
                ("operator", Form::new().with(s.keyword_color)),
                ("keyword", Form::new().with(s.keyword_color)),
                ("punctuation.bracket", Form::new().with(s.text_subtle)),
                ("punctuation.delimiter", Form::new().with(s.text_subtle)),
                ("comment", Form::new().with(s.text_muted)),
                (
                    "comment.documentation",
                    Form::new().with(s.text_muted).bold(),
                ),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(s.red).bold()),
                ("markup.italic", Form::new().with(s.red).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(s.blue).bold()),
                ("markup.math", Form::new().with(s.yellow)),
                ("markup.quote", Form::new().with(s.green).bold()),
                ("markup.environment", Form::new().with(s.purple)),
                ("markup.environment.name", Form::new().with(s.blue)),
                ("markup.link", Form::new().with(s.teal).underlined()),
                ("markup.raw", Form::new().with(s.string_color)),
                ("markup.list", Form::new().with(s.yellow)),
                ("markup.list.checked", Form::new().with(s.green)),
                ("markup.list.unchecked", Form::new().with(s.text_subtle)),
                ("diff.plus", Form::new().with(s.green)),
                ("diff.delta", Form::new().with(s.blue)),
                ("diff.delta.renamed", Form::new().with(s.yellow)),
                ("diff.minus", Form::new().with(s.red)),
                ("unresolved", Form::new().underlined().underline(s.red)),
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
        |s, has_background| {
            let default = if has_background {
                Form::new().with(s.text).on(s.bg)
            } else {
                Form::new().with(s.text)
            };

            [
                ("default", default),
                ("accent", Form::new().with(s.teal).bold()),
                ("default.error", Form::new().with(s.red)),
                ("accent.error", Form::new().with(s.red).bold()),
                ("default.warn", Form::new().with(s.yellow)),
                ("accent.warn", Form::new().with(s.orange).bold()),
                ("default.info", Form::new().with(s.blue)),
                ("accent.info", Form::new().with(s.teal).bold()),
                ("default.debug", Form::new().with(s.text_muted)),
                ("accent.debug", Form::new().with(s.purple).bold()),
                ("cursor.main", Form::new().with(s.bg).on(s.text)),
                ("cursor.extra", Form::new().with(s.bg).on(s.teal)),
                ("selection.main", Form::new().on(s.surface_highlight)),
                ("selection.extra", Form::new().on(s.surface)),
                ("cursor.main.indent", Form::new().with_on(s.text)),
                ("cursor.extra.indent", Form::new().with_on(s.teal)),
                (
                    "selection.main.indent",
                    Form::new().with_on(s.surface_highlight),
                ),
                ("selection.extra.indent", Form::new().with_on(s.surface)),
                ("cloak", Form::new().reset().with(s.text_subtle).on(s.bg)),
                ("replace", Form::new().with(s.surface)),
                (
                    "replace.newline.trailing",
                    Form::new().with(s.red).on(s.surface_highlight),
                ),
                ("toggle.hover", Form::new().on(s.surface_inactive)),
                ("toggle.click", Form::new().on(s.surface_highlight)),
                ("linenum.main", Form::new().with(s.yellow)),
                ("linenum.wrapped", Form::new().with(s.teal)),
                ("file", Form::new().with(s.yellow)),
                ("selections", Form::new().with(s.blue)),
                ("coord", Form::new().with(s.orange)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(s.green)),
                ("terminal.border", Form::new().with(s.surface).on(s.bg)),
                ("terminal.frame", Form::new().with(s.text).on(s.bg)),
                ("notifs.colon", Form::new().with(s.text_muted)),
                ("prompt", Form::new().with(s.green)),
                ("prompt.colon", Form::new().with(s.text_muted)),
                ("default.StatusLine", default.on(s.surface_panel)),
                ("default.LogBook", default.on(s.surface_panel)),
                ("default.VertRule", default.with(s.surface)),
                ("default.LineNumbers", default.with(s.text_subtle)),
                (
                    "matched_pair",
                    Form::new().with(s.orange).on(s.surface_highlight).bold(),
                ),
                ("logbook.location", Form::new().with(s.text_muted)),
                (
                    "default.Buffer.current_line",
                    default.on(s.surface_highlight).interpolate(default, 50),
                ),
                ("default.Completions", default.on(s.surface_panel)),
                (
                    "selected.Completions",
                    Form::new().with(s.text).on(s.surface_highlight),
                ),
                ("default.WhichKey", default.with(s.text)),
                ("key", Form::new().with(s.orange)),
                ("key.special", Form::new().with(s.teal)),
                ("cursor.main.Insert", Form::new().with(s.bg).on(s.purple)),
                ("cursor.extra.Insert", Form::new().with(s.bg).on(s.yellow)),
                ("param", Form::new().with(s.purple)),
                ("param.flag", Form::new().with(s.teal)),
                ("variable", Form::new().with(s.text)),
                ("variable.builtin", Form::new().with(s.teal)),
                ("variable.member", Form::new().with(s.aqua)),
                ("constant", Form::new().with(s.blue)),
                ("constant.builtin", Form::new().with(s.blue)),
                ("static", Form::new().with(s.blue).reset()),
                ("module", Form::new().with(s.blue).italic()),
                ("label", Form::new().with(s.teal)),
                ("string", Form::new().with(s.yellow)),
                ("string.escape", Form::new().with(s.orange)),
                ("string.special.path", Form::new().with(s.blue).underlined()),
                ("character", Form::new().with(s.orange)),
                ("boolean", Form::new().with(s.blue)),
                ("number", Form::new().with(s.orange)),
                ("type", Form::new().with(s.green).italic()),
                ("type.builtin", Form::new().with(s.green).reset()),
                ("type.enum", Form::new().with(s.green).reset()),
                ("type.enum.variant", Form::new().with(s.orange).italic()),
                ("attribute", Form::new().with(s.green).italic()),
                ("property", Form::new().with(s.gold).italic()),
                ("function", Form::new().with(s.blue).reset()),
                ("function.macro", Form::new().with(s.purple).italic()),
                ("constructor", Form::new().with(s.sand)),
                ("operator", Form::new().with(s.teal)),
                ("keyword", Form::new().with(s.purple).italic()),
                ("punctuation.bracket", Form::new().with(s.text_subtle)),
                ("punctuation.delimiter", Form::new().with(s.text_subtle)),
                ("comment", Form::new().with(s.comment).italic()),
                (
                    "comment.documentation",
                    Form::new().with(s.comment).bold().italic(),
                ),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(s.orange).bold()),
                ("markup.italic", Form::new().with(s.yellow).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(s.blue).bold()),
                ("markup.math", Form::new().with(s.teal)),
                ("markup.quote", Form::new().with(s.green).bold()),
                ("markup.environment", Form::new().with(s.purple)),
                ("markup.environment.name", Form::new().with(s.blue)),
                ("markup.link", Form::new().with(s.teal).underlined()),
                ("markup.raw", Form::new().with(s.yellow)),
                ("markup.list", Form::new().with(s.yellow)),
                ("markup.list.checked", Form::new().with(s.green)),
                ("markup.list.unchecked", Form::new().with(s.text_subtle)),
                ("diff.plus", Form::new().with(s.green)),
                ("diff.delta", Form::new().with(s.blue)),
                ("diff.delta.renamed", Form::new().with(s.yellow)),
                ("diff.minus", Form::new().with(s.red)),
                ("unresolved", Form::new().underlined().underline(s.red)),
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
        |s, has_background| {
            let default = if has_background {
                Form::new().with(s.fg).on(s.bg)
            } else {
                Form::new().with(s.fg)
            };

            [
                ("default", default),
                ("accent", Form::new().with(s.cyan).bold()),
                ("default.error", Form::new().with(s.red)),
                ("accent.error", Form::new().with(s.red).bold()),
                ("default.warn", Form::new().with(s.yellow)),
                ("accent.warn", Form::new().with(s.orange).bold()),
                ("default.info", Form::new().with(s.cyan)),
                ("accent.info", Form::new().with(s.cyan).bold()),
                ("default.debug", Form::new().with(s.comment)),
                ("accent.debug", Form::new().with(s.purple).bold()),
                ("cursor.main", Form::new().with(s.bg).on(s.yellow)),
                ("cursor.extra", Form::new().with(s.bg).on(s.fg)),
                ("selection.main", Form::new().on(s.selection)),
                (
                    "selection.extra",
                    Form::new().on(s.selection).interpolate(default, 50),
                ),
                ("cursor.main.indent", Form::new().with_on(s.yellow)),
                ("cursor.extra.indent", Form::new().with_on(s.fg)),
                ("selection.main.indent", Form::new().with_on(s.selection)),
                (
                    "selection.extra.indent",
                    Form::new().with_on(s.selection).interpolate(default, 50),
                ),
                ("cloak", Form::new().reset().with(s.comment).on(s.bg)),
                ("replace", Form::new().with(s.current_line)),
                (
                    "replace.newline.trailing",
                    Form::new().with(s.red).on(s.current_line),
                ),
                ("toggle.hover", Form::new().on(s.current_line)),
                ("toggle.click", Form::new().on(s.comment)),
                // duat-base forms
                ("linenum.main", Form::new().with(s.current_line)),
                ("linenum.wrapped", Form::new().with(s.cyan)),
                ("file", Form::new().with(s.yellow)),
                ("selections", Form::new().with(s.purple)),
                ("coord", Form::new().with(s.orange)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(s.green)),
                ("terminal.border", Form::new().with(s.current_line).on(s.bg)),
                ("terminal.frame", Form::new().with(s.fg).on(s.bg)),
                ("notifs.colon", Form::new().with(s.comment)),
                ("prompt", Form::new().with(s.green)),
                ("prompt.colon", Form::new().with(s.comment)),
                ("default.StatusLine", default.on(s.current_line)),
                ("default.LogBook", default.on(s.current_line)),
                ("default.VertRule", default.with(s.current_line)),
                ("default.LineNumbers", default.with(s.comment)),
                (
                    "matched_pair",
                    Form::new().with(s.orange).on(s.current_line).bold(),
                ),
                ("logbook.location", Form::new().with(s.comment)),
                (
                    "default.Buffer.current_line",
                    default.on(s.current_line).interpolate(default, 50),
                ),
                ("default.Completions", default.on(s.current_line)),
                ("selected.Completions", Form::new().with(s.bg).on(s.purple)),
                ("default.WhichKey", default.with(s.fg)),
                ("key", Form::new().with(s.orange)),
                ("key.special", Form::new().with(s.cyan)),
                // For duatmode
                ("cursor.main.Insert", Form::new().with(s.bg).on(s.pink)),
                ("cursor.extra.Insert", Form::new().with(s.bg).on(s.yellow)),
                ("param", Form::new().with(s.purple)),
                ("param.flag", Form::new().with(s.pink)),
                // Tree sitter Forms
                ("variable", Form::new().with(s.fg)),
                ("variable.builtin", Form::new().with(s.orange)),
                ("variable.member", Form::new().with(s.fg)),
                ("constant", Form::new().with(s.purple)),
                ("constant.builtin", Form::new().with(s.purple)),
                ("static", Form::new().with(s.purple).reset()),
                ("module", Form::new().with(s.cyan).italic()),
                ("label", Form::new().with(s.cyan)),
                ("string", Form::new().with(s.yellow)),
                ("string.escape", Form::new().with(s.pink)),
                ("string.special.path", Form::new().with(s.cyan).underlined()),
                ("character", Form::new().with(s.pink)),
                ("boolean", Form::new().with(s.purple)),
                ("number", Form::new().with(s.purple)),
                ("type", Form::new().with(s.cyan).italic()),
                ("type.builtin", Form::new().with(s.cyan).reset()),
                ("type.enum", Form::new().with(s.cyan).reset()),
                ("type.enum.variant", Form::new().with(s.purple).italic()),
                ("attribute", Form::new().with(s.green)),
                ("property", Form::new().with(s.fg)),
                ("function", Form::new().with(s.green).reset()),
                ("function.macro", Form::new().with(s.pink).italic()),
                ("constructor", Form::new().with(s.cyan)),
                ("operator", Form::new().with(s.pink)),
                ("keyword", Form::new().with(s.pink)),
                ("punctuation.bracket", Form::new().with(s.fg)),
                ("punctuation.delimiter", Form::new().with(s.fg)),
                ("comment", Form::new().with(s.comment)),
                ("comment.documentation", Form::new().with(s.comment).bold()),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(s.orange).bold()),
                ("markup.italic", Form::new().with(s.yellow).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(s.purple).bold()),
                ("markup.math", Form::new().with(s.cyan)),
                ("markup.quote", Form::new().with(s.yellow).bold()),
                ("markup.environment", Form::new().with(s.pink)),
                ("markup.environment.name", Form::new().with(s.cyan)),
                ("markup.link", Form::new().with(s.cyan).underlined()),
                ("markup.raw", Form::new().with(s.green)),
                ("markup.list", Form::new().with(s.pink)),
                ("markup.list.checked", Form::new().with(s.green)),
                ("markup.list.unchecked", Form::new().with(s.comment)),
                ("diff.plus", Form::new().with(s.green)),
                ("diff.delta", Form::new().with(s.cyan)),
                ("diff.delta.renamed", Form::new().with(s.yellow)),
                ("diff.minus", Form::new().with(s.red)),
                ("unresolved", Form::new().underlined().underline(s.red)),
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
        |s, has_background| {
            let default = if has_background {
                Form::new().with(s.nord4).on(s.nord0)
            } else {
                Form::new().with(s.nord4)
            };

            [
                ("default", default),
                ("accent", Form::new().with(s.nord8).bold()),
                ("default.error", Form::new().with(s.nord11)),
                ("accent.error", Form::new().with(s.nord11).bold()),
                ("default.warn", Form::new().with(s.nord13)),
                ("accent.warn", Form::new().with(s.nord12).bold()),
                ("default.info", Form::new().with(s.nord9)),
                ("accent.info", Form::new().with(s.nord8).bold()),
                ("default.debug", Form::new().with(s.nord3)),
                ("accent.debug", Form::new().with(s.nord15).bold()),
                ("cursor.main", Form::new().with(s.nord0).on(s.nord4)),
                ("cursor.extra", Form::new().with(s.nord0).on(s.nord7)),
                ("selection.main", Form::new().on(s.nord2)),
                ("selection.extra", Form::new().on(s.nord1)),
                ("cursor.main.indent", Form::new().with_on(s.nord4)),
                ("cursor.extra.indent", Form::new().with_on(s.nord7)),
                ("selection.main.indent", Form::new().with_on(s.nord2)),
                ("selection.extra.indent", Form::new().with_on(s.nord1)),
                ("cloak", Form::new().reset().with(s.nord3).on(s.nord0)),
                ("replace", Form::new().with(s.nord1)),
                (
                    "replace.newline.trailing",
                    Form::new().with(s.nord11).on(s.nord1),
                ),
                ("toggle.hover", Form::new().on(s.nord1)),
                ("toggle.click", Form::new().on(s.nord2)),
                ("linenum.main", Form::new().with(s.nord13)),
                ("linenum.wrapped", Form::new().with(s.nord7)),
                ("file", Form::new().with(s.nord13)),
                ("selections", Form::new().with(s.nord9)),
                ("coord", Form::new().with(s.nord12)),
                ("separator", Form::mimic("punctuation.delimiter")),
                ("mode", Form::new().with(s.nord14)),
                ("terminal.border", Form::new().with(s.nord1).on(s.nord0)),
                ("terminal.frame", Form::new().with(s.nord4).on(s.nord0)),
                ("notifs.colon", Form::new().with(s.nord3)),
                ("prompt", Form::new().with(s.nord14)),
                ("prompt.colon", Form::new().with(s.nord3)),
                ("default.StatusLine", default.on(s.nord1)),
                ("default.LogBook", default.on(s.nord1)),
                ("default.VertRule", default.with(s.nord1)),
                ("default.LineNumbers", default.with(s.nord3)),
                (
                    "matched_pair",
                    Form::new().with(s.nord12).on(s.nord2).bold(),
                ),
                ("logbook.location", Form::new().with(s.nord3)),
                (
                    "default.Buffer.current_line",
                    default.on(s.nord1).interpolate(default, 50),
                ),
                ("default.Completions", default.on(s.nord1)),
                (
                    "selected.Completions",
                    Form::new().with(s.nord0).on(s.nord3),
                ),
                ("default.WhichKey", default.with(s.nord4)),
                ("key", Form::new().with(s.nord12)),
                ("key.special", Form::new().with(s.nord7)),
                ("cursor.main.Insert", Form::new().with(s.nord0).on(s.nord15)),
                ("cursor.extra.Insert", Form::new().with(s.nord0).on(s.nord13)),
                ("param", Form::new().with(s.nord15)),
                ("param.flag", Form::new().with(s.nord8)),
                // Tree sitter Forms
                ("variable", Form::new().with(s.nord4)),
                ("variable.builtin", Form::new().with(s.nord12)),
                ("variable.member", Form::new().with(s.nord4)),
                ("constant", Form::new().with(s.nord12)),
                ("constant.builtin", Form::new().with(s.nord12)),
                ("static", Form::new().with(s.nord12).reset()),
                ("module", Form::new().with(s.nord9).italic()),
                ("label", Form::new().with(s.nord7)),
                ("string", Form::new().with(s.nord14)),
                ("string.escape", Form::new().with(s.nord13)),
                (
                    "string.special.path",
                    Form::new().with(s.nord8).underlined(),
                ),
                ("character", Form::new().with(s.nord14)),
                ("boolean", Form::new().with(s.nord12)),
                ("number", Form::new().with(s.nord15)),
                ("type", Form::new().with(s.nord7).italic()),
                ("type.builtin", Form::new().with(s.nord7).reset()),
                ("type.enum", Form::new().with(s.nord7).reset()),
                ("type.enum.variant", Form::new().with(s.nord12).italic()),
                ("attribute", Form::new().with(s.nord13)),
                ("property", Form::new().with(s.nord4)),
                ("function", Form::new().with(s.nord8).reset()),
                ("function.macro", Form::new().with(s.nord8).italic()),
                ("constructor", Form::new().with(s.nord7)),
                ("operator", Form::new().with(s.nord9)),
                ("keyword", Form::new().with(s.nord9)),
                ("punctuation.bracket", Form::new().with(s.nord3)),
                ("punctuation.delimiter", Form::new().with(s.nord4)),
                ("comment", Form::new().with(s.nord3)),
                ("comment.documentation", Form::new().with(s.nord3).bold()),
                ("markup", Form::new()),
                ("markup.strong", Form::new().with(s.nord12).bold()),
                ("markup.italic", Form::new().with(s.nord13).italic()),
                ("markup.strikethrough", Form::new().crossed_out()),
                ("markup.underline", Form::new().underlined()),
                ("markup.heading", Form::new().with(s.nord9).bold()),
                ("markup.math", Form::new().with(s.nord8)),
                ("markup.quote", Form::new().with(s.nord14).bold()),
                ("markup.environment", Form::new().with(s.nord15)),
                ("markup.environment.name", Form::new().with(s.nord9)),
                ("markup.link", Form::new().with(s.nord8).underlined()),
                ("markup.raw", Form::new().with(s.nord7)),
                ("markup.list", Form::new().with(s.nord9)),
                ("markup.list.checked", Form::new().with(s.nord14)),
                ("markup.list.unchecked", Form::new().with(s.nord3)),
                ("diff.plus", Form::new().with(s.nord14)),
                ("diff.delta", Form::new().with(s.nord8)),
                ("diff.delta.renamed", Form::new().with(s.nord13)),
                ("diff.minus", Form::new().with(s.nord11)),
                ("unresolved", Form::new().underlined().underline(s.nord11)),
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

