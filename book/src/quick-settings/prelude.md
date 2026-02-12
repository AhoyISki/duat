# The `prelude` module

At the top of your crate, you should be able to find this:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    // The stuff inside your setup...
}
```

This will import everything in the `prelude` module of `duat`. This should have
everything you will need in order to configure Duat, not including things from
other crates that you may want to import (such as plugins).

When calling `use duat::prelude::*`, most imported things will be in the form 
of modules, like this:

```rust
# use duat::prelude::*;
use duat::opts;
```

This is importing the `opts` module, as opposed to importing its items 
directly, like this:

```rust
# use duat::prelude::*;
use duat::opts::*;
```

This means that, for most options, their path is made up of a 
`{module}::{function}` combo. So the usual `setup` function should look 
something like this:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    opts::set(|opts| {
        opts.wrap_lines = true;
        opts.wrapping_cap = Some(80);
    });

    form::set("caret.main", Form::new().yellow());

    cmd::add("set-rel-lines", |pa: &mut Pass| {
        let handles: Vec<_> = context::windows()
            .handles(pa)
            .filter_map(|handle| handle.try_downcast::<LineNumbers>())
            .collect();

        for handle in handles {
            handle.write(pa).relative = true;
        }

        Ok(Some(txt!("Lines were set to [a]relative")))
    });

    map::<Insert>("jk", "<Esc>:w<Enter>");
}
```

The exceptions to this are the `map`, `alias` and `plug` functions and the 
`setup_duat!` macro. These items are imported directly.

The following chapters should give a quick overview of these items imported 
from the prelude module.

## The `opts` module

This module contains a bunch of commonly used options. It covers settings for 
the various `Widget`s of Duat, most notably the `Buffer` widget, which is where 
editing takes place.

Below are the available functions on this module, as well as their default 
values.

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    // Default options for the Buffer widget
    opts::set(|opts| {
        // Buffer options:
        opts.wrap_lines = false;
        opts.wrap_on_word = false;
        // Where to wrap, as opposed to at the rightmost edge.
        opts.wrapping_cap = None::<u32>;
        // Indent wrapped lines.
        opts.indent_wraps = true;
        opts.tabstop = 4;
        // Minimum cursor distance from the top and bottom edges.
        opts.scrolloff.x = 3;
        // Minimum cursor distance from the left and right edges.
        opts.scrolloff.y = 3;
        opts.extra_word_chars = &[];
        // Forces scrolloff at the end of a line.
        opts.force_scrolloff = false;
        
        // General settings:
        // Place the bottom widgets on the top of the screen.
        opts.footer_on_top = false;
        // Make the bottom widgets take up only one line of space.
        opts.one_line_footer = false;
        // Shows available keybindings
        opts.help_key = Some(KeyEvent::new(KeyCode::Char('?'), mode::KeyMod::CONTROL));
        
        // duatmode settings:
        // Inserts a \t instead of spaces when pressing Tab
        opts.duatmode.insert_tabs = false;
        // How to handle the Tab key
        opts.duatmode.tab_mode = opts::TabMode::VerySmart;
        // Auto indent new lines on tree-sitter Buffers
        opts.duatmode.auto_indent = true;
        // Characters that trigger a reindentation
        opts.duatmode.indent_chars = &['\n', '(', ')', '{', '}', '[', ']'];
        // Reindent when pressing 'I' in normal mode
        opts.duatmode.indent_on_capital_i = true;
        // Makes the 'f' and 't' keys set the search pattern
        opts.duatmode.f_and_t_set_search = true;
        // Bracket pairs to be considered by keys like 'm' and the 'u' object
        opts.duatmode.set_brackets([["(", ")"], ["{", "}"], ["[", "]"]]);
        
        // LineNumbers options:
        opts.line_numbers.relative = false;
        // Where to align the numbers
        opts.line_numbers.align = std::fmt::Alignment::Left;
        // Where to align the main line number
        opts.line_numbers.main_align = std::fmt::Alignment::Right;
        // Wether to show wrapped line's numbers
        opts.line_numbers.show_wraps = false;
        // Place the widget on the right, as opposed to on the left
        opts.line_numbers.on_the_right = false;
        
        // Notifications options:
        // Reformat the notifications messages
        opts.notifications.fmt(|rec| todo!("default fmt function"));
        // Which mask to use to show the messages
        opts.notifications.set_mask(|rec| todo!("error for error, info for info, etc"));
        // Which log levels will actually show up on the notifications
        opts.notifications.set_allowed_levels([
            context::Level::Error,
            context::Level::Warn,
            context::Level::Info
        ]);
        
        // WhichKey options:
        // How to format each keybinding entry on the widget
        opts.whichkey.fmt(|desc| todo!("default fmt function"));
        // Disable the widget for the given Mode
        // opts.whichkey.disable_for::<{Mode in question}>();
        // Always show the widget for the given Mode
        opts.whichkey.always_show::<User>();
        // Removes the Mode from the disable_for and always_show lists
        // opts.whichkey.show_normally::<{Mode in question}>();
        
        // LogBook options:
        // How to format each message
        opts.logs.fmt(|rec| todo!("default log fmt"));
        opts.logs.close_on_unfocus = true;
        // It can be shown via the "logs" command
        opts.logs.hidden = false;
        // Where to place it
        opts.logs.side = ui::Side::Below;
        // Is ignored when the side is Left or Right
        opts.logs.height = 8.0;
        // Is ignored when the side is Above or Below
        opts.logs.width = 50.0;
        // Wether to show the source of the message (on the default fmt)
        opts.logs.show_source = true;
    });

    // Default options for the StatusLine widget
    opts::fmt_status(|pa| {
        // If on one line footer mode:
        let mode = mode_txt();
        let param = duat_param_txt();
        status!("{Spacer}{name_txt} {mode} {sels_txt} {param} {main_txt}");
        // If on regular mode (default):
        let mode = mode_txt();
        let param = duat_param_txt();
        status!("{mode} {name_txt}{Spacer}{sels_txt} {param} {main_txt}")
    });
}
```

For more information about modification of the `StatusLine`, see the chapter on 
[modding the `StatusLine`]. For information on modding the `Notifications` and 
`LogBook` widgets, see the [`Text`] chapter


## `form`: How text is colored

In duat, the way text is styled is through `Form`s. The `Form` struct, 
alongside the `form` module, are imported by the prelude:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    let color1 = Color::new("#575279");
    let color2 = Color::new("#faf4ed");
    // Setting by Form
    form::set("punctuation.bracket", Form::new().red());
    form::set("default", Form::new().with(color1).on(color2));
    form::set("matched_pair", Form::new().blue().underlined());

    // Setting by reference
    form::set("accent.debug", Form::mimic("default.debug"));
}
```

The main function that you will use from this module is `form::set`. This 
function sets the form on the left to the value on the right. This value can be 
of two types:

- A `Form` argument will be used to color the form directly.
- A `&str` argument will "reference" the form on the right. If the form on the 
  right is altered, so will the one on the left. This reduces the need for 
  setting a ton of forms in things like colorschemes.

### How forms should be named

Every form in duat should be named like this: `[a-z0-9]+(\.[a-z0-9]+)*`. That 
way, inheritance of forms becomes very predictable, and it's much easier for 
plugin writers to depend on that feature.

There is one exception to this rule however, that being the `default` form. The 
`default` form, unlike other forms, can have `Widget` specific implementations, 
like `default.StatusLine`, which will change the `default` form _only_ on 
`StatusLine`s, and is set by default.

### Colorschemes

The other main function that you will use from this module is the 
`form::set_colorscheme` function. This function will change the colorscheme to 
a previously named one:

```rust
# mod duat_catppuccin {
#     use duat::prelude::*;
#     #[derive(Default)]
#     pub struct Catppuccin;
#     impl Plugin for Catppuccin {
#         fn plug(self, _: &Plugins) { todo!() }
#     }
# }
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    // Adds four colorschemes, "catppuccin-latte" among them.
    plug(duat_catppuccin::Catppuccin::default());

    colorscheme::set("catppuccin-latte");
}
```

### `Form` inheritance

Another aspect of duat's forms that can save a lot of typing is the concept of `Form` inheritance. In Duat, forms follow the following structure:

- If `form.subform` is unset, it will reference `form`;
- If `form.subform` is set to `Form::green()`, it won't be changed when 
  `form` changes, staying at `Form::green()`;
- If `form.subform` is set to reference `other_form`, changing 
  `other_form` will also change `form.subform`, but changing `form` 
  won't;

As a consequence of this, for example, if you were to set the `markup` form to 
something, every form with a name like `markup.*` _that isn't already set_, 
would follow the change to the `markup` form.

Additionally, if the form `f0.f1.f2` is set to something, the forms `f0` and 
`f1.f2` would also be set, although they will reference the `default` form in 
that situation, not whatever `f0.f1.f2` was set to.

**Quiz**

Given the following sequence of `form::set`s, what will each `Form` be at the 
end?

```rust
# use duat::prelude::*;
# fn test() {
form::set("parent", Form::new().green());
form::set("parent.child.granchild", Form::new().blue());
form::set("grandparent.parent.child", Form::mimic("parent.child"));
form::set("parent", Form::new().red());
# }
```

<details>
<summary>See results</summary>

- "parent": `Form::new().red()`.
- "parent.child": `Form::new().red()`.
- "parent.child.grandchild": `Form::new().blue()`.
- "grandparent.parent.child": `Form::new().red()`.

</details>

### Masks

A mask is essentially the opposite of the inheritance concept. Instead of the
longer form inheriting from the shorter forms, the shorter forms will be mapped
for longer ones.

It works like this: Say I have a `File` widget, and in it, there are instances
of the `function` form, used to highlight function identifiers. If there is a
 `function.error` form, and I tell the `File` to use the `error` mask, instead
of using the `function` form, Duat will use `function.error`.

In duat, by default there are four masks: `error`, `warning`, `info`, and
`inactive`. The first three are used primarily to show color coded
notifications. The last one is unused, but you can use it to change how
unfocused buffers should be displayed.

You can also add more masks through `form::enable_mask`. If you want to learn
more about masks and how to use them, you should check out the [masks 
chapter](masks.md)

### List of forms

Currently, in `duat`, these are the forms in use:

- `default`: Is applied to every text. Can be `Widget` dependent, like
  `default.LineNumbers`.
- `accent`: This form is used when formatting error, warning, information, or
  debug messages. Is used mostly with the `error`, `warn` and `info` [masks].
- `caret.main`: The form to use when printing the main "caret" (each cursor has
  a selection, an anchor, and a caret).
- `caret.extra`: Same as `caret.main`, but for cursors other than the main one.
- `selection.main`: Color to be used on the main selection.
- `selection.extra`: Color to be used on extra selections.
- `cloak`: This form is supposed to be a common "get rid of all forms
  temporarily" form. You should use this when you want to, for example, remove
  all visible color from the screen, in order to highlight something.  Some
  plugins make use of this form, like `duat-hop` and `duat-sneak`, which are
  recreations of some neovim plugins.
- `alias`: Is used on aliases, see the [map and alias] 
  chapter for more information.
- `matched_pair`: Isn't technically part of duat, but it's part of a default 
  plugin.

Some other forms are used by specific `Widgets`. Remember, the default form for
every `Widget` will always be `default.{WidgetName}`:

- `LineNumbers`:
  - `linenum.main`: The form to be used on the main line's number.
  - `linenum.wrapped`: The form to be used on wrapped lines.
  - `linenum.wrapped.main`: Same, but for the main line, inherits from 
    `linenum.wrapped` by default.

  Do note that you can set the form of the remaining lines by setting 
  `default.LineNumbers`. And due to [form inheritance], setting `linenum` will 
  set `linenum.wrapped`, `linenum.main` and `linenum.wrapped.main`.

- `StatusLine`:
  - `buffer`, `buffer.new`, `buffer.unsaved`, `buffer.new.scratch`: Are all used by 
    [`name_txt`], which shows the `File`'s name and some other info.
  - `mode`: Is used by [`mode_txt`].
  - `coord` and `separator`: Are used by [`main_txt`].
  - `selections`: Is used by [`selections_txt`].
  - `key` and `key.special`: Are used by [`cur_map_txt`].

- `Completions`:
  - `selected.Completions` changes the selected entry's form. 

- `Notifications`:
  - `notifs.target`: The form for the "target" of the notification.
  - `notifs.colon`: The form used by the `':'` that follows the target.
  
  Since the `Notifications` widget makes heavy use of masks, you can also set 
  `notifs.target.error`, if you want a different target color only when error 
  messages are sent, for example.

- `PromptLine`:
  - `prompt`: For the prompt on the prompt line.
  - `prompt.colon`: For the `':'` that follows it.
  - `caller.info` and `caller.error`: For the caller, if it exists or not, 
  respectively.
  - `parameter.info` and `parameter.error`: For parameters, if they fit or not, 
  respectively.
  - `regex.literal`, `regex.operator.(flags|dot|repetition|alternation)`, 
  `regex.class.(unicode|perl|bracketed)`, `regex.bracket.(class|group)`: A bunch 
  of forms used for highlighting regex searches.

- `LogBook`:
  - `log_book.(error|warn|info|debug)`: For the types of messages.
  - `log_book.colon`: For the `':'` that follows them.
  - `log_book.target`: For the "target" of the message.
  - `log_book.bracket`: For the `(`s surrounding the target.

- `VertRule`:
  - `rule.upper` and `rule.lower`: The forms to use above and below the main 
    line.

And finally, there are also all the forms used by `duat-treesitter`. Since the 
queries were taken from nvim-treesitter, the form names follow the same patters 
as those from neovim. Remember, setting `form` will automatically set 
`form.child` and `form.child.grandchild`, and so forth , _unless_ that form is 
already set to something:

| Form name                     | Purpose                                   
|-------------------------------|---------
| `variable`                    | various variable names                    
| `variable.builtin`            | built-in variable names (e.g. this, self)
| `variable.parameter`          | parameters of a function
| `variable.parameter.builtin`  | special parameters (e.g. _, it)
| `variable.member`             | object and struct fields
| `constant`                    | constant identifiers
| `constant.builtin`            | built-in constant values
| `constant.macro`              | constants defined by the preprocessor
| `module`                      | modules or namespaces
| `module.builtin`              | built-in modules or namespaces
| `label`                       | GOTO and other labels (e.g. label: in C), including heredoc labels
| `string`                      | string literals
| `string.documentation`        | string documenting code (e.g. Python docstrings)
| `string.regexp`               | regular expressions
| `string.escape`               | escape sequences
| `string.special`              | other special strings (e.g. dates)
| `string.special.symbol`       | symbols or atoms
| `string.special.path`         | filenames
| `string.special.url`          | URIs (e.g. hyperlinks)
| `character`                   | character literals
| `character.special`           | special characters (e.g. wildcards)
| `boolean`                     | boolean literals
| `number`                      | numeric literals
| `number.float`                | floating-point number literals
| `type`                        | type or class definitions and annotations
| `type.builtin`                | built-in types
| `type.definition`             | identifiers in type definitions (e.g. typedef \<type\> \<identifier\> in C)
| `attribute`                   | attribute annotations (e.g. Python decorators, Rust lifetimes)
| `attribute.builtin`           | builtin annotations (e.g. @property in Python)
| `property`                    | the key in key/value pairs
| `function`                    | function definitions
| `function.builtin`            | built-in functions
| `function.call`               | function calls
| `function.macro`              | preprocessor macros
| `function.method`             | method definitions
| `function.method.call`        | method calls
| `constructor`                 | constructor calls and definitions
| `operator`                    | symbolic operators (e.g. +, *)
| `keyword`                     | keywords not fitting into specific categories
| `keyword.coroutine`           | keywords related to coroutines (e.g. go in Go, async/await in Python)
| `keyword.function`            | keywords that define a function (e.g. func in Go, def in Python)
| `keyword.operator`            | operators that are English words (e.g. and, or)
| `keyword.import`              | keywords for including or exporting modules (e.g. import, from in Python)
| `keyword.type`                | keywords describing namespaces and composite types (e.g. struct, enum)
| `keyword.modifier`            | keywords modifying other constructs (e.g. const, static, public)
| `keyword.repeat`              | keywords related to loops (e.g. for, while)
| `keyword.return`              | keywords like return and yield
| `keyword.debug`               | keywords related to debugging
| `keyword.exception`           | keywords related to exceptions (e.g. throw, catch)
| `keyword.conditional`         | keywords related to conditionals (e.g. if, else)
| `keyword.conditional.ternary` ternary | operator (e.g. ?, :)
| `keyword.directive`           | various preprocessor directives and shebangs
| `keyword.directive.define`    | preprocessor definition directives
| `punctuation.delimiter`       | delimiters (e.g. ;, ., ,)
| `punctuation.bracket`         | brackets (e.g. (), {}, [])
| `punctuation.special`         | special symbols (e.g. {} in string interpolation)
| `comment`                     | line and block comments
| `comment.documentation`       | comments documenting code
| `comment.error`               | error-type comments (e.g. ERROR, FIXME, DEPRECATED)
| `comment.warning`             | warning-type comments (e.g. WARNING, FIX, HACK)
| `comment.todo`                | todo-type comments (e.g. TODO, WIP)
| `comment.note`                | note-type comments (e.g. NOTE, INFO, XXX)
| `markup.strong`               | bold text
| `markup.italic`               | italic text
| `markup.strikethrough`        | struck-through text
| `markup.underline`            | underlined text (only for literal underline markup!)
| `markup.heading`              | headings, titles (including markers)
| `markup.heading.1`            | top-level heading
| `markup.heading.2`            | section heading
| `markup.heading.3`            | subsection heading
| `markup.heading.4`            | and so on
| `markup.heading.5`            | and so forth
| `markup.heading.6`            | six levels ought to be enough for anybody
| `markup.quote`                | block quotes
| `markup.math`                 | math environments (e.g. $ ... $ in LaTeX)
| `markup.link`                 | text references, footnotes, citations, etc.
| `markup.link.label`           | link, reference descriptions
| `markup.link.url`             | URL-style links
| `markup.raw`                  | literal or verbatim text (e.g. inline code)
| `markup.raw.block`            | literal or verbatim text as a stand-alone block
| `markup.list`                 | list markers
| `markup.list.checked`         | checked todo-style list markers
| `markup.list.unchecked`       | unchecked todo-style list markers
| `diff.plus`                   | added text (for diff files)
| `diff.minus`                  | deleted text (for diff files)
| `diff.delta`                  | changed text (for diff files)
| `tag`                         | XML-style tag names (e.g. in XML, HTML, etc.)
| `tag.builtin`                 | builtin tag names (e.g. HTML5 tags)
| `tag.attribute`               | XML-style tag attributes
| `tag.delimiter`               | XML-style tag delimiters

## `map` and `alias`: modifying keys

In Duat, mapping works somewhat like Vim/neovim, but not quite. This is how it 
works:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    map::<User>("f", "<Esc><A-j>|fold -s -w 80<Enter>");
    alias::<Insert>("jk", "<Esc>");
    alias::<Prompt>("jk", "<Esc>");
}
```

In mapping, there are two main functions: `map` and `alias`. `map` will take 
the keys as is, and if the sequence matches, outputs the remapping, otherwise, 
outputs the keys that were sent. `alias` does the same thing, but it also 
""prints"" the sequence that was sent, making it _look_ like you are typing 
real text. Here's a showcase of the difference:

<p align="center"><img src="../../../../assets/map-alias.gif"/></p>

Both of these functions also take a _required_ type argument. This type 
argument is the `Mode` where this mapping will take place. So in the first 
example, in `Insert` and `Prompt` mode, if you type `jk`, the `j` will show up 
as ""text"", but when you press `k`, you will immediately exit to `Normal` 
`Mode`.

`User` is a standard `Mode` in Duat. It is meant to be a "hub" for Plugin 
writers to put default mappings on. Sort of like the leader key in Vim/Neovim. 
On `Normal` mode, by default, this mode is entered by pressing the space bar. 
While you _can_ change that like this:


```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    map::<Normal>(" ", "");
    // In rust, you have to escap a backslash
    map::<Normal>(r"\", " ");
}
```

You _should_ prefer doing this:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    map::<Normal>(" ", "");
    map::<Normal>("\\", User);
}
```

In this case, instead of putting a sequence of keys to replace the mapped ones, 
I placed the mode directly. 

This is allowed in order to support custom `Mode`s. That way, you can just 
place the `Mode` as the second argument, and the mapping will switch modes 
instead of sending keys. This also works with `alias`es.

> [!NOTE]
>
> In this case, since `User` is a struct with no fields, I could just put 
> `User` as the second argument, which acts as a constructor. But in most other 
> `Mode`s, you're gonna have to write something like `Insert::new()` as the 
> argument instead.

### List of keys and modifiers

Syntax wise, the keys are very similar to vim style. Regular characters are 
placed normally, special keys are enclosed in `<`,`>` pairs, and modified keys 
are enclosed in these pairs, with a `<{mod}-{key}>` syntax. Examples:

- `abc<C-Up><F12>`.
- `<A-Enter><AS-Left>`.
- `づあっと`.

This is the list of recognized special keys:

- `<Enter>`,
- `<Tab>`,
- `<Backspace>`,
- `<Del>`,
- `<Esc>`,
- `<Up>`,
- `<Down>`,
- `<Left>`,
- `<Right>`,
- `<PageU>`,
- `<PageD>`,
- `<Home>`,
- `<End>`,
- `<Ins>`,
- `<F{1-12}>`,

And these are the allowed modifiers, which, as you can see above, can be 
composed together:

- `C => Control`,
- `A => Alt`,
- `S => Shift`,
- `M => Meta`,
- `super => Super`,
- `hyper => Hyper`,

## `cursor`: How to print cursors

The `cursor` module is like the `print` module, in that it provides some basic 
options on how cursors should be printed. These options primarily concern if 
cursors should be printed as "real cursors" (The blinking kind, that can turn 
into a bar and stuff), or as just `Form`s.

- `cursor::set_main` will set the "shape" of the main cursor. This takes a 
  `CursorShape` argument, and lets you set its shape to a vertical bar, a 
  horizontal bar, and make it blink.
- `cursor::set_extra` is the same but for extra cursors. Do note that this may 
  not work on some `Ui`s, mainly terminals, which only allow for one cursor at a 
  time.
- `cursor::unset_main` and `cursor::unset_extra`: Disables cursor shapes for 
  every type of cursor, replacing them with a `Form`, which will be `caret.main` 
  and `caret.extra`, respectively
- `cursor::unset`: The same as calling `unset_main` and `unset_extra`.

[modding the `StatusLine`]: ../../scripting-duat/mod-status.md
[`Text`]: ../../scripting-duat/text/chapter.md
[masks]: ../../scripting-duat/masks.md
[map and alias]: prelude/map-and-alias.md
[form inheritance]: form-module.md#form-inheritance
[`name_txt`]: https://docs.rs/duat/latest/duat/state/fn.name_txt.html
[`mode_txt`]: https://docs.rs/duat/latest/duat/state/fn.mode_txt.html
[`main_txt`]: https://docs.rs/duat/latest/duat/state/fn.main_txt.html
[`selections_txt`]: https://docs.rs/duat/latest/duat/state/fn.selections_txt.html
[`cur_map_txt`]: https://docs.rs/duat/latest/duat/state/fn.cur_map_txt.html
