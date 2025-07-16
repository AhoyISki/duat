# List of forms

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

Some other forms are used by specific `Widgets`

- `LineNumbers`:
  - `linenum.main`: The form to be used on the main line's number.
  - `linenum.wrapped`: The form to be used on wrapped lines.
  - `linenum.wrapped.main`: Same, but for the main line, inherits from 
    `linenum.wrapped` by default.

  Do note that you can set the form of the remaining lines by setting 
  `default.LineNumbers`. And due to [form inheritance], setting `linenum` will 
  set all three forms.

- `StatusLine`:
  - `file`, `file.new`, `file.unsaved`, `file.new.scratch`: Are all used by 
    [`file_fmt`], which shows the `File`'s name and some other info.
  - `mode`: Is used by [`mode_fmt`].
  - `coord` and `separator`: Are used by [`main_fmt`].
  - `selections`: Is used by [`selections_fmt`].
  - `key` and `key.special`: Are used by [`cur_map_fmt`].

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

And finally, there are also all the forms used by `duat-treesitter`. Since the queries were taken from nvim-treesitter, the form names follow the same patters as those from neovim:

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
| `type.definition`             | identifiers in type definitions (e.g. typedef <type> <identifier> in C)
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

[masks]: ../../scripting-duat/masks.md
[map and alias]: prelude/map-and-alias.md
[form inheritance]: form-module.md#form-inheritance
[`file_fmt`]: https://docs.rs/duat/latest/duat/state/fn.file_fmt.html
[`mode_fmt`]: https://docs.rs/duat/latest/duat/state/fn.mode_fmt.html
[`main_fmt`]: https://docs.rs/duat/latest/duat/state/fn.main_fmt.html
[`selections_fmt`]: https://docs.rs/duat/latest/duat/state/fn.selections_fmt.html
[`cur_map_fmt`]: https://docs.rs/duat/latest/duat/state/fn.cur_map_fmt.html
