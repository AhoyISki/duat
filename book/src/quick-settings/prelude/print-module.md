# `print`: How duat prints files

The print module has a bunch of simple functions to change how duat should 
print `File` widgets:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    print::dont_wrap();
    print::indent_wraps(true);
    print::tabstop(4);
    print::scrolloff(3, 3);
    print::word_chars!("A-Za-z0-9_-_");
    print::new_line(' ');
}
```

These are the default options (for `File`s). One thing to note about this 
module, is that it is _for `File`s_ and `File`s _only_. That is, nothing in 
here will affect other widgets, like `LineNumbers` or `StatusLine`. If you want 
to modify those, you can head over to the [`hook`](hook.md) chapter.

The functions in this module will affect all files unconditionally, if you want 
to do that on a `File` by `File` basis, again, see the [`hook`](hook.md) 
chapter.

This is the current list of options in this module:

- `print::dont_wrap`: The default, don't wrap text at all;
- `print::wrap_on_edge`: Wraps the text on the edge of its area;
- `print::wrap_at`: Wraps at a distance from the left edge, can go over   the 
  right edge;
- `print::wrap_on_word`: Wraps on word termination, instead of any character;
- `print::indent_wraps`: Copies the indentation on wrapped lines, can be used 
  with the other options;
- `print::tabstop`: The width of tabs, is also used to determine how many 
  spaces a  `\t` should add. Is 4 by default;
- `print::new_line`: What character to print when printing `'\n'`s. Is `' '` by 
  default;
- `print::trailing_new_line`: What character to print when printing `'\n'`s 
  that are preceeded by spaces. Is not set by default;

## The `print::word_chars!` macro

The `print::word_chars!` macro determines which characters should be part of a 
word. This is used by `wrap_on_words` and tends to also be used to modify text 
(in the `<Ctrl-Backspace>` key in most editors, for example).

This macro is evaluated at compile time, so you don't have to worry about it 
being correct or not, and the syntax is similar to regex, but only with ranges, 
like this:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    print::word_chars!("A-Za-z0-9---_-_");
}
```

In this case, every sequence of lowercase and capital letters, numbers, dashes 
and underscores would be considered part of a word.
