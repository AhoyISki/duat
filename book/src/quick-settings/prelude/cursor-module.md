# `cursor`: How to print cursors

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
