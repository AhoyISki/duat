# Builtin commands

In Duat, you can run commands by pressing the `:` key. This will focus on the 
`PromptLine` `Widget`, where you can run commands.

You can press `<Up>` and `<Down>` to navigate the history of commands, and you 
can also press `<Tab>` and `<S-Tab>` to scroll through the commands.

Here's the list of commands:

Writing/quitting:

- `write`, `w` writes the current `Buffer`. If there's an path argument, saves 
  to that path.
- `quit`, `q` quits the current `Buffer`, fails if it isn't written. If there's 
  an argument, quits that `Buffer` instead.
- `quit!`, `q!` quits the current `Buffer`, even if it isn't written. If 
  there's an argument, quits that `Buffer` instead.
- `write-quit`, `wq` writes and quits the current `Buffer`. If there's an 
  argument, quits that `Buffer` instead.
- `write-all`, `wa` writes all `Buffer`s.
- `quit-all`, `qa` tries to quit all `Buffer`s, failing if some aren't written.
- `quit-all!`, `qa!` quits all `Buffer`s, even if they aren't written.
- `write-all-quit`, `waq` writes to all `Buffer`s and quits.
- `write-all-quit!`, `waq!` writes to all `Buffer`s and quits, even if writing 
  fails for some reason.

Switching `Buffer`s

- `edit`, `e` opens a new `Buffer` on the current window.
- `open`, `o` opens a new `Buffer` on another window.
- `buffer`, `b` switches to another `Buffer`.
- `next-buffer` switches to the next `Buffer` opened.
- `prev-buffer` switches to the previous `Buffer` opened.
- `last-buffer` switches to the previously focused `Buffer`.
- `swap` Swaps the positions of the current `Buffer` and another. If there are 
  two arguments, swaps those two buffers instead.

Other

- `set-form` takes in a name and 0 to 3 colors (`##rrggbb`, `rgb r g b` or `hsl 
h s l`) and sets that name's `fg`, `bg` and `ul` colors. More settings coming 
in the future.
- `colorscheme` sets the colorscheme.
- `alias` Aliases a word to a command call.


