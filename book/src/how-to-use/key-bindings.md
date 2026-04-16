# Key bindings

Duat's default mode is one heavily inspired by Kakoune. This means that it is
modal, and follows "object action" semantics, unlike (neo)vim, which follows
"action object" semantics.

Duat is extremely capable in the multi-cursor department, so Duat's default
mode makes heavy use of multiple cursors. Every action will be done on every
cursor, unless specified otherwise.

Given that, here are the bindings for `duatmode`:

## Insert mode

Insert mode is the text editing mode of Duat, much like Vim’s. It
is also entered via various keys in `Normal` mode.

On insert mode, keys are sent normally, with the exception of the
following:

`<Tab>` and `<s-Tab>` will do different things depending on your
[tab mode].

`<s-n>` and `<s-p>` go to the next and previous completion
entries.

`<Esc>` exits insert mode, returning to `Normal` mode\`.

## Normal mode

The keys in `normal` mode follow the following patterns:

* `word` characters follow Duat’s [word chars], which are normally
  used to define where lines wrap.
* `WORD` characters are just any non-whitespace character.

In normal mode, another factor is the `param` value, which is
incremented by typing digits.  For example, if you type
`10<Right>`, the selections will move 10 times to the right.

<details>
<summary><b>Object selection</b></summary>

In Duat, there are various types of “objects” for selection. These
get used on `Normal` mode key sequences, most notably on `'` and
`"`. Each of them defines something to be selected:

`b`, `(`, `)`\
Inside/around parenthesis.

`B`, `{`, `}`\
Inside/around curly braces.

`r`, `[`, `]`\
Inside/around brackets.

`a`, `<`, `>`\
Inside/around angle brackets.

`q`, `'`\
Inside/around single quotes.

`Q`, `"`\
Inside/around double quotes.

`g`, `` ` ``\
Inside/around graves.

`w`, `e`
Inside/around `word`s and `WORD`s.

`s`\
Inside/around sentences.

`p`\
Inside/around paragraphs.

`i`\
Inside/around lines of equal or greater indentation.

` `\
Inside/around whitespace.

</details>

<details>
<summary><b>Selection keys</b></summary>

`h`, `<Left>`\
Move left. Wraps around lines.

`j`\
Move down

`<Down>`\
Move down to the next wrapped line (i.s vim’s `gj`).

`k`\
Move up.

`<Up>`\
Move up to the previous wrapped line (i.e. vim’s `gk`).

`l`, `<Right>`\
Move right. Wraps around lines.

`H`, `<s-Left>`, `J`, `<s-Down>`, `K`, `<s-Up>`, `L`, `<s-Right>`\
Same as the previous characters, but extends the selection

`w`\
Selects the `word`/spaces ahead of the selection.

`e`\
Selects the `WORD`/spaces ahead of the selection.

`b`\
Selects the `word`/spaces behind the selection.

`v`\
Selects the `WORD`/spaces behind the selection

`<(W|E|B|V)>`\
The same as `(w|b|e|v)`, but extends the selection.

`f{char}`\
Selects to the next occurrence of the `{char}`.

`t{char}`\
Selects until the next occurrence of the `{char}`.

`<(F|T)>{char}`\
Same as `(f|t)`, but extends the selection.

`<a-(f|t)>{char}`\
Same as `(f|t)`, but in the opposite direction.

`<a-(F|T)>{char}`\
Same as `<a-(f|t)>`, but in extends the selection.

`{param}g`\
Goes to the `{param}`th line. If param was not set, enters `go to`
mode.

`{param}G`\
Extends to the `{param}`th line. If param was not set, enters `go to` mode, and actions will extend.

`x`\
Extends selection to encompass full lines.

`%`\
Selects the whole buffer.

`<a-h>`, `<Home>`\
Selects to the start of the line.

`<a-l>`, `<End>`\
Selects until the end of the line.

`<a-H>`, `<s-Home>`, `<a-L>`, `<s-End>`\
Same as the previous two, but extends the selection.

`"`\
Select around object

`[`,`]`\
Select around start/end of object

`{`,`}`\
Extend around start/end of object

`'`\
Select inside object

`<a-[>`,`<a-]>`\
Select inside start/end of object

`<a-{>`,`<a-}>`\
Extend inside start/end of object

`m`\
Selects to the next pair of matching brackets.

`<a-m>`\
Selects the previous pair of matching brackets.

`M`, `<a-M>`\
Same as the previous two, but extends the selection.

`<a-u>`\
Returns to the previous state for the selections.

`<a-U>`\
Goes to the next state for the selections.

`;`\
Reduces selections to just the cursor.

`<a-;>`\
Flips the cursor and anchor of selectionss around.

`,`\
Removes extra selections.

`C`\
Creates a selection on the column below the last one.

`<a-C>`\
Creates a selection on the column above the first one.

`<a-:>`\
Places the cursor ahead of the anchor in all selections.

`X`\
Divides selection into multiple selections, one per line.

`D`\
Splits into two selections, one at each end of the selection.

`<a-_>`\
Merges all adjacent selections.

`<a-q>`\
Replays the recorded macro.

`<a-Q>`\
Starts/stops recording a macro.

`<c-r>`\
Reloads the configuration crate.

</details>

<details>
<summary><b>Text modification</b></summary>

`i`\
Enter `insert` mode before selections.

`a`\
Enter `insert` mode after selection.

`I`\
Moves to the beginning of the line (after indent) and enters
`insert` mode.

`A`\
Moves to the end of the line and enters `insert` mode.

`y`\
Yanks selections.

`d`\
Deletes and yanks the selections.

`s`\
Deletes, yanks, and enter `insert` mode.

`p`\
Pastes after end of each selection (multi line selections are
placed on the next line).

`P`\
Pastes at the start of each selection (multi line pastes are
placed on the previous line).

`R`\
Replaces with the pasted text, without yanking.

`<A-d>`\
Deletes selections without yanking.

`<A-s>`\
Deletes selections without yanking, then enters `insert` mode.

`o`\
Creates a new line below and enters `insert` mode in it.

`O`\
Creates a new line above and enters `insert` mode in it.

`<A-(o|O)>`\
Same as `(o|O)`, but just adds the new line without moving.

`r{key}`\
Replaces each character with `{key}`

`u`\
[Undoes] the last `moment`

`U`\
[Redoes] the next `moment`

`>`\
Adds indentation to the selected lines.

`<`\
Removes indentation to the selected lines.

`<A-j>`\
Merges selected lines.

`` ` ``\
Changes selection to lowercase.

`~`\
Changes selection to uppercase.

``<A-`>``\
Swaps the case of each character.

`<A-)>`\
Rotates each selection's content forwards.

`<A-(>`\
Rotates each selection's content backwards.

`|`\
Pipes the selections to an external command, replacing
their content with the output.

`<c-r>`\
Reload Duat's config.

</details>

<details>
<summary><b>Search</b></summary>

Searching in Duat is incremental in nature. It takes a regex
pattern and applies it in real time, which will yield multiple
selections, depending on which key you typed to start searching:

`/`\
Searches forward for the next pattern.

`<A-/>`\
Searches backwards for the previous pattern.

`?`\
Extends forward for the next pattern.

`<A-?>`\
Extends backwards for the previous pattern.

`s`\
Selects the pattern from within current selections.

`S`\
Splits current selections by the pattern.

`<A-k>`\
Keeps only the selections that match the pattern.

`<A-K>`\
Keeps only the selections that _don't_ match the pattern.

`n`\
Go to next match for pattern.

`N`\
Create a new cursor on the next match for pattern.

`<A-n>`\
Go to previous match for pattern.

`<A-N>`\
Create a new cursor on the previous match for pattern.

`*`\
Makes the main selection the searching pattern.

</details>

### goto mode

Goto mode is accessed by the `g` and `G` keys in `normal` mode.
It serves a s a way to quickly move places, be it on selections or to other
`Buffer`s and such.

Note that if these keys are preceded by a count, like `10g`, you
will instead immediately go to the 10th line, instead of goto mode.

<details>
<summary><b>Key bindings</b></summary>

`goto` mode is entered with the `g` or `G` keys in `normal` mode.

On every key that selects, `G` will have the same behavior, but
extending the selection instead.

`h`\
Move to the beginning of the line (before indents, column 0).

`l`\
Go to the end of the line.

`i`\
Go to the beginning of the line, after indents.

`g`,`k`\
Go to the first line.

`j`\
Go to the last line.

`a`\
Go to the last buffer. Repeating will return to this buffer

`n`\
Go to the next buffer (includes other windows).

`N`\
Go to the previous buffer (includes other windows).

</details>

## User mode

In Duat, `User` mode is a "generalized mode", which should be
used by `Plugin`s for key maps. As an example, [`duat-sneak`]
makes use of this mode to enter `Sneak` mode.

Other "monolithic modes" (Vim, Helix, Emacs, etc) should make use
of this `User` mode for the same purpose. Think of it like the
leader key in (neo)vim.

To enter `User` mode, you type `<Space>` in `Normal` mode.

<details>
<summary><b>Key bindings</b></summary>

`l`\
Enter LSP mode, giving you access to a bunch of LSP related actions.

`L`\
Enter the logs, letting you see the history of notifications to
Duat.
</details>

### LSP mode

<details>
<summary<b>Key bindings</b></summary>

`f`\
Format the current `Buffer`.

[tab mode]: https://docs.rs/duat/latest/duat/prelude/enum.TabMode.html
[word chars]: https://docs.rs/duat/latest/duat/opts/struct.Opts.html#structfield.extra_word_chars
[`duat-sneak`]: https://github.com/AhoyISki/duat-sneak
