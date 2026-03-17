# Key bindings

Duat's default mode is one heavily inspired by Kakoune. This means that it is 
modal, and follows "object action" semantics, unlike (neo)vim, which follows 
"action object" semantics.

Duat is extremely capable in the multi-cursor department, so Duat's default 
mode makes heavy use of multiple cursors. Every action will be done on every 
cursor, unless specified otherwise.

Given that, here are the bindings for `duatmode`:

# Keymaps

On every key, if the action involves selections, unless stated
otherwise, it will take place in all selections.

## `Insert` mode

Insert mode is the text editing mode of Duat, much like Vim's. It
is also entered via various keys in `Normal` mode.

On insert mode, keys are sent normally, with the exception of the
following:

`<Tab>` and `<S-Tab>` will do different things depending on your
[tab mode].

`<C-n>` and `<C-p>` go to the next and previous completion
entries.

`<Esc>` exits insert mode, returning to `Normal` mode`.

## Normal mode

The keys in `normal` mode follow the following patterns:

- `word` characters follow Duat's [word chars], which are normally
  used to define where lines wrap.
- `WORD` characters are just any non-whitespace character.

In normal mode, another factor is the `param` value, which is
incremented by typing digits.  For example, if you type
`10<Right>`, the selections will move 10 times to the right.

<details>
<summary><b>Object selection</b></summary>

In Duat, there are various types of "objects" for selection. These
get used on `Normal` mode key sequences, most notably on `<A-i>`
and `<A-a>`. Each of them defines something to be selected:

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

`w`, `<A-w>`
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
Move down to the next wrapped line (i.c vim's `gj`).

`k`\
Move up.

`<Up>`\
Move up to the previous wrapped line (i.e. vim's `gk`).

`l`, `<Right>`\
Move right. Wraps around lines.

`H`, `<S-Left>`, `J`, `<S-Down>`, `K`, `<S-Up>`, `L`, `<S-Right>`\
Same as the previous characters, but extends the selection

`w`\
Selects the `word` and following space ahead of the selection.

`b`\
Selects the `word` followed by spaces behind the selection.

`e`\
Selects to the end of the next `word` ahead of the selection.

`<(W|B|E)>`\
The same as `(w|b|e)`, but extends the selection.

`<A-(w|b|e)>`\
The same as `(w|b|e)`, but over a `WORD`.

`<A-(W|B|E)>`\
The same as `<A-(w|b|e)>`, but extends the selection.

`f{char}`\
Selects to the next occurrence of the `{char}`.

`t{char}`\
Selects until the next occurrence of the `{char}`.

`<(F|T)>{char}`\
Same as `(f|t)`, but extends the selection.

`<A-(f|t)>{char}`\
Same as `(f|t)`, but in the opposite direction.

`<A-(F|T)>{char}`\
Same as `<A-(f|t)>`, but in extends the selection.

`{param}g`\
Goes to the `{param}`th line. If param was not set, enters `go to`
mode.

`{param}G`\
Extends to the `{param}`th line. If param was not set, enters `go
to` mode, and actions will extend.

`x`\
Extends selection to encompass full lines.

`%`\
Selects the whole buffer.

`<A-h>`, `<Home>`\
Selects to the start of the line.

`<A-l>`, `<End>`\
Selects until the end of the line.

`<A-H>`, `<S-Home>`, `<A-L>`, `<S-End>`\
Same as the previous two, but extends the selection.

`m`\
Selects to the next pair of matching brackets.

`<A-m>`\
Selects the previous pair of matching brackets.

`M`, `<A-M>`\
Same as the previous two, but extends the selection.

`<A-u>`\
Returns to the previous state for the selections.

`<A-U>`\
Goes to the next state for the selections.

`;`\
Reduces selections to just the [caret].

`<A-;>`\
Flips the [caret] and [anchor] of selectionss around.

`,`\
Removes extra selections.

`C`\
Creates a selection on the column below the last one.

`<A-C>`\
Creates a selection on the column above the first one.

`<A-:>`\
Places the [caret] ahead of the [anchor] in all selections.

`<A-s>`\
Divides selection into multiple selections, one per line.

`<A-S>`\
Splits into two selections, one at each end of the selection.

`<A-_>`\
Merges all adjacent selections.

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

`c`\
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

`<A-c>`\
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
Changes mode to [`PipeSelections`], letting you pipe each
selection to an external program.

</details>

<details>
<summary><b>Search</b></summary>

The searching in this plugin is done through the [`IncSearch`]
[`Mode`] from Duat, with some [`IncSearcher`]s defined in this
crate. This means that search will be done incrementally over a
Regex pattern.

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

## goto mode

Goto mode is accessed by the `g` and `G` keys in `normal` mode.
It serves a s a way to quickly move places, be it on selections or to other 
`Buffer`s and such.

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
used by `Plugin`s for key maps. For example, you could map `l`
on `User` mode to do LSP related actions.

Other "monolithic modes" (Vim, Helix, Emacs, etc) should make use
of this `User` mode for the same purpose. Think of it like the
leader key in (neo)vim.

To enter `User` mode, you type `<Space>` in `Normal` mode.
