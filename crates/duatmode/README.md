# duatmode ![License: GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue) [![duatmode on crates.io](https://img.shields.io/crates/v/duatmode)](https://crates.io/crates/duatmode) [![duatmode on docs.rs](https://docs.rs/duatmode/badge.svg)](https://docs.rs/duatmode) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duatmode)

`duatmode` is the default mode for the Duat text editor. It is
based on [kakoune][__link0]’s keybindings, with some alterations of my own.

This plugin is included in Duat by default, as it is considered
part of it’s identity. Given that, the options aren’t set by this
plugin’s [`Plugin`][__link1], but by [`duatmode::opts`][__link2], which is exported
in duat’s [`opts`][__link3] module by default

## Keymaps

On every key, if the action involves selections, unless stated
otherwise, it will take place in all selections.

### `Insert` mode

Insert mode is the text editing mode of Duat, much like Vim’s. It
is also entered via various keys in `Normal` mode.

On insert mode, keys are sent normally, with the exception of the
following:

`<Tab>` and `<s-Tab>` will do different things depending on your
[tab mode][__link4].

`<c-n>` and `<c-p>` go to the next and previous completion
entries.

`<Esc>` exits insert mode, returning to `Normal` mode\`.

### `Normal` mode

The keys in `normal` mode follow the following patterns:

* `word` characters follow Duat’s [word chars][__link5], which are normally
  used to define where lines wrap.
* `WORD` characters are just any non-whitespace character.

In normal mode, another factor is the `param` value, which is
incremented by typing digits.  For example, if you type
`10<Right>`, the selections will move 10 times to the right.

<details>
<summary>

#### Object selection

</summary>

In Duat, there are various types of “objects” for selection. These
get used on `Normal` mode key sequences, most notably on `'` and
`"`. Each of them defines something to be selected:

`b`, `(`, `)`  
Inside/around parenthesis.

`B`, `{`, `}`  
Inside/around curly braces.

`r`, `[`, `]`  
Inside/around brackets.

`a`, `<`, `>`  
Inside/around angle brackets.

`q`, `'`  
Inside/around single quotes.

`Q`, `"`  
Inside/around double quotes.

`g`, `` ` ``  
Inside/around graves.

`w`, `e`
Inside/around `word`s and `WORD`s.

`s`  
Inside/around sentences.

`p`  
Inside/around paragraphs.

`i`  
Inside/around lines of equal or greater indentation.

` `  
Inside/around whitespace.

</details>

<details>
<summary>

### Selection keys

</summary>

`h`, `<Left>`  
Move left. Wraps around lines.

`j`  
Move down

`<Down>`  
Move down to the next wrapped line (i.c vim’s `gj`).

`k`  
Move up.

`<Up>`  
Move up to the previous wrapped line (i.e. vim’s `gk`).

`l`, `<Right>`  
Move right. Wraps around lines.

`H`, `<s-Left>`, `J`, `<s-Down>`, `K`, `<s-Up>`, `L`, `<s-Right>`  
Same as the previous characters, but extends the selection

`w`  
Selects the `word`/spaces ahead of the selection.

`e`  
Selects the `WORD`/spaces ahead of the selection.

`b`  
Selects the `word`/spaces behind the selection.

`v`  
Selects the `WORD`/spaces behind the selection

`<(W|E|B|V)>`  
The same as `(w|b|e|v)`, but extends the selection.

`f{char}`  
Selects to the next occurrence of the `{char}`.

`t{char}`  
Selects until the next occurrence of the `{char}`.

`<(F|T)>{char}`  
Same as `(f|t)`, but extends the selection.

`<a-(f|t)>{char}`  
Same as `(f|t)`, but in the opposite direction.

`<a-(F|T)>{char}`  
Same as `<a-(f|t)>`, but in extends the selection.

`{param}g`  
Goes to the `{param}`th line. If param was not set, enters `go to`
mode.

`{param}G`  
Extends to the `{param}`th line. If param was not set, enters `go to` mode, and actions will extend.

`x`  
Extends selection to encompass full lines.

`%`  
Selects the whole buffer.

`<a-h>`, `<Home>`  
Selects to the start of the line.

`<a-l>`, `<End>`  
Selects until the end of the line.

`<a-H>`, `<s-Home>`, `<a-L>`, `<s-End>`  
Same as the previous two, but extends the selection.

`"`  
Select around object

`[`,`]`  
Select around start/end of object

`{`,`}`  
Extend around start/end of object

`'`  
Select inside object

`<a-[>`,`<a-]>`  
Select inside start/end of object

`<a-{>`,`<a-}>`  
Extend inside start/end of object

`m`  
Selects to the next pair of matching brackets.

`<a-m>`  
Selects the previous pair of matching brackets.

`M`, `<a-M>`  
Same as the previous two, but extends the selection.

`<a-u>`  
Returns to the previous state for the selections.

`<a-U>`  
Goes to the next state for the selections.

`;`  
Reduces selections to just the [caret][__link6].

`<a-;>`  
Flips the [caret][__link7] and [anchor][__link8] of selectionss around.

`,`  
Removes extra selections.

`C`  
Creates a selection on the column below the last one.

`<a-C>`  
Creates a selection on the column above the first one.

`<a-:>`  
Places the [caret][__link9] ahead of the [anchor][__link10] in all selections.

`X`  
Divides selection into multiple selections, one per line.

`D`  
Splits into two selections, one at each end of the selection.

`<a-_>`  
Merges all adjacent selections.

`<a-q>`  
Replays the recorded macro.

`<a-Q>`  
Starts/stops recording a macro.

</details>

<details>
<summary>

#### Text modification

</summary>

`i`  
Enter `insert` mode before selections, keys inserted (except for
`<Delete>`) will only move the selection.

`a`  
Enter `insert` mode after selection, keys inserted will extend the
selection.

`I`  
Moves to the beginning of the line (after indent) and enters
`insert` mode.

`A`  
Moves to the end of the line and enters `insert` mode.

`y`  
Yanks selections.

`d`  
Deletes and yanks the selections.

`c`  
Deletes, yanks, and enter `insert` mode.

`p`  
Pastes after end of each selection (multi line selections are
placed on the next line).

`P`  
Pastes at the start of each selection (multi line pastes are
placed on the previous line).

`R`  
Replaces with the pasted text, without yanking.

`<a-d>`  
Deletes selections without yanking.

`<a-c>`  
Deletes selections without yanking, then enters `insert` mode.

`o`  
Creates a new line below and enters `insert` mode in it.

`O`  
Creates a new line above and enters `insert` mode in it.

`<a-(o|O)>`  
Same as `(o|O)`, but just adds the new line without moving.

`r{key}`  
Replaces each character with `{key}`

`u`  
[Undoes][__link11] the last `moment`

`U`  
[Redoes][__link12] the next `moment`

`>`  
Adds indentation to the selected lines.

`<`  
Removes indentation to the selected lines.

`<a-j>`  
Merges selected lines.

`` ` ``  
Changes selection to lowercase.

`~`  
Changes selection to uppercase.

``<a-`>``  
Swaps the case of each character.

`<a-)>`  
Rotates each selection’s content forwards.

`<a-(>`  
Rotates each selection’s content backwards.

`|`  
Changes mode to [`PipeSelections`][__link13], letting you pipe each
selection to an external program.

</details>

<details>
<summary>

#### Search

</summary>

The searching in this plugin is done through the [`IncSearch`][__link14]
[`Mode`][__link15] from Duat, with some [`IncSearcher`][__link16]s defined in this
crate. This means that search will be done incrementally over a
Regex pattern.

`/`  
Searches forward for the next pattern.

`<a-/>`  
Searches backwards for the previous pattern.

`?`  
Extends forward for the next pattern.

`<a-?>`  
Extends backwards for the previous pattern.

`s`  
Selects the pattern from within current selections.

`S`  
Splits current selections by the pattern.

`<a-k>`  
Keeps only the selections that match the pattern.

`<a-K>`  
Keeps only the selections that *don’t* match the pattern.

`n`  
Go to next match for pattern.

`N`  
Create a new cursor on the next match for pattern.

`<a-n>`  
Go to previous match for pattern.

`<a-N>`  
Create a new cursor on the previous match for pattern.

`*`  
Makes the main selection the searching pattern.

</details>

<details>
<summary>

### `goto` mode

</summary>

`goto` mode is entered with the `g` or `G` keys in `normal` mode.

On every key that selects, `G` will have the same behavior, but
extending the selection instead.

`h`  
Move to the beginning of the line (before indents, column 0).

`l`  
Go to the end of the line.

`i`  
Go to the beginning of the line, after indents.

`g`,`k`  
Go to the first line.

`j`  
Go to the last line.

`a`  
Go to the last buffer. Repeating will return to this buffer

`n`  
Go to the next buffer (includes other windows).

`N`  
Go to the previous buffer (includes other windows).

</details>

<details>
<summary>

### User mode

</summary>

In Duat, [`User`][__link17] mode is a “generalized mode”, which should be
used by [`Plugin`][__link18]s for key maps. For example, you could map `l`
on `User` mode to do LSP related actions.

Other “monolithic modes” (Vim, Helix, Emacs, etc) should make use
of this [`User`][__link19] mode for the same purpose. Think of it like the
leader key in (neo)vim.

To enter `User` mode, you type `<Space>` in `Normal` mode.

</details>


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG6FZ99dyf85bG2mBrP-naXRHG-TuFOlR0fnBG6Zg3bFImaOVYWSDgmlkdWF0X2Jhc2VlMC45LjCCaWR1YXRfY29yZWUwLjkuMIJoZHVhdG1vZGVlMC45LjA
 [__link0]: https://github.com/mawww/kakoune
 [__link1]: https://docs.rs/duat_core/0.9.0/duat_core/?search=Plugin
 [__link10]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::Cursor::anchor
 [__link11]: https://docs.rs/duat_core/0.9.0/duat_core/?search=text::TextMut::undo
 [__link12]: https://docs.rs/duat_core/0.9.0/duat_core/?search=text::TextMut::redo
 [__link13]: https://docs.rs/duat_base/0.9.0/duat_base/?search=modes::PipeSelections
 [__link14]: https://docs.rs/duat_base/0.9.0/duat_base/?search=modes::IncSearch
 [__link15]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::Mode
 [__link16]: https://docs.rs/duat_base/0.9.0/duat_base/?search=modes::IncSearcher
 [__link17]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::User
 [__link18]: https://docs.rs/duat_core/0.9.0/duat_core/?search=Plugin
 [__link19]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::User
 [__link2]: https://docs.rs/duatmode/0.9.0/duatmode/opts/index.html
 [__link3]: https://docs.rs/duat/latest/duat/opts
 [__link4]: https://docs.rs/duatmode/0.9.0/duatmode/?search=insert::TabMode
 [__link5]: https://docs.rs/duat_core/0.9.0/duat_core/?search=opts::PrintOpts::extra_word_chars
 [__link6]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::Cursor::caret
 [__link7]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::Cursor::caret
 [__link8]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::Cursor::anchor
 [__link9]: https://docs.rs/duat_core/0.9.0/duat_core/?search=mode::Cursor::caret
