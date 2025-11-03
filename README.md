# duatmode ![License: GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue) [![duatmode on crates.io](https://img.shields.io/crates/v/duatmode)](https://crates.io/crates/duatmode) [![duatmode on docs.rs](https://docs.rs/duatmode/badge.svg)](https://docs.rs/duatmode) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duatmode)

`duatmode` is the default mode for the Duat text editor. It is
based on [kakoune][__link0]’s keybindings, with some alterations of my own.

This plugin is included in Duat by default, as it is considered
part of it’s identity. Given that, the options aren’t set by this
plugin’s [`Plugin`][__link1], but by [`duatmode::opts`][__link2], which is exported
in duat’s [`opts`][__link3] module by default

## Keymaps

On every key, if the action involves selections, unless stated
otherwise, the action will take place in all selections.

### `Normal` mode

The keys in `normal` mode follow the following patterns:

* `word` characters follow Duat’s [word chars][__link4], which are normally
  used to define where lines wrap.
* `WORD` characters are just any non-whitespace character.

<details>
<summary>

#### Object selection

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

`H`, `<S-Left>`, `J`, `<S-Down>`, `K`, `<S-Up>`, `L`, `<S-Right>`  
Same as the previous characters, but extends the selection

`w`  
Selects the `word` and following space ahead of the selection.

`b`  
Selects the `word` followed by spaces behind the selection.

`e`  
Selects to the end of the next `word` ahead of the selection.

`<(W|B|E)>`  
The same as `(w|b|e)`, but extends the selection.

`<A-(w|b|e)>`  
The same as `(w|b|e)`, but over a `WORD`.

`<A-(W|B|E)>`  
The same as `<A-(w|b|e)>`, but extends the selection.

`f{char}`  
Selects to the next occurrence of the `{char}`.

`t{char}`  
Selects until the next occurrence of the `{char}`.

`<(F|T)>{char}`  
Same as `(f|t)`, but extends the selection.

`<A-(f|t)>{char}`  
Same as `(f|t)`, but in the opposite direction.

`<A-(F|T)>{char}`  
Same as `<A-(f|t)>`, but in extends the selection.

`x`  
Extends selection to encompass full lines.

`%`  
Selects the whole buffer.

`<A-h>`, `<Home>`  
Selects to the start of the line.

`<A-l>`, `<End>`  
Selects until the end of the line.

`<A-H>`, `<S-Home>`, `<A-L>`, `<S-End>`  
Same as the previous two, but extends the selection.

`;`  
Reduces selections to just the [caret][__link5].

`<A-;>`  
Flips the [caret][__link6] and [anchor][__link7] of selectionss around.

`<A-:>`  
Places the [caret][__link8] ahead of the [anchor][__link9] in all selections.

`<A-s>`  
Divides selection into multiple selections, one per line.

`<A-S>`  
Splits into two selections, one at each end of the selection.

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

`<A-d>`  
Deletes selections without yanking.

`<A-c>`  
Deletes selections without yanking, then enters `insert` mode.

`o`  
Creates a new line below and enters `insert` mode in it.

`O`  
Creates a new line above and enters `insert` mode in it.

`<A-(o|O)>`  
Same as `(o|O)`, but just adds the new line without moving.

`r{key}`  
Replaces each character with `{key}`

`u`  
[Undoes][__link10] the last `moment`

`U`  
[Redoes][__link11] the next `moment`

`` ` ``  
Changes selection to lowercase.

`~`  
Changes selection to uppercase.

``<A-`>``  
Swaps the case of each character.

`<A-)>`  
Rotates each selection’s content forwards.

`<A-(>`  
Rotates each selection’s content backwards.

`|`  
Changes mode to [`PipeSelections`][__link12], letting you pipe each
selection to an external program.

</details>

<details>
<summary>

#### Search

</summary>

The searching in this plugin is done through the [`IncSearch`][__link13]
[`Mode`][__link14] from Duat, with some [`IncSearcher`][__link15]s defined in this
crate. This means that search will be done incrementally over a
Regex pattern.

`/`  
Searches forward for the next pattern.

`<A-/>`  
Searches backwards for the previous pattern.

`?`  
Extends forward for the next pattern.

`<A-?>`  
Extends backwards for the previous pattern.

`s`  
Selects the pattern from within current selections.

`S`  
Splits current selections by the pattern.

`<A-k>`  
Keeps only the selections that match the pattern.

`<A-K>`  
Keeps only the selections that *don’t* match the pattern.

`n`  
Go to next match for pattern.

`N`  
Create a new cursor on the next match for pattern.

`<A-n>`  
Go to previous match for pattern.

`<A-N>`  
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


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG87WniIsC2u5G7al6lr2ftONG9CZDLwM_bHzG2b3TIqM5CIuYWSDgmlkdWF0X2Jhc2VlMC43LjCCaWR1YXRfY29yZWUwLjcuMIJoZHVhdG1vZGVlMC43LjA
 [__link0]: https://github.com/mawww/kakoune
 [__link1]: https://docs.rs/duat_core/0.7.0/duat_core/?search=Plugin
 [__link10]: https://docs.rs/duat_core/0.7.0/duat_core/?search=text::Text::undo
 [__link11]: https://docs.rs/duat_core/0.7.0/duat_core/?search=text::Text::redo
 [__link12]: https://docs.rs/duat_base/0.7.0/duat_base/?search=modes::PipeSelections
 [__link13]: https://docs.rs/duat_base/0.7.0/duat_base/?search=modes::IncSearch
 [__link14]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Mode
 [__link15]: https://docs.rs/duat_base/0.7.0/duat_base/?search=modes::IncSearcher
 [__link2]: https://docs.rs/duatmode/0.7.0/duatmode/opts/index.html
 [__link3]: https://docs.rs/duat/latest/duat/opts
 [__link4]: https://docs.rs/duat_core/0.7.0/duat_core/?search=opts::Opts::extra_word_chars
 [__link5]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Cursor::caret
 [__link6]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Cursor::caret
 [__link7]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Cursor::anchor
 [__link8]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Cursor::caret
 [__link9]: https://docs.rs/duat_core/0.7.0/duat_core/?search=mode::Cursor::anchor
