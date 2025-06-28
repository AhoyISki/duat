# duat-kak ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-kak on crates.io](https://img.shields.io/crates/v/duat-kak)](https://crates.io/crates/duat-kak) [![duat-kak on docs.rs](https://docs.rs/duat-kak/badge.svg)](https://docs.rs/duat-kak) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-kak)

`duat-kak` is the implementation of the [kakoune][__link0] editing model
for Duat. It’s still a work in progress, but it already implements
most of the common commands from Kakoune, with some modifications
that I thought made sense.

The plugin currently has 2 options: `insert_tabs` and
`set_cursor_forms`. `insert_tabs` will make the `Tab` key insert a
`\t` character, instead of an appropriate amount of spaces.
`set_cursor_forms` will create a hook to set the `caret.main.`
and `caret.extra` forms to mode specific variants, c.g.
`caret.main.Insert`.

## Installation

Just like other Duat plugins, this one can be installed by calling
`cargo add` in the config directory:

```bash
cargo add duat-kak@"*" --rename kak
```

Or, if you are using a `--git-deps` version of duat, do this:

```bash
cargo add --git https://github.com/AhoyISki/duat-kak --rename kak
```

## Keymaps

This is a list of *currently* mapped keys, not the ones that
appear in Kakoune.

When reading keys, they follow Duat’s [mapping][__link1] rules, that is:

* `<A-{key}>` is a chord of `Alt + {key}`, same with `<C-{key}>`
  and `Control` and `<S-{key}>` with `Shift` (although that one is
  not usually needed).
* Special keys are enclosed in `<` `>` pairs (c.g. `<Home>`).
* Multiple keys in a row represent a sequence.

In any mode, the `<Esc>` key will take you back to `normal` mode.

In this plugin, a `moment` contains all of the changes performed
by each [`Cursor`][__link2], so if you press `u`, multiple changes may be
undone.

### `Normal` mode

The keys in `normal` mode follow the following patterns:

* All actions will be done to all selections.
* `word` characters follow Duat’s [word chars][__link3], which are normally
  used to define where lines wrap.
* `WORD` characters are just any non-whitespace character.
* All keys that say “select”, when typed with `<Shift>` will
  extend the selection instead (not necessarily growing it).
* Yanked selections are always pasted in the order they were
  yanked, looping around if there are less yanks than [`Cursor`][__link4]s.

#### Object selection

`h`, `<Left>`  
Selects the character to the left. Wraps around lines.

`j`  
Selects the character below on the next line.

`<Down>`  
Selects the character below on the next wrapped line (i.c vim’s
`gj`).

`k`  
Selects the character above on the previous line.

`<Up>`  
Selects the character above on the previous wrapped line (i.c.
vim’s `gk`).

`l`, `<Right>`  
Selects the character to the right. Wraps around lines.

`w`  
Selects the `word` and following space to the right of the
selection.

`b`  
Selects the `word` followed by spaces to the left of the
selection.

`c`  
Selects to the end of the next `word` to the right of the
selection.

`<A-(w|b|c)>`  
The same as `(w|b|c)`, but over a `WORD`.

`f{key}`  
Selects to the next occurrence of the `{key}` character.

`t{key}`  
Selects until the next occurrence of the `{key}` character.

`<A-(f|t)>{key}`  
Same as `(f|t)`, but in the opposite direction.

`x`  
Extends selection to encompass full lines.

`%`  
Selects the whole buffer.

`<A-h>`, `<Home>`  
Selects to the start of the line.

`<A-l>`, `<End>`  
Selects until the end of the line.

#### Changing text

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
Same as `(o|O)`, but just adds the new line.

`r{key}`  
Replaces each character with `{key}`

`u`  
[Undoes][__link5] the last `moment`

`U`  
[Redoes][__link6] the next `moment`

`` ` ``  
Changes to lowercase.

`~`  
Changes to uppercase.

`<A->`  
Swaps the case of each character.

`<A-)>`  
Rotates each selection’s content forwards.

`<A-(>`  
Rptates each selection’s content backwards.

`|`  
Changes mode to [`PipeSelections`][__link7], letting you pipe each
selection to an external program.

#### Incremental Search

The searching in this plugin is done through the [`IncSearch`][__link8]
[`Mode`][__link9] from Duat, with some [`IncSearcher`][__link10]s defined in this
crate. This means that search will be done incrementally over a
Regex pattern.

`/`  
Searches forward for the next pattern, on each [`Cursor`][__link11].

`<A-/>`  
Searches backwards for the previous pattern, on each [`Cursor`][__link12].

`(?|<A-?>)`  
Follows the `Shift` pattern described above, so its the same as
`(/|<A-/>)`, but extending the selection instead.

`s`  
Selects the pattern from within current selections.

`S`  
Splits current selections by the pattern.

#### Selection manipulation

`;`  
Reduces selections to just the [caret][__link13].

`<A-;>`  
Flips the [caret][__link14] and [anchor][__link15] of [`Cursor`][__link16]s around.

`<A-:>`  
Places the [caret][__link17] ahead of the [anchor][__link18] in all selections.

`<A-s>`  
Divides selection into multiple selections, one per line.

`<A-S>`  
Splits into two [`Cursor`][__link19]s, one at each end of the selection.

### `goto` mode

`goto` mode is entered with the `g` or `G` keys in `normal` mode.
The `G` follows the same `Shift` pattern described above.

`h`  
Go to the beginning of the line (before indents, column 0).

`l`  
Go to the end of the line.

`i`  
Go to the beginning of the line, after indents.

`g`,`k`  
Go to the first line.

`j`  
Go to the last line.

`a`  
Go to the previous [`File`][__link20].

`n`  
Go to the next [`File`][__link21] (includes other windows).

`N`  
Go to the previous [`File`][__link22] (includes other windows).


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG-UXbt1FTpDfGx7CpzozFw8ZG8Q6dLtj5mSgG6EgTKrWPz6QYWSEgmRNb2Rl9oJpZHVhdF9jb3JlZTAuNS4zgmpkdWF0X3V0aWxzZTAuMi4xgmdtYXBwaW5n9g
 [__link0]: https://github.com/mawww/kakoune
 [__link1]: https://crates.io/crates/mapping
 [__link10]: https://docs.rs/duat_utils/0.2.1/duat_utils/?search=modes::IncSearcher
 [__link11]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link12]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link13]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor::caret
 [__link14]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor::caret
 [__link15]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor::anchor
 [__link16]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link17]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor::caret
 [__link18]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor::anchor
 [__link19]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link2]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link20]: https://docs.rs/duat_core/0.5.3/duat_core/?search=file::File
 [__link21]: https://docs.rs/duat_core/0.5.3/duat_core/?search=file::File
 [__link22]: https://docs.rs/duat_core/0.5.3/duat_core/?search=file::File
 [__link3]: https://docs.rs/duat_core/0.5.3/duat_core/?search=cfg::word_chars
 [__link4]: https://docs.rs/duat_core/0.5.3/duat_core/?search=mode::Cursor
 [__link5]: https://docs.rs/duat_core/0.5.3/duat_core/?search=text::Text::undo
 [__link6]: https://docs.rs/duat_core/0.5.3/duat_core/?search=text::Text::redo
 [__link7]: https://docs.rs/duat_utils/0.2.1/duat_utils/?search=modes::PipeSelections
 [__link8]: https://docs.rs/duat_utils/0.2.1/duat_utils/?search=modes::IncSearch
 [__link9]: https://crates.io/crates/Mode
