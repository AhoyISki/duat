# duat-kak ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-kak on crates.io](https://img.shields.io/crates/v/duat-kak)](https://crates.io/crates/duat-kak) [![duat-kak on docs.rs](https://docs.rs/duat-kak/badge.svg)](https://docs.rs/duat-kak) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-kak)

`duat-kak` is the implementation of the [kakoune][__link0] editing model
for Duat. It’s still a work in progress, but it already implements
most of the common commands from Kakoune, with some modifications
that I thought made sense.

The plugin currently has 2 options: `insert_tabs` and
`set_cursor_forms`. `insert_tabs` will make the `Tab` key insert a
`\t` character, instead of an appropriate amount of spaces.
`set_cursor_forms` will create a hook to set the `MainCursor`,
`ExtraCursor`, `MainSelection` and `ExtraSelection` forms to mode
specific varieties, e.g. `MainCursorInsert`.

## Keymaps

### Normal mode

* `h`, `<Left>`  
  Moves the selection to the left, reducing it to just the cursor

<table>
  <td style="text-align: center">

`j`

  </td>
  <td>Moves the selection down to the next line, reducing it to
just the cursor</td>  </tr>
 <tr>
  <td style="text-align: center">

`k`

  </td>
  <td>Moves the selection up to the previous line, reducing it to
just the cursor</td>  </tr>
 <tr>
  <td style="text-align: center">

`h`<br>`<Left>`

  </td>
  <td>Moves the selection to the left, reducing it to just the
cursor</td>  </tr>
</table


 [__link0]: https://github.com/mawww/kakoune
