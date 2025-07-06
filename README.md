# duat-filetype ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-filetype on crates.io](https://img.shields.io/crates/v/duat-filetype)](https://crates.io/crates/duat-filetype) [![duat-filetype on docs.rs](https://docs.rs/duat-filetype/badge.svg)](https://docs.rs/duat-filetype)

Filetypes for the Duat text editor

This [`Plugin`][__link0] is included by default, as it is considered a core
utility of duat. It adds the two following traits:

* [`FileType`][__link1]: This trait grants the [`filetype`][__link2]
  method, which lets you access the filetype directly. Its
  implementors are the [`File`][__link3] widget, [`String`][__link4] and [`&str`][__link5]
  and [`PathBuf`][__link6] and [`Path`][__link7].
* [`PassFileType`][__link8]: This trait also has a
  [`filetype`][__link9] method, but it requires a
  [`Pass`][__link10], bypassing the need to, for example, [`read`][__link11] a
  [`Handle<File<Ui>, Ui>`][__link12]. Its implementors are
  [`RwData<File<Ui>>`][__link13], [`Handle<File<Ui>, Ui>`][__link14],
  [`FileHandle<Ui>`][__link15] and [`FileBuilder<Ui>`][__link16], from the
  [`OnFileOpen`][__link17] [hook][__link18].

Both of these traits are included by default in Duat’s
[`prelude`][__link19], but if you want to use them in a plugin, first, add
`duat-filetype` to the dependencies:

```bash
cargo add duat-filetype
```

Or, for latest git version:

```bash
cargo add --git https://github.com/AhoyISki/duat-filetype
```

Then, just `use` it in the file:

```rust
use duat_filetype::FileType;

fn is_toml(file_name: &str) -> bool {
    file_name.filetype() == Some("toml")
}
```


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG2pGQum9LFWDG2ttHAKL9mlbG6iPx1QRI8g0G8xp_wellraxYWSCg21kdWF0LWZpbGV0eXBlZTAuMi4wbWR1YXRfZmlsZXR5cGWCaWR1YXRfY29yZWUwLjUuMw
 [__link0]: https://docs.rs/duat_core/0.5.3/duat_core/?search=prelude::Plugin
 [__link1]: https://docs.rs/duat-filetype/0.2.0/duat_filetype/trait.FileType.html
 [__link10]: https://docs.rs/duat_core/0.5.3/duat_core/?search=prelude::Pass
 [__link11]: https://docs.rs/duat_core/0.5.3/duat_core/?search=prelude::Handle::read
 [__link12]: https://docs.rs/duat_core/0.5.3/duat_core/?search=prelude::Handle
 [__link13]: https://docs.rs/duat_core/0.5.3/duat_core/?search=prelude::RwData
 [__link14]: https://docs.rs/duat_core/0.5.3/duat_core/?search=prelude::Handle
 [__link15]: https://docs.rs/duat_core/0.5.3/duat_core/?search=prelude::FileHandle
 [__link16]: https://docs.rs/duat_core/0.5.3/duat_core/?search=ui::FileBuilder
 [__link17]: https://docs.rs/duat_core/0.5.3/duat_core/?search=hook::OnFileOpen
 [__link18]: https://docs.rs/duat_core/0.5.3/duat_core/?search=hook
 [__link19]: https://docs.rs/duat/latest/duat/prelude
 [__link2]: https://docs.rs/duat-filetype/0.2.0/duat_filetype/?search=FileType::filetype
 [__link3]: https://docs.rs/duat_core/0.5.3/duat_core/?search=prelude::File
 [__link4]: https://doc.rust-lang.org/stable/std/string/struct.String.html
 [__link5]: https://doc.rust-lang.org/stable/std/primitive.str.html
 [__link6]: https://doc.rust-lang.org/stable/std/?search=path::PathBuf
 [__link7]: https://doc.rust-lang.org/stable/std/?search=path::Path
 [__link8]: https://docs.rs/duat-filetype/0.2.0/duat_filetype/trait.PassFileType.html
 [__link9]: https://docs.rs/duat-filetype/0.2.0/duat_filetype/?search=PassFileType::filetype
