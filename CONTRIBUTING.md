# Contributing to Duat

Thank you for your interest in contributing to Duat — a modern, highly customizable text editor with first-class multiple selection support, written and configured in Rust. Contributions of all kinds are welcome: bug reports, feature ideas, code, documentation, and plugins.

For a full overview of the project, see the [README](README.md).

---

## Ways to Contribute

### Bug Reports
Open an issue using the [Bug Report template](.github/ISSUE_TEMPLATE/BUG_REPORT.yml). Include reproduction steps, your Duat version, Rust version, and OS.

### Feature Requests
Open an issue using the [Feature Request template](.github/ISSUE_TEMPLATE/FEATURE_REQUEST.yml). The template asks whether the feature belongs in the core editor or would be better suited to a plugin — thinking this through in advance helps speed up the discussion.

### Code
Submit a pull request against `master`. See [Submitting a Pull Request](#submitting-a-pull-request) below.

### Documentation
The user-facing book lives in `book/` and is built with [mdBook](https://rust-lang.github.io/mdBook/). Fixes and new chapters are very welcome.

### Plugins
Duat is extended through ordinary Cargo crates — you can publish your own plugin independently. There is no need to contribute plugins to this repository unless they are candidates for the standard distribution.

---

## Development Setup

**Prerequisites:** Rust stable toolchain, `git`.

```sh
git clone https://github.com/AhoyISki/duat
cd duat
cargo build
```

Build warnings from `crates/duat-base/src/widgets/picker.rs` (unused variables, missing docs) are expected — that code is still in progress.

**Running the editor:**

```sh
cargo run
```

On first launch, Duat will prompt you to create a config:

```
Do you want to start a new config in ~/.config/duat? [y/N]
```

It will then ask whether your config should depend on the git version of Duat:

```
Do you want to depend on the git version of duat? [y/N]
```

If you want to check out the latest features (unstable), answer `y` to this question. If not, Duat will ask the following:

```
What about a local version of Duat? [y/N]
```

If you want to contribute to Duat by modifying the code base, answer `y` to this question. This will then prompt the following query:

```
Write the path to the directory containing the Cargo.toml:
```

You should then write the (absolute or relative) path to the directory containing Duat's `Cargo.toml`, i.e., the place where you executed the initial `git clone https://github.com/ahoyiski/duat`.

**Running the tests:**

```sh
cargo test
```

---

## Workspace Architecture

The repository is a Cargo workspace. Each crate has a focused responsibility:

| Crate | Purpose |
|---|---|
| `duat-core` | Core editor primitives (text, buffers, hooks, UI traits) |
| `duat-base` | Built-in base plugins shipped with the editor |
| `duat-term` | Terminal UI implementation |
| `duat-filetype` | File type detection and autocommenting |
| `duat-treesitter` | Tree-sitter integration |
| `duat-lsp` | LSP integration (experimental) |
| `duat-match-pairs` | Highlighting bracket pairs |
| `duat-jump-list` | Jumping via `<a-u>` and `<a-U>` |
| `duatmode` | The default duat controls via a `Normal` and `Insert` modes |
| `duat-txt-macro` | The `txt!` proc-macro |

A short description of how the crates relate to each other is in [crates/README.md](crates/README.md).

---

## Code Style

**Formatting:** Run `cargo fmt` before committing. The project uses `rustfmt.toml` (100-character line width, 4-space indentation, Rust 2024 style edition).

**Linting:** Run `cargo clippy --workspace` and fix any warnings before opening a PR.

**Documentation:** The workspace enforces `missing_docs = "warn"`. Every public item must have a `///` doc comment. When adding or changing public API, update the docs accordingly.

**Imports:** rustfmt groups imports by `StdExternalCrate` automatically — let the formatter handle it.

---

## Submitting a Pull Request

1. Fork the repository and create a branch from `master`.
2. Make your changes.
3. Run `cargo fmt` and `cargo clippy --workspace`; fix any issues.
4. Open a PR against `master`. CI will run the full test suite automatically.
5. Write a clear description of what the PR does and link any related issues (`Fixes #123`).

---

## Commit Message Style

The project uses a mix of conventional commits (`feat:`, `fix:`) and plain descriptive messages — either style is fine. Keep messages concise. Reference issue or PR numbers where relevant:

```
Fix startup panic caused by invalid clap argument ID (#64)
feat: add nix flake (#61)
Added scratch buffers
```

---

## Documentation (mdBook)

To preview the book locally:

```sh
cargo install mdbook
mdbook serve book/
```

The book is built and deployed to GitHub Pages automatically on every push to `master`.

## License

By contributing to Duat, you agree that your contributions will be licensed under the [GNU General Public License v3.0 or later](LICENSE), the same license as the project.
