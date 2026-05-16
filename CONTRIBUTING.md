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

Answer `y` to both when developing against the local repo. Duat will pull the git sources and compile your config against them — this takes a minute on the first run.

**Running the tests:**

```sh
cargo test
```

---

## Workspace Architecture

The repository is a Cargo workspace. Each crate has a focused responsibility:

| Crate | Purpose |
|---|---|
| `duat-core` | Core editor primitives (text, buffers, modes, hooks, UI traits) |
| `duat-base` | Built-in base plugins shipped with the editor |
| `duat-term` | Terminal UI implementation |
| `duat-filetype` | File type detection |
| `duat-treesitter` | Syntax highlighting via Tree-sitter |
| `duat-lsp` | Language Server Protocol client |
| `duat-match-pairs` | Bracket and pair matching |
| `duat-jump-list` | Jump list navigation |
| `duatmode` | Mode system |
| `duat-txt-macro` | Text macros |

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
4. Ensure `cargo test` passes locally.
5. Open a PR against `master`. CI will run the full test suite automatically.
6. Write a clear description of what the PR does and link any related issues (`Fixes #123`).

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

---

## Troubleshooting

### Config fails to compile after pulling new changes

When working against a local build (`cargo run`), your config at `~/.config/duat` is compiled separately and linked against the version of Duat it was last updated for. After pulling changes to the repo, the compiled config may be out of sync with the new binary, causing schema or API mismatches.

Run the following to recompile your config against the current local build:

```sh
cargo run -- --update --reload
```

This updates and reloads your config using the same version of Duat you are running locally, making the schemas compatible again.

## License

By contributing to Duat, you agree that your contributions will be licensed under the [GNU General Public License v3.0 or later](LICENSE), the same license as the project.
