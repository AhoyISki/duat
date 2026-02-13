use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use serde::Deserialize;
use serde_json::Value;

#[derive(Debug, Clone, Default, Deserialize)]
pub struct LanguageServerConfig {
    pub command: Option<String>,
    #[serde(default)]
    pub args: Vec<String>,
    #[serde(default)]
    pub envs: HashMap<String, String>,
    pub settings: Option<Value>,
    pub experimental: Option<Value>,
    #[serde(default)]
    pub root_globs: Vec<String>,
    #[serde(default)]
    pub symbol_names: HashMap<String, String>,
}

static FILETYPES: LazyLock<Mutex<HashMap<&str, Vec<&str>>>> = LazyLock::new(|| {
    Mutex::new(HashMap::from_iter([
        ("c", vec!["clangd"]),
        ("cpp", vec!["clangd"]),
        ("objc", vec!["clangd"]),
        ("closure", vec!["closure-lsp"]),
        ("cmake", vec!["cmake-language-server"]),
        ("crystal", vec!["crystalline"]),
        ("css", vec!["vscode-css-language-server"]),
        ("less", vec!["vscode-css-language-server"]),
        ("scss", vec!["vscode-css-language-server"]),
        ("d", vec!["dls"]),
        ("di", vec!["dls"]),
        ("dart", vec!["dart-lsp"]),
        ("elixir", vec!["elixir-ls"]),
        ("elm", vec!["elm-language-server"]),
        ("elvish", vec!["elvish"]),
        ("erlang", vec!["elp"]),
        ("fsharp", vec!["fsautocomplete"]),
        ("go", vec!["gopls"]),
        ("haskell", vec!["haskell-language-server"]),
        ("html", vec!["vscode-html-language-server"]),
        ("java", vec!["jdtls"]),
        ("javascript", vec!["typescript-language-server"]),
        ("typescript", vec!["typescript-language-server"]),
        ("vue", vec!["typescript-language-server"]),
        ("json", vec!["vscode-json-language-server"]),
        ("latex", vec!["texlab"]),
        ("rust", vec!["rust-analyzer"]),
    ]))
});

// Configuration for language servers, taken from kak-lsp.
static CONFIGS: LazyLock<Mutex<HashMap<String, LanguageServerConfig>>> = LazyLock::new(|| {
    let mut table = toml::Table::new();

    // Doing it this way to avoid heavy macro recursion.
    macro_rules! entry {
        ($lsp:literal { $($tokens:tt)* }) => {
            table.insert($lsp.to_string(), toml::Value::from(toml::toml! { $($tokens)* }));
        }
    }

    entry!("clangd" {
        args = ["--log=error"]
        root_globs = ["compile_commands.json", ".clangd", ".git", ".hg"]
    });

    entry!("closure-lsp" {
        root_globs = ["project.clj", ".git", ".hg"]
        [settings]
        // See https://clojure-lsp.io/settings/#all-settings
        // source-paths-ignore-regex = ["resources.*", "target.*"]
    });

    entry!("cmake-language-serever" {
        root_globs = ["CMakeLists.txt", ".git", ".hg"]
    });

    entry!("crystalline" {
        root_globs = ["shard.yml"]
    });

    entry!("vscode-css-language-server" {
        // Documented options see
        // https://github.com/sublimelsp/LSP-css/blob/master/LSP-css.sublime-settings
        root_globs = ["package.json", ".git", ".hg"]
        args = ["--stdio"]
        [settings]
        provideFormatter = true
        handledSchemas = ["file"]
        css.format.enable = true
        css.validProperties = []
        css.validate = true
        scss.validProperties = []
        scss.format.enable = true
        scss.validate = true
        less.validProperties = []
        less.format.enable = true
        less.validate = true
    });

    entry!("dls" {
        root_globs = [".git", "dub.sdl", "dub.json"]
    });

    entry!("dart-lsp" {
        root_globs = ["pubspec.yaml", ".git", ".hg"]
        command = "dart"
        args = ["language-server"]
    });

    entry!("elixir-ls" {
        root_globs = ["mix.exs"]
        [settings]
        // See https://github.com/elixir-lsp/elixir-ls/blob/master/apps/language_server/lib/language_server/server.ex
        // dialyzerEnable = true
    });

    entry!("elm-language-server" {
        root_globs = ["elm.json"]
        args = ["--stdio"]
        [settings]
        // See https://github.com/elm-tooling/elm-language-server#server-settings
        runtime = "node"
        elmPath = "elm"
        elmFormatPath = "elm-format"
        elmTestPath = "elm-test"
    });

    entry!("elvish" {
        root_globs = [".git", ".hg"]
        args = ["-lsp"]
    });

    entry!("elp" {
        root_globs = ["rebar.config", "erlang.mk", ".git", ".hg"]
        args = [ "server" ]
    });

    entry!("fsautocomplete" {
        root_globs = [".git", ".hg", ".sln", ".fsproj"]
        [settings]
        AutomaticWorkspaceInit = true
    });

    entry!("gopls" {
        root_globs = ["Gopkg.toml", "go.mod", ".git", ".hg"]
        [settings.gopls]
        // See https://github.com/golang/tools/blob/master/gopls/doc/settings.md
        // "build.buildFlags" = []
        hints.assignVariableTypes = true
        hints.compositeLiteralFields = true
        hints.compositeLiteralTypes = true
        hints.constantValues = true
        hints.functionTypeParameters = true
        hints.parameterNames = true
        hints.rangeVariableTypes = true
        usePlaceholders = true
    });

    entry!("haskell-language-server" {
        root_globs = ["hie.yaml", "cabal.project", "Setup.hs", "stack.yaml", "*.cabal"]
        command = "haskell-language-server-wrapper"
        args = ["--lsp"]
        [settings]
        // See https://haskell-language-server.readthedocs.io/en/latest/configuration.html
        // haskell.formattingProvider = "ormolu"
        // There now exists also static-ls, which uses less memory, is faster and suited
        // even for big Haskell code bases. But it needs more configuration.
        // https://github.com/josephsumabat/static-ls
        // See https://github.com/josephsumabat/static-ls?tab=readme-ov-file#quick-start
        // and https://github.com/josephsumabat/static-ls/blob/main/docs/advanced-setup.md
        // [static-ls]
        // root_globs = ["*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml", "Setup.hs"]
        // command = "static-ls"
    });

    entry!("vscode-html-language-server" {
        // Documented options see
        // https://github.com/sublimelsp/LSP-html/blob/master/LSP-html.sublime-settings
        root_globs = ["package.json", ".git", ".hg"]
        args = ["--stdio"]
        [settings]
        provideFormatter = true
        [settings]
        embeddedLanguages.css = true
        embeddedLanguages.javascript = true
        html.autoClosingTags = true
        html.format.enable = true
        html.mirrorCursorOnMatchingTag = true
        html.validate.scripts = true
        html.validate.styles = true
        css.validate = true
        css.format.enable = true
        css.validProperties = []
        javascript.format.enable = true
        javascript.validate.enable = true
        // This is mainly a linter for HTML and to be used together with vscode-html-language-server
        // https://github.com/kristoff-it/superhtml
        // [superhtml]
        // root_globs = ["package.json", ".git", ".hg"]
        // args = ["lsp"]
    });

    // entry!("lake" {
    //     TODO: figure out wth goin on.
    //     [lake] for lean
    //     root_globs = ["lakefile.lean", "lakefile.toml", ".git", ".hg"]
    //     command = "sh"
    //     args = [
    //         "-c",
    //         "kak_buffile=%val{buffile}
    //           %opt{lsp_find_root} lakefile.lean lakefile.toml .git .hg >/dev/null
    //           exec lake serve
    //         "
    //     ]
    // });

    entry!("rust-analyzer" {
        root_globs = ["Cargo.toml"]
        single_instance = true
        [experimental]
        commands.commands = ["rust-analyzer.runSingle"]
        hoverActions = true
        [settings]
        rust-analyzer.check.command = "clippy"
        [symbol_kinds]
        Constant = "const"
        Enum = "enum"
        EnumMember = ""
        Field = ""
        Function = "fn"
        Interface = "trait"
        Method = "fn"
        Module = "mod"
        Object = ""
        Struct = "struct"
        TypeParameter = "type"
        Variable = "let"
    });

    entry!("jdtls" {
        root_globs = ["mvnw", "gradlew", ".git", ".hg"]
        // settings_section = "_"
        // workspace_did_change_configuration_subsection = "settings"
        // [settings._.settings]
        // See https://github.com/eclipse-jdtls/eclipse.jdt.ls/blob/main/org.eclipse.jdt.ls.core/src/org/eclipse/jdt/ls/core/internal/preferences/Preferences.java
        // "java.format.enabled" = true
    });

    entry!("typescript-language-server" {
        root_globs = ["package.json", "tsconfig.json", "jsconfig.json", ".git", ".hg"]
        args = ["--stdio"]
        [settings]
        // quotePreference = "double"
        // typescript.format.semicolons = "insert"
    });
    // entry!("deno" {
    //     root_globs = ["package.json", "tsconfig.json", ".git", ".hg"]
    //     args = ["lsp"]
    //     [settings]
    //     enable = true
    //     lint = true
    // });
    // set-option buffer lsp_servers %opt{lsp_server_biome}
    // entry!("eslint-language-server" {
    //     root_globs = [".eslintrc", ".eslintrc.json"]
    //     args = ["--stdio"]
    //     workaround_eslint = true
    //     [settings]
    //     codeActionsOnSave = { mode = "all", "source.fixAll.eslint" = true }
    //     format = { enable = true }
    //     quiet = false
    //     rulesCustomizations = []
    //     run = "onType"
    //     validate = "on"
    //     experimental = {}
    //     problems = { shortenToSingleLine = false }
    //     codeAction.disableRuleComment = { enable = true, location = "separateLine" }
    //     codeAction.showDocumentation = { enable = false }
    // });
    // entry!("tailwindcss-language-server" {
    //     root_globs = ["tailwind.*"]
    //     args = ["--stdio"]
    //     [settings.tailwindCSS]
    //     editor = {}
    // });

    entry!("typescript-language-server" {
        root_globs = ["package.json", "tsconfig.json", "jsconfig.json", ".git", ".hg"]
        args = ["--stdio"]
        [settings]
        plugins = [{
            name = "@vue/typescript-plugin",
            location = "vue-language-server",
            languages = ["vue"]
        }]
    });
    // entry!("tailwindcss-language-server" {
    //      root_globs = ["tailwind.*"]
    //      args = ["--stdio"]
    //      [settings.tailwindCSS]
    //      editor = {}
    // });

    entry!("vscode-json-language-server" {
        root_globs = ["package.json", ".git", ".hg"]
        args = ["--stdio"]
        [settings]
        provideFormatter = true
        json.format.enable = true
        json.validate.enable = true
        // These are just some example JSON schemas, you need to add whatever JSON files you edit.
        // The needed URLs you can find at https://www.schemastore.org/json/
        // Configuration see
        // https://github.com/microsoft/vscode/blob/main/extensions/json-language-features/server/README.md#configuration
        [[settings.json.schemas]]
        fileMatch = ["/package.json"]
        url = "https://json.schemastore.org/package.json"
        [[settings.json.schemas]]
        fileMatch = ["/.markdownlintrc","/.markdownlint.json","/.markdownlint.jsonc"]
        url = "https://raw.githubusercontent.com/DavidAnson/markdownlint/main/schema/markdownlint-config-schema.json"
        [[settings.json.schemas]]
        fileMatch = ["/.prettierrc", "/.prettierrc.json"]
        url = "https://json.schemastore.org/prettierrc.json"
        [[settings.json.schemas]]
        fileMatch = ["/compile_commands.json"]
        url = "https://json.schemastore.org/compile-commands.json"
        [[settings.json.schemas]]
        fileMatch = ["/tsconfig*.json"]
        url = "https://json.schemastore.org/tsconfig.json"
    });

    entry!("julia-language-server" {
        // Requires Julia package "LanguageServer"
        // Run: `julia --project=@kak-lsp -e 'import Pkg; Pkg.add("LanguageServer")'` to install it
        // Configuration adapted from https://github.com/neovim/nvim-lspconfig/blob/bcebfac7429cd8234960197dca8de1767f3ef5d3/lua/lspconfig/julials.lua
        root_globs = ["Project.toml", ".git", ".hg"]
        command = "julia"
        args = [
            "--startup-file=no",
            "--history-file=no",
            "-e",
            r#"
            ls_install_path = joinpath(get(DEPOT_PATH, 1, joinpath(homedir(), ".julia")), "environments", "duat-lsp");
            pushfirst!(LOAD_PATH, ls_install_path);
            using LanguageServer;
            popfirst!(LOAD_PATH);
            depot_path = get(ENV, "JULIA_DEPOT_PATH", "");
            server = LanguageServer.LanguageServerInstance(stdin, stdout, "", depot_path);
            server.runlinter = true;
            run(server);
            "#,
        ]
        [settings]
        // See https://github.com/julia-vscode/LanguageServer.jl/blob/master/src/requests/workspace.jl
        // Format options. See https://github.com/julia-vscode/DocumentFormat.jl/blob/master/src/DocumentFormat.jl
        // "julia.format.indent" = 4
        // Lint options. See https://github.com/julia-vscode/StaticLint.jl/blob/master/src/linting/checks.jl
        // "julia.lint.call" = true
        // Other options, see https://github.com/julia-vscode/LanguageServer.jl/blob/master/src/requests/workspace.jl
        // "julia.lint.run" = true
    });

    entry!("texlab" {
        root_globs = [".git", ".hg"]
        [settings.texlab]
        // See https://github.com/latex-lsp/texlab/wiki/Configuration
        //
        // Preview configuration for zathura with SyncTeX search.
        // For other PDF viewers see https://github.com/latex-lsp/texlab/wiki/Previewing
        forwardSearch.executable = "zathura"
        forwardSearch.args = [
            "%p",
            "--synctex-forward", // Support texlab-forward-search
            "%l:1:%f",
            "--synctex-editor-command", // Inverse search: use Control+Left-Mouse-Button to jump to source.
            r#"
                sh -c '
                    echo "
                        evaluate-commands -client %%opt{texlab_client} %%{
                            evaluate-commands -try-client %%opt{jumpclient} %%{
                                edit -- %%{input} %%{line}
                            }
                        }
                    " | kak -p $kak_session
                '
            "#,
        ]
    });
    // entry!("ltex-ls" {
    //     root_globs = [".git", ".hg"]
    // });

    Mutex::new(table.try_into().unwrap())
});
