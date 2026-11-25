use std::{collections::HashMap, sync::LazyLock};

use super::LanguageOptions;

pub static LANGUAGE_OPTIONS: LazyLock<HashMap<&str, LanguageOptions>> = LazyLock::new(|| {
    HashMap::from_iter([
        LanguageOptions::pair_const(
            "ada",
            ("https://github.com/briot/tree-sitter-ada", None),
            &["@briot"],
        ),
        LanguageOptions::pair_const(
            "agda",
            ("https://github.com/tree-sitter/tree-sitter-agda", None),
            &["@Decodetalkers"],
        ),
        LanguageOptions::pair_fn(
            "angular",
            ("https://github.com/dlvandenberg/tree-sitter-angular", None),
            &["@dlvandenberg"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "apex",
            ("https://github.com/aheber/tree-sitter-sfapex", None),
            &[
                ("apex::LANGUAGE", false),
                ("sflog::LANGUAGE", false),
                ("soql::LANGUAGE", false),
                ("sosl::LANGUAGE", false),
            ],
            ("sfapex", None),
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pair_const(
            "arduino",
            (
                "https://github.com/ObserverOfTime/tree-sitter-arduino",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "asm",
            ("https://github.com/RubixDev/tree-sitter-asm", None),
            &["@RubixDev"],
        ),
        // Fails to compile
        // LanguageOptions::pair_fn(
        //     "astro",
        //     ("https://github.com/virchau13/tree-sitter-astro", None),
        //     &["@virchau13"],
        // ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "authzed",
            ("https://github.com/mleonidas/tree-sitter-authzed", None),
            &[("language", true)],
            ("azd", None),
            &["@mattpolzin"],
        ),
        // Seems to freeze?
        // LanguageOptions::pair_fn(
        //     "awk",
        //     ("https://github.com/Beaglefoot/tree-sitter-awk", None),
        //     &[],
        // ),
        LanguageOptions::pair_const(
            "bash",
            ("https://github.com/tree-sitter/tree-sitter-bash", None),
            &["@TravonteD"],
        ),
        LanguageOptions::pair_const(
            "bass",
            ("https://github.com/vito/tree-sitter-bass", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "beancount",
            ("https://github.com/polarmutex/tree-sitter-beancount", None),
            &["@polarmutex"],
        ),
        LanguageOptions::pair_const(
            "bibtex",
            ("https://github.com/latex-lsp/tree-sitter-bibtex", None),
            &["@theHamsta", "@clason"],
        ),
        LanguageOptions::pair_const(
            "bicep",
            ("https://github.com/amaanq/tree-sitter-bicep", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "bitbake",
            ("https://github.com/amaanq/tree-sitter-bitbake", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "blueprint",
            ("https://gitlab.com/gabmus/tree-sitter-blueprint.git", None),
            &["@gabmus"],
        ),
        LanguageOptions::pair_const(
            "bp",
            ("https://github.com/ambroisie/tree-sitter-bp", None),
            &["@ambroisie"],
        ),
        LanguageOptions::pair_const(
            "c",
            ("https://github.com/tree-sitter/tree-sitter-c", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "c_sharp",
            ("https://github.com/tree-sitter/tree-sitter-c-sharp", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "cairo",
            ("https://github.com/amaanq/tree-sitter-cairo", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "capnp",
            ("https://github.com/amaanq/tree-sitter-capnp", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "chatito",
            (
                "https://github.com/ObserverOfTime/tree-sitter-chatito",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "circom",
            ("https://github.com/Decurity/tree-sitter-circom", None),
            &["@alexandr-martirosyan"],
        ),
        LanguageOptions::pair_const(
            "clojure",
            ("https://github.com/sogaiu/tree-sitter-clojure", None),
            &["@NoahTheDuke"],
        ),
        LanguageOptions::pair_const(
            "cmake",
            ("https://github.com/uyha/tree-sitter-cmake", None),
            &["@uyha"],
        ),
        LanguageOptions::pair_const(
            "comment",
            ("https://github.com/stsewd/tree-sitter-comment", None),
            &["@stsewd"],
        ),
        LanguageOptions::pair_const(
            "commonlisp",
            ("https://github.com/theHamsta/tree-sitter-commonlisp", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "cooklang",
            ("https://github.com/addcninblue/tree-sitter-cooklang", None),
            &["@addcninblue"],
        ),
        LanguageOptions::pair_const(
            "corn",
            ("https://github.com/jakestanger/tree-sitter-corn", None),
            &["@jakestanger"],
        ),
        LanguageOptions::pair_const(
            "cpon",
            ("https://github.com/amaanq/tree-sitter-cpon", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "cpp",
            ("https://github.com/tree-sitter/tree-sitter-cpp", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "css",
            ("https://github.com/tree-sitter/tree-sitter-css", None),
            &["@TravonteD"],
        ),
        LanguageOptions::pairs_with_symbol(
            "csv",
            ("https://github.com/amaanq/tree-sitter-csv", None),
            &[
                ("language_csv", true),
                ("language_psv", true),
                ("language_tsv", true),
            ],
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "cuda",
            ("https://github.com/theHamsta/tree-sitter-cuda", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "cue",
            ("https://github.com/eonpatapon/tree-sitter-cue", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "cylc",
            ("https://github.com/elliotfontaine/tree-sitter-cylc", None),
            &["@elliotfontaine"],
        ),
        LanguageOptions::pair_const(
            "d",
            ("https://github.com/gdamore/tree-sitter-d", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "dart",
            ("https://github.com/UserNobody14/tree-sitter-dart", None),
            &["@akinsho"],
        ),
        LanguageOptions::pair_const(
            "desktop",
            ("https://github.com/ValdezFOmar/tree-sitter-desktop", None),
            &["@ValdezFOmar"],
        ),
        LanguageOptions::pair_const(
            "devicetree",
            ("https://github.com/joelspadin/tree-sitter-devicetree", None),
            &["@jedrzejboczar"],
        ),
        LanguageOptions::pair_const(
            "dhall",
            ("https://github.com/jbellerb/tree-sitter-dhall", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "diff",
            ("https://github.com/the-mikedavis/tree-sitter-diff", None),
            &["@gbprod"],
        ),
        LanguageOptions::pair_const(
            "disassembly",
            (
                "https://github.com/ColinKennedy/tree-sitter-disassembly",
                None,
            ),
            &["@ColinKennedy"],
        ),
        LanguageOptions::pair_const(
            "djot",
            ("https://github.com/treeman/tree-sitter-djot", None),
            &["@NoahTheDuke"],
        ),
        LanguageOptions::pair_const(
            "dockerfile",
            (
                "https://github.com/camdencheek/tree-sitter-dockerfile",
                None,
            ),
            &["@camdencheek"],
        ),
        LanguageOptions::pair_const(
            "dot",
            ("https://github.com/rydesun/tree-sitter-dot", None),
            &["@rydesun"],
        ),
        LanguageOptions::pair_const(
            "doxygen",
            ("https://github.com/amaanq/tree-sitter-doxygen", None),
            &["@amaanq"],
        ),
        LanguageOptions::pairs_with_symbol(
            "dtd",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-xml",
                None,
            ),
            &[("LANGUAGE_DTD", false)],
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "duat_text",
            ("https://github.com/AhoyISki/tree-sitter-duat-text", None),
            &["AhoyISki"],
        ),
        LanguageOptions::pair_const(
            "earthfile",
            ("https://github.com/glehmann/tree-sitter-earthfile", None),
            &["@glehmann"],
        ),
        LanguageOptions::pair_fn(
            "ebnf",
            ("https://github.com/RubixDev/ebnf", None),
            &["@RubixDev"],
        ),
        LanguageOptions::pair_const(
            "editorconfig",
            (
                "https://github.com/ValdezFOmar/tree-sitter-editorconfig",
                None,
            ),
            &["@ValdezFOmar"],
        ),
        LanguageOptions::pair_const(
            "eds",
            ("https://github.com/uyha/tree-sitter-eds", None),
            &["@uyha"],
        ),
        LanguageOptions::pair_const(
            "eex",
            ("https://github.com/connorlay/tree-sitter-eex", None),
            &["@connorlay"],
        ),
        LanguageOptions::pair_const(
            "elixir",
            ("https://github.com/elixir-lang/tree-sitter-elixir", None),
            &["@connorlay"],
        ),
        LanguageOptions::pair_const(
            "elm",
            ("https://github.com/elm-tooling/tree-sitter-elm", None),
            &["@zweimach"],
        ),
        LanguageOptions::pair_const(
            "elsa",
            ("https://github.com/glapa-grossklag/tree-sitter-elsa", None),
            &["@glapa-grossklag", "@amaanq"],
        ),
        LanguageOptions::pair_const(
            "elvish",
            ("https://github.com/elves/tree-sitter-elvish", None),
            &["@elves"],
        ),
        LanguageOptions::pair_const(
            "embedded_template",
            (
                "https://github.com/tree-sitter/tree-sitter-embedded-template",
                None,
            ),
            &[],
        ),
        LanguageOptions::pair_const(
            "erlang",
            ("https://github.com/WhatsApp/tree-sitter-erlang", None),
            &["@filmor"],
        ),
        LanguageOptions::pair_const(
            "facility",
            ("https://github.com/FacilityApi/tree-sitter-facility", None),
            &["@bryankenote"],
        ),
        LanguageOptions::pair_const(
            "faust",
            ("https://github.com/khiner/tree-sitter-faust", None),
            &["@khiner"],
        ),
        LanguageOptions::pair_const(
            "fennel",
            ("https://github.com/alexmozaidze/tree-sitter-fennel", None),
            &["@alexmozaidze"],
        ),
        LanguageOptions::pair_const(
            "fidl",
            ("https://github.com/google/tree-sitter-fidl", None),
            &["@chaopeng"],
        ),
        LanguageOptions::pair_const(
            "firrtl",
            ("https://github.com/amaanq/tree-sitter-firrtl", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_fn(
            "fish",
            ("https://github.com/ram02z/tree-sitter-fish", None),
            &["@ram02z"],
        ),
        LanguageOptions::pair_const(
            "foam",
            ("https://github.com/FoamScience/tree-sitter-foam", None),
            &["@FoamScience"],
        ),
        LanguageOptions::pair_const(
            "forth",
            ("https://github.com/AlexanderBrevig/tree-sitter-forth", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "fortran",
            ("https://github.com/stadelmanma/tree-sitter-fortran", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "fsh",
            ("https://github.com/mgramigna/tree-sitter-fsh", None),
            &["@mgramigna"],
        ),
        LanguageOptions::pairs_with_symbol(
            "fsharp",
            ("https://github.com/ionide/tree-sitter-fsharp", None),
            &[("LANGUAGE_FSHARP", false)],
            &["@nsidorenco"],
        ),
        LanguageOptions::pair_const(
            "func",
            ("https://github.com/amaanq/tree-sitter-func", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "fusion",
            ("https://gitlab.com/jirgn/tree-sitter-fusion.git", None),
            &["@jirgn"],
        ),
        LanguageOptions::pair_const(
            "gap",
            ("https://github.com/gap-system/tree-sitter-gap", None),
            &["@reiniscirpons"],
        ),
        LanguageOptions::pair_const(
            "gaptst",
            ("https://github.com/gap-system/tree-sitter-gaptst", None),
            &["@reiniscirpons"],
        ),
        LanguageOptions::pair_const(
            "gdscript",
            ("https://github.com/PrestonKnopp/tree-sitter-gdscript", None),
            &["@PrestonKnopp"],
        ),
        LanguageOptions::pair_const(
            "gdshader",
            ("https://github.com/GodOfAvacyn/tree-sitter-gdshader", None),
            &["@godofavacyn"],
        ),
        LanguageOptions::pair_const(
            "git_rebase",
            (
                "https://github.com/the-mikedavis/tree-sitter-git-rebase",
                None,
            ),
            &["@gbprod"],
        ),
        LanguageOptions::pair_const(
            "gitattributes",
            (
                "https://github.com/ObserverOfTime/tree-sitter-gitattributes",
                None,
            ),
            &["@ObserverOfTime("],
        ),
        LanguageOptions::pair_const(
            "gitcommit",
            ("https://github.com/gbprod/tree-sitter-gitcommit", None),
            &["@gbprod"],
        ),
        LanguageOptions::pair_const(
            "git_config",
            (
                "https://github.com/the-mikedavis/tree-sitter-git-config",
                None,
            ),
            &["@amaanq("],
        ),
        LanguageOptions::pair_const(
            "gitignore",
            (
                "https://github.com/shunsambongi/tree-sitter-gitignore",
                None,
            ),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "gleam",
            ("https://github.com/gleam-lang/tree-sitter-gleam", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "glimmer",
            ("https://github.com/ember-tooling/tree-sitter-glimmer", None),
            &["@NullVoxPopuli"],
        ),
        LanguageOptions::pair_const(
            "glimmer_javascript",
            (
                "https://github.com/NullVoxPopuli/tree-sitter-glimmer-javascript",
                None,
            ),
            &["@NullVoxPopuli"],
        ),
        LanguageOptions::pair_const(
            "glimmer_typescript",
            (
                "https://github.com/NullVoxPopuli/tree-sitter-glimmer-typescript",
                None,
            ),
            &["@NullVoxPopuli"],
        ),
        LanguageOptions::pair_const(
            "glsl",
            ("https://github.com/theHamsta/tree-sitter-glsl", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "gn",
            ("https://github.com/amaanq/tree-sitter-gn", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "gnuplot",
            ("https://github.com/dpezto/tree-sitter-gnuplot", None),
            &["@dpezto"],
        ),
        LanguageOptions::pair_const(
            "go",
            ("https://github.com/tree-sitter/tree-sitter-go", None),
            &["@theHamsta", "@WinWisely268"],
        ),
        LanguageOptions::pair_const(
            "goctl",
            ("https://github.com/chaozwn/tree-sitter-goctl", None),
            &["@chaozwn"],
        ),
        LanguageOptions::pair_const(
            "godot_resource",
            (
                "https://github.com/PrestonKnopp/tree-sitter-godot-resource",
                None,
            ),
            &["@pierpo"],
        ),
        LanguageOptions::pair_const(
            "gomod",
            ("https://github.com/camdencheek/tree-sitter-go-mod", None),
            &["@camdencheek"],
        ),
        LanguageOptions::pair_const(
            "gosum",
            ("https://github.com/amaanq/tree-sitter-go-sum", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "gowork",
            ("https://github.com/omertuc/tree-sitter-go-work", None),
            &["@omertuc"],
        ),
        LanguageOptions::pair_const(
            "gotmpl",
            ("https://github.com/ngalaiko/tree-sitter-go-template", None),
            &["@qvalentin"],
        ),
        LanguageOptions::pair_const(
            "gpg",
            (
                "https://github.com/ObserverOfTime/tree-sitter-gpg-config",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "gren",
            ("https://github.com/MaeBrooks/tree-sitter-gren", None),
            &["@MaeBrooks"],
        ),
        LanguageOptions::pair_const(
            "groovy",
            ("https://github.com/murtaza64/tree-sitter-groovy", None),
            &["@murtaza64"],
        ),
        LanguageOptions::pair_const(
            "graphql",
            ("https://github.com/bkegley/tree-sitter-graphql", None),
            &["@bkegley"],
        ),
        LanguageOptions::pair_const(
            "gstlaunch",
            ("https://github.com/theHamsta/tree-sitter-gstlaunch", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "hack",
            ("https://github.com/slackhq/tree-sitter-hack", None),
            &[],
        ),
        LanguageOptions::pair_const(
            "hare",
            ("https://github.com/amaanq/tree-sitter-hare", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "haskell",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-haskell",
                None,
            ),
            &["@mrcjkb"],
        ),
        LanguageOptions::pair_const(
            "haskell_persistent",
            (
                "https://github.com/MercuryTechnologies/tree-sitter-haskell-persistent",
                None,
            ),
            &["@lykahb"],
        ),
        LanguageOptions::pair_const(
            "hcl",
            ("https://github.com/MichaHoffmann/tree-sitter-hcl", None),
            &["@MichaHoffmann"],
        ),
        LanguageOptions::pair_const(
            "heex",
            ("https://github.com/connorlay/tree-sitter-heex", None),
            &["@connorlay"],
        ),
        // No rust bindings 😠
        // LanguageOptions::pair_with_symbol(
        //     "helm",
        //     ("https://github.com/ngalaiko/tree-sitter-go-template", None),
        //     ("language", true),
        //     &[ "@qvalentin" ],
        // ),
        LanguageOptions::pair_const(
            "hjson",
            ("https://github.com/winston0410/tree-sitter-hjson", None),
            &["@winston0410"],
        ),
        LanguageOptions::pair_const(
            "hlsl",
            ("https://github.com/theHamsta/tree-sitter-hlsl", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "hocon",
            ("https://github.com/antosha417/tree-sitter-hocon", None),
            &["@antosha417"],
        ),
        LanguageOptions::pair_const(
            "hoon",
            ("https://github.com/urbit-pilled/tree-sitter-hoon", None),
            &["@urbit-pilled"],
        ),
        LanguageOptions::pair_const(
            "html",
            ("https://github.com/tree-sitter/tree-sitter-html", None),
            &["@TravonteD"],
        ),
        LanguageOptions::pair_const(
            "htmldjango",
            (
                "https://github.com/interdependence/tree-sitter-htmldjango",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "http",
            ("https://github.com/rest-nvim/tree-sitter-http", None),
            &["@amaanq", "@NTBBloodbath"],
        ),
        LanguageOptions::pair_const(
            "hurl",
            ("https://github.com/pfeiferj/tree-sitter-hurl", None),
            &["@pfeiferj"],
        ),
        LanguageOptions::pair_const(
            "hyprlang",
            (
                "https://github.com/luckasRanarison/tree-sitter-hyprlang",
                None,
            ),
            &["@luckasRanarison"],
        ),
        LanguageOptions::pair_const(
            "idl",
            ("https://github.com/cathaysia/tree-sitter-idl", None),
            &["@cathaysia"],
        ),
        LanguageOptions::pair_const(
            "idris",
            ("https://github.com/kayhide/tree-sitter-idris", None),
            &["@srghma"],
        ),
        LanguageOptions::pair_const(
            "ini",
            ("https://github.com/justinmk/tree-sitter-ini", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "inko",
            ("https://github.com/inko-lang/tree-sitter-inko", None),
            &["@yorickpeterse"],
        ),
        LanguageOptions::pair_const(
            "ipkg",
            ("https://github.com/srghma/tree-sitter-ipkg", None),
            &["@srghma"],
        ),
        LanguageOptions::pair_const(
            "ispc",
            ("https://github.com/fab4100/tree-sitter-ispc", None),
            &["@fab4100"],
        ),
        LanguageOptions::pair_const(
            "janet_simple",
            ("https://github.com/sogaiu/tree-sitter-janet-simple", None),
            &["@sogaiu"],
        ),
        LanguageOptions::pair_const(
            "java",
            ("https://github.com/tree-sitter/tree-sitter-java", None),
            &["@p00f"],
        ),
        LanguageOptions::pair_const(
            "javascript",
            (
                "https://github.com/tree-sitter/tree-sitter-javascript",
                None,
            ),
            &["@steelsojka"],
        ),
        LanguageOptions::pair_fn(
            "jq",
            ("https://github.com/flurie/tree-sitter-jq", None),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "jsdoc",
            ("https://github.com/tree-sitter/tree-sitter-jsdoc", None),
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const(
            "json",
            ("https://github.com/tree-sitter/tree-sitter-json", None),
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const(
            "json5",
            ("https://github.com/Joakker/tree-sitter-json5", None),
            &["@Joakker"],
        ),
        LanguageOptions::pair_const(
            "jsonc",
            ("https://gitlab.com/WhyNotHugo/tree-sitter-jsonc.git", None),
            &["@WhyNotHugo"],
        ),
        LanguageOptions::pair_const(
            "jsonnet",
            ("https://github.com/sourcegraph/tree-sitter-jsonnet", None),
            &["@nawordar"],
        ),
        LanguageOptions::pair_const(
            "julia",
            ("https://github.com/tree-sitter/tree-sitter-julia", None),
            &["@fredrikekre"],
        ),
        LanguageOptions::pair_const(
            "just",
            ("https://github.com/IndianBoy42/tree-sitter-just", None),
            &["@Hubro"],
        ),
        LanguageOptions::pair_fn(
            "kconfig",
            ("https://github.com/amaanq/tree-sitter-kconfig", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_fn(
            "kdl",
            ("https://github.com/amaanq/tree-sitter-kdl", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "kotlin",
            ("https://github.com/fwcd/tree-sitter-kotlin", None),
            &["@SalBakraa"],
        ),
        LanguageOptions::pair_const(
            "koto",
            ("https://github.com/koto-lang/tree-sitter-koto", None),
            &["@irh"],
        ),
        LanguageOptions::pair_const(
            "kusto",
            ("https://github.com/Willem-J-an/tree-sitter-kusto", None),
            &["@Willem-J-an"],
        ),
        LanguageOptions::pair_const(
            "lalrpop",
            ("https://github.com/traxys/tree-sitter-lalrpop", None),
            &["@traxys"],
        ),
        LanguageOptions::pair_const(
            "latex",
            ("https://github.com/latex-lsp/tree-sitter-latex", None),
            &["@theHamsta", "@clason"],
        ),
        LanguageOptions::pair_const(
            "ledger",
            ("https://github.com/cbarrete/tree-sitter-ledger", None),
            &["@cbarrete"],
        ),
        LanguageOptions::pair_const(
            "leo",
            ("https://github.com/r001/tree-sitter-leo", None),
            &["@r001"],
        ),
        LanguageOptions::pair_const(
            "llvm",
            ("https://github.com/benwilliamgraham/tree-sitter-llvm", None),
            &["@benwilliamgraham"],
        ),
        LanguageOptions::pair_const(
            "linkerscript",
            ("https://github.com/amaanq/tree-sitter-linkerscript", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "liquid",
            ("https://github.com/hankthetank27/tree-sitter-liquid", None),
            &["@hankthetank27"],
        ),
        LanguageOptions::pair_const(
            "liquidsoap",
            ("https://github.com/savonet/tree-sitter-liquidsoap", None),
            &["@toots"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "lua",
            ("https://github.com/MunifTanjim/tree-sitter-lua", None),
            &[("LANGUAGE", false)],
            ("lua", Some("0.3.1-dev.0")),
            &["@muniftanjim"],
        ),
        LanguageOptions::pair_const(
            "luadoc",
            ("https://github.com/amaanq/tree-sitter-luadoc", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "luap",
            ("https://github.com/amaanq/tree-sitter-luap", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "luau",
            ("https://github.com/amaanq/tree-sitter-luau", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "hlsplaylist",
            ("https://github.com/Freed-Wu/tree-sitter-hlsplaylist", None),
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair_const(
            "m68k",
            ("https://github.com/grahambates/tree-sitter-m68k", None),
            &["@grahambates"],
        ),
        LanguageOptions::pair_const(
            "make",
            ("https://github.com/alemuller/tree-sitter-make", None),
            &["@lewis6991"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "markdown",
            ("https://github.com/MDeiml/tree-sitter-markdown", None),
            &[("LANGUAGE", false), ("INLINE_LANGUAGE", false)],
            ("md", None),
            &["@MDeiml"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "markdown_inline",
            ("https://github.com/MDeiml/tree-sitter-markdown", None),
            &[("INLINE_LANGUAGE", false), ("LANGUAGE", false)],
            ("md", None),
            &["@MDeiml"],
        ),
        LanguageOptions::pair_const(
            "matlab",
            ("https://github.com/acristoffers/tree-sitter-matlab", None),
            &["@acristoffers"],
        ),
        LanguageOptions::pair_const(
            "menhir",
            ("https://github.com/Kerl13/tree-sitter-menhir", None),
            &["@Kerl13"],
        ),
        LanguageOptions::pair_const(
            "mermaid",
            ("https://github.com/monaqa/tree-sitter-mermaid", None),
            &[],
        ),
        LanguageOptions::pair_const(
            "meson",
            ("https://github.com/Decodetalkers/tree-sitter-meson", None),
            &["@Decodetalkers"],
        ),
        LanguageOptions::pair_const(
            "mlir",
            ("https://github.com/artagnon/tree-sitter-mlir", None),
            &["@artagnon"],
        ),
        LanguageOptions::pair_const(
            "muttrc",
            ("https://github.com/neomutt/tree-sitter-muttrc", None),
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair_const(
            "nasm",
            ("https://github.com/naclsn/tree-sitter-nasm", None),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "nginx",
            ("https://github.com/opa-oz/tree-sitter-nginx", None),
            &["@opa-oz"],
        ),
        LanguageOptions::pair_const(
            "nickel",
            ("https://github.com/nickel-lang/tree-sitter-nickel", None),
            &[],
        ),
        LanguageOptions::pair_const(
            "nim",
            ("https://github.com/alaviss/tree-sitter-nim", None),
            &["@aMOPel"],
        ),
        LanguageOptions::pair_const(
            "nim_format_string",
            (
                "https://github.com/aMOPel/tree-sitter-nim-format-string",
                None,
            ),
            &["@aMOPel"],
        ),
        LanguageOptions::pair_const(
            "ninja",
            ("https://github.com/alemuller/tree-sitter-ninja", None),
            &["@alemuller"],
        ),
        LanguageOptions::pair_const(
            "nix",
            ("https://github.com/cstrahan/tree-sitter-nix", None),
            &["@leo60228"],
        ),
        LanguageOptions::pair_const(
            "norg",
            ("https://github.com/nvim-neorg/tree-sitter-norg", None),
            &["@JoeyGrajciar", "@vhyrro"],
        ),
        LanguageOptions::pair_const(
            "nqc",
            ("https://github.com/amaanq/tree-sitter-nqc", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "nu",
            ("https://github.com/nushell/tree-sitter-nu", None),
            &["@abhisheksingh0x558"],
        ),
        LanguageOptions::pair_const(
            "objc",
            ("https://github.com/amaanq/tree-sitter-objc", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "objdump",
            ("https://github.com/ColinKennedy/tree-sitter-objdump", None),
            &["@ColinKennedy"],
        ),
        LanguageOptions::pairs_with_symbol(
            "ocaml",
            ("https://github.com/tree-sitter/tree-sitter-ocaml", None),
            &[
                ("LANGUAGE_OCAML", false),
                ("LANGUAGE_OCAML_INTERFACE", false),
            ],
            &["@undu"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "ocaml_interface",
            ("https://github.com/tree-sitter/tree-sitter-ocaml", None),
            &[
                ("LANGUAGE_OCAML_INTERFACE", false),
                ("LANGUAGE_OCAML", false),
            ],
            ("ocaml", None),
            &["@undu"],
        ),
        LanguageOptions::pair_const(
            "ocamllex",
            ("https://github.com/atom-ocaml/tree-sitter-ocamllex", None),
            &["@undu"],
        ),
        LanguageOptions::pair_const(
            "odin",
            ("https://github.com/amaanq/tree-sitter-odin", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "org",
            ("https://github.com/milisims/tree-sitter-org", None),
            &[],
        ),
        LanguageOptions::pair_const(
            "pascal",
            ("https://github.com/Isopod/tree-sitter-pascal", None),
            &["@Isopod"],
        ),
        LanguageOptions::pair_const(
            "passwd",
            ("https://github.com/ath3/tree-sitter-passwd", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "pem",
            ("https://github.com/ObserverOfTime/tree-sitter-pem", None),
            &["@ObserverOfTime"],
        ),
        // Fails to compile
        // LanguageOptions::pair_const(
        //     "perl",
        //     ("https://github.com/tree-sitter-perl/tree-sitter-perl", None),
        //     &["@RabbiVeesh", "@LeoNerd"],
        // ),
        LanguageOptions::pairs_with_symbol(
            "php",
            ("https://github.com/tree-sitter/tree-sitter-php", None),
            &[("LANGUAGE_PHP", false), ("LANGUAGE_PHP_ONLY", false)],
            &["@tk-shirasaka", "@calebdw"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "php_only",
            ("https://github.com/tree-sitter/tree-sitter-php", None),
            &[("LANGUAGE_PHP_ONLY", false), ("LANGUAGE_PHP", false)],
            ("php", None),
            &["@tk-shirasaka", "@calebdw"],
        ),
        LanguageOptions::pair_const(
            "phpdoc",
            ("https://github.com/claytonrcarter/tree-sitter-phpdoc", None),
            &["@mikehaertl"],
        ),
        LanguageOptions::pair_fn(
            "pioasm",
            ("https://github.com/leo60228/tree-sitter-pioasm", None),
            &["@leo60228"],
        ),
        LanguageOptions::pair_const(
            "po",
            ("https://github.com/erasin/tree-sitter-po", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "pod",
            ("https://github.com/tree-sitter-perl/tree-sitter-pod", None),
            &["@RabbiVeesh", "@LeoNerd"],
        ),
        LanguageOptions::pair_const(
            "poe_filter",
            (
                "https://github.com/ObserverOfTime/tree-sitter-poe-filter",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "pony",
            ("https://github.com/amaanq/tree-sitter-pony", None),
            &["@amaanq", "@mfelsche"],
        ),
        LanguageOptions::pair_const(
            "powershell",
            (
                "https://github.com/airbus-cert/tree-sitter-powershell",
                None,
            ),
            &["@L2jLiga"],
        ),
        LanguageOptions::pair_const(
            "printf",
            ("https://github.com/ObserverOfTime/tree-sitter-printf", None),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "prisma",
            ("https://github.com/victorhqc/tree-sitter-prisma", None),
            &["@elianiva"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "problog",
            ("https://github.com/foxyseta/tree-sitter-prolog", None),
            &[("problog", true), ("prolog", true)],
            ("prolog", None),
            &["@foxyseta"],
        ),
        LanguageOptions::pairs_with_symbol(
            "prolog",
            ("https://github.com/foxyseta/tree-sitter-prolog", None),
            &[("prolog", true), ("problog", true)],
            &["@foxyseta"],
        ),
        LanguageOptions::pair_const(
            "promql",
            ("https://github.com/MichaHoffmann/tree-sitter-promql", None),
            &["@MichaHoffmann"],
        ),
        LanguageOptions::pair_const(
            "properties",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-properties",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "proto",
            ("https://github.com/treywood/tree-sitter-proto", None),
            &["@treywood"],
        ),
        LanguageOptions::pair_const(
            "prql",
            ("https://github.com/PRQL/tree-sitter-prql", None),
            &["@matthias-Q"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "psv",
            ("https://github.com/amaanq/tree-sitter-csv", None),
            &[
                ("language_psv", true),
                ("language_csv", true),
                ("language_tsv", true),
            ],
            ("csv", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "pug",
            ("https://github.com/zealot128/tree-sitter-pug", None),
            &["@zealot128"],
        ),
        LanguageOptions::pair_const(
            "puppet",
            ("https://github.com/amaanq/tree-sitter-puppet", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "purescript",
            ("https://github.com/postsolar/tree-sitter-purescript", None),
            &["@postsolar"],
        ),
        LanguageOptions::pair_const(
            "pymanifest",
            (
                "https://github.com/ObserverOfTime/tree-sitter-pymanifest",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "python",
            ("https://github.com/tree-sitter/tree-sitter-python", None),
            &["@stsewd", "@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "ql",
            ("https://github.com/tree-sitter/tree-sitter-ql", None),
            &["@pwntester"],
        ),
        LanguageOptions::pair_const(
            "qmldir",
            ("https://github.com/Decodetalkers/tree-sitter-qmldir", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "qmljs",
            ("https://github.com/yuja/tree-sitter-qmljs", None),
            &["@Decodetalkers"],
        ),
        LanguageOptions::pair_const(
            "query",
            ("https://github.com/nvim-treesitter/tree-sitter-query", None),
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const(
            "r",
            ("https://github.com/r-lib/tree-sitter-r", None),
            &["@ribru17"],
        ),
        LanguageOptions::pair_const(
            "racket",
            ("https://github.com/6cdh/tree-sitter-racket", None),
            &[],
        ),
        LanguageOptions::pair_const(
            "ralph",
            ("https://github.com/alephium/tree-sitter-ralph", None),
            &["@tdroxler"],
        ),
        LanguageOptions::pair_const(
            "rasi",
            ("https://github.com/Fymyte/tree-sitter-rasi", None),
            &["@Fymyte"],
        ),
        LanguageOptions::pair_const(
            "rbs",
            ("https://github.com/joker1007/tree-sitter-rbs", None),
            &["@joker1007"],
        ),
        LanguageOptions::pair_const(
            "re2c",
            ("https://github.com/amaanq/tree-sitter-re2c", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "readline",
            ("https://github.com/ribru17/tree-sitter-readline", None),
            &["@ribru17"],
        ),
        LanguageOptions::pair_const(
            "regex",
            ("https://github.com/tree-sitter/tree-sitter-regex", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "rego",
            ("https://github.com/FallenAngel97/tree-sitter-rego", None),
            &["@FallenAngel97"],
        ),
        LanguageOptions::pair_const(
            "requirements",
            (
                "https://github.com/ObserverOfTime/tree-sitter-requirements",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "rescript",
            (
                "https://github.com/rescript-lang/tree-sitter-rescript",
                None,
            ),
            &["@ribru17"],
        ),
        LanguageOptions::pair_const(
            "rnoweb",
            ("https://github.com/bamonroe/tree-sitter-rnoweb", None),
            &["@bamonroe"],
        ),
        LanguageOptions::pair_const(
            "robot",
            ("https://github.com/Hubro/tree-sitter-robot", None),
            &["@Hubro"],
        ),
        LanguageOptions::pair_const(
            "robots",
            ("https://github.com/opa-oz/tree-sitter-robots-txt", None),
            &["@opa-oz"],
        ),
        LanguageOptions::pair_const(
            "roc",
            ("https://github.com/faldor20/tree-sitter-roc", None),
            &["@nat-418"],
        ),
        LanguageOptions::pair_const(
            "ron",
            ("https://github.com/amaanq/tree-sitter-ron", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "rst",
            ("https://github.com/stsewd/tree-sitter-rst", None),
            &["@stsewd"],
        ),
        LanguageOptions::pair_const(
            "ruby",
            ("https://github.com/tree-sitter/tree-sitter-ruby", None),
            &["@TravonteD"],
        ),
        LanguageOptions::pair_const(
            "runescript",
            ("https://github.com/2004Scape/tree-sitter-runescript", None),
            &["@2004Scape"],
        ),
        LanguageOptions::pair_const(
            "rust",
            ("https://github.com/tree-sitter/tree-sitter-rust", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "scala",
            ("https://github.com/tree-sitter/tree-sitter-scala", None),
            &["@stevanmilic"],
        ),
        LanguageOptions::pair_const(
            "scfg",
            ("https://github.com/rockorager/tree-sitter-scfg", None),
            &["@WhyNotHugo"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "scheme",
            ("https://github.com/6cdh/tree-sitter-scheme", None),
            &[("LANGUAGE", false)],
            ("scheme", Some("0.24.7-1")),
            &[],
        ),
        LanguageOptions::pair_const(
            "scss",
            ("https://github.com/serenadeai/tree-sitter-scss", None),
            &["@elianiva"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "sflog",
            ("https://github.com/aheber/tree-sitter-sfapex", None),
            &[
                ("sflog::LANGUAGE", false),
                ("apex::LANGUAGE", false),
                ("soql::LANGUAGE", false),
                ("sosl::LANGUAGE", false),
            ],
            ("sfapex", None),
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pair_const(
            "slang",
            ("https://github.com/theHamsta/tree-sitter-slang", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "slim",
            ("https://github.com/theoo/tree-sitter-slim", None),
            &["@theoo"],
        ),
        LanguageOptions::pair_const(
            "slint",
            ("https://github.com/slint-ui/tree-sitter-slint", None),
            &["@hunger"],
        ),
        LanguageOptions::pair_const(
            "smali",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-smali",
                None,
            ),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "snakemake",
            ("https://github.com/osthomas/tree-sitter-snakemake", None),
            &[],
        ),
        LanguageOptions::pair_const(
            "smithy",
            ("https://github.com/indoorvivants/tree-sitter-smithy", None),
            &["@amaanq", "@keynmol"],
        ),
        LanguageOptions::pair_const(
            "solidity",
            ("https://github.com/JoranHonig/tree-sitter-solidity", None),
            &["@amaanq"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "soql",
            ("https://github.com/aheber/tree-sitter-sfapex", None),
            &[
                ("soql::LANGUAGE", false),
                ("sflog::LANGUAGE", false),
                ("apex::LANGUAGE", false),
                ("sosl::LANGUAGE", false),
            ],
            ("sfapex", None),
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "sosl",
            ("https://github.com/aheber/tree-sitter-sfapex", None),
            &[
                ("soql::LANGUAGE", false),
                ("sflog::LANGUAGE", false),
                ("apex::LANGUAGE", false),
                ("sosl::LANGUAGE", false),
            ],
            ("sfapex", None),
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pair_const(
            "sourcepawn",
            ("https://github.com/nilshelmig/tree-sitter-sourcepawn", None),
            &["@Sarrus1"],
        ),
        LanguageOptions::pair_const(
            "sparql",
            ("https://github.com/GordianDziwis/tree-sitter-sparql", None),
            &["@GordianDziwis"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "sql",
            (
                "https://github.com/DerekStride/tree-sitter-sql",
                Some("gh-pages"),
            ),
            &[("LANGUAGE", false)],
            ("sequel", None),
            &["@derekstride"],
        ),
        LanguageOptions::pair_const(
            "squirrel",
            ("https://github.com/amaanq/tree-sitter-squirrel", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "ssh_config",
            (
                "https://github.com/ObserverOfTime/tree-sitter-ssh-config",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "starlark",
            ("https://github.com/amaanq/tree-sitter-starlark", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "strace",
            ("https://github.com/sigmaSd/tree-sitter-strace", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "styled",
            ("https://github.com/mskelton/tree-sitter-styled", None),
            &["@mskelton"],
        ),
        LanguageOptions::pair_const(
            "supercollider",
            (
                "https://github.com/madskjeldgaard/tree-sitter-supercollider",
                None,
            ),
            &["@madskjeldgaard"],
        ),
        LanguageOptions::pairs_with_symbol(
            "superhtml",
            ("https://github.com/kristoff-it/superhtml", None),
            &[("function", true)],
            &["@rockorager"],
        ),
        LanguageOptions::pair_const(
            "surface",
            ("https://github.com/connorlay/tree-sitter-surface", None),
            &["@connorlay"],
        ),
        LanguageOptions::pair_const(
            "svelte",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-svelte",
                None,
            ),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "sway",
            ("https://github.com/FuelLabs/tree-sitter-sway.git", None),
            &["@ribru17"],
        ),
        LanguageOptions::pair_const(
            "swift",
            ("https://github.com/alex-pinkus/tree-sitter-swift", None),
            &["@alex-pinkus"],
        ),
        LanguageOptions::pair_const(
            "sxhkdrc",
            ("https://github.com/RaafatTurki/tree-sitter-sxhkdrc", None),
            &["@RaafatTurki"],
        ),
        LanguageOptions::pair_const(
            "systemtap",
            ("https://github.com/ok-ryoko/tree-sitter-systemtap", None),
            &["@ok-ryoko"],
        ),
        LanguageOptions::pair_const(
            "t32",
            ("https://gitlab.com/xasc/tree-sitter-t32.git", None),
            &["@xasc"],
        ),
        LanguageOptions::pair_const(
            "tablegen",
            ("https://github.com/amaanq/tree-sitter-tablegen", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "tact",
            ("https://github.com/tact-lang/tree-sitter-tact", None),
            &["@novusnota"],
        ),
        LanguageOptions::pair_const(
            "teal",
            ("https://github.com/euclidianAce/tree-sitter-teal", None),
            &["@euclidianAce"],
        ),
        LanguageOptions::pair_const(
            "templ",
            ("https://github.com/vrischmann/tree-sitter-templ", None),
            &["@vrischmann"],
        ),
        LanguageOptions::pair_const(
            "tcl",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-tcl",
                None,
            ),
            &["@lewis6991"],
        ),
        // No rust bindings 😠
        // LanguageOptions::pair_with_symbol_and_crate(
        //     "terraform",
        //     ("https://github.com/MichaHoffmann/tree-sitter-hcl", None),
        //     ("LANGUAGE", false),
        //     "hcl",
        //     &[ "@MichaHoffmann" ],
        // ),
        LanguageOptions::pair_const(
            "textproto",
            (
                "https://github.com/PorterAtGoogle/tree-sitter-textproto",
                None,
            ),
            &["@Porter"],
        ),
        LanguageOptions::pair_const(
            "thrift",
            ("https://github.com/duskmoon314/tree-sitter-thrift", None),
            &["@amaanq", "@duskmoon314"],
        ),
        LanguageOptions::pair_const(
            "tiger",
            ("https://github.com/ambroisie/tree-sitter-tiger", None),
            &["@ambroisie"],
        ),
        LanguageOptions::pair_const(
            "tlaplus",
            (
                "https://github.com/tlaplus-community/tree-sitter-tlaplus",
                None,
            ),
            &["@ahelwer", "@susliko"],
        ),
        LanguageOptions::pair_const(
            "tmux",
            ("https://github.com/Freed-Wu/tree-sitter-tmux", None),
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair_const(
            "todotxt",
            ("https://github.com/arnarg/tree-sitter-todotxt", None),
            &["@arnarg"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "toml",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-toml",
                None,
            ),
            &[("LANGUAGE", false)],
            ("toml-ng", None),
            &["@tk-shirasaka"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "tsv",
            ("https://github.com/amaanq/tree-sitter-csv", None),
            &[
                ("language_tsv", true),
                ("language_csv", true),
                ("language_psv", true),
            ],
            ("csv", None),
            &["@amaanq"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "tsx",
            (
                "https://github.com/tree-sitter/tree-sitter-typescript",
                None,
            ),
            &[("LANGUAGE_TSX", false), ("LANGUAGE_TYPESCRIPT", false)],
            ("typescript", None),
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const(
            "turtle",
            ("https://github.com/GordianDziwis/tree-sitter-turtle", None),
            &["@GordianDziwis"],
        ),
        LanguageOptions::pair_const(
            "twig",
            ("https://github.com/gbprod/tree-sitter-twig", None),
            &["@gbprod"],
        ),
        LanguageOptions::pairs_with_symbol(
            "typescript",
            (
                "https://github.com/tree-sitter/tree-sitter-typescript",
                None,
            ),
            &[("LANGUAGE_TYPESCRIPT", false), ("LANGUAGE_TSX", false)],
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const(
            "typespec",
            ("https://github.com/happenslol/tree-sitter-typespec", None),
            &["@happenslol"],
        ),
        LanguageOptions::pair_const(
            "typoscript",
            (
                "https://github.com/Teddytrombone/tree-sitter-typoscript",
                None,
            ),
            &["@Teddytrombone"],
        ),
        // Fails to compile
        // LanguageOptions::pair_fn("typst", ("https://github.com/uben0/tree-sitter-typst", None), &[
        //     "@uben0",
        //     "@RaafatTurki",
        // ]),
        LanguageOptions::pair_const(
            "udev",
            ("https://github.com/ObserverOfTime/tree-sitter-udev", None),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "ungrammar",
            ("https://github.com/Philipp-M/tree-sitter-ungrammar", None),
            &["@Philipp-M", "@amaanq"],
        ),
        LanguageOptions::pair_fn(
            "unison",
            ("https://github.com/kylegoetz/tree-sitter-unison", None),
            &["@tapegram"],
        ),
        LanguageOptions::pair_const(
            "usd",
            ("https://github.com/ColinKennedy/tree-sitter-usd", None),
            &["@ColinKennedy"],
        ),
        LanguageOptions::pair_const(
            "uxntal",
            ("https://github.com/amaanq/tree-sitter-uxntal", None),
            &["@amaanq"],
        ),
        // No rust bindings 😠
        // LanguageOptions::new(
        //     "v",
        //     ("https://github.com/vlang/v-analyzer", None),
        //     ("LANGUAGE", false),
        //     "tree_sitter_v",
        //     &["@kkharji", "@amaanq"],
        // ),
        LanguageOptions::pair_const(
            "vala",
            ("https://github.com/vala-lang/tree-sitter-vala", None),
            &["@Prince781"],
        ),
        LanguageOptions::pair_const(
            "vento",
            ("https://github.com/ventojs/tree-sitter-vento", None),
            &["@wrapperup", "@oscarotero"],
        ),
        LanguageOptions::pair_const(
            "verilog",
            ("https://github.com/tree-sitter/tree-sitter-verilog", None),
            &["@zhangwwpeng"],
        ),
        LanguageOptions::pair_const(
            "vhdl",
            ("https://github.com/jpt13653903/tree-sitter-vhdl", None),
            &["@jpt13653903"],
        ),
        LanguageOptions::pair_const(
            "vhs",
            ("https://github.com/charmbracelet/tree-sitter-vhs", None),
            &["@caarlos0"],
        ),
        LanguageOptions::pair_const(
            "vim",
            ("https://github.com/neovim/tree-sitter-vim", None),
            &["@clason"],
        ),
        LanguageOptions::pair_const(
            "vimdoc",
            ("https://github.com/neovim/tree-sitter-vimdoc", None),
            &["@clason"],
        ),
        LanguageOptions::pair_const(
            "vrl",
            ("https://github.com/belltoy/tree-sitter-vrl", None),
            &["@belltoy"],
        ),
        LanguageOptions::pair_const(
            "vue",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-vue",
                None,
            ),
            &["@WhyNotHugo", "@lucario387"],
        ),
        LanguageOptions::pair_const(
            "wgsl",
            ("https://github.com/szebniok/tree-sitter-wgsl", None),
            &["@szebniok"],
        ),
        LanguageOptions::pair_const(
            "wgsl_bevy",
            ("https://github.com/theHamsta/tree-sitter-wgsl-bevy", None),
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "wing",
            ("https://github.com/winglang/tree-sitter-wing", None),
            &["@gshpychka", "@MarkMcCulloh"],
        ),
        LanguageOptions::pair_const(
            "wit",
            ("https://github.com/liamwh/tree-sitter-wit", None),
            &["@liamwh"],
        ),
        LanguageOptions::pair_const(
            "xcompose",
            (
                "https://github.com/ObserverOfTime/tree-sitter-xcompose",
                None,
            ),
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pairs_with_symbol(
            "xml",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-xml",
                None,
            ),
            &[("LANGUAGE_XML", false)],
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "xresources",
            (
                "https://github.com/ValdezFOmar/tree-sitter-xresources",
                None,
            ),
            &["@ValdezFOmar"],
        ),
        LanguageOptions::pair_const(
            "yaml",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-yaml",
                None,
            ),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "yang",
            ("https://github.com/Hubro/tree-sitter-yang", None),
            &["@Hubro"],
        ),
        LanguageOptions::pair_fn(
            "yuck",
            ("https://github.com/Philipp-M/tree-sitter-yuck", None),
            &["@Philipp-M", "@amaanq"],
        ),
        LanguageOptions::pair_fn(
            "zathurarc",
            ("https://github.com/Freed-Wu/tree-sitter-zathurarc", None),
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair_const(
            "zig",
            (
                "https://github.com/tree-sitter-grammars/tree-sitter-zig",
                None,
            ),
            &["@amaanq"],
        ),
        LanguageOptions::pair_fn(
            "ziggy",
            ("https://github.com/kristoff-it/ziggy", None),
            &["@rockorager"],
        ),
        LanguageOptions::pair_fn(
            "ziggy_schema",
            ("https://github.com/kristoff-it/ziggy", None),
            &["@rockorager"],
        ),
    ])
});
