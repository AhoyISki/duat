use std::{collections::HashMap, sync::LazyLock};

use super::LanguageOptions;

pub static LANGUAGE_OPTIONS: LazyLock<HashMap<&str, LanguageOptions>> = LazyLock::new(|| {
    HashMap::from_iter([
        LanguageOptions::pair("ada", "https://github.com/briot/tree-sitter-ada", &[
            "@briot",
        ]),
        LanguageOptions::pair(
            "agda",
            "https://github.com/tree-sitter/tree-sitter-agda",
            &["@Decodetalkers"],
        ),
        LanguageOptions::pairs_with_symbol(
            "angular",
            "https://github.com/dlvandenberg/tree-sitter-angular",
            &[("language", true)],
            &["@dlvandenberg"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "apex",
            "https://github.com/aheber/tree-sitter-sfapex",
            &[
                ("apex::LANGUAGE", false),
                ("sflog::LANGUAGE", false),
                ("soql::LANGUAGE", false),
                ("sosl::LANGUAGE", false),
            ],
            "sfapex",
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pair(
            "arduino",
            "https://github.com/ObserverOfTime/tree-sitter-arduino",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair("asm", "https://github.com/RubixDev/tree-sitter-asm", &[
            "@RubixDev",
        ]),
        LanguageOptions::pair(
            "astro",
            "https://github.com/virchau13/tree-sitter-astro",
            &["@virchau13"],
        ),
        LanguageOptions::pair(
            "authzed",
            "https://github.com/mleonidas/tree-sitter-authzed",
            &["@mattpolzin"],
        ),
        LanguageOptions::pair("awk", "https://github.com/Beaglefoot/tree-sitter-awk", &[]),
        LanguageOptions::pair(
            "bash",
            "https://github.com/tree-sitter/tree-sitter-bash",
            &["@TravonteD"],
        ),
        LanguageOptions::pair("bass", "https://github.com/vito/tree-sitter-bass", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "beancount",
            "https://github.com/polarmutex/tree-sitter-beancount",
            &["@polarmutex"],
        ),
        LanguageOptions::pair(
            "bibtex",
            "https://github.com/latex-lsp/tree-sitter-bibtex",
            &["@theHamsta", "@clason"],
        ),
        LanguageOptions::pair("bicep", "https://github.com/amaanq/tree-sitter-bicep", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "bitbake",
            "https://github.com/amaanq/tree-sitter-bitbake",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "blueprint",
            "https://gitlab.com/gabmus/tree-sitter-blueprint.git",
            &["@gabmus"],
        ),
        LanguageOptions::pair("bp", "https://github.com/ambroisie/tree-sitter-bp", &[
            "@ambroisie",
        ]),
        LanguageOptions::pair("c", "https://github.com/tree-sitter/tree-sitter-c", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "c_sharp",
            "https://github.com/tree-sitter/tree-sitter-c-sharp",
            &["@amaanq"],
        ),
        LanguageOptions::pair("cairo", "https://github.com/amaanq/tree-sitter-cairo", &[
            "@amaanq",
        ]),
        LanguageOptions::pair("capnp", "https://github.com/amaanq/tree-sitter-capnp", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "chatito",
            "https://github.com/ObserverOfTime/tree-sitter-chatito",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "circom",
            "https://github.com/Decurity/tree-sitter-circom",
            &["@alexandr-martirosyan"],
        ),
        LanguageOptions::pair(
            "clojure",
            "https://github.com/sogaiu/tree-sitter-clojure",
            &["@NoahTheDuke"],
        ),
        LanguageOptions::pair("cmake", "https://github.com/uyha/tree-sitter-cmake", &[
            "@uyha",
        ]),
        LanguageOptions::pair(
            "comment",
            "https://github.com/stsewd/tree-sitter-comment",
            &["@stsewd"],
        ),
        LanguageOptions::pair(
            "commonlisp",
            "https://github.com/theHamsta/tree-sitter-commonlisp",
            &["@theHamsta"],
        ),
        LanguageOptions::pair(
            "cooklang",
            "https://github.com/addcninblue/tree-sitter-cooklang",
            &["@addcninblue"],
        ),
        LanguageOptions::pair(
            "corn",
            "https://github.com/jakestanger/tree-sitter-corn",
            &["@jakestanger"],
        ),
        LanguageOptions::pair("cpon", "https://github.com/amaanq/tree-sitter-cpon", &[
            "@amaanq",
        ]),
        LanguageOptions::pair("cpp", "https://github.com/tree-sitter/tree-sitter-cpp", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair("css", "https://github.com/tree-sitter/tree-sitter-css", &[
            "@TravonteD",
        ]),
        LanguageOptions::pairs_with_symbol(
            "csv",
            "https://github.com/amaanq/tree-sitter-csv",
            &[
                ("language_csv", true),
                ("language_psv", true),
                ("language_tsv", true),
            ],
            &["@amaanq"],
        ),
        LanguageOptions::pair("cuda", "https://github.com/theHamsta/tree-sitter-cuda", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair("cue", "https://github.com/eonpatapon/tree-sitter-cue", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "cylc",
            "https://github.com/elliotfontaine/tree-sitter-cylc",
            &["@elliotfontaine"],
        ),
        LanguageOptions::pair("d", "https://github.com/gdamore/tree-sitter-d", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "dart",
            "https://github.com/UserNobody14/tree-sitter-dart",
            &["@akinsho"],
        ),
        LanguageOptions::pair(
            "desktop",
            "https://github.com/ValdezFOmar/tree-sitter-desktop",
            &["@ValdezFOmar"],
        ),
        LanguageOptions::pair(
            "devicetree",
            "https://github.com/joelspadin/tree-sitter-devicetree",
            &["@jedrzejboczar"],
        ),
        LanguageOptions::pair("dhall", "https://github.com/jbellerb/tree-sitter-dhall", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "diff",
            "https://github.com/the-mikedavis/tree-sitter-diff",
            &["@gbprod"],
        ),
        LanguageOptions::pair(
            "disassembly",
            "https://github.com/ColinKennedy/tree-sitter-disassembly",
            &["@ColinKennedy"],
        ),
        LanguageOptions::pair("djot", "https://github.com/treeman/tree-sitter-djot", &[
            "@NoahTheDuke",
        ]),
        LanguageOptions::pair(
            "dockerfile",
            "https://github.com/camdencheek/tree-sitter-dockerfile",
            &["@camdencheek"],
        ),
        LanguageOptions::pair("dot", "https://github.com/rydesun/tree-sitter-dot", &[
            "@rydesun",
        ]),
        LanguageOptions::pair(
            "doxygen",
            "https://github.com/amaanq/tree-sitter-doxygen",
            &["@amaanq"],
        ),
        LanguageOptions::pairs_with_symbol(
            "dtd",
            "https://github.com/tree-sitter-grammars/tree-sitter-xml",
            &[("LANGUAGE_DTD", false)],
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "duat_text",
            "https://github.com/AhoyISki/tree-sitter-duat-text",
            &["AhoyISki"],
        ),
        LanguageOptions::pair(
            "earthfile",
            "https://github.com/glehmann/tree-sitter-earthfile",
            &["@glehmann"],
        ),
        LanguageOptions::pairs_with_symbol(
            "ebnf",
            "https://github.com/RubixDev/ebnf",
            &[("language", true)],
            &["@RubixDev"],
        ),
        LanguageOptions::pair(
            "editorconfig",
            "https://github.com/ValdezFOmar/tree-sitter-editorconfig",
            &["@ValdezFOmar"],
        ),
        LanguageOptions::pair("eds", "https://github.com/uyha/tree-sitter-eds", &["@uyha"]),
        LanguageOptions::pair("eex", "https://github.com/connorlay/tree-sitter-eex", &[
            "@connorlay",
        ]),
        LanguageOptions::pair(
            "elixir",
            "https://github.com/elixir-lang/tree-sitter-elixir",
            &["@connorlay"],
        ),
        LanguageOptions::pair("elm", "https://github.com/elm-tooling/tree-sitter-elm", &[
            "@zweimach",
        ]),
        LanguageOptions::pair(
            "elsa",
            "https://github.com/glapa-grossklag/tree-sitter-elsa",
            &["@glapa-grossklag", "@amaanq"],
        ),
        LanguageOptions::pair("elvish", "https://github.com/elves/tree-sitter-elvish", &[
            "@elves",
        ]),
        LanguageOptions::pair(
            "embedded_template",
            "https://github.com/tree-sitter/tree-sitter-embedded-template",
            &[],
        ),
        LanguageOptions::pair(
            "erlang",
            "https://github.com/WhatsApp/tree-sitter-erlang",
            &["@filmor"],
        ),
        LanguageOptions::pair(
            "facility",
            "https://github.com/FacilityApi/tree-sitter-facility",
            &["@bryankenote"],
        ),
        LanguageOptions::pair("faust", "https://github.com/khiner/tree-sitter-faust", &[
            "@khiner",
        ]),
        LanguageOptions::pair(
            "fennel",
            "https://github.com/alexmozaidze/tree-sitter-fennel",
            &["@alexmozaidze"],
        ),
        LanguageOptions::pair("fidl", "https://github.com/google/tree-sitter-fidl", &[
            "@chaopeng",
        ]),
        LanguageOptions::pair("firrtl", "https://github.com/amaanq/tree-sitter-firrtl", &[
            "@amaanq",
        ]),
        LanguageOptions::pair("fish", "https://github.com/ram02z/tree-sitter-fish", &[
            "@ram02z",
        ]),
        LanguageOptions::pair(
            "foam",
            "https://github.com/FoamScience/tree-sitter-foam",
            &["@FoamScience"],
        ),
        LanguageOptions::pair(
            "forth",
            "https://github.com/AlexanderBrevig/tree-sitter-forth",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "fortran",
            "https://github.com/stadelmanma/tree-sitter-fortran",
            &["@amaanq"],
        ),
        LanguageOptions::pair("fsh", "https://github.com/mgramigna/tree-sitter-fsh", &[
            "@mgramigna",
        ]),
        LanguageOptions::pairs_with_symbol(
            "fsharp",
            "https://github.com/ionide/tree-sitter-fsharp",
            &[("LANGUAGE_FSHARP", false)],
            &["@nsidorenco"],
        ),
        LanguageOptions::pair("func", "https://github.com/amaanq/tree-sitter-func", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "fusion",
            "https://gitlab.com/jirgn/tree-sitter-fusion.git",
            &["@jirgn"],
        ),
        LanguageOptions::pair("gap", "https://github.com/gap-system/tree-sitter-gap", &[
            "@reiniscirpons",
        ]),
        LanguageOptions::pair(
            "gaptst",
            "https://github.com/gap-system/tree-sitter-gaptst",
            &["@reiniscirpons"],
        ),
        LanguageOptions::pair(
            "gdscript",
            "https://github.com/PrestonKnopp/tree-sitter-gdscript",
            &["@PrestonKnopp"],
        ),
        LanguageOptions::pair(
            "gdshader",
            "https://github.com/GodOfAvacyn/tree-sitter-gdshader",
            &["@godofavacyn"],
        ),
        LanguageOptions::pair(
            "git_rebase",
            "https://github.com/the-mikedavis/tree-sitter-git-rebase",
            &["@gbprod"],
        ),
        LanguageOptions::pair(
            "gitattributes",
            "https://github.com/ObserverOfTime/tree-sitter-gitattributes",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "gitcommit",
            "https://github.com/gbprod/tree-sitter-gitcommit",
            &["@gbprod"],
        ),
        LanguageOptions::pair(
            "git_config",
            "https://github.com/the-mikedavis/tree-sitter-git-config",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "gitignore",
            "https://github.com/shunsambongi/tree-sitter-gitignore",
            &["@theHamsta"],
        ),
        LanguageOptions::pair(
            "gleam",
            "https://github.com/gleam-lang/tree-sitter-gleam",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "glimmer",
            "https://github.com/ember-tooling/tree-sitter-glimmer",
            &["@NullVoxPopuli"],
        ),
        LanguageOptions::pair(
            "glimmer_javascript",
            "https://github.com/NullVoxPopuli/tree-sitter-glimmer-javascript",
            &["@NullVoxPopuli"],
        ),
        LanguageOptions::pair(
            "glimmer_typescript",
            "https://github.com/NullVoxPopuli/tree-sitter-glimmer-typescript",
            &["@NullVoxPopuli"],
        ),
        LanguageOptions::pair("glsl", "https://github.com/theHamsta/tree-sitter-glsl", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair("gn", "https://github.com/amaanq/tree-sitter-gn", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "gnuplot",
            "https://github.com/dpezto/tree-sitter-gnuplot",
            &["@dpezto"],
        ),
        LanguageOptions::pair("go", "https://github.com/tree-sitter/tree-sitter-go", &[
            "@theHamsta",
            "@WinWisely268",
        ]),
        LanguageOptions::pair("goctl", "https://github.com/chaozwn/tree-sitter-goctl", &[
            "@chaozwn",
        ]),
        LanguageOptions::pair(
            "godot_resource",
            "https://github.com/PrestonKnopp/tree-sitter-godot-resource",
            &["@pierpo"],
        ),
        LanguageOptions::pair(
            "gomod",
            "https://github.com/camdencheek/tree-sitter-go-mod",
            &["@camdencheek"],
        ),
        LanguageOptions::pair("gosum", "https://github.com/amaanq/tree-sitter-go-sum", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "gowork",
            "https://github.com/omertuc/tree-sitter-go-work",
            &["@omertuc"],
        ),
        LanguageOptions::pair(
            "gotmpl",
            "https://github.com/ngalaiko/tree-sitter-go-template",
            &["@qvalentin"],
        ),
        LanguageOptions::pair(
            "gpg",
            "https://github.com/ObserverOfTime/tree-sitter-gpg-config",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair("gren", "https://github.com/MaeBrooks/tree-sitter-gren", &[
            "@MaeBrooks",
        ]),
        LanguageOptions::pair(
            "groovy",
            "https://github.com/murtaza64/tree-sitter-groovy",
            &["@murtaza64"],
        ),
        LanguageOptions::pair(
            "graphql",
            "https://github.com/bkegley/tree-sitter-graphql",
            &["@bkegley"],
        ),
        LanguageOptions::pair(
            "gstlaunch",
            "https://github.com/theHamsta/tree-sitter-gstlaunch",
            &["@theHamsta"],
        ),
        LanguageOptions::pair("hack", "https://github.com/slackhq/tree-sitter-hack", &[]),
        LanguageOptions::pair("hare", "https://github.com/amaanq/tree-sitter-hare", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "haskell",
            "https://github.com/tree-sitter/tree-sitter-haskell",
            &["@mrcjkb"],
        ),
        LanguageOptions::pair(
            "haskell_persistent",
            "https://github.com/MercuryTechnologies/tree-sitter-haskell-persistent",
            &["@lykahb"],
        ),
        LanguageOptions::pair(
            "hcl",
            "https://github.com/MichaHoffmann/tree-sitter-hcl",
            &["@MichaHoffmann"],
        ),
        LanguageOptions::pair("heex", "https://github.com/connorlay/tree-sitter-heex", &[
            "@connorlay",
        ]),
        // No rust bindings 😠
        // LanguageOptions::pair_with_symbol(
        //     "helm",
        //     "https://github.com/ngalaiko/tree-sitter-go-template",
        //     ("language", true),
        //     &[ "@qvalentin" ],
        // ),
        LanguageOptions::pair(
            "hjson",
            "https://github.com/winston0410/tree-sitter-hjson",
            &["@winston0410"],
        ),
        LanguageOptions::pair("hlsl", "https://github.com/theHamsta/tree-sitter-hlsl", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair(
            "hocon",
            "https://github.com/antosha417/tree-sitter-hocon",
            &["@antosha417"],
        ),
        LanguageOptions::pair(
            "hoon",
            "https://github.com/urbit-pilled/tree-sitter-hoon",
            &["@urbit-pilled"],
        ),
        LanguageOptions::pair(
            "html",
            "https://github.com/tree-sitter/tree-sitter-html",
            &["@TravonteD"],
        ),
        LanguageOptions::pair(
            "htmldjango",
            "https://github.com/interdependence/tree-sitter-htmldjango",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair("http", "https://github.com/rest-nvim/tree-sitter-http", &[
            "@amaanq",
            "@NTBBloodbath",
        ]),
        LanguageOptions::pair("hurl", "https://github.com/pfeiferj/tree-sitter-hurl", &[
            "@pfeiferj",
        ]),
        LanguageOptions::pair(
            "hyprlang",
            "https://github.com/luckasRanarison/tree-sitter-hyprlang",
            &["@luckasRanarison"],
        ),
        LanguageOptions::pair("idl", "https://github.com/cathaysia/tree-sitter-idl", &[
            "@cathaysia",
        ]),
        LanguageOptions::pair("idris", "https://github.com/kayhide/tree-sitter-idris", &[
            "@srghma",
        ]),
        LanguageOptions::pair("ini", "https://github.com/justinmk/tree-sitter-ini", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair("inko", "https://github.com/inko-lang/tree-sitter-inko", &[
            "@yorickpeterse",
        ]),
        LanguageOptions::pair("ipkg", "https://github.com/srghma/tree-sitter-ipkg", &[
            "@srghma",
        ]),
        LanguageOptions::pair("ispc", "https://github.com/fab4100/tree-sitter-ispc", &[
            "@fab4100",
        ]),
        LanguageOptions::pair(
            "janet_simple",
            "https://github.com/sogaiu/tree-sitter-janet-simple",
            &["@sogaiu"],
        ),
        LanguageOptions::pair(
            "java",
            "https://github.com/tree-sitter/tree-sitter-java",
            &["@p00f"],
        ),
        LanguageOptions::pair(
            "javascript",
            "https://github.com/tree-sitter/tree-sitter-javascript",
            &["@steelsojka"],
        ),
        LanguageOptions::pair("jq", "https://github.com/flurie/tree-sitter-jq", &[
            "@ObserverOfTime",
        ]),
        LanguageOptions::pair(
            "jsdoc",
            "https://github.com/tree-sitter/tree-sitter-jsdoc",
            &["@steelsojka"],
        ),
        LanguageOptions::pair(
            "json",
            "https://github.com/tree-sitter/tree-sitter-json",
            &["@steelsojka"],
        ),
        LanguageOptions::pair("json5", "https://github.com/Joakker/tree-sitter-json5", &[
            "@Joakker",
        ]),
        LanguageOptions::pair(
            "jsonc",
            "https://gitlab.com/WhyNotHugo/tree-sitter-jsonc.git",
            &["@WhyNotHugo"],
        ),
        LanguageOptions::pair(
            "jsonnet",
            "https://github.com/sourcegraph/tree-sitter-jsonnet",
            &["@nawordar"],
        ),
        LanguageOptions::pair(
            "julia",
            "https://github.com/tree-sitter/tree-sitter-julia",
            &["@fredrikekre"],
        ),
        LanguageOptions::pair(
            "just",
            "https://github.com/IndianBoy42/tree-sitter-just",
            &["@Hubro"],
        ),
        LanguageOptions::pair(
            "kconfig",
            "https://github.com/amaanq/tree-sitter-kconfig",
            &["@amaanq"],
        ),
        LanguageOptions::pair("kdl", "https://github.com/amaanq/tree-sitter-kdl", &[
            "@amaanq",
        ]),
        LanguageOptions::pair("kotlin", "https://github.com/fwcd/tree-sitter-kotlin", &[
            "@SalBakraa",
        ]),
        LanguageOptions::pair("koto", "https://github.com/koto-lang/tree-sitter-koto", &[
            "@irh",
        ]),
        LanguageOptions::pair(
            "kusto",
            "https://github.com/Willem-J-an/tree-sitter-kusto",
            &["@Willem-J-an"],
        ),
        LanguageOptions::pair(
            "lalrpop",
            "https://github.com/traxys/tree-sitter-lalrpop",
            &["@traxys"],
        ),
        LanguageOptions::pair(
            "latex",
            "https://github.com/latex-lsp/tree-sitter-latex",
            &["@theHamsta", "@clason"],
        ),
        LanguageOptions::pair(
            "ledger",
            "https://github.com/cbarrete/tree-sitter-ledger",
            &["@cbarrete"],
        ),
        LanguageOptions::pair("leo", "https://github.com/r001/tree-sitter-leo", &["@r001"]),
        LanguageOptions::pair(
            "llvm",
            "https://github.com/benwilliamgraham/tree-sitter-llvm",
            &["@benwilliamgraham"],
        ),
        LanguageOptions::pair(
            "linkerscript",
            "https://github.com/amaanq/tree-sitter-linkerscript",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "liquid",
            "https://github.com/hankthetank27/tree-sitter-liquid",
            &["@hankthetank27"],
        ),
        LanguageOptions::pair(
            "liquidsoap",
            "https://github.com/savonet/tree-sitter-liquidsoap",
            &["@toots"],
        ),
        LanguageOptions::pair("lua", "https://github.com/MunifTanjim/tree-sitter-lua", &[
            "@muniftanjim",
        ]),
        LanguageOptions::pair("luadoc", "https://github.com/amaanq/tree-sitter-luadoc", &[
            "@amaanq",
        ]),
        LanguageOptions::pair("luap", "https://github.com/amaanq/tree-sitter-luap", &[
            "@amaanq",
        ]),
        LanguageOptions::pair("luau", "https://github.com/amaanq/tree-sitter-luau", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "hlsplaylist",
            "https://github.com/Freed-Wu/tree-sitter-hlsplaylist",
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair(
            "m68k",
            "https://github.com/grahambates/tree-sitter-m68k",
            &["@grahambates"],
        ),
        LanguageOptions::pair("make", "https://github.com/alemuller/tree-sitter-make", &[
            "@lewis6991",
        ]),
        LanguageOptions::pairs_with_symbol_and_crate(
            "markdown",
            "https://github.com/MDeiml/tree-sitter-markdown",
            &[("LANGUAGE", false), ("INLINE_LANGUAGE", false)],
            "md",
            &["@MDeiml"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "markdown_inline",
            "https://github.com/MDeiml/tree-sitter-markdown",
            &[("INLINE_LANGUAGE", false), ("LANGUAGE", false)],
            "md",
            &["@MDeiml"],
        ),
        LanguageOptions::pair(
            "matlab",
            "https://github.com/acristoffers/tree-sitter-matlab",
            &["@acristoffers"],
        ),
        LanguageOptions::pair("menhir", "https://github.com/Kerl13/tree-sitter-menhir", &[
            "@Kerl13",
        ]),
        LanguageOptions::pair(
            "mermaid",
            "https://github.com/monaqa/tree-sitter-mermaid",
            &[],
        ),
        LanguageOptions::pair(
            "meson",
            "https://github.com/Decodetalkers/tree-sitter-meson",
            &["@Decodetalkers"],
        ),
        LanguageOptions::pair("mlir", "https://github.com/artagnon/tree-sitter-mlir", &[
            "@artagnon",
        ]),
        LanguageOptions::pair(
            "muttrc",
            "https://github.com/neomutt/tree-sitter-muttrc",
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair("nasm", "https://github.com/naclsn/tree-sitter-nasm", &[
            "@ObserverOfTime",
        ]),
        LanguageOptions::pair("nginx", "https://github.com/opa-oz/tree-sitter-nginx", &[
            "@opa-oz",
        ]),
        LanguageOptions::pair(
            "nickel",
            "https://github.com/nickel-lang/tree-sitter-nickel",
            &[],
        ),
        LanguageOptions::pair("nim", "https://github.com/alaviss/tree-sitter-nim", &[
            "@aMOPel",
        ]),
        LanguageOptions::pair(
            "nim_format_string",
            "https://github.com/aMOPel/tree-sitter-nim-format-string",
            &["@aMOPel"],
        ),
        LanguageOptions::pair(
            "ninja",
            "https://github.com/alemuller/tree-sitter-ninja",
            &["@alemuller"],
        ),
        LanguageOptions::pair("nix", "https://github.com/cstrahan/tree-sitter-nix", &[
            "@leo60228",
        ]),
        LanguageOptions::pair("norg", "https://github.com/nvim-neorg/tree-sitter-norg", &[
            "@JoeyGrajciar",
            "@vhyrro",
        ]),
        LanguageOptions::pair("nqc", "https://github.com/amaanq/tree-sitter-nqc", &[
            "@amaanq",
        ]),
        LanguageOptions::pair("nu", "https://github.com/nushell/tree-sitter-nu", &[
            "@abhisheksingh0x558",
        ]),
        LanguageOptions::pair("objc", "https://github.com/amaanq/tree-sitter-objc", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "objdump",
            "https://github.com/ColinKennedy/tree-sitter-objdump",
            &["@ColinKennedy"],
        ),
        LanguageOptions::pairs_with_symbol(
            "ocaml",
            "https://github.com/tree-sitter/tree-sitter-ocaml",
            &[
                ("LANGUAGE_OCAML", false),
                ("LANGUAGE_OCAML_INTERFACE", false),
            ],
            &["@undu"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "ocaml_interface",
            "https://github.com/tree-sitter/tree-sitter-ocaml",
            &[
                ("LANGUAGE_OCAML_INTERFACE", false),
                ("LANGUAGE_OCAML", false),
            ],
            "ocaml",
            &["@undu"],
        ),
        LanguageOptions::pair(
            "ocamllex",
            "https://github.com/atom-ocaml/tree-sitter-ocamllex",
            &["@undu"],
        ),
        LanguageOptions::pair("odin", "https://github.com/amaanq/tree-sitter-odin", &[
            "@amaanq",
        ]),
        LanguageOptions::pair("org", "https://github.com/milisims/tree-sitter-org", &[]),
        LanguageOptions::pair("pascal", "https://github.com/Isopod/tree-sitter-pascal", &[
            "@Isopod",
        ]),
        LanguageOptions::pair("passwd", "https://github.com/ath3/tree-sitter-passwd", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "pem",
            "https://github.com/ObserverOfTime/tree-sitter-pem",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "perl",
            "https://github.com/tree-sitter-perl/tree-sitter-perl",
            &["@RabbiVeesh", "@LeoNerd"],
        ),
        LanguageOptions::pairs_with_symbol(
            "php",
            "https://github.com/tree-sitter/tree-sitter-php",
            &[("LANGUAGE_PHP", false), ("LANGUAGE_PHP_ONLY", false)],
            &["@tk-shirasaka", "@calebdw"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "php_only",
            "https://github.com/tree-sitter/tree-sitter-php",
            &[("LANGUAGE_PHP_ONLY", false), ("LANGUAGE_PHP", false)],
            "php",
            &["@tk-shirasaka", "@calebdw"],
        ),
        LanguageOptions::pair(
            "phpdoc",
            "https://github.com/claytonrcarter/tree-sitter-phpdoc",
            &["@mikehaertl"],
        ),
        LanguageOptions::pair(
            "pioasm",
            "https://github.com/leo60228/tree-sitter-pioasm",
            &["@leo60228"],
        ),
        LanguageOptions::pair("po", "https://github.com/erasin/tree-sitter-po", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "pod",
            "https://github.com/tree-sitter-perl/tree-sitter-pod",
            &["@RabbiVeesh", "@LeoNerd"],
        ),
        LanguageOptions::pair(
            "poe_filter",
            "https://github.com/ObserverOfTime/tree-sitter-poe-filter",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair("pony", "https://github.com/amaanq/tree-sitter-pony", &[
            "@amaanq",
            "@mfelsche",
        ]),
        LanguageOptions::pair(
            "powershell",
            "https://github.com/airbus-cert/tree-sitter-powershell",
            &["@L2jLiga"],
        ),
        LanguageOptions::pair(
            "printf",
            "https://github.com/ObserverOfTime/tree-sitter-printf",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "prisma",
            "https://github.com/victorhqc/tree-sitter-prisma",
            &["@elianiva"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "problog",
            "https://github.com/foxyseta/tree-sitter-prolog",
            &[("problog", true), ("prolog", true)],
            "prolog",
            &["@foxyseta"],
        ),
        LanguageOptions::pairs_with_symbol(
            "prolog",
            "https://github.com/foxyseta/tree-sitter-prolog",
            &[("prolog", true), ("problog", true)],
            &["@foxyseta"],
        ),
        LanguageOptions::pair(
            "promql",
            "https://github.com/MichaHoffmann/tree-sitter-promql",
            &["@MichaHoffmann"],
        ),
        LanguageOptions::pair(
            "properties",
            "https://github.com/tree-sitter-grammars/tree-sitter-properties",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair("proto", "https://github.com/treywood/tree-sitter-proto", &[
            "@treywood",
        ]),
        LanguageOptions::pair("prql", "https://github.com/PRQL/tree-sitter-prql", &[
            "@matthias-Q",
        ]),
        LanguageOptions::pairs_with_symbol_and_crate(
            "psv",
            "https://github.com/amaanq/tree-sitter-csv",
            &[
                ("language_psv", true),
                ("language_csv", true),
                ("language_tsv", true),
            ],
            "csv",
            &["@amaanq"],
        ),
        LanguageOptions::pair("pug", "https://github.com/zealot128/tree-sitter-pug", &[
            "@zealot128",
        ]),
        LanguageOptions::pair("puppet", "https://github.com/amaanq/tree-sitter-puppet", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "purescript",
            "https://github.com/postsolar/tree-sitter-purescript",
            &["@postsolar"],
        ),
        LanguageOptions::pair(
            "pymanifest",
            "https://github.com/ObserverOfTime/tree-sitter-pymanifest",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "python",
            "https://github.com/tree-sitter/tree-sitter-python",
            &["@stsewd", "@theHamsta"],
        ),
        LanguageOptions::pair("ql", "https://github.com/tree-sitter/tree-sitter-ql", &[
            "@pwntester",
        ]),
        LanguageOptions::pair(
            "qmldir",
            "https://github.com/Decodetalkers/tree-sitter-qmldir",
            &["@amaanq"],
        ),
        LanguageOptions::pair("qmljs", "https://github.com/yuja/tree-sitter-qmljs", &[
            "@Decodetalkers",
        ]),
        LanguageOptions::pair(
            "query",
            "https://github.com/nvim-treesitter/tree-sitter-query",
            &["@steelsojka"],
        ),
        LanguageOptions::pair("r", "https://github.com/r-lib/tree-sitter-r", &["@ribru17"]),
        LanguageOptions::pair("racket", "https://github.com/6cdh/tree-sitter-racket", &[]),
        LanguageOptions::pair("ralph", "https://github.com/alephium/tree-sitter-ralph", &[
            "@tdroxler",
        ]),
        LanguageOptions::pair("rasi", "https://github.com/Fymyte/tree-sitter-rasi", &[
            "@Fymyte",
        ]),
        LanguageOptions::pair("rbs", "https://github.com/joker1007/tree-sitter-rbs", &[
            "@joker1007",
        ]),
        LanguageOptions::pair("re2c", "https://github.com/amaanq/tree-sitter-re2c", &[
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "readline",
            "https://github.com/ribru17/tree-sitter-readline",
            &["@ribru17"],
        ),
        LanguageOptions::pair(
            "regex",
            "https://github.com/tree-sitter/tree-sitter-regex",
            &["@theHamsta"],
        ),
        LanguageOptions::pair(
            "rego",
            "https://github.com/FallenAngel97/tree-sitter-rego",
            &["@FallenAngel97"],
        ),
        LanguageOptions::pair(
            "requirements",
            "https://github.com/ObserverOfTime/tree-sitter-requirements",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "rescript",
            "https://github.com/rescript-lang/tree-sitter-rescript",
            &["@ribru17"],
        ),
        LanguageOptions::pair(
            "rnoweb",
            "https://github.com/bamonroe/tree-sitter-rnoweb",
            &["@bamonroe"],
        ),
        LanguageOptions::pair("robot", "https://github.com/Hubro/tree-sitter-robot", &[
            "@Hubro",
        ]),
        LanguageOptions::pair(
            "robots",
            "https://github.com/opa-oz/tree-sitter-robots-txt",
            &["@opa-oz"],
        ),
        LanguageOptions::pair("roc", "https://github.com/faldor20/tree-sitter-roc", &[
            "@nat-418",
        ]),
        LanguageOptions::pair("ron", "https://github.com/amaanq/tree-sitter-ron", &[
            "@amaanq",
        ]),
        LanguageOptions::pair("rst", "https://github.com/stsewd/tree-sitter-rst", &[
            "@stsewd",
        ]),
        LanguageOptions::pair(
            "ruby",
            "https://github.com/tree-sitter/tree-sitter-ruby",
            &["@TravonteD"],
        ),
        LanguageOptions::pair(
            "runescript",
            "https://github.com/2004Scape/tree-sitter-runescript",
            &["@2004Scape"],
        ),
        LanguageOptions::pair(
            "rust",
            "https://github.com/tree-sitter/tree-sitter-rust",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "scala",
            "https://github.com/tree-sitter/tree-sitter-scala",
            &["@stevanmilic"],
        ),
        LanguageOptions::pair("scfg", "https://github.com/rockorager/tree-sitter-scfg", &[
            "@WhyNotHugo",
        ]),
        LanguageOptions::pair("scheme", "https://github.com/6cdh/tree-sitter-scheme", &[]),
        LanguageOptions::pair("scss", "https://github.com/serenadeai/tree-sitter-scss", &[
            "@elianiva",
        ]),
        LanguageOptions::pairs_with_symbol_and_crate(
            "sflog",
            "https://github.com/aheber/tree-sitter-sfapex",
            &[
                ("sflog::LANGUAGE", false),
                ("apex::LANGUAGE", false),
                ("soql::LANGUAGE", false),
                ("sosl::LANGUAGE", false),
            ],
            "sfapex",
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pair(
            "slang",
            "https://github.com/theHamsta/tree-sitter-slang",
            &["@theHamsta"],
        ),
        LanguageOptions::pair("slim", "https://github.com/theoo/tree-sitter-slim", &[
            "@theoo",
        ]),
        LanguageOptions::pair("slint", "https://github.com/slint-ui/tree-sitter-slint", &[
            "@hunger",
        ]),
        LanguageOptions::pair(
            "smali",
            "https://github.com/tree-sitter-grammars/tree-sitter-smali",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "snakemake",
            "https://github.com/osthomas/tree-sitter-snakemake",
            &[],
        ),
        LanguageOptions::pair(
            "smithy",
            "https://github.com/indoorvivants/tree-sitter-smithy",
            &["@amaanq", "@keynmol"],
        ),
        LanguageOptions::pair(
            "solidity",
            "https://github.com/JoranHonig/tree-sitter-solidity",
            &["@amaanq"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "soql",
            "https://github.com/aheber/tree-sitter-sfapex",
            &[
                ("soql::LANGUAGE", false),
                ("sflog::LANGUAGE", false),
                ("apex::LANGUAGE", false),
                ("sosl::LANGUAGE", false),
            ],
            "sfapex",
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "sosl",
            "https://github.com/aheber/tree-sitter-sfapex",
            &[
                ("soql::LANGUAGE", false),
                ("sflog::LANGUAGE", false),
                ("apex::LANGUAGE", false),
                ("sosl::LANGUAGE", false),
            ],
            "sfapex",
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pair(
            "sourcepawn",
            "https://github.com/nilshelmig/tree-sitter-sourcepawn",
            &["@Sarrus1"],
        ),
        LanguageOptions::pair(
            "sparql",
            "https://github.com/GordianDziwis/tree-sitter-sparql",
            &["@GordianDziwis"],
        ),
        LanguageOptions::pair("sql", "https://github.com/derekstride/tree-sitter-sql", &[
            "@derekstride",
        ]),
        LanguageOptions::pair(
            "squirrel",
            "https://github.com/amaanq/tree-sitter-squirrel",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "ssh_config",
            "https://github.com/ObserverOfTime/tree-sitter-ssh-config",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "starlark",
            "https://github.com/amaanq/tree-sitter-starlark",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "strace",
            "https://github.com/sigmaSd/tree-sitter-strace",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "styled",
            "https://github.com/mskelton/tree-sitter-styled",
            &["@mskelton"],
        ),
        LanguageOptions::pair(
            "supercollider",
            "https://github.com/madskjeldgaard/tree-sitter-supercollider",
            &["@madskjeldgaard"],
        ),
        LanguageOptions::pairs_with_symbol(
            "superhtml",
            "https://github.com/kristoff-it/superhtml",
            &[("function", true)],
            &["@rockorager"],
        ),
        LanguageOptions::pair(
            "surface",
            "https://github.com/connorlay/tree-sitter-surface",
            &["@connorlay"],
        ),
        LanguageOptions::pair(
            "svelte",
            "https://github.com/tree-sitter-grammars/tree-sitter-svelte",
            &["@amaanq"],
        ),
        LanguageOptions::pair(
            "sway",
            "https://github.com/FuelLabs/tree-sitter-sway.git",
            &["@ribru17"],
        ),
        LanguageOptions::pair(
            "swift",
            "https://github.com/alex-pinkus/tree-sitter-swift",
            &["@alex-pinkus"],
        ),
        LanguageOptions::pair(
            "sxhkdrc",
            "https://github.com/RaafatTurki/tree-sitter-sxhkdrc",
            &["@RaafatTurki"],
        ),
        LanguageOptions::pair(
            "systemtap",
            "https://github.com/ok-ryoko/tree-sitter-systemtap",
            &["@ok-ryoko"],
        ),
        LanguageOptions::pair("t32", "https://gitlab.com/xasc/tree-sitter-t32.git", &[
            "@xasc",
        ]),
        LanguageOptions::pair(
            "tablegen",
            "https://github.com/amaanq/tree-sitter-tablegen",
            &["@amaanq"],
        ),
        LanguageOptions::pair("tact", "https://github.com/tact-lang/tree-sitter-tact", &[
            "@novusnota",
        ]),
        LanguageOptions::pair(
            "teal",
            "https://github.com/euclidianAce/tree-sitter-teal",
            &["@euclidianAce"],
        ),
        LanguageOptions::pair(
            "templ",
            "https://github.com/vrischmann/tree-sitter-templ",
            &["@vrischmann"],
        ),
        LanguageOptions::pair(
            "tcl",
            "https://github.com/tree-sitter-grammars/tree-sitter-tcl",
            &["@lewis6991"],
        ),
        // No rust bindings 😠
        // LanguageOptions::pair_with_symbol_and_crate(
        //     "terraform",
        //     "https://github.com/MichaHoffmann/tree-sitter-hcl",
        //     ("LANGUAGE", false),
        //     "hcl",
        //     &[ "@MichaHoffmann" ],
        // ),
        LanguageOptions::pair(
            "textproto",
            "https://github.com/PorterAtGoogle/tree-sitter-textproto",
            &["@Porter"],
        ),
        LanguageOptions::pair(
            "thrift",
            "https://github.com/duskmoon314/tree-sitter-thrift",
            &["@amaanq", "@duskmoon314"],
        ),
        LanguageOptions::pair(
            "tiger",
            "https://github.com/ambroisie/tree-sitter-tiger",
            &["@ambroisie"],
        ),
        LanguageOptions::pair(
            "tlaplus",
            "https://github.com/tlaplus-community/tree-sitter-tlaplus",
            &["@ahelwer", "@susliko"],
        ),
        LanguageOptions::pair("tmux", "https://github.com/Freed-Wu/tree-sitter-tmux", &[
            "@Freed-Wu",
        ]),
        LanguageOptions::pair(
            "todotxt",
            "https://github.com/arnarg/tree-sitter-todotxt",
            &["@arnarg"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "toml",
            "https://github.com/tree-sitter-grammars/tree-sitter-toml",
            &[("LANGUAGE", false)],
            "toml-ng",
            &["@tk-shirasaka"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "tsv",
            "https://github.com/amaanq/tree-sitter-csv",
            &[
                ("language_tsv", true),
                ("language_csv", true),
                ("language_psv", true),
            ],
            "csv",
            &["@amaanq"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "tsx",
            "https://github.com/tree-sitter/tree-sitter-typescript",
            &[("LANGUAGE_TSX", false), ("LANGUAGE_TYPESCRIPT", false)],
            "typescript",
            &["@steelsojka"],
        ),
        LanguageOptions::pair(
            "turtle",
            "https://github.com/GordianDziwis/tree-sitter-turtle",
            &["@GordianDziwis"],
        ),
        LanguageOptions::pair("twig", "https://github.com/gbprod/tree-sitter-twig", &[
            "@gbprod",
        ]),
        LanguageOptions::pairs_with_symbol(
            "typescript",
            "https://github.com/tree-sitter/tree-sitter-typescript",
            &[("LANGUAGE_TYPESCRIPT", false), ("LANGUAGE_TSX", false)],
            &["@steelsojka"],
        ),
        LanguageOptions::pair(
            "typespec",
            "https://github.com/happenslol/tree-sitter-typespec",
            &["@happenslol"],
        ),
        LanguageOptions::pair(
            "typoscript",
            "https://github.com/Teddytrombone/tree-sitter-typoscript",
            &["@Teddytrombone"],
        ),
        LanguageOptions::pair("typst", "https://github.com/uben0/tree-sitter-typst", &[
            "@uben0",
            "@RaafatTurki",
        ]),
        LanguageOptions::pair(
            "udev",
            "https://github.com/ObserverOfTime/tree-sitter-udev",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "ungrammar",
            "https://github.com/Philipp-M/tree-sitter-ungrammar",
            &["@Philipp-M", "@amaanq"],
        ),
        LanguageOptions::pair(
            "unison",
            "https://github.com/kylegoetz/tree-sitter-unison",
            &["@tapegram"],
        ),
        LanguageOptions::pair("usd", "https://github.com/ColinKennedy/tree-sitter-usd", &[
            "@ColinKennedy",
        ]),
        LanguageOptions::pair("uxntal", "https://github.com/amaanq/tree-sitter-uxntal", &[
            "@amaanq",
        ]),
        // No rust bindings 😠
        // LanguageOptions::new(
        //     "v",
        //     "https://github.com/vlang/v-analyzer",
        //     ("LANGUAGE", false),
        //     "tree_sitter_v",
        //     &["@kkharji", "@amaanq"],
        // ),
        LanguageOptions::pair("vala", "https://github.com/vala-lang/tree-sitter-vala", &[
            "@Prince781",
        ]),
        LanguageOptions::pair("vento", "https://github.com/ventojs/tree-sitter-vento", &[
            "@wrapperup",
            "@oscarotero",
        ]),
        LanguageOptions::pair(
            "verilog",
            "https://github.com/gmlarumbe/tree-sitter-systemverilog",
            &["@zhangwwpeng"],
        ),
        LanguageOptions::pair(
            "vhdl",
            "https://github.com/jpt13653903/tree-sitter-vhdl",
            &["@jpt13653903"],
        ),
        LanguageOptions::pair(
            "vhs",
            "https://github.com/charmbracelet/tree-sitter-vhs",
            &["@caarlos0"],
        ),
        LanguageOptions::pair("vim", "https://github.com/neovim/tree-sitter-vim", &[
            "@clason",
        ]),
        LanguageOptions::pair("vimdoc", "https://github.com/neovim/tree-sitter-vimdoc", &[
            "@clason",
        ]),
        LanguageOptions::pair("vrl", "https://github.com/belltoy/tree-sitter-vrl", &[
            "@belltoy",
        ]),
        LanguageOptions::pair(
            "vue",
            "https://github.com/tree-sitter-grammars/tree-sitter-vue",
            &["@WhyNotHugo", "@lucario387"],
        ),
        LanguageOptions::pair("wgsl", "https://github.com/szebniok/tree-sitter-wgsl", &[
            "@szebniok",
        ]),
        LanguageOptions::pair(
            "wgsl_bevy",
            "https://github.com/theHamsta/tree-sitter-wgsl-bevy",
            &["@theHamsta"],
        ),
        LanguageOptions::pair("wing", "https://github.com/winglang/tree-sitter-wing", &[
            "@gshpychka",
            "@MarkMcCulloh",
        ]),
        LanguageOptions::pair("wit", "https://github.com/liamwh/tree-sitter-wit", &[
            "@liamwh",
        ]),
        LanguageOptions::pair(
            "xcompose",
            "https://github.com/ObserverOfTime/tree-sitter-xcompose",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pairs_with_symbol(
            "xml",
            "https://github.com/tree-sitter-grammars/tree-sitter-xml",
            &[("LANGUAGE_XML", false)],
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair(
            "xresources",
            "https://github.com/ValdezFOmar/tree-sitter-xresources",
            &["@ValdezFOmar"],
        ),
        LanguageOptions::pair(
            "yaml",
            "https://github.com/tree-sitter-grammars/tree-sitter-yaml",
            &["@amaanq"],
        ),
        LanguageOptions::pair("yang", "https://github.com/Hubro/tree-sitter-yang", &[
            "@Hubro",
        ]),
        LanguageOptions::pair("yuck", "https://github.com/Philipp-M/tree-sitter-yuck", &[
            "@Philipp-M",
            "@amaanq",
        ]),
        LanguageOptions::pair(
            "zathurarc",
            "https://github.com/Freed-Wu/tree-sitter-zathurarc",
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair(
            "zig",
            "https://github.com/tree-sitter-grammars/tree-sitter-zig",
            &["@amaanq"],
        ),
        LanguageOptions::pairs_with_symbol(
            "ziggy",
            "https://github.com/kristoff-it/ziggy",
            &[("language", true)],
            &["@rockorager"],
        ),
        LanguageOptions::pairs_with_symbol(
            "ziggy_schema",
            "https://github.com/kristoff-it/ziggy",
            &[("language", true)],
            &["@rockorager"],
        ),
    ])
});
