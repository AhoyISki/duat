use std::{collections::HashMap, sync::LazyLock};

use super::LanguageOptions;

pub static LANGUAGE_OPTIONS: LazyLock<HashMap<&str, LanguageOptions>> = LazyLock::new(|| {
    HashMap::from_iter([
        LanguageOptions::pair_const("ada", "https://github.com/briot/tree-sitter-ada", &[
            "@briot",
        ]),
        LanguageOptions::pair_const(
            "agda",
            "https://github.com/tree-sitter/tree-sitter-agda",
            &["@Decodetalkers"],
        ),
        LanguageOptions::pair_fn(
            "angular",
            "https://github.com/dlvandenberg/tree-sitter-angular",
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
            ("sfapex", None),
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pair_const(
            "arduino",
            "https://github.com/ObserverOfTime/tree-sitter-arduino",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const("asm", "https://github.com/RubixDev/tree-sitter-asm", &[
            "@RubixDev",
        ]),
        // Fails to compile
        // LanguageOptions::pair_fn(
        //     "astro",
        //     "https://github.com/virchau13/tree-sitter-astro",
        //     &["@virchau13"],
        // ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "authzed",
            "https://github.com/mleonidas/tree-sitter-authzed",
            &[("language", true)],
            ("azd", None),
            &["@mattpolzin"],
        ),
        // Seems to freeze?
        // LanguageOptions::pair_fn(
        //     "awk",
        //     "https://github.com/Beaglefoot/tree-sitter-awk",
        //     &[],
        // ),
        LanguageOptions::pair_const(
            "bash",
            "https://github.com/tree-sitter/tree-sitter-bash",
            &["@TravonteD"],
        ),
        LanguageOptions::pair_const("bass", "https://github.com/vito/tree-sitter-bass", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "beancount",
            "https://github.com/polarmutex/tree-sitter-beancount",
            &["@polarmutex"],
        ),
        LanguageOptions::pair_const(
            "bibtex",
            "https://github.com/latex-lsp/tree-sitter-bibtex",
            &["@theHamsta", "@clason"],
        ),
        LanguageOptions::pair_const("bicep", "https://github.com/amaanq/tree-sitter-bicep", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "bitbake",
            "https://github.com/amaanq/tree-sitter-bitbake",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "blueprint",
            "https://gitlab.com/gabmus/tree-sitter-blueprint.git",
            &["@gabmus"],
        ),
        LanguageOptions::pair_const("bp", "https://github.com/ambroisie/tree-sitter-bp", &[
            "@ambroisie",
        ]),
        LanguageOptions::pair_const("c", "https://github.com/tree-sitter/tree-sitter-c", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "c_sharp",
            "https://github.com/tree-sitter/tree-sitter-c-sharp",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const("cairo", "https://github.com/amaanq/tree-sitter-cairo", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const("capnp", "https://github.com/amaanq/tree-sitter-capnp", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "chatito",
            "https://github.com/ObserverOfTime/tree-sitter-chatito",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "circom",
            "https://github.com/Decurity/tree-sitter-circom",
            &["@alexandr-martirosyan"],
        ),
        LanguageOptions::pair_const(
            "clojure",
            "https://github.com/sogaiu/tree-sitter-clojure",
            &["@NoahTheDuke"],
        ),
        LanguageOptions::pair_const("cmake", "https://github.com/uyha/tree-sitter-cmake", &[
            "@uyha",
        ]),
        LanguageOptions::pair_const(
            "comment",
            "https://github.com/stsewd/tree-sitter-comment",
            &["@stsewd"],
        ),
        LanguageOptions::pair_const(
            "commonlisp",
            "https://github.com/theHamsta/tree-sitter-commonlisp",
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "cooklang",
            "https://github.com/addcninblue/tree-sitter-cooklang",
            &["@addcninblue"],
        ),
        LanguageOptions::pair_const(
            "corn",
            "https://github.com/jakestanger/tree-sitter-corn",
            &["@jakestanger"],
        ),
        LanguageOptions::pair_const("cpon", "https://github.com/amaanq/tree-sitter-cpon", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const("cpp", "https://github.com/tree-sitter/tree-sitter-cpp", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair_const("css", "https://github.com/tree-sitter/tree-sitter-css", &[
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
        LanguageOptions::pair_const("cuda", "https://github.com/theHamsta/tree-sitter-cuda", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair_const("cue", "https://github.com/eonpatapon/tree-sitter-cue", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "cylc",
            "https://github.com/elliotfontaine/tree-sitter-cylc",
            &["@elliotfontaine"],
        ),
        LanguageOptions::pair_const("d", "https://github.com/gdamore/tree-sitter-d", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "dart",
            "https://github.com/UserNobody14/tree-sitter-dart",
            &["@akinsho"],
        ),
        LanguageOptions::pair_const(
            "desktop",
            "https://github.com/ValdezFOmar/tree-sitter-desktop",
            &["@ValdezFOmar"],
        ),
        LanguageOptions::pair_const(
            "devicetree",
            "https://github.com/joelspadin/tree-sitter-devicetree",
            &["@jedrzejboczar"],
        ),
        LanguageOptions::pair_const("dhall", "https://github.com/jbellerb/tree-sitter-dhall", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "diff",
            "https://github.com/the-mikedavis/tree-sitter-diff",
            &["@gbprod"],
        ),
        LanguageOptions::pair_const(
            "disassembly",
            "https://github.com/ColinKennedy/tree-sitter-disassembly",
            &["@ColinKennedy"],
        ),
        LanguageOptions::pair_const("djot", "https://github.com/treeman/tree-sitter-djot", &[
            "@NoahTheDuke",
        ]),
        LanguageOptions::pair_const(
            "dockerfile",
            "https://github.com/camdencheek/tree-sitter-dockerfile",
            &["@camdencheek"],
        ),
        LanguageOptions::pair_const("dot", "https://github.com/rydesun/tree-sitter-dot", &[
            "@rydesun",
        ]),
        LanguageOptions::pair_const(
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
        LanguageOptions::pair_const(
            "duat_text",
            "https://github.com/AhoyISki/tree-sitter-duat-text",
            &["AhoyISki"],
        ),
        LanguageOptions::pair_const(
            "earthfile",
            "https://github.com/glehmann/tree-sitter-earthfile",
            &["@glehmann"],
        ),
        LanguageOptions::pair_fn("ebnf", "https://github.com/RubixDev/ebnf", &["@RubixDev"]),
        LanguageOptions::pair_const(
            "editorconfig",
            "https://github.com/ValdezFOmar/tree-sitter-editorconfig",
            &["@ValdezFOmar"],
        ),
        LanguageOptions::pair_const("eds", "https://github.com/uyha/tree-sitter-eds", &["@uyha"]),
        LanguageOptions::pair_const("eex", "https://github.com/connorlay/tree-sitter-eex", &[
            "@connorlay",
        ]),
        LanguageOptions::pair_const(
            "elixir",
            "https://github.com/elixir-lang/tree-sitter-elixir",
            &["@connorlay"],
        ),
        LanguageOptions::pair_const("elm", "https://github.com/elm-tooling/tree-sitter-elm", &[
            "@zweimach",
        ]),
        LanguageOptions::pair_const(
            "elsa",
            "https://github.com/glapa-grossklag/tree-sitter-elsa",
            &["@glapa-grossklag", "@amaanq"],
        ),
        LanguageOptions::pair_const("elvish", "https://github.com/elves/tree-sitter-elvish", &[
            "@elves",
        ]),
        LanguageOptions::pair_const(
            "embedded_template",
            "https://github.com/tree-sitter/tree-sitter-embedded-template",
            &[],
        ),
        LanguageOptions::pair_const(
            "erlang",
            "https://github.com/WhatsApp/tree-sitter-erlang",
            &["@filmor"],
        ),
        LanguageOptions::pair_const(
            "facility",
            "https://github.com/FacilityApi/tree-sitter-facility",
            &["@bryankenote"],
        ),
        LanguageOptions::pair_const("faust", "https://github.com/khiner/tree-sitter-faust", &[
            "@khiner",
        ]),
        LanguageOptions::pair_const(
            "fennel",
            "https://github.com/alexmozaidze/tree-sitter-fennel",
            &["@alexmozaidze"],
        ),
        LanguageOptions::pair_const("fidl", "https://github.com/google/tree-sitter-fidl", &[
            "@chaopeng",
        ]),
        LanguageOptions::pair_const("firrtl", "https://github.com/amaanq/tree-sitter-firrtl", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_fn(
            "fish",
            "https://github.com/ram02z/tree-sitter-fish",
            &["@ram02z"],
        ),
        LanguageOptions::pair_const(
            "foam",
            "https://github.com/FoamScience/tree-sitter-foam",
            &["@FoamScience"],
        ),
        LanguageOptions::pair_const(
            "forth",
            "https://github.com/AlexanderBrevig/tree-sitter-forth",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "fortran",
            "https://github.com/stadelmanma/tree-sitter-fortran",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const("fsh", "https://github.com/mgramigna/tree-sitter-fsh", &[
            "@mgramigna",
        ]),
        LanguageOptions::pairs_with_symbol(
            "fsharp",
            "https://github.com/ionide/tree-sitter-fsharp",
            &[("LANGUAGE_FSHARP", false)],
            &["@nsidorenco"],
        ),
        LanguageOptions::pair_const("func", "https://github.com/amaanq/tree-sitter-func", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "fusion",
            "https://gitlab.com/jirgn/tree-sitter-fusion.git",
            &["@jirgn"],
        ),
        LanguageOptions::pair_const("gap", "https://github.com/gap-system/tree-sitter-gap", &[
            "@reiniscirpons",
        ]),
        LanguageOptions::pair_const(
            "gaptst",
            "https://github.com/gap-system/tree-sitter-gaptst",
            &["@reiniscirpons"],
        ),
        LanguageOptions::pair_const(
            "gdscript",
            "https://github.com/PrestonKnopp/tree-sitter-gdscript",
            &["@PrestonKnopp"],
        ),
        LanguageOptions::pair_const(
            "gdshader",
            "https://github.com/GodOfAvacyn/tree-sitter-gdshader",
            &["@godofavacyn"],
        ),
        LanguageOptions::pair_const(
            "git_rebase",
            "https://github.com/the-mikedavis/tree-sitter-git-rebase",
            &["@gbprod"],
        ),
        LanguageOptions::pair_const(
            "gitattributes",
            "https://github.com/ObserverOfTime/tree-sitter-gitattributes",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_fn(
            "gitcommit",
            "https://github.com/gbprod/tree-sitter-gitcommit",
            &["@gbprod"],
        ),
        LanguageOptions::pair_const(
            "git_config",
            "https://github.com/the-mikedavis/tree-sitter-git-config",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "gitignore",
            "https://github.com/shunsambongi/tree-sitter-gitignore",
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "gleam",
            "https://github.com/gleam-lang/tree-sitter-gleam",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "glimmer",
            "https://github.com/ember-tooling/tree-sitter-glimmer",
            &["@NullVoxPopuli"],
        ),
        LanguageOptions::pair_const(
            "glimmer_javascript",
            "https://github.com/NullVoxPopuli/tree-sitter-glimmer-javascript",
            &["@NullVoxPopuli"],
        ),
        LanguageOptions::pair_const(
            "glimmer_typescript",
            "https://github.com/NullVoxPopuli/tree-sitter-glimmer-typescript",
            &["@NullVoxPopuli"],
        ),
        LanguageOptions::pair_const("glsl", "https://github.com/theHamsta/tree-sitter-glsl", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair_const("gn", "https://github.com/amaanq/tree-sitter-gn", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "gnuplot",
            "https://github.com/dpezto/tree-sitter-gnuplot",
            &["@dpezto"],
        ),
        LanguageOptions::pair_const("go", "https://github.com/tree-sitter/tree-sitter-go", &[
            "@theHamsta",
            "@WinWisely268",
        ]),
        LanguageOptions::pair_const("goctl", "https://github.com/chaozwn/tree-sitter-goctl", &[
            "@chaozwn",
        ]),
        LanguageOptions::pair_const(
            "godot_resource",
            "https://github.com/PrestonKnopp/tree-sitter-godot-resource",
            &["@pierpo"],
        ),
        LanguageOptions::pair_const(
            "gomod",
            "https://github.com/camdencheek/tree-sitter-go-mod",
            &["@camdencheek"],
        ),
        LanguageOptions::pair_const("gosum", "https://github.com/amaanq/tree-sitter-go-sum", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "gowork",
            "https://github.com/omertuc/tree-sitter-go-work",
            &["@omertuc"],
        ),
        LanguageOptions::pair_const(
            "gotmpl",
            "https://github.com/ngalaiko/tree-sitter-go-template",
            &["@qvalentin"],
        ),
        LanguageOptions::pair_const(
            "gpg",
            "https://github.com/ObserverOfTime/tree-sitter-gpg-config",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const("gren", "https://github.com/MaeBrooks/tree-sitter-gren", &[
            "@MaeBrooks",
        ]),
        LanguageOptions::pair_const(
            "groovy",
            "https://github.com/murtaza64/tree-sitter-groovy",
            &["@murtaza64"],
        ),
        LanguageOptions::pair_const(
            "graphql",
            "https://github.com/bkegley/tree-sitter-graphql",
            &["@bkegley"],
        ),
        LanguageOptions::pair_const(
            "gstlaunch",
            "https://github.com/theHamsta/tree-sitter-gstlaunch",
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const("hack", "https://github.com/slackhq/tree-sitter-hack", &[]),
        LanguageOptions::pair_const("hare", "https://github.com/amaanq/tree-sitter-hare", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "haskell",
            "https://github.com/tree-sitter/tree-sitter-haskell",
            &["@mrcjkb"],
        ),
        LanguageOptions::pair_const(
            "haskell_persistent",
            "https://github.com/MercuryTechnologies/tree-sitter-haskell-persistent",
            &["@lykahb"],
        ),
        LanguageOptions::pair_const(
            "hcl",
            "https://github.com/MichaHoffmann/tree-sitter-hcl",
            &["@MichaHoffmann"],
        ),
        LanguageOptions::pair_const("heex", "https://github.com/connorlay/tree-sitter-heex", &[
            "@connorlay",
        ]),
        // No rust bindings 😠
        // LanguageOptions::pair_with_symbol(
        //     "helm",
        //     "https://github.com/ngalaiko/tree-sitter-go-template",
        //     ("language", true),
        //     &[ "@qvalentin" ],
        // ),
        LanguageOptions::pair_const(
            "hjson",
            "https://github.com/winston0410/tree-sitter-hjson",
            &["@winston0410"],
        ),
        LanguageOptions::pair_const("hlsl", "https://github.com/theHamsta/tree-sitter-hlsl", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair_const(
            "hocon",
            "https://github.com/antosha417/tree-sitter-hocon",
            &["@antosha417"],
        ),
        LanguageOptions::pair_const(
            "hoon",
            "https://github.com/urbit-pilled/tree-sitter-hoon",
            &["@urbit-pilled"],
        ),
        LanguageOptions::pair_const(
            "html",
            "https://github.com/tree-sitter/tree-sitter-html",
            &["@TravonteD"],
        ),
        LanguageOptions::pair_const(
            "htmldjango",
            "https://github.com/interdependence/tree-sitter-htmldjango",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const("http", "https://github.com/rest-nvim/tree-sitter-http", &[
            "@amaanq",
            "@NTBBloodbath",
        ]),
        LanguageOptions::pair_const("hurl", "https://github.com/pfeiferj/tree-sitter-hurl", &[
            "@pfeiferj",
        ]),
        LanguageOptions::pair_const(
            "hyprlang",
            "https://github.com/luckasRanarison/tree-sitter-hyprlang",
            &["@luckasRanarison"],
        ),
        LanguageOptions::pair_const("idl", "https://github.com/cathaysia/tree-sitter-idl", &[
            "@cathaysia",
        ]),
        LanguageOptions::pair_const("idris", "https://github.com/kayhide/tree-sitter-idris", &[
            "@srghma",
        ]),
        LanguageOptions::pair_const("ini", "https://github.com/justinmk/tree-sitter-ini", &[
            "@theHamsta",
        ]),
        LanguageOptions::pair_const("inko", "https://github.com/inko-lang/tree-sitter-inko", &[
            "@yorickpeterse",
        ]),
        LanguageOptions::pair_const("ipkg", "https://github.com/srghma/tree-sitter-ipkg", &[
            "@srghma",
        ]),
        LanguageOptions::pair_const("ispc", "https://github.com/fab4100/tree-sitter-ispc", &[
            "@fab4100",
        ]),
        LanguageOptions::pair_const(
            "janet_simple",
            "https://github.com/sogaiu/tree-sitter-janet-simple",
            &["@sogaiu"],
        ),
        LanguageOptions::pair_const(
            "java",
            "https://github.com/tree-sitter/tree-sitter-java",
            &["@p00f"],
        ),
        LanguageOptions::pair_const(
            "javascript",
            "https://github.com/tree-sitter/tree-sitter-javascript",
            &["@steelsojka"],
        ),
        LanguageOptions::pair_fn("jq", "https://github.com/flurie/tree-sitter-jq", &[
            "@ObserverOfTime",
        ]),
        LanguageOptions::pair_const(
            "jsdoc",
            "https://github.com/tree-sitter/tree-sitter-jsdoc",
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const(
            "json",
            "https://github.com/tree-sitter/tree-sitter-json",
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const("json5", "https://github.com/Joakker/tree-sitter-json5", &[
            "@Joakker",
        ]),
        LanguageOptions::pair_const(
            "jsonc",
            "https://gitlab.com/WhyNotHugo/tree-sitter-jsonc.git",
            &["@WhyNotHugo"],
        ),
        LanguageOptions::pair_const(
            "jsonnet",
            "https://github.com/sourcegraph/tree-sitter-jsonnet",
            &["@nawordar"],
        ),
        LanguageOptions::pair_const(
            "julia",
            "https://github.com/tree-sitter/tree-sitter-julia",
            &["@fredrikekre"],
        ),
        LanguageOptions::pair_const(
            "just",
            "https://github.com/IndianBoy42/tree-sitter-just",
            &["@Hubro"],
        ),
        LanguageOptions::pair_fn(
            "kconfig",
            "https://github.com/amaanq/tree-sitter-kconfig",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const("kdl", "https://github.com/amaanq/tree-sitter-kdl", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const("kotlin", "https://github.com/fwcd/tree-sitter-kotlin", &[
            "@SalBakraa",
        ]),
        LanguageOptions::pair_const("koto", "https://github.com/koto-lang/tree-sitter-koto", &[
            "@irh",
        ]),
        LanguageOptions::pair_const(
            "kusto",
            "https://github.com/Willem-J-an/tree-sitter-kusto",
            &["@Willem-J-an"],
        ),
        LanguageOptions::pair_const(
            "lalrpop",
            "https://github.com/traxys/tree-sitter-lalrpop",
            &["@traxys"],
        ),
        LanguageOptions::pair_const(
            "latex",
            "https://github.com/latex-lsp/tree-sitter-latex",
            &["@theHamsta", "@clason"],
        ),
        LanguageOptions::pair_const(
            "ledger",
            "https://github.com/cbarrete/tree-sitter-ledger",
            &["@cbarrete"],
        ),
        LanguageOptions::pair_const("leo", "https://github.com/r001/tree-sitter-leo", &["@r001"]),
        LanguageOptions::pair_const(
            "llvm",
            "https://github.com/benwilliamgraham/tree-sitter-llvm",
            &["@benwilliamgraham"],
        ),
        LanguageOptions::pair_const(
            "linkerscript",
            "https://github.com/amaanq/tree-sitter-linkerscript",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "liquid",
            "https://github.com/hankthetank27/tree-sitter-liquid",
            &["@hankthetank27"],
        ),
        LanguageOptions::pair_const(
            "liquidsoap",
            "https://github.com/savonet/tree-sitter-liquidsoap",
            &["@toots"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "lua",
            "https://github.com/MunifTanjim/tree-sitter-lua",
            &[("LANGUAGE", false)],
            ("lua", Some("0.3.1-dev.0")),
            &["@muniftanjim"],
        ),
        LanguageOptions::pair_const("luadoc", "https://github.com/amaanq/tree-sitter-luadoc", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const("luap", "https://github.com/amaanq/tree-sitter-luap", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const("luau", "https://github.com/amaanq/tree-sitter-luau", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "hlsplaylist",
            "https://github.com/Freed-Wu/tree-sitter-hlsplaylist",
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair_const(
            "m68k",
            "https://github.com/grahambates/tree-sitter-m68k",
            &["@grahambates"],
        ),
        LanguageOptions::pair_const("make", "https://github.com/alemuller/tree-sitter-make", &[
            "@lewis6991",
        ]),
        LanguageOptions::pairs_with_symbol_and_crate(
            "markdown",
            "https://github.com/MDeiml/tree-sitter-markdown",
            &[("LANGUAGE", false), ("INLINE_LANGUAGE", false)],
            ("md", None),
            &["@MDeiml"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "markdown_inline",
            "https://github.com/MDeiml/tree-sitter-markdown",
            &[("INLINE_LANGUAGE", false), ("LANGUAGE", false)],
            ("md", None),
            &["@MDeiml"],
        ),
        LanguageOptions::pair_const(
            "matlab",
            "https://github.com/acristoffers/tree-sitter-matlab",
            &["@acristoffers"],
        ),
        LanguageOptions::pair_const("menhir", "https://github.com/Kerl13/tree-sitter-menhir", &[
            "@Kerl13",
        ]),
        LanguageOptions::pair_const(
            "mermaid",
            "https://github.com/monaqa/tree-sitter-mermaid",
            &[],
        ),
        LanguageOptions::pair_const(
            "meson",
            "https://github.com/Decodetalkers/tree-sitter-meson",
            &["@Decodetalkers"],
        ),
        LanguageOptions::pair_const("mlir", "https://github.com/artagnon/tree-sitter-mlir", &[
            "@artagnon",
        ]),
        LanguageOptions::pair_const(
            "muttrc",
            "https://github.com/neomutt/tree-sitter-muttrc",
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair_const("nasm", "https://github.com/naclsn/tree-sitter-nasm", &[
            "@ObserverOfTime",
        ]),
        LanguageOptions::pair_const("nginx", "https://github.com/opa-oz/tree-sitter-nginx", &[
            "@opa-oz",
        ]),
        LanguageOptions::pair_const(
            "nickel",
            "https://github.com/nickel-lang/tree-sitter-nickel",
            &[],
        ),
        LanguageOptions::pair_const("nim", "https://github.com/alaviss/tree-sitter-nim", &[
            "@aMOPel",
        ]),
        LanguageOptions::pair_const(
            "nim_format_string",
            "https://github.com/aMOPel/tree-sitter-nim-format-string",
            &["@aMOPel"],
        ),
        LanguageOptions::pair_const(
            "ninja",
            "https://github.com/alemuller/tree-sitter-ninja",
            &["@alemuller"],
        ),
        LanguageOptions::pair_const("nix", "https://github.com/cstrahan/tree-sitter-nix", &[
            "@leo60228",
        ]),
        LanguageOptions::pair_const("norg", "https://github.com/nvim-neorg/tree-sitter-norg", &[
            "@JoeyGrajciar",
            "@vhyrro",
        ]),
        LanguageOptions::pair_const("nqc", "https://github.com/amaanq/tree-sitter-nqc", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const("nu", "https://github.com/nushell/tree-sitter-nu", &[
            "@abhisheksingh0x558",
        ]),
        LanguageOptions::pair_const("objc", "https://github.com/amaanq/tree-sitter-objc", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
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
            ("ocaml", None),
            &["@undu"],
        ),
        LanguageOptions::pair_const(
            "ocamllex",
            "https://github.com/atom-ocaml/tree-sitter-ocamllex",
            &["@undu"],
        ),
        LanguageOptions::pair_const("odin", "https://github.com/amaanq/tree-sitter-odin", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const("org", "https://github.com/milisims/tree-sitter-org", &[]),
        LanguageOptions::pair_const("pascal", "https://github.com/Isopod/tree-sitter-pascal", &[
            "@Isopod",
        ]),
        LanguageOptions::pair_const("passwd", "https://github.com/ath3/tree-sitter-passwd", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "pem",
            "https://github.com/ObserverOfTime/tree-sitter-pem",
            &["@ObserverOfTime"],
        ),
        // Fails to compile
        // LanguageOptions::pair_const(
        //     "perl",
        //     "https://github.com/tree-sitter-perl/tree-sitter-perl",
        //     &["@RabbiVeesh", "@LeoNerd"],
        // ),
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
            ("php", None),
            &["@tk-shirasaka", "@calebdw"],
        ),
        LanguageOptions::pair_const(
            "phpdoc",
            "https://github.com/claytonrcarter/tree-sitter-phpdoc",
            &["@mikehaertl"],
        ),
        LanguageOptions::pair_fn(
            "pioasm",
            "https://github.com/leo60228/tree-sitter-pioasm",
            &["@leo60228"],
        ),
        LanguageOptions::pair_const("po", "https://github.com/erasin/tree-sitter-po", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "pod",
            "https://github.com/tree-sitter-perl/tree-sitter-pod",
            &["@RabbiVeesh", "@LeoNerd"],
        ),
        LanguageOptions::pair_const(
            "poe_filter",
            "https://github.com/ObserverOfTime/tree-sitter-poe-filter",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const("pony", "https://github.com/amaanq/tree-sitter-pony", &[
            "@amaanq",
            "@mfelsche",
        ]),
        LanguageOptions::pair_const(
            "powershell",
            "https://github.com/airbus-cert/tree-sitter-powershell",
            &["@L2jLiga"],
        ),
        LanguageOptions::pair_const(
            "printf",
            "https://github.com/ObserverOfTime/tree-sitter-printf",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "prisma",
            "https://github.com/victorhqc/tree-sitter-prisma",
            &["@elianiva"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "problog",
            "https://github.com/foxyseta/tree-sitter-prolog",
            &[("problog", true), ("prolog", true)],
            ("prolog", None),
            &["@foxyseta"],
        ),
        LanguageOptions::pairs_with_symbol(
            "prolog",
            "https://github.com/foxyseta/tree-sitter-prolog",
            &[("prolog", true), ("problog", true)],
            &["@foxyseta"],
        ),
        LanguageOptions::pair_const(
            "promql",
            "https://github.com/MichaHoffmann/tree-sitter-promql",
            &["@MichaHoffmann"],
        ),
        LanguageOptions::pair_const(
            "properties",
            "https://github.com/tree-sitter-grammars/tree-sitter-properties",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const("proto", "https://github.com/treywood/tree-sitter-proto", &[
            "@treywood",
        ]),
        LanguageOptions::pair_const("prql", "https://github.com/PRQL/tree-sitter-prql", &[
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
            ("csv", None),
            &["@amaanq"],
        ),
        LanguageOptions::pair_const("pug", "https://github.com/zealot128/tree-sitter-pug", &[
            "@zealot128",
        ]),
        LanguageOptions::pair_const("puppet", "https://github.com/amaanq/tree-sitter-puppet", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "purescript",
            "https://github.com/postsolar/tree-sitter-purescript",
            &["@postsolar"],
        ),
        LanguageOptions::pair_const(
            "pymanifest",
            "https://github.com/ObserverOfTime/tree-sitter-pymanifest",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "python",
            "https://github.com/tree-sitter/tree-sitter-python",
            &["@stsewd", "@theHamsta"],
        ),
        LanguageOptions::pair_const("ql", "https://github.com/tree-sitter/tree-sitter-ql", &[
            "@pwntester",
        ]),
        LanguageOptions::pair_const(
            "qmldir",
            "https://github.com/Decodetalkers/tree-sitter-qmldir",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const("qmljs", "https://github.com/yuja/tree-sitter-qmljs", &[
            "@Decodetalkers",
        ]),
        LanguageOptions::pair_const(
            "query",
            "https://github.com/nvim-treesitter/tree-sitter-query",
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const("r", "https://github.com/r-lib/tree-sitter-r", &["@ribru17"]),
        LanguageOptions::pair_const("racket", "https://github.com/6cdh/tree-sitter-racket", &[]),
        LanguageOptions::pair_const("ralph", "https://github.com/alephium/tree-sitter-ralph", &[
            "@tdroxler",
        ]),
        LanguageOptions::pair_const("rasi", "https://github.com/Fymyte/tree-sitter-rasi", &[
            "@Fymyte",
        ]),
        LanguageOptions::pair_const("rbs", "https://github.com/joker1007/tree-sitter-rbs", &[
            "@joker1007",
        ]),
        LanguageOptions::pair_const("re2c", "https://github.com/amaanq/tree-sitter-re2c", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const(
            "readline",
            "https://github.com/ribru17/tree-sitter-readline",
            &["@ribru17"],
        ),
        LanguageOptions::pair_const(
            "regex",
            "https://github.com/tree-sitter/tree-sitter-regex",
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const(
            "rego",
            "https://github.com/FallenAngel97/tree-sitter-rego",
            &["@FallenAngel97"],
        ),
        LanguageOptions::pair_const(
            "requirements",
            "https://github.com/ObserverOfTime/tree-sitter-requirements",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "rescript",
            "https://github.com/rescript-lang/tree-sitter-rescript",
            &["@ribru17"],
        ),
        LanguageOptions::pair_const(
            "rnoweb",
            "https://github.com/bamonroe/tree-sitter-rnoweb",
            &["@bamonroe"],
        ),
        LanguageOptions::pair_const("robot", "https://github.com/Hubro/tree-sitter-robot", &[
            "@Hubro",
        ]),
        LanguageOptions::pair_const(
            "robots",
            "https://github.com/opa-oz/tree-sitter-robots-txt",
            &["@opa-oz"],
        ),
        LanguageOptions::pair_const("roc", "https://github.com/faldor20/tree-sitter-roc", &[
            "@nat-418",
        ]),
        LanguageOptions::pair_const("ron", "https://github.com/amaanq/tree-sitter-ron", &[
            "@amaanq",
        ]),
        LanguageOptions::pair_const("rst", "https://github.com/stsewd/tree-sitter-rst", &[
            "@stsewd",
        ]),
        LanguageOptions::pair_const(
            "ruby",
            "https://github.com/tree-sitter/tree-sitter-ruby",
            &["@TravonteD"],
        ),
        LanguageOptions::pair_const(
            "runescript",
            "https://github.com/2004Scape/tree-sitter-runescript",
            &["@2004Scape"],
        ),
        LanguageOptions::pair_const(
            "rust",
            "https://github.com/tree-sitter/tree-sitter-rust",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "scala",
            "https://github.com/tree-sitter/tree-sitter-scala",
            &["@stevanmilic"],
        ),
        LanguageOptions::pair_const("scfg", "https://github.com/rockorager/tree-sitter-scfg", &[
            "@WhyNotHugo",
        ]),
        LanguageOptions::pair_const("scheme", "https://github.com/6cdh/tree-sitter-scheme", &[]),
        LanguageOptions::pair_const("scss", "https://github.com/serenadeai/tree-sitter-scss", &[
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
            ("sfapex", None),
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pair_const(
            "slang",
            "https://github.com/theHamsta/tree-sitter-slang",
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const("slim", "https://github.com/theoo/tree-sitter-slim", &[
            "@theoo",
        ]),
        LanguageOptions::pair_const("slint", "https://github.com/slint-ui/tree-sitter-slint", &[
            "@hunger",
        ]),
        LanguageOptions::pair_const(
            "smali",
            "https://github.com/tree-sitter-grammars/tree-sitter-smali",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "snakemake",
            "https://github.com/osthomas/tree-sitter-snakemake",
            &[],
        ),
        LanguageOptions::pair_const(
            "smithy",
            "https://github.com/indoorvivants/tree-sitter-smithy",
            &["@amaanq", "@keynmol"],
        ),
        LanguageOptions::pair_const(
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
            ("sfapex", None),
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
            ("sfapex", None),
            &["@aheber", "@xixiaofinland"],
        ),
        LanguageOptions::pair_const(
            "sourcepawn",
            "https://github.com/nilshelmig/tree-sitter-sourcepawn",
            &["@Sarrus1"],
        ),
        LanguageOptions::pair_const(
            "sparql",
            "https://github.com/GordianDziwis/tree-sitter-sparql",
            &["@GordianDziwis"],
        ),
        LanguageOptions::pair_const("sql", "https://github.com/DerekStride/tree-sitter-sql", &[
            "@derekstride",
        ]),
        LanguageOptions::pair_const(
            "squirrel",
            "https://github.com/amaanq/tree-sitter-squirrel",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "ssh_config",
            "https://github.com/ObserverOfTime/tree-sitter-ssh-config",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "starlark",
            "https://github.com/amaanq/tree-sitter-starlark",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "strace",
            "https://github.com/sigmaSd/tree-sitter-strace",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "styled",
            "https://github.com/mskelton/tree-sitter-styled",
            &["@mskelton"],
        ),
        LanguageOptions::pair_const(
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
        LanguageOptions::pair_const(
            "surface",
            "https://github.com/connorlay/tree-sitter-surface",
            &["@connorlay"],
        ),
        LanguageOptions::pair_const(
            "svelte",
            "https://github.com/tree-sitter-grammars/tree-sitter-svelte",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const(
            "sway",
            "https://github.com/FuelLabs/tree-sitter-sway.git",
            &["@ribru17"],
        ),
        LanguageOptions::pair_const(
            "swift",
            "https://github.com/alex-pinkus/tree-sitter-swift",
            &["@alex-pinkus"],
        ),
        LanguageOptions::pair_const(
            "sxhkdrc",
            "https://github.com/RaafatTurki/tree-sitter-sxhkdrc",
            &["@RaafatTurki"],
        ),
        LanguageOptions::pair_const(
            "systemtap",
            "https://github.com/ok-ryoko/tree-sitter-systemtap",
            &["@ok-ryoko"],
        ),
        LanguageOptions::pair_const("t32", "https://gitlab.com/xasc/tree-sitter-t32.git", &[
            "@xasc",
        ]),
        LanguageOptions::pair_const(
            "tablegen",
            "https://github.com/amaanq/tree-sitter-tablegen",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const("tact", "https://github.com/tact-lang/tree-sitter-tact", &[
            "@novusnota",
        ]),
        LanguageOptions::pair_const(
            "teal",
            "https://github.com/euclidianAce/tree-sitter-teal",
            &["@euclidianAce"],
        ),
        LanguageOptions::pair_const(
            "templ",
            "https://github.com/vrischmann/tree-sitter-templ",
            &["@vrischmann"],
        ),
        LanguageOptions::pair_const(
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
        LanguageOptions::pair_const(
            "textproto",
            "https://github.com/PorterAtGoogle/tree-sitter-textproto",
            &["@Porter"],
        ),
        LanguageOptions::pair_const(
            "thrift",
            "https://github.com/duskmoon314/tree-sitter-thrift",
            &["@amaanq", "@duskmoon314"],
        ),
        LanguageOptions::pair_const(
            "tiger",
            "https://github.com/ambroisie/tree-sitter-tiger",
            &["@ambroisie"],
        ),
        LanguageOptions::pair_const(
            "tlaplus",
            "https://github.com/tlaplus-community/tree-sitter-tlaplus",
            &["@ahelwer", "@susliko"],
        ),
        LanguageOptions::pair_const("tmux", "https://github.com/Freed-Wu/tree-sitter-tmux", &[
            "@Freed-Wu",
        ]),
        LanguageOptions::pair_const(
            "todotxt",
            "https://github.com/arnarg/tree-sitter-todotxt",
            &["@arnarg"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "toml",
            "https://github.com/tree-sitter-grammars/tree-sitter-toml",
            &[("LANGUAGE", false)],
            ("toml-ng", None),
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
            ("csv", None),
            &["@amaanq"],
        ),
        LanguageOptions::pairs_with_symbol_and_crate(
            "tsx",
            "https://github.com/tree-sitter/tree-sitter-typescript",
            &[("LANGUAGE_TSX", false), ("LANGUAGE_TYPESCRIPT", false)],
            ("typescript", None),
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const(
            "turtle",
            "https://github.com/GordianDziwis/tree-sitter-turtle",
            &["@GordianDziwis"],
        ),
        LanguageOptions::pair_const("twig", "https://github.com/gbprod/tree-sitter-twig", &[
            "@gbprod",
        ]),
        LanguageOptions::pairs_with_symbol(
            "typescript",
            "https://github.com/tree-sitter/tree-sitter-typescript",
            &[("LANGUAGE_TYPESCRIPT", false), ("LANGUAGE_TSX", false)],
            &["@steelsojka"],
        ),
        LanguageOptions::pair_const(
            "typespec",
            "https://github.com/happenslol/tree-sitter-typespec",
            &["@happenslol"],
        ),
        LanguageOptions::pair_const(
            "typoscript",
            "https://github.com/Teddytrombone/tree-sitter-typoscript",
            &["@Teddytrombone"],
        ),
        // Fails to compile
        // LanguageOptions::pair_fn("typst", "https://github.com/uben0/tree-sitter-typst", &[
        //     "@uben0",
        //     "@RaafatTurki",
        // ]),
        LanguageOptions::pair_const(
            "udev",
            "https://github.com/ObserverOfTime/tree-sitter-udev",
            &["@ObserverOfTime"],
        ),
        LanguageOptions::pair_const(
            "ungrammar",
            "https://github.com/Philipp-M/tree-sitter-ungrammar",
            &["@Philipp-M", "@amaanq"],
        ),
        LanguageOptions::pair_const(
            "unison",
            "https://github.com/kylegoetz/tree-sitter-unison",
            &["@tapegram"],
        ),
        LanguageOptions::pair_const("usd", "https://github.com/ColinKennedy/tree-sitter-usd", &[
            "@ColinKennedy",
        ]),
        LanguageOptions::pair_const("uxntal", "https://github.com/amaanq/tree-sitter-uxntal", &[
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
        LanguageOptions::pair_const("vala", "https://github.com/vala-lang/tree-sitter-vala", &[
            "@Prince781",
        ]),
        LanguageOptions::pair_const("vento", "https://github.com/ventojs/tree-sitter-vento", &[
            "@wrapperup",
            "@oscarotero",
        ]),
        LanguageOptions::pair_const(
            "verilog",
            "https://github.com/tree-sitter/tree-sitter-verilog",
            &["@zhangwwpeng"],
        ),
        LanguageOptions::pair_const(
            "vhdl",
            "https://github.com/jpt13653903/tree-sitter-vhdl",
            &["@jpt13653903"],
        ),
        LanguageOptions::pair_const(
            "vhs",
            "https://github.com/charmbracelet/tree-sitter-vhs",
            &["@caarlos0"],
        ),
        LanguageOptions::pair_const("vim", "https://github.com/neovim/tree-sitter-vim", &[
            "@clason",
        ]),
        LanguageOptions::pair_const("vimdoc", "https://github.com/neovim/tree-sitter-vimdoc", &[
            "@clason",
        ]),
        LanguageOptions::pair_const("vrl", "https://github.com/belltoy/tree-sitter-vrl", &[
            "@belltoy",
        ]),
        LanguageOptions::pair_const(
            "vue",
            "https://github.com/tree-sitter-grammars/tree-sitter-vue",
            &["@WhyNotHugo", "@lucario387"],
        ),
        LanguageOptions::pair_const("wgsl", "https://github.com/szebniok/tree-sitter-wgsl", &[
            "@szebniok",
        ]),
        LanguageOptions::pair_const(
            "wgsl_bevy",
            "https://github.com/theHamsta/tree-sitter-wgsl-bevy",
            &["@theHamsta"],
        ),
        LanguageOptions::pair_const("wing", "https://github.com/winglang/tree-sitter-wing", &[
            "@gshpychka",
            "@MarkMcCulloh",
        ]),
        LanguageOptions::pair_const("wit", "https://github.com/liamwh/tree-sitter-wit", &[
            "@liamwh",
        ]),
        LanguageOptions::pair_const(
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
        LanguageOptions::pair_const(
            "xresources",
            "https://github.com/ValdezFOmar/tree-sitter-xresources",
            &["@ValdezFOmar"],
        ),
        LanguageOptions::pair_const(
            "yaml",
            "https://github.com/tree-sitter-grammars/tree-sitter-yaml",
            &["@amaanq"],
        ),
        LanguageOptions::pair_const("yang", "https://github.com/Hubro/tree-sitter-yang", &[
            "@Hubro",
        ]),
        LanguageOptions::pair_fn("yuck", "https://github.com/Philipp-M/tree-sitter-yuck", &[
            "@Philipp-M",
            "@amaanq",
        ]),
        LanguageOptions::pair_fn(
            "zathurarc",
            "https://github.com/Freed-Wu/tree-sitter-zathurarc",
            &["@Freed-Wu"],
        ),
        LanguageOptions::pair_const(
            "zig",
            "https://github.com/tree-sitter-grammars/tree-sitter-zig",
            &["@amaanq"],
        ),
        LanguageOptions::pair_fn("ziggy", "https://github.com/kristoff-it/ziggy", &[
            "@rockorager",
        ]),
        LanguageOptions::pair_fn("ziggy_schema", "https://github.com/kristoff-it/ziggy", &[
            "@rockorager",
        ]),
    ])
});
