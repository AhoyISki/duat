//! Prefix autocompletion for various filetypes.
//!
//! This module provides the [`AutoPrefix`] trait, which lets [`Cursor`]s add prefixes (e.g. comments, list headings, etc).
use std::{collections::HashMap, sync::LazyLock};

use super::FileType;
use duat_core::{mode::Cursor, text::RegexHaystack};

/// Trait to add the comment prefix of the previous line.
pub trait AutoPrefix {
    /// Adds the comment prefix of the previous line
    ///
    /// This function will assume that the current cursor position is
    /// where you want to add the prefix, with indentation already
    /// taken into account.
    ///
    /// Returns `true` if the line was commented.
    fn add_comment(&mut self) -> bool;
}

impl AutoPrefix for Cursor<'_> {
    fn add_comment(&mut self) -> bool {
        let Some(prev_lnum) = self.caret().line().checked_sub(1) else {
            return false;
        };

        if let Some(filetype) = self.filetype()
            && let Some(prefixes) = PREFIXES.get(filetype)
        {
            let line = self.text().line(prev_lnum);
            for prefix in prefixes.iter() {
                if let Some(range) = line.search(prefix.from).next()
                    && prefix.blocker.is_none_or(|blocker| {
                        line.search(blocker)
                            .next()
                            .is_none_or(|blocker_range| blocker_range.end <= range.start)
                    })
                {
                    self.insert(prefix.set);
                    self.move_hor(prefix.set.chars().count() as i32);

                    return true;
                }
            }
        }

        false
    }
}

struct Prefix {
    from: &'static str,
    set: &'static str,
    blocker: Option<&'static str>,
}

const fn pf(from: &'static str, set: &'static str, blocker: Option<&'static str>) -> Prefix {
    Prefix { from, set, blocker }
}

// List taken from https://gist.github.com/dk949/88b2652284234f723decaeb84db2576c
static PREFIXES: LazyLock<HashMap<&str, &[Prefix]>> = LazyLock::new(|| {
    const HASH: Prefix = pf(r"^\s*#", "#", None);
    const DOUBLE_HASH: &[Prefix] = &[pf(r"^\s*##", "##", None)];

    const SLASHES: Prefix = pf(r"^\s*//", "//", None);
    const DOC_SLASHES: Prefix = pf(r"^\s*///", "///", None);
    const INNER_DOC_SLASHES: Prefix = pf(r"^\s*//!", "//!", None);
    const ASTERISK: Prefix = pf(r"^\s*\*", " *", Some(r#"\*/"#));
    const SLASH_ASTERISK: Prefix = pf(r"^\s*/\*", " *", Some(r#"\*/"#));
    const SLASH_ASTERISK_LINE: &[Prefix] = &[pf(r"^\s*/\*", "/*", None)];

    const QUOTE: Prefix = pf(r"^\s*'", "'", None);
    const DQUOTE: Prefix = pf(r#"^\s*""#, r#"""#, None);
    const SEMICOLON: Prefix = pf(r"^\s;", ";", None);
    const PERCENT: Prefix = pf(r"^\s*%", "%", None);
    const EXCLAMATION: Prefix = pf(r"^\s*!", "!", None);
    const DOLLAR_SIGN: Prefix = pf(r"^\s*$", "$", None);
    const SLASH: Prefix = pf(r"^\s*/", "/", None);

    const DASHES: Prefix = pf(r"^\s*--", "--", None);
    const REM: Prefix = pf("^REM", "REM", None);
    const DOT_ESCAPED_DQUOTE: &[Prefix] = &[pf(r#"^\s\.\\""#, r#".\""#, None)];
    const DOUBLE_COLON: Prefix = pf(r"^\s*::", "::", None);
    const ASTERISK_LINE: Prefix = pf(r"^\s*\*", "*", None);
    const LISP: &[Prefix] = &[
        const { pf(r"^\s*;;;;", ";;;;", None) },
        const { pf(r"^\s*;;;", ";;;", None) },
        const { pf(r"^\s*;;", ";;", None) },
        SEMICOLON,
    ];

    HashMap::from_iter([
        // Hash group
        ("aap", &[HASH] as &[Prefix]),
        ("ampl", &[HASH]),
        ("ansible", &[HASH]),
        ("apache", &[HASH]),
        ("apachestyle", &[HASH]),
        ("awk", &[HASH]),
        ("bc", &[HASH]),
        ("cairo", &[HASH]),
        ("cfg", &[HASH]),
        ("cl", &[HASH]),
        ("cmake", &[HASH]),
        ("coffeescript", &[HASH]),
        ("conkyrc", &[HASH]),
        ("crontab", &[HASH]),
        ("cucumber", &[HASH]),
        ("cython", &[HASH]),
        ("dakota", &[HASH]),
        ("debcontrol", &[HASH]),
        ("debsources", &[HASH]),
        ("desktop", &[HASH]),
        ("dhcpd", &[HASH]),
        ("diff", &[HASH]),
        ("dockerfile", &[HASH]),
        ("ebuild", &[HASH]),
        ("ecd", &[HASH]),
        ("eclass", &[HASH]),
        ("elixir", &[HASH]),
        ("elmfilt", &[HASH]),
        ("ember-script", &[HASH]),
        ("esmtprc", &[HASH]),
        ("exim", &[HASH]),
        ("expect", &[HASH]),
        ("exports", &[HASH]),
        ("fancy", &[HASH]),
        ("fgl", &[HASH]),
        ("fluent", &[HASH]),
        ("fstab", &[HASH]),
        ("fvwm", &[HASH]),
        ("gdb", &[HASH]),
        ("gdscript3", &[HASH]),
        ("gentoo-conf-d", &[HASH]),
        ("gentoo-env-d", &[HASH]),
        ("gentoo-init-d", &[HASH]),
        ("gentoo-make-conf", &[HASH]),
        ("gentoo-package-keywords", &[HASH]),
        ("gentoo-package-mask", &[HASH]),
        ("gentoo-package-use", &[HASH]),
        ("gitcommit", &[HASH]),
        ("gitignore", &[HASH]),
        ("gitrebase", &[HASH]),
        ("gnuplot", &[HASH]),
        ("gtkrc", &[HASH]),
        ("hb", &[HASH]),
        ("hog", &[HASH]),
        ("hostsaccess", &[HASH]),
        ("hxml", &[HASH]),
        ("ia64", &[HASH]),
        ("icon", &[HASH]),
        ("inittab", &[HASH]),
        ("jproperties", &[HASH]),
        ("julia", &[HASH]),
        ("kivy", &[HASH]),
        ("ldif", &[HASH]),
        ("lilo", &[HASH]),
        ("lout", &[HASH]),
        ("lss", &[HASH]),
        ("lynx", &[HASH]),
        ("mako", DOUBLE_HASH),
        ("maple", &[HASH]),
        ("meson", &[HASH]),
        ("mips", &[HASH]),
        ("mirah", &[HASH]),
        ("mush", &[HASH]),
        ("nginx", &[HASH]),
        ("nimrod", &[HASH]),
        ("nix", &[HASH]),
        ("nsis", &[HASH]),
        ("ntp", &[HASH]),
        ("ora", &[HASH]),
        ("paludis-use-conf", &[HASH]),
        ("pcap", &[HASH]),
        ("perl", &[HASH]),
        ("pine", &[HASH]),
        ("po", &[HASH]),
        ("praat", &[HASH]),
        ("privoxy", &[HASH]),
        ("ps1", &[HASH]),
        ("psf", &[HASH]),
        ("ptcap", &[HASH]),
        ("puppet", &[HASH]),
        ("pyrex", &[HASH]),
        ("python", &[HASH]),
        ("radiance", &[HASH]),
        ("ratpoison", &[HASH]),
        ("rego", &[HASH]),
        ("remind", &[HASH]),
        ("resolv", &[HASH]),
        ("rib", &[HASH]),
        ("robot", &[HASH]),
        ("robots", &[HASH]),
        ("rspec", &[HASH]),
        ("ruby", &[HASH]),
        ("scons", &[HASH]),
        ("sdc", &[HASH]),
        ("sed", &[HASH]),
        ("sh", &[HASH]),
        ("shader_test", &[HASH]),
        ("sls", &[HASH]),
        ("sm", &[HASH]),
        ("snakemake", &[HASH]),
        ("snippets", &[HASH]),
        ("snnsnet", &[HASH]),
        ("snnspat", &[HASH]),
        ("snnsres", &[HASH]),
        ("spec", &[HASH]),
        ("squid", &[HASH]),
        ("sshconfig", &[HASH]),
        ("sshdconfig", &[HASH]),
        ("tcl", &[HASH]),
        ("tf", &[HASH]),
        ("tidy", &[HASH]),
        ("tli", &[HASH]),
        ("tmux", &[HASH]),
        ("toml", &[HASH]),
        ("tsscl", &[HASH]),
        ("ttl", &[HASH]),
        ("tup", &[HASH]),
        ("upstart", &[HASH]),
        ("vgrindefs", &[HASH]),
        ("vrml", &[HASH]),
        ("wget", &[HASH]),
        ("wml", &[HASH]),
        ("xmath", &[HASH]),
        ("yaml", &[HASH]),
        ("r", &[HASH]),
        ("renpy", &[HASH]),
        ("htmlcheetah", DOUBLE_HASH),
        ("velocity", DOUBLE_HASH),
        ("webmacro", DOUBLE_HASH),
        // Slash group
        ("groff", const { &[pf(r"^\s*\#", r"\#", None)] }),
        ("acedb", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("actionscript", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("asy", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("bind-named", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("cg", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("ch", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("clean", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("clipper", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("cs", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("cuda", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("d", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("dot", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("dylan", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("fx", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("glsl", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("go", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("groovy", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("h", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("haxe", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("hercules", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("hyphy", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("idl", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("ishd", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("java", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("javacc", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("javascript", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("javascript.jquery", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("json5", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("jsonc", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("jsonnet", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("kscript", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("lpc", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("mel", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("named", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("objc", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("objcpp", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("objj", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("ooc", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("pccts", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("php", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("pike", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("pilrc", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("plm", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("pov", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("processing", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("proto", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("rc", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("scala", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("scss", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("slice", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("stan", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("stp", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("supercollider", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("swift", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("systemverilog", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("tads", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("teak", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("tsalt", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("typescript", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("uc", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("vala", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("vera", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("verilog", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        (
            "verilog_systemverilog",
            &[SLASH_ASTERISK, ASTERISK, SLASHES],
        ),
        ("sass", &[SLASH_ASTERISK, ASTERISK, SLASHES]),
        ("asciidoc", &[SLASHES]),
        ("ats", &[SLASHES]),
        ("bib", &[SLASHES]),
        ("calibre", &[SLASHES]),
        ("cocci", &[SLASHES]),
        ("cypher", &[SLASHES]),
        ("faust", &[SLASHES]),
        ("fsharp", &[SLASHES]),
        ("openroad", &[SLASHES]),
        ("ox", &[SLASHES]),
        ("pfmain", &[SLASHES]),
        ("scilab", &[SLASHES]),
        ("specman", &[SLASHES]),
        ("xkb", &[SLASHES]),
        ("c", &[SLASH_ASTERISK, ASTERISK, DOC_SLASHES, SLASHES]),
        ("cpp", &[SLASH_ASTERISK, ASTERISK, DOC_SLASHES, SLASHES]),
        (
            "rust",
            &[
                SLASH_ASTERISK,
                ASTERISK,
                DOC_SLASHES,
                INNER_DOC_SLASHES,
                SLASHES,
            ],
        ),
        ("zig", &[DOC_SLASHES, INNER_DOC_SLASHES, SLASHES]),
        ("spectre", &[SLASHES, const { pf(r"^\s*\*", "*", None) }]),
        ("emblem", &[const { pf(r"^\s*/", "/", None) }]),
        ("aml", SLASH_ASTERISK_LINE),
        ("natural", SLASH_ASTERISK_LINE),
        ("vsejcl", SLASH_ASTERISK_LINE),
        ("less", &[SLASH_ASTERISK]),
        // Tag group
        ("aspvbs", &[QUOTE]),
        // Dash group
        ("ada", &[DASHES]),
        ("ahdl", &[DASHES]),
        ("applescript", &[DASHES]),
        ("asn", &[DASHES]),
        ("cabal", &[DASHES]),
        ("csp", &[DASHES]),
        ("eiffel", &[DASHES]),
        ("elm", &[DASHES]),
        ("gdmo", &[DASHES]),
        ("haskell", &[DASHES]),
        ("hive", &[DASHES]),
        ("idris", &[DASHES]),
        ("lace", &[DASHES]),
        ("lean", &[DASHES]),
        ("lua", &[DASHES]),
        ("mib", &[DASHES]),
        ("occam", &[DASHES]),
        ("sa", &[DASHES]),
        ("sather", &[DASHES]),
        ("sqlforms", &[DASHES]),
        ("sqlj", &[DASHES]),
        ("vhdl", &[DASHES]),
        ("lhaskell", const { &[pf(r"^\s*>--", ">--", None)] }),
        // Semicolon group
        ("amiga", &[SEMICOLON]),
        ("armasm", &[SEMICOLON]),
        ("asm68k", &[SEMICOLON]),
        ("autohotkey", &[SEMICOLON]),
        ("asterisk", &[SEMICOLON]),
        ("autoit", &[SEMICOLON]),
        ("bindzone", &[SEMICOLON]),
        ("clojure", &[SEMICOLON]),
        ("def", &[SEMICOLON]),
        ("dns", &[SEMICOLON]),
        ("dosini", &[SEMICOLON]),
        ("dracula", &[SEMICOLON]),
        ("dsl", &[SEMICOLON]),
        ("fasm", &[SEMICOLON]),
        ("gitconfig", &[SEMICOLON]),
        ("idlang", &[SEMICOLON]),
        ("iss", &[SEMICOLON]),
        ("jess", &[SEMICOLON]),
        ("kix", &[SEMICOLON]),
        ("llvm", &[SEMICOLON]),
        ("masm", &[SEMICOLON]),
        ("monk", &[SEMICOLON]),
        ("nagios", &[SEMICOLON]),
        ("nasm", &[SEMICOLON]),
        ("ncf", &[SEMICOLON]),
        ("newlisp", &[SEMICOLON]),
        ("omnimark", &[SEMICOLON]),
        ("pic", &[SEMICOLON]),
        ("povini", &[SEMICOLON]),
        ("racket", &[SEMICOLON]),
        ("rebol", &[SEMICOLON]),
        ("registry", &[SEMICOLON]),
        ("scsh", &[SEMICOLON]),
        ("skill", &[SEMICOLON]),
        ("smith", &[SEMICOLON]),
        ("ss", &[SEMICOLON]),
        ("tags", &[SEMICOLON]),
        ("tasm", &[SEMICOLON]),
        ("winbatch", &[SEMICOLON]),
        ("wvdial", &[SEMICOLON]),
        ("z8a", &[SEMICOLON]),
        ("ppwiz", &[SEMICOLON]),
        ("asm", &[SEMICOLON, HASH]),
        ("samba", &[SEMICOLON, HASH]),
        ("ledger", &[SEMICOLON, HASH]),
        ("closure", LISP),
        ("fennel", LISP),
        ("lisp", LISP),
        ("m17ndb", LISP),
        ("scheme", LISP),
        // Keyword group
        ("basic", &[QUOTE, REM]),
        ("simula", &[PERCENT]),
        ("cvs", const { &[pf("^CVS:", "^CVS:", None)] }),
        ("dosbatch", &[REM, DOUBLE_COLON]),
        ("m4", const { &[pf(r"^\s*dnl", "dnl", None)] }),
        ("opl", &[REM]),
        (
            "texinfo",
            const {
                &[
                    pf(r"^\s*@comment", "@comment", None),
                    pf(r"^\s*@c", "@c", None),
                ]
            },
        ),
        // Percent group
        ("abc", &[PERCENT]),
        ("asp", &[PERCENT]),
        ("bbx", &[PERCENT]),
        ("bst", &[PERCENT]),
        ("ist", &[PERCENT]),
        ("lilypond", &[PERCENT]),
        ("lprolog", &[PERCENT]),
        ("lytex", &[PERCENT]),
        ("map", &[PERCENT]),
        ("matlab", &[PERCENT]),
        ("pdf", &[PERCENT]),
        ("postscr", &[PERCENT]),
        ("ppd", &[PERCENT]),
        ("slang", &[PERCENT]),
        ("slrnrc", &[PERCENT]),
        ("tex", &[PERCENT]),
        ("texmf", &[PERCENT]),
        ("txt2tags", &[PERCENT]),
        ("virata", &[PERCENT]),
        ("sile", &[PERCENT]),
        ("erlang", &[PERCENT]),
        // Double quote group
        ("ave", &[QUOTE]),
        ("elf", &[QUOTE]),
        ("lscript", &[QUOTE]),
        ("spin", &[QUOTE]),
        ("vb", &[QUOTE]),
        ("man", const { &[pf(r#"^\s\.""#, r#".""#, None)] }),
        ("mandoc", DOT_ESCAPED_DQUOTE),
        ("troff", DOT_ESCAPED_DQUOTE),
        ("nroff", &[DQUOTE]),
        ("st", &[DQUOTE]),
        ("vim", &[DQUOTE]),
        // Exclamation mark group
        ("apdl", &[EXCLAMATION]),
        ("fortran", &[EXCLAMATION]),
        ("incar", &[EXCLAMATION]),
        ("inform", &[EXCLAMATION]),
        ("molpro", &[EXCLAMATION]),
        ("poscar", &[EXCLAMATION]),
        ("rgb", &[EXCLAMATION]),
        ("sqr", &[EXCLAMATION]),
        ("uc4", &[EXCLAMATION]),
        ("uil", &[EXCLAMATION]),
        ("vasp", &[EXCLAMATION]),
        ("xdefaults", &[EXCLAMATION]),
        ("xpm2", &[EXCLAMATION]),
        (
            "factor",
            &[EXCLAMATION, const { pf(r"^\s*!#", "!#", None) }],
        ),
        // Dollar sign group
        ("master", &[DOLLAR_SIGN]),
        ("nastran", &[DOLLAR_SIGN]),
        ("patran", &[DOLLAR_SIGN]),
        ("sinda", &[DOLLAR_SIGN]),
        ("spice", &[DOLLAR_SIGN]),
        ("tak", &[DOLLAR_SIGN]),
        ("trasys", &[DOLLAR_SIGN]),
        ("dcl", const { &[pf(r"^\s*$!", "$!", None)] }),
        // Mixed group
        ("hocon", &[SLASHES, HASH]),
        ("octave", &[PERCENT, HASH]),
        ("plsql", &[DASHES]),
        ("sql", &[DASHES]),
        ("prolog", &[PERCENT]),
        ("minizinc", &[PERCENT]),
        ("sentinel", &[HASH]),
        ("terraform", &[HASH]),
        // Other
        ("btm", &[DOUBLE_COLON]),
        ("caos", &[ASTERISK_LINE]),
        ("cterm", &[ASTERISK_LINE]),
        ("form", &[ASTERISK_LINE]),
        ("foxpro", &[ASTERISK_LINE]),
        ("gams", &[ASTERISK_LINE]),
        ("sicad", &[ASTERISK_LINE]),
        ("snobol4", &[ASTERISK_LINE]),
        ("focexec", const { &[pf(r"^\s*-\*", "-*", None)] }),
        ("haml", &[const { pf(r"^\s*-#", "-#", None) }, SLASH]),
        ("haml", &[SLASH, const { pf(r"^\s*/!", "/!", None) }]),
    ])
});
