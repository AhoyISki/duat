//! Tests for the duat book
//!
//! Items are sorted by the order they show up in the book.
macro test($path:literal, $name:ident { $($module:tt)* }) {
    #[doc = include_str!(concat!("src/", $path, ".md"))]
    mod $name {
        $($module)*
    }
}

test!("introduction", introduction {});
test!("installation", installation {});

test!("quick-settings/chapter", quick_settings {
    use crate::book::test;
    test!(
        "quick-settings/setup-and-setup-duat",
        setup_and_setup_duat {}
    );

    test!("quick-settings/prelude/chapter", prelude {
        use crate::book::test;
        test!("quick-settings/prelude/print-module", print_module {});
        test!("quick-settings/prelude/form-module", form_module {});
        test!("quick-settings/prelude/map-and-alias", map_and_alias {});
        test!("quick-settings/prelude/cursor-module", cursor_module {});
    });

    test!("quick-settings/frequent-snippets/chapter", frequent_snippets {
        use crate::book::test;
        test!("quick-settings/frequent-snippets/map-jk", map_jk {});
        test!("quick-settings/frequent-snippets/status-per-file", status_per_file {});
        test!("quick-settings/frequent-snippets/prompt-status-line", prompt_status_line {});
        test!("quick-settings/frequent-snippets/common-status", common_status {});
        test!("quick-settings/frequent-snippets/rel-align-linenums", rel_align_linenums {});
        test!("quick-settings/frequent-snippets/file-tabstops", file_tabstops {});
        test!("quick-settings/frequent-snippets/nerd-status", nerd_status {});
        test!("quick-settings/frequent-snippets/file-window-status", file_window_status {});
    });

    test!("quick-settings/list-of-forms", list_of_forms {});
});

test!("scripting-duat/chapter", scripting_duat {
    use crate::book::test;
    test!("scripting-duat/pass", pass {});
    test!("scripting-duat/hook-module", hook_module {});

    test!("scripting-duat/text/chapter", text {
        use crate::book::test;
        test!("scripting-duat/text/builder-and-txt", builder_and_txt {});
        test!("scripting-duat/text/tags", tags {});
    });

    test!("scripting-duat/mod-status", mod_status {});
    test!("scripting-duat/context-module", context_module {});
    test!("scripting-duat/cmd-module", cmd_module {});
    test!("scripting-duat/mod-layout", mod_layout {});
});

test!("extending-duat/chapter", extending_duat {
    use crate::book::test;
    test!("extending-duat/duat-core", duat_core {});
    test!("extending-duat/plugins", plugins {});
    test!("extending-duat/widgets", widgets {});
    test!("extending-duat/modes", modes {});
    test!("extending-duat/parsers", parsers {});
});
