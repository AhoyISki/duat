//! Tests for the duat book
//!
//! Items are sorted by the order they show up in the book.
#[macro_export]
macro_rules! test {
    ($path:literal, $name:ident { $($module:tt)* }) => {
        #[doc = include_str!(concat!("src/", $path, ".md"))]
        mod $name {
            $($module)*
        }
    }
}

test!("introduction", introduction {});
test!("installation", installation {});

test!("quick-settings/chapter", quick_settings {
    use crate::test;
    test!(
        "quick-settings/setup-and-setup-duat",
        setup_and_setup_duat {}
    );

    test!("quick-settings/prelude", prelude {});
    test!("quick-settings/frequent-snippets", frequent_snippets {});
});

test!("scripting-duat/chapter", scripting_duat {
    use crate::test;
    test!("scripting-duat/pass", pass {});
    test!("scripting-duat/hook-module", hook_module {});
    test!("scripting-duat/text", text {});
    test!("scripting-duat/mod-status", mod_status {});
    test!("scripting-duat/context-module", context_module {});
    test!("scripting-duat/cmd-module", cmd_module {});
    test!("scripting-duat/mod-layout", mod_layout {});
});

test!("extending-duat/chapter", extending_duat {
    use crate::test;
    test!("extending-duat/plugins", plugins {});
    test!("extending-duat/widgets", widgets {});
    test!("extending-duat/modes", modes {});
    test!("extending-duat/parsers", parsers {});
});
