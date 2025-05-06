#![feature(decl_macro)]

pub mod modes;
pub mod state;
pub mod widgets;

mod private_exports {
    pub use duat_core;
    pub use format_like::format_like;

    pub macro parse_str($pre_fn_and_checker:expr, $str:literal) {{
        use crate::{
            private_exports::duat_core::text::Builder,
            widgets::{Reader, State},
        };

        let (mut pre_fn, checker) = $pre_fn_and_checker;

        let (mut appender, new_checker) = State::from($str).fns();

        let checker = move || checker() || new_checker();

        let pre_fn = move |builder: &mut Builder, reader: &mut Reader<_>| {
            pre_fn(builder, reader);
            appender(builder, reader);
        };

        (pre_fn, checker)
    }}

    pub macro parse_status_part {
        ($pre_fn_and_checker:expr, "", $part:expr) => {{
            use crate::{
                private_exports::duat_core::text::Builder,
                widgets::{Reader, State},
            };

            #[allow(unused_mut)]
            let (mut pre_fn, checker) = $pre_fn_and_checker;

            let (mut appender, new_checker) = State::from($part).fns();

            let checker = move || checker() || new_checker();

            let pre_fn = move |builder: &mut Builder, reader: &mut Reader<_>| {
                pre_fn(builder, reader);
                appender(builder, reader);
            };

            (pre_fn, checker)
        }},
        ($pre_fn_and_checker:expr, $modif:literal, $part:expr) => {{
            use crate::{
                private_exports::duat_core::text::Builder,
                widgets::status_line::{Reader, State},
            };

            let (mut pre_fn, checker) = $pre_fn_and_checker;

            let (mut appender, ch) = State::from(format!(concat!("{:", $modif, "}"), $part)).fns();

            let checker = move || checker() || ch();

            let pre_fn = move |builder: &mut Builder, reader: &mut Reader<_>| {
                pre_fn(builder, reader);
                appender(builder, reader);
            };

            (pre_fn, checker)
        }}
    }

    pub macro parse_form {
        (pre_fn_and_checker:expr, "",) => {{
            use crate::{
                private_exports::duat_core::{form, text::Builder},
                widgets::Reader,
            };

            let (mut pre_fn, checker) = $pre_fn_and_checker;

            let pre_fn = move |builder: &mut Builder, reader: &mut Reader<_>| {
                pre_fn(builder, reader);
                builder.push(form::DEFAULT_ID);
            };

            (pre_fn, checker)
        }},
        ($pre_fn_and_checker:expr, "",$($form:tt)*) => {{
            use crate::{
                private_exports::duat_core::{form, text::Builder},
                widgets::Reader,
            };

            let (mut pre_fn, checker) = $pre_fn_and_checker;

            let id = form::id_of!(concat!($(stringify!($form)),*));

            let pre_fn = move |builder: &mut Builder, reader: &mut Reader<_>| {
                pre_fn(builder, reader);
                builder.push(id);
            };

            (pre_fn, checker)
        }},
        ($pre_fn_and_checker:expr, $modif:literal, $($form:ident).*) => {{
            compile_error!(concat!("at the moment, Forms don't support modifiers like ", $modif))
        }}
    }
}
