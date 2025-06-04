#![feature(decl_macro, closure_lifetime_binder, default_field_values)]

pub mod modes;
pub mod state;
pub mod widgets;

mod private_exports {
    pub use duat_core;
    pub use format_like::format_like;

    pub macro parse_str($appender_checker:expr, $str:literal) {{
        use crate::{
            private_exports::duat_core::{context::FileHandle, data::Pass, text::Builder},
            widgets::State,
        };

        let (mut appender, checker) = $appender_checker;
        let (mut ap, _) = State::from($str).fns();

        let appender = move |pa: &Pass, builder: &mut Builder, reader: &FileHandle<_>| {
            appender(pa, builder, reader);
            ap(pa, builder, reader);
        };

        (appender, checker)
    }}

    pub macro parse_status_part {
        ($appender_checker:expr, "", $part:expr) => {{
            use crate::{
                private_exports::duat_core::{context::FileHandle, data::Pass, text::Builder},
                widgets::State,
            };

			#[allow(unused_mut)]
    		let (mut appender, checker) = $appender_checker;
            let (mut ap, ch) = State::from($part).fns();

            let checker = move || checker() || ch();

            let pre_fn = move |pa: &Pass, builder: &mut Builder, handle: &FileHandle<_>| {
                appender(pa, builder, handle);
                ap(pa, builder, handle);
            };

            (pre_fn, checker)
        }},
        ($appender_checker:expr, $modif:literal, $part:expr) => {{
            use crate::{
                private_exports::duat_core::{context::FileHandle, data::Pass, text::Builder},
                widgets::State,
            };

            let (mut appender, checker) = $appender_checker;
            let (mut ap, ch) =
                State::from(format!(concat!("{:", $modif, "}"), $part)).fns();

            let checker = move || checker() || ch();

            let appender = move |pa: &Pass, builder: &mut Builder, handle: &FileHandle<_>| {
                appender(pa, builder, handle);
                ap(pa, builder, handle);
            };

            (appender, checker)
        }}
    }

    pub macro parse_form {
        ($appender_checker:expr, "",) => {{
            use crate::{
                private_exports::duat_core::{context::FileHandle, data::Pass, form, text::Builder},
            };

            let (mut appender, checker) = $appender_checker;
            let appender = move |pa: &Pass, builder: &mut Builder, handle: &FileHandle<_>| {
                appender(builder, handle);
                builder.push(form::DEFAULT_ID);
            };

            (appender, checker)
        }},
        ($appender_checker:expr, "", $($form:tt)*) => {{
            use crate::{
                private_exports::duat_core::{context::FileHandle, data::Pass, form, text::Builder},
            };

            let (mut appender, checker) = $appender_checker;
            let id = form::id_of!(concat!($(stringify!($form)),*));

            let appender = move |pa: &Pass, builder: &mut Builder, handle: &FileHandle<_>| {
                appender(pa, builder, handle);
                builder.push(id);
            };

            (appender, checker)
        }},
        ($pre_fn_and_checker:expr, $modif:literal, $($form:ident).*) => {{
            compile_error!(concat!("at the moment, Forms don't support modifiers like ", $modif))
        }}
    }
}
