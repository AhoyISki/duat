/// The macro that creates a [`StatusLine`]
///
/// This macro works like the [`txt!`] macro, in  that [`Form`]s
/// are pushed with `[{FormName}]`. However, [`txt!`]  is
/// evaluated immediately, while [`status!`] is evaluated when
/// updates occur.
///
/// The macro will mostly read from the [`Buffer`] widget and its
/// related structs. In order to do that, it will accept functions
/// as arguments. These functions can take any of the following
/// parameters, with up to 8 arguments each:
///
/// - [`&Buffer`]: The `Buffer` in question.
/// - [`&Handle`]: The `Handle` of said `Buffer`.
/// - [`&Area`]: The `Area` of said `Buffer`.
/// - [`&Pass`]: For global reading access.
/// - [`&Text`]: The `Text` of the `Buffer`.
/// - [`&Selections`]: The `Selections` of the `Buffer`.
/// - [`&Selection`]: The main `Selection` of the `Buffer`.
/// - [`&Window`]: The `Window` the `Buffer` is situated in.
///
/// Here's some examples:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::widgets::status;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn name_but_funky(buf: &Buffer) -> String {
///     buf.name()
///         .chars()
///         .enumerate()
///         .map(|(i, char)| {
///             if i % 2 == 1 {
///                 char.to_uppercase().to_string()
///             } else {
///                 char.to_lowercase().to_string()
///             }
///         })
///         .collect()
/// }
///
/// fn powerline_main_txt(buffer: &Buffer, area: &Area) -> Text {
///     let selections = buffer.selections();
///     let opts = buffer.get_print_opts();
///     let v_caret = selections
///         .get_main()
///         .unwrap()
///         .v_caret(buffer.text(), area, opts);
///
///     txt!(
///         "[separator][coord]{}[separator][coord]{}[separator][coord]{}",
///         v_caret.visual_col(),
///         v_caret.line(),
///         buffer.len_lines()
///     )
/// }
///
/// fn setup() {
///     opts::set_status(|pa| status!("[buffer]{name_but_funky}{Spacer}{powerline_main_txt}"));
/// }
/// ```
///
/// There are other types of arguments you can push, not
/// necessarily tied to a `Buffer`:
///
/// - Static arguments:
///   - A [`Text`] argument, which can be formatted in a similar way
///     throught the [`txt!`] macro;
///   - Any [`impl Display`], such as numbers, strings, chars, etc.
///     [`impl Debug`] types also work, when including the usual
///     `":?"` and derived suffixes;
/// - Dynamic arguments:
///   - An [`RwData`] or [`DataMap`]s of the previous two types. These
///     will update whenever the data inside is changed;
///
/// Here's an examples:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::widgets::status;
/// setup_duat!(setup);
/// use std::sync::atomic::{AtomicUsize, Ordering};
///
/// use duat::prelude::*;
///
/// fn setup() {
///     let changing_str = data::RwData::new("Initial text".to_string());
///
///     fn counter(update: bool) -> usize {
///         static COUNT: AtomicUsize = AtomicUsize::new(0);
///         if update {
///             COUNT.fetch_add(1, Ordering::Relaxed) + 1
///         } else {
///             COUNT.load(Ordering::Relaxed)
///         }
///     }
///
///     hook::add::<WindowOpened>({
///         let changing_str = changing_str.clone();
///         move |pa, window| {
///             let changing_str = changing_str.clone();
///             let checker = changing_str.checker();
///
///             let text = txt!("Static text");
///             let counter = move || counter(checker());
///
///             status!("{changing_str} [counter]{counter}[] {text}")
///                 .above()
///                 .push_on(pa, window);
///             Ok(())
///         }
///     });
///
///     cmd::add("set-str", move |pa: &mut Pass, new: String| {
///         *changing_str.write(pa) = new.to_string();
///         Ok(None)
///     });
/// }
/// ```
///
/// In the above example, I added some dynamic [`Text`], through
/// the usage of an [`RwData<Text>`], I added some static
/// [`Text`], some [`Form`]s (`"counter"` and `"default"`) and
/// even a counter,.
///
/// [`StatusLine`]: super::StatusLine
/// [`txt!`]: duat_core::text::txt
/// [`Buffer`]: duat_core::buffer::Buffer
/// [`&Buffer`]: duat_core::buffer::Buffer
/// [`&Handle`]: duat_core::context::Handle
/// [`&Area`]: duat_core::ui::Area
/// [`&Pass`]: duat_core::data::Pass
/// [`&Text`]: duat_core::text::Text
/// [`&Selections`]: duat_core::mode::Selections
/// [`&Selection`]: duat_core::mode::Selection
/// [`&Window`]: duat_core::ui::Window
/// [`impl Display`]: std::fmt::Display
/// [`impl Debug`]: std::fmt::Debug
/// [`Text`]: duat_core::text::Text
/// [`RwData`]: duat_core::data::RwData
/// [`DataMap`]: duat_core::data::DataMap
/// [`FnOnce(&Pass) -> RwData/DataMap`]: FnOnce
/// [`(Fn(&Pass) -> Text/Display/Debug, Fn(&Pass) -> bool)`]: Fn
/// [`RwData<Text>`]: duat_core::data::RwData
/// [`Form`]: duat_core::form::Form
/// [`&Area`]: duat_core::ui::Area
/// [`Area`]: duat_core::ui::Area
/// [`Widget`]: duat_core::ui::Widget
#[macro_export]
#[doc(hidden)]
macro_rules! __status__ {
        ($($parts:tt)*) => {{
        #[allow(unused_imports)]
        use $crate::{
            private_exports::{
                Handle, Pass, PushSpecs, Builder,
                format_like
            },
            widgets::StatusLineFmt,
        };
        #[allow(unused_imports)]
        use $crate::{__parse_form__, __parse_status_part__, __parse_str__};

        let text_fn = |_: &Pass, _: &mut Builder, _: &Handle| {};
        let checker = |_: &Pass| false;

        let (text_fn, checker) = format_like!(
            __parse_str__,
            [('{', __parse_status_part__, false), ('[', __parse_form__, true)],
            (text_fn, checker),
            $($parts)*
        );

        StatusLineFmt::new_with(
            (
                Box::new(move |pa: &Pass, mut builder: Builder, handle: &Handle| {
                    builder.no_space_after_empty = true;
                    text_fn(pa, &mut builder, &handle);
                    builder.build()
                }),
                Box::new(checker)
            ),
        )
    }}
}

#[macro_export]
#[doc(hidden)]
macro_rules! __parse_str__ {
    ($status_line:expr, $str:literal) => {{
        use $crate::{
            private_exports::{Builder, Handle, Pass},
            widgets::State,
        };

        let (mut appender, checker) = $status_line;
        let (mut ap, _) = State::from($str).fns();

        let appender = move |pa: &Pass, builder: &mut Builder, handle: &Handle| {
            appender(pa, builder, handle);
            ap(pa, builder, handle);
        };

        (appender, checker)
    }};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __parse_status_part__ {
    ($status_line:expr,"", $part:expr) => {{
        use $crate::{
            private_exports::{Builder, Handle, Pass},
            widgets::State,
        };

        #[allow(unused_mut)]
        let (mut appender, checker) = $status_line;
        let (ap, ch) = State::from($part).fns();

        let checker = move |pa: &Pass| checker(pa) || ch(pa);

        let appender = move |pa: &Pass, builder: &mut Builder, handle: &Handle| {
            appender(pa, builder, handle);
            ap(pa, builder, handle);
        };

        (appender, checker)
    }};
    ($status_line:expr, $modif:literal, $part:expr) => {{
        use $crate::{
            private_exports::{Builder, Handle, Pass},
            widgets::State,
        };

        let (mut appender, checker) = $status_line;
        let (ap, ch) = State::from(format!(concat!("{:", $modif, "}"), $part)).fns();

        let checker = move |pa: &Pass| checker(pa) || ch(pa);

        let appender = move |pa: &Pass, builder: &mut Builder, handle: &Handle| {
            appender(pa, builder, handle);
            ap(pa, builder, handle);
        };

        (appender, checker)
    }};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __parse_form__ {
    ($status_line:expr, "",) => {{
        use $crate::private_exports::{Handle, Pass, form, Builder};

        let (appender, checker) = $status_line;
        let appender = move |pa: &Pass, builder: &mut Builder, handle: &Handle| {
            appender(pa, builder, handle);
            builder.push(form::DEFAULT_ID);
        };

        (appender, checker)
    }};
    ($status_line:expr, "", $($form:tt)*) => {{
        use $crate::private_exports::{Handle, Pass, form, Builder};

        let (appender, checker) = $status_line;
        let id = form::id_of!(concat!($(stringify!($form)),*));

        let appender = move |pa: &Pass, builder: &mut Builder, handle: &Handle| {
            appender(pa, builder, handle);
            builder.push(id);
        };

        (appender, checker)
    }};
    ($pre_fn_and_checker:expr, $modif:literal, $($form:ident).*) => {{
        compile_error!(concat!(
            "at the moment, Forms in StatusLines don't support modifiers like ",
            $modif
        ))
    }}
}
