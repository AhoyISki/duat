//! Pre configuration setup of Duat
//!
//! Before loading the user's config crate, Duat needs to do some
//! initial setup. For example, the [`CurBuffer`] and [`CurWidget`]
//! variables are not set in the start of the program, since they
//! require a [`Ui`], which cannot be defined in static time.
use std::{
    any::TypeId,
    path::Path,
    sync::{Mutex, mpsc::Sender},
};

use duat_base::{
    modes::Pager,
    widgets::{FooterWidgets, LogBook, WhichKey, status},
};
use duat_core::{
    buffer::History,
    clipboard::Clipboard,
    context::{self, DuatReceiver, DuatSender, Logs},
    data::Pass,
    form::{Form, Palette},
    hook::{KeyTyped, ModeSwitched},
    opts::PrintOpts,
    session::{ReloadEvent, ReloadedBuffer, SessionCfg},
    text::txt,
    ui::{DynSpawnSpecs, Orientation, Ui, Widget},
};
use duat_filetype::FileType;
use duat_term::VertRule;

use crate::{
    form,
    hook::{self, BufferClosed, BufferReloaded, WindowCreated},
    mode,
    opts::{OPTS, STATUSLINE_FMT},
    prelude::BufferSaved,
    widgets::Buffer,
};

// Setup statics.
static PANIC_INFO: Mutex<Option<String>> = Mutex::new(None);
pub static ALREADY_PLUGGED: Mutex<Vec<TypeId>> = Mutex::new(Vec::new());

#[doc(hidden)]
pub fn pre_setup(ui: Ui, initials: Option<Initials>, duat_tx: Option<DuatSender>) {
    std::panic::set_hook(Box::new(move |panic_info| {
        context::log_panic(panic_info);
        let backtrace = std::backtrace::Backtrace::capture();
        *PANIC_INFO.lock().unwrap() = Some(format!("{panic_info}\n{backtrace}"))
    }));

    if let Some((logs, forms_init, (crate_dir, profile))) = initials {
        log::set_logger(Box::leak(Box::new(logs.clone()))).unwrap();
        context::set_logs(logs);
        duat_core::form::set_initial(forms_init);
        duat_core::utils::set_crate_dir_and_profile(Some(crate_dir), profile);
    }

    if let Some(duat_tx) = duat_tx {
        duat_core::context::set_sender(duat_tx);
    }

    mode::set_default(crate::mode::Regular);
    mode::set_default(Pager::<LogBook>::new());

    // Layout hooks

    hook::add::<Buffer>(|pa, handle| {
        VertRule::builder().push_on(pa, handle);
        OPTS.lock().unwrap().line_numbers.push_on(pa, handle);
    })
    .grouped("BufferWidgets");

    hook::add::<WindowCreated>(|pa, handle| {
        use crate::{state::*, text::Spacer};

        let opts = OPTS.lock().unwrap();

        let status = match &mut *STATUSLINE_FMT.lock().unwrap() {
            Some(status_fn) => status_fn(pa),
            None if opts.one_line_footer => {
                let mode_txt = mode_txt();
                let duat_param_txt = duat_param_txt();
                status!("{Spacer}{name_txt} {mode_txt} {sels_txt} {duat_param_txt} {main_txt}")
            }
            None => {
                let mode_txt = mode_txt();
                let duat_param_txt = duat_param_txt();
                status!("{mode_txt} {name_txt}{Spacer}{sels_txt} {duat_param_txt} {main_txt}")
            }
        };

        let mut footer = FooterWidgets::new(status).notifs(opts.notifications.clone());

        if opts.footer_on_top {
            footer = footer.above();
        }

        if opts.one_line_footer {
            footer = footer.one_line();
        }

        drop(opts);
        footer.push_on(pa, handle);
    })
    .grouped("FooterWidgets");

    hook::add::<WindowCreated>(|pa, window| {
        let opts = OPTS.lock().unwrap();

        let log_book = opts.logs;
        drop(opts);
        log_book.push_on(pa, window);
    })
    .grouped("LogBook");

    // Cache hooks

    hook::add::<BufferReloaded>(|pa, (handle, cache)| {
        let buffer = handle.write(pa);

        let path = buffer.path();
        buffer.text_mut().new_moment();

        if let Some(main) = buffer.selections().get_main()
            && let Err(err) = cache.store(&path, main.clone())
        {
            context::error!("{err}");
        }

        duat_core::try_or_log_err! {
            handle.area().store_cache(pa, &path)?;
        }
    });

    hook::add::<BufferClosed>(|pa, (handle, cache)| {
        let buffer = handle.write(pa);

        let path = buffer.path();
        cache.delete_for::<History>(&path);
        if !buffer.exists() || buffer.text().has_unsaved_changes() {
            cache.delete(path);
        }
    });

    hook::add::<BufferClosed>(|pa, (handle, cache)| {
        let buffer = handle.write(pa);

        let path = buffer.path();
        buffer.text_mut().new_moment();

        if let Some("gitcommit") = path.filetype() {
            cache.delete(path);
            return;
        }

        if let Some(main) = buffer.selections().get_main()
            && let Err(err) = cache.store(&path, main.clone())
        {
            context::error!("{err}");
        }

        duat_core::try_or_log_err! {
            handle.area().store_cache(pa, &path)?;
        }
    })
    .grouped("CacheCursorPosition");

    // WhichKey hooks
    let wk_specs = DynSpawnSpecs {
        orientation: Orientation::VerRightBelow,
        width: None,
        height: Some(20.0),
        hidden: false,
        inside: true,
    };

    let show_which_key = move |pa: &mut Pass| {
        let mut wk_specs = wk_specs;
        let opts = OPTS.lock().unwrap();
        wk_specs.orientation = opts.whichkey.orientation;
        WhichKey::open(
            pa,
            opts.whichkey.fmt_getter.as_ref().map(|fg| fg()),
            wk_specs,
        );
    };

    let cur_seq = mode::current_sequence();
    hook::add::<KeyTyped>(move |pa, _| {
        let opts = OPTS.lock().unwrap();
        let current_ty = mode::current_type_id();
        let cur_seq = cur_seq.call(pa).0;
        if !cur_seq.is_empty() || opts.whichkey.always_shown_modes.contains(&current_ty) {
            drop(opts);
            show_which_key(pa);
        }
    })
    .grouped("WhichKey");
    hook::add::<ModeSwitched>(move |pa, _| {
        let opts = OPTS.lock().unwrap();
        if opts
            .whichkey
            .always_shown_modes
            .contains(&mode::current_type_id())
        {
            drop(opts);
            show_which_key(pa);
        } else {
            for handle in context::windows().handles_of::<WhichKey>(pa) {
                let _ = handle.close(pa);
            }
        }
    })
    .grouped("WhichKey");

    let cur_seq = mode::current_sequence();
    hook::add::<KeyTyped>(move |pa, key_event| {
        if cur_seq.call(pa).0.is_empty() && OPTS.lock().unwrap().help_key == Some(key_event) {
            show_which_key(pa);
        }
    });

    // Other hooks

    hook::add::<BufferSaved>(|pa, (handle, _, is_quitting)| {
        let path = handle.read(pa).path();
        let path = Path::new(&path);
        if !is_quitting
            && let Ok(crate_dir) = crate::utils::crate_dir()
            && path.starts_with(crate_dir)
        {
            crate::prelude::cmd::queue("reload");
        }
    })
    .grouped("ReloadOnWrite");
    duat_base::widgets::setup_completions();

    form::enable_mask("error");
    form::enable_mask("warn");
    form::enable_mask("info");
    form::enable_mask("inactive");

    // Setup for the LineNumbers
    form::set_weak("linenum.main", Form::yellow());
    form::set_weak("linenum.wrapped", Form::cyan().italic());

    // Setup for the StatusLine
    form::set_weak("buffer", Form::yellow().italic());
    form::set_weak("selections", Form::dark_blue());
    form::set_weak("coord", "contant");
    form::set_weak("separator", "punctuation.delimiter");
    form::set_weak("mode", Form::green());
    form::set_weak("default.StatusLine", Form::on_dark_grey());

    // Setup for the LogBook
    form::set_weak("default.LogBook", Form::on_dark_grey());
    form::set_weak("log_book.error", "default.error");
    form::set_weak("log_book.warn", "default.warn");
    form::set_weak("log_book.info", "default.info");
    form::set_weak("log_book.debug", "default.debug");
    form::set_weak("log_book.colon", "prompt.colon");
    form::set_weak("log_book.bracket", "punctuation.bracket");
    form::set_weak("log_book.target", "module");

    // Setup for the PromptLine
    form::set_weak("prompt.preview", "comment");

    // Setup for Completions
    form::set_weak("default.Completions", Form::on_dark_grey());
    form::set_weak("selected.Completions", Form::black().on_grey());

    // Setup for WhichKey
    form::set_weak("key", "const");
    form::set_weak("key.mod", "punctuation.bracket");
    form::set_weak("key.angle", "punctuation.bracket");
    form::set_weak("key.special", Form::yellow());
    form::set_weak("remap", Form::italic());

    crate::cmd::add("logs", |pa: &mut Pass| {
        mode::set(pa, Pager::<LogBook>::new());
        Ok(None)
    })
    .doc(
        txt!("Open the [a]Logs[] and enter [mode]Pager[] mode"),
        None,
    );

    mode::map::<mode::User>("L", Pager::<LogBook>::new()).doc(txt!("Open [mode]Logs"));

    #[cfg(feature = "treesitter")]
    {
        use duat_core::Plugins;

        Plugins::_new().require::<duat_match_pairs::MatchPairs>();
    }

    #[cfg(feature = "term-ui")]
    duat_core::ui::config_address_space_ui_setup::<duat_term::Ui>(ui);

    duat_core::Plugins::_new().require::<duatmode::DuatMode>();
}

#[doc(hidden)]
pub fn run_duat(
    (ui, clipb): MetaStatics,
    buffers: Vec<Vec<ReloadedBuffer>>,
    duat_rx: DuatReceiver,
    reload_tx: Option<Sender<ReloadEvent>>,
) -> (Vec<Vec<ReloadedBuffer>>, DuatReceiver) {
    ui.load();

    let default_buffer_opts = {
        let opts = OPTS.lock().unwrap();
        PrintOpts {
            wrap_lines: opts.wrap_lines,
            wrap_on_word: opts.wrap_on_word,
            wrapping_cap: opts.wrapping_cap,
            indent_wraps: opts.indent_wraps,
            tabstop: opts.tabstop,
            print_new_line: opts.print_new_line,
            scrolloff: opts.scrolloff,
            force_scrolloff: opts.force_scrolloff,
            extra_word_chars: opts.extra_word_chars,
            show_ghosts: opts.show_ghosts,
            allow_overscroll: opts.allow_overscroll,
        }
    };

    let opts = SessionCfg::new(clipb, default_buffer_opts);
    let already_plugged = std::mem::take(&mut *ALREADY_PLUGGED.lock().unwrap());

    match opts
        .build(ui, buffers, already_plugged)
        .start(duat_rx, reload_tx)
    {
        Some(ret) => ret,
        None => {
            ui.close();
            println!("{}", PANIC_INFO.lock().unwrap().as_ref().unwrap());
            std::process::exit(-1);
        }
    }
}

////////// Types used for startup and reloading

/// Channels to send information between the runner and executable
#[doc(hidden)]
pub type Channels = (DuatSender, DuatReceiver, Sender<ReloadEvent>);
/// Items that will live for the duration of Duat
#[doc(hidden)]
pub type MetaStatics = (Ui, &'static Mutex<Clipboard>);
/// Initial setup items
#[doc(hidden)]
pub type Initials = (
    Logs,
    (&'static Mutex<Vec<&'static str>>, &'static Palette),
    (&'static Path, &'static str),
);
