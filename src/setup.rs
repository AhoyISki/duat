//! Pre configuration setup of Duat
//!
//! Before loading the user's config crate, Duat needs to do some
//! initial setup. For example, the [`CurBuffer`] and [`CurWidget`]
//! variables are not set in the start of the program, since they
//! require a [`Ui`], which cannot be defined in static time.
use std::{
    any::TypeId,
    path::Path,
    sync::{
        Mutex,
        atomic::Ordering,
        mpsc::{Receiver, Sender},
    },
};

use duat_base::{
    modes::Pager,
    widgets::{FooterWidgets, LogBook, Notifications, WhichKey, WordsCompletionParser, status},
};
use duat_core::{
    clipboard::Clipboard,
    context::{self, Logs},
    form::{Form, Palette},
    hook::KeyTyped,
    session::{DuatEvent, ReloadEvent, ReloadedBuffer, SessionCfg},
    text::History,
    ui::{DynSpawnSpecs, Orientation, Ui, Widget},
};
use duat_filetype::FileType;
use duat_term::VertRule;

use crate::{
    form,
    hook::{self, BufferClosed, BufferReloaded, WindowCreated},
    mode,
    opts::{
        BUFFER_OPTS, FOOTER_ON_TOP, LINENUMBERS_OPTS, LOGBOOK_FN, NOTIFICATIONS_FN,
        ONE_LINE_FOOTER, STATUSLINE_FMT,
    },
    prelude::BufferWritten,
    widgets::Buffer,
};

// Setup statics.
pub static ALREADY_PLUGGED: Mutex<Vec<TypeId>> = Mutex::new(Vec::new());

#[doc(hidden)]
pub fn pre_setup(initials: Option<Initials>, duat_tx: Option<Sender<DuatEvent>>) {
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
        LINENUMBERS_OPTS.lock().unwrap().push_on(pa, handle);
        Ok(())
    })
    .grouped("BufferWidgets");

    hook::add::<WindowCreated>(|pa, handle| {
        use crate::{
            state::*,
            text::{AlignRight, Spacer},
        };

        let one_line_footer = ONE_LINE_FOOTER.load(Ordering::Relaxed);
        let status = match &mut *STATUSLINE_FMT.lock().unwrap() {
            Some(status_fn) => status_fn(pa),
            None if one_line_footer => {
                let mode_txt = mode_txt();
                let duat_param_txt = duat_param_txt();
                status!("{AlignRight}{name_txt} {mode_txt} {sels_txt} {duat_param_txt} {main_txt}")
            }
            None => {
                let mode_txt = mode_txt();
                let duat_param_txt = duat_param_txt();
                status!("{mode_txt} {name_txt}{Spacer}{sels_txt} {duat_param_txt} {main_txt}")
            }
        };

        let mut footer = FooterWidgets::new(status).notifs({
            let mut notifs = Notifications::builder();
            NOTIFICATIONS_FN.lock().unwrap()(&mut notifs);
            notifs
        });

        if FOOTER_ON_TOP.load(Ordering::Relaxed) {
            footer = footer.above();
        }

        if one_line_footer {
            footer = footer.one_line();
        }

        footer.push_on(pa, handle);
        Ok(())
    })
    .grouped("FooterWidgets");

    hook::add::<WindowCreated>(|pa, window| {
        let mut builder = LogBook::builder();
        LOGBOOK_FN.lock().unwrap()(&mut builder);
        builder.push_on(pa, window);
        Ok(())
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

        handle.area().store_cache(pa, &path)
    });

    hook::add::<BufferClosed>(|pa, (handle, cache)| {
        let buffer = handle.write(pa);

        let path = buffer.path();
        cache.delete_for::<History>(&path);
        if !buffer.exists() || buffer.text().has_unsaved_changes() {
            cache.delete(path);
        }
        Ok(())
    });

    hook::add::<BufferClosed>(|pa, (handle, cache)| {
        let buffer = handle.write(pa);

        let path = buffer.path();
        buffer.text_mut().new_moment();

        if let Some("gitcommit") = path.filetype() {
            cache.delete(path);
            return Ok(());
        }

        if let Some(main) = buffer.selections().get_main()
            && let Err(err) = cache.store(&path, main.clone())
        {
            context::error!("{err}");
        }

        handle.area().store_cache(pa, &path)
    })
    .grouped("CacheCursorPosition");

    // Other hooks

    hook::add::<BufferWritten>(|_, (path, _, is_quitting)| {
        let path = Path::new(path);
        if !is_quitting
            && let Ok(crate_dir) = crate::utils::crate_dir()
            && path.starts_with(crate_dir)
        {
            crate::prelude::cmd::queue("reload");
        }
        Ok(())
    })
    .grouped("ReloadOnWrite");

    let cur_seq = mode::current_sequence();
    hook::add::<KeyTyped>(move |pa, key| {
        if matches!(key, mode::ctrl!('?')) || !cur_seq.call(pa).0.is_empty() {
            let specs = DynSpawnSpecs {
                orientation: Orientation::VerRightBelow,
                width: None,
                height: Some(20.0),
                hidden: false,
                inside: true,
            };

            WhichKey::open(pa, specs);
        }
        Ok(())
    });

    hook::add::<Buffer>(|pa, handle| WordsCompletionParser::add_to_buffer(handle.write(pa)));

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
    form::set_weak("default.WhichKey", Form::on_dark_grey());
    form::set_weak("key", "const");
    form::set_weak("key.mod", "attribute");
    form::set_weak("key.angle", "punctuation.bracket");
    form::set_weak("key.special", Form::yellow());
    form::set_weak("remap", Form::italic());

    crate::cmd::add!("logs", |pa| {
        mode::set(Pager::<LogBook>::new());
        Ok(None)
    });

    mode::map::<mode::User>("L", Pager::<LogBook>::new());

    #[cfg(feature = "treesitter")]
    {
        use duat_core::Plugins;

        Plugins::_new().require::<duat_match_pairs::MatchPairs>();
    }

    duat_core::Plugins::_new().require::<duatmode::DuatMode>();
}

#[doc(hidden)]
pub fn run_duat(
    (ui, clipb): MetaStatics,
    buffers: Vec<Vec<ReloadedBuffer>>,
    duat_rx: Receiver<DuatEvent>,
    reload_tx: Option<Sender<ReloadEvent>>,
) -> (Vec<Vec<ReloadedBuffer>>, Receiver<DuatEvent>) {
    std::panic::set_hook(Box::new(move |panic_info| {
        ui.close();
        println!("Duat panicked: {panic_info}");
    }));

    ui.load();

    let opts = SessionCfg::new(clipb, *BUFFER_OPTS.lock().unwrap());
    let already_plugged = std::mem::take(&mut *ALREADY_PLUGGED.lock().unwrap());

    let unwind_safe = std::panic::AssertUnwindSafe((ui, buffers));

    let Ok(ret) = std::panic::catch_unwind(move || {
        let unwind_safe = unwind_safe;
        let std::panic::AssertUnwindSafe((ui, buffers)) = unwind_safe;
        opts.build(ui, buffers, already_plugged)
            .start(duat_rx, reload_tx)
    }) else {
        std::process::exit(-1);
    };

    ret
}

////////// Types used for startup and reloading

/// Channels to send information between the runner and executable
#[doc(hidden)]
pub type Channels = (Sender<DuatEvent>, Receiver<DuatEvent>, Sender<ReloadEvent>);
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
