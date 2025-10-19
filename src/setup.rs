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
        LazyLock, Mutex, RwLock,
        atomic::{AtomicUsize, Ordering},
        mpsc::{Receiver, Sender},
    },
};

use duat_core::{
    clipboard::Clipboard,
    context::{self, Logs},
    form::{Form, Palette},
    session::{DuatEvent, ReloadEvent, ReloadedBuffer, SessionCfg},
    text::History,
    ui::{Ui, Widget},
};
use duat_filetype::FileType;
use duat_term::VertRule;
use duat_utils::{
    modes::Pager,
    widgets::{FooterWidgets, LogBook},
};

use crate::{
    form,
    hook::{self, BufferClosed, BufferReloaded, WindowCreated},
    mode,
    opts::{BUFFER_OPTS, FOOTER_ON_TOP, LINENUMBERS_OPTS, ONE_LINE_FOOTER, STATUSLINE_FMT},
    prelude::BufferWritten,
    widgets::Buffer,
};

// Setup statics.
pub static PLUGIN_FN: LazyLock<RwLock<Box<PluginFn>>> =
    LazyLock::new(|| RwLock::new(Box::new(|_| {})));
pub static ALREADY_PLUGGED: Mutex<Vec<TypeId>> = Mutex::new(Vec::new());

#[doc(hidden)]
pub fn pre_setup(initials: Option<Initials>, duat_tx: Option<Sender<DuatEvent>>) {
    start_counting_spawned_threads();

    if let Some((logs, forms_init, (crate_dir, profile))) = initials {
        log::set_logger(Box::leak(Box::new(logs.clone()))).unwrap();
        context::set_logs(logs);
        duat_core::form::set_initial(forms_init);
        duat_core::utils::set_crate_dir_and_profile(Some(crate_dir), profile);
    }

    if let Some(duat_tx) = duat_tx {
        duat_core::context::set_sender(duat_tx);
    }

    mode::set_default(crate::regular::Regular);
    mode::set_default(Pager::<LogBook>::new());

    hook::add_grouped::<Buffer>("BufferWidgets", |pa, handle| {
        VertRule::builder().push_on(pa, handle);
        LINENUMBERS_OPTS.lock().unwrap().push_on(pa, handle);
        Ok(())
    });

    hook::add_grouped::<WindowCreated>("FooterWidgets", |pa, builder| {
        let status = STATUSLINE_FMT.lock().unwrap()(pa);
        let mut footer = FooterWidgets::new(status);
        if FOOTER_ON_TOP.load(Ordering::Relaxed) {
            footer = footer.above();
        }

        if ONE_LINE_FOOTER.load(Ordering::Relaxed) {
            footer = footer.one_line();
        }

        footer.push_on(pa, builder);
        Ok(())
    });

    hook::add_grouped::<WindowCreated>("LogBook", |pa, builder| {
        LogBook::builder().push_on(pa, builder);
        Ok(())
    });

    hook::add_grouped::<BufferWritten>("ReloadOnWrite", |_, (path, _, is_quitting)| {
        let path = Path::new(path);
        if !is_quitting
            && let Ok(crate_dir) = crate::utils::crate_dir()
            && path.starts_with(crate_dir)
        {
            crate::prelude::cmd::queue("reload");
        }
        Ok(())
    });

    hook::add::<BufferReloaded>(|pa, (handle, cache)| {
        let buffer = handle.write(pa);

        let path = buffer.path();
        buffer.text_mut().new_moment();

        if let Some(main) = buffer.selections().get_main()
            && let Err(err) = cache.store(&path, main.clone())
        {
            context::error!(target: "BufferReloaded", "{err}");
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

    hook::add_grouped::<BufferClosed>("CacheCursorPosition", |pa, (handle, cache)| {
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
            context::error!(target: "BufferClosed", "{err}");
        }

        handle.area().store_cache(pa, &path)
    });

    form::enable_mask("error");
    form::enable_mask("warn");
    form::enable_mask("info");
    form::enable_mask("inactive");

    // Setup for the LineNumbers
    duat_core::form::set_weak("linenum.main", Form::yellow());
    duat_core::form::set_weak("linenum.wrapped", Form::cyan().italic());
    duat_core::form::set_weak("linenum.wrapped.main", "linenum.wrapped");

    // Setup for the StatusLine
    duat_core::form::set_weak("buffer", Form::yellow().italic());
    duat_core::form::set_weak("selections", Form::dark_blue());
    duat_core::form::set_weak("coord", Form::dark_yellow());
    duat_core::form::set_weak("separator", Form::cyan());
    duat_core::form::set_weak("mode", Form::green());
    duat_core::form::set_weak("default.StatusLine", Form::on_dark_grey());

    // Setup for the LogBook
    duat_core::form::set_weak("default.LogBook", Form::on_dark_grey());
    duat_core::form::set_weak("log_book.error", "default.error");
    duat_core::form::set_weak("log_book.warn", "default.warn");
    duat_core::form::set_weak("log_book.info", "default.info");
    duat_core::form::set_weak("log_book.debug", "default.debug");
    duat_core::form::set_weak("log_book.colon", "prompt.colon");
    duat_core::form::set_weak("log_book.bracket", "punctuation.bracket");
    duat_core::form::set_weak("log_book.target", "module");

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
        std::process::exit(-1);
    }));

    ui.load();

    let opts = SessionCfg::new(clipb, *BUFFER_OPTS.lock().unwrap());
    let already_plugged = std::mem::take(&mut *ALREADY_PLUGGED.lock().unwrap());

    std::panic::abort_unwind(|| {
        opts.build(ui, buffers, already_plugged)
            .start(duat_rx, &SPAWN_COUNT, reload_tx)
    })
}

type PluginFn = dyn FnOnce(&mut SessionCfg) + Send + Sync + 'static;

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

/// Starts counting how many threads are running
fn start_counting_spawned_threads() {
    thread_local! {
        static SPAWN_COUNTER: SpawnCounter = SpawnCounter::new();
    }

    std::thread::add_spawn_hook(|_| || SPAWN_COUNTER.with(|_| {}));

    struct SpawnCounter;

    impl SpawnCounter {
        fn new() -> Self {
            SPAWN_COUNT.fetch_add(1, Ordering::Relaxed);
            Self
        }
    }

    impl Drop for SpawnCounter {
        fn drop(&mut self) {
            SPAWN_COUNT.fetch_sub(1, Ordering::Relaxed);
        }
    }
}

static SPAWN_COUNT: AtomicUsize = AtomicUsize::new(0);
