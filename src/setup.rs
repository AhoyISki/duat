//! Pre configuration setup of Duat
//!
//! Before loading the user's config crate, Duat needs to do some
//! initial setup. For example, the [`CurFile`] and [`CurWidget`]
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
    cfg::PrintCfg,
    clipboard::Clipboard,
    context::{self, CurFile, CurWidget, Logs},
    form::Palette,
    session::{DuatEvent, ReloadEvent, ReloadedFile, SessionCfg},
    text::History,
    ui::{self, Area, Widget},
};
use duat_filetype::FileType;
use duat_term::VertRule;
use duat_utils::{
    modes::Pager,
    widgets::{FooterWidgets, LogBook},
};

use crate::{
    CfgFn, Ui, form,
    hook::{self, FileClosed, FileReloaded, WindowCreated},
    mode,
    prelude::{FileWritten, LineNumbers},
    widgets::File,
};

// Setup statics.
pub static PRINT_CFG: RwLock<Option<PrintCfg>> = RwLock::new(None);
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

    // State statics.
    let cur_file: &'static CurFile<Ui> = Box::leak(Box::new(CurFile::new()));
    let cur_widget: &'static CurWidget<Ui> = Box::leak(Box::new(CurWidget::new()));
    static CUR_WINDOW: AtomicUsize = AtomicUsize::new(0);

    duat_core::context::setup_context::<Ui>(
        cur_file,
        cur_widget,
        CUR_WINDOW.load(Ordering::Relaxed),
    );

    mode::set_default(crate::regular::Regular);
    mode::set_default(Pager::<LogBook, Ui>::new());

    hook::add_grouped::<File>("FileWidgets", |pa, file| {
        VertRule::builder().push_on(pa, file);
        LineNumbers::builder().push_on(pa, file);
        cfg
    });

    hook::add_grouped::<WindowCreated>("FooterWidgets", |pa, builder| {
        FooterWidgets::default().push_on(pa, builder);
    });

    hook::add_grouped::<WindowCreated>("LogBook", |pa, builder| {
        LogBook::builder().push_on(pa, builder);
    });

    hook::add_grouped::<FileWritten>("ReloadOnWrite", |_, (path, _, is_quitting)| {
        let path = Path::new(path);
        if !is_quitting
            && let Ok(crate_dir) = crate::crate_dir()
            && path.starts_with(crate_dir)
        {
            crate::prelude::cmd::queue("reload");
        }
    });

    hook::add::<FileReloaded>(|pa, (handle, cache)| {
        let (file, area) = handle.write_with_area(pa);

        let path = file.path();
        file.text_mut().new_moment();

        if let Some(area_cache) = area.cache()
            && let Err(err) = cache.store(&path, area_cache)
        {
            context::error!("{err}");
        }

        if let Some(main) = file.selections_mut().get_main()
            && let Err(err) = cache.store(path, main.clone())
        {
            context::error!("{err}");
        }
    });

    hook::add::<FileClosed>(|pa, (handle, cache)| {
        let file = handle.write(pa);

        let path = file.path();
        cache.delete_for::<History>(&path);
        if !file.exists() || file.text().has_unsaved_changes() {
            cache.delete(path);
        }
    });

    hook::add_grouped::<FileClosed>("CacheCursorPosition", |pa, (handle, cache)| {
        let (file, area) = handle.write_with_area(pa);

        let path = file.path();
        file.text_mut().new_moment();

        if let Some("gitcommit") = path.filetype() {
            cache.delete(path);
            return;
        }

        if let Some(area_cache) = area.cache()
            && let Err(err) = cache.store(&path, area_cache)
        {
            context::error!(target: "FileClosed", "{err}");
        }

        if let Some(main) = file.selections_mut().get_main()
            && let Err(err) = cache.store(path, main.clone())
        {
            context::error!(target: "FileClosed", "{err}");
        }
    });

    form::enable_mask("error");
    form::enable_mask("warn");
    form::enable_mask("info");
    form::enable_mask("inactive");

    mode::map::<mode::User>("L", Pager::<LogBook, Ui>::new());

    #[cfg(feature = "treesitter")]
    {
        use duat_core::Plugins;

        Plugins::<Ui>::_new().require::<duat_match_pairs::MatchPairs>();
    }
}

#[doc(hidden)]
pub fn run_duat(
    (ui_ms, clipb): MetaStatics,
    files: Vec<Vec<ReloadedFile>>,
    duat_rx: Receiver<DuatEvent>,
    reload_tx: Option<Sender<ReloadEvent>>,
) -> (Vec<Vec<ReloadedFile>>, Receiver<DuatEvent>) {
    <Ui as ui::Ui>::load(ui_ms);

    let mut cfg = SessionCfg::new(clipb, match PRINT_CFG.write().unwrap().take() {
        Some(cfg) => cfg,
        None => PrintCfg::default_for_input(),
    });

    let already_plugged = std::mem::take(&mut *ALREADY_PLUGGED.lock().unwrap());
    cfg.build(ui_ms, files, already_plugged)
        .start(duat_rx, &SPAWN_COUNT, reload_tx)
}

type PluginFn = dyn FnOnce(&mut SessionCfg<Ui>) + Send + Sync + 'static;

////////// Types used for startup and reloading

/// Channels to send information between the runner and executable
#[doc(hidden)]
pub type Channels = (Sender<DuatEvent>, Receiver<DuatEvent>, Sender<ReloadEvent>);
/// Items that will live for the duration of Duat
#[doc(hidden)]
pub type MetaStatics = (
    &'static <Ui as ui::Ui>::MetaStatics,
    &'static Mutex<Clipboard>,
);
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
