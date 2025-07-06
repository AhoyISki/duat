//! Pre configuration setup of Duat
//!
//! Before loading the user's config crate, Duat needs to do some
//! initial setup. For example, the [`CurFile`] and [`CurWidget`]
//! variables are not set in the start of the program, since they
//! require a [`Ui`], which cannot be defined in static time.
use std::{
    cell::RefCell,
    path::Path,
    sync::{
        LazyLock, Mutex, RwLock,
        atomic::{AtomicUsize, Ordering},
        mpsc::{Receiver, Sender},
    },
    time::Instant,
};

use duat_core::{
    cfg::PrintCfg,
    clipboard::Clipboard,
    context::{self, CurFile, CurWidget, Logs},
    session::{FileRet, SessionCfg},
    ui::{self, DuatEvent, Widget, Window},
};
use duat_term::VertRule;
use duat_utils::{
    modes::{Pager, Regular},
    widgets::{FooterWidgets, LogBook},
};

use crate::{
    CfgFn, Ui, form,
    hook::{self, OnFileOpen, OnWindowOpen},
    mode,
    prelude::{FileWritten, LineNumbers},
};

// Setup statics.
pub static CFG_FN: CfgFn = RwLock::new(None);
pub static PRINT_CFG: RwLock<Option<PrintCfg>> = RwLock::new(None);
pub static PLUGIN_FN: LazyLock<RwLock<Box<PluginFn>>> =
    LazyLock::new(|| RwLock::new(Box::new(|_| {})));

#[doc(hidden)]
pub fn pre_setup(logs: Option<Logs>, duat_tx: &'static Sender<DuatEvent>) {
    if let Some(logs) = logs {
        log::set_logger(Box::leak(Box::new(logs.clone()))).unwrap();
        context::set_logs(logs);
    }

    // State statics.
    let cur_file: &'static CurFile<Ui> = Box::leak(Box::new(CurFile::new()));
    let cur_widget: &'static CurWidget<Ui> = Box::leak(Box::new(CurWidget::new()));
    let windows: &'static RefCell<Vec<Window<Ui>>> = Box::leak(Box::new(RefCell::new(Vec::new())));
    static CUR_WINDOW: AtomicUsize = AtomicUsize::new(0);

    // SAFETY: This function is supposed to be called only from the main
    // thread, this is that thread.
    unsafe {
        duat_core::context::setup_context::<Ui>(
            cur_file,
            cur_widget,
            CUR_WINDOW.load(Ordering::Relaxed),
            windows,
            duat_tx,
        );
    }

    mode::set_default(Regular);
    mode::set_default(Pager::<LogBook, Ui>::new());

    hook::add_grouped::<OnFileOpen>("MatchPairs", |pa, builder| {
        builder.add_reader(pa, duat_match_pairs::MatchParens);
    });

    hook::add_grouped::<OnFileOpen>("FileWidgets", |pa, builder| {
        builder.push(pa, VertRule::cfg());
        builder.push(pa, LineNumbers::cfg());
    });

    hook::add_grouped::<OnWindowOpen>("WindowWidgets", |pa, builder| {
        builder.push(pa, FooterWidgets::default());
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

    form::enable_mask("error");
    form::enable_mask("warn");
    form::enable_mask("info");

    mode::map::<mode::User>("L", Pager::<LogBook, Ui>::new())
}

#[doc(hidden)]
pub fn run_duat(
    (ui_ms, clipb): MetaStatics,
    prev: Vec<Vec<FileRet>>,
    duat_rx: Receiver<DuatEvent>,
) -> (Vec<Vec<FileRet>>, Receiver<DuatEvent>, Option<Instant>) {
    <Ui as ui::Ui>::load(ui_ms);
    let mut cfg = SessionCfg::new(clipb);

    if let Some(cfg_fn) = CFG_FN.write().unwrap().take() {
        cfg_fn(&mut cfg)
    }

    let print_cfg = match PRINT_CFG.write().unwrap().take() {
        Some(cfg) => cfg,
        None => PrintCfg::default_for_input(),
    };

    cfg.set_print_cfg(print_cfg);

    let session = if prev.is_empty() {
        cfg.session_from_args(ui_ms)
    } else {
        cfg.session_from_prev(ui_ms, prev)
    };
    session.start(duat_rx)
}

type PluginFn = dyn FnOnce(&mut SessionCfg<Ui>) + Send + Sync + 'static;
#[doc(hidden)]
pub type Messengers = (&'static Sender<DuatEvent>, Receiver<DuatEvent>);
#[doc(hidden)]
pub type MetaStatics = (
    &'static <Ui as ui::Ui>::MetaStatics,
    &'static Mutex<Clipboard>,
);
