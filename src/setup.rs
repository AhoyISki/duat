//! Pre configuration setup of Duat
//!
//! Before loading the user's config crate, Duat needs to do some
//! initial setup. For example, the [`CurFile`] and [`CurWidget`]
//! variables are not set in the start of the program, since they
//! require a [`Ui`], which cannot be defined in static time.
use std::{
    path::Path,
    sync::{
        LazyLock,
        atomic::{AtomicUsize, Ordering},
        mpsc::{Receiver, Sender},
    },
    time::Instant,
};

use duat_core::{
    Mutex, RwLock,
    cfg::PrintCfg,
    clipboard::Clipboard,
    context::{CurFile, CurWidget},
    mode::Regular,
    session::{FileRet, SessionCfg},
    ui::{self, Area, Constraint, DuatEvent, Window},
    widgets::Widget,
};
use duat_term::VertRule;

use crate::{
    CfgFn, Ui,
    hooks::{self, FocusedOn, OnFileOpen, OnWindowOpen, UnfocusedFrom},
    mode,
    prelude::{FileWritten, LineNumbers, Notifier, PromptLine, StatusLine},
};

// Setup statics.
pub static CFG_FN: CfgFn = RwLock::new(None);
pub static PRINT_CFG: RwLock<Option<PrintCfg>> = RwLock::new(None);
pub static PLUGIN_FN: LazyLock<RwLock<Box<PluginFn>>> =
    LazyLock::new(|| RwLock::new(Box::new(|_| {})));

#[doc(hidden)]
pub fn pre_setup() {
    mode::set_default(Regular);

    // State statics.
    let cur_file: &'static CurFile<Ui> = Box::leak(Box::new(CurFile::new()));
    let cur_widget: &'static CurWidget<Ui> = Box::leak(Box::new(CurWidget::new()));
    let windows: &'static RwLock<Vec<Window<Ui>>> = Box::leak(Box::new(RwLock::new(Vec::new())));
    static CUR_WINDOW: AtomicUsize = AtomicUsize::new(0);

    duat_core::context::setup_non_statics::<Ui>(
        cur_file,
        cur_widget,
        CUR_WINDOW.load(Ordering::Relaxed),
        windows,
    );

    hooks::add_grouped::<OnFileOpen>("FileWidgets", |builder| {
        builder.push(VertRule::cfg());
        builder.push(LineNumbers::cfg());
    });

    hooks::add_grouped::<OnWindowOpen>("WindowWidgets", |builder| {
        builder.push(StatusLine::cfg());
        let (child, _) = builder.push(PromptLine::cfg());
        builder.push_to(child, Notifier::cfg());
    });

    hooks::add_grouped::<UnfocusedFrom<PromptLine<Ui>>>("HidePromptLine", |(_, area)| {
        area.constrain_ver([Constraint::Len(0.0)]).unwrap();
    });

    hooks::add_grouped::<FocusedOn<PromptLine<Ui>>>("HidePromptLine", |(_, area)| {
        area.constrain_ver([Constraint::Ratio(1, 1), Constraint::Len(1.0)])
            .unwrap();
    });

    hooks::add_grouped::<FileWritten>("ReloadOnWrite", |(path, _)| {
        let path = Path::new(path);
        if let Some(config_dir) = crate::crate_dir()
            && path.starts_with(config_dir)
        {
            crate::prelude::cmd::run("reload");
        }
    });
}

#[doc(hidden)]
pub fn run_duat(
    (ui_ms, clipb): MetaStatics,
    prev: Vec<Vec<FileRet>>,
    (duat_tx, duat_rx): Messengers,
) -> Option<(Vec<Vec<FileRet>>, Receiver<DuatEvent>, Instant)> {
    <Ui as ui::Ui>::load(ui_ms);
    let mut cfg = SessionCfg::new(clipb);

    if let Some(cfg_fn) = CFG_FN.write().take() {
        cfg_fn(&mut cfg)
    }

    let print_cfg = match PRINT_CFG.write().take() {
        Some(cfg) => cfg,
        None => PrintCfg::default_for_input(),
    };

    cfg.set_print_cfg(print_cfg);

    let session = if prev.is_empty() {
        cfg.session_from_args(ui_ms, duat_tx)
    } else {
        cfg.session_from_prev(ui_ms, prev, duat_tx)
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
