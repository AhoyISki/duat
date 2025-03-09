//! Pre configuration setup of Duat
//!
//! Before loading the user's config crate, Duat needs to do some
//! initial setup. For example, the [`CurFile`] and [`CurWidget`]
//! variables are not set in the start of the program, since they
//! require a [`Ui`], which cannot be defined in static time.
use std::sync::{
    LazyLock, RwLock,
    atomic::{AtomicUsize, Ordering},
    mpsc::{Receiver, Sender},
};

use duat_core::{
    Mutex,
    cfg::PrintCfg,
    clipboard::Clipboard,
    context::{CurFile, CurWidget},
    data::RwData,
    mode::Regular,
    session::{FileRet, SessionCfg},
    ui::{self, Area, Constraint, DuatEvent, UiEvent, Window},
    widgets::Widget,
};
use duat_term::VertRule;

use crate::{
    CfgFn, Ui,
    hooks::{self, FocusedOn, OnFileOpen, OnWindowOpen, UnfocusedFrom},
    mode,
    prelude::{CmdLine, LineNumbers, StatusLine},
};

// State statics.
static CUR_FILE: CurFile<Ui> = CurFile::new();
static CUR_WIDGET: CurWidget<Ui> = CurWidget::new();
static CUR_WINDOW: AtomicUsize = AtomicUsize::new(0);
static WINDOWS: LazyLock<RwData<Vec<Window<Ui>>>> = LazyLock::new(RwData::default);
// Setup statics.
pub static CFG_FN: CfgFn = RwLock::new(None);
pub static PRINT_CFG: RwLock<Option<PrintCfg>> = RwLock::new(None);
pub static PLUGIN_FN: LazyLock<RwLock<Box<PluginFn>>> =
    LazyLock::new(|| RwLock::new(Box::new(|_| {})));

#[doc(hidden)]
pub fn pre_setup() {
    mode::set_default(Regular);

    duat_core::context::setup_non_statics(
        &CUR_FILE,
        &CUR_WIDGET,
        CUR_WINDOW.load(Ordering::Relaxed),
        &WINDOWS,
    );

    hooks::add_grouped::<OnFileOpen>("FileWidgets", |builder| {
        builder.push(VertRule::cfg());
        builder.push(LineNumbers::cfg());
    });

    hooks::add_grouped::<OnWindowOpen>("WindowWidgets", |builder| {
        let (child, _) = builder.push(StatusLine::cfg());
        builder.push_to(CmdLine::cfg().left_ratioed(4, 7), child);
    });

    hooks::add_grouped::<UnfocusedFrom<CmdLine>>("HideCmdLine", |(_, area)| {
        area.constrain_ver(Constraint::Length(0.0)).unwrap();
    });
    hooks::add_grouped::<FocusedOn<CmdLine>>("HideCmdLine", |(_, area)| {
        // Constraint::Min(0.0) == "Constraint::Free"
        area.constrain_ver(Constraint::Min(0.0)).unwrap();
    });
}

#[doc(hidden)]
pub fn run_duat(
    (ui_ms, clipb): MetaStatics,
    prev: Vec<Vec<FileRet>>,
    (duat_tx, duat_rx, ui_tx): Messengers,
) -> (Vec<Vec<FileRet>>, Receiver<DuatEvent>) {
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
        cfg.session_from_args(ui_ms, duat_tx, ui_tx)
    } else {
        cfg.session_from_prev(ui_ms, prev, duat_tx, ui_tx)
    };
    session.start(duat_rx)
}

type PluginFn = dyn FnOnce(&mut SessionCfg<Ui>) + Send + Sync + 'static;
#[doc(hidden)]
pub type Messengers = (
    &'static Sender<DuatEvent>,
    Receiver<DuatEvent>,
    Sender<UiEvent>,
);
#[doc(hidden)]
pub type MetaStatics = (
    &'static <Ui as ui::Ui>::MetaStatics,
    &'static Mutex<Clipboard>,
);
