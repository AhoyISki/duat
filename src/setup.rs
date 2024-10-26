//! Pre configuration setup of Duat
//!
//! Before loading the user's config crate, Duat needs to do some
//! initial setup. For example, the [`CurFile`] and [`CurWidget`]
//! variables are not set in the start of the program, since they
//! require a [`Ui`], which cannot be defined in static time.
use std::sync::{
    LazyLock, RwLock,
    atomic::{AtomicUsize, Ordering},
    mpsc,
};

use duat_core::{
    context::{CurFile, CurWidget},
    data::RwData,
    session::SessionCfg,
    text::PrintCfg,
    ui::{Event, Ui as TraitUi, Window},
    widgets::{File, ShowNotifications, Widget},
};
use duat_term::VertRule;

use crate::{
    CfgFn, Ui, commands,
    hooks::{self, OnFileOpen, OnWindowOpen, UnfocusedFrom},
    prelude::{CommandLine, LineNumbers, StatusLine},
};

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
    duat_core::context::setup(
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
        builder.push_to(CommandLine::cfg().left_ratioed(4, 7), child);
    });

    hooks::add_grouped::<UnfocusedFrom<CommandLine>>("CmdLineNotifications", |_cmd_line| {
        commands::set_cmd_mode(ShowNotifications::new());
    });
}

#[doc(hidden)]
pub fn run_duat(
    prev: Vec<(RwData<File>, bool)>,
    tx: mpsc::Sender<Event>,
    rx: mpsc::Receiver<Event>,
    statics: <Ui as TraitUi>::StaticFns,
) -> Vec<(RwData<File>, bool)> {
    let ui = Ui::new(statics);

    let mut cfg = SessionCfg::new(ui);

    if let Some(cfg_fn) = CFG_FN.write().unwrap().take() {
        cfg_fn(&mut cfg)
    }

    let print_cfg = match PRINT_CFG.write().unwrap().take() {
        Some(cfg) => cfg,
        None => PrintCfg::default_for_input(),
    };

    cfg.set_print_cfg(print_cfg);

    let session = if prev.is_empty() {
        cfg.session_from_args(tx)
    } else {
        cfg.session_from_prev(prev, tx)
    };
    session.start(rx)
}

type PluginFn = dyn FnOnce(&mut SessionCfg<Ui>) + Send + Sync + 'static;
