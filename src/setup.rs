use std::sync::{
    atomic::{AtomicBool, AtomicUsize},
    mpsc, LazyLock, RwLock,
};

use duat_core::{
    commands::Commands,
    data::{Context, CurFile, CurWidget, RwData},
    session::SessionCfg,
    text::{PrintCfg, Text},
    ui::{Event, Ui as TraitUi, Window},
    widgets::{File, ShowNotifications},
};
use duat_term::VertRule;

use crate::{
    commands,
    hooks::{self, OnFileOpen, OnUiStart, OnWindowOpen, UnfocusedFrom},
    prelude::{CommandLine, LineNumbers, StatusLine},
    CfgFn, Ui,
};

// Context's statics.
static CUR_FILE: CurFile<Ui> = CurFile::new();
static CUR_WIDGET: CurWidget<Ui> = CurWidget::new();
static CUR_WINDOW: AtomicUsize = AtomicUsize::new(0);
static WINDOWS: LazyLock<RwData<Vec<Window<Ui>>>> = LazyLock::new(RwData::default);
static NOTIFICATIONS: LazyLock<RwData<Text>> = LazyLock::new(RwData::default);
static HAS_ENDED: AtomicBool = AtomicBool::new(false);

pub static COMMANDS: Commands<Ui> = Commands::new(
    &CUR_FILE,
    &CUR_WIDGET,
    &CUR_WINDOW,
    &WINDOWS,
    &NOTIFICATIONS,
);

pub static CONTEXT: Context<Ui> = Context::new(
    &CUR_FILE,
    &CUR_WIDGET,
    &CUR_WINDOW,
    &WINDOWS,
    &COMMANDS,
    &NOTIFICATIONS,
    &HAS_ENDED,
);

// Setup statics.
pub static CFG_FN: CfgFn = RwLock::new(None);
pub static PRINT_CFG: RwLock<Option<PrintCfg>> = RwLock::new(None);
pub static PLUGIN_FN: LazyLock<RwLock<Box<PluginFn>>> =
    LazyLock::new(|| RwLock::new(Box::new(|_| {})));

#[doc(hidden)]
pub fn pre_setup() {
    hooks::add_grouped::<OnFileOpen>("FileWidgets", |builder| {
        builder.push::<VertRule>();
        builder.push::<LineNumbers>();
    });

    hooks::add_grouped::<OnWindowOpen>("WindowWidgets", |builder| {
        let (child, _) = builder.push::<StatusLine>();
        builder.push_cfg_to(CommandLine::cfg().left_ratioed(4, 7), child);
    });

    hooks::add_grouped::<UnfocusedFrom<CommandLine>>("CmdLineNotifications", |_cmd_line| {
        commands::set_mode::<ShowNotifications>();
    });
}

#[doc(hidden)]
pub fn run_duat(
    prev: Vec<(RwData<File>, bool)>,
    tx: mpsc::Sender<Event>,
    rx: mpsc::Receiver<Event>,
    statics: <Ui as TraitUi>::StaticFns,
) -> Vec<(RwData<File>, bool)> {
    let ui = RwData::new(Ui::new(statics));

    duat_core::hooks::trigger::<OnUiStart>(ui.clone());

    let mut cfg = SessionCfg::new(ui, CONTEXT);

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
