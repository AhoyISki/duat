use std::sync::{atomic::AtomicBool, mpsc, LazyLock, RwLock};

use duat_core::{
    commands::Commands,
    data::{CommandLineModes, Context, CurFile, CurWidget, RwData},
    session::SessionCfg,
    text::{PrintCfg, Text},
    ui::{Event, Ui as TraitUi, Window},
    widgets::{File, RunCommands, ShowNotifications},
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
static WINDOWS: LazyLock<RwLock<Vec<Window<Ui>>>>;
static NOTIFICATIONS: LazyLock<RwData<Text>> = LazyLock::new(RwData::default);
static HAS_ENDED: AtomicBool = AtomicBool::new(false);
static CMD_MODES: CommandLineModes<Ui> = CommandLineModes::new();
pub static COMMANDS: Commands<Ui> = Commands::new(&WINDOWS, &CUR_FILE, &CUR_WIDGET, &NOTIFICATIONS);

pub static CONTEXT: Context<Ui> = Context::new(
    &COMMANDS,
    &NOTIFICATIONS,
    &CUR_FILE,
    &CUR_WIDGET,
    &WINDOWS,
    &HAS_ENDED,
    &CMD_MODES,
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
        builder.push_cfg_to(CommandLine::cfg().left_ratioed(2, 5), child);
    });

    hooks::add_grouped::<UnfocusedFrom<CommandLine>>("CmdLineNotifications", |_cmd_line| {
        commands::run("set-cmd-mode ShowNotifications").unwrap();
    });

    CONTEXT.add_cmd_mode(RunCommands::new(CONTEXT));
}

#[doc(hidden)]
pub fn run_duat(
    prev: Vec<(RwData<File>, bool)>,
    tx: mpsc::Sender<Event>,
    rx: mpsc::Receiver<Event>,
    statics: <Ui as TraitUi>::StaticFns,
) -> Vec<(RwData<File>, bool)> {
    let mut ui = Ui::new(statics);

    if hooks::group_exists("CmdLineNotifications") {
        CONTEXT.add_cmd_mode(ShowNotifications::new(CONTEXT));
    }

    duat_core::hooks::trigger::<OnUiStart>(&mut ui);

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
