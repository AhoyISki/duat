use std::sync::{
    atomic::{AtomicBool, AtomicUsize},
    mpsc, RwLock,
};

use duat_core::{
    commands::Commands,
    data::{CurrentFile, CurrentWidget, RwData},
    session::SessionCfg,
    text::PrintCfg,
    ui::{Event, Ui as TraitUi},
    widgets::File,
    Globals,
};
use duat_term::VertRule;

use crate::{
    config::{hooks, CfgFn},
    prelude::{CommandLine, LineNumbers, StatusLine},
    OnFileOpen, OnUiStart, OnWindowOpen, Ui,
};

// Globals's statics.
pub static CURRENT_FILE: CurrentFile<Ui> = CurrentFile::new();
pub static CURRENT_WIDGET: CurrentWidget<Ui> = CurrentWidget::new();
pub static COMMANDS: Commands<Ui> = Commands::new(&CURRENT_FILE, &CURRENT_WIDGET);
pub static HANDLES: AtomicUsize = AtomicUsize::new(0);
pub static HAS_ENDED: AtomicBool = AtomicBool::new(false);

pub static GLOBALS: Globals<Ui> = Globals::new(
    &CURRENT_FILE,
    &CURRENT_WIDGET,
    &COMMANDS,
    &HANDLES,
    &HAS_ENDED,
);

// Setup statics.
pub static CFG_FN: CfgFn = RwLock::new(None);
pub static PRINT_CFG: RwLock<Option<PrintCfg>> = RwLock::new(None);

#[doc(hidden)]
pub fn pre_hooks() {
    hooks::add_grouped::<OnFileOpen>("FileWidgets", |builder| {
        builder.push::<VertRule>();
        builder.push::<LineNumbers>();
    });

    hooks::add_grouped::<OnWindowOpen>("WindowWidgets", |builder| {
        let (child, _) = builder.push::<StatusLine>();
        builder.push_cfg_to(CommandLine::cfg().left_with_percent(40), child);
    });
}

#[doc(hidden)]
pub fn run_duat(
    prev: Vec<(RwData<File>, bool)>,
    tx: mpsc::Sender<Event>,
    rx: mpsc::Receiver<Event>,
    statics: <Ui as TraitUi>::StaticFns,
) -> Vec<(RwData<File>, bool)> {
    let mut ui = Ui::new(statics);

    duat_core::hooks::activate::<OnUiStart>(&mut ui);

    let mut cfg = SessionCfg::new(ui, GLOBALS);

    if let Some(cfg_fn) = CFG_FN.write().unwrap().take() {
        cfg_fn(&mut cfg)
    }

    cfg.set_print_cfg(PRINT_CFG.write().unwrap().take().unwrap_or_default());

    let session = if prev.is_empty() {
        cfg.session_from_args(tx)
    } else {
        cfg.session_from_prev(prev, tx)
    };
    session.start(rx)
}
