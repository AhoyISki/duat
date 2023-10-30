use std::{
    collections::HashMap,
    fmt::Display,
    mem::MaybeUninit,
    sync::{Arc, LazyLock, RwLock},
};

use super::{Args, CmdResult, Error, Flags, Result};
use crate::{
    data::{Data, RwData},
    text::{text, Text},
    ui::{Area, Ui, Window},
    widgets::PassiveWidget,
    CURRENT_FILE, CURRENT_WIDGET,
};

