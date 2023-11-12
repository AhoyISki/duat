#![allow(incomplete_features, clippy::type_complexity)]
#![feature(
    lazy_cell,
    extract_if,
    iter_intersperse,
    iter_order_by,
    result_option_inspect,
    trait_upcasting,
    let_chains,
    control_flow_enum,
    decl_macro,
    generic_const_exprs,
    step_trait,
    type_alias_impl_trait
)]
#![doc = include_str!("../README.md")]

use std::{
    ops::{Deref, DerefMut},
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
    thread::JoinHandle,
};

use data::{CurrentFile, CurrentWidget};
use ui::Ui;

use self::commands::Commands;

pub mod commands;
pub mod data;
pub mod file;
pub mod history;
pub mod input;
pub mod palette;
pub mod position;
pub mod session;
pub mod text;
pub mod ui;
pub mod widgets;

pub mod prelude {
    pub use crate::{
        commands,
        palette::{self, CursorShape, Form},
        session::{Session, SessionCfg},
        text::{text, PrintCfg},
        widgets::File,
    };
}

pub struct Globals<U>
where
    U: Ui,
{
    pub current_file: &'static CurrentFile<U>,
    pub current_widget: &'static CurrentWidget<U>,
    pub commands: &'static Commands<U>,
    handles: &'static AtomicUsize,
    has_ended: &'static AtomicBool,
}

impl<U> Clone for Globals<U>
where
    U: Ui,
{
    fn clone(&self) -> Self {
        *self
    }
}
impl<U> Copy for Globals<U> where U: Ui {}

impl<U> Globals<U>
where
    U: Ui,
{
    pub const fn new(
        current_file: &'static CurrentFile<U>,
        current_widget: &'static CurrentWidget<U>,
        commands: &'static Commands<U>,
        handles: &'static AtomicUsize,
        has_ended: &'static AtomicBool,
    ) -> Self {
        Self {
            current_file,
            current_widget,
            commands,
            handles,
            has_ended,
        }
    }

    pub fn spawn<R: Send + 'static>(&self, f: impl FnOnce() -> R + Send + 'static) -> Handle<R> {
        self.handles.fetch_add(1, Ordering::Relaxed);
        Handle {
            handle: std::thread::spawn(f),
            handles: self.handles,
        }
    }

    pub fn has_ended(&self) -> bool {
        self.has_ended.load(Ordering::Relaxed)
    }

    fn threads_are_running(&self) -> bool {
        self.handles.load(Ordering::Relaxed) > 0
    }

    fn end(&self) {
        self.has_ended.store(true, Ordering::Relaxed);
    }
}

pub struct Handle<T> {
    handle: JoinHandle<T>,
    handles: &'static AtomicUsize,
}

impl<T> Deref for Handle<T> {
    type Target = JoinHandle<T>;

    fn deref(&self) -> &Self::Target {
        &self.handle
    }
}

impl<T> DerefMut for Handle<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.handle
    }
}

impl<T> Drop for Handle<T> {
    fn drop(&mut self) {
        self.handles.fetch_sub(1, Ordering::Relaxed);
    }
}

// Debugging objects.
pub static DEBUG_TIME_START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();

/// Internal macro used to log information.
pub macro log_info($($text:tt)*) {{
    use std::{fs, io::Write, time::Instant};
    let mut log = fs::OpenOptions::new().append(true).open("log").unwrap();
    let mut text = format!($($text)*);

    if let Some(start) = $crate::DEBUG_TIME_START.get() {
        if text.lines().count() > 1 {
            let chars = text.char_indices().filter_map(|(pos, char)| (char == '\n').then_some(pos));
            let nl_indices: Vec<usize> = chars.collect();
            for index in nl_indices.iter().rev() {
                text.insert_str(index + 1, "  ");
            }

            let duration = Instant::now().duration_since(*start);
            write!(log, "\nat {:.4?}:\n  {text}", duration).unwrap();
        } else {
            let duration = Instant::now().duration_since(*start);
            write!(log, "\nat {:.4?}: {text}", duration).unwrap();
        }
    } else {
        write!(log, "\n{text}").unwrap();
    }
}}
