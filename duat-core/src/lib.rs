#![feature(
    extract_if,
    iter_intersperse,
    iter_order_by,
    trait_upcasting,
    let_chains,
    control_flow_enum,
    decl_macro,
    step_trait,
    type_alias_impl_trait
)]
#![doc = include_str!("../README.md")]

use std::{
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        LazyLock, Mutex, Once,
    },
    thread::JoinHandle,
};

use data::{CurrentFile, CurrentWidget};
use ui::Ui;

use self::commands::Commands;

pub mod commands;
pub mod data;
pub mod file;
pub mod history;
pub mod hooks;
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

pub struct Context<U>
where
    U: Ui,
{
    pub current_file: &'static CurrentFile<U>,
    pub current_widget: &'static CurrentWidget<U>,
    pub commands: &'static Commands<U>,
    handles: &'static AtomicUsize,
    has_ended: &'static AtomicBool,
}

impl<U> Clone for Context<U>
where
    U: Ui,
{
    fn clone(&self) -> Self {
        *self
    }
}
impl<U> Copy for Context<U> where U: Ui {}

impl<U> Context<U>
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

    pub fn spawn<R: Send + 'static>(
        &self,
        f: impl FnOnce() -> R + Send + 'static,
    ) -> JoinHandle<R> {
        self.handles.fetch_add(1, Ordering::Relaxed);
        std::thread::spawn(|| {
            let ret = f();
            self.handles.fetch_sub(1, Ordering::Relaxed);
            ret
        })
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

// Debugging objects.
pub static DEBUG_TIME_START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
pub static HOOK: Once = Once::new();
pub static LOG: LazyLock<Mutex<String>> = LazyLock::new(|| Mutex::new(String::new()));

/// Internal macro used to log information.
pub macro log_info($($text:tt)*) {{
    use std::{fmt::Write, time::Instant};

    use crate::{HOOK, LOG};

    let mut text = format!($($text)*);

    HOOK.call_once(|| {
        let old_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            old_hook(info);
            println!("Logs:");
            println!("{}\n", LOG.lock().unwrap());
        }));
    });

    if let Some(start) = $crate::DEBUG_TIME_START.get() {
        if text.lines().count() > 1 {
            let chars = text.char_indices().filter_map(|(pos, char)| (char == '\n').then_some(pos));
            let nl_indices: Vec<usize> = chars.collect();
            for index in nl_indices.iter().rev() {
                text.insert_str(index + 1, "  ");
            }

            let duration = Instant::now().duration_since(*start);
            write!(LOG.lock().unwrap(), "\nat {:.4?}:\n  {text}", duration).unwrap();
        } else {
            let duration = Instant::now().duration_since(*start);
            write!(LOG.lock().unwrap(), "\nat {:.4?}: {text}", duration).unwrap();
        }
    } else {
        write!(LOG.lock().unwrap(), "\n{text}").unwrap();
    }

	let len_lines = LOG.lock().unwrap().lines().count().saturating_sub(35);
    let trimmed = LOG.lock().unwrap().split_inclusive('\n').skip(len_lines).collect();
    *LOG.lock().unwrap() = trimmed;
}}
