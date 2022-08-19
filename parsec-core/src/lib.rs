pub mod action;
pub mod buffer;
pub mod config;
pub mod cursor;
mod file;
pub mod input;
pub mod layout;
pub mod tags;
pub mod ui;

// Useful for testing.
pub static mut FOR_TEST: bool = false;
