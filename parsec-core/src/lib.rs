pub mod action;
pub mod buffer;
pub mod config;
pub mod cursor;
mod file;
pub mod input;
pub mod output;
pub mod tags;

// Useful for testing.
pub static mut FOR_TEST: bool = false;
