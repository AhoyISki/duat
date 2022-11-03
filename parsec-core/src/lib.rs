use std::{marker::PhantomData, thread, time::Duration};

use crossterm::event::{self, Event, KeyCode};
use input::{EditingScheme, FileRemapper};
use layout::Layout;
use ui::Ui;

pub mod action;
pub mod config;
pub mod cursor;
mod file;
pub mod input;
pub mod layout;
pub mod tags;
pub mod ui;

pub struct Application<L, E, U>
where
    L: Layout<U>,
    E: EditingScheme,
    U: Ui,
{
    layout: L,
    key_remapper: FileRemapper<E>,
    // ↓ stupid ↓ //
    _phantom_stuff: (PhantomData<E>, PhantomData<U>)
}

impl<L, E, U> Application<L, E, U>
where
    L: Layout<U>,
    E: EditingScheme,
    U: Ui,
{
    pub fn new(layout: L, key_remapper: FileRemapper<E>) -> Self {
        Application {
            layout,
            key_remapper,
            _phantom_stuff: (PhantomData::default(), PhantomData::default())
        }
    }

	pub fn application_loop(&mut self) {
    	
	}
}

/// Creates a vector of `&str`s from a `String`, making sure to keep at least one empty
/// string at the end, in case of an empty, or `\n` terminated string.
fn split_string_lines(string: &String) -> Vec<String> {
    if string.is_empty() {
        vec![String::from("")]
    } else {
        let mut lines: Vec<String> = string.split_inclusive('\n').map(|s| s.to_string()).collect();
        if string.ends_with('\n') {
            lines.push(String::from(""));
        }
        lines
    }
}

// Useful for testing.
pub static mut FOR_TEST: bool = false;

////////// Ad-hoc functions until they eventually get stabilized.
pub fn saturating_add_signed(lhs: usize, rhs: isize) -> usize {
    if rhs > 0 {
        lhs.saturating_add(rhs as usize)
    } else {
        lhs.saturating_sub(rhs as usize)
    }
}


