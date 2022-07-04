pub mod action;
pub mod buffer;
pub mod cursor;
pub mod config;
mod file;
pub mod input;
pub mod output;

use unicode_width::UnicodeWidthStr;
use unicode_segmentation::UnicodeSegmentation;

use output::{TextChar, MainChar};

fn convert_to_text_chars(line: String) -> Vec<TextChar> {
    let mut text_chars = Vec::with_capacity(line.bytes().len());
    for grapheme in line.graphemes(true) {
        let width = UnicodeWidthStr::width(grapheme) as u8;

		let mut chars = grapheme.chars();
		text_chars.push(TextChar::Primary(MainChar::new(chars.next().unwrap(), width)));

		while let Some(ch) = chars.next() {
			text_chars.push(TextChar::Secondary(ch));
		}
    }

	text_chars
}
