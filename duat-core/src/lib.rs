//! The core of Duat, this crate is meant to be used only for the
//! creation of plugins for Duat.
//!
//! The capabilities of `duat-core` are largely the same as the those
//! of Duat, however, the main difference is the multi [`Ui`] APIs of
//! this crate. In it, the public functions and types are defined in
//! terms of `U: Ui`,  which means that they can work on various
//! different interfaces:
//!
//! # Quick Start
//!
//! This crate is composed of a few main modules, which will be used
//! in order to extend Duat:
//!
//! - [`ui`]: Has everything to do with the interface of Duat, that
//!   includes things like:
//!   - [`Widget`]s: As the name implies, this is the trait for
//!     objects that will show up on the screen. The most noteworthy
//!     [`Widget`] is [`File`], which displays the contents of a file.
//!   - [`WidgetCfg`]s: These are [`Widget`] builders. They are used
//!     in the `setup` function of Duat's config, through the
//!     [`WidgetCreated`] and [`WindowCreated`] [hook]s.
//!   - [`Ui`] and [`Area`]s: These are used if you want to create
//!     your own interface for Duat. Very much a work in progress, so
//!     I wouldn't recommend trying that yet.
//!
//! - [`text`]: Defines the struct used to show characters on screen
//!   - [`Text`]: Is everything that Duat shows on screen (except
//!     [`Ui`] specific decorations). This includes a UTF-8 string and
//!     tags to modify it.
//!   - [`Tag`]s: This is how Duat determines how [`Text`] will be
//!     displayed on screen. There are tags for styling, text
//!     alignment, spacing and all sorts of other things.
//!   - [`txt!`]: This macro, with syntax reminiscent of [`format!`]
//!     from Rust's [`std`],  can be used to create [`Text`] through
//!     the [`text::Builder`] struct.
//!
//! - [`mode`]: Defines how Duat will take input in order to control
//!   [`Widget`]s, includes things like:
//!   - [`Mode`](mode::Mode)s: have the function [`send_key`], which
//!     takes a [key] and the current [widget] as input, and decides
//!     what to do with them. Only one [`Mode`](mode::Mode) is active
//!     at any given time.
//!   - [`map`] and [`alias`]: These functions provide vim-style
//!     remapping on a given [`Mode`](mode::Mode), also letting you
//!     switch modes on [key] sequences.
//!   - [`set`], [`set_default`], [`reset`]: These functions are used
//!     in order to switch [`Mode`](mode::Mode) on demand. Do note
//!     that the switching is done asynchronously.
//!
//! - [`hook`]: Provides utilities for hooking functions in Duat
//!   - [`Hookable`]: An event that you want to provide hooks for, in
//!     order to trigger functions whenever it takes place.
//!   - [`add`], [`add_grouped`], [`remove`]: These functions let you
//!     add or remove functions from [`Hookable`] events. Their
//!     arguments are statically determine by said [`Hookable`]s.
//!
//! - [`cmd`]: Creation of commands in Duat, which can be called at
//!   runtime by the user.
//!   - [`add!`]: This macro lets you create a command, with one or
//!     more callers, and any number of [`Parameter`]s
//!   - [`Parameter`]: A command argument parsed from a string. There
//!     are a bunch of predefined [`Parameter`]s, and things like
//!     [`Vec<P>`] where `P: Parameter`, can also be as
//!     [`Parameter`]s, if you want multiple of the same kind.
//!   - [`call`], [`queue`], [`call_notify`], [`queue_and`], etc:
//!     functions to call or queue commands, which one should be used
//!     depends on the context of the function calling them.
//!
//! - [`form`]: How to stylize [`Text`]
//!   - [`Form`](form::Form): Has many options on what [`Text`] should
//!     look like, are the same as those found on unix terminals.
//!   - [`set`](form::set), [`set_weak`]: These functions let you set
//!     forms with a name. They can be set to a [`Form`](form::Form)
//!     or reference another name of a form.
//!   - [`ColorScheme`]s: These are general purpose
//!     [`Form`](form::Form) setters, with a name that can be called
//!     from the `colorscheme` [command]
//!
//! These are the elements available to you if you want to extend
//! Duat. Additionally, there are some other things that have been
//! left out, but they are available in the [`prelude`], so you can
//! just import it:
//!
//! ```rust
//! // Usually at the top of the crate, below `//!` comments:
//! use duat_core::prelude::*;
//! ```
//!
//! # How to extend Duat
//!
//! Duat is extended primarily through the use of [`Plugin`]s from
//! external crates, these will be plugged in the main config through
//! the [`plug!`] macro, and are modified in place through the builder
//! pattern.
//!
//! For this demonstration, I will create a [`Plugin`] that keeps
//! track of the word count in a [`File`], without reparsing it every
//! time said [`File`] changes.
//!
//! ## Creating a [`Plugin`]
//!
//! First of all, assuming that you have succeeded in following the
//! [installation instructions of duat], you should create a crate
//! with `cargo init`:
//!
//! ```bash
//! cargo init --lib duat-word-count
//! cd duat-word-count
//! ```
//!
//! Wihin that crate, you're should add the `duat-core` dependency:
//!
//! ```bash
//! cargo add duat-core
//! ```
//!
//! Or, if you're using git dependencies:
//!
//! ```bash
//! cargo add duat-core --git https://github.com/AhoyISki/duat
//! ```
//!
//! Finally, you can remove everything in `duat-word-count/src/lib.rs`
//! and start writing your plugin.
//!
//! ```rust
//! // In duat-word-count/src/lib.rs
//! use duat_core::prelude::*;
//!
//! /// A [`Plugin`] to count the number of words in [`File`]s
//! pub struct WordCount;
//!
//! impl<U: Ui> Plugin<U> for WordCount {
//!     fn plug(self) {
//!         todo!();
//!     }
//! }
//! ```
//!
//! In the example, `WordCount` is a plugin that can be included in
//! Duat's `config` crate. It will give the user the ability to get
//! how many words are in a [`File`], without having to reparse the
//! whole buffer every time, given that it could be a very large file.
//! In order to configure the [`Plugin`], you should make use of the
//! builder pattern, returning the [`Plugin`] on every modification.
//!
//! ```rust
//! use duat_core::prelude::*;
//!
//! /// A [`Plugin`] to count the number of words in [`File`]s
//! pub struct WordCount(bool);
//!
//! impl WordCount {
//!     /// Returns a new instance of the [`WordCount`] plugin
//!     pub fn new() -> Self {
//!         WordCount(false)
//!     }
//!
//!     /// Count everything that isn't whitespace as a word character
//!     pub fn not_whitespace(self) -> Self {
//!         WordCount(true)
//!     }
//! }
//!
//! impl<U: Ui> Plugin<U> for WordCount {
//!     fn plug(self) {
//!         todo!();
//!     }
//! }
//! ```
//!
//! Now, there is an option to exclude only whitespace, not just
//! including regular alphanumeric characters. This would count, for
//! example "x(x^3 + 3)" as 3 words, rather than 4.
//!
//! Next, I need to add something to keep track of the number of words
//! in a [`File`]. For [`File`]s specifically, there is a built-in way
//! to keep track of changes through the [`Parser`] trait:
//!
//! ```rust
//! use duat_core::prelude::*;
//!
//! /// A [`Parser`] to keep track of words in a [`File`]
//! struct WordCounter {
//!     words: usize,
//!     regex: &'static str,
//! }
//!
//! impl<U: Ui> Parser<U> for WordCounter {
//!     fn parse(&mut self, pa: &mut Pass, snap: FileSnapshot, ranges: Option<&mut Ranges>) {
//!         todo!();
//!     }
//! }
//! ```
//!
//! Whenever changes take place in a [`File`], those changes will be
//! reported in a [`Moment`], which is essentially just a list of
//! [`Change`]s that took place. This [`Moment`], in a
//! [`FileSnapshot`], will be sent to the [`Parser::parse`] function,
//! in which you are supposed to change the internal state of the
//! [`Parser`] to accomodate the [`Change`]s.
//!
//! The [`FileSnapshot`] gives you a "snapshot" of what the [`File`]
//! looked like after said [`Moment`] took place. It includes the
//! [`Moment`] in question, the [`Bytes`] of the [`File`]'s [`Text`],
//! and the [`PrintCfg`] at that moment in time.
//!
//! First, I'm going to write a function that figures out how many
//! words were added or removed by a [`Change`]:
//!
//! ```rust
//! use duat_core::{prelude::*, text::Change};
//!
//! fn word_diff(regex: &str, bytes: &Bytes, change: Change<&str>) -> i32 {
//!     let [start, _] = bytes.points_of_line(change.start().line());
//!     let [_, end] = bytes.points_of_line(change.added_end().line());
//!
//!     // Recreate the line as it was before the change
//!     // behind_change is just the part of the line before the point
//!     // where a change starts.
//!     // ahead_of_change is the part of the line after the end of
//!     // the Change
//!     let mut behind_change = bytes.strs(start..change.start()).unwrap().to_string();
//!     let ahead_of_change = bytes.strs(change.added_end()..end).unwrap();
//!     // change.taken_str() is the &str that was taken by the Change
//!     behind_change.push_str(change.taken_str());
//!     // By adding these three together, I now have:
//!     // {behind_change}{change.taken_str()}{ahead_of_change}
//!     // Which is what the line looked like before the Change happened
//!     behind_change.extend(ahead_of_change);
//!
//!     // Here, I'm just counting the number of occurances of the
//!     // regex in the line before and after the change.
//!     let words_before = behind_change.search_fwd(regex, ..).unwrap().count();
//!     let words_after = bytes.search_fwd(regex, start..end).unwrap().count();
//!
//!     words_after as i32 - words_before as i32
//! }
//! ```
//!
//! In this method, I am calculating the difference between the number
//! of words in the line before and after the [`Change`] took place.
//! Here [`Bytes::points_of_line`] returns the [`Point`]s where a
//! line starts and ends. I know there are better ways to do this by
//! comparing the text that [was taken] to [what was added],
//! with the context of the lines of the change, but this is
//! just a demonstration, and the more efficient method is left as an
//! exercise to the viewer 😉.
//!
//! Now, just call this on [`parse`]:
//!
//! ```rust
//! # fn word_diff(_: &str, _: &Bytes, _: Change<&str>) -> i32 { 0 }
//! use duat_core::{prelude::*, text::Change};
//!
//! /// A [`Parser`] to keep track of words in a [`File`]
//! struct WordCounter {
//!     words: usize,
//!     regex: &'static str,
//! }
//!
//! impl<U: Ui> Parser<U> for WordCounter {
//!     fn parse(&mut self, pa: &mut Pass, snap: FileSnapshot, _: Option<&mut Ranges>) {
//!         // Rust iterators are magic 🪄
//!         let diff: i32 = snap
//!             .moment
//!             .changes()
//!             .map(|change| word_diff(self.regex, &snap.bytes, change))
//!             .sum();
//!
//!         self.words = (self.words as i32 + diff) as usize;
//!     }
//! }
//! ```
//!
//! And that's it for the [`Parser`] implementation! Now, how do we
//! add it to a [`File`]?
//!
//! In order to add this [`Parser`] to a [`File`], we're going to
//! need a [`ParserCfg`], which is used for configuring [`Parser`]s
//! before they are added:
//!
//! ```rust
//! # struct WordCounter {
//! #     words: usize,
//! #     regex: &'static str,
//! # }
//! # impl<U: Ui> Parser<U> for WordCounter {
//! #     fn parse(&mut self, _: &mut Pass, _: FileSnapshot, _: Option<&mut Ranges>) {}
//! # }
//! use duat_core::prelude::*;
//!
//! struct WordCounterCfg(bool);
//!
//! impl<U: Ui> ParserCfg<U> for WordCounterCfg {
//!     type Parser = WordCounter;
//!
//!     fn init(self, file: &File<U>) -> Result<ParserBox<U>, Text> {
//!         let regex = if self.0 { r"\S+" } else { r"\w+" };
//!         let words = file.bytes().search_fwd(regex, ..).unwrap().count();
//!
//!         let word_counter = WordCounter { words, regex };
//!         Ok(ParserBox::new(file, word_counter))
//!     }
//! }
//! ```
//!
//! In this function, I am returning the `WordCounter`, with a
//! precalculated number of words (since I have to calculate this
//! value at some point), based on the current state of the [`File`].
//!
//! The [`ParserBox`] return value is a wrapper for "constructing the
//! [`Parser`]". To create a [`ParserBox`], there are two functions:
//! [`new`] and [`new_remote`]. The first one is essentially just a
//! wrapper around the [`Parser`]. The second one takes a closure that
//! will build the [`Parser`] in a second thread, this can be useful
//! if you want to create your [`Parser`] remotely.
//!
//! One thing to note is that the [`Parser`] and [`ParserCfg`] can be
//! the same struct, it all depends on your constraints. For most
//! [`Parser`] implementations, that may not be the case, but for this
//! one, instead of storing a `bool` in `WordCounterCfg`, I could've
//! just stored the regex directly, like this:
//!
//! ```rust
//! # struct WordCounter {
//! #     words: usize,
//! #     regex: &'static str,
//! # }
//! # impl<U: Ui> Parser<U> for WordCounter {
//! # fn parse(&mut self, _: &mut Pass, _: FileSnapshot, _: Option<&mut Ranges>) {
//! #     todo!();
//! # }
//! # }
//! use duat_core::prelude::*;
//!
//! impl WordCounter {
//!     /// Returns a new instance of [`WordCounter`]
//!     pub fn new() -> Self {
//!         WordCounter { words: 0, regex: r"\w+" }
//!     }
//! }
//!
//! impl<U: Ui> ParserCfg<U> for WordCounter {
//!     type Parser = Self;
//!
//!     fn init(self, file: &File<U>) -> Result<ParserBox<U>, Text> {
//!         let words = file.bytes().search_fwd(self.regex, ..).unwrap().count();
//!
//!         Ok(ParserBox::new(file, Self { words, ..self }))
//!     }
//! }
//! ```
//!
//! But the former is done for the purpose of demonstration, since (I
//! don't think) this will be the case for most [`Parser`]s.
//!
//! Now, to wrap this all up, the plugin needs to add this [`Parser`]
//! to every opened [`File`]. We do this through the use of a [hook]:
//!
//! ```rust
//! # struct WordCounterCfg(bool);
//! # impl<U: Ui> ParserCfg<U> for WordCounterCfg {
//! #     type Parser = WordCounter;
//! #     fn init(self, _: &File<U>) -> Result<ParserBox<U>, Text> { todo!() }
//! # }
//! # /// A [`Parser`] to keep track of words in a [`File`]
//! # struct WordCounter {
//! #     words: usize,
//! #     regex: &'static str
//! # }
//! # impl<U: Ui> Parser<U> for WordCounter {
//! #    fn parse(&mut self, _: &mut Pass, _: FileSnapshot, _: Option<&mut Ranges>) {}
//! # }
//! use duat_core::prelude::*;
//!
//! /// A [`Plugin`] to count the number of words in [`File`]s
//! pub struct WordCount(bool);
//!
//! impl WordCount {
//!     /// Returns a new instance of the [`WordCount`] plugin
//!     pub fn new() -> Self {
//!         WordCount(false)
//!     }
//!
//!     /// Count everything that isn't whitespace as a word character
//!     pub fn not_whitespace(self) -> Self {
//!         WordCount(true)
//!     }
//! }
//!
//! impl<U: Ui> Plugin<U> for WordCount {
//!     fn plug(self) {
//!         let not_whitespace = self.0;
//!
//!         hook::add::<File<U>, U>(move |pa, (mut cfg, builder)| {
//!             cfg.with_parser(WordCounterCfg(not_whitespace))
//!         });
//!     }
//! }
//! ```
//!
//! Now, whenever a [`File`] is opened, this [`Parser`] will be added
//! to it. This is just one out of many types of [hook] that Duat
//! provides by default. In Duat, you can even [create your own], and
//! [choose when to trigger them].
//!
//! However, while we have added the [`Parser`], how is the user
//! supposed to access this value? Well, one convenient way to do this
//! is through a simple function:
//!
//! ```rust
//! # struct WordCounterCfg(bool);
//! # impl<U: Ui> ParserCfg<U> for WordCounterCfg {
//! #     type Parser = WordCounter;
//! #     fn init(self, _: &File<U>) -> Result<ParserBox<U>, Text> { todo!() }
//! # }
//! # /// A [`Parser`] to keep track of words in a [`File`]
//! # struct WordCounter {
//! #     words: usize,
//! #     regex: &'static str
//! # }
//! # impl<U: Ui> Parser<U> for WordCounter {
//! # fn parse(&mut self, _: &mut Pass, _: FileSnapshot, _: Option<&mut Ranges>) {}
//! # }
//! use duat_core::prelude::*;
//!
//! /// The number of words in a [`File`]
//! pub fn file_words<U: Ui>(file: &File<U>) -> usize {
//!     file.read_parser(|word_counter: &WordCounter| word_counter.words)
//!         .unwrap_or(0)
//! }
//! ```
//!
//! Now, we have a finished plugin:
//!
//! ```rust
//! use duat_core::{prelude::*, text::Change};
//!
//! /// A [`Plugin`] to count the number of words in [`File`]s
//! pub struct WordCount(bool);
//!
//! impl WordCount {
//!     /// Returns a new instance of [`WordCount`]
//!     pub fn new() -> Self {
//!         WordCount(false)
//!     }
//!
//!     /// Count everything that isn't whitespace as a word character
//!     pub fn not_whitespace(self) -> Self {
//!         WordCount(true)
//!     }
//! }
//!
//! impl<U: Ui> Plugin<U> for WordCount {
//!     fn plug(self) {
//!         let not_whitespace = self.0;
//!
//!         hook::add::<File<U>, U>(move |_, (mut cfg, _)| {
//!             cfg.with_parser(WordCounterCfg(not_whitespace))
//!         });
//!     }
//! }
//!
//! /// The number of words in a [`File`]
//! pub fn file_words<U: Ui>(file: &File<U>) -> usize {
//!     file.read_parser(|word_counter: &WordCounter| word_counter.words)
//!         .unwrap_or(0)
//! }
//!
//! /// A [`Parser`] to keep track of words in a [`File`]
//! struct WordCounter {
//!     words: usize,
//!     regex: &'static str,
//! }
//!
//! impl<U: Ui> Parser<U> for WordCounter {
//!     fn parse(&mut self, pa: &mut Pass, snap: FileSnapshot, _: Option<&mut Ranges>) {
//!         let diff: i32 = snap
//!             .moment
//!             .changes()
//!             .map(|change| word_diff(self.regex, &snap.bytes, change))
//!             .sum();
//!
//!         self.words = (self.words as i32 + diff) as usize;
//!     }
//! }
//!
//! struct WordCounterCfg(bool);
//!
//! impl<U: Ui> ParserCfg<U> for WordCounterCfg {
//!     type Parser = WordCounter;
//!
//!     fn init(self, file: &File<U>) -> Result<ParserBox<U>, Text> {
//!         let regex = if self.0 { r"\S+" } else { r"\w+" };
//!
//!         let words = file.bytes().search_fwd(regex, ..).unwrap().count();
//!
//!         Ok(ParserBox::new(file, WordCounter { words, regex }))
//!     }
//! }
//!
//! fn word_diff(regex: &str, bytes: &Bytes, change: Change<&str>) -> i32 {
//!     let [start, _] = bytes.points_of_line(change.start().line());
//!     let [_, end] = bytes.points_of_line(change.added_end().line());
//!
//!     // Recreate the line as it was before the change
//!     let mut line_before = bytes.strs(start..change.start()).unwrap().to_string();
//!     line_before.push_str(change.taken_str());
//!     line_before.extend(bytes.strs(change.added_end()..end).unwrap());
//!
//!     let words_before = line_before.search_fwd(regex, ..).unwrap().count();
//!     let words_after = bytes.search_fwd(regex, start..end).unwrap().count();
//!
//!     words_after as i32 - words_before as i32
//! }
//! ```
//!
//! Once you're done modifying your plugin, you should be ready to
//! publish it to [crates.io]. This is the common registry for
//! packages (crates in Rust), and is also where Duat will pull
//! plugins from. Before publishing, try to follow [these guidelines]
//! in order to improve the usability of the plugin. Now, you should
//! be able to just do this in the `duat-word-count` directory:
//!
//! ```bash
//! cargo publish
//! ```
//!
//! Ok, it's published, but how does one use it?
//!
//! ## Using plugins
//!
//! Assuming that you've already [installed duat], you should have a
//! config crate in `~/.config/duat` (or `$XDG_CONFIG_HOME/duat`), in
//! it, you can call the following command:
//!
//! ```bash
//! cargo add duat-word-count@"*" --rename word-count
//! ```
//!
//! Then, in `src/lib.rs`, you can add the following:
//!
//! ```rust
//! # mod word_count {
//! #     use duat_core::prelude::*;
//! #     pub struct WordCount(bool);
//! #     impl WordCount {
//! #         pub fn new() -> Self { WordCount(false) }
//! #         pub fn not_whitespace(self) -> Self { WordCount(true) }
//! #     }
//! #     impl<U: Ui> Plugin<U> for WordCount {
//! #         fn plug(self) { todo!(); }
//! #     }
//! # };
//! # duat_core::doc_duat!(duat);
//! setup_duat!(setup);
//! use duat::prelude::*;
//! use word_count::*;
//!
//! fn setup() {
//!     plug!(WordCount::new().not_whitespace());
//!
//!     hook::add::<StatusLine<Ui>>(|pa, (sl, _)| {
//!         sl.replace(status!(
//!             "{name_txt} has [wc]{file_words}[] words{Spacer}{mode_txt} {sels_txt} {main_txt}"
//!         ))
//!     });
//! }
//! ```
//!
//! Now, the default [`StatusLine`] should have word count added in,
//! alongside the other usual things in there. It's been added in the
//! `{file_words}` part of the string, which just interpolated that
//! function, imported by `use word_count::*;`, into the status line.
//!
//! There are many other things that plugins can do, like create
//! custom [`Widget`]s, [`Mode`](mode::Mode)s that can change how Duat
//! behaves, customized [commands] and [hook]s, and many such things
//!
//! [`Widget`]: crate::ui::Widget
//! [`File`]: crate::file::File
//! [`WidgetCfg`]: crate::ui::WidgetCfg
//! [`WidgetCreated`]: crate::hook::WidgetCreated
//! [`WindowCreated`]: crate::hook::WindowCreated
//! [`Area`]: crate::ui::Area
//! [`send_key`]: crate::mode::Mode::send_key
//! [key]: crate::mode::KeyEvent
//! [widget]: crate::context::Handle
//! [`map`]: crate::mode::map
//! [`alias`]: crate::mode::alias
//! [`set`]: crate::mode::set
//! [`set_default`]: crate::mode::set_default
//! [`reset`]: crate::mode::reset
//! [`Hookable`]: crate::hook::Hookable
//! [`add`]: crate::hook::add
//! [`add_grouped`]: crate::hook::add_grouped
//! [`remove`]: crate::hook::remove
//! [`Tag`]: crate::text::Tag
//! [`add!`]: crate::cmd::add
//! [`Parameter`]: crate::cmd::Parameter
//! [`call`]: crate::cmd::call
//! [`queue`]: crate::cmd::queue
//! [`call_notify`]: crate::cmd::call_notify
//! [`queue_and`]: crate::cmd::queue_and
//! [`set_weak`]: crate::form::set_weak
//! [`ColorScheme`]: crate::form::ColorScheme
//! [command]: crate::cmd
//! [`cargo`]: https://doc.rust-lang.org/book/ch01-01-installation.html
//! [builder pattern]: https://rust-unofficial.github.io/patterns/patterns/creational/builder.html
//! [`Parser`]: crate::file::Parser
//! [`ParserCfg`]: crate::file::ParserCfg
//! [`Moment`]: crate::text::Moment
//! [`Change`]: crate::text::Change
//! [`Parser::parse`]: crate::file::Parser::parse
//! [`Parser::update_range`]: crate::file::Parser::update_range
//! [`update_range`]: crate::file::Parser::update_range
//! [`Bytes`]: crate::text::Bytes
//! [`Bytes::points_of_line`]: crate::text::Bytes::points_of_line
//! [`Point`]: crate::text::Point
//! [was taken]: crate::text::Change::taken_str
//! [what was added]: crate::text::Change::added_str
//! [`<WordCounter as Parser>::parse`]: crate::file::Parser::parse
//! [`read`]: crate::data::RwData::read
//! [`write`]: crate::data::RwData::write
//! [number one rule of Rust]: https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html
//! [create your own]: crate::hook::Hookable
//! [choose when to trigger them]: crate::hook::trigger
//! [`plug!`]: https://docs.rs/duat/latest/duat/prelude/macro.plug.html
//! [crates.io]: https://crates.io
//! [these guidelines]: https://doc.rust-lang.org/book/ch14-02-publishing-to-crates-io.html
//! [installed duat]: https://github.com/AhoyISki/duat?tab=readme-ov-file#getting-started
//! [dependencies]: https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html
//! [`StatusLine`]: https://docs.rs/duat/latest/duat/prelude/macro.status.html
//! [commands]: crate::cmd
//! [`RwData<Self>`]: crate::data::RwData
//! [`ParserBox`]: crate::file::ParserBox
//! [`new_remote`]: crate::file::ParserBox::new_remote
//! [`new`]: crate::file::ParserBox::new
//! [installation instructions of duat]: https://github.com/AhoyISki/duat?tab=readme-ov-file#getting-started
//! [`parse`]: crate::file::Parser::parse
//! [`FileSnapshot`]: crate::file::FileSnapshot
//! [`PrintCfg`]: crate::cfg::PrintCfg
#![feature(
    decl_macro,
    step_trait,
    type_alias_impl_trait,
    trait_alias,
    debug_closure_helpers,
    unboxed_closures,
    associated_type_defaults,
    dropck_eyepatch,
    fn_traits,
    box_as_ptr,
    iter_array_chunks,
    thread_spawn_hook
)]
#![allow(clippy::single_range_in_vec_init)]

use std::{any::TypeId, sync::OnceLock};

#[allow(unused_imports)]
use dirs_next::cache_dir;
pub use main_thread_only::MainThreadOnly;
use parking_lot::Mutex;

use self::ui::Ui;

pub mod prelude {
    //! The prelude of Duat
    //!
    //! I recommend adding `use duat_core::prelude::*` to the top of
    //! modules, in order to bring everything that you could possibly
    //! need into scope. This is especially the case if you're not
    //! working with an LSP which can just bring those symbols into
    //! scope.
    pub use lender::Lender;

    pub use crate::{
        Plugin, Plugins,
        cfg::PrintCfg,
        cmd,
        context::{self, Handle},
        data::{Pass, RwData},
        file::{File, FileTracker, Parser, ParserCfg},
        form::{self, Form},
        hook,
        mode::{self, KeyCode, KeyEvent, KeyMod, Mode, key},
        ranges::Ranges,
        text::{
            AlignCenter, AlignLeft, AlignRight, Bytes, Conceal, Ghost, Matcheable, Moment, Point,
            Spacer, Tagger, Text, txt,
        },
        ui::{Area, BuildInfo, GetAreaId, PushSpecs, Ui, Widget, WidgetCfg},
    };
}

pub mod cfg;
pub mod cmd;
pub mod context;
pub mod data;
mod doc_duat_macro;
pub mod file;
pub mod form;
pub mod hook;
pub mod mode;
mod ranges;
#[doc(hidden)]
pub mod session;
pub mod text;
pub mod ui;
pub mod utils;

/// A plugin for Duat
///
/// Plugins must follow the builder pattern, and can be specific to
/// certain [`Ui`]s. Generally, plugins should do all the setup
/// necessary for their function when [`Plugin::plug`] is called.
///
/// [`Plugin`] will usually be [plugged] by a `macro` in the Duat
/// config crate. This macro requires that the [`Plugin`] be
/// compatible with the [`Ui`]:
///
/// ```rust
/// # use duat_core::{Plugin, ui::Ui};
/// // It's not a supertrait of Plugin, but you must implement
/// // Default in order to use the plugin.
/// #[derive(Default)]
/// struct MyPlugin(bool);
///
/// impl<U: Ui> Plugin<U> for MyPlugin {
///     fn plug(self) {
///         //..
///     }
/// }
///
/// impl MyPlugin {
///     /// Returns a new instance of the [`MyPlugin`] plugin
///     pub fn new() -> Self {
///         Self(false)
///     }
///
///     /// Modifies [`MyPlugin`]
///     pub fn modify(self) -> Self {
///         //..
/// #       self
///     }
/// }
/// ```
///
/// In this case, this [`Plugin`] is compatible with every possible
/// [`Ui`].
///
/// [plugged]: Plugin::plug
/// [`PhantomData`]: std::marker::PhantomData
pub trait Plugin<U: Ui>: 'static {
    /// Sets up the [`Plugin`]
    fn plug(self, plugins: &Plugins<U>);
}

static PLUGINS: OnceLock<Box<dyn std::any::Any + Send + Sync>> = OnceLock::new();

/// A struct for [`Plugin`]s to declare dependencies on other
/// [`Plugin`]s
pub struct Plugins<'a, U: Ui>(&'a MainThreadOnly<Mutex<Vec<(PluginFn<U>, TypeId)>>>);

impl<U: Ui> Plugins<'_, U> {
    /// Returnss a new instance of [`Plugins`]
    ///
    /// **FOR USE BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    pub fn _new() -> Self {
        let plugins = PLUGINS.get_or_init(|| {
            Box::new(MainThreadOnly::new(Mutex::new(
                Vec::<(PluginFn<U>, TypeId)>::new(),
            )))
        });
        Self(plugins.downcast_ref().expect("Used two different Uis."))
    }

    /// Require that a [`Plugin`] be added
    ///
    /// This plugin may have already been added, or it might be added
    /// by this call.
    ///
    /// > [!NOTE]
    /// >
    /// > As a [`Plugin`] writer, you are not allowed to change the
    /// > configurations of the [`Plugin`], only the end user, by
    /// > calling the [`plug!`] macro, is allowed to do that.
    ///
    /// [`plug!`]: https://docs.rs/duat/latest/duat/macro.plug.html
    pub fn require<P: Plugin<U> + Default>(&self) {
        // SAFETY: This function can only push new elements to the list, not
        // accessing the !Send functions within.
        let mut plugins = unsafe { self.0.get() }.lock();
        if !plugins.iter().any(|(_, ty)| *ty == TypeId::of::<P>()) {
            plugins.push((
                Some(Box::new(|plugins| P::default().plug(plugins))),
                TypeId::of::<P>(),
            ));
        };
    }
}

mod main_thread_only {
    /// A container meant for access in only the main thread
    ///
    /// Use this if you want a static value that is not
    /// [`Send`]/[`Sync`].
    #[derive(Default)]
    #[doc(hidden)]
    pub struct MainThreadOnly<T>(T);

    impl<T> MainThreadOnly<T> {
        /// Returns a new [`MainThreadOnly`]
        pub const fn new(value: T) -> Self {
            Self(value)
        }

        /// Acquires the inner value.
        ///
        /// # Safety
        ///
        /// You must ensure that this operation is taking place in the
        /// main thread of execution, although this function might
        /// take a [`Pass`] parameter later on, in order to
        /// lift that requirement.
        pub unsafe fn get(&self) -> &T {
            &self.0
        }
    }

    unsafe impl<T> Send for MainThreadOnly<T> {}
    unsafe impl<T> Sync for MainThreadOnly<T> {}
}

pub mod clipboard {
    //! Clipboard interaction for Duat
    //!
    //! Just a regular clipboard, no image functionality.
    use std::sync::{Mutex, OnceLock};

    /// A clipboard for Duat, can be platform based, or local
    #[doc(hidden)]
    pub enum Clipboard {
        #[cfg(target_os = "android")]
        Platform,
        #[cfg(not(target_os = "android"))]
        Platform(clipboard::Clipboard),
        Local(String),
    }

    impl Default for Clipboard {
        fn default() -> Self {
            #[cfg(not(target_os = "android"))]
            match clipboard::Clipboard::new() {
                Ok(clipb) => Self::Platform(clipb),
                Err(_) => Self::Local(String::new()),
            }

            #[cfg(target_os = "android")]
            Self::Platform
        }
    }

    static CLIPB: OnceLock<&'static Mutex<Clipboard>> = OnceLock::new();

    /// Gets a [`String`] from the clipboard
    ///
    /// This can fail if the clipboard does not contain UTF-8 encoded
    /// text.
    ///
    /// Or if there is no clipboard i guess
    pub fn get_text() -> Option<String> {
        let mut clipb = CLIPB.get().unwrap().lock().unwrap();
        match &mut *clipb {
            #[cfg(target_os = "android")]
            Clipboard::Platform => clipboard::get_text()
                .map_err(|err| crate::context::error!("{err}"))
                .ok(),
            #[cfg(not(target_os = "android"))]
            Clipboard::Platform(clipb) => clipb.get_text().ok(),
            Clipboard::Local(clipb) => Some(clipb.clone()).filter(String::is_empty),
        }
    }

    /// Sets a [`String`] to the clipboard
    pub fn set_text(text: impl std::fmt::Display) {
        let mut clipb = CLIPB.get().unwrap().lock().unwrap();
        match &mut *clipb {
            #[cfg(target_os = "android")]
            Clipboard::Platform => {
                if let Err(err) = clipboard::set_text(text.to_string()) {
                    crate::context::error!("{err}");
                }
            }
            #[cfg(not(target_os = "android"))]
            Clipboard::Platform(clipb) => clipb.set_text(text.to_string()).unwrap(),
            Clipboard::Local(clipb) => *clipb = text.to_string(),
        }
    }

    pub(crate) fn set_clipboard(clipb: &'static Mutex<Clipboard>) {
        CLIPB.set(clipb).map_err(|_| {}).expect("Setup ran twice");
    }
}

////////// Text Builder macros (for pub/private bending)
mod private_exports {
    pub use format_like::format_like;

    pub macro log($target:expr, $lvl:expr, $($arg:tt)*) {{
        #[allow(unused_must_use)]
        let text = $crate::text::txt!($($arg)*).build();

		$crate::context::logs().push_record($crate::context::Record::new(
    		text,
    		$lvl,
    		$target,
    		Some(module_path!()),
    		Some(file!()),
    		Some(line!())
		));
    }}

    pub macro parse_str($builder:expr, $str:literal) {{
        let builder = $builder;
        builder.push_str($str);
        builder
    }}

    pub macro parse_arg {
        ($builder:expr, "", $arg:expr) => {{
            let builder = $builder;
            builder.push_builder_part($arg.into());
            builder
        }},
        ($builder:expr, $modif:literal, $arg:expr) => {{
            let builder = $builder;
            builder.push_str(format!(concat!("{:", $modif, "}"), &$arg));
            builder
        }},
    }

    pub macro parse_form {
        ($builder:expr, $priority:literal,) => {{
            const PRIORITY: u8 = $crate::priority($priority);
            let builder = $builder;
            let id = $crate::form::DEFAULT_ID;
            builder.push_builder_part(id.to_tag(PRIORITY).into());
            builder
        }},
        ($builder:expr, $priority:literal, a) => {{
            const PRIORITY: u8 = $crate::priority($priority);
            let builder = $builder;
            let id = $crate::form::ACCENT_ID;
            builder.push_builder_part(id.to_tag(PRIORITY).into());
            builder
        }},
        ($builder:expr, $priority:literal, $($form:tt)*) => {{
            const PRIORITY: u8 = $crate::priority($priority);
            let builder = $builder;
            let id = $crate::form::id_of!(concat!($(stringify!($form)),*));
            builder.push_builder_part(id.to_tag(PRIORITY).into());
            builder
        }},
    }
}

/// Converts a string to a valid priority
#[doc(hidden)]
pub const fn priority(priority: &str) -> u8 {
    let mut bytes = priority.as_bytes();
    let mut val = 0;

    while let [byte, rest @ ..] = bytes {
        assert!(b'0' <= *byte && *byte <= b'9', "invalid digit");
        val = val * 10 + (*byte - b'0') as usize;
        bytes = rest;
    }

    assert!(val <= 250, "priority cannot exceed 250");

    val as u8
}

type PluginFn<U> = Option<Box<dyn FnOnce(&Plugins<U>)>>;
