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
//!     [`OnFileOpen`] and [`OnWindowOpen`] [hook]s.
//!   - [`Ui`] and [`RawArea`]s: These are used if you want to create
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
//!     let mut behind_change = bytes.strs(start..change.start()).to_string();
//!     let ahead_of_change = bytes.strs(change.added_end()..end);
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
//!         Ok(ParserBox::new_local(file, word_counter))
//!     }
//! }
//! ```
//!
//! In this function, I am returning the `WordCounter`, with a
//! precalculated number of words (since I have to calculate this
//! value at some point), based on the current state of the [`File`].
//!
//! The [`ParserBox`] return value is a wrapper for "constructing the
//! [`Parser`]". If you use a function like [`ParserBox::new_send`],
//! you'd enable out-of-thread updating for the [`Parser`]. If you
//! used [`ParserBox::new_remote`], the [`Parser`] itself would be
//! constructed out of thread. In this case, because we are using
//! [`new_local`], the [`Parser`] won't ever be sent to other threads.
//! This is almost always what you want.
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
//!         Ok(ParserBox::new_local(file, Self { words, ..self }))
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
//! use duat_core::{hook::OnFileOpen, prelude::*};
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
//!         hook::add::<OnFileOpen<U>, U>(move |pa, builder| {
//!             builder.add_parser(pa, WordCounterCfg(not_whitespace));
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
//! use duat_core::{hook::OnFileOpen, prelude::*, text::Change};
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
//!         hook::add::<OnFileOpen<U>, U>(move |pa, builder| {
//!             builder.add_parser(pa, WordCounterCfg(not_whitespace));
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
//!         Ok(ParserBox::new_local(file, WordCounter { words, regex }))
//!     }
//! }
//!
//! fn word_diff(regex: &str, bytes: &Bytes, change: Change<&str>) -> i32 {
//!     let [start, _] = bytes.points_of_line(change.start().line());
//!     let [_, end] = bytes.points_of_line(change.added_end().line());
//!
//!     // Recreate the line as it was before the change
//!     let mut line_before = bytes.strs(start..change.start()).to_string();
//!     line_before.push_str(change.taken_str());
//!     line_before.extend(bytes.strs(change.added_end()..end));
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
//! # use duat_core::doc_duat as duat;
//! setup_duat!(setup);
//! use duat::prelude::*;
//! use word_count::*;
//!
//! fn setup() {
//!     plug!(WordCount::new().not_whitespace());
//!
//!     hook::add::<StatusLine<Ui>>(|pa, (sl, _)| {
//!         sl.replace(status!(
//!             "{file_fmt} has [wc]{file_words}[] words{Spacer}{mode_fmt} {sels_fmt} {main_fmt}"
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
//! [`WidgetCfg`]: crate::ui::WidgetCfg
//! [`OnFileOpen`]: crate::hook::OnFileOpen
//! [`OnWindowOpen`]: crate::hook::OnWindowOpen
//! [`RawArea`]: crate::ui::RawArea
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
//! [`ParserBox::new_send`]: crate::file::ParserBox::new_send
//! [`ParserBox::new_remote`]: crate::file::ParserBox::new_remote
//! [`new_local`]: crate::file::ParserBox::new_local
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
    box_as_ptr
)]
#![allow(clippy::single_range_in_vec_init)]

use std::{
    any::{TypeId, type_name},
    collections::HashMap,
    ops::Range,
    path::{Path, PathBuf},
    sync::{LazyLock, RwLock},
};

#[allow(unused_imports)]
use dirs_next::cache_dir;
pub use main_thread_only::MainThreadOnly;

use self::{
    data::Pass,
    file::File,
    text::Text,
    ui::{Node, Ui, Widget, Window},
};
use crate::text::txt;

pub mod cfg;
pub mod cmd;
pub mod context;
pub mod data;
#[doc(hidden)]
pub mod doc_duat;
pub mod file;
pub mod form;
pub mod hook;
pub mod mode;
#[doc(hidden)]
pub mod session;
pub mod text;
pub mod ui;

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
pub trait Plugin<U: Ui>: Sized {
    /// Sets up the [`Plugin`]
    fn plug(self);
}

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
        Plugin,
        cfg::PrintCfg,
        cmd,
        context::{self, FileHandle, Handle},
        data::{Pass, RwData},
        file::{File, FileParts, FileSnapshot, Parser, ParserBox, ParserCfg},
        form::{self, Form},
        hook,
        mode::{self, KeyCode, KeyEvent, KeyMod, Mode, key},
        ranges::Ranges,
        text::{
            AlignCenter, AlignLeft, AlignRight, Bytes, Conceal, Ghost, Matcheable, Moment, Point,
            Spacer, Tagger, Text, txt,
        },
        ui::{PushSpecs, RawArea, Ui, Widget, WidgetCfg},
    };
}

mod ranges {
    use std::ops::Range;

    use gapbuf::{GapBuffer, gap_buffer};

    use crate::merging_range_by_guess_and_lazy_shift;

    /// A list of non intersecting exclusive [`Range<usize>`]s
    ///
    /// The primary purpose of this struct is to serve [`Parser`]s by
    /// telling Duat which ranges need to be updated. This lets Duat
    /// minimize as much as possible the amount of work done to update
    /// the [`Text`] when it changes in a [`File`].
    ///
    /// [`Text`]: crate::text::Text
    /// [`File`]: crate::File
    /// [`Parser`]: crate::file::Parser
    #[derive(Clone, Default, Debug)]
    pub struct Ranges {
        list: GapBuffer<Range<usize>>,
        shift_state: (usize, i32),
        min_len: usize,
    }

    impl Ranges {
        /// Return a new instance of [`Ranges`]
        ///
        /// This assumes that `max` is the total "length" of the
        /// element in use.
        pub fn full(max: usize) -> Self {
            Self {
                list: gap_buffer![0..max],
                ..Self::empty()
            }
        }

        /// Returns a new empty [`Ranges`]
        pub fn empty() -> Self {
            Self {
                list: GapBuffer::new(),
                shift_state: (0, 0),
                min_len: 0,
            }
        }

        /// Sets a minimum length to keep [`Range`]s
        pub fn set_min_len(&mut self, min: usize) {
            self.min_len = min;

            let mut i = 0;
            self.list.retain(|range| {
                i += 1;

                if range.len() < self.min_len {
                    if i - 1 < self.shift_state.0 {
                        self.shift_state.0 -= 1;
                    }
                    false
                } else {
                    true
                }
            });
        }

        /// Adds a range to the list of [`Range<usize>`]s
        ///
        /// This range will be merged in with the others on the list,
        /// so it may bridge gaps between ranges or for longer
        /// ranges within, without allowing for the existance
        /// of intersecting ranges.
        pub fn add(&mut self, new: Range<usize>) {
            let (shift_from, total_diff) = std::mem::take(&mut self.shift_state);
            let m_range = merging_range_by_guess_and_lazy_shift(
                (&self.list, self.list.len()),
                (shift_from, [new.start, new.end]),
                (shift_from, total_diff, 0, sh),
                (|r| r.start, |r| r.end),
            );

            // The ranges in this region have not been callibrated yet.
            if shift_from <= m_range.end {
                for range in self.list.range_mut(shift_from..m_range.end).iter_mut() {
                    range.start = sh(range.start, total_diff);
                    range.end = sh(range.end, total_diff);
                }
            }

            let added_range = {
                let slice = self.list.range(m_range.clone());
                let mut iter = slice.iter();

                let first = iter.next().cloned().unwrap_or(new.clone());
                let last = iter.next_back().cloned().unwrap_or(first.clone());

                first.start.min(new.start)..last.end.max(new.end)
            };

            let shift_from = shift_from.saturating_sub(m_range.len()).max(m_range.start) + 1;
            if shift_from < self.list.len() + 1 - m_range.len() {
                self.shift_state = (shift_from, total_diff);
            }

            self.list.splice(m_range.clone(), [added_range]);
        }

        /// Removes a [`Range`] from the list, returning an
        /// [`Iterator`] over the [`Range`]s that were taken
        pub fn remove(
            &mut self,
            within: Range<usize>,
        ) -> impl ExactSizeIterator<Item = Range<usize>> + '_ {
            let (shift_from, total_diff) = std::mem::take(&mut self.shift_state);
            let m_range = merging_range_by_guess_and_lazy_shift(
                (&self.list, self.list.len()),
                (shift_from, [within.start, within.end]),
                (shift_from, total_diff, 0, sh),
                (|r| r.start, |r| r.end),
            );

            // The ranges in this region have not been callibrated yet.
            if shift_from <= m_range.end {
                for range in self.list.range_mut(shift_from..m_range.end).iter_mut() {
                    range.start = sh(range.start, total_diff);
                    range.end = sh(range.end, total_diff);
                }
            }

            let split_off = {
                let slice = self.list.range(m_range.clone());

                let first = slice.iter().next().cloned();
                let last = slice.iter().next_back().cloned();

                let split_start = first.filter(|f| f.start < within.start).map(|first| {
                    self.list[m_range.start].start = within.start;
                    first.start..within.start
                });
                let split_end = last.filter(|l| l.end > within.end).map(|last| {
                    self.list[m_range.end - 1].end = within.end;
                    within.end..last.end
                });

                let min_len = |range: &Range<usize>| range.len() >= self.min_len;

                [split_start.filter(min_len), split_end.filter(min_len)]
            };

            let added = split_off.iter().flatten().count();

            let shift_from = shift_from.saturating_sub(m_range.len()).max(m_range.start) + added;
            if shift_from < self.list.len() + added - m_range.len() {
                self.shift_state = (shift_from, total_diff);
            }

            self.list.splice(m_range, split_off.into_iter().flatten())
        }

        /// Applies the [`add`] function to another [`Ranges`]s
        ///
        /// [`add`]: Self::add
        pub fn merge(&mut self, other: Self) {
            for range in other.list {
                self.add(range)
            }
        }

        /// Shifts the [`Range<usize>`]s by a list of [`Change`]s
        ///
        /// [`Change`]: crate::text::Change
        pub fn shift_by(&mut self, from: usize, diff: i32) {
            let (shift_from, total_diff) = std::mem::take(&mut self.shift_state);

            // The range of changes that will be drained
            let m_range = merging_range_by_guess_and_lazy_shift(
                (&self.list, self.list.len()),
                (shift_from, [from, from + ((-diff.min(0)) as usize)]),
                (shift_from, total_diff, 0, sh),
                (|r| r.start, |r| r.end),
            );

            // If shift_from < m_range.end, I need to shift the changes between
            // the two, so that they match the shifting of the changes
            // before shift_from
            if shift_from < m_range.end && total_diff != 0 {
                for range in self.list.range_mut(shift_from..m_range.end).iter_mut() {
                    range.start = sh(range.start, total_diff);
                    range.end = sh(range.end, total_diff);
                }
            // If shift_from > m_range.end, There are now three
            // shifted states among ranges: The ones
            // before m_range.end, between m_range.end
            // and shift_from, and after shift_from. I
            // will update the second group so that it is
            // shifted by shift + change. shift(), that
            // way, it will be shifted like
            // the first group.
            } else if shift_from > m_range.end && diff != 0 {
                for range in &mut self.list.range_mut(m_range.end..shift_from) {
                    range.start = sh(range.start, diff);
                    range.end = sh(range.end, diff);
                }
            }

            let range_left = {
                let slice = self.list.range(m_range.clone());
                let mut iter = slice.iter();

                iter.next().and_then(|first| {
                    let last = iter.next_back().unwrap_or(first);
                    let range = first.start.min(from)..sh(last.end, diff).max(from);

                    (range.len() >= self.min_len).then_some(range)
                })
            };

            let added = range_left.is_some() as usize;

            let shift_from = shift_from.saturating_sub(m_range.len()).max(m_range.start) + added;
            if shift_from < self.list.len() + added - m_range.len() {
                self.shift_state = (shift_from, total_diff + diff);
            }

            self.list.splice(m_range.clone(), range_left);
        }

        /// Returns the number of [`Range<usize>`]s
        pub fn len(&self) -> usize {
            self.list.len()
        }

        /// Returns `true` if there are no [`Range<usize>`]s
        pub fn is_empty(&self) -> bool {
            self.list.is_empty()
        }
    }

    impl IntoIterator for Ranges {
        type IntoIter = IntoIter;
        type Item = Range<usize>;

        #[define_opaque(IntoIter)]
        fn into_iter(self) -> Self::IntoIter {
            let (shift_from, diff) = self.shift_state;

            let mut i = 0;
            self.list.into_iter().map(move |mut range| {
                if i >= shift_from {
                    range.start = sh(range.start, diff);
                    range.end = sh(range.end, diff);
                }
                i += 1;

                range
            })
        }
    }

    /// Shorthand to shift a [`usize`] by an [`i32`]
    fn sh(n: usize, diff: i32) -> usize {
        (n as i32 + diff) as usize
    }

    pub type IntoIter = impl ExactSizeIterator<Item = Range<usize>>;
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
        Platform(arboard::Clipboard),
        Local(String),
    }

    impl Default for Clipboard {
        fn default() -> Self {
            match arboard::Clipboard::new() {
                Ok(clipb) => Self::Platform(clipb),
                Err(_) => Self::Local(String::new()),
            }
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
            Clipboard::Platform(clipb) => clipb.get_text().ok(),
            Clipboard::Local(clipb) => Some(clipb.clone()).filter(String::is_empty),
        }
    }

    /// Sets a [`String`] to the clipboard
    pub fn set_text(text: impl std::fmt::Display) {
        let mut clipb = CLIPB.get().unwrap().lock().unwrap();
        match &mut *clipb {
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
            builder.push($arg);
            builder
        }},
        ($builder:expr, $modif:literal, $arg:expr) => {{
            let builder = $builder;
            builder.push(format!(concat!("{:", $modif, "}"), &$arg));
            builder
        }},
    }

    pub macro parse_form {
        ($builder:expr, $priority:literal,) => {{
            const PRIORITY: u8 = $crate::priority($priority);
            let builder = $builder;
            let id = $crate::form::DEFAULT_ID;
            builder.push(id.to_tag(PRIORITY));
            builder
        }},
        ($builder:expr, $priority:literal, a) => {{
            const PRIORITY: u8 = $crate::priority($priority);
            let builder = $builder;
            let id = $crate::form::ACCENT_ID;
            builder.push(id.to_tag(PRIORITY));
            builder
        }},
        ($builder:expr, $priority:literal, $($form:tt)*) => {{
            const PRIORITY: u8 = $crate::priority($priority);
            let builder = $builder;
            let id = $crate::form::id_of!(concat!($(stringify!($form)),*));
            builder.push(id.to_tag(PRIORITY));
            builder
        }},
    }
}

////////// General utility

/// Takes a type and generates an appropriate name for it
///
/// Use this function if you need a name of a type to be
/// referrable by string, such as by commands or by the
/// user.
///
/// # NOTE
///
/// Any `<Ui>` or `Ui, ` type arguments will be removed from the final
/// result, since Duat is supposed to have only one [`Ui`] in use.
pub fn duat_name<T: ?Sized + 'static>() -> &'static str {
    fn duat_name_inner(type_id: TypeId, type_name: &str) -> &'static str {
        static NAMES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
            LazyLock::new(RwLock::default);
        let mut names = NAMES.write().unwrap();

        if let Some(name) = names.get(&type_id) {
            name
        } else {
            let mut name = String::new();

            for path in type_name.split_inclusive(['<', '>', ',', ' ']) {
                for segment in path.split("::") {
                    let is_type = segment.chars().any(|c| c.is_uppercase());
                    let is_punct = segment.chars().all(|c| !c.is_alphanumeric());
                    let is_dyn = segment.starts_with("dyn");
                    if is_type || is_punct || is_dyn {
                        name.push_str(segment);
                    }
                }
            }

            while let Some((i, len)) = None
                .or_else(|| name.find("<Ui>").map(|i| (i, "<Ui>".len())))
                .or_else(|| name.find("Ui, ").map(|i| (i, "Ui, ".len())))
                .or_else(|| name.find("::<Ui>").map(|i| (i, "::<Ui>".len())))
            {
                unsafe {
                    name.as_mut_vec().splice(i..(i + len), []);
                }
            }

            names.insert(type_id, name.leak());
            names.get(&type_id).unwrap()
        }
    }

    duat_name_inner(TypeId::of::<T>(), std::any::type_name::<T>())
}

/// Returns the source crate of a given type
pub fn src_crate<T: ?Sized + 'static>() -> &'static str {
    fn src_crate_inner(type_id: TypeId, type_name: &'static str) -> &'static str {
        static CRATES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
            LazyLock::new(|| RwLock::new(HashMap::new()));
        let mut crates = CRATES.write().unwrap();

        if let Some(src_crate) = crates.get(&type_id) {
            src_crate
        } else {
            let src_crate = type_name.split([' ', ':']).find(|w| *w != "dyn").unwrap();

            crates.insert(type_id, src_crate);
            crates.get(&type_id).unwrap()
        }
    }

    src_crate_inner(TypeId::of::<T>(), std::any::type_name::<T>())
}

/// The path for the config crate of Duat
pub fn crate_dir() -> Result<&'static Path, Text> {
    static CRATE_DIR: LazyLock<Option<&Path>> = LazyLock::new(|| {
        dirs_next::config_dir().map(|config_dir| {
            let path: &'static str = config_dir.join("duat").to_string_lossy().to_string().leak();

            std::fs::create_dir_all(path).unwrap();
            Path::new(path)
        })
    });

    CRATE_DIR.ok_or_else(|| txt!("Config directory is [a]undefined").build())
}

/// The path for a plugin's auxiliary files
///
/// If you want to store something in a more permanent basis, and also
/// possibly allow for the user to modify some files (e.g. a TOML file
/// with definitions for various LSPs), you should place it in here.
///
/// This function will also create said directory, if it doesn't
/// already exist, only returning [`Some`], if it managed to verify
/// its existance.
pub fn plugin_dir(plugin: &str) -> Result<PathBuf, Text> {
    assert_ne!(plugin, "", "Can't have an empty plugin name");

    static PLUGIN_DIR: LazyLock<Option<&Path>> = LazyLock::new(|| {
        dirs_next::data_local_dir().map(|local_dir| {
            let path: &'static str = local_dir
                .join("duat/plugins")
                .to_string_lossy()
                .to_string()
                .leak();

            Path::new(path)
        })
    });

    let plugin_dir = (*PLUGIN_DIR)
        .ok_or_else(|| txt!("Local directory is [a]undefined"))?
        .join(plugin);
    std::fs::create_dir_all(&plugin_dir)?;

    Ok(plugin_dir)
}

/// Convenience function for the bounds of a range
#[track_caller]
fn get_ends(range: impl std::ops::RangeBounds<usize>, max: usize) -> (usize, usize) {
    let start = match range.start_bound() {
        std::ops::Bound::Included(start) => *start,
        std::ops::Bound::Excluded(start) => *start + 1,
        std::ops::Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        std::ops::Bound::Included(end) => *end + 1,
        std::ops::Bound::Excluded(end) => *end,
        std::ops::Bound::Unbounded => max,
    };
    assert!(
        start <= max,
        "index out of bounds: the len is {max}, but the index is {start}",
    );
    assert!(
        end <= max,
        "index out of bounds: the len is {max}, but the index is {end}",
    );

    (start, end)
}

/// Adds two shifts together
pub fn add_shifts(lhs: [i32; 3], rhs: [i32; 3]) -> [i32; 3] {
    let b = lhs[0] + rhs[0];
    let c = lhs[1] + rhs[1];
    let l = lhs[2] + rhs[2];
    [b, c, l]
}

/// Allows binary searching with an initial guess and displaced
/// entries
///
/// This function essentially looks at a list of entries and with a
/// starting shift position, shifts them by an amount, before
/// comparing inside of the binary search.
fn merging_range_by_guess_and_lazy_shift<T, U: Copy + Ord + std::fmt::Debug, V: Copy>(
    (container, len): (&impl std::ops::Index<usize, Output = T>, usize),
    (guess_i, [start, end]): (usize, [U; 2]),
    (shift_from, shift, zero_shift, shift_fn): (usize, V, V, fn(U, V) -> U),
    (start_fn, end_fn): (fn(&T) -> U, fn(&T) -> U),
) -> Range<usize> {
    let sh = |n: usize| if n >= shift_from { shift } else { zero_shift };
    let start_of = |i: usize| shift_fn(start_fn(&container[i]), sh(i));
    let end_of = |i: usize| shift_fn(end_fn(&container[i]), sh(i));
    let search = |n: usize, t: &T| shift_fn(start_fn(t), sh(n));

    let mut m_range = if let Some(prev_i) = guess_i.checked_sub(1)
        && (prev_i < len && start_of(prev_i) <= start && start <= end_of(prev_i))
    {
        prev_i..guess_i
    } else {
        match binary_search_by_key_and_index(container, len, start, search) {
            Ok(i) => i..i + 1,
            Err(i) => {
                if let Some(prev_i) = i.checked_sub(1)
                    && start <= end_of(prev_i)
                {
                    prev_i..i
                } else {
                    i..i
                }
            }
        }
    };

    // On Cursors, the Cursors can intersect, so we need to check
    while m_range.start > 0 && start <= end_of(m_range.start - 1) {
        m_range.start -= 1;
    }

    // This block determines how far ahead this cursor will merge
    if m_range.end < len && end >= start_of(m_range.end) {
        m_range.end = match binary_search_by_key_and_index(container, len, end, search) {
            Ok(i) => i + 1,
            Err(i) => i,
        }
    }

    while m_range.end + 1 < len && end >= start_of(m_range.end + 1) {
        m_range.end += 1;
    }

    m_range
}

fn binary_search_by_key_and_index<T, K>(
    container: &(impl std::ops::Index<usize, Output = T> + ?Sized),
    len: usize,
    key: K,
    f: impl Fn(usize, &T) -> K,
) -> std::result::Result<usize, usize>
where
    K: PartialEq + Eq + PartialOrd + Ord,
{
    let mut size = len;
    let mut left = 0;
    let mut right = size;

    while left < right {
        let mid = left + size / 2;

        let k = f(mid, &container[mid]);

        match k.cmp(&key) {
            std::cmp::Ordering::Less => left = mid + 1,
            std::cmp::Ordering::Equal => return Ok(mid),
            std::cmp::Ordering::Greater => right = mid,
        }

        size = right - left;
    }

    Err(left)
}

/// An entry for a file with the given name
#[allow(clippy::result_large_err)]
fn file_entry<'a, U: Ui>(
    pa: &Pass,
    windows: &'a [Window<U>],
    name: &str,
) -> Result<(usize, usize, &'a Node<U>), Text> {
    windows
        .iter()
        .enumerate()
        .flat_map(window_index_widget)
        .find(|(.., node)| node.read_as(pa, |f: &File<U>| f.name() == name) == Some(true))
        .ok_or_else(|| txt!("File with name [a]{name}[] not found").build())
}

/// An entry for a widget of a specific type
#[allow(clippy::result_large_err)]
fn widget_entry<'a, W: Widget<U>, U: Ui>(
    pa: &Pass,
    windows: &'a [Window<U>],
    w: usize,
) -> Result<(usize, usize, &'a Node<U>), Text> {
    let handle = context::fixed_file::<U>(pa).unwrap();

    if let Some(handle) = handle.get_related_widget::<W>(pa) {
        windows
            .iter()
            .enumerate()
            .flat_map(window_index_widget)
            .find(|(.., n)| n.ptr_eq(handle.widget()))
    } else {
        iter_around(windows, w, 0).find(|(.., node)| node.data_is::<W>())
    }
    .ok_or(txt!("No widget of type [a]{}[] found", type_name::<W>()).build())
}

/// Iterator over a group of windows, that returns the window's index
fn window_index_widget<U: Ui>(
    (index, window): (usize, &Window<U>),
) -> impl ExactSizeIterator<Item = (usize, usize, &Node<U>)> + DoubleEndedIterator {
    window
        .nodes()
        .enumerate()
        .map(move |(i, entry)| (index, i, entry))
}

/// Iterates around a specific widget, going forwards
fn iter_around<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, usize, &Node<U>)> + '_ {
    let prev_len: usize = windows.iter().take(window).map(Window::len_widgets).sum();

    windows
        .iter()
        .enumerate()
        .skip(window)
        .flat_map(window_index_widget)
        .skip(widget + 1)
        .chain(
            windows
                .iter()
                .enumerate()
                .take(window + 1)
                .flat_map(window_index_widget)
                .take(prev_len + widget),
        )
}

/// Iterates around a specific widget, going backwards
fn iter_around_rev<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, usize, &Node<U>)> {
    let next_len: usize = windows.iter().skip(window).map(Window::len_widgets).sum();

    windows
        .iter()
        .enumerate()
        .rev()
        .skip(windows.len() - window)
        .flat_map(move |(i, win)| {
            window_index_widget((i, win))
                .rev()
                .skip(win.len_widgets() - widget)
        })
        .chain(
            windows
                .iter()
                .enumerate()
                .rev()
                .take(windows.len() - window)
                .flat_map(move |(i, win)| window_index_widget((i, win)).rev())
                .take(next_len - (widget + 1)),
        )
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
