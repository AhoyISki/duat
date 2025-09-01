# duat-core ![License: GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue) [![duat-core on crates.io](https://img.shields.io/crates/v/duat-core)](https://crates.io/crates/duat-core) [![duat-core on docs.rs](https://docs.rs/duat-core/badge.svg)](https://docs.rs/duat-core) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-core)

The core of Duat, this crate is meant to be used only for the
creation of plugins for Duat.

The capabilities of `duat-core` are largely the same as the those
of Duat, however, the main difference is the multi [`Ui`][__link0] APIs of
this crate. In it, the public functions and types are defined in
terms of `U: Ui`,  which means that they can work on various
different interfaces:

## Quick Start

This crate is composed of a few main modules, which will be used
in order to extend Duat:

* [`ui`][__link1]: Has everything to do with the interface of Duat, that
  includes things like:
  
  * [`Widget`][__link2]s: As the name implies, this is the trait for
    objects that will show up on the screen. The most noteworthy
    [`Widget`][__link3] is [`File`][__link4], which displays the contents of a file.
  * [`WidgetCfg`][__link5]s: These are [`Widget`][__link6] builders. They are used
    in the `setup` function of Duat’s config, through the
    [`WidgetCreated`][__link7] and [`WindowCreated`][__link8] [hook][__link9]s.
  * [`Ui`][__link10] and [`Area`][__link11]s: These are used if you want to create
    your own interface for Duat. Very much a work in progress, so
    I wouldn’t recommend trying that yet.
* [`text`][__link12]: Defines the struct used to show characters on screen
  
  * [`Text`][__link13]: Is everything that Duat shows on screen (except
    [`Ui`][__link14] specific decorations). This includes a UTF-8 string and
    tags to modify it.
  * [`Tag`][__link15]s: This is how Duat determines how [`Text`][__link16] will be
    displayed on screen. There are tags for styling, text
    alignment, spacing and all sorts of other things.
  * [`txt!`][__link17]: This macro, with syntax reminiscent of [`format!`][__link18]
    from Rust’s [`std`][__link19],  can be used to create [`Text`][__link20] through
    the [`text::Builder`][__link21] struct.
* [`mode`][__link22]: Defines how Duat will take input in order to control
  [`Widget`][__link23]s, includes things like:
  
  * [`Mode`][__link24]s: have the function [`send_key`][__link25], which
    takes a [key][__link26] and the current [widget][__link27] as input, and decides
    what to do with them. Only one [`Mode`][__link28] is active
    at any given time.
  * [`map`][__link29] and [`alias`][__link30]: These functions provide vim-style
    remapping on a given [`Mode`][__link31], also letting you
    switch modes on [key][__link32] sequences.
  * [`set`][__link33], [`set_default`][__link34], [`reset`][__link35]: These functions are used
    in order to switch [`Mode`][__link36] on demand. Do note
    that the switching is done asynchronously.
* [`hook`][__link37]: Provides utilities for hooking functions in Duat
  
  * [`Hookable`][__link38]: An event that you want to provide hooks for, in
    order to trigger functions whenever it takes place.
  * [`add`][__link39], [`add_grouped`][__link40], [`remove`][__link41]: These functions let you
    add or remove functions from [`Hookable`][__link42] events. Their
    arguments are statically determine by said [`Hookable`][__link43]s.
* [`cmd`][__link44]: Creation of commands in Duat, which can be called at
  runtime by the user.
  
  * [`add!`][__link45]: This macro lets you create a command, with one or
    more callers, and any number of [`Parameter`][__link46]s
  * [`Parameter`][__link47]: A command argument parsed from a string. There
    are a bunch of predefined [`Parameter`][__link48]s, and things like
    [`Vec<P>`][__link49] where `P: Parameter`, can also be as
    [`Parameter`][__link50]s, if you want multiple of the same kind.
  * [`call`][__link51], [`queue`][__link52], [`call_notify`][__link53], [`queue_and`][__link54], etc:
    functions to call or queue commands, which one should be used
    depends on the context of the function calling them.
* [`form`][__link55]: How to stylize [`Text`][__link56]
  
  * [`Form`][__link57]: Has many options on what [`Text`][__link58] should
    look like, are the same as those found on unix terminals.
  * [`set`][__link59], [`set_weak`][__link60]: These functions let you set
    forms with a name. They can be set to a [`Form`][__link61]
    or reference another name of a form.
  * [`ColorScheme`][__link62]s: These are general purpose
    [`Form`][__link63] setters, with a name that can be called
    from the `colorscheme` [command][__link64]

These are the elements available to you if you want to extend
Duat. Additionally, there are some other things that have been
left out, but they are available in the [`prelude`][__link65], so you can
just import it:

```rust
// Usually at the top of the crate, below `//!` comments:
use duat_core::prelude::*;
```

## How to extend Duat

Duat is extended primarily through the use of [`Plugin`][__link66]s from
external crates, these will be plugged in the main config through
the [`plug`][__link67] function, and are modified in place through the
builder pattern.

By default, Duat includes the [`MatchPairs`][__link68] and [`Treesitter`][__link69]
plugins. The former highlightin the matching pair to the cursor,
and the latter parsing the text into a syntax tree that is used
for syntax highlighting, indentation, and much more.

For this demonstration, I will create a [`Plugin`][__link70] that keeps
track of the word count in a [`File`][__link71], without counting the words
every time said [`File`][__link72] changes.

### Creating a [`Plugin`][__link73]

First of all, assuming that you have succeeded in following the
[installation instructions of duat][__link74], you should create a crate
with `duat --init-plugin`:

```bash
duat --init-plugin duat-word-count
```

This should create a directory named `"duat-word-count"`, where
the `WordCount` plugin is defined. There is some example
boilerplate, removing it, you should get something like this:

```rust
// In duat-word-count/src/lib.rs
use duat_core::prelude::*;

/// A [`Plugin`] to count the number of words in [`File`]s
#[derive(Default)]
pub struct WordCount;

impl<U: Ui> Plugin<U> for WordCount {
    fn plug(self, plugins: &Plugins<U>) {
        todo!();
    }
}
```

The [`Plugins`][__link75] struct can be used to require that other
[`Plugin`][__link76]s be added to Duat as well. We won’t use it in this
example.

In the code above, `WordCount` is a plugin that can be included in
Duat’s `config` crate. It will give the user the ability to get
how many words are in a [`File`][__link77], without having to reparse the
whole buffer every time, given that it could be a very large file.
In order to configure the [`Plugin`][__link78], you should make use of the
builder pattern, returning the [`Plugin`][__link79] on every modification.

```rust
use duat_core::prelude::*;

/// A [`Plugin`] to count the number of words in [`File`]s
#[derive(Default)]
pub struct WordCount(bool);

impl WordCount {
    /// Returns a new instance of the [`WordCount`] plugin
    pub fn new() -> Self {
        WordCount(false)
    }

    /// Count everything that isn't whitespace as a word character
    pub fn not_whitespace(self) -> Self {
        WordCount(true)
    }
}

impl<U: Ui> Plugin<U> for WordCount {
    fn plug(self, _: &Plugins<U>) {
        todo!();
    }
}
```

Now, there is an option to exclude only whitespace, not just
including regular alphanumeric characters. This would count, for
example “x(x^3 + 3)” as 3 words, rather than 4.

Next, I need to add something to keep track of the number of words
in a [`File`][__link80]. For [`File`][__link81]s specifically, there is a built-in way
to keep track of changes through the [`Parser`][__link82] trait:

```rust
use duat_core::prelude::*;

/// A [`Parser`] to keep track of words in a [`File`]
struct WordCounter {
    tracker: FileTracker,
    words: usize,
    regex: &'static str,
}

impl<U: Ui> Parser<U> for WordCounter {
    fn parse(&mut self) -> bool {
        todo!();
    }
}
```

There are a few things to unpack here:

* The `WordCounter` has a [`FileTracker`][__link83] struct within: This
  tracker is acquired when constructing the [`Parser`][__link84], and it can
  track [`Change`][__link85]s to the [`File`][__link86]. It does this by having its
  own copy of the [`File`][__link87]s [`Bytes`][__link88], which it updates as
  requested, allowing parsers to work in different threads.
* The [`Parser::parse`][__link89] function: This function is called every
  time the [`File`][__link90] is updated, and a return value of `true` means
  that this [`Parser`][__link91] wants to update the [`File`][__link92] itself, which
  will call [`Parser::update`][__link93]. I won’t be using this feature, so
  I can just return `false`.

This is the basic layout for a [`Parser`][__link94] that doesn’t need to
modify the [`File`][__link95] itself, which is our case.

Next, I’ll make use of duat’s [`Text`][__link96] API in order to figure out
the word count difference given a [`Change`][__link97] to the text:

```rust
use duat_core::{prelude::*, text::Change};

fn word_diff(regex: &str, bytes: &Bytes, change: Change<&str>) -> i32 {
    // The starting and ending points of the lines where the Change
    // took place.
    let [start, _] = bytes.points_of_line(change.start().line());
    let [_, end] = bytes.points_of_line(change.added_end().line());

    // Recreate the lines as they were before the change
    // behind_change is just the part of the line before the point
    // where a change starts.
    // ahead_of_change is the part of the line after the end of
    // the Change
    let behind_change = bytes.strs(start..change.start()).unwrap().to_string();
    let ahead_of_change = bytes.strs(change.added_end()..end).unwrap();
    // change.taken_str() is the &str that was taken by the Change
    let taken_by_change = change.taken_str();
    // By adding these three together, I now have a string for what
    // the lines looked like before the Change took place:
    let lines_before = format!("{behind_change}{taken_by_change}{ahead_of_change}");

    // Here, I'm just counting the number of occurances of the
    // regex in the lines before and after the change.
    let words_before = lines_before.search_fwd(regex, ..).unwrap().count();
    let words_after = bytes.search_fwd(regex, start..end).unwrap().count();

    words_after as i32 - words_before as i32
}
```

In this method, I am calculating the difference between the number
of words in the line before and after the [`Change`][__link98] took place.
Here [`Bytes::points_of_line`][__link99] returns the [`Point`][__link100]s where a
line starts and ends. I know there are better ways to do this by
comparing the text that [was taken][__link101] to [what was added][__link102],
with the context of the lines of the change, but this is
just a demonstration, and the more efficient method is left as an
exercise to the viewer 😉.

Now, just call this on [`parse`][__link103]:

```rust
use duat_core::{prelude::*, text::Change};

/// A [`Parser`] to keep track of words in a [`File`]
struct WordCounter {
    tracker: FileTracker,
    words: usize,
    regex: &'static str,
}

impl<U: Ui> Parser<U> for WordCounter {
    fn parse(&mut self) -> bool {
        // Fetches the latest updates from the File
        self.tracker.update();

        // Rust iterators are magic 🪄
        let diff: i32 = self
            .tracker
            .moment()
            .changes()
            .map(|change| word_diff(self.regex, &self.tracker.bytes(), change))
            .sum();

        self.words = (self.words as i32 + diff) as usize;

        // We don't care about updating the File, so just return false.
        false
    }
}
```

You’ll notice that the [`FileTracker`][__link104] has a
[`FileTracker::moment`][__link105] method. This method returns a [`Moment`][__link106],
which is a list of all the [`Change`][__link107]s that took place since the
previous call to [`FileTracker::update`][__link108]. By iterating through
these changes, the [`Parser`][__link109] can keep up with every [`Change`][__link110]
that takes place in the [`File`][__link111].

And that’s it for the [`Parser`][__link112] implementation! Now, how do we
add it to a [`File`][__link113]?

In order to add this [`Parser`][__link114] to a [`File`][__link115], we’re going to
need a [`ParserCfg`][__link116], which is used for configuring [`Parser`][__link117]s
before they are added:

```rust
use duat_core::prelude::*;

struct WordCounterCfg(bool);

impl<U: Ui> ParserCfg<U> for WordCounterCfg {
    type Parser = WordCounter;

    fn build(self, file: &File<U>, tracker: FileTracker) -> Result<Self::Parser, Text> {
        let regex = if self.0 { r"\S+" } else { r"\w+" };
        let words = file.bytes().search_fwd(regex, ..).unwrap().count();

        Ok(WordCounter { tracker, words, regex })
    }
}
```

In this function, I am returning the `WordCounter` with a
precalculated number of words (since I have to calculate this
value at some point), based on the current state of the [`File`][__link118].

This is the point where you are given the [`FileTracker`][__link119], which a
[`Parser`][__link120] can use to track that specific [`File`][__link121]. This
[`Plugin`][__link122] is a relatively simple example, but the [`FileTracker`][__link123]
is really useful for Duat to inform plugin writers when they
actually need to update some part of the [`File`][__link124]’s [`Text`][__link125], in
order to prevent inefficient updates to the text.

Now, to wrap this all up, the plugin needs to add this [`Parser`][__link126]
to every [`File`][__link127]. We do this through the use of a [hook][__link128]:

```rust
use duat_core::prelude::*;

/// A [`Plugin`] to count the number of words in [`File`]s
#[derive(Default)]
pub struct WordCount(bool);

impl WordCount {
    /// Returns a new instance of the [`WordCount`] plugin
    pub fn new() -> Self {
        WordCount(false)
    }

    /// Count everything that isn't whitespace as a word character
    pub fn not_whitespace(self) -> Self {
        WordCount(true)
    }
}

impl<U: Ui> Plugin<U> for WordCount {
    fn plug(self, _: &Plugins<U>) {
        let not_whitespace = self.0;

        hook::add::<File<U>, U>(move |pa, (mut cfg, _)| {
            cfg.with_parser(WordCounterCfg(not_whitespace))
        });
    }
}
```

Now, whenever a [`File`][__link129] is opened, this [`Parser`][__link130] will be added
to it. This is just one out of many types of [hook][__link131] that Duat
provides by default. In Duat, you can even [create your own][__link132], and
[choose when to trigger them][__link133].

However, while we have added the [`Parser`][__link134], how is the user
supposed to access this value? Well, one convenient way to do this
is through a simple function:

```rust
use duat_core::prelude::*;

/// The number of words in a [`File`]
pub fn file_words<U: Ui>(file: &File<U>) -> usize {
    file.read_parser(|word_counter: &WordCounter| word_counter.words)
        .unwrap_or(0)
}
```

Now, we have a finished plugin:

```rust
use duat_core::{prelude::*, text::Change};

/// A [`Plugin`] to count the number of words in [`File`]s
#[derive(Default)]
pub struct WordCount(bool);

impl WordCount {
    /// Returns a new instance of [`WordCount`]
    pub fn new() -> Self {
        WordCount(false)
    }

    /// Count everything that isn't whitespace as a word character
    pub fn not_whitespace(self) -> Self {
        WordCount(true)
    }
}

impl<U: Ui> Plugin<U> for WordCount {
    fn plug(self, _: &Plugins<U>) {
        let not_whitespace = self.0;
        hook::add::<File<U>, U>(move |_, (mut cfg, _)| {
            cfg.with_parser(WordCounterCfg(not_whitespace))
        });
    }
}

/// The number of words in a [`File`]
pub fn file_words<U: Ui>(file: &File<U>) -> usize {
    file.read_parser(|word_counter: &WordCounter| word_counter.words)
        .unwrap_or(0)
}

/// A [`Parser`] to keep track of words in a [`File`]
struct WordCounter {
    tracker: FileTracker,
    words: usize,
    regex: &'static str,
}

impl<U: Ui> Parser<U> for WordCounter {
    fn parse(&mut self) -> bool {
        self.tracker.update();

        let diff = |change| word_diff(self.regex, &self.tracker.bytes(), change);
        let diff: i32 = self.tracker.moment().changes().map(diff).sum();
        self.words = (self.words as i32 + diff) as usize;

        false
    }
}

#[derive(Default)]
struct WordCounterCfg(bool);

impl<U: Ui> ParserCfg<U> for WordCounterCfg {
    type Parser = WordCounter;

    fn build(self, file: &File<U>, tracker: FileTracker) -> Result<Self::Parser, Text> {
        let regex = if self.0 { r"\S+" } else { r"\w+" };
        let words = file.bytes().search_fwd(regex, ..).unwrap().count();

        Ok(WordCounter { tracker, words, regex })
    }
}

fn word_diff(regex: &str, bytes: &Bytes, change: Change<&str>) -> i32 {
    let [start, _] = bytes.points_of_line(change.start().line());
    let [_, end] = bytes.points_of_line(change.added_end().line());

    let behind_change = bytes.strs(start..change.start()).unwrap().to_string();
    let ahead_of_change = bytes.strs(change.added_end()..end).unwrap();
    let taken_by_change = change.taken_str();
    let lines_before = format!("{behind_change}{taken_by_change}{ahead_of_change}");

    let words_before = lines_before.search_fwd(regex, ..).unwrap().count();
    let words_after = bytes.search_fwd(regex, start..end).unwrap().count();

    words_after as i32 - words_before as i32
}
```

Once you’re done modifying your plugin, you should be ready to
publish it to [crates.io][__link135]. This is the common registry for
packages (crates in Rust), and is also where Duat will pull
plugins from. Before publishing, try to follow [these guidelines][__link136]
in order to improve the usability of the plugin. Now, you should
be able to just do this in the `duat-word-count` directory:

```bash
cargo publish
```

Ok, it’s published, but how does one use it?

### Using plugins

Assuming that you’ve already [installed duat][__link137], you should have a
config crate in `~/.config/duat` (or `$XDG_CONFIG_HOME/duat`), in
it, you can call the following command:

```bash
cargo add duat-word-count@"*" --rename word-count
```

Then, in `src/lib.rs`, you can add the following:

```rust
setup_duat!(setup);
use duat::prelude::*;
use word_count::*;

fn setup() {
    plug(WordCount::new().not_whitespace());

    hook::add::<StatusLine<Ui>>(|pa, (sl, _)| {
        sl.fmt(status!(
            "{name_txt} has [wc]{file_words}[] words{Spacer}{mode_txt} {sels_txt} {main_txt}"
        ))
    });
}
```

Now, the default [`StatusLine`][__link138] should have word count added in,
alongside the other usual things in there. It’s been added in the
`{file_words}` part of the string, which just interpolated that
function, imported by `use word_count::*;`, into the status line.

There are many other things that plugins can do, like create
custom [`Widget`][__link139]s, [`Mode`][__link140]s that can change how Duat
behaves, customized [commands][__link141] and [hook][__link142]s, and many such things


# Plugin examples

## `duat-sneak`

![sneak](../assets/sneak-demonstration.gif)

[`duat-sneak`], inspired by [`vim-sneak`], lets you traverse the
screen by searching through character sequences.

[`duat-sneak`]: https://github.com/AhoyISki/duat-sneak
[`vim-sneak`]: https://github.com/justinmk/vim-sneak
 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEGy_bXjvdZUMkGwCrwWRAzyECGzH81m5BV9E_G9FxROuvZ7aRYXKEG2jLwVGrhutIG0ZvoEAutd2eG6Xu8_beodzmGyN5y5nlltBzYWSBg2lkdWF0LWNvcmVlMC42LjBpZHVhdF9jb3Jl
 [__link0]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Ui
 [__link1]: https://docs.rs/duat-core/0.6.0/duat_core/ui/index.html
 [__link10]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Ui
 [__link100]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Point
 [__link101]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change::taken_str
 [__link102]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change::added_str
 [__link103]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser::parse
 [__link104]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::FileTracker
 [__link105]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::FileTracker::moment
 [__link106]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Moment
 [__link107]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change
 [__link108]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::FileTracker::update
 [__link109]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link11]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Area
 [__link110]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change
 [__link111]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link112]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link113]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link114]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link115]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link116]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::ParserCfg
 [__link117]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link118]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link119]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::FileTracker
 [__link12]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link120]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link121]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link122]: https://docs.rs/duat-core/0.6.0/duat_core/trait.Plugin.html
 [__link123]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::FileTracker
 [__link124]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link125]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link126]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link127]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link128]: https://docs.rs/duat-core/0.6.0/duat_core/hook/index.html
 [__link129]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link13]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link130]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link131]: https://docs.rs/duat-core/0.6.0/duat_core/hook/index.html
 [__link132]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::Hookable
 [__link133]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::trigger
 [__link134]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link135]: https://crates.io
 [__link136]: https://doc.rust-lang.org/book/ch14-02-publishing-to-crates-io.html
 [__link137]: https://github.com/AhoyISki/duat?tab=readme-ov-file#getting-started
 [__link138]: https://docs.rs/duat/latest/duat/prelude/macro.status.html
 [__link139]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Widget
 [__link14]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Ui
 [__link140]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::Mode
 [__link141]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd
 [__link142]: https://docs.rs/duat-core/0.6.0/duat_core/hook/index.html
 [__link15]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Tag
 [__link16]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link17]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::txt
 [__link18]: https://doc.rust-lang.org/stable/std/macro.format.html
 [__link19]: https://doc.rust-lang.org/stable/std
 [__link2]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Widget
 [__link20]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link21]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Builder
 [__link22]: https://docs.rs/duat-core/0.6.0/duat_core/mode/index.html
 [__link23]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Widget
 [__link24]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::Mode
 [__link25]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::Mode::send_key
 [__link26]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::KeyEvent
 [__link27]: https://docs.rs/duat-core/0.6.0/duat_core/?search=context::Handle
 [__link28]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::Mode
 [__link29]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::map
 [__link3]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Widget
 [__link30]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::alias
 [__link31]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::Mode
 [__link32]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::KeyEvent
 [__link33]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::set
 [__link34]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::set_default
 [__link35]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::reset
 [__link36]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::Mode
 [__link37]: https://docs.rs/duat-core/0.6.0/duat_core/hook/index.html
 [__link38]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::Hookable
 [__link39]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::add
 [__link4]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link40]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::add_grouped
 [__link41]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::remove
 [__link42]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::Hookable
 [__link43]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::Hookable
 [__link44]: https://docs.rs/duat-core/0.6.0/duat_core/cmd/index.html
 [__link45]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::add
 [__link46]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::Parameter
 [__link47]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::Parameter
 [__link48]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::Parameter
 [__link49]: https://doc.rust-lang.org/stable/std/vec/struct.Vec.html
 [__link5]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::WidgetCfg
 [__link50]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::Parameter
 [__link51]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::call
 [__link52]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::queue
 [__link53]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::call_notify
 [__link54]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::queue_and
 [__link55]: https://docs.rs/duat-core/0.6.0/duat_core/form/index.html
 [__link56]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link57]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::Form
 [__link58]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link59]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::set
 [__link6]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Widget
 [__link60]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::set_weak
 [__link61]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::Form
 [__link62]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::ColorScheme
 [__link63]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::Form
 [__link64]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd
 [__link65]: https://docs.rs/duat-core/0.6.0/duat_core/prelude/index.html
 [__link66]: https://docs.rs/duat-core/0.6.0/duat_core/trait.Plugin.html
 [__link67]: https://docs.rs/duat/latest/duat/prelude/function.plug.html
 [__link68]: https://docs.rs/duat-match-pairs/latest/duat_match_pairs/struct.MatchPairs.html
 [__link69]: https://docs.rs/duat-tree-sitter/latest/duat_tree_sitter/struct.Treesitter.html
 [__link7]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::WidgetCreated
 [__link70]: https://docs.rs/duat-core/0.6.0/duat_core/trait.Plugin.html
 [__link71]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link72]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link73]: https://docs.rs/duat-core/0.6.0/duat_core/trait.Plugin.html
 [__link74]: https://github.com/AhoyISki/duat?tab=readme-ov-file#getting-started
 [__link75]: https://docs.rs/duat-core/0.6.0/duat_core/struct.Plugins.html
 [__link76]: https://docs.rs/duat-core/0.6.0/duat_core/trait.Plugin.html
 [__link77]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link78]: https://docs.rs/duat-core/0.6.0/duat_core/trait.Plugin.html
 [__link79]: https://docs.rs/duat-core/0.6.0/duat_core/trait.Plugin.html
 [__link8]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::WindowCreated
 [__link80]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link81]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link82]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link83]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::FileTracker
 [__link84]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link85]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change
 [__link86]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link87]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link88]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Bytes
 [__link89]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser::parse
 [__link9]: https://docs.rs/duat-core/0.6.0/duat_core/hook/index.html
 [__link90]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link91]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link92]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link93]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser::update
 [__link94]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link95]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link96]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link97]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change
 [__link98]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change
 [__link99]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Bytes::points_of_line
