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
    `Widget` is the [`File`][__link3], which displays the contents of a
    file.
  * [`WidgetCfg`][__link4]s: These are `Widget` builders. They are used in
    the `setup` function of Duat’s config, through the
    `WidgetCreated` and `WindowCreated` hooks.
  * [`Ui`][__link5] and [`Area`][__link6]s: These are used if you want to create
    your own interface for Duat. Very much a work in progress, so
    I wouldn’t recommend trying that yet.
* [`text`][__link7]: Defines the struct used to show characters on screen
  
  * [`Text`][__link8]: Is everything that Duat shows on screen (except
    [`Ui`][__link9] specific decorations). This includes a UTF-8 string and
    tags to modify it.
  * [`Tag`][__link10]s: This is how Duat determines how `Text` will be
    displayed on screen. There are tags for styling, text
    alignment, spacing and all sorts of other things.
  * [`txt!`][__link11]: This macro, with syntax reminiscent of [`format!`][__link12]
    from Rust’s `std`, can be used to declaratively construct a
    `Text`.
* [`mode`][__link13]: Defines how Duat will take input in order to control
  `Widget`s, includes things like:
  
  * [`Mode`][__link14]s: have the function `send_key`, which
    takes a key and the current widget as input, and decides what
    to do with them. Only one `Mode` is active at any given time.
  * [`map`][__link15] and [`alias`][__link16]: These functions provide vim-style
    remapping on a given `Mode`, also letting you   switch modes
    on key sequences.
  * [`set`][__link17], [`set_default`][__link18], [`reset`][__link19]: These functions are used
    in order to switch `Mode` on demand. Do note that the
    switching is done asynchronously.
* [`hook`][__link20]: Provides utilities for hooking functions in Duat
  
  * [`Hookable`][__link21]: An event that you want to provide hooks for, in
    order to trigger functions whenever it takes place.
  * [`add`][__link22], [`add_grouped`][__link23], [`remove`][__link24]: These functions let you
    add or remove functions from `Hookable` events. Their
    arguments are statically determine by said `Hookable`s.
* [`cmd`][__link25]: Creation of commands in Duat, which can be called at
  runtime by the user.
  
  * [`add!`][__link26]: This macro lets you create a command, with one or
    more callers, and any number of `Parameter`s
  * [`Parameter`][__link27]: A command argument parsed from a string. There
    are a bunch of predefined `Parameter`s, and things like
    `Vec<P>` where `P: Parameter`, can also be as `Parameter`s, if
    you want multiple of the same kind.
  * [`call`][__link28], [`queue`][__link29], [`queue_and`][__link30], etc: functions to call or
    queue commands, which one should be used depends on the
    context of the function calling them.
* [`form`][__link31]: How to stylize `Text`
  
  * [`Form`][__link32]: Has many options on what `Text` should
    look like, are the same as those found on unix terminals.
  * [`set`][__link33], [`set_weak`][__link34]: These functions let you set
    forms with a name. They can be set to a `Form` or reference
    another name of a form.
  * [`ColorScheme`][__link35]s: These are general purpose `Form` setters,
    with a name that can be called from the `colorscheme` command.

These are the elements available to you if you want to extend
Duat. Additionally, there are some other things that have been
left out, but they are available in the [`prelude`][__link36], so you can
just import it:

```rust
// Usually at the top of the crate, below `//!` comments:
use duat_core::prelude::*;
```

## How to extend Duat

Duat is extended primarily through the use of [`Plugin`][__link37]s from
external crates, these will be plugged in the main config through
the [`plug`][__link38] function, and are modified in place through the
builder pattern.

By default, Duat includes the `MatchPairs` and `Treesitter`
plugins. The former highlightin the matching pair to the cursor,
and the latter parsing the text into a syntax tree that is used
for syntax highlighting, indentation, and much more.

For this demonstration, I will create a `Plugin` that keeps
track of the word count in a `File`, without counting the words
every time said `File` changes.

### Creating a `Plugin`

First of all, assuming that you have succeeded in following the
[installation instructions of duat][__link39], you should create a crate
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

The [`Plugins`][__link40] struct can be used to require that other
`Plugin`s be added to Duat as well. We won’t use it in this
example.

In the code above, `WordCount` is a plugin that can be included in
Duat’s `config` crate. It will give the user the ability to get
how many words are in a `File`, without having to reparse the
whole buffer every time, given that it could be a very large file.
In order to configure the `Plugin`, you should make use of the
builder pattern, returning the `Plugin` on every modification.

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
in a `File`. For `File`s specifically, there is a built-in way to
keep track of changes through the [`Parser`][__link41] trait:

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

* The `WordCounter` has a [`FileTracker`][__link42] struct within: This
  tracker is acquired when constructing the `Parser`, and it can
  track [`Change`][__link43]s to the `File`. It does this by having its own
  copy of the `File`s [`Bytes`][__link44], which it updates as requested,
  allowing parsers to work in different threads.
* The [`Parser::parse`][__link45] function: This function is called every
  time the `File` is updated, and a return value of `true` means
  that this `Parser` wants to update the `File` itself, which will
  call [`Parser::update`][__link46]. I won’t be using this feature, so I can
  just return `false`.

This is the basic layout for a `Parser` that doesn’t need to
modify the `File` itself, which is our case.

Next, I’ll make use of duat’s `Text` API in order to figure out
the word count difference given a `Change` to the text:

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
of words in the line before and after the `Change` took place.
Here `Bytes::points_of_line` returns the [`Point`][__link47]s where a
line starts and ends. I know there are better ways to do this by
comparing the text that [was taken][__link48] to [what was added][__link49],
with the context of the lines of the change, but this is
just a demonstration, and the more efficient method is left as an
exercise to the viewer 😉.

Now, just call this on `Parser::parse`:

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

You’ll notice that the `FileTracker` has a
[`FileTracker::moment`][__link50] method. This method returns a [`Moment`][__link51],
which is a list of all the `Change`s that took place since the
previous call to `FileTracker::update`. By iterating through these
changes, the `Parser` can keep up with every `Change` that takes
place in the `File`.

And that’s it for the `Parser` implementation! Now, how do we add
it to a `File`?

In order to add this `Parser` to a `File`, we’re going to need a
[`ParserCfg`][__link52], which is used for configuring `Parser`s before they
are added:

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
value at some point), based on the current state of the `File`.

This is the point where you are given the `FileTracker`, which a
`Parser` can use to track that specific `File`. This
`Plugin` is a relatively simple example, but the `FileTracker`
is really useful for Duat to inform plugin writers when they
actually need to update some part of the `File`’s `Text`, in
order to prevent inefficient updates to the text.

Now, to wrap this all up, the plugin needs to add this `Parser`
to every `File`. We do this through the use of a hook:

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

Now, whenever a `File` is opened, this `Parser` will be added
to it. This is just one out of many types of hook that Duat
provides by default. In Duat, you can even [create your own][__link53], and
[choose when to trigger them][__link54].

However, while we have added the `Parser`, how is the user
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
publish it to [crates.io][__link55]. This is the common registry for
packages (crates in Rust), and is also where Duat will pull
plugins from. Before publishing, try to follow [these guidelines][__link56]
in order to improve the usability of the plugin. Now, you should
be able to just do this in the `duat-word-count` directory:

```bash
cargo publish
```

Ok, it’s published, but how does one use it?

### Using plugins

Assuming that you’ve already [installed duat][__link57], you should have a
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

Now, the default [`StatusLine`][__link58] should have word count added in,
alongside the other usual things in there. It’s been added in the
`{file_words}` part of the string, which just interpolated that
function, imported by `use word_count::*;`, into the status line.


# Plugin examples

## `duat-sneak`

![sneak](../assets/sneak-demonstration.gif)

[`duat-sneak`], inspired by [`vim-sneak`], lets you traverse the
screen by searching through character sequences.

[`duat-sneak`]: https://github.com/AhoyISki/duat-sneak
[`vim-sneak`]: https://github.com/justinmk/vim-sneak
 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEGy_bXjvdZUMkGwCrwWRAzyECGzH81m5BV9E_G9FxROuvZ7aRYXKEG0RKkt7p0Je4GysXVLZq3JbbG0IgSq_OzvVhG_DdvXtC_8oqYWSBg2lkdWF0LWNvcmVlMC42LjBpZHVhdF9jb3Jl
 [__link0]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Ui
 [__link1]: https://docs.rs/duat-core/0.6.0/duat_core/ui/index.html
 [__link10]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Tag
 [__link11]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::txt
 [__link12]: https://doc.rust-lang.org/stable/std/macro.format.html
 [__link13]: https://docs.rs/duat-core/0.6.0/duat_core/mode/index.html
 [__link14]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::Mode
 [__link15]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::map
 [__link16]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::alias
 [__link17]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::set
 [__link18]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::set_default
 [__link19]: https://docs.rs/duat-core/0.6.0/duat_core/?search=mode::reset
 [__link2]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Widget
 [__link20]: https://docs.rs/duat-core/0.6.0/duat_core/hook/index.html
 [__link21]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::Hookable
 [__link22]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::add
 [__link23]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::add_grouped
 [__link24]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::remove
 [__link25]: https://docs.rs/duat-core/0.6.0/duat_core/cmd/index.html
 [__link26]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::add
 [__link27]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::Parameter
 [__link28]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::call
 [__link29]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::queue
 [__link3]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::File
 [__link30]: https://docs.rs/duat-core/0.6.0/duat_core/?search=cmd::queue_and
 [__link31]: https://docs.rs/duat-core/0.6.0/duat_core/form/index.html
 [__link32]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::Form
 [__link33]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::set
 [__link34]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::set_weak
 [__link35]: https://docs.rs/duat-core/0.6.0/duat_core/?search=form::ColorScheme
 [__link36]: https://docs.rs/duat-core/0.6.0/duat_core/prelude/index.html
 [__link37]: https://docs.rs/duat-core/0.6.0/duat_core/trait.Plugin.html
 [__link38]: https://docs.rs/duat/latest/duat/prelude/function.plug.html
 [__link39]: https://github.com/AhoyISki/duat?tab=readme-ov-file#getting-started
 [__link4]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::WidgetCfg
 [__link40]: https://docs.rs/duat-core/0.6.0/duat_core/struct.Plugins.html
 [__link41]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser
 [__link42]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::FileTracker
 [__link43]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change
 [__link44]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Bytes
 [__link45]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser::parse
 [__link46]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::Parser::update
 [__link47]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Point
 [__link48]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change::taken_str
 [__link49]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Change::added_str
 [__link5]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Ui
 [__link50]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::FileTracker::moment
 [__link51]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Moment
 [__link52]: https://docs.rs/duat-core/0.6.0/duat_core/?search=file::ParserCfg
 [__link53]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::Hookable
 [__link54]: https://docs.rs/duat-core/0.6.0/duat_core/?search=hook::trigger
 [__link55]: https://crates.io
 [__link56]: https://doc.rust-lang.org/book/ch14-02-publishing-to-crates-io.html
 [__link57]: https://github.com/AhoyISki/duat?tab=readme-ov-file#getting-started
 [__link58]: https://docs.rs/duat/latest/duat/prelude/macro.status.html
 [__link6]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Area
 [__link7]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link8]: https://docs.rs/duat-core/0.6.0/duat_core/?search=text::Text
 [__link9]: https://docs.rs/duat-core/0.6.0/duat_core/?search=ui::Ui
