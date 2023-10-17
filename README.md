# Parsec

Parsec is a work in progress for a text editor that is configured through the use of a rust crate, allowing for a massive amount of customizability.

## Roadmap

These are the goals that have been acomplished or are on their way:

- [x] Implement basic visual functionality (printing, scrolling, etc);
- [x] Implement wrapping;
- [x] Implement editing;
- [x] Create a kak mode;
- [x] Implement the use of multiple cursors;
- [x] Implement a history system;
- [x] Implement colors;
- [x] Implement widgets and designated areas;
- [x] Make all of these things easy to use on a public interface;
- [x] Create a number line and a separator line;
- [x] Create a status line;
- [x] File switching;
- [x] Create a command creation interface and a command line;
- [x] Add the ability to frame areas;
- [x] Implement concealment;
- [ ] Add the ability to create hooks;
- [ ] Add floating widgets, not tied to the session layout;
- [ ] Implement autocompletion lists;
- [ ] Create a "normal editing" mode;
- [ ] Create a vim mode;
- [ ] Create a more generalized plugin system;
- [ ] Create an LSP plugin;

︙

- [ ] Create a GTK or ICED frontend;

__NOTE:__ These are not set in stone, and may be done out of order.

## Trying it out

If you wish to try it out, assuming you have cargo installed, the recommended way is the one below:
```
git clone https://github.com/AhoyISki/parsec
cd parsec/example
cargo run --release <your file path here>
```
It will run the example crate, a basic, documented configuration that shows how to customize and extend parsec. When running the example crate in this manner, you will be pulling `parsec-core`, `parsec-term` and `parsec-kak` from the local repository, as opposed to crates.io. This is useful in order to keep up with the latest version of Parsec, updating as desired with `git pull`. While these three crates are in crates.io, the version in there won't be kept up to date as often as the ones in the repository.

## Motivation

This project was mostly created as a fun side project to occupy my time, and I was also unsatified with the current offerings of text editors on github.
# parsec-example
# parsec-example
# parsec-example
