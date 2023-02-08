# Parsec

Parsec is a work in progress for a text editor that is configured through the use of a rust crate, allowing for a massive amount of customizability.

## DO NOT USE PARSEC YET

It is very much in pre-alpha stage right now, and much of the functionality is yet to come, so using it
right now is mostly just a waste of time.

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
- [ ] Create a command creation interface and a command line;
- [ ] Add the ability to create hooks;
- [ ] Implement folding;
- [ ] Add floating widgets, not tied to the session layout;
- [ ] Implement autocompletion lists;
- [ ] Create a "normal editing" mode;
- [ ] Create a vim mode;
- [ ] Create a more generalized plugin system;
- [ ] Create an LSP plugin;
...
- [ ] Create a GTK or ICED frontend;

__NOTE:__ These are not set in stone, and may be done out of order.

## Trying it out

If you wish to, you can copy the example folder's content into your computer and compile it. It is a very standard configuration for Parsec, using the kakoune editing mode. It is also annotated so you can tell what is going on, and how to further configure it.

## Motivation

This project was mostly created as a fun side project to occupy my time, and I was also unsatified with the current offerings of text editors on github.
