//! This module basically only exists to reduce the burden
//! on rust-analyzer on other places.
use duat_core::{
    alt, ctrl,
    mode::{Bindings, KeyCode::*, bindings},
    shift, txt, unmod,
};

/// The bindings for the [`Normal`] mode.
///
/// [`Normal`]: crate::Normal
pub fn normal_bindings() -> Bindings {
    let word = txt!("[a]word[separator],[a]WORD");
    let select = txt!("[a]Select[separator],[a]extend");
    let below = txt!("[a]below[separator],[a]above");
    let ahead = txt!("[a]ahead[separator],[a]behind");
    let undo = txt!("[a]Undo[separator],[a]redo");

    let object = move |action: &str| Bindings {
        title: Some(txt!("{action}")),
        ..bindings!(match _ {
            unmod!('b' | '(' | ')') => txt!("parenthesis block"),
            unmod!('B' | '{' | '}') => txt!("brace block"),
            unmod!('r' | '[' | ']') => txt!("bracket block"),
            unmod!('a' | '<' | '>') => txt!("angle bracket block"),
            unmod!('Q' | '"') => txt!("double quote string"),
            unmod!('q' | '\'') => txt!("single quote string"),
            unmod!('g' | '`') => txt!("grave quote string"),
            unmod!('w') | unmod!('e') => word.clone(),
            unmod!('s') => txt!("sentence"),
            unmod!('p') => txt!("paragraph"),
            unmod!(' ') => txt!("whitespace"),
            unmod!('i') => txt!("indent"),
            unmod!('u') => txt!("argument"),
        })
    };

    let r#match = {
        let object = object.clone();
        move |action: &str| Bindings {
            title: Some(txt!("{action} match")),
            ..bindings!(match _ {
                unmod!('l' | 'm') => txt!("next pair"),
                unmod!('M') => txt!("extend to next pair"),
                unmod!('h') | alt!('m') => txt!("prev pair"),
                alt!('M') => txt!("extend to prev pair"),
                unmod!('i') => (
                    txt!("inside object"),
                    object(&format!("{action} inside object"))
                ),
                unmod!('a') => (
                    txt!("around object"),
                    object(&format!("{action} around object"))
                ),
                unmod!('s') => (txt!("surround selection"), Bindings {
                    title: Some(txt!("surround with")),
                    ..bindings!(match _ {
                        unmod!('b' | '(' | ')') => txt!("parenthesis"),
                        unmod!('B' | '{' | '}') => txt!("braces"),
                        unmod!('r' | '[' | ']') => txt!("brackets"),
                        unmod!('a' | '<' | '>') => txt!("angle brackets"),
                        unmod!('Q' | '"') => txt!("double quotes"),
                        unmod!('q' | '\'') => txt!("single quotes"),
                        unmod!('g' | '`') => txt!("grave quotes"),
                        unmod!('t') => txt!("html tag"),
                        unmod!(Char(..)) => txt!("another character"),
                    })
                }),
            })
        }
    };

    let goto = Bindings {
        title: Some(txt!("goto")),
        ..bindings!(match _ {
            unmod!('j' | 'e') => txt!("end of [a]Buffer"),
            unmod!('k' | 'g') => txt!("start of [a]Buffer"),
            unmod!('h') => txt!("start of line"),
            unmod!('l') => txt!("end of line"),
            unmod!('i') => txt!("first character in line"),
            unmod!('a') => txt!("last swapped [a]Buffer"),
            unmod!('n') => txt!("next [a]Buffer"),
            unmod!('N') => txt!("prev [a]Buffer"),
            unmod!('o') => txt!("move [a]Buffer[] to new window"),
        })
    };

    let rotate = |direction: &str| Bindings {
        title: Some(txt!("Rotate {direction}")),
        ..bindings!(match _ {
            unmod!('s') => txt!("rotate selection"),
            unmod!('c') => txt!("rotate content"),
        })
    };

    let mut tf = {
        let words = [
            ["select", "to", "next"],
            ["extend", "to", "next"],
            ["select", "until", "next"],
            ["extend", "until", "next"],
            ["select", "to", "prev"],
            ["extend", "to", "prev"],
            ["select", "until", "prev"],
            ["extend", "until", "prev"],
        ];

        words
            .map(|[sel, to, next]| Bindings {
                title: Some(txt!("{sel} {to} {next}")),
                ..bindings!(match _ {
                    unmod!(Char(..)) => txt!("[key.char]{{char}}[] to {sel} {to}"),
                })
            })
            .into_iter()
    };

    let mut obj = {
        let words = [
            ["select", "to", "start"],
            ["select", "to", "end"],
            ["extend", "to", "start"],
            ["extend", "to", "end"],
            ["select", "until", "start"],
            ["select", "until", "end"],
            ["extend", "until", "start"],
            ["extend", "until", "end"],
        ];
        words
            .map(|[sel, to, dir]| object(format!("{sel} {to} whole object {dir}").as_str()))
            .into_iter()
    };

    bindings!(match _ {
        unmod!('h' | 'j' | 'k' | 'l') => txt!("Move cursor"),
        unmod!('H' | 'J' | 'K' | 'L') => txt!("Select and move cursor"),
        unmod!(Left | Up | Down | Right) => txt!("Move cursor wrapped"),
        shift!(Left | Up | Down | Right) => txt!("Select and move cursor wrapped"),
        unmod!('w' | 'W') => txt!("{select} to end of [a]word[]/space"),
        unmod!('e' | 'E') => txt!("{select} to end of [a]WORD[]/space"),
        unmod!('b' | 'B') => txt!("{select} to start of [a]word[]/space"),
        unmod!('v' | 'V') => txt!("{select} to start of [a]WORD[]/space"),
        unmod!('x') => txt!("Select whole line"),
        unmod!('f') => (txt!("Select to next match"), tf.next().unwrap()),
        unmod!('F') => (txt!("Extend to next match"), tf.next().unwrap()),
        unmod!('t') => (txt!("Select until next match"), tf.next().unwrap()),
        unmod!('T') => (txt!("Extend until next match"), tf.next().unwrap()),
        alt!('f') => (txt!("Select to prev match"), tf.next().unwrap()),
        alt!('F') => (txt!("Extend to prev match"), tf.next().unwrap()),
        alt!('t') => (txt!("Select until prev match"), tf.next().unwrap()),
        alt!('T') => (txt!("Extend until prev match"), tf.next().unwrap()),
        alt!('.') => txt!(
            "Repeats the last \
             [a]'[separator],[a]\"[separator],[a]g[separator],[a]f[separator],[a]t[] \
             [separator]or[] v sequence"
        ),
        alt!('l' | 'L') | unmod!(End) | shift!(End) => txt!("{select} to end of line"),
        alt!('h' | 'H') | unmod!(Home) | shift!(Home) => txt!("{select} to start of line"),
        unmod!('[') => (txt!("Select to [a]object[] start"), obj.next().unwrap()),
        unmod!(']') => (txt!("Select to [a]object[] end"), obj.next().unwrap()),
        unmod!('{') => (txt!("Extend to [a]object[] start"), obj.next().unwrap()),
        unmod!('}') => (txt!("Extend to [a]object[] end"), obj.next().unwrap()),
        alt!('[') => (txt!("Select until [a]object[] start"), obj.next().unwrap()),
        alt!(']') => (txt!("Select until [a]object[] end"), obj.next().unwrap()),
        alt!('{') => (txt!("Extend until [a]object[] start"), obj.next().unwrap()),
        alt!('}') => (txt!("Extend until [a]object[] end"), obj.next().unwrap()),
        unmod!('%') => txt!("Select whole [a]Buffer"),
        unmod!('m') | alt!('m') => (txt!("Select in [mode]Match[] mode"), r#match("select")),
        unmod!('M') | alt!('M') => (txt!("Extend in [mode]Match[] mode"), r#match("extend")),
        unmod!('i') => txt!("[mode]Insert[] before selection"),
        unmod!('I') => txt!("[mode]Insert[] at the line's start"),
        unmod!('a') => txt!("[mode]Insert[] after selection"),
        unmod!('A') => txt!("[mode]Insert[] at the line's end"),
        unmod!('o' | 'O') => txt!("[mode]Insert[] on new line {below}"),
        alt!('o' | 'O') => txt!("Add new line {below}"),
        unmod!('.') => txt!("Repeats the last [mode]Insert[] command"),
        unmod!('r') => (txt!("Replace range"), match _ {
            unmod!(Char(..)) => txt!("Replace range with [key.char]{{char}}"),
        }),
        unmod!('`') => txt!("Lowercase the selection"),
        unmod!('~') => txt!("Uppercase the selection"),
        alt!('`') => txt!("Swap case of selection"),
        alt!(';') => txt!("Swap cursor and anchor"),
        unmod!(';') => txt!("Reduce selection to cursor"),
        alt!(':') => txt!("Place cursor on end"),
        unmod!(')') => (txt!("Enter right [mode]Rotate[] mode"), rotate("right")),
        unmod!('(') => (txt!("Enter left [mode]Rotate[] mode"), rotate("left")),
        alt!('_') => txt!("Merge adjacent selections"),
        unmod!('X') => txt!("Split selections on lines"),
        shift!('D') => txt!("Divide selections on each end"),
        unmod!('>') => txt!("Indent selections's lines"),
        unmod!('<') => txt!("Dedent selections's lines"),
        alt!('j') => txt!("Merge selections's lines"),
        unmod!('y') => txt!("Yank selections"),
        unmod!('d' | 'c') => txt!("[a]Delete[separator],[a]change[] selection"),
        alt!('d' | 'c') => txt!("[a]Delete[separator],[a]change[] selection w/o yanking"),
        unmod!('p' | 'P') => txt!("Paste [a]ahead[separator],[a]behind[]"),
        unmod!('R') => txt!("Replace selections with pasted content"),
        unmod!(',') => txt!("Remove extra selections"),
        unmod!('C') | alt!('C') => txt!("Copy selection {below}"),
        unmod!('/') | alt!('/') => txt!("[mode]Search[] {ahead}"),
        unmod!('?') | alt!('?') => txt!("[move]Search[] and select {ahead}"),
        unmod!('s') => txt!("[mode]Select[] matches in selections"),
        unmod!('S') => txt!("[mode]Split[] selections by matches"),
        alt!('k') => txt!("[mode]Keep[] matching selections"),
        alt!('K') => txt!("[mode]Keep[] [a]non[] matching selections"),
        unmod!('n' | 'N') => txt!("{select} to next search match"),
        alt!('n' | 'N') => txt!("{select} to prev search match"),
        unmod!('*') => txt!("Set main selection as search pattern"),
        unmod!('u' | 'U') => undo.clone(),
        ctrl!('l') => txt!("Move to next snippet jump"),
        ctrl!('h') => txt!("Move to prev snippet jump"),
        unmod!(':') => txt!("[a]Run commands[] in prompt line"),
        unmod!('|') => txt!("[a]Pipe selections[] to external command"),
        unmod!('g') => (txt!("Go to [parameter]line[] or to places"), goto.clone()),
        unmod!('G') => (txt!("Select to [paramenter]line[] or to places"), goto),
        alt!('Q') => txt!("Toggle macro recording"),
        alt!('q') => txt!("Replay macro"),
        ctrl!('o') => txt!("Go to prev jump"),
        ctrl!('i') | unmod!(Tab) => txt!("Go to next jump"),
        unmod!(' ') => txt!("Enter [mode]User[] mode"),
        alt!('u' | 'U') => txt!("{undo} last selection change"),
        ctrl!('r') => txt!("Reload the config crate"),
    })
}

/// The bindings for the [`Insert`] mode.
///
/// [`Insert`]: crate::Insert
pub fn insert_bindings() -> Bindings {
    bindings!(match _ {
        unmod!(Char(..) | Enter) => txt!("Insert the character"),
        unmod!(Left | Down | Up | Right) => txt!("Move cursor"),
        shift!(Left | Down | Up | Right) => txt!("Select and move cursor"),
        unmod!(Home | End) => txt!("Move to [a]start[][separator],[a]end[] of line"),
        ctrl!('n') => txt!("Next completion entry"),
        ctrl!('p') | shift!(BackTab) => txt!("Previous completion entry"),
        unmod!(Tab) => txt!("Reindent or next completion entry"),
        unmod!(Backspace | Delete) => txt!("Remove character or selection"),
        ctrl!('l') => txt!("Replace next snippet jump"),
        ctrl!('h') => txt!("Replace prev snippet jump"),
        unmod!(Esc) => txt!("Return to [mode]Normal[] mode"),
        alt!(';') => txt!("Run a single [mode]Normal[] mode command"),
        ctrl!('u') => txt!("Merge changes to this point in a single [a]Moment"),
    })
}
