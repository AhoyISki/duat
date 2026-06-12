//! This module basically only exists to reduce the burden
//! on rust-analyzer on other places.
use duat_core::{
    mode::{Bindings, KeyCode::*, alt, bindings, ctrl, event, shift},
    txt,
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
            event!('b' | '(' | ')') => txt!("parenthesis block"),
            event!('B' | '{' | '}') => txt!("brace block"),
            event!('r' | '[' | ']') => txt!("bracket block"),
            event!('a' | '<' | '>') => txt!("angle bracket block"),
            event!('Q' | '"') => txt!("double quote string"),
            event!('q' | '\'') => txt!("single quote string"),
            event!('g' | '`') => txt!("grave quote string"),
            event!('w') | event!('e') => word.clone(),
            event!('s') => txt!("sentence"),
            event!('p') => txt!("paragraph"),
            event!(' ') => txt!("whitespace"),
            event!('i') => txt!("indent"),
            event!('u') => txt!("argument"),
        })
    };

    let r#match = {
        let object = object.clone();
        move |action: &str| Bindings {
            title: Some(txt!("{action} match")),
            ..bindings!(match _ {
                event!('l' | 'm') => txt!("next pair"),
                event!('M') => txt!("extend to next pair"),
                event!('h') | alt!('m') => txt!("prev pair"),
                alt!('M') => txt!("extend to prev pair"),
                event!('i') => (
                    txt!("inside object"),
                    object(&format!("{action} inside object"))
                ),
                event!('a') => (
                    txt!("around object"),
                    object(&format!("{action} around object"))
                ),
                event!('s') => (txt!("surround selection"), Bindings {
                    title: Some(txt!("surround with")),
                    ..bindings!(match _ {
                        event!('b' | '(' | ')') => txt!("parenthesis"),
                        event!('B' | '{' | '}') => txt!("braces"),
                        event!('r' | '[' | ']') => txt!("brackets"),
                        event!('a' | '<' | '>') => txt!("angle brackets"),
                        event!('Q' | '"') => txt!("double quotes"),
                        event!('q' | '\'') => txt!("single quotes"),
                        event!('g' | '`') => txt!("grave quotes"),
                        event!('t') => txt!("html tag"),
                        event!(Char(..)) => txt!("another character"),
                    })
                }),
            })
        }
    };

    let goto = Bindings {
        title: Some(txt!("goto")),
        ..bindings!(match _ {
            event!('h') => txt!("start of line"),
            event!('j') => txt!("end of [a]Buffer"),
            event!('k' | 'g') => txt!("start of [a]Buffer"),
            event!('l') => txt!("end of line"),
            event!('i') => txt!("first character in line"),
            event!('a') => txt!("last swapped [a]Buffer"),
            event!('n') => txt!("next [a]Buffer"),
            event!('N') => txt!("prev [a]Buffer"),
            event!('o') => txt!("move [a]Buffer[] to new window"),
        })
    };

    let rotate = |direction: &str| Bindings {
        title: Some(txt!("Rotate {direction}")),
        ..bindings!(match _ {
            event!('s') => txt!("rotate selection"),
            event!('c') => txt!("rotate content"),
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
                    event!(Char(..)) => txt!("[key.char]{{char}}[] to {sel} {to}"),
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
        event!('h' | 'j' | 'k' | 'l') => txt!("Move cursor"),
        event!('H' | 'J' | 'K' | 'L') => txt!("Select and move cursor"),
        event!(Left | Up | Down | Right) => txt!("Move cursor wrapped"),
        shift!(Left | Up | Down | Right) => txt!("Select and move cursor wrapped"),
        event!('w' | 'W') => txt!("{select} to end of [a]word[]/space"),
        event!('e' | 'E') => txt!("{select} to end of [a]WORD[]/space"),
        event!('b' | 'B') => txt!("{select} to start of [a]word[]/space"),
        event!('v' | 'V') => txt!("{select} to start of [a]WORD[]/space"),
        event!('x') => txt!("Select whole line"),
        event!('f') => (txt!("Select to next match"), tf.next().unwrap()),
        event!('F') => (txt!("Extend to next match"), tf.next().unwrap()),
        event!('t') => (txt!("Select until next match"), tf.next().unwrap()),
        event!('T') => (txt!("Extend until next match"), tf.next().unwrap()),
        alt!('f') => (txt!("Select to prev match"), tf.next().unwrap()),
        alt!('F') => (txt!("Extend to prev match"), tf.next().unwrap()),
        alt!('t') => (txt!("Select until prev match"), tf.next().unwrap()),
        alt!('T') => (txt!("Extend until prev match"), tf.next().unwrap()),
        alt!('.') => txt!(
            "Repeats the last \
             [a]'[separator],[a]\"[separator],[a]g[separator],[a]f[separator],[a]t[] \
             [separator]or[] v sequence"
        ),
        alt!('l' | 'L') | event!(End) | shift!(End) => txt!("{select} to end of line"),
        alt!('h' | 'H') | event!(Home) | shift!(Home) => txt!("{select} to start of line"),
        event!('[') => (txt!("Select to [a]object[] start"), obj.next().unwrap()),
        event!(']') => (txt!("Select to [a]object[] end"), obj.next().unwrap()),
        event!('{') => (txt!("Extend to [a]object[] start"), obj.next().unwrap()),
        event!('}') => (txt!("Extend to [a]object[] end"), obj.next().unwrap()),
        alt!('[') => (txt!("Select until [a]object[] start"), obj.next().unwrap()),
        alt!(']') => (txt!("Select until [a]object[] end"), obj.next().unwrap()),
        alt!('{') => (txt!("Extend until [a]object[] start"), obj.next().unwrap()),
        alt!('}') => (txt!("Extend until [a]object[] end"), obj.next().unwrap()),
        event!('%') => txt!("Select whole [a]Buffer"),
        event!('m') | alt!('m') => (txt!("Select in [mode]Match[] mode"), r#match("select")),
        event!('M') | alt!('M') => (txt!("Extend in [mode]Match[] mode"), r#match("extend")),
        event!('i') => txt!("[mode]Insert[] before selection"),
        event!('I') => txt!("[mode]Insert[] at the line's start"),
        event!('a') => txt!("[mode]Insert[] after selection"),
        event!('A') => txt!("[mode]Insert[] at the line's end"),
        event!('o' | 'O') => txt!("[mode]Insert[] on new line {below}"),
        alt!('o' | 'O') => txt!("Add new line {below}"),
        event!('.') => txt!("Repeats the last [mode]Insert[] command"),
        event!('r') => (txt!("Replace range"), match _ {
            event!(Char(..)) => txt!("Replace range with [key.char]{{char}}"),
        }),
        event!('`') => txt!("Lowercase the selection"),
        event!('~') => txt!("Uppercase the selection"),
        alt!('`') => txt!("Swap case of selection"),
        alt!(';') => txt!("Swap cursor and anchor"),
        event!(';') => txt!("Reduce selection to cursor"),
        alt!(':') => txt!("Place cursor on end"),
        event!(')') => (txt!("Enter right [mode]Rotate[] mode"), rotate("right")),
        event!('(') => (txt!("Enter left [mode]Rotate[] mode"), rotate("left")),
        alt!('_') => txt!("Merge adjacent selections"),
        event!('X') => txt!("Split selections on lines"),
        shift!('D') => txt!("Divide selections on each end"),
        event!('>') => txt!("Indent selections's lines"),
        event!('<') => txt!("Dedent selections's lines"),
        alt!('j') => txt!("Merge selections's lines"),
        event!('y') => txt!("Yank selections"),
        event!('d' | 'c') => txt!("[a]Delete[separator],[a]change[] selection"),
        alt!('d' | 'c') => txt!("[a]Delete[separator],[a]change[] selection w/o yanking"),
        event!('p' | 'P') => txt!("Paste [a]ahead[separator],[a]behind[]"),
        event!('R') => txt!("Replace selections with pasted content"),
        event!(',') => txt!("Remove extra selections"),
        event!('C') | alt!('C') => txt!("Copy selection {below}"),
        event!('/') | alt!('/') => txt!("[mode]Search[] {ahead}"),
        event!('?') | alt!('?') => txt!("[move]Search[] and select {ahead}"),
        event!('s') => txt!("[mode]Select[] matches in selections"),
        event!('S') => txt!("[mode]Split[] selections by matches"),
        alt!('k') => txt!("[mode]Keep[] matching selections"),
        alt!('K') => txt!("[mode]Keep[] [a]non[] matching selections"),
        event!('n' | 'N') => txt!("{select} to next search match"),
        alt!('n' | 'N') => txt!("{select} to prev search match"),
        event!('*') => txt!("Set main selection as search pattern"),
        event!('u' | 'U') => undo.clone(),
        ctrl!('l') => txt!("Move to next snippet jump"),
        ctrl!('h') => txt!("Move to prev snippet jump"),
        event!(':') => txt!("[a]Run commands[] in prompt line"),
        event!('|') => txt!("[a]Pipe selections[] to external command"),
        event!('g') => (txt!("Go to [parameter]line[] or to places"), goto.clone()),
        event!('G') => (txt!("Select to [paramenter]line[] or to places"), goto),
        alt!('Q') => txt!("Toggle macro recording"),
        alt!('q') => txt!("Replay macro"),
        ctrl!('o') => txt!("Go to prev jump"),
        ctrl!('i') | event!(Tab) => txt!("Go to next jump"),
        event!(' ') => txt!("Enter [mode]User[] mode"),
        alt!('u' | 'U') => txt!("{undo} last selection change"),
        ctrl!('r') => txt!("Reload the config crate"),
    })
}

/// The bindings for the [`Insert`] mode.
///
/// [`Insert`]: crate::Insert
pub fn insert_bindings() -> Bindings {
    bindings!(match _ {
        event!(Char(..) | Enter) => txt!("Insert the character"),
        event!(Left | Down | Up | Right) => txt!("Move cursor"),
        shift!(Left | Down | Up | Right) => txt!("Select and move cursor"),
        event!(Home | End) => txt!("Move to [a]start[][separator],[a]end[] of line"),
        ctrl!('n') => txt!("Next completion entry"),
        ctrl!('p') | shift!(BackTab) => txt!("Previous completion entry"),
        event!(Tab) => txt!("Reindent or next completion entry"),
        event!(Backspace | Delete) => txt!("Remove character or selection"),
        ctrl!('l') => txt!("Replace next snippet jump"),
        ctrl!('h') => txt!("Replace prev snippet jump"),
        event!(Esc) => txt!("Return to [mode]Normal[] mode"),
        alt!(';') => txt!("Run a single [mode]Normal[] mode command"),
        ctrl!('u') => txt!("Merge changes to this point in a single [a]Moment"),
    })
}
