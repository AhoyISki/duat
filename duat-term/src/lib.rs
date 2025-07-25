#![feature(decl_macro, debug_closure_helpers)]
use std::{
    cell::RefCell,
    fmt::Debug,
    io::{self, Write},
    rc::Rc,
    sync::{Arc, Mutex, mpsc},
    time::Duration,
};

pub use area::{Area, Coords};
use crossterm::{
    cursor,
    event::{self, Event as CtEvent, poll as ct_poll, read as ct_read},
    execute,
    style::{ContentStyle, Print},
    terminal::{self, ClearType},
};
use duat_core::{
    MainThreadOnly,
    form::Color,
    ui::{self, Sender},
};

use self::{layout::Layout, print::Printer};
pub use self::{
    print::{Brush, Frame},
    rules::{VertRule, VertRuleCfg},
};

mod area;
mod layout;
mod print;
mod rules;

pub struct Ui;

impl ui::Ui for Ui {
    type Area = Area;
    type MetaStatics = Mutex<MetaStatics>;

    fn open(ms: &'static Self::MetaStatics, tx: Sender) {
        use event::{KeyboardEnhancementFlags as KEF, PushKeyboardEnhancementFlags};

        let thread = std::thread::Builder::new().name("print loop".to_string());
        let rx = ms.lock().unwrap().rx.take().unwrap();

        let _ = thread.spawn(move || {
            // Wait for everything to be setup before doing anything to the
            // terminal, for a less jarring effect.
            let Ok(Event::NewPrinter(mut printer)) = rx.recv() else {
                unreachable!("Failed to load the Ui");
            };

            terminal::enable_raw_mode().unwrap();

            // Initial terminal setup
            // Some key chords (like alt+shift+o for some reason) don't work
            // without this.
            if let Ok(true) = terminal::supports_keyboard_enhancement() {
                queue!(
                    io::stdout(),
                    PushKeyboardEnhancementFlags(
                        KEF::DISAMBIGUATE_ESCAPE_CODES | KEF::REPORT_ALTERNATE_KEYS
                    )
                );
            }

            execute!(
                io::stdout(),
                terminal::EnterAlternateScreen,
                terminal::Clear(ClearType::All),
                terminal::DisableLineWrap,
                event::EnableBracketedPaste,
                event::EnableFocusChange,
                event::EnableMouseCapture
            )
            .unwrap();

            loop {
                if let Ok(true) = ct_poll(Duration::from_millis(20)) {
                    let res = match ct_read().unwrap() {
                        CtEvent::Key(key) => tx.send_key(key),
                        CtEvent::Resize(..) => {
                            printer.update(true);
                            tx.send_resize()
                        }
                        CtEvent::FocusGained => tx.send_focused(),
                        CtEvent::FocusLost => tx.send_unfocused(),
                        CtEvent::Mouse(_) | CtEvent::Paste(_) => Ok(()),
                    };
                    if res.is_err() {
                        break;
                    }
                }

                printer.print();

                match rx.try_recv() {
                    Ok(Event::NewPrinter(new_printer)) => printer = new_printer,
                    Ok(Event::Quit) => break,
                    Err(_) => {}
                }
            }
        });
    }

    fn close(ms: &'static Self::MetaStatics) {
        ms.lock().unwrap().tx.send(Event::Quit).unwrap();
        execute!(
            io::stdout(),
            terminal::Clear(ClearType::All),
            terminal::LeaveAlternateScreen,
            terminal::EnableLineWrap,
            event::DisableBracketedPaste,
            event::DisableFocusChange,
            event::DisableMouseCapture,
            cursor::Show,
        )
        .unwrap();
        terminal::disable_raw_mode().unwrap();
    }

    fn new_root(
        ms: &'static Self::MetaStatics,
        cache: <Self::Area as ui::RawArea>::Cache,
    ) -> Self::Area {
        let mut ms = ms.lock().unwrap();
        let printer = (ms.printer_fn)();

        let main_id = {
            // SAFETY: Ui::MetaStatics is not Send + Sync, so this can't be called
            // from another thread
            let mut layouts = unsafe { ms.layouts.get() }.borrow_mut();
            let layout = Layout::new(ms.fr, printer.clone(), cache);
            let main_id = layout.main_id();
            layouts.push(layout);
            main_id
        };

        let root = Area::new(main_id, unsafe { ms.layouts.get() }.clone());
        ms.windows.push((root.clone(), printer.clone()));
        if ms.windows.len() == 1 {
            ms.tx.send(Event::NewPrinter(printer)).unwrap();
        }

        root
    }

    fn switch_window(ms: &'static Self::MetaStatics, win: usize) {
        let mut ms = ms.lock().unwrap();
        ms.win = win;
        ms.tx.send(Event::NewPrinter(ms.cur_printer())).unwrap()
    }

    fn flush_layout(ms: &'static Self::MetaStatics) {
        ms.lock().unwrap().cur_printer().update(false);
    }

    fn load(_ms: &'static Self::MetaStatics) {
        // Hook for returning to regular terminal state
        std::panic::set_hook(Box::new(|info| {
            let trace = std::backtrace::Backtrace::capture();
            terminal::disable_raw_mode().unwrap();
            execute!(
                io::stdout(),
                terminal::Clear(ClearType::All),
                terminal::LeaveAlternateScreen,
                cursor::MoveToColumn(0),
                terminal::Clear(ClearType::FromCursorDown),
                terminal::EnableLineWrap,
                cursor::Show,
                Print(info)
            )
            .unwrap();
            if let std::backtrace::BacktraceStatus::Captured = trace.status() {
                println!();
                execute!(io::stdout(), cursor::MoveToColumn(0)).unwrap();
                for line in trace.to_string().lines() {
                    if !line.contains("             at ") {
                        println!("{line}");
                        queue!(io::stdout(), cursor::MoveToColumn(0));
                    }
                }
            }
            for line in info.to_string().lines() {
                println!("{line}");
                execute!(io::stdout(), cursor::MoveToColumn(0)).unwrap();
            }
        }));
    }

    fn unload(ms: &'static Self::MetaStatics) {
        let mut ms = ms.lock().unwrap();
        ms.windows = Vec::new();
        // SAFETY: Ui::MetaStatics is not Send + Sync, so this can't be called
        // from another thread
        *unsafe { ms.layouts.get() }.borrow_mut() = Vec::new();
        ms.win = 0;
    }

    fn remove_window(ms: &'static Self::MetaStatics, win: usize) {
        let mut ms = ms.lock().unwrap();
        ms.windows.remove(win);
        // SAFETY: Ui::MetaStatics is not Send + Sync, so this can't be called
        // from another thread
        unsafe { ms.layouts.get() }.borrow_mut().remove(win);
        if ms.win > win {
            ms.win -= 1;
        }
    }
}

impl Clone for Ui {
    fn clone(&self) -> Self {
        panic!("You are not supposed to clone the Ui");
    }
}

impl Default for Ui {
    fn default() -> Self {
        panic!("You are not supposed to use the default constructor of the Ui");
    }
}

#[doc(hidden)]
pub struct MetaStatics {
    windows: Vec<(Area, Arc<Printer>)>,
    layouts: MainThreadOnly<Rc<RefCell<Vec<Layout>>>>,
    win: usize,
    fr: Frame,
    printer_fn: fn() -> Arc<Printer>,
    rx: Option<mpsc::Receiver<Event>>,
    tx: mpsc::Sender<Event>,
}

impl MetaStatics {
    fn cur_printer(&self) -> Arc<Printer> {
        if let Some((_, printer)) = self.windows.get(self.win) {
            printer.clone()
        } else {
            unreachable!("Started printing before a window was created");
        }
    }
}

impl Default for MetaStatics {
    fn default() -> Self {
        let (tx, rx) = mpsc::channel();
        Self {
            windows: Vec::new(),
            layouts: MainThreadOnly::default(),
            win: 0,
            fr: Frame::default(),
            printer_fn: || Arc::new(Printer::new()),
            rx: Some(rx),
            tx,
        }
    }
}

// SAFETY: The Area (the part that is not Send + Sync) is only ever
// accessed from the main thread.
unsafe impl Send for MetaStatics {}

#[derive(Debug)]
pub enum Anchor {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

enum Event {
    NewPrinter(Arc<Printer>),
    Quit,
}

impl Eq for Event {}

impl PartialEq for Event {
    fn eq(&self, other: &Self) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(other)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct AreaId(usize);

impl AreaId {
    /// Generates a unique index for [`Rect`]s.
    fn new() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

        AreaId(INDEX_COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

impl std::fmt::Debug for AreaId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

type Equality = cassowary::Constraint;

fn print_style(
    w: &mut impl Write,
    style: ContentStyle,
    ansi_codes: &mut micromap::Map<CStyle, String, 16>,
) {
    if let Some(ansi) = ansi_codes.get(&CStyle(style)) {
        w.write_all(ansi.as_bytes()).unwrap();
    } else if style != ContentStyle::default() {
        let ansi = {
            let mut ansi = String::new();
            use crossterm::style::Attribute::{self, *};
            const ATTRIBUTES: [(Attribute, &str); 10] = [
                (Reset, "0"),
                (Bold, "1"),
                (Dim, "2"),
                (Italic, "3"),
                (Underlined, "4"),
                (DoubleUnderlined, "4;2"),
                (Undercurled, "4;3"),
                (Underdotted, "4;4"),
                (Underdashed, "4;5"),
                (Reverse, "7"),
            ];
            color_values!(U8, "", "");
            color_values!(U8_SC, "", ";");
            color_values!(U8_FG_RGB, "38;2;", ";");
            color_values!(U8_FG_ANSI, "38;5;", ";");
            color_values!(U8_BG_RGB, "48;2;", ";");
            color_values!(U8_BG_ANSI, "48;5;", ";");
            color_values!(U8_UL_RGB, "58;2;", ";");
            color_values!(U8_UL_ANSI, "58;5;", ";");

            ansi.push_str("\x1b[");

            let mut semicolon = false;
            if !style.attributes.is_empty() {
                for (attr, code) in ATTRIBUTES {
                    if style.attributes.has(attr) {
                        if semicolon {
                            ansi.push(';')
                        }
                        ansi.push_str(code);
                        semicolon = true;
                    }
                }
            }

            let semicolon = if let Some(color) = style.foreground_color {
                if semicolon {
                    ansi.push(';');
                }
                match color {
                    Color::Reset => ansi.push_str("39"),
                    Color::Black => ansi.push_str("30"),
                    Color::DarkRed => ansi.push_str("31"),
                    Color::DarkGreen => ansi.push_str("32"),
                    Color::DarkYellow => ansi.push_str("33"),
                    Color::DarkBlue => ansi.push_str("34"),
                    Color::DarkMagenta => ansi.push_str("35"),
                    Color::DarkCyan => ansi.push_str("36"),
                    Color::Grey => ansi.push_str("37"),
                    Color::DarkGrey => ansi.push_str("90"),
                    Color::Red => ansi.push_str("91"),
                    Color::Green => ansi.push_str("92"),
                    Color::Yellow => ansi.push_str("93"),
                    Color::Blue => ansi.push_str("94"),
                    Color::Magenta => ansi.push_str("95"),
                    Color::Cyan => ansi.push_str("96"),
                    Color::White => ansi.push_str("97"),
                    Color::Rgb { r, g, b } => {
                        ansi.push_str(U8_FG_RGB[r as usize]);
                        ansi.push_str(U8_SC[g as usize]);
                        ansi.push_str(U8[b as usize])
                    }
                    Color::AnsiValue(val) => ansi.push_str(U8_FG_ANSI[val as usize]),
                };
                true
            } else {
                semicolon
            };

            let semicolon = if let Some(color) = style.background_color {
                if semicolon {
                    ansi.push(';');
                }
                match color {
                    Color::Reset => ansi.push_str("49"),
                    Color::Black => ansi.push_str("40"),
                    Color::DarkRed => ansi.push_str("41"),
                    Color::DarkGreen => ansi.push_str("42"),
                    Color::DarkYellow => ansi.push_str("43"),
                    Color::DarkBlue => ansi.push_str("44"),
                    Color::DarkMagenta => ansi.push_str("45"),
                    Color::DarkCyan => ansi.push_str("46"),
                    Color::Grey => ansi.push_str("47"),
                    Color::DarkGrey => ansi.push_str("100"),
                    Color::Red => ansi.push_str("101"),
                    Color::Green => ansi.push_str("102"),
                    Color::Yellow => ansi.push_str("103"),
                    Color::Blue => ansi.push_str("104"),
                    Color::Magenta => ansi.push_str("105"),
                    Color::Cyan => ansi.push_str("106"),
                    Color::White => ansi.push_str("107"),
                    Color::Rgb { r, g, b } => {
                        ansi.push_str(U8_BG_RGB[r as usize]);
                        ansi.push_str(U8_SC[g as usize]);
                        ansi.push_str(U8[b as usize]);
                    }
                    Color::AnsiValue(val) => ansi.push_str(U8_BG_ANSI[val as usize]),
                };
                true
            } else {
                semicolon
            };

            if let Some(color) = style.underline_color {
                if semicolon {
                    ansi.push(';');
                }
                match color {
                    Color::Reset => ansi.push_str("59"),
                    Color::Black => ansi.push_str("58;0"),
                    Color::DarkRed => ansi.push_str("58;1"),
                    Color::DarkGreen => ansi.push_str("58;2"),
                    Color::DarkYellow => ansi.push_str("58;3"),
                    Color::DarkBlue => ansi.push_str("58;4"),
                    Color::DarkMagenta => ansi.push_str("58;5"),
                    Color::DarkCyan => ansi.push_str("58;6"),
                    Color::Grey => ansi.push_str("58;7"),
                    Color::DarkGrey => ansi.push_str("58;8"),
                    Color::Red => ansi.push_str("58;9"),
                    Color::Green => ansi.push_str("58;10"),
                    Color::Yellow => ansi.push_str("58;11"),
                    Color::Blue => ansi.push_str("58;12"),
                    Color::Magenta => ansi.push_str("58;13"),
                    Color::Cyan => ansi.push_str("58;14"),
                    Color::White => ansi.push_str("58;15"),
                    Color::Rgb { r, g, b } => {
                        ansi.push_str(U8_UL_RGB[r as usize]);
                        ansi.push_str(U8_SC[g as usize]);
                        ansi.push_str(U8[b as usize])
                    }
                    Color::AnsiValue(val) => ansi.push_str(U8_UL_ANSI[val as usize]),
                };
            }

            ansi.push('m');

            ansi
        };

        w.write_all(ansi.as_bytes()).unwrap();

        ansi_codes.checked_insert(CStyle(style), ansi);
    }
}

macro queue($writer:expr $(, $command:expr)* $(,)?) {
    crossterm::queue!($writer $(, $command)*).unwrap()
}

macro style($lines:expr, $ansi_codes:expr, $style:expr) {{
    #[cfg(unix)]
    print_style(&mut $lines, $style, $ansi_codes);
    #[cfg(not(unix))]
    queue!($lines, crossterm::style::SetStyle($style));
}}

#[rustfmt::skip]
macro color_values($name:ident, $p:literal, $s:literal) {
    macro c($n:literal) {
        concat!($p, $n, $s)
    }
    const $name: [&str; 256] = [
        c!(0), c!(1), c!(2), c!(3), c!(4), c!(5), c!(6), c!(7), c!(8), c!(9), c!(10), c!(11),
        c!(12), c!(13), c!(14), c!(15), c!(16), c!(17), c!(18), c!(19), c!(20), c!(21), c!(22),
        c!(23), c!(24), c!(25), c!(26), c!(27), c!(28), c!(29), c!(30), c!(31), c!(32), c!(33),
        c!(34), c!(35), c!(36), c!(37), c!(38), c!(39), c!(40), c!(41), c!(42), c!(43), c!(44),
        c!(45), c!(46), c!(47), c!(48), c!(49), c!(50), c!(51), c!(52), c!(53), c!(54), c!(55),
        c!(56), c!(57), c!(58), c!(59), c!(60), c!(61), c!(62), c!(63), c!(64), c!(65), c!(66),
        c!(67), c!(68), c!(69), c!(70), c!(71), c!(72), c!(73), c!(74), c!(75), c!(76), c!(77),
        c!(78), c!(79), c!(80), c!(81), c!(82), c!(83), c!(84), c!(85), c!(86), c!(87), c!(88),
        c!(89), c!(90), c!(91), c!(92), c!(93), c!(94), c!(95), c!(96), c!(97), c!(98), c!(99),
        c!(100), c!(101), c!(102), c!(103), c!(104), c!(105), c!(106), c!(107), c!(108), c!(109),
        c!(110), c!(111), c!(112), c!(113), c!(114), c!(115), c!(116), c!(117), c!(118), c!(119),
        c!(120), c!(121), c!(122), c!(123), c!(124), c!(125), c!(126), c!(127), c!(128), c!(129),
        c!(130), c!(131), c!(132), c!(133), c!(134), c!(135), c!(136), c!(137), c!(138), c!(139),
        c!(140), c!(141), c!(142), c!(143), c!(144), c!(145), c!(146), c!(147), c!(148), c!(149),
        c!(150), c!(151), c!(152), c!(153), c!(154), c!(155), c!(156), c!(157), c!(158), c!(159),
        c!(160), c!(161), c!(162), c!(163), c!(164), c!(165), c!(166), c!(167), c!(168), c!(169),
        c!(170), c!(171), c!(172), c!(173), c!(174), c!(175), c!(176), c!(177), c!(178), c!(179),
        c!(180), c!(181), c!(182), c!(183), c!(184), c!(185), c!(186), c!(187), c!(188), c!(189),
        c!(190), c!(191), c!(192), c!(193), c!(194), c!(195), c!(196), c!(197), c!(198), c!(199),
        c!(200), c!(201), c!(202), c!(203), c!(204), c!(205), c!(206), c!(207), c!(208), c!(209),
        c!(210), c!(211), c!(212), c!(213), c!(214), c!(215), c!(216), c!(217), c!(218), c!(219),
        c!(220), c!(221), c!(222), c!(223), c!(224), c!(225), c!(226), c!(227), c!(228), c!(229),
        c!(230), c!(231), c!(232), c!(233), c!(234), c!(235), c!(236), c!(237), c!(238), c!(239),
        c!(240), c!(241), c!(242), c!(243), c!(244), c!(245), c!(246), c!(247), c!(248), c!(249),
        c!(250), c!(251), c!(252), c!(253), c!(254), c!(255),
    ];
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CStyle(ContentStyle);

impl std::hash::Hash for CStyle {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.foreground_color.hash(state);
        self.0.background_color.hash(state);
        self.0.foreground_color.hash(state);
        let attr: u32 = unsafe { std::mem::transmute(self.0.attributes) };
        attr.hash(state);
    }
}
