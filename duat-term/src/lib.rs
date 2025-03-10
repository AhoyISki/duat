#![feature(
    iter_collect_into,
    let_chains,
    if_let_guard,
    decl_macro,
    debug_closure_helpers
)]
use std::{
    fmt::Debug,
    io::{self, Write},
    sync::{Arc, Mutex, mpsc},
    time::Duration,
};

pub use area::{Area, Coords};
use crossterm::{
    cursor,
    event::{self, PopKeyboardEnhancementFlags},
    execute,
    style::ContentStyle,
    terminal::{self, ClearType},
};
use duat_core::{
    DuatError,
    data::RwData,
    form::Color,
    text::err,
    ui::{self, Sender, UiEvent},
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

pub struct Ui {
    windows: Vec<(Area, Arc<Printer>)>,
    layouts: RwData<Vec<Layout>>,
    win: usize,
    fr: Frame,
    printer_fn: fn() -> Arc<Printer>,
}

impl Ui {
    fn cur_printer(&self) -> &Arc<Printer> {
        if let Some((_, printer)) = self.windows.get(self.win) {
            printer
        } else {
            unreachable!("Started printing before a window was created");
        }
    }
}

impl ui::Ui for Ui {
    type Area = Area;
    type MetaStatics = Mutex<Ui>;

    fn open(ms: &'static Self::MetaStatics, tx: Sender, rx: mpsc::Receiver<UiEvent>) {
        // Initial terminal setup
        use crossterm::event::{KeyboardEnhancementFlags as KEF, PushKeyboardEnhancementFlags};
        execute!(
            io::stdout(),
            terminal::EnterAlternateScreen,
            terminal::DisableLineWrap
        )
        .unwrap();
        // Some key chords (like alt+shift+o for some reason) don't work
        // without this.
        if terminal::supports_keyboard_enhancement().is_ok() {
            execute!(
                io::stdout(),
                PushKeyboardEnhancementFlags(KEF::DISAMBIGUATE_ESCAPE_CODES)
            )
            .unwrap()
        }
        terminal::enable_raw_mode().unwrap();

        // The main application input loop
        let thread = std::thread::Builder::new().name("print loop".to_string());
        let _ = thread.spawn(move || {
            loop {
                match rx.recv().unwrap() {
                    UiEvent::PausePrinting => continue,
                    UiEvent::Quit => return,
                    UiEvent::ResumePrinting => {}
                }

                let printer = ms.lock().unwrap().cur_printer().clone();

                loop {
                    if let Ok(true) = event::poll(Duration::from_millis(13)) {
                        let res = match event::read().unwrap() {
                            event::Event::Key(key) => tx.send_key(key),
                            event::Event::Resize(..) => {
                                printer.update(true);
                                tx.send_resize()
                            }
                            event::Event::FocusGained
                            | event::Event::FocusLost
                            | event::Event::Mouse(_)
                            | event::Event::Paste(_) => Ok(()),
                        };
                        if res.is_err() {
                            break;
                        }
                    }

                    printer.print();

                    if let Ok(event) = rx.try_recv() {
                        match event {
                            UiEvent::ResumePrinting => {}
                            UiEvent::PausePrinting => break,
                            UiEvent::Quit => return,
                        }
                    }
                }
            }
        });
    }

    fn close(_ms: &'static Self::MetaStatics) {
        terminal::disable_raw_mode().unwrap();
        execute!(
            io::stdout(),
            terminal::Clear(ClearType::All),
            terminal::LeaveAlternateScreen,
            terminal::EnableLineWrap,
            cursor::Show,
            PopKeyboardEnhancementFlags
        )
        .unwrap();
    }

    fn load(_ms: &'static Self::MetaStatics) {
        // Hook for returning to regular terminal state
        std::panic::set_hook(Box::new(|info| {
            let trace = std::backtrace::Backtrace::capture();
            terminal::disable_raw_mode().unwrap();
            queue!(
                io::stdout(),
                terminal::Clear(ClearType::All),
                terminal::LeaveAlternateScreen,
                cursor::MoveToColumn(0),
                terminal::Clear(ClearType::FromCursorDown),
                terminal::EnableLineWrap,
                cursor::Show,
                PopKeyboardEnhancementFlags
            );
            if let std::backtrace::BacktraceStatus::Captured = trace.status() {
                for line in trace.to_string().lines() {
                    println!("{line}");
                    queue!(io::stdout(), cursor::MoveToColumn(0));
                }
            }
            for line in info.to_string().lines() {
                println!("{line}");
                queue!(io::stdout(), cursor::MoveToColumn(0));
            }
        }));
    }

    fn new_root(
        ms: &'static Self::MetaStatics,
        cache: <Self::Area as ui::Area>::Cache,
    ) -> Self::Area {
        let mut ui = ms.lock().unwrap();
        let printer = (ui.printer_fn)();

        let root = ui.layouts.mutate(|layouts| {
            let layout = Layout::new(ui.fr, printer.clone(), cache);
            let main_id = layout.main_id();
            layouts.push(layout);

            Area::new(main_id, ui.layouts.clone())
        });
        let area = root.clone();

        ui.windows.push((root, printer));

        area
    }

    fn switch_window(ms: &'static Self::MetaStatics, win: usize) {
        ms.lock().unwrap().win = win;
    }

    fn flush_layout(ms: &'static Self::MetaStatics) {
        let ui = ms.lock().unwrap();
        ui.cur_printer().update(false);
    }

    fn unload(ms: &'static Self::MetaStatics) {
        let mut ui = ms.lock().unwrap();
        ui.windows = Vec::new();
        *ui.layouts.write() = Vec::new();
        ui.win = 0;
    }

    fn remove_window(ms: &'static Self::MetaStatics, win: usize) {
        let mut ui = ms.lock().unwrap();
        ui.windows.remove(win);
        ui.layouts.write().remove(win);
        if ui.win > win {
            ui.win -= 1;
        }
    }
}

impl Default for Ui {
    fn default() -> Self {
        Self {
            windows: Vec::new(),
            layouts: RwData::default(),
            win: 0,
            fr: Frame::default(),
            printer_fn: Arc::default
        }
    }
}

impl Clone for Ui {
    fn clone(&self) -> Self {
        panic!("You are not supposed to clone the Ui");
    }
}

pub enum ConstraintErr {
    NoParent,
    Impossible,
}

impl std::fmt::Display for ConstraintErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl std::fmt::Debug for ConstraintErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // NOTE: Might not be true in the future.
            ConstraintErr::NoParent => {
                write!(f, "No parents, so its constraint can't be changed.")
            }
            ConstraintErr::Impossible => {
                write!(f, "The constraint change is impossible.")
            }
        }
    }
}

impl DuatError for ConstraintErr {
    fn into_text(self) -> Box<duat_core::text::Text> {
        Box::new(match self {
            ConstraintErr::NoParent => {
                err!("The constraints of the master node " [*a] "can't" [] " change.")
            }
            ConstraintErr::Impossible => {
                err!("The requested constraint change is impossible.")
            }
        })
    }
}

#[derive(Debug)]
pub enum Anchor {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

impl std::error::Error for ConstraintErr {}

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

fn print_style(w: &mut impl Write, style: ContentStyle) {
    use crossterm::style::Attribute::{self, *};
    const ATTRIBUTES: [(Attribute, &[u8]); 10] = [
        (Reset, b"0"),
        (Bold, b"1"),
        (Dim, b"2"),
        (Italic, b"3"),
        (Underlined, b"4"),
        (DoubleUnderlined, b"4;2"),
        (Undercurled, b"4;3"),
        (Underdotted, b"4;4"),
        (Underdashed, b"4;5"),
        (Reverse, b"7"),
    ];
    color_values!(U8, "", "");
    color_values!(U8_SC, "", ";");
    color_values!(U8_FG_RGB, "38;2;", ";");
    color_values!(U8_FG_ANSI, "38;5;", ";");
    color_values!(U8_BG_RGB, "48;2;", ";");
    color_values!(U8_BG_ANSI, "48;5;", ";");
    color_values!(U8_UL_RGB, "58;2;", ";");
    color_values!(U8_UL_ANSI, "58;5;", ";");

    if style == ContentStyle::default() {
        return;
    }
    let _ = w.write_all(b"\x1b[");

    let mut semicolon = false;
    if !style.attributes.is_empty() {
        for (attr, ansi) in ATTRIBUTES {
            if style.attributes.has(attr) {
                if semicolon {
                    w.write_all(b";").unwrap()
                }
                w.write_all(ansi).unwrap();
                semicolon = true;
            }
        }
    }

    let semicolon = if let Some(color) = style.foreground_color {
        if semicolon {
            w.write_all(b";").unwrap()
        }
        let _ = match color {
            Color::Reset => w.write_all(b"39"),
            Color::Black => w.write_all(b"30"),
            Color::DarkRed => w.write_all(b"31"),
            Color::DarkGreen => w.write_all(b"32"),
            Color::DarkYellow => w.write_all(b"33"),
            Color::DarkBlue => w.write_all(b"34"),
            Color::DarkMagenta => w.write_all(b"35"),
            Color::DarkCyan => w.write_all(b"36"),
            Color::Grey => w.write_all(b"37"),
            Color::DarkGrey => w.write_all(b"90"),
            Color::Red => w.write_all(b"91"),
            Color::Green => w.write_all(b"92"),
            Color::Yellow => w.write_all(b"93"),
            Color::Blue => w.write_all(b"94"),
            Color::Magenta => w.write_all(b"95"),
            Color::Cyan => w.write_all(b"96"),
            Color::White => w.write_all(b"97"),
            Color::Rgb { r, g, b } => {
                let _ = w.write_all(U8_FG_RGB[r as usize].as_bytes());
                let _ = w.write_all(U8_SC[g as usize].as_bytes());
                w.write_all(U8[b as usize].as_bytes())
            }
            Color::AnsiValue(ansi) => w.write_all(U8_FG_ANSI[ansi as usize].as_bytes()),
        };
        true
    } else {
        semicolon
    };

    let semicolon = if let Some(color) = style.background_color {
        if semicolon {
            w.write_all(b";").unwrap()
        }
        match color {
            Color::Reset => w.write_all(b"49").unwrap(),
            Color::Black => w.write_all(b"40").unwrap(),
            Color::DarkRed => w.write_all(b"41").unwrap(),
            Color::DarkGreen => w.write_all(b"42").unwrap(),
            Color::DarkYellow => w.write_all(b"43").unwrap(),
            Color::DarkBlue => w.write_all(b"44").unwrap(),
            Color::DarkMagenta => w.write_all(b"45").unwrap(),
            Color::DarkCyan => w.write_all(b"46").unwrap(),
            Color::Grey => w.write_all(b"47").unwrap(),
            Color::DarkGrey => w.write_all(b"100").unwrap(),
            Color::Red => w.write_all(b"101").unwrap(),
            Color::Green => w.write_all(b"102").unwrap(),
            Color::Yellow => w.write_all(b"103").unwrap(),
            Color::Blue => w.write_all(b"104").unwrap(),
            Color::Magenta => w.write_all(b"105").unwrap(),
            Color::Cyan => w.write_all(b"106").unwrap(),
            Color::White => w.write_all(b"107").unwrap(),
            Color::Rgb { r, g, b } => {
                w.write_all(U8_BG_RGB[r as usize].as_bytes()).unwrap();
                w.write_all(U8_SC[g as usize].as_bytes()).unwrap();
                w.write_all(U8[b as usize].as_bytes()).unwrap()
            }
            Color::AnsiValue(ansi) => w.write_all(U8_BG_ANSI[ansi as usize].as_bytes()).unwrap(),
        };
        true
    } else {
        semicolon
    };

    if let Some(color) = style.underline_color {
        if semicolon {
            w.write_all(b";").unwrap()
        }
        match color {
            Color::Reset => w.write_all(b"59").unwrap(),
            Color::Black => w.write_all(b"58;0").unwrap(),
            Color::DarkRed => w.write_all(b"58;1").unwrap(),
            Color::DarkGreen => w.write_all(b"58;2").unwrap(),
            Color::DarkYellow => w.write_all(b"58;3").unwrap(),
            Color::DarkBlue => w.write_all(b"58;4").unwrap(),
            Color::DarkMagenta => w.write_all(b"58;5").unwrap(),
            Color::DarkCyan => w.write_all(b"58;6").unwrap(),
            Color::Grey => w.write_all(b"58;7").unwrap(),
            Color::DarkGrey => w.write_all(b"58;8").unwrap(),
            Color::Red => w.write_all(b"58;9").unwrap(),
            Color::Green => w.write_all(b"58;10").unwrap(),
            Color::Yellow => w.write_all(b"58;11").unwrap(),
            Color::Blue => w.write_all(b"58;12").unwrap(),
            Color::Magenta => w.write_all(b"58;13").unwrap(),
            Color::Cyan => w.write_all(b"58;14").unwrap(),
            Color::White => w.write_all(b"58;15").unwrap(),
            Color::Rgb { r, g, b } => {
                w.write_all(U8_UL_RGB[r as usize].as_bytes()).unwrap();
                w.write_all(U8_SC[g as usize].as_bytes()).unwrap();
                w.write_all(U8[b as usize].as_bytes()).unwrap()
            }
            Color::AnsiValue(ansi) => w.write_all(U8_UL_ANSI[ansi as usize].as_bytes()).unwrap(),
        };
    }

    w.write_all(b"m").unwrap();
}

macro queue($writer:expr $(, $command:expr)* $(,)?) {
    crossterm::queue!($writer $(, $command)*).unwrap()
}

macro style($lines:expr, $style:expr) {{
    #[cfg(unix)]
    print_style(&mut $lines, $style);
    #[cfg(not(unix))]
    queue!(
        $lines,
        crossterm::style::ResetColor,
        crossterm::style::SetStyle($style)
    );
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
