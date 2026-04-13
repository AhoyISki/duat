//! A terminal implementation for Duat's Ui
//!
//! This implementation is a sort of trial of the "preparedness" of
//! [`RawUi`]'s API, in order to figure out what should be included
//! and what shouldn't.
use std::{
    ffi::OsString,
    fmt::Debug,
    io::{self, Write},
    sync::{Arc, Mutex, mpsc},
};

use crossterm::{
    cursor,
    event::{self, Event as CtEvent},
    execute, queue,
    style::{Color, ContentStyle},
    terminal::{self, ClearType},
};
use duat_core::{
    context::DuatSender,
    form::{self, Form},
    session::UiMouseEvent,
    ui::{
        self,
        traits::{RawArea, RawUi},
    },
};

use self::printer::Printer;
pub use self::{
    area::{Area, Coords},
    layout::{Frame, FrameStyle},
    printer::{Border, BorderStyle},
    rules::{SepChar, VertRule, VertRuleBuilder},
};
use crate::layout::Layouts;

mod area;
mod layout;
mod printer;
mod rules;

/// The [`RawUi`] implementation for `duat-term`
pub struct Ui {
    mt: Mutex<InnerUi>,
}

struct InnerUi {
    windows: Vec<(Area, Arc<Printer>)>,
    layouts: Layouts,
    win: usize,
    border: Border,
    term_tx: mpsc::Sender<Event>,
}

// SAFETY: The Area (the part that is not Send + Sync) is only ever
// accessed from the main thread.
unsafe impl Send for InnerUi {}

impl RawUi for Ui {
    type Area = Area;

    fn open() -> Vec<OsString> {
        use event::{KeyboardEnhancementFlags as KEF, PushKeyboardEnhancementFlags};

        terminal::enable_raw_mode().unwrap();

        // Initial terminal setup
        // Some key chords (like alt+shift+o for some reason) don't work
        // without this.
        queue!(
            io::stdout(),
            terminal::EnterAlternateScreen,
            terminal::Clear(ClearType::All),
            terminal::DisableLineWrap,
            event::EnableFocusChange,
            event::EnableMouseCapture
        )
        .unwrap();

        #[cfg(target_os = "windows")]
        unsafe {
            if windows_sys::Win32::System::Console::SetConsoleOutputCP(65001) == 0 {
                panic!("Failed to set output mode to utf8");
            }
        }

        if let Ok(true) = terminal::supports_keyboard_enhancement() {
            queue!(
                io::stdout(),
                PushKeyboardEnhancementFlags(
                    KEF::DISAMBIGUATE_ESCAPE_CODES | KEF::REPORT_ALTERNATE_KEYS
                )
            )
            .unwrap();
        }

        Vec::new()
    }

    fn close() {
        if let Ok(true) = terminal::supports_keyboard_enhancement() {
            queue!(io::stdout(), event::PopKeyboardEnhancementFlags).unwrap();
        }

        execute!(
            io::stdout(),
            // If entering failed for some reason earlier, don't clear the main screen.
            terminal::EnterAlternateScreen,
            terminal::Clear(ClearType::All),
            terminal::LeaveAlternateScreen,
            cursor::MoveToColumn(0),
            terminal::Clear(ClearType::FromCursorDown),
            terminal::EnableLineWrap,
            event::DisableFocusChange,
            event::DisableMouseCapture,
            cursor::Show,
        )
        .unwrap();

        terminal::disable_raw_mode().unwrap();
    }

    fn load(duat_tx: DuatSender) -> Self {
        form::set_weak("rule.upper", Form::mimic("default.VertRule"));
        form::set_weak("rule.lower", Form::mimic("default.VertRule"));
        rules::add_vertrule_hook();

        let (term_tx, term_rx) = mpsc::channel();
        let terminal_border_id = form::id_of!("terminal.border");

        let print_thread = std::thread::Builder::new().name("print loop".to_string());
        let _ = print_thread.spawn(move || {
            // Wait for everything to be setup before doing anything to the
            // terminal, for a less jarring effect.
            let mut printer = term_rx
                .iter()
                .find_map(|event| match event {
                    Event::NewPrinter(printer) => Some(printer),
                    _ => None,
                })
                .unwrap();

            for event in term_rx {
                match event {
                    Event::Print => printer.print(terminal_border_id),
                    Event::UpdatePrinter => printer.update(true, true),
                    Event::ClearPrinter => printer.clear(),
                    Event::NewPrinter(new_printer) => printer = new_printer,
                }
            }
        });

        let _ = std::thread::Builder::new()
            .name("crossterm".to_string())
            .spawn({
                let term_tx = term_tx.clone();
                move || {
                    while let Ok(event) = crossterm::event::read() {
                        match event {
                            CtEvent::Key(key) => {
                                if !matches!(key.kind, event::KeyEventKind::Release) {
                                    duat_tx.send_key(key);
                                }
                            }
                            CtEvent::Resize(..) => {
                                term_tx.send(Event::UpdatePrinter).unwrap();
                                duat_tx.send_resize();
                            }
                            CtEvent::FocusGained => duat_tx.send_focused(),
                            CtEvent::FocusLost => duat_tx.send_unfocused(),
                            CtEvent::Mouse(event) => duat_tx.send_mouse(UiMouseEvent {
                                coord: ui::Coord {
                                    x: event.column as f32,
                                    y: event.row as f32,
                                },
                                kind: event.kind,
                                modifiers: event.modifiers,
                            }),
                        }
                    }
                }
            });

        Self {
            mt: Mutex::new(InnerUi {
                windows: Vec::new(),
                layouts: Layouts::default(),
                win: 0,
                border: Border::default(),
                term_tx,
            }),
        }
    }

    fn unload(&self) {
        let mut ui = self.mt.lock().unwrap();
        ui.windows = Vec::new();
        // SAFETY: Ui is not Send + Sync, so this can't be called
        // from another thread
        ui.layouts.reset();
        ui.win = 0;
        ui.term_tx.send(Event::ClearPrinter).unwrap();
    }

    fn new_root(&self, cache: <Self::Area as RawArea>::Cache) -> Self::Area {
        let mut ui = self.mt.lock().unwrap();
        let printer = Arc::new(Printer::new());

        // SAFETY: Ui::MetaStatics is not Send + Sync, so this can't be called
        // from another thread
        let main_id = ui.layouts.new_layout(printer.clone(), ui.border, cache);

        let root = Area::new(main_id, ui.layouts.clone());
        ui.windows.push((root.clone(), printer.clone()));
        if ui.windows.len() == 1 {
            ui.term_tx.send(Event::NewPrinter(printer)).unwrap();
        }

        root
    }

    fn new_dyn_spawned(
        &self,
        id: ui::SpawnId,
        specs: ui::DynSpawnSpecs,
        cache: <Self::Area as RawArea>::Cache,
        win: usize,
    ) -> Self::Area {
        let ui = self.mt.lock().unwrap();
        let id = ui.layouts.spawn_on_text(id, specs, cache, win);

        Area::new(id, ui.layouts.clone())
    }

    fn new_static_spawned(
        &self,
        id: ui::SpawnId,
        specs: ui::StaticSpawnSpecs,
        cache: <Self::Area as RawArea>::Cache,
        win: usize,
    ) -> Self::Area {
        let ui = self.mt.lock().unwrap();
        let id = ui.layouts.spawn_static(id, specs, cache, win);

        Area::new(id, ui.layouts.clone())
    }

    fn switch_window(&self, win: usize) {
        let mut ui = self.mt.lock().unwrap();
        ui.win = win;
        let printer = ui.windows[win].1.clone();
        printer.update(true, true);
        ui.term_tx.send(Event::NewPrinter(printer)).unwrap()
    }

    fn flush_layout(&self) {
        let ui = self.mt.lock().unwrap();
        if let Some((_, printer)) = ui.windows.get(ui.win) {
            printer.update(false, false);
        }
    }

    fn print(&self) {
        self.mt.lock().unwrap().term_tx.send(Event::Print).unwrap();
    }

    fn remove_window(&self, win: usize) {
        let mut ui = self.mt.lock().unwrap();
        ui.windows.remove(win);
        // SAFETY: Ui is not Send + Sync, so this can't be called
        // from another thread
        ui.layouts.remove_window(win);
        if ui.win > win {
            ui.win -= 1;
        }
    }

    fn size(&'static self) -> ui::Coord {
        let ui = self.mt.lock().unwrap();
        ui.windows[0].1.update(false, false);
        let coord = ui.windows[0].1.max_value();
        ui::Coord { x: coord.x as f32, y: coord.y as f32 }
    }
}

#[derive(Debug)]
pub enum Anchor {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

enum Event {
    Print,
    UpdatePrinter,
    ClearPrinter,
    NewPrinter(Arc<Printer>),
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

type Equality = kasuari::Constraint;

#[rustfmt::skip]
macro_rules! color_values {
    ($name:ident, $p:literal, $s:literal) => {
        macro_rules! s {
            ($n:literal) => {
                concat!($p, $n, $s).as_bytes()
            }
        }
        
        const $name: [&[u8]; 256] = [
            s!(0), s!(1), s!(2), s!(3), s!(4), s!(5), s!(6), s!(7), s!(8), s!(9), s!(10), s!(11),
            s!(12), s!(13), s!(14), s!(15), s!(16), s!(17), s!(18), s!(19), s!(20), s!(21), s!(22),
            s!(23), s!(24), s!(25), s!(26), s!(27), s!(28), s!(29), s!(30), s!(31), s!(32), s!(33),
            s!(34), s!(35), s!(36), s!(37), s!(38), s!(39), s!(40), s!(41), s!(42), s!(43), s!(44),
            s!(45), s!(46), s!(47), s!(48), s!(49), s!(50), s!(51), s!(52), s!(53), s!(54), s!(55),
            s!(56), s!(57), s!(58), s!(59), s!(60), s!(61), s!(62), s!(63), s!(64), s!(65), s!(66),
            s!(67), s!(68), s!(69), s!(70), s!(71), s!(72), s!(73), s!(74), s!(75), s!(76), s!(77),
            s!(78), s!(79), s!(80), s!(81), s!(82), s!(83), s!(84), s!(85), s!(86), s!(87), s!(88),
            s!(89), s!(90), s!(91), s!(92), s!(93), s!(94), s!(95), s!(96), s!(97), s!(98), s!(99),
            s!(100), s!(101), s!(102), s!(103), s!(104), s!(105), s!(106), s!(107), s!(108),
            s!(109), s!(110), s!(111), s!(112), s!(113), s!(114), s!(115), s!(116), s!(117),
            s!(118), s!(119), s!(120), s!(121), s!(122), s!(123), s!(124), s!(125), s!(126),
            s!(127), s!(128), s!(129), s!(130), s!(131), s!(132), s!(133), s!(134), s!(135),
            s!(136), s!(137), s!(138), s!(139), s!(140), s!(141), s!(142), s!(143), s!(144),
            s!(145), s!(146), s!(147), s!(148), s!(149), s!(150), s!(151), s!(152), s!(153),
            s!(154), s!(155), s!(156), s!(157), s!(158), s!(159), s!(160), s!(161), s!(162),
            s!(163), s!(164), s!(165), s!(166), s!(167), s!(168), s!(169), s!(170), s!(171),
            s!(172), s!(173), s!(174), s!(175), s!(176), s!(177), s!(178), s!(179), s!(180),
            s!(181), s!(182), s!(183), s!(184), s!(185), s!(186), s!(187), s!(188), s!(189),
            s!(190), s!(191), s!(192), s!(193), s!(194), s!(195), s!(196), s!(197), s!(198),
            s!(199), s!(200), s!(201), s!(202), s!(203), s!(204), s!(205), s!(206), s!(207),
            s!(208), s!(209), s!(210), s!(211), s!(212), s!(213), s!(214), s!(215), s!(216),
            s!(217), s!(218), s!(219), s!(220), s!(221), s!(222), s!(223), s!(224), s!(225),
            s!(226), s!(227), s!(228), s!(229), s!(230), s!(231), s!(232), s!(233), s!(234),
            s!(235), s!(236), s!(237), s!(238), s!(239), s!(240), s!(241), s!(242), s!(243),
            s!(244), s!(245), s!(246), s!(247), s!(248), s!(249), s!(250), s!(251), s!(252),
            s!(253), s!(254), s!(255),
        ];
    }
}

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

    w.write_all(b"\x1b[").unwrap();

    let mut semicolon = false;
    if !style.attributes.is_empty() {
        for (attr, code) in ATTRIBUTES {
            if style.attributes.has(attr) {
                if semicolon {
                    w.write_all(b";").unwrap()
                }
                w.write_all(code).unwrap();
                semicolon = true;
            }
        }
    }

    let semicolon = if let Some(color) = style.foreground_color {
        if semicolon {
            w.write_all(b";").unwrap();
        }
        match color {
            Color::Reset => w.write_all(b"39").unwrap(),
            Color::Black => w.write_all(b"30").unwrap(),
            Color::DarkRed => w.write_all(b"31").unwrap(),
            Color::DarkGreen => w.write_all(b"32").unwrap(),
            Color::DarkYellow => w.write_all(b"33").unwrap(),
            Color::DarkBlue => w.write_all(b"34").unwrap(),
            Color::DarkMagenta => w.write_all(b"35").unwrap(),
            Color::DarkCyan => w.write_all(b"36").unwrap(),
            Color::Grey => w.write_all(b"37").unwrap(),
            Color::DarkGrey => w.write_all(b"90").unwrap(),
            Color::Red => w.write_all(b"91").unwrap(),
            Color::Green => w.write_all(b"92").unwrap(),
            Color::Yellow => w.write_all(b"93").unwrap(),
            Color::Blue => w.write_all(b"94").unwrap(),
            Color::Magenta => w.write_all(b"95").unwrap(),
            Color::Cyan => w.write_all(b"96").unwrap(),
            Color::White => w.write_all(b"97").unwrap(),
            Color::Rgb { r, g, b } => {
                w.write_all(U8_FG_RGB[r as usize]).unwrap();
                w.write_all(U8_SC[g as usize]).unwrap();
                w.write_all(U8[b as usize]).unwrap()
            }
            Color::AnsiValue(val) => w.write_all(U8_FG_ANSI[val as usize]).unwrap(),
        };
        true
    } else {
        semicolon
    };

    let semicolon = if let Some(color) = style.background_color {
        if semicolon {
            w.write_all(b";").unwrap();
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
                w.write_all(U8_BG_RGB[r as usize]).unwrap();
                w.write_all(U8_SC[g as usize]).unwrap();
                w.write_all(U8[b as usize]).unwrap()
            }
            Color::AnsiValue(val) => w.write_all(U8_BG_ANSI[val as usize]).unwrap(),
        };
        true
    } else {
        semicolon
    };

    if let Some(color) = style.underline_color {
        if semicolon {
            w.write_all(b";").unwrap();
        }
        match color {
            Color::Reset => w.write_all(b"59").unwrap(),
            Color::Black => w.write_all(b"58;5;0").unwrap(),
            Color::DarkRed => w.write_all(b"58;5;1").unwrap(),
            Color::DarkGreen => w.write_all(b"58;5;2").unwrap(),
            Color::DarkYellow => w.write_all(b"58;5;3").unwrap(),
            Color::DarkBlue => w.write_all(b"58;5;4").unwrap(),
            Color::DarkMagenta => w.write_all(b"58;5;5").unwrap(),
            Color::DarkCyan => w.write_all(b"58;5;6").unwrap(),
            Color::Grey => w.write_all(b"58;5;7").unwrap(),
            Color::DarkGrey => w.write_all(b"58;5;8").unwrap(),
            Color::Red => w.write_all(b"58;5;9").unwrap(),
            Color::Green => w.write_all(b"58;5;10").unwrap(),
            Color::Yellow => w.write_all(b"58;5;11").unwrap(),
            Color::Blue => w.write_all(b"58;5;12").unwrap(),
            Color::Magenta => w.write_all(b"58;5;13").unwrap(),
            Color::Cyan => w.write_all(b"58;5;14").unwrap(),
            Color::White => w.write_all(b"58;5;15").unwrap(),
            Color::Rgb { r, g, b } => {
                w.write_all(U8_UL_RGB[r as usize]).unwrap();
                w.write_all(U8_SC[g as usize]).unwrap();
                w.write_all(U8[b as usize]).unwrap()
            }
            Color::AnsiValue(val) => w.write_all(U8_UL_ANSI[val as usize]).unwrap(),
        };
    }

    w.write_all(b"m").unwrap();
}

/// The priority for edges for areas that must not overlap
const EDGE_PRIO: kasuari::Strength = kasuari::Strength::REQUIRED;
/// The priority for manually defined lengths
const MANUAL_LEN_PRIO: kasuari::Strength = kasuari::Strength::new(12.0);
/// The priority for lengths defined when creating Areas
const LEN_PRIO: kasuari::Strength = kasuari::Strength::new(11.0);
/// The priority for borders
const BORDER_PRIO: kasuari::Strength = kasuari::Strength::new(10.0);
/// The priority for hiding things
const HIDDEN_PRIO: kasuari::Strength = kasuari::Strength::new(9.0);
/// The priority for the length of spawned Areas
const SPAWN_LEN_PRIO: kasuari::Strength = kasuari::Strength::new(8.0);
/// The priority for positioning of dynamically spawned Areas
const DYN_SPAWN_POS_PRIO: kasuari::Strength = kasuari::Strength::new(7.0);
/// The priority for the center and len variables of spawned Areas
const SPAWN_DIMS_PRIO: kasuari::Strength = kasuari::Strength::new(6.0);
/// The priority for frames around spawned Areas
const FRAME_PRIO: kasuari::Strength = kasuari::Strength::new(5.0);
/// The priority for the length of spawned Areas
const CONS_SPAWN_LEN_PRIO: kasuari::Strength = kasuari::Strength::new(4.0);
/// The priority for positioning of statically spawned Areas
const STATIC_SPAWN_POS_PRIO: kasuari::Strength = kasuari::Strength::new(3.0);
/// The priority for the alignment of spawned Areas
const SPAWN_ALIGN_PRIO: kasuari::Strength = kasuari::Strength::new(2.0);
/// The priority for lengths that should try to be equal (a.k.a Files)
const EQ_LEN_PRIO: kasuari::Strength = kasuari::Strength::new(1.0);
