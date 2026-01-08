//! A terminal implementation for Duat's Ui
//!
//! This implementation is a sort of trial of the "preparedness" of
//! [`RawUi`]'s API, in order to figure out what should be included
//! and what shouldn't.
use std::{
    fmt::Debug,
    io::{self, Write},
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
        mpsc,
    },
    time::Duration,
};

use crossterm::{
    cursor,
    event::{self, Event as CtEvent, poll as ct_poll, read as ct_read},
    execute, queue,
    style::ContentStyle,
    terminal::{self, ClearType},
};
use duat_core::{
    context::DuatSender,
    form::{self, Color},
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
    shared_fns::*,
};
use crate::layout::Layouts;

mod area;
mod layout;
mod printer;
mod rules;

mod shared_fns {
    use std::sync::OnceLock;

    use crate::FrameStyle;

    /// Execution address space functions
    #[derive(Clone, Copy)]
    pub(super) struct ExecSpaceFns {
        set_default_frame_style: fn(FrameStyle),
    }

    impl Default for ExecSpaceFns {
        fn default() -> Self {
            Self {
                set_default_frame_style: crate::layout::set_default_frame_style,
            }
        }
    }

    pub(super) static FNS: OnceLock<ExecSpaceFns> = OnceLock::new();

    /// Sets the default [`FrameStyle`] for all spawned [`Area`]s
    ///
    /// By default, it is [`FrameStyle::Regular`], which uses
    /// characters like `─`, `│` and `┐`.
    ///
    /// [`Area`]: crate::Area
    pub fn set_default_frame_style(frame_style: FrameStyle) {
        (FNS.get().unwrap().set_default_frame_style)(frame_style)
    }

    /// Resetting functions, right before reloading.
    pub(super) fn reset_state() {
        set_default_frame_style(FrameStyle::Regular);
    }
}

/// The [`RawUi`] implementation for `duat-term`
pub struct Ui {
    mt: Mutex<InnerUi>,
    fns: shared_fns::ExecSpaceFns,
}

struct InnerUi {
    windows: Vec<(Area, Arc<Printer>)>,
    layouts: Layouts,
    win: usize,
    frame: Border,
    printer_fn: fn() -> Arc<Printer>,
    rx: Option<mpsc::Receiver<Event>>,
    tx: mpsc::Sender<Event>,
}

// SAFETY: The Area (the part that is not Send + Sync) is only ever
// accessed from the main thread.
unsafe impl Send for InnerUi {}

impl RawUi for Ui {
    type Area = Area;

    fn get_once() -> Option<&'static Self> {
        static GOT: AtomicBool = AtomicBool::new(false);
        let (tx, rx) = mpsc::channel();
        shared_fns::FNS.set(ExecSpaceFns::default()).ok().unwrap();

        (!GOT.fetch_or(true, Ordering::Relaxed)).then(|| {
            Box::leak(Box::new(Self {
                mt: Mutex::new(InnerUi {
                    windows: Vec::new(),
                    layouts: Layouts::default(),
                    win: 0,
                    frame: Border::default(),
                    printer_fn: || Arc::new(Printer::new()),
                    rx: Some(rx),
                    tx,
                }),
                fns: ExecSpaceFns::default(),
            })) as &'static Self
        })
    }

    fn config_address_space_setup(&'static self) {
        shared_fns::FNS.set(self.fns).ok().unwrap();
    }

    fn open(&self, duat_tx: DuatSender) {
        use event::{KeyboardEnhancementFlags as KEF, PushKeyboardEnhancementFlags};

        form::set_weak("rule.upper", "default.VertRule");
        form::set_weak("rule.lower", "default.VertRule");

        let term_rx = self.mt.lock().unwrap().rx.take().unwrap();
        let term_tx = self.mt.lock().unwrap().tx.clone();

        let print_thread = std::thread::Builder::new().name("print loop".to_string());
        let _ = print_thread.spawn(move || {
            // Wait for everything to be setup before doing anything to the
            // terminal, for a less jarring effect.
            let Ok(Event::NewPrinter(mut printer)) = term_rx.recv() else {
                unreachable!("Failed to load the Ui");
            };

            terminal::enable_raw_mode().unwrap();

            // Initial terminal setup
            // Some key chords (like alt+shift+o for some reason) don't work
            // without this.
            execute!(
                io::stdout(),
                terminal::EnterAlternateScreen,
                terminal::Clear(ClearType::All),
                terminal::DisableLineWrap,
                event::EnableFocusChange,
                event::EnableMouseCapture
            )
            .unwrap();

            if let Ok(true) = terminal::supports_keyboard_enhancement() {
                execute!(
                    io::stdout(),
                    PushKeyboardEnhancementFlags(
                        KEF::DISAMBIGUATE_ESCAPE_CODES | KEF::REPORT_ALTERNATE_KEYS
                    )
                )
                .unwrap();
            }

            loop {
                match term_rx.recv() {
                    Ok(Event::Print) => printer.print(),
                    Ok(Event::UpdatePrinter) => printer.update(true, true),
                    Ok(Event::ClearPrinter) => printer.clear(),
                    Ok(Event::NewPrinter(new_printer)) => printer = new_printer,
                    Ok(Event::Quit) => break,
                    Err(_) => {}
                }
            }
        });

        let _ = std::thread::Builder::new()
            .name("crossterm".to_string())
            .spawn(move || {
                loop {
                    let Ok(true) = ct_poll(Duration::from_millis(20)) else {
                        continue;
                    };

                    match ct_read() {
                        Ok(CtEvent::Key(key)) => {
                            if !matches!(key.kind, event::KeyEventKind::Release) {
                                duat_tx.send_key(key);
                            }
                        }
                        Ok(CtEvent::Resize(..)) => {
                            term_tx.send(Event::UpdatePrinter).unwrap();
                            duat_tx.send_resize();
                        }
                        Ok(CtEvent::FocusGained) => duat_tx.send_focused(),
                        Ok(CtEvent::FocusLost) => duat_tx.send_unfocused(),
                        Ok(CtEvent::Mouse(event)) => duat_tx.send_mouse(UiMouseEvent {
                            coord: ui::Coord {
                                x: event.column as f32,
                                y: event.row as f32,
                            },
                            kind: event.kind,
                            modifiers: event.modifiers,
                        }),
                        Err(_) => {}
                    }
                }
            });
    }

    fn close(&self) {
        if self.mt.lock().unwrap().tx.send(Event::Quit).is_err() {
            return;
        }

        if let Ok(true) = terminal::supports_keyboard_enhancement() {
            queue!(io::stdout(), event::PopKeyboardEnhancementFlags).unwrap();
        }

        execute!(
            io::stdout(),
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

    fn new_root(&self, cache: <Self::Area as RawArea>::Cache) -> Self::Area {
        let mut ui = self.mt.lock().unwrap();
        let printer = (ui.printer_fn)();

        // SAFETY: Ui::MetaStatics is not Send + Sync, so this can't be called
        // from another thread
        let main_id = ui.layouts.new_layout(printer.clone(), ui.frame, cache);

        let root = Area::new(main_id, ui.layouts.clone());
        ui.windows.push((root.clone(), printer.clone()));
        if ui.windows.len() == 1 {
            ui.tx.send(Event::NewPrinter(printer)).unwrap();
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
        ui.tx.send(Event::NewPrinter(printer)).unwrap()
    }

    fn flush_layout(&self) {
        let ui = self.mt.lock().unwrap();
        if let Some((_, printer)) = ui.windows.get(ui.win) {
            printer.update(false, false);
        }
    }

    fn print(&self) {
        self.mt.lock().unwrap().tx.send(Event::Print).unwrap();
    }

    fn load(&'static self) {
        // Hook for returning to regular terminal state
        std::panic::set_hook(Box::new(|info| {
            self.close();
            println!("{info}");
        }));
    }

    fn unload(&self) {
        let mut ui = self.mt.lock().unwrap();
        ui.windows = Vec::new();
        // SAFETY: Ui is not Send + Sync, so this can't be called
        // from another thread
        ui.layouts.reset();
        ui.win = 0;
        ui.tx.send(Event::ClearPrinter).unwrap();
        shared_fns::reset_state();
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

type Equality = kasuari::Constraint;

#[rustfmt::skip]
macro_rules! color_values {
    ($name:ident, $p:literal, $s:literal) => {
        macro_rules! c {
            ($n:literal) => {
                concat!($p, $n, $s).as_bytes()
            }
        }
        
        const $name: [&[u8]; 256] = [
            c!(0), c!(1), c!(2), c!(3), c!(4), c!(5), c!(6), c!(7), c!(8), c!(9), c!(10), c!(11),
            c!(12), c!(13), c!(14), c!(15), c!(16), c!(17), c!(18), c!(19), c!(20), c!(21), c!(22),
            c!(23), c!(24), c!(25), c!(26), c!(27), c!(28), c!(29), c!(30), c!(31), c!(32), c!(33),
            c!(34), c!(35), c!(36), c!(37), c!(38), c!(39), c!(40), c!(41), c!(42), c!(43), c!(44),
            c!(45), c!(46), c!(47), c!(48), c!(49), c!(50), c!(51), c!(52), c!(53), c!(54), c!(55),
            c!(56), c!(57), c!(58), c!(59), c!(60), c!(61), c!(62), c!(63), c!(64), c!(65), c!(66),
            c!(67), c!(68), c!(69), c!(70), c!(71), c!(72), c!(73), c!(74), c!(75), c!(76), c!(77),
            c!(78), c!(79), c!(80), c!(81), c!(82), c!(83), c!(84), c!(85), c!(86), c!(87), c!(88),
            c!(89), c!(90), c!(91), c!(92), c!(93), c!(94), c!(95), c!(96), c!(97), c!(98), c!(99),
            c!(100), c!(101), c!(102), c!(103), c!(104), c!(105), c!(106), c!(107), c!(108),
            c!(109), c!(110), c!(111), c!(112), c!(113), c!(114), c!(115), c!(116), c!(117),
            c!(118), c!(119), c!(120), c!(121), c!(122), c!(123), c!(124), c!(125), c!(126),
            c!(127), c!(128), c!(129), c!(130), c!(131), c!(132), c!(133), c!(134), c!(135),
            c!(136), c!(137), c!(138), c!(139), c!(140), c!(141), c!(142), c!(143), c!(144),
            c!(145), c!(146), c!(147), c!(148), c!(149), c!(150), c!(151), c!(152), c!(153),
            c!(154), c!(155), c!(156), c!(157), c!(158), c!(159), c!(160), c!(161), c!(162),
            c!(163), c!(164), c!(165), c!(166), c!(167), c!(168), c!(169), c!(170), c!(171),
            c!(172), c!(173), c!(174), c!(175), c!(176), c!(177), c!(178), c!(179), c!(180),
            c!(181), c!(182), c!(183), c!(184), c!(185), c!(186), c!(187), c!(188), c!(189),
            c!(190), c!(191), c!(192), c!(193), c!(194), c!(195), c!(196), c!(197), c!(198),
            c!(199), c!(200), c!(201), c!(202), c!(203), c!(204), c!(205), c!(206), c!(207),
            c!(208), c!(209), c!(210), c!(211), c!(212), c!(213), c!(214), c!(215), c!(216),
            c!(217), c!(218), c!(219), c!(220), c!(221), c!(222), c!(223), c!(224), c!(225),
            c!(226), c!(227), c!(228), c!(229), c!(230), c!(231), c!(232), c!(233), c!(234),
            c!(235), c!(236), c!(237), c!(238), c!(239), c!(240), c!(241), c!(242), c!(243),
            c!(244), c!(245), c!(246), c!(247), c!(248), c!(249), c!(250), c!(251), c!(252),
            c!(253), c!(254), c!(255),
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
/// The priority for frames around spawned Areas
const FRAME_PRIO: kasuari::Strength = kasuari::Strength::new(8.0);
/// The priority for positioning of dynamically spawned Areas
const DYN_SPAWN_POS_PRIO: kasuari::Strength = kasuari::Strength::new(7.0);
/// The priority for the center and len variables of spawned Areas
const SPAWN_DIMS_PRIO: kasuari::Strength = kasuari::Strength::new(6.0);
/// The priority for the length of spawned Areas
const SPAWN_LEN_PRIO: kasuari::Strength = kasuari::Strength::new(5.0);
/// The priority for the length of spawned Areas
const CONS_SPAWN_LEN_PRIO: kasuari::Strength = kasuari::Strength::new(4.0);
/// The priority for positioning of statically spawned Areas
const STATIC_SPAWN_POS_PRIO: kasuari::Strength = kasuari::Strength::new(3.0);
/// The priority for the alignment of spawned Areas
const SPAWN_ALIGN_PRIO: kasuari::Strength = kasuari::Strength::new(2.0);
/// The priority for lengths that should try to be equal (a.k.a Files)
const EQ_LEN_PRIO: kasuari::Strength = kasuari::Strength::new(1.0);
