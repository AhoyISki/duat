use std::{
    any::{Any, TypeId},
    cell::RefCell,
    sync::LazyLock,
    vec::IntoIter,
};

use crossterm::event::KeyEvent;

use super::Mode;
use crate::{
    context::{self, Handle},
    data::{Pass, RwData},
    duat_name,
    file::File,
    file_entry,
    hook::{self, KeysSent, KeysSentTo, ModeCreated, ModeSwitched},
    main_thread_only::MainThreadOnly,
    ui::{DuatEvent, Node, Ui, Widget},
    widget_entry,
};

static SEND_KEYS: MainThreadOnly<RefCell<Option<KeyFn>>> = MainThreadOnly::new(RefCell::new(None));
static RESET_MODES: MainThreadOnly<RefCell<Vec<(TypeId, Box<dyn FnMut(&mut Pass) -> bool>)>>> =
    MainThreadOnly::new(RefCell::new(Vec::new()));
static SET_MODE: MainThreadOnly<RefCell<Option<ModeFn>>> = MainThreadOnly::new(RefCell::new(None));
static MODE: LazyLock<MainThreadOnly<RefCell<Box<dyn Any>>>> =
    LazyLock::new(|| MainThreadOnly::new(RefCell::new(Box::new("no mode") as Box<dyn Any>)));
static BEFORE_EXIT: MainThreadOnly<RefCell<fn(&mut Pass)>> =
    MainThreadOnly::new(RefCell::new(|_| {}));

type KeyFn = fn(&mut Pass, &mut IntoIter<KeyEvent>) -> Option<ModeFn>;
type ModeFn = Box<dyn FnOnce(&mut Pass) -> bool>;

/// Whether or not the [`Mode`] has changed
///
/// Since this function is only called by Duat, I can ensure that
/// it will be called from the main thread, so no checks are done
/// in that regard.
pub(crate) fn take_set_mode_fn(_: &mut Pass) -> Option<ModeFn> {
    unsafe { SET_MODE.get() }.borrow_mut().take()
}

/// Sets the new default mode
///
/// This is the mode that will be set when [`mode::reset`] is
/// called.
///
/// [`mode::reset`]: reset
pub fn set_default<M: Mode<U>, U: Ui>(mode: M) {
    crate::context::queue(move |_| {
        let mut reset_modes = unsafe { RESET_MODES.get() }.borrow_mut();

        let i = if let Some(i) = reset_modes
            .iter()
            .position(|(ty, _)| *ty == TypeId::of::<M::Widget>())
        {
            reset_modes[i].1 = Box::new(move |pa| {
                let mode = mode.clone();
                set_mode_fn::<M, U>(pa, mode)
            });
            i
        } else {
            reset_modes.push((
                TypeId::of::<M::Widget>(),
                Box::new(move |pa| {
                    let mode = mode.clone();
                    set_mode_fn::<M, U>(pa, mode)
                }),
            ));
            reset_modes.len() - 1
        };

        if TypeId::of::<M::Widget>() == TypeId::of::<File<U>>() {
            let set_mode = unsafe { SET_MODE.get() };
            let prev = set_mode.take();
            *set_mode.borrow_mut() = Some(Box::new(move |pa| {
                if let Some(f) = prev {
                    f(pa);
                    unsafe { RESET_MODES.get() }.borrow_mut()[i].1(pa)
                } else {
                    unsafe { RESET_MODES.get() }.borrow_mut()[i].1(pa)
                }
            }));
        }
    });
}

/// Sets the [`Mode`], switching to the appropriate [`Widget`]
///
/// [`Widget`]: Mode::Widget
pub fn set<U: Ui>(mode: impl Mode<U>) {
    crate::context::queue(move |_| {
        let set_mode = unsafe { SET_MODE.get() };
        let prev = set_mode.take();
        *set_mode.borrow_mut() = Some(Box::new(move |pa| {
            if let Some(prev) = prev {
                prev(pa);
            }
            set_mode_fn(pa, mode)
        }));
    });
}

/// Resets the mode to the [default] of a given [`Widget`]
///
/// Does nothing if no default was set for the given [`Widget`].
///
/// [default]: set_default
pub fn reset<W: Widget<U>, U: Ui>() {
    crate::context::queue(move |_| {
        let reset_modes = unsafe { RESET_MODES.get() }.borrow_mut();
        if let Some(i) = reset_modes
            .iter()
            .position(|(ty, _)| *ty == TypeId::of::<W>())
        {
            *unsafe { SET_MODE.get() }.borrow_mut() = Some(Box::new(move |pa| {
                unsafe { RESET_MODES.get() }.borrow_mut()[i].1(pa)
            }));
        } else if TypeId::of::<W>() == TypeId::of::<File<U>>() {
            panic!("Something went terribly wrong, somehow");
        } else {
            context::error!(
                "There is no default [a]Mode[] set for [a]{}[]",
                crate::duat_name::<W>()
            );
        };
    })
}

/// Resets to the default [`Mode`] of the given [`Widget`], on a
/// given [`Handle<W, U>`]
pub fn reset_to<W: Widget<U>, U: Ui>(handle: Handle<W, U>) {
    crate::context::queue(move |pa| {
        let reset_modes = unsafe { RESET_MODES.get() }.borrow_mut();
        let windows = context::windows::<U>(pa).borrow();

        let i = reset_modes
            .iter()
            .position(|(ty, _)| *ty == TypeId::of::<W>());

        if let Some(i) = i {
            if let Some(node) = windows
                .iter()
                .flat_map(|w| w.nodes())
                .find(|n| n.ptr_eq(handle.widget()))
                .cloned()
            {
                *unsafe { SET_MODE.get() }.borrow_mut() = Some(Box::new(move |pa| {
                    switch_widget(pa, node);
                    (unsafe { RESET_MODES.get() }.borrow_mut()[i].1)(pa)
                }));
            } else {
                context::error!("The Handle in question is no longer in use",);
            }
        } else if TypeId::of::<W>() == TypeId::of::<File<U>>() {
            panic!("Something went terribly wrong, somehow");
        } else {
            context::error!(
                "There is no default [a]Mode[] set for [a]{}[]",
                crate::duat_name::<W>()
            );
        };
    })
}

/// Switches to the [`File`] with the given name
pub(crate) fn reset_to_file<U: Ui>(pa: &Pass, name: impl std::fmt::Display, switch_window: bool) {
    let windows = context::windows::<U>(pa).borrow();
    let name = name.to_string();
    match file_entry(pa, &windows, &name) {
        Ok((win, _, node)) => {
            if win != context::cur_window() && switch_window {
                crate::context::sender()
                    .send(DuatEvent::SwitchWindow(win))
                    .unwrap();
            }
            let node = node.clone();
            // SAFETY: There is a Pass argument.
            let set_mode = unsafe { SET_MODE.get() };
            *set_mode.borrow_mut() = Some(Box::new(move |pa| {
                if let Some((_, f)) = unsafe { RESET_MODES.get() }
                    .borrow_mut()
                    .iter_mut()
                    .find(|(ty, _)| *ty == TypeId::of::<File<U>>())
                {
                    switch_widget(pa, node);
                    f(pa)
                } else {
                    panic!("Something went terribly wrong, somehow");
                }
            }))
        }
        Err(err) => context::error!("{err}"),
    }
}

/// Switches to a certain widget
pub(super) fn switch_widget<U: Ui>(pa: &mut Pass, node: Node<U>) {
    let cur_widget = context::cur_widget::<U>(pa).unwrap();
    unsafe { BEFORE_EXIT.get() }.replace(|_| {})(pa);

    let handle = {
        let (widget, area, mask, _) = node.parts();
        Handle::from_parts(widget.clone(), area.clone(), mask.clone())
    };
    cur_widget.node(pa).on_unfocus(pa, handle);

    context::set_cur(pa, node.as_file(), node.clone());

    let handle = {
        let node = cur_widget.node(pa);
        let (widget, area, mask, _) = node.parts();
        Handle::from_parts(widget.clone(), area.clone(), mask.clone())
    };
    node.on_focus(pa, handle);
}

/// Sends the [`KeyEvent`] to the active [`Mode`]
pub(super) fn send_keys_to(pa: &mut Pass, keys: Vec<KeyEvent>) {
    let mut keys = keys.into_iter();
    // SAFETY: There is a Pass argument.
    let send_keys = unsafe { SEND_KEYS.get() };
    let mut sk = send_keys.take().unwrap();

    while keys.len() > 0 {
        if let Some(set_mode) = sk(pa, &mut keys)
            && set_mode(pa)
        {
            sk = send_keys.take().unwrap();
        } else {
            // You probably don't really want to send the remaining keys to the
            // current mode if set_mode fails.
            break;
        }
    }

    send_keys.replace(Some(sk));
}

/// Static dispatch function that sends keys to a [`Mode`]
#[allow(clippy::await_holding_refcell_ref)]
fn send_keys_fn<M: Mode<U>, U: Ui>(pa: &mut Pass, keys: &mut IntoIter<KeyEvent>) -> Option<ModeFn> {
    let node = context::cur_widget::<U>(pa).map(|cw| cw.node(pa)).unwrap();

    let (widget, area, mask, _) = node.parts();
    let widget = widget.try_downcast::<M::Widget>().unwrap();

    let mut sent_keys = Vec::new();
    let handle = Handle::from_parts(widget.clone(), area.clone(), mask.clone());

    let mode_fn = {
        // SAFETY: This function's caller has a Pass argument.
        let mut mode = unsafe { MODE.get() }.borrow_mut();
        let mode: &mut M = mode.downcast_mut().unwrap();

        loop {
            if let Some(mode_fn) = take_set_mode_fn(pa) {
                break Some(mode_fn);
            }
            let Some(key) = keys.next() else { break None };
            sent_keys.push(key);

            mode.send_key(pa, key, handle.clone());
        }
    };

    hook::trigger(pa, KeysSentTo((sent_keys.clone(), handle.clone())));
    hook::trigger(pa, KeysSent(sent_keys));

    mode_fn
}

/// Static dispatch function to set the [`Mode`]
fn set_mode_fn<M: Mode<U>, U: Ui>(pa: &mut Pass, mode: M) -> bool {
    // If we are on the correct widget, no switch is needed.
    if context::cur_widget::<U>(pa).unwrap().type_id(pa) != TypeId::of::<M::Widget>() {
        let node = {
            let windows = context::windows(pa).borrow();
            let w = context::cur_window();
            if TypeId::of::<M::Widget>() == TypeId::of::<File<U>>() {
                let name = context::fixed_file::<U>(pa)
                    .unwrap()
                    .read(pa, |file, _| file.name());
                file_entry(pa, &windows, &name).map(|(.., node)| node.clone())
            } else {
                widget_entry::<M::Widget, U>(pa, &windows, w).map(|(.., node)| node.clone())
            }
        };

        match node {
            Ok(node) => switch_widget(pa, node),
            Err(err) => {
                context::error!("{err}");
                return false;
            }
        };
    } else {
        unsafe { BEFORE_EXIT.get() }.borrow_mut()(pa);
    }

    let wid = context::cur_widget::<U>(pa).unwrap();
    wid.node(pa).parts();
    // SAFETY: Other than the internal borrow in CurWidget, no other
    // borrows happen
    let (wid, area, mask) = wid
        .mutate_data_as(pa, |w: &RwData<M::Widget>, area, mask, _| {
            (w.clone(), area.clone(), mask.clone())
        })
        .unwrap();

    let handle = Handle::from_parts(wid, area, mask);
    let mc = ModeCreated((Some(mode), handle.clone()));
    let mut mode = hook::trigger(pa, mc).0.0.unwrap();

    mode.on_switch(pa, handle.clone());

    // SAFETY: There is a Pass argument.
    unsafe {
        crate::mode::set_send_key::<M, U>();
    }

    let (old, new) = context::raw_mode_name().write(pa, |old_mode| {
        let new_mode = duat_name::<M>();
        let ret = (*old_mode, new_mode);
        *old_mode = new_mode;
        ret
    });

    hook::trigger(pa, ModeSwitched((old, new)));

    unsafe {
        MODE.get().replace(Box::new(mode));
        SEND_KEYS
            .get()
            .replace(Some(|pa, keys| send_keys_fn::<M, U>(pa, keys)));
        BEFORE_EXIT.get().replace(|pa| before_exit_fn::<M, U>(pa));
    }

    true
}

/// Static dispatch function to use before exiting a given
/// [`Mode`]
fn before_exit_fn<M: Mode<U>, U: Ui>(pa: &mut Pass) {
    let wid = context::cur_widget(pa).unwrap();

    let (wid, area, mask) = wid
        .mutate_data_as(pa, |w: &RwData<M::Widget>, area, mask, _| {
            (w.clone(), area.clone(), mask.clone())
        })
        .unwrap();

    let handle = Handle::from_parts(wid, area, mask);

    // SAFETY: This function's caller has a Pass argument.
    let mut mode = unsafe { MODE.get() }.borrow_mut();
    let mode: &mut M = mode.downcast_mut().unwrap();

    mode.before_exit(pa, handle);
}
