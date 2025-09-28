use std::{
    any::{Any, TypeId},
    cell::RefCell,
    sync::{LazyLock, Mutex},
    vec::IntoIter,
};

use crossterm::event::KeyEvent;

use super::Mode;
use crate::{
    context::{self, Handle},
    data::Pass,
    file::{File, PathKind},
    hook::{self, KeysSent, KeysSentTo, ModeCreated, ModeSwitched},
    main_thread_only::MainThreadOnly,
    session::DuatEvent,
    ui::{Node, Ui, Widget},
    utils::duat_name,
};

static SEND_KEYS: MainThreadOnly<RefCell<Option<KeyFn>>> = MainThreadOnly::new(RefCell::new(None));
static RESET_MODES: Mutex<Vec<(TypeId, Box<dyn FnMut(&mut Pass) -> bool + Send>)>> =
    Mutex::new(Vec::new());
static SET_MODE: Mutex<Option<ModeFn>> = Mutex::new(None);
static MODE: LazyLock<MainThreadOnly<RefCell<Box<dyn Any>>>> =
    LazyLock::new(|| MainThreadOnly::new(RefCell::new(Box::new("no mode") as Box<dyn Any>)));
static BEFORE_EXIT: MainThreadOnly<RefCell<fn(&mut Pass)>> =
    MainThreadOnly::new(RefCell::new(|_| {}));

type KeyFn = fn(&mut Pass, &mut IntoIter<KeyEvent>) -> Option<ModeFn>;
type ModeFn = Box<dyn FnOnce(&mut Pass) -> bool + Send>;

/// Whether or not the [`Mode`] has changed
///
/// Since this function is only called by Duat, I can ensure that
/// it will be called from the main thread, so no checks are done
/// in that regard.
pub(crate) fn take_set_mode_fn(_: &mut Pass) -> Option<ModeFn> {
    SET_MODE.lock().unwrap().take()
}

/// Sets the new default mode
///
/// This is the mode that will be set when [`mode::reset`] is
/// called.
///
/// [`mode::reset`]: reset
pub fn set_default<M: Mode<U>, U: Ui>(mode: M) {
    let mut reset_modes = RESET_MODES.lock().unwrap();

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
        let mut set_mode = SET_MODE.lock().unwrap();
        let prev = set_mode.take();
        *set_mode = Some(Box::new(move |pa| {
            if let Some(f) = prev {
                f(pa);
            }
            RESET_MODES.lock().unwrap()[i].1(pa)
        }));
    }
}

/// Sets the [`Mode`], switching to the appropriate [`Widget`]
///
/// [`Widget`]: Mode::Widget
pub fn set<U: Ui>(mode: impl Mode<U>) {
    let mut set_mode = SET_MODE.lock().unwrap();
    let prev = set_mode.take();
    *set_mode = Some(Box::new(move |pa| {
        if let Some(prev) = prev {
            prev(pa);
        }
        set_mode_fn(pa, mode)
    }));
}

/// Resets the mode to the [default] of a given [`Widget`]
///
/// Does nothing if no default was set for the given [`Widget`].
///
/// [default]: set_default
pub fn reset<W: Widget<U>, U: Ui>() {
    let reset_modes = RESET_MODES.lock().unwrap();
    if let Some(i) = reset_modes
        .iter()
        .position(|(ty, _)| *ty == TypeId::of::<W>())
    {
        *SET_MODE.lock().unwrap() = Some(Box::new(move |pa| RESET_MODES.lock().unwrap()[i].1(pa)));
    } else if TypeId::of::<W>() == TypeId::of::<File<U>>() {
        panic!("Something went terribly wrong, somehow");
    } else {
        context::error!(
            "There is no default [a]Mode[] set for [a]{}[]",
            crate::utils::duat_name::<W>()
        );
    };
}

/// Resets to the default [`Mode`] of the given [`Widget`], on a
/// given [`Handle<W, U>`]
pub fn reset_to<U: Ui>(handle: Handle<dyn Widget<U>, U>) {
    let reset_modes = RESET_MODES.lock().unwrap();

    let i = reset_modes
        .iter()
        .position(|(ty, _)| *ty == handle.widget().type_id());

    if let Some(i) = i {
        *SET_MODE.lock().unwrap() = Some(Box::new(move |pa| {
            let node = context::windows::<U>()
                .entries(pa)
                .find(|(.., node)| node.ptr_eq(handle.widget()))
                .map(|(.., node)| node.clone());

            if let Some(node) = node {
                let node = node.clone();
                switch_widget(pa, node);
                (RESET_MODES.lock().unwrap()[i].1)(pa)
            } else {
                context::error!("The Handle in question is no longer in use");
                false
            }
        }));
    } else if handle.widget().type_id() == TypeId::of::<File<U>>() {
        panic!("Something went terribly wrong, somehow");
    } else {
        context::error!("There is no default [a]Mode[] set for the [a]Widget",);
    };
}

/// Switches to the [`File`] with the given name
pub(crate) fn reset_to_file<U: Ui>(path_kind: PathKind, switch_window: bool) {
    *SET_MODE.lock().unwrap() = Some(Box::new(move |pa| {
        match context::windows::<U>().file_entry(pa, path_kind) {
            Ok((win, _, handle)) => {
                if win != context::cur_window() && switch_window {
                    crate::context::sender()
                        .send(DuatEvent::SwitchWindow(win))
                        .unwrap();
                }

                if let Some((_, f)) = RESET_MODES
                    .lock()
                    .unwrap()
                    .iter_mut()
                    .find(|(ty, _)| *ty == TypeId::of::<File<U>>())
                {
                    switch_widget(pa, Node::from_handle(handle));
                    f(pa)
                } else {
                    panic!("Something went terribly wrong, somehow");
                }
            }
            Err(err) => {
                context::error!("{err}");
                false
            }
        }
    }))
}

/// Switches to a certain widget
pub(super) fn switch_widget<U: Ui>(pa: &mut Pass, node: Node<U>) {
    let cur_widget = context::cur_widget::<U>(pa).unwrap();
    unsafe { BEFORE_EXIT.get() }.replace(|_| {})(pa);

    let handle = node.handle().clone();
    cur_widget.node(pa).on_unfocus(pa, handle);

    context::set_cur(pa, node.try_downcast(), node.clone());

    node.on_focus(pa, cur_widget.node(pa).handle().clone());
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
    let handle = context::cur_widget::<U>(pa)
        .map(|cw| cw.node(pa))
        .unwrap()
        .try_downcast()
        .unwrap();

    let mut sent_keys = Vec::new();

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

    hook::trigger(pa, KeysSentTo::<M, U>((sent_keys.clone(), handle.clone())));
    hook::trigger(pa, KeysSent(sent_keys));

    mode_fn
}

/// Static dispatch function to set the [`Mode`]
fn set_mode_fn<M: Mode<U>, U: Ui>(pa: &mut Pass, mode: M) -> bool {
    // If we are on the correct widget, no switch is needed.
    if context::cur_widget::<U>(pa).unwrap().type_id(pa) != TypeId::of::<M::Widget>() {
        let node = {
            let windows = context::windows();
            let w = context::cur_window();
            if TypeId::of::<M::Widget>() == TypeId::of::<File<U>>() {
                let pk = context::fixed_file::<U>(pa).unwrap().read(pa).path_kind();
                windows
                    .file_entry(pa, pk)
                    .map(|(.., handle)| Node::from_handle(handle))
            } else {
                windows
                    .widget_entry::<M::Widget>(pa, w)
                    .map(|(.., node)| node.clone())
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

    let handle = wid
        .mutate_data_as(pa, |handle: &Handle<M::Widget, U>| handle.clone())
        .unwrap();

    let mc = ModeCreated((Some(mode), handle.clone()));
    let mut mode = hook::trigger(pa, mc).0.0.unwrap();

    mode.on_switch(pa, handle.clone());

    // SAFETY: There is a Pass argument.
    unsafe {
        crate::mode::set_send_key::<M, U>();
    }

    let new_name = duat_name::<M>();
    let old_name = std::mem::replace(context::raw_mode_name().write(pa), new_name);

    hook::trigger(pa, ModeSwitched((old_name, new_name)));

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

    let handle = wid
        .mutate_data_as(pa, |handle: &Handle<M::Widget, U>| handle.clone())
        .unwrap();

    // SAFETY: This function's caller has a Pass argument.
    let mut mode = unsafe { MODE.get() }.borrow_mut();
    let mode: &mut M = mode.downcast_mut().unwrap();

    mode.before_exit(pa, handle);
}
