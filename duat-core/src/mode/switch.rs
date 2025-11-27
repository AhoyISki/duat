use std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
    sync::{LazyLock, Mutex},
    vec::IntoIter,
};

use crossterm::event::KeyEvent;

use super::Mode;
use crate::{
    buffer::Buffer,
    context::{self, Handle},
    data::Pass,
    hook::{self, FocusChanged, KeysSent, KeysSentTo, ModeSet, ModeSwitched},
    main_thread_only::MainThreadOnly,
    ui::{Node, Widget},
    utils::{catch_panic, duat_name},
};

static SEND_KEYS: MainThreadOnly<RefCell<Option<KeyFn>>> = MainThreadOnly::new(RefCell::new(None));
static RESET_MODES: LazyLock<Mutex<HashMap<TypeId, Box<dyn FnMut(&mut Pass) -> bool + Send>>>> =
    LazyLock::new(Mutex::default);
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
pub fn set_default<M: Mode>(mode: M) {
    let mut reset_modes = RESET_MODES.lock().unwrap();
    let type_id = TypeId::of::<M::Widget>();

    if let Some(reset_fn) = reset_modes.get_mut(&type_id) {
        *reset_fn = Box::new(move |pa| {
            let mode = mode.clone();
            set_mode_fn::<M>(pa, mode)
        });
    } else {
        reset_modes.insert(
            TypeId::of::<M::Widget>(),
            Box::new(move |pa| {
                let mode = mode.clone();
                set_mode_fn::<M>(pa, mode)
            }),
        );
    };

    if TypeId::of::<M::Widget>() == TypeId::of::<Buffer>() {
        let mut set_mode = SET_MODE.lock().unwrap();
        let prev = set_mode.take();
        *set_mode = Some(Box::new(move |pa| {
            if let Some(f) = prev {
                f(pa);
            }
            RESET_MODES.lock().unwrap().get_mut(&type_id).unwrap()(pa)
        }));
    }
}

/// Sets the [`Mode`], switching to the appropriate [`Widget`]
///
/// [`Widget`]: Mode::Widget
pub fn set(mode: impl Mode) {
    let mut set_mode = SET_MODE.lock().unwrap();
    let prev = set_mode.take();
    *set_mode = Some(Box::new(move |pa| {
        if let Some(prev) = prev {
            prev(pa);
        }
        set_mode_fn(pa, mode)
    }));
}

/// Returns `true` if the [`Widget`] has a default [`Mode`]
pub fn has_default<W: Widget>() -> bool {
    RESET_MODES
        .lock()
        .unwrap()
        .get(&TypeId::of::<W>())
        .is_some()
}

/// Resets the mode to the [default] of a given [`Widget`]
///
/// Does nothing if no default was set for the given [`Widget`].
///
/// [default]: set_default
pub fn reset<W: Widget>() {
    let reset_modes = RESET_MODES.lock().unwrap();
    let type_id = TypeId::of::<W>();

    if reset_modes.get(&type_id).is_some() {
        *SET_MODE.lock().unwrap() = Some(Box::new(move |pa| {
            RESET_MODES.lock().unwrap().get_mut(&type_id).unwrap()(pa)
        }));
    } else if TypeId::of::<W>() == TypeId::of::<Buffer>() {
        panic!("Something went terribly wrong, somehow");
    } else {
        context::error!(
            "There is no default [a]Mode[] set for [a]{}[]",
            crate::utils::duat_name::<W>()
        );
    };
}

/// Resets to the default [`Mode`] of the given [`Widget`], on a
/// given [`Handle`]
pub fn reset_to(handle: Handle<dyn Widget>) {
    let reset_modes = RESET_MODES.lock().unwrap();
    let type_id = handle.widget().type_id();

    if reset_modes.get(&type_id).is_some() {
        *SET_MODE.lock().unwrap() = Some(Box::new(move |pa| {
            let node = context::windows()
                .entries(pa)
                .find(|(.., node)| node.ptr_eq(handle.widget()))
                .map(|(.., node)| node.clone());

            if let Some(node) = node {
                let node = node.clone();
                switch_widget(pa, node);
                RESET_MODES.lock().unwrap().get_mut(&type_id).unwrap()(pa)
            } else {
                context::error!("The Handle was already closed");
                false
            }
        }));
    } else {
        context::error!("There is no default [a]Mode[] set for the [a]Widget",);
    };
}

/// Switches to a certain widget
pub(super) fn switch_widget(pa: &mut Pass, node: Node) {
    let cur_widget = context::current_widget_node(pa);
    unsafe { BEFORE_EXIT.get() }.replace(|_| {})(pa);

    let handle = node.handle().clone();

    hook::trigger(
        pa,
        FocusChanged((cur_widget.node(pa).handle().clone(), node.handle().clone())),
    );

    cur_widget.node(pa).on_unfocus(pa, handle);

    context::set_current_node(pa, node.clone());

    node.on_focus(pa, cur_widget.node(pa).handle().clone());
}

/// Sends the [`KeyEvent`] to the active [`Mode`]
pub(super) fn send_keys_to(pa: &mut Pass, keys: Vec<KeyEvent>) {
    let mut keys = keys.into_iter();
    // SAFETY: There is a Pass argument.
    let send_keys = unsafe { SEND_KEYS.get() };
    let mut sk = send_keys.take().unwrap();

    let _ = catch_panic(|| {
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
    });

    send_keys.replace(Some(sk));
}

/// Static dispatch function that sends keys to a [`Mode`]
fn send_keys_fn<M: Mode>(pa: &mut Pass, keys: &mut IntoIter<KeyEvent>) -> Option<ModeFn> {
    let handle = context::current_widget_node(pa)
        .node(pa)
        .try_downcast()
        .unwrap();

    let mut sent_keys = Vec::new();

    let mode_fn = {
        // SAFETY: This function's caller has a Pass argument.
        let mut mode = unsafe { MODE.get() }.borrow_mut();
        let mode: &mut M = mode.downcast_mut().unwrap();

        loop {
            if keys.len() > 0
                && let Some(mode_fn) = take_set_mode_fn(pa)
            {
                break Some(mode_fn);
            }
            let Some(key) = keys.next() else { break None };
            sent_keys.push(key);

            mode.send_key(pa, key, handle.clone());
        }
    };

    hook::trigger(pa, KeysSentTo::<M>((sent_keys.clone(), handle.clone())));
    hook::trigger(pa, KeysSent(sent_keys));

    mode_fn
}

/// Static dispatch function to set the [`Mode`]
fn set_mode_fn<M: Mode>(pa: &mut Pass, mode: M) -> bool {
    // If we are on the correct widget, no switch is needed.
    if context::current_widget_node(pa).type_id(pa) != TypeId::of::<M::Widget>() {
        let node = {
            let windows = context::windows();
            if TypeId::of::<M::Widget>() == TypeId::of::<Buffer>() {
                let pk = context::current_buffer(pa).read(pa).path_kind();
                windows
                    .buffer_entry(pa, pk)
                    .map(|(.., handle)| Node::from_handle(handle))
            } else {
                windows.node_of::<M::Widget>(pa).cloned()
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

    let wid = context::current_widget_node(pa);

    let handle = wid
        .mutate_data_as(pa, |handle: &Handle<M::Widget>| handle.clone())
        .unwrap();

    crate::mode::set_mode_for_remapper::<M>(pa);

    // Things that happen before the switch, in order to signal that a
    // switch has happened.
    *MODE_NAME.lock().unwrap() = std::any::type_name::<M>();
    unsafe {
        SEND_KEYS
            .get()
            .replace(Some(|pa, keys| send_keys_fn::<M>(pa, keys)));
        BEFORE_EXIT.get().replace(|pa| before_exit_fn::<M>(pa));
    }

    // Where the mode switch actually happens.
    let new_name = duat_name::<M>();
    let old_name = std::mem::replace(context::raw_mode_name().write(pa), new_name);

    hook::trigger(pa, ModeSwitched((old_name, new_name)));

    let mc = ModeSet((mode, handle.clone()));
    let mut mode = hook::trigger(pa, mc).0.0;
    mode.on_switch(pa, handle.clone());

    unsafe {
        MODE.get().replace(Box::new(mode));
    }

    true
}

/// Static dispatch function to use before exiting a given
/// [`Mode`]
fn before_exit_fn<M: Mode>(pa: &mut Pass) {
    let wid = context::current_widget_node(pa);

    let handle = wid
        .mutate_data_as(pa, |handle: &Handle<M::Widget>| handle.clone())
        .unwrap();

    // SAFETY: This function's caller has a Pass argument.
    let mut mode = unsafe { MODE.get() }.borrow_mut();
    let mode: &mut M = mode.downcast_mut().unwrap();

    mode.before_exit(pa, handle);
}

static MODE_NAME: Mutex<&str> = Mutex::new("");
