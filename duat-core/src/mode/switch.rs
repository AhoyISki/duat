//! Functionality for switching modes
//!
//! This module just defines the functions that are necessary whenever
//! the current mode switches. It does all the synchronization between
//! the parts of Duat that rely on the current mode.
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use crossterm::event::KeyEvent;

use super::Mode;
use crate::{
    buffer::Buffer,
    context::{self, Handle},
    data::{Pass, RwData},
    hook::{self, FocusChanged, KeySent, KeySentTo, ModeSwitched},
    ui::{Node, Widget},
    utils::{catch_panic, duat_name},
};

static MODE_NAME: Mutex<&str> = Mutex::new("");
static MODE_AND_BEFORE_EXIT: LazyLock<RwData<Option<(Box<dyn Any>, BeforeExitFn)>>> =
    LazyLock::new(RwData::default);
static SEND_KEY: LazyLock<RwData<Option<KeyFn>>> = LazyLock::new(RwData::default);
static RESET_MODES: LazyLock<Mutex<HashMap<TypeId, ResetFn>>> = LazyLock::new(Mutex::default);

type KeyFn = fn(&mut Pass, KeyEvent);
type BeforeExitFn = fn(&mut Pass, Box<dyn Any>, Handle<dyn Widget>);
type ResetFn = Box<dyn FnMut(&mut Pass) -> Option<Handle<dyn Widget>> + Send>;

/// Sets the new default mode
///
/// This is the mode that will be set when [`mode::reset`] is
/// called. The `mode::reset` function is always called for the
/// [`Buffer`] `Widget` when Duat starts up, so you can use this
/// function to effectively change the default mode of Duat.
///
/// [`mode::reset`]: reset
pub fn set_default<M: Mode>(mode: M) {
    let mut reset_modes = RESET_MODES.lock().unwrap();
    let type_id = TypeId::of::<M::Widget>();

    if let Some(reset_fn) = reset_modes.get_mut(&type_id) {
        *reset_fn = Box::new(move |pa| {
            let mode = mode.clone();
            set::<M>(pa, mode).map(|handle| handle.to_dyn())
        });
    } else {
        reset_modes.insert(
            TypeId::of::<M::Widget>(),
            Box::new(move |pa| {
                let mode = mode.clone();
                set::<M>(pa, mode).map(|handle| handle.to_dyn())
            }),
        );
    };
}

/// Sets the [`Mode`], switching to the appropriate [`Widget`]
///
/// If you call this within a [`Mode::send_key`] function, the rest of
/// the function will proceed normally. However, if there were any
/// remaining keys to be processed, they will be sent to the new
/// `Mode` with its new [`Widget`], not to the `Mode` of the
/// `send_key` function in use.
///
/// One other thing to note about this function is the
/// [`Mode::before_exit`] function. If you call `mode::set` from the
/// `Mode::send_key` function, then `Mode::before_exit` will be called
/// _after_ the end of the function's call. If you call `mode::set`
/// from anywhere else, then `Mode::before_exit` will be called
/// immediately.
///
/// This is done because [`Mode::before_exit`] requires a mutable
/// reference to the `Mode`, and if you call it from
/// [`Mode::send_key`], the `send_key` function is making use of that
/// mutable reference.
///
/// [`Widget`]: Mode::Widget
pub fn set<M: Mode>(pa: &mut Pass, mut mode: M) -> Option<Handle<M::Widget>> {
    // If we are on the correct widget, no switch is needed.
    let former = if context::current_widget_node(pa).type_id(pa) != TypeId::of::<M::Widget>() {
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
                return None;
            }
        }
    } else {
        context::current_widget(pa).clone()
    };

    let node = context::current_widget_node(pa);

    let handle = node
        .mutate_data_as(pa, |handle: &Handle<M::Widget>| handle.clone())
        .unwrap();

    crate::mode::set_mode_for_remapper::<M>(pa);

    mode.on_switch(pa, handle.clone());

    // Things that happen before the switch, in order to signal that a
    // switch has happened.
    *MODE_NAME.lock().unwrap() = std::any::type_name::<M>();

    if let Some((mode, before_exit)) = MODE_AND_BEFORE_EXIT
        .write(pa)
        .replace((Box::new(mode), before_exit_fn::<M>))
    {
        before_exit(pa, mode, former);
    }

    SEND_KEY
        .write(pa)
        .replace(|pa, keys| send_key_fn::<M>(pa, keys));

    // Where the mode switch actually happens.
    let new_name = duat_name::<M>();
    let old_name = std::mem::replace(context::mode_name().write(pa), new_name);

    hook::trigger(pa, ModeSwitched((old_name, new_name)));

    Some(handle)
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
pub fn reset<W: Widget>(pa: &mut Pass) -> Option<Handle<W>> {
    let mut reset_modes = RESET_MODES.lock().unwrap();
    let type_id = TypeId::of::<W>();

    if let Some(reset_fn) = reset_modes.get_mut(&type_id) {
        reset_fn(pa).and_then(|handle| handle.try_downcast())
    } else if TypeId::of::<W>() == TypeId::of::<Buffer>() {
        panic!("Something went terribly wrong, somehow");
    } else {
        context::error!(
            "There is no default [a]Mode[] set for [a]{}[]",
            crate::utils::duat_name::<W>()
        );
        None
    }
}

/// Resets to the default [`Mode`] of the given [`Widget`], on a
/// given [`Handle`]
pub fn reset_to(pa: &mut Pass, handle: &Handle<impl Widget + ?Sized>) {
    let mut reset_modes = RESET_MODES.lock().unwrap();
    let type_id = handle.widget().type_id();

    if let Some(reset_fn) = reset_modes.get_mut(&type_id) {
        let node = context::windows()
            .entries(pa)
            .find(|(.., node)| node.ptr_eq(handle.widget()))
            .map(|(.., node)| node.clone());

        if let Some(node) = node {
            let node = node.clone();
            switch_widget(pa, node);
            reset_fn(pa);
        } else {
            context::error!("The Handle was already closed");
        }
    } else {
        context::error!("There is no default [a]Mode[] set for the [a]Widget",);
    };
}

/// Switches to a certain widget
pub(super) fn switch_widget(pa: &mut Pass, node: Node) -> Handle<dyn Widget> {
    let cur_widget = context::current_widget_node(pa);
    let former = cur_widget.node(pa).handle().clone();
    let current = node.handle().clone();

    hook::trigger(pa, FocusChanged((former.clone(), current.clone())));

    cur_widget.node(pa).on_unfocus(pa, current.clone());
    context::set_current_node(pa, node.clone());
    node.on_focus(pa, former.clone());

    former.clone()
}

/// Sends the [`KeyEvent`] to the active [`Mode`]
pub(super) fn send_keys_to(pa: &mut Pass, keys: Vec<KeyEvent>) {
    let _ = catch_panic(|| {
        for key in keys {
            let send_keys = SEND_KEY.read(pa).unwrap();
            send_keys(pa, key);
        }
    });
}

/// Static dispatch function that sends keys to a [`Mode`]
fn send_key_fn<M: Mode>(pa: &mut Pass, key_event: KeyEvent) {
    let handle = context::current_widget_node(pa)
        .node(pa)
        .try_downcast()
        .unwrap();

    let (mut mode, before_exit): (Box<M>, _) = {
        let (mode, before_exit) = MODE_AND_BEFORE_EXIT.write(pa).take().unwrap();
        (mode.downcast().unwrap(), before_exit)
    };

    mode.send_key(pa, key_event, handle.clone());

    hook::trigger(pa, KeySentTo::<M>((key_event, handle.clone())));
    hook::trigger(pa, KeySent(key_event));

    let mode_and_before_exit = MODE_AND_BEFORE_EXIT.write(pa);
    if mode_and_before_exit.is_none() {
        *mode_and_before_exit = Some((mode, before_exit));
    } else {
        mode.before_exit(pa, handle);
    }
}

/// Static dispatch function to use before exiting a given
/// [`Mode`]
fn before_exit_fn<M: Mode>(pa: &mut Pass, mode: Box<dyn Any>, handle: Handle<dyn Widget>) {
    let handle = handle.try_downcast().unwrap();

    let mut mode: Box<M> = mode.downcast().unwrap();
    mode.before_exit(pa, handle);
}
