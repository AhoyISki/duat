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
    hook::{self, FocusChanged, KeySent, ModeSwitched},
    ui::{Node, Widget},
    utils::{catch_panic, duat_name},
};

static MODE_NAME: Mutex<&str> = Mutex::new("");
static MODE: LazyLock<RwData<Option<Box<dyn Any + Send>>>> = LazyLock::new(RwData::default);
static SEND_KEY: LazyLock<RwData<Option<KeyFn>>> = LazyLock::new(RwData::default);
static RESET_MODES: LazyLock<Mutex<HashMap<TypeId, ResetFn>>> = LazyLock::new(Mutex::default);

type KeyFn = fn(&mut Pass, KeyEvent);
type ResetFn = Box<dyn FnMut(&mut Pass) + Send>;

/// Sets the new default mode
///
/// This is the mode that will be set when [`mode::reset`] is
/// called. The `mode::reset` function is always called for the
/// [`Buffer`] `Widget` when Duat starts up, so you can use this
/// function to effectively change the default mode of Duat.
///
/// [`mode::reset`]: reset
pub fn set_default<M: Mode + Clone>(mode: M) {
    let mut reset_modes = RESET_MODES.lock().unwrap();
    let type_id = TypeId::of::<M::Widget>();

    if let Some(reset_fn) = reset_modes.get_mut(&type_id) {
        *reset_fn = Box::new(move |pa| {
            let mode = mode.clone();
            set::<M>(pa, mode);
        });
    } else {
        reset_modes.insert(
            TypeId::of::<M::Widget>(),
            Box::new(move |pa| {
                let mode = mode.clone();
                set::<M>(pa, mode);
            }),
        );
    };
}

/// Sets the [`Mode`], switching to the appropriate [`Widget`].
///
/// This function will also trigger the [`ModeSwitched`] hook.
/// However, it differs in how exactly that will happen:
///
/// - If you're in a [`Mode::send_key`] call, it's not possible to
///   trigger this hook right away because the previous mode would be
///   in use. So the hook will be triggered once the `send_key` call
///   ends instead.
/// - If you're _not_ in a [`Mode::send_key`] call, the hook will be
///   triggered immediately.
///
/// [`Widget`]: Mode::Widget
pub fn set<M: Mode>(pa: &mut Pass, mode: M) {
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
                return;
            }
        }
    } else {
        context::current_widget(pa).clone()
    };

    let new_name = duat_name::<M>();
    let old_name = std::mem::replace(context::mode_name().write(pa), new_name);

    crate::mode::set_mode_for_remapper::<M>(pa);

    let mode_opt = MODE.write(pa).take();

    // This is the case if we're not in a send_key call.
    let new_mode = if let Some(old_mode) = mode_opt {
        let new = (new_name, Box::new(mode) as Box<dyn Any + Send>);
        let old = (old_name, old_mode);
        let ms = hook::trigger(pa, ModeSwitched { old, new });
        ms.new.1
    } else {
        Box::new(mode)
    };

    // Things that happen before the switch, in order to signal that a
    // switch has happened.
    *MODE_NAME.lock().unwrap() = std::any::type_name::<M>();
    *MODE.write(pa) = Some(new_mode);
    *SEND_KEY.write(pa) = Some(|pa, keys| send_key_fn::<M>(pa, keys));
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
pub fn reset<W: Widget>(pa: &mut Pass) {
    let mut reset_modes = RESET_MODES.lock().unwrap();
    let type_id = TypeId::of::<W>();

    if let Some(reset_fn) = reset_modes.get_mut(&type_id) {
        reset_fn(pa);
    } else if TypeId::of::<W>() == TypeId::of::<Buffer>() {
        panic!("Something went terribly wrong, somehow");
    } else {
        context::error!(
            "There is no default [a]Mode[] set for [a]{}[]",
            crate::utils::duat_name::<W>()
        );
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

    let mut mode: Box<M> = {
        let mode = MODE.write(pa).take().unwrap();
        mode.downcast().unwrap()
    };

    catch_panic(|| mode.send_key(pa, key_event, handle.clone()));

    hook::trigger(pa, KeySent(key_event));

    let mode_opt = MODE.write(pa).take();
    // A new mode may have been set in the send_key function.
    match mode_opt {
        Some(new_mode) => {
            // Where the mode switch actually happens.
            let old_name = duat_name::<M>();
            let new_name = *context::mode_name().read(pa);

            let new = (new_name, new_mode);
            let old = (old_name, mode as Box<dyn Any + Send>);
            let ms = hook::trigger(pa, ModeSwitched { old, new });
            *MODE.write(pa) = Some(ms.new.1);
        }
        None => *MODE.write(pa) = Some(mode),
    }
}
