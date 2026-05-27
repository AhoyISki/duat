//! Functionality for switching modes
//!
//! This module just defines the functions that are necessary whenever
//! the current mode switches. It does all the synchronization between
//! the parts of Duat that rely on the current mode.
use std::{
    any::{Any, TypeId},
    sync::{LazyLock, Mutex},
};

use crossterm::event::KeyEvent;

use super::Mode;
use crate::{
    buffer::Buffer,
    context::{self, Handle},
    data::{Pass, RwData},
    hook::{self, ModeSwitched},
    ui::Widget,
    utils::{catch_panic, duat_name},
};

static MODE_NAME: Mutex<&str> = Mutex::new("");
static MODE: LazyLock<RwData<Option<Box<dyn Any + Send>>>> =
    LazyLock::new(|| RwData::new(Some(Box::new(()))));
static SEND_KEY: LazyLock<RwData<Option<KeyFn>>> = LazyLock::new(RwData::default);
static SET_DEFAULT: Mutex<Option<ResetFn>> = Mutex::new(None);
static DEFERRED: LazyLock<RwData<Vec<Box<dyn FnOnce(&mut Pass) + Send>>>> =
    LazyLock::new(RwData::default);

type KeyFn = fn(&mut Pass, KeyEvent);
type ResetFn = Box<dyn FnMut(&mut Pass, Handle) + Send>;

/// Sets the new default mode.
///
/// This mode needs to be usable across various different [`Widget`]s,
/// since it will be set by default if one needs to focus on them.
///
/// This is the mode that will be set when [`mode::reset`] is
/// called. The `mode::reset` function is always called for the
/// [`Buffer`] `Widget` when Duat starts up, so you can use this
/// function to effectively change the default mode of Duat.
///
/// [`mode::reset`]: reset
/// [`Buffer`]: crate::buffer::Buffer
pub fn set_default<M: Mode>(mut mode_fn: impl FnMut(Handle) -> M + Send + 'static) {
    *SET_DEFAULT.lock().unwrap() = Some(Box::new(move |pa, widget| {
        widget.set_as_active(pa);
        set(pa, mode_fn(widget))
    }));
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
pub fn set<M: Mode>(pa: &mut Pass, mode: M) {
    let Some(old_mode) = MODE.write(pa).take() else {
        DEFERRED.write(pa).push(Box::new(move |pa| set(pa, mode)));
        return;
    };

    let new_name = duat_name::<M>();
    let old_name = std::mem::replace(context::mode_name().write(pa), new_name);

    // Things that happen before the switch, in order to signal that a
    // switch has happened.
    *MODE_NAME.lock().unwrap() = std::any::type_name::<M>();
    *SEND_KEY.write(pa) = Some(|pa, keys| send_key_fn::<M>(pa, keys));
    crate::mode::set_mode_for_remapper::<M>(pa);

    // This is the case if we're not in a send_key call.
    let new_mode = if !old_mode.is::<()>() {
        let old = (old_mode, old_name);
        let new = (Box::new(mode) as Box<dyn Any + Send>, new_name);
        let ms = hook::trigger(pa, ModeSwitched { old, new });
        ms.new.0
    } else {
        Box::new(mode)
    };

    *MODE.write(pa) = Some(new_mode);

    for deferred_set in std::mem::take(DEFERRED.write(pa)) {
        deferred_set(pa);
    }
}

/// Resets the mode to the [default] on a given [`Widget`]
///
/// This default [`Mode`] should make sense to be used on any
/// `Widget`, not just [`Buffer`]s.
///
/// This function will also [make the `Handle` active], making it so
/// [aliases] and [`Completions`] show up on it.
///
/// [default]: set_default
/// [`duatmode::Normal`]: https://docs.rs/crates/duat/latest/prelude/struct.Normal.html
/// [aliases]: super::alias
/// [`Completions`]: https://docs.rs/crates/duat/latest/widgets/struct.Completions.html
/// [`Buffer`]: crate::buffer::Buffer
#[track_caller]
pub fn reset<W: Widget>(pa: &mut Pass) {
    let mut set_default = SET_DEFAULT.lock().unwrap();

    if let Some(set_default) = set_default.as_mut() {
        if TypeId::of::<W>() == TypeId::of::<Buffer>() {
            set_default(pa, context::current_buffer(pa).to_dyn());
        } else if let Some(widget) = context::handle_of::<W>(pa) {
            set_default(pa, widget.to_dyn())
        } else {
            context::error!("No widget of type [a]{}", crate::utils::duat_name::<W>());
        }
    } else {
        panic!("No default mode set");
    }
}

/// Resets to the default [`Mode`] of the given [`Widget`], on a
/// given [`Handle`]
#[track_caller]
pub fn reset_to(pa: &mut Pass, widget: &Handle<impl Widget + ?Sized>) {
    let mut set_default = SET_DEFAULT.lock().unwrap();

    if let Some(set_default) = set_default.as_mut() {
        if let Some((_, node)) = {
            context::windows()
                .entries(pa)
                .find(|(_, node)| node.handle() == widget)
        } {
            set_default(pa, node.handle().clone());
        } else {
            context::error!("Tried setting a [a]closed[] Handle as active");
        }
    } else {
        panic!("No default mode set");
    }
}

/// Sends the [`KeyEvent`] to the active [`Mode`]
pub(super) fn send_final_key(pa: &mut Pass, key: KeyEvent) {
    SEND_KEY.read(pa).unwrap()(pa, key);
}

/// Static dispatch function that sends keys to a [`Mode`]
fn send_key_fn<M: Mode>(pa: &mut Pass, key_event: KeyEvent) {
    let mut mode: Box<M> = {
        let mode = MODE.write(pa).take().unwrap();
        mode.downcast().unwrap()
    };

    catch_panic(|| mode.send_key(pa, key_event));

    *MODE.write(pa) = Some(mode);
    for deferred_set in std::mem::take(DEFERRED.write(pa)) {
        deferred_set(pa);
    }
}
