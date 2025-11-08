//! The acceptable patterns in a [`StatusLine`]
//!
//! The patterns that can be put in a [`StatusLine`] are largely the
//! same as the ones that can be put inside a [`text!`] macro,
//! however, some additions are made.
//!
//! Specifically, arguments that read from the [`Buffer`] and its
//! related structs are also accepted by [`status!`].
//!
//! In addition, arguments with arbitrary update schedules are also
//! accepted, such as the [data] types, and parser/checker function
//! pairs.
//!
//! [`StatusLine`]: super::StatusLine
//! [`status!`]: super::status
//! [`Buffer`]: super::Buffer
//! [data]: crate::data
use std::{fmt::Display, marker::PhantomData};

use duat_core::{
    buffer::Buffer,
    context::{self, Handle},
    data::{DataMap, Pass, RwData},
    mode::{Selection, Selections},
    text::{AlignCenter, AlignLeft, AlignRight, Builder, BuilderPart, Ghost, Spacer, Text},
    ui::{Area, Window},
};

use crate::widgets::status_line::CheckerFn;

/// A struct that reads state in order to return [`Text`].
enum Appender<_T: Clone = (), D: Display + Clone = String> {
    FromFn(BuilderFn),
    Part(BuilderPart<D, _T>),
}

/// A part of the [`StatusLine`]
///
/// This can either be a static part, like [`Text`], [`impl Display`]
/// type, or it can be a reader of the [`Buffer`] and its structs, or
/// it can update independently.
///
/// [`StatusLine`]: super::StatusLine
/// [`impl Display`]: std::fmt::Display
/// [`Buffer`]: duat_core::buffer::Buffer
#[doc(hidden)]
pub struct State<_T = (), D = String>
where
    _T: Clone + Send,
    D: Display + Clone + Send,
{
    appender: Appender<_T, D>,
    checker: Option<CheckerFn>,
    ghost: PhantomData<_T>,
}

impl<_T, D> State<_T, D>
where
    _T: Clone + Send + 'static,
    D: Display + Clone + Send + 'static,
{
    /// Returns the two building block functions for the
    /// [`Statusline`]
    ///
    /// [`StatusLine`]: super::StatusLine
    pub fn fns(self) -> StateFns {
        (
            match self.appender {
                Appender::FromFn(f) => f,
                Appender::Part(builder_part) => {
                    Box::new(move |_, b, _| b.push(builder_part.clone()))
                }
            },
            Box::new(move |pa| self.checker.as_ref().is_some_and(|check| check(pa))),
        )
    }
}

impl<D: Display + Clone + Send + 'static> From<D> for State<D, D> {
    fn from(value: D) -> Self {
        Self {
            appender: Appender::Part(value.into()),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl From<Text> for State<()> {
    fn from(value: Text) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(value)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D: Display + Clone + Send + 'static> From<RwData<D>> for State<DataArg<D>> {
    fn from(value: RwData<D>) -> Self {
        let checker = value.checker();
        Self {
            appender: Appender::FromFn(Box::new(move |pa, b, _| b.push(value.read(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl From<RwData<Text>> for State<DataArg<()>> {
    fn from(value: RwData<Text>) -> Self {
        let checker = value.checker();
        Self {
            appender: Appender::FromFn(Box::new(move |pa, b, _| b.push(value.read(pa).clone()))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<I: ?Sized, O: Display> From<DataMap<I, O>> for State<DataArg<String>> {
    fn from(value: DataMap<I, O>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::FromFn(Box::new(move |pa, b, _| b.push(value.call(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<I: ?Sized> From<DataMap<I, Text>> for State<DataArg<Text>> {
    fn from(value: DataMap<I, Text>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::FromFn(Box::new(move |pa, b, _| b.push(value.call(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl From<AlignLeft> for State<AlignLeft> {
    fn from(_: AlignLeft) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::AlignLeft),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl From<AlignCenter> for State<AlignCenter> {
    fn from(_: AlignCenter) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::AlignCenter),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl From<AlignRight> for State<AlignRight> {
    fn from(_: AlignRight) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::AlignRight),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl From<Spacer> for State<()> {
    fn from(_: Spacer) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(Spacer)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<T: Into<Text> + Clone> From<Ghost<T>> for State<()> {
    fn from(value: Ghost<T>) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(value)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

////////// From functions

macro_rules! implFromFn {
    ($($arg:ident),*) => {
        #[allow(unused_parens, non_snake_case)]
        impl<Fmt, D, $($arg),*> From<Fmt> for State<FnArg<($($arg),*), String>>
        where
            Fmt: Fn($(&$arg),*) -> D + Send + 'static,
            D: Display,
            $($arg: StateArg),*
        {
            #[allow(unused_variables)]
            fn from(value: Fmt) -> Self {
                Self {
                    appender: Appender::FromFn(Box::new(move |pa, b, handle| {
                        $(
                            let $arg = $arg::get(pa, handle);
                        )*
                        b.push(value($($arg),*));
                    })),
                    checker: None,
                    ghost: PhantomData,
                }
            }
        }

        #[allow(unused_parens, non_snake_case)]
        impl<Fmt, $($arg),*> From<Fmt> for State<FnArg<($($arg),*), Text>>
        where
            Fmt: Fn($(&$arg),*) -> Text + Send + 'static,
            $($arg: StateArg),*
        {
            #[allow(unused_variables)]
            fn from(value: Fmt) -> Self {
                Self {
                    appender: Appender::FromFn(Box::new(move |pa, b, handle| {
                        $(
                            let $arg = $arg::get(pa, handle);
                        )*
                        b.push(value($($arg),*));
                    })),
                    checker: None,
                    ghost: PhantomData,
                }
            }
        }
    }
}

implFromFn!();
implFromFn!(Arg1);
implFromFn!(Arg1, Arg2);
implFromFn!(Arg1, Arg2, Arg3);
implFromFn!(Arg1, Arg2, Arg3, Arg4);
implFromFn!(Arg1, Arg2, Arg3, Arg4, Arg5);
implFromFn!(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6);
implFromFn!(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7);
implFromFn!(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8);

trait StateArg {
    fn get<'a>(pa: &'a Pass, handle: &'a Handle) -> &'a Self;
}

impl StateArg for Buffer {
    fn get<'a>(pa: &'a Pass, handle: &'a Handle) -> &'a Self {
        handle.read(pa)
    }
}

impl StateArg for Handle {
    fn get<'a>(_: &'a Pass, handle: &'a Handle) -> &'a Self {
        handle
    }
}

impl StateArg for Area {
    fn get<'a>(pa: &'a Pass, handle: &'a Handle) -> &'a Self {
        handle.area().read(pa)
    }
}

impl StateArg for Pass {
    fn get<'a>(pa: &'a Pass, _: &'a Handle) -> &'a Self {
        pa
    }
}

impl StateArg for Text {
    fn get<'a>(pa: &'a Pass, handle: &'a Handle) -> &'a Self {
        handle.text(pa)
    }
}

impl StateArg for Selections {
    fn get<'a>(pa: &'a Pass, handle: &'a Handle) -> &'a Self {
        handle.text(pa).selections()
    }
}

impl StateArg for Selection {
    fn get<'a>(pa: &'a Pass, handle: &'a Handle) -> &'a Self {
        handle.text(pa).selections().get_main().unwrap()
    }
}

impl StateArg for Window {
    fn get<'a>(pa: &'a Pass, handle: &'a Handle) -> &'a Self {
        context::windows()
            .iter(pa)
            .find(|window| window.handles(pa).any(|h| h == handle))
            .unwrap()
    }
}

// Dummy structs to prevent implementation conflicts.
#[doc(hidden)]
#[derive(Clone)]
pub struct DataArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct FnArg<Args, T>(PhantomData<(Args, T)>);

impl<Args, T> Clone for FnArg<Args, T> {
    fn clone(&self) -> Self {
        Self(PhantomData)
    }
}

unsafe impl<Args, T> Send for FnArg<Args, T> {}

type BuilderFn = Box<dyn Fn(&Pass, &mut Builder, &Handle) + Send>;
type StateFns = (BuilderFn, Box<dyn Fn(&Pass) -> bool + Send>);
