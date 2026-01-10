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
    text::{AsBuilderPart, Builder, Ghost, Spacer, Text},
    ui::{Area, Window},
};

use crate::widgets::status_line::CheckerFn;

/// A struct that reads state in order to return [`Text`].
enum Appender<Part: AsBuilderPart<D, _T> = String, _T = (), D: Display = String> {
    FromFn(BuilderFn),
    Part(Part, PhantomData<(D, _T)>),
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
pub struct State<_T1 = (), Part: AsBuilderPart<D, _T2> = String, _T2 = (), D: Display = String> {
    appender: Appender<Part, _T2, D>,
    checker: Option<CheckerFn>,
    ghost: PhantomData<_T1>,
}

impl<_T1, Part, _T2, D> State<_T1, Part, _T2, D>
where
    Part: AsBuilderPart<D, _T2> + Send + 'static,
    _T2: 'static,
    D: Display + 'static,
{
    /// Returns the two building block functions for the
    /// [`Statusline`]
    ///
    /// [`StatusLine`]: super::StatusLine
    pub fn fns(self) -> StateFns {
        (
            match self.appender {
                Appender::FromFn(f) => f,
                Appender::Part(part, _) => {
                    Box::new(move |_, b, _| b.push_builder_part(part.as_builder_part()))
                }
            },
            Box::new(move |pa| self.checker.as_ref().is_some_and(|check| check(pa))),
        )
    }
}

impl<D: Display + Send + 'static> From<D> for State<D, D, D, D> {
    fn from(value: D) -> Self {
        Self {
            appender: Appender::Part(value, PhantomData),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl From<Text> for State<(), Text> {
    fn from(value: Text) -> Self {
        Self {
            appender: Appender::Part(value, PhantomData),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D: Display + 'static> From<RwData<D>> for State<DataArg<D>, D, D, D> {
    fn from(value: RwData<D>) -> Self {
        let checker = value.checker();
        Self {
            appender: Appender::FromFn(Box::new(move |pa, b, _| b.push(value.read(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl From<RwData<Text>> for State<DataArg<()>, Text> {
    fn from(value: RwData<Text>) -> Self {
        let checker = value.checker();
        Self {
            appender: Appender::FromFn(Box::new(move |pa, b, _| b.push(value.read(pa).clone()))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<I: ?Sized, O: Display> From<DataMap<I, O>> for State<DataArg<O>, O, O, O> {
    fn from(value: DataMap<I, O>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::FromFn(Box::new(move |pa, b, _| b.push(value.call(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<I: ?Sized> From<DataMap<I, Text>> for State<DataArg<Text>, Text> {
    fn from(value: DataMap<I, Text>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::FromFn(Box::new(move |pa, b, _| b.push(value.call(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl From<Spacer> for State<Spacer, Spacer> {
    fn from(_: Spacer) -> Self {
        Self {
            appender: Appender::Part(Spacer, PhantomData),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl From<Ghost> for State<(), Ghost> {
    fn from(value: Ghost) -> Self {
        Self {
            appender: Appender::Part(value, PhantomData),
            checker: None,
            ghost: PhantomData,
        }
    }
}

////////// From functions

macro_rules! implFromFn {
    ($($arg:ident),*) => {
        #[allow(unused_parens, non_snake_case)]
        impl<Fmt, D, $($arg),*> From<Fmt> for State<FnArg<($($arg),*), D>, D, D, D>
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
        impl<Fmt, $($arg),*> From<Fmt> for State<FnArg<($($arg),*), Text>, Text>
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
        handle.text(pa).main_sel()
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
