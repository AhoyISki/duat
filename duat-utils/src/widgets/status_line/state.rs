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
    data::{DataMap, RwData},
    prelude::*,
    text::{Builder, BuilderPart},
    ui::traits::Area,
};

use crate::widgets::status_line::CheckerFn;

/// A struct that reads state in order to return [`Text`].
enum Appender<_T: Clone = (), D: Display + Clone = String, W: Widget = Buffer> {
    TextFnCheckerArg(TextFnCheckerFn),
    FromWidget(WidgetAreaFn<W>),
    Part(BuilderPart<D, _T>),
}

/// A part of the [`StatusLine`]
///
/// This can either be a static part, like [`Text`], [`impl Display`]
/// type, or it can be a reader of the [`Buffer`] and its structs, or it
/// can update independently.
///
/// [`StatusLine`]: super::StatusLine
/// [`impl Display`]: std::fmt::Display
/// [`Buffer`]: crate::buffer::Buffer
#[doc(hidden)]
pub struct State<_T = (), D = String, W = Buffer>
where
    _T: Clone + Send,
    D: Display + Clone + Send,
    W: Widget,
{
    appender: Appender<_T, D, W>,
    checker: Option<CheckerFn>,
    ghost: PhantomData<_T>,
}

impl<_T, D, W> State<_T, D, W>
where
    _T: Clone + Send + 'static,
    D: Display + Clone + Send + 'static,
    W: Widget,
{
    /// Returns the two building block functions for the
    /// [`Statusline`]
    ///
    /// [`StatusLine`]: super::StatusLine
    pub fn fns(self) -> StateFns {
        (
            match self.appender {
                Appender::TextFnCheckerArg(f) => Box::new(move |pa, b, _| f(pa, b)),
                Appender::FromWidget(f) => Box::new(move |pa, b, reader| {
                    if let Some((widget, area, _)) = reader.read_related(pa).next() {
                        f(b, pa, widget, area.read(pa));
                    }
                }),
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
            appender: Appender::TextFnCheckerArg(Box::new(move |pa, b| b.push(value.read(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl From<RwData<Text>> for State<DataArg<()>> {
    fn from(value: RwData<Text>) -> Self {
        let checker = value.checker();
        Self {
            appender: Appender::TextFnCheckerArg(Box::new(move |pa, b| {
                b.push(value.read(pa).clone())
            })),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<I: ?Sized, O: Display> From<DataMap<I, O>> for State<DataArg<String>> {
    fn from(value: DataMap<I, O>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::TextFnCheckerArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<I: ?Sized> From<DataMap<I, Text>> for State<DataArg<Text>> {
    fn from(value: DataMap<I, Text>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::TextFnCheckerArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<D, TextFn, Checker> From<(TextFn, Checker)> for State<TextFnCheckerArg<String>>
where
    D: Display,
    TextFn: Fn(&Pass) -> D + Send + 'static,
    Checker: Fn(&Pass) -> bool + Send + 'static,
{
    fn from((value, checker): (TextFn, Checker)) -> Self {
        State {
            appender: Appender::TextFnCheckerArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<TextFn, Checker> From<(TextFn, Checker)> for State<TextFnCheckerArg<Text>>
where
    TextFn: Fn(&Pass) -> Text + Send + 'static,
    Checker: Fn(&Pass) -> bool + Send + 'static,
{
    fn from((value, checker): (TextFn, Checker)) -> Self {
        State {
            appender: Appender::TextFnCheckerArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(move |pa| checker(pa))),
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn> From<ReadFn> for State<WidgetArg<String>, String, W>
where
    D: Display + 'static,
    W: Widget + Sized,
    ReadFn: Fn(&W) -> D + Send + 'static,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, _, widget, _| b.push(value(widget)))),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn> From<ReadFn> for State<WidgetArg<Text>, String, W>
where
    W: Widget + Sized,
    ReadFn: Fn(&W) -> Text + Send + 'static,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, _, widget, _| b.push(value(widget)))),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn> From<ReadFn> for State<WidgetAreaArg<String>, String, W>
where
    D: Display + 'static,
    W: Widget + Sized,
    ReadFn: Fn(&W, &dyn Area) -> D + Send + 'static,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, _, widget, area| {
                b.push(value(widget, area))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn> From<ReadFn> for State<WidgetAreaArg<Text>, String, W>
where
    W: Widget + Sized,
    ReadFn: Fn(&W, &dyn Area) -> Text + Send + 'static,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, _, widget, area| {
                b.push(value(widget, area))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn> From<ReadFn> for State<PassWidgetArg<String>, String, W>
where
    D: Display + 'static,
    W: Widget + Sized,
    ReadFn: Fn(&Pass, &W) -> D + Send + 'static,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, pa, widget, _| {
                b.push(value(pa, widget))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn> From<ReadFn> for State<PassWidgetArg<Text>, String, W>
where
    W: Widget + Sized,
    ReadFn: Fn(&Pass, &W) -> Text + Send + 'static,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, pa, widget, _| {
                b.push(value(pa, widget))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn> From<ReadFn> for State<PassWidgetAreaArg<String>, String, W>
where
    D: Display + 'static,
    W: Widget + Sized,
    ReadFn: Fn(&Pass, &W, &dyn Area) -> D + Send + 'static,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, pa, widget, area| {
                b.push(value(pa, widget, area))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn> From<ReadFn> for State<PassWidgetAreaArg<Text>, String, W>
where
    W: Widget + Sized,
    ReadFn: Fn(&Pass, &W, &dyn Area) -> Text + Send + 'static,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, pa, widget, area| {
                b.push(value(pa, widget, area))
            })),
            checker: None,
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

// Dummy structs to prevent implementation conflicts.
#[doc(hidden)]
#[derive(Clone)]
pub struct DataArg<T>(PhantomData<T>);
#[doc(hidden)]
#[derive(Clone)]
pub struct TextFnArg<T>(PhantomData<T>);
#[doc(hidden)]
#[derive(Clone)]
pub struct TextFnCheckerArg<T>(PhantomData<T>);
#[doc(hidden)]
#[derive(Clone)]
pub struct WidgetArg<W>(PhantomData<W>);
#[doc(hidden)]
#[derive(Clone)]
pub struct WidgetAreaArg<W>(PhantomData<W>);
#[derive(Clone)]
pub struct PassWidgetArg<W>(PhantomData<W>);
#[doc(hidden)]
#[derive(Clone)]
pub struct PassWidgetAreaArg<W>(PhantomData<W>);

// The various types of function aliases
type TextFnCheckerFn = Box<dyn Fn(&Pass, &mut Builder) + 'static + Send>;
type WidgetAreaFn<W> = Box<dyn Fn(&mut Builder, &Pass, &W, &dyn Area) + Send + 'static>;
type BuilderFn = Box<dyn Fn(&Pass, &mut Builder, &Handle<Buffer>) + Send>;
type StateFns = (BuilderFn, Box<dyn Fn(&Pass) -> bool + Send>);
