//! The acceptable patterns in a [`StatusLine`]
//!
//! The patterns that can be put in a [`StatusLine`] are largely the
//! same as the ones that can be put inside a [`text!`] macro,
//! however, some additions are made.
//!
//! Specifically, arguments that read from the [`File`] and its
//! related structs are also accepted by [`status!`].
//!
//! In addition, arguments with arbitrary update schedules are also
//! accepted, such as the [data] types, and parser/checker function
//! pairs.
//!
//! [`StatusLine`]: super::StatusLine
//! [`status!`]: super::status
//! [`File`]: super::File
//! [data]: crate::data
use std::{fmt::Display, marker::PhantomData};

use super::Reader;
use crate::{
    data::{DataMap, RwData},
    text::{AlignCenter, AlignLeft, AlignRight, Builder, BuilderPart, Ghost, Spacer, Text},
    ui::Ui,
    widgets::Widget,
};

/// A struct that reads state in order to return [`Text`].
enum Appender<U: Ui, _T: Clone = (), D: Display + Send + Clone = String, W = ()> {
    NoArgs(Box<dyn FnMut(&mut Builder) + Send + 'static>),
    FromWidget(WidgetAreaFn<W, U>),
    Part(BuilderPart<D, _T>),
}

/// A part of the [`StatusLine`]
///
/// This can either be a static part, like [`Text`], [`impl Display`]
/// type, or it can be a reader of the [`File`] and its structs, or it
/// can update independently.
///
/// [`StatusLine`]: super::StatusLine
/// [`impl Display`]: std::fmt::Display
/// [`File`]: crate::widgets::File
pub struct State<U: Ui, _T: Clone = (), D: Display + Send + Clone = String, W: 'static = ()> {
    appender: Appender<U, _T, D, W>,
    checker: Option<Box<dyn Fn() -> bool + Send + Sync>>,
    ghost: PhantomData<_T>,
}

impl<U, _T, D, W> State<U, _T, D, W>
where
    U: Ui,
    _T: Send + Clone + 'static,
    D: Display + Send + Clone + 'static,
    W: 'static,
{
    pub fn fns(self) -> (ReaderFn<U>, Box<dyn Fn() -> bool + Send + Sync>) {
        (
            match self.appender {
                Appender::NoArgs(mut f) => Box::new(move |b, _| f(b)),
                Appender::FromWidget(mut f) => Box::new(move |b, reader| {
                    reader.inspect_related(|w, a| f(b, w, a));
                }),
                Appender::Part(builder_part) => Box::new(move |b, _| b.push(builder_part.clone())),
            },
            Box::new(move || self.checker.as_ref().is_some_and(|check| check())),
        )
    }
}

impl<D: Display + Send + Clone + 'static, U: Ui> From<D> for State<U, D, D> {
    fn from(value: D) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(value)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<Text> for State<U, Text> {
    fn from(value: Text) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(value)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D: Display + Send + Clone, U: Ui> From<RwData<D>> for State<U, DataArg<D>> {
    fn from(value: RwData<D>) -> Self {
        Self {
            appender: Appender::NoArgs({
                let value = value.clone();
                Box::new(move |b| b.push(value.read().clone()))
            }),
            checker: Some(Box::new(value.checker())),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<RwData<Text>> for State<U, DataArg<Text>> {
    fn from(value: RwData<Text>) -> Self {
        Self {
            appender: Appender::NoArgs({
                let value = value.clone();
                Box::new(move |b| b.push(value.read().clone()))
            }),
            checker: Some(Box::new(value.checker())),
            ghost: PhantomData,
        }
    }
}

impl<U, I, O> From<DataMap<I, O>> for State<U, DataArg<String>>
where
    U: Ui,
    I: ?Sized + Send,
    O: Display + 'static,
{
    fn from(value: DataMap<I, O>) -> Self {
        let (mut reader, checker) = value.fns();
        State {
            appender: Appender::NoArgs(Box::new(move |b| b.push(reader()))),
            checker: Some(checker),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui, I: ?Sized + Send> From<DataMap<I, Text>> for State<U, DataArg<Text>> {
    fn from(value: DataMap<I, Text>) -> Self {
        let (mut reader, checker) = value.fns();
        State {
            appender: Appender::NoArgs(Box::new(move |b| b.push(reader()))),
            checker: Some(checker),
            ghost: PhantomData,
        }
    }
}

impl<U, F, I, O> From<F> for State<U, IntoDataArg<String>>
where
    U: Ui,
    F: FnOnce() -> DataMap<I, O>,
    I: ?Sized + Send + 'static,
    O: Display + 'static,
{
    fn from(value: F) -> Self {
        let (mut reader, checker) = value().fns();
        State {
            appender: Appender::NoArgs(Box::new(move |b| b.push(reader()))),
            checker: Some(checker),
            ghost: PhantomData,
        }
    }
}

impl<U, F, I> From<F> for State<U, IntoDataArg<Text>>
where
    U: Ui,
    F: FnOnce() -> DataMap<I, Text>,
    I: ?Sized + Send + 'static,
{
    fn from(value: F) -> Self {
        let (mut reader, checker) = value().fns();
        State {
            appender: Appender::NoArgs(Box::new(move |b| b.push(reader()))),
            checker: Some(checker),
            ghost: PhantomData,
        }
    }
}

impl<D, Reader, Checker, U> From<(Reader, Checker)> for State<U, NoArg<String>>
where
    D: Display,
    Reader: Fn() -> D + Send + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
    U: Ui,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        State {
            appender: Appender::NoArgs(Box::new(move |b| b.push(reader()))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<Reader, Checker, U> From<(Reader, Checker)> for State<U, NoArg<Text>>
where
    Reader: Fn() -> Text + Send + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
    U: Ui,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        State {
            appender: Appender::NoArgs(Box::new(move |b| b.push(reader()))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> From<ReadFn> for State<U, WidgetArg<String>, String, W>
where
    D: Display + Send + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&W) -> D + Send + 'static,
    U: Ui,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, widget, _| b.push(value(widget)))),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn, U> From<ReadFn> for State<U, WidgetArg<Text>, String, W>
where
    W: Widget<U> + Sized,
    ReadFn: Fn(&W) -> Text + Send + 'static,
    U: Ui,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, widget, _| b.push(value(widget)))),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> From<ReadFn> for State<U, WidgetAreaArg<String>, String, W>
where
    D: Display + Send + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&W, &U::Area) -> D + Send + 'static,
    U: Ui,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, widget, area| {
                b.push(value(widget, area))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn, U> From<ReadFn> for State<U, WidgetAreaArg<Text>, String, W>
where
    W: Widget<U> + Sized,
    ReadFn: Fn(&W, &U::Area) -> Text + Send + 'static,
    U: Ui,
{
    fn from(value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, widget, area| {
                b.push(value(widget, area))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<AlignLeft> for State<U, AlignLeft> {
    fn from(_: AlignLeft) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::AlignLeft),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<AlignCenter> for State<U, AlignCenter> {
    fn from(_: AlignCenter) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::AlignCenter),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<AlignRight> for State<U, AlignRight> {
    fn from(_: AlignRight) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::AlignRight),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<Spacer> for State<U, Spacer> {
    fn from(_: Spacer) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(Spacer)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<Ghost> for State<U, Ghost> {
    fn from(value: Ghost) -> Self {
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
pub struct IntoDataArg<T>(PhantomData<T>);
#[doc(hidden)]
#[derive(Clone)]
pub struct NoArg<T>(PhantomData<T>);
#[doc(hidden)]
#[derive(Clone)]
pub struct WidgetArg<W>(PhantomData<W>);
#[doc(hidden)]
#[derive(Clone)]
pub struct WidgetAreaArg<W>(PhantomData<W>);

// The various types of function aliases
type WidgetAreaFn<W, U> = Box<dyn FnMut(&mut Builder, &W, &<U as Ui>::Area) + Send + 'static>;
type ReaderFn<U> = Box<dyn FnMut(&mut Builder, &mut Reader<U>) + Send>;
