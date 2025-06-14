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

use duat_core::{
    data::DataMap,
    prelude::*,
    text::{Builder, BuilderPart},
};

/// A struct that reads state in order to return [`Text`].
enum Appender<U: Ui, _T: Clone = (), D: Display + Clone = String, W = ()> {
    PassArg(PassArgFn),
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
/// [`File`]: crate::file::File
#[doc(hidden)]
pub struct State<U: Ui, _T: Clone = (), D: Display + Clone = String, W: 'static = ()> {
    appender: Appender<U, _T, D, W>,
    checker: Option<Box<dyn Fn() -> bool>>,
    ghost: PhantomData<_T>,
}

impl<U, _T, D, W> State<U, _T, D, W>
where
    U: Ui,
    _T: Clone + 'static,
    D: Display + Clone + 'static,
    W: 'static,
{
    /// Returns the two building block functions for the
    /// [`Statusline`]
    ///
    /// [`StatusLine`]: super::StatusLine
    pub fn fns(self) -> (BuilderFn<U>, Box<dyn Fn() -> bool>) {
        (
            match self.appender {
                Appender::PassArg(mut f) => Box::new(move |pa, b, _| f(pa, b)),
                Appender::FromWidget(mut f) => Box::new(move |pa, b, reader| {
                    reader.read_related(pa, |w, a| f(b, w, a));
                }),
                Appender::Part(builder_part) => {
                    Box::new(move |_, b, _| b.push(builder_part.clone()))
                }
            },
            Box::new(move || self.checker.as_ref().is_some_and(|check| check())),
        )
    }
}

impl<D: Display + Clone + 'static, U: Ui> From<D> for State<U, D, D> {
    fn from(value: D) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(value)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<Text> for State<U, ()> {
    fn from(value: Text) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(value)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D: Display + Clone + 'static, U: Ui> From<RwData<D>> for State<U, DataArg<D>> {
    fn from(value: RwData<D>) -> Self {
        Self {
            appender: Appender::PassArg({
                let value = value.clone();
                Box::new(move |pa, b| value.read(pa, |d| b.push(d)))
            }),
            checker: Some(Box::new(value.checker())),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<RwData<Text>> for State<U, DataArg<()>> {
    fn from(value: RwData<Text>) -> Self {
        let checker = value.checker();
        Self {
            appender: Appender::PassArg({
                Box::new(move |pa, b| value.read(pa, |d| b.push(d.clone())))
            }),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<I: ?Sized, O: Display, U: Ui> From<DataMap<I, O>> for State<U, DataArg<String>> {
    fn from(value: DataMap<I, O>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui, I: ?Sized> From<DataMap<I, Text>> for State<U, DataArg<Text>> {
    fn from(value: DataMap<I, Text>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<U, F, I, O> From<F> for State<U, IntoDataMapArg<String>>
where
    U: Ui,
    F: FnOnce() -> DataMap<I, O>,
    I: ?Sized + 'static,
    O: Display + 'static,
{
    fn from(value: F) -> Self {
        let value = value();
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<U, F, I> From<F> for State<U, IntoDataMapArg<Text>>
where
    U: Ui,
    F: FnOnce() -> DataMap<I, Text>,
    I: ?Sized + 'static,
{
    fn from(value: F) -> Self {
        let value = value();
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<U, F, D> From<F> for State<U, IntoRwDataArg<String>>
where
    U: Ui,
    F: FnOnce() -> RwData<D>,
    D: Display + Clone + 'static,
{
    fn from(value: F) -> Self {
        let value = value();
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value.get(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<U, F> From<F> for State<U, IntoRwDataArg<Text>>
where
    U: Ui,
    F: FnOnce() -> RwData<Text>,
{
    fn from(value: F) -> Self {
        let value = value();
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value.get(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<D, Reader, Checker, U> From<(Reader, Checker)> for State<U, NoArg<String>>
where
    D: Display,
    Reader: Fn(&Pass) -> D + 'static,
    Checker: Fn() -> bool + 'static,
    U: Ui,
{
    fn from((value, checker): (Reader, Checker)) -> Self {
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<Reader, Checker, U> From<(Reader, Checker)> for State<U, NoArg<Text>>
where
    Reader: Fn(&Pass) -> Text + 'static,
    Checker: Fn() -> bool + 'static,
    U: Ui,
{
    fn from((value, checker): (Reader, Checker)) -> Self {
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> From<ReadFn> for State<U, WidgetArg<String>, String, W>
where
    D: Display + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&W) -> D + 'static,
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
    ReadFn: Fn(&W) -> Text + 'static,
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
    D: Display + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&W, &U::Area) -> D + 'static,
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
    ReadFn: Fn(&W, &U::Area) -> Text + 'static,
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

impl<U: Ui> From<Spacer> for State<U, ()> {
    fn from(_: Spacer) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(Spacer)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<T: Into<Text> + Clone, U: Ui> From<Ghost<T>> for State<U, ()> {
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
pub struct IntoDataMapArg<T>(PhantomData<T>);
#[derive(Clone)]
pub struct IntoRwDataArg<T>(PhantomData<T>);
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
type PassArgFn = Box<dyn FnMut(&Pass, &mut Builder) + 'static>;
type WidgetAreaFn<W, U> = Box<dyn FnMut(&mut Builder, &W, &<U as Ui>::Area) + 'static>;
type BuilderFn<U> = Box<dyn FnMut(&Pass, &mut Builder, &FileHandle<U>)>;
