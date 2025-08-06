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
    data::{DataMap, RwData},
    prelude::*,
    text::{Builder, BuilderPart},
};

use crate::widgets::status_line::CheckerFn;

/// A struct that reads state in order to return [`Text`].
enum Appender<U: Ui, _T: Clone = (), D: Display + Clone = String, W: Widget<U> = File<U>> {
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
pub struct State<U, _T = (), D = String, W = File<U>>
where
    U: Ui,
    _T: Clone + Send,
    D: Display + Clone + Send,
    W: Widget<U>,
{
    appender: Appender<U, _T, D, W>,
    checker: Option<CheckerFn>,
    ghost: PhantomData<_T>,
}

impl<U, _T, D, W> State<U, _T, D, W>
where
    U: Ui,
    _T: Clone + Send + 'static,
    D: Display + Clone + Send + 'static,
    W: Widget<U>,
{
    /// Returns the two building block functions for the
    /// [`Statusline`]
    ///
    /// [`StatusLine`]: super::StatusLine
    pub fn fns(self) -> StateFns<U> {
        (
            match self.appender {
                Appender::PassArg(f) => Box::new(move |pa, b, _| f(pa, b)),
                Appender::FromWidget(f) => Box::new(move |pa, b, reader| {
                    if let Some(handle) = reader.get_related(pa) {
                        f(b, pa, handle.read(pa), handle.area(pa));
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

/// Creates a [`State`] from some value, making use of a [`Pass`]
#[doc(hidden)]
pub trait FromWithPass<T> {
    fn from_with_pass(pa: &Pass, value: T) -> Self;
}

impl<D: Display + Clone + Send + 'static, U: Ui> FromWithPass<D> for State<U, D, D> {
    fn from_with_pass(_: &Pass, value: D) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(value)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> FromWithPass<Text> for State<U, ()> {
    fn from_with_pass(_: &Pass, value: Text) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(value)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D: Display + Clone + Send + 'static, U: Ui> FromWithPass<RwData<D>> for State<U, DataArg<D>> {
    fn from_with_pass(_: &Pass, value: RwData<D>) -> Self {
        Self {
            appender: Appender::PassArg({
                let value = value.clone();
                Box::new(move |pa, b| b.push(value.read(pa)))
            }),
            checker: Some({
                let checker = value.checker();
                Box::new(move |_| checker())
            }),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> FromWithPass<RwData<Text>> for State<U, DataArg<()>> {
    fn from_with_pass(_: &Pass, value: RwData<Text>) -> Self {
        let checker = value.checker();
        Self {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value.read(pa).clone()))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<I: ?Sized, O: Display, U: Ui> FromWithPass<DataMap<I, O>> for State<U, DataArg<String>> {
    fn from_with_pass(_: &Pass, value: DataMap<I, O>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui, I: ?Sized> FromWithPass<DataMap<I, Text>> for State<U, DataArg<Text>> {
    fn from_with_pass(_: &Pass, value: DataMap<I, Text>) -> Self {
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<U, F, I, O> FromWithPass<F> for State<U, IntoDataMapArg<String>>
where
    U: Ui,
    F: FnOnce(&Pass) -> DataMap<I, O>,
    I: ?Sized + 'static,
    O: Display + 'static,
{
    fn from_with_pass(pa: &Pass, value: F) -> Self {
        let value = value(pa);
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<U, F, I> FromWithPass<F> for State<U, IntoDataMapArg<Text>>
where
    U: Ui,
    F: FnOnce(&Pass) -> DataMap<I, Text>,
    I: ?Sized + 'static,
{
    fn from_with_pass(pa: &Pass, value: F) -> Self {
        let value = value(pa);
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<U, F, D> FromWithPass<F> for State<U, IntoRwDataArg<String>>
where
    U: Ui,
    F: FnOnce(&Pass) -> RwData<D>,
    D: Display + Clone + 'static,
{
    fn from_with_pass(pa: &Pass, value: F) -> Self {
        let value = value(pa);
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value.read(pa).clone()))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<U, F> FromWithPass<F> for State<U, IntoRwDataArg<Text>>
where
    U: Ui,
    F: FnOnce(&Pass) -> RwData<Text>,
{
    fn from_with_pass(pa: &Pass, value: F) -> Self {
        let value = value(pa);
        let checker = value.checker();
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value.read(pa).clone()))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<D, Parser, Checker, U> FromWithPass<(Parser, Checker)> for State<U, NoArg<String>>
where
    D: Display,
    Parser: Fn(&Pass) -> D + Send + 'static,
    Checker: Fn(&Pass) -> bool + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, (value, checker): (Parser, Checker)) -> Self {
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<Parser, Checker, U> FromWithPass<(Parser, Checker)> for State<U, NoArg<Text>>
where
    Parser: Fn(&Pass) -> Text + Send + 'static,
    Checker: Fn() -> bool + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, (value, checker): (Parser, Checker)) -> Self {
        State {
            appender: Appender::PassArg(Box::new(move |pa, b| b.push(value(pa)))),
            checker: Some(Box::new(move |_| checker())),
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> FromWithPass<ReadFn> for State<U, WidgetArg<String>, String, W>
where
    D: Display + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&W) -> D + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, _, widget, _| b.push(value(widget)))),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn, U> FromWithPass<ReadFn> for State<U, WidgetArg<Text>, String, W>
where
    W: Widget<U> + Sized,
    ReadFn: Fn(&W) -> Text + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, _, widget, _| b.push(value(widget)))),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> FromWithPass<ReadFn> for State<U, WidgetAreaArg<String>, String, W>
where
    D: Display + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&W, &U::Area) -> D + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, _, widget, area| {
                b.push(value(widget, area))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn, U> FromWithPass<ReadFn> for State<U, WidgetAreaArg<Text>, String, W>
where
    W: Widget<U> + Sized,
    ReadFn: Fn(&W, &U::Area) -> Text + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, _, widget, area| {
                b.push(value(widget, area))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> FromWithPass<ReadFn> for State<U, PassWidgetArg<String>, String, W>
where
    D: Display + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&Pass, &W) -> D + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, pa, widget, _| {
                b.push(value(pa, widget))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn, U> FromWithPass<ReadFn> for State<U, PassWidgetArg<Text>, String, W>
where
    W: Widget<U> + Sized,
    ReadFn: Fn(&Pass, &W) -> Text + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, pa, widget, _| {
                b.push(value(pa, widget))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> FromWithPass<ReadFn> for State<U, PassWidgetAreaArg<String>, String, W>
where
    D: Display + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&Pass, &W, &U::Area) -> D + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, pa, widget, area| {
                b.push(value(pa, widget, area))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn, U> FromWithPass<ReadFn> for State<U, PassWidgetAreaArg<Text>, String, W>
where
    W: Widget<U> + Sized,
    ReadFn: Fn(&Pass, &W, &U::Area) -> Text + Send + 'static,
    U: Ui,
{
    fn from_with_pass(_: &Pass, value: ReadFn) -> Self {
        State {
            appender: Appender::FromWidget(Box::new(move |b, pa, widget, area| {
                b.push(value(pa, widget, area))
            })),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> FromWithPass<AlignLeft> for State<U, AlignLeft> {
    fn from_with_pass(_: &Pass, _: AlignLeft) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::AlignLeft),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> FromWithPass<AlignCenter> for State<U, AlignCenter> {
    fn from_with_pass(_: &Pass, _: AlignCenter) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::AlignCenter),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> FromWithPass<AlignRight> for State<U, AlignRight> {
    fn from_with_pass(_: &Pass, _: AlignRight) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::AlignRight),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> FromWithPass<Spacer> for State<U, ()> {
    fn from_with_pass(_: &Pass, _: Spacer) -> Self {
        Self {
            appender: Appender::Part(BuilderPart::from(Spacer)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<T: Into<Text> + Clone, U: Ui> FromWithPass<Ghost<T>> for State<U, ()> {
    fn from_with_pass(_: &Pass, value: Ghost<T>) -> Self {
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
#[derive(Clone)]
pub struct PassWidgetArg<W>(PhantomData<W>);
#[doc(hidden)]
#[derive(Clone)]
pub struct PassWidgetAreaArg<W>(PhantomData<W>);

// The various types of function aliases
type PassArgFn = Box<dyn Fn(&Pass, &mut Builder) + 'static + Send>;
type WidgetAreaFn<W, U> = Box<dyn Fn(&mut Builder, &Pass, &W, &<U as Ui>::Area) + Send + 'static>;
type BuilderFn<U> = Box<dyn Fn(&Pass, &mut Builder, &Handle<File<U>, U>) + Send>;
type StateFns<U> = (BuilderFn<U>, Box<dyn Fn(&Pass) -> bool + Send>);
