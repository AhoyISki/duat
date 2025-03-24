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
    text::{Builder, Tag, Text},
    ui::Ui,
    widgets::Widget,
};

/// A struct that reads state in order to return [`Text`].
enum Appender<W, U: Ui> {
    NoArgs(Box<dyn FnMut() -> Append + Send + 'static>),
    FromWidget(WidgetAreaFn<W, U>),
    Str(String),
    Text(Text),
    Tag(Tag),
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
pub struct State<W: 'static, Dummy, U: Ui> {
    appender: Appender<W, U>,
    checker: Option<Box<dyn Fn() -> bool + Send + Sync>>,
    ghost: PhantomData<Dummy>,
}

impl<W: 'static, Dummy, U: Ui> State<W, Dummy, U> {
    pub fn fns(self) -> (ReaderFn<U>, Box<dyn Fn() -> bool + Send + Sync>) {
        (
            match self.appender {
                Appender::NoArgs(mut f) => Box::new(move |b, _| f().push_to(b)),
                Appender::FromWidget(mut f) => Box::new(move |b, reader| {
                    if let Some(append) = reader.inspect_related(&mut f) {
                        append.push_to(b)
                    }
                }),
                Appender::Str(str) => Box::new(move |b, _| {
                    if !(str == " " && b.last_was_empty()) {
                        b.push_str(&str)
                    }
                }),
                Appender::Text(text) => Box::new(move |b, _| b.push_text(text.clone())),
                Appender::Tag(tag) => Box::new(move |b, _| {
                    b.push_tag(tag.clone());
                }),
            },
            Box::new(move || self.checker.as_ref().is_some_and(|check| check())),
        )
    }
}

impl<D: Display + Send , U: Ui> From<D> for State<(), String, U> {
    fn from(value: D) -> Self {
        Self {
            appender: Appender::Str::<(), U>(value.to_string()),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<Text> for State<(), Text, U> {
    fn from(value: Text) -> Self {
        Self {
            appender: Appender::Text::<(), U>(value),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D: Display + Send, U: Ui> From<RwData<D>> for State<(), DataArg<String>, U> {
    fn from(value: RwData<D>) -> Self {
        Self {
            appender: Appender::NoArgs::<(), U>({
                let value = value.clone();
                Box::new(move || Append::String(value.read().to_string()))
            }),
            checker: Some(Box::new(value.checker())),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<RwData<Text>> for State<(), DataArg<Text>, U> {
    fn from(value: RwData<Text>) -> Self {
        Self {
            appender: Appender::NoArgs::<(), U>({
                let value = value.clone();
                Box::new(move || Append::Text(value.read().clone()))
            }),
            checker: Some(Box::new(value.checker())),
            ghost: PhantomData,
        }
    }
}

impl<U, I, O> From<DataMap<I, O>> for State<(), DataArg<String>, U>
where
    U: Ui,
    I: ?Sized + Send,
    O: Display + 'static,
{
    fn from(value: DataMap<I, O>) -> Self {
        let (mut reader, checker) = value.fns();
        State {
            appender: Appender::NoArgs(Box::new(move || Append::String(reader().to_string()))),
            checker: Some(checker),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui, I: ?Sized + Send> From<DataMap<I, Text>> for State<(), DataArg<Text>, U> {
    fn from(value: DataMap<I, Text>) -> Self {
        let (mut reader, checker) = value.fns();
        let reader = move || Append::Text(reader());
        State {
            appender: Appender::NoArgs(Box::new(reader)),
            checker: Some(checker),
            ghost: PhantomData,
        }
    }
}

impl<U, F, I, O> From<F> for State<(), IntoDataArg<String>, U>
where
    U: Ui,
    F: FnOnce() -> DataMap<I, O>,
    I: ?Sized + Send + 'static,
    O: Display + 'static,
{
    fn from(value: F) -> Self {
        let (mut reader, checker) = value().fns();
        State {
            appender: Appender::NoArgs(Box::new(move || Append::String(reader().to_string()))),
            checker: Some(checker),
            ghost: PhantomData,
        }
    }
}

impl<U, F, I> From<F> for State<(), IntoDataArg<Text>, U>
where
    U: Ui,
    F: FnOnce() -> DataMap<I, Text>,
    I: ?Sized + Send + 'static,
{
    fn from(value: F) -> Self {
        let (mut reader, checker) = value().fns();
        let reader = move || Append::Text(reader());
        State {
            appender: Appender::NoArgs(Box::new(reader)),
            checker: Some(checker),
            ghost: PhantomData,
        }
    }
}

impl<D, Reader, Checker, U> From<(Reader, Checker)> for State<(), NoArg<String>, U>
where
    D: Display,
    Reader: Fn() -> D + Send + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
    U: Ui,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        let reader = move || Append::String(reader().to_string());
        State {
            appender: Appender::NoArgs::<(), U>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<Reader, Checker, U> From<(Reader, Checker)> for State<(), NoArg<Text>, U>
where
    Reader: Fn() -> Text + Send + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
    U: Ui,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        let reader = move || Append::Text(reader());
        State {
            appender: Appender::NoArgs::<(), U>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> From<ReadFn> for State<W, WidgetArg<String>, U>
where
    D: Display + Send + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&W) -> D + Send + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |widget: &W, _area: &U::Area| -> Append {
            Append::String(reader(widget).to_string())
        };
        State {
            appender: Appender::FromWidget(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn, U> From<ReadFn> for State<W, WidgetArg<Text>, U>
where
    W: Widget<U> + Sized,
    ReadFn: Fn(&W) -> Text + Send + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |widget: &W, _area: &U::Area| -> Append { Append::Text(reader(widget)) };
        State {
            appender: Appender::FromWidget(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> From<ReadFn> for State<W, WidgetAreaArg<String>, U>
where
    D: Display + Send + 'static,
    W: Widget<U> + Sized,
    ReadFn: Fn(&W, &U::Area) -> D + Send + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |widget: &W, area: &U::Area| -> Append {
            Append::String(reader(widget, area).to_string())
        };
        State {
            appender: Appender::FromWidget(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn, U> From<ReadFn> for State<W, WidgetAreaArg<Text>, U>
where
    W: Widget<U> + Sized,
    ReadFn: Fn(&W, &U::Area) -> Text + Send + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader =
            move |widget: &W, area: &U::Area| -> Append { Append::Text(reader(widget, area)) };
        State {
            appender: Appender::FromWidget(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<T: Into<Tag>, U: Ui> From<T> for State<(), Tag, U> {
    fn from(value: T) -> Self {
        Self {
            appender: Appender::Tag::<(), U>(value.into()),
            checker: None,
            ghost: PhantomData,
        }
    }
}

// Dummy structs to prevent implementation conflicts.
#[doc(hidden)]
pub struct DataArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct IntoDataArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct NoArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct WidgetArg<W>(PhantomData<W>);
#[doc(hidden)]
pub struct WidgetAreaArg<W>(PhantomData<W>);

// The various types of function aliases
type WidgetAreaFn<W, U> = Box<dyn FnMut(&W, &<U as Ui>::Area) -> Append + Send + 'static>;
type ReaderFn<U> = Box<dyn FnMut(&mut Builder, &mut Reader<U>) + Send>;

enum Append {
    String(String),
    Text(Text),
}

impl Append {
    fn push_to(self, builder: &mut Builder) {
        match self {
            Append::String(string) => builder.push_str(&string),
            Append::Text(text) => builder.push_text(text),
        }
    }
}
