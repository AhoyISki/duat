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
//! [data]: crate::data
use std::{fmt::Display, marker::PhantomData};

use crate::{
    context::FileReader,
    data::{DataMap, RoData, RwData},
    mode::Cursors,
    text::{Builder, Tag, Text, text},
    ui::Ui,
    widgets::{File, Widget},
};

/// A struct that reads state in order to return [`Text`].
enum Appender<T> {
    NoArgs(Box<dyn FnMut() -> Append + Send + Sync + 'static>),
    FromWidget(RelatedFn<T>),
    FromFileAndWidget(FileAndRelatedFn<T>),
    FromCursors(RelatedFn<Cursors>),
    Str(String),
    Text(Text),
}

/// A part of the [`StatusLine`]
///
/// This can either be a static part, like [`Text`], [`impl Display`]
/// type, or it can be a reader of the [`File`] and its structs, or it
/// can update independently.
///
/// [`StatusLine`]: super::StatusLine
/// [`impl Display`]: std::fmt::Display
pub struct State<T: 'static, Dummy, U> {
    appender: Appender<T>,
    checker: Option<Box<dyn Fn() -> bool + Send + Sync>>,
    ghost: PhantomData<(Dummy, U)>,
}

impl<T: 'static, Dummy, U: Ui> State<T, Dummy, U> {
    pub fn fns(self) -> (ReaderFn<U>, Box<dyn Fn() -> bool + Send + Sync>) {
        (
            match self.appender {
                Appender::NoArgs(mut f) => Box::new(move |builder, _| f().push_to(builder)),
                Appender::FromWidget(mut f) => Box::new(move |builder, reader| {
                    if let Some(append) = reader.inspect_related(&mut f) {
                        append.push_to(builder)
                    }
                }),
                Appender::FromFileAndWidget(mut f) => Box::new(move |builder, reader| {
                    if let Some(append) = reader.inspect_file_and(|file, widget| f(file, widget)) {
                        append.push_to(builder)
                    }
                }),
                Appender::FromCursors(mut f) => Box::new(move |builder, reader| {
                    reader
                        .inspect(|file, _| f(file.cursors().unwrap()))
                        .push_to(builder);
                }),
                Appender::Str(str) => Box::new(move |builder, _| {
                    if !(str == " " && builder.last_was_empty()) {
                        builder.push_str(&str)
                    }
                }),
                Appender::Text(text) => Box::new(move |builder, _| builder.push_text(text.clone())),
            },
            Box::new(move || self.checker.as_ref().is_some_and(|check| check())),
        )
    }
}

impl<D: Display + Send + Sync, U: Ui> From<D> for State<(), String, U> {
    fn from(value: D) -> Self {
        Self {
            appender: Appender::Str::<()>(value.to_string()),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<Text> for State<(), Text, U> {
    fn from(value: Text) -> Self {
        Self {
            appender: Appender::Text::<()>(value),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<Tag> for State<(), Tag, U> {
    fn from(value: Tag) -> Self {
        Self {
            appender: Appender::Text::<()>(text!(value)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D: Display + Send + Sync, U: Ui> From<RwData<D>> for State<(), DataArg<String>, U> {
    fn from(value: RwData<D>) -> Self {
        Self {
            appender: Appender::NoArgs::<()>({
                let value = RoData::from(&value);
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
            appender: Appender::NoArgs::<()>({
                let value = RoData::from(&value);
                Box::new(move || Append::Text(value.read().clone()))
            }),
            checker: Some(Box::new(value.checker())),
            ghost: PhantomData,
        }
    }
}

impl<D: Display + Send + Sync, U: Ui> From<RoData<D>> for State<(), DataArg<String>, U> {
    fn from(value: RoData<D>) -> Self {
        Self {
            appender: Appender::NoArgs::<()>({
                let value = value.clone();
                Box::new(move || Append::String(value.read().to_string()))
            }),
            checker: Some(Box::new(move || value.has_changed())),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<RoData<Text>> for State<(), DataArg<Text>, U> {
    fn from(value: RoData<Text>) -> Self {
        Self {
            appender: Appender::NoArgs::<()>({
                let value = value.clone();
                Box::new(move || Append::Text(value.read().clone()))
            }),
            checker: Some(Box::new(move || value.has_changed())),
            ghost: PhantomData,
        }
    }
}

impl<U, I, O> From<DataMap<I, O>> for State<(), DataArg<String>, U>
where
    U: Ui,
    I: ?Sized + Send + Sync,
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

impl<U, I> From<DataMap<I, Text>> for State<(), DataArg<Text>, U>
where
    U: Ui,
    I: ?Sized + Send + Sync,
{
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
    I: ?Sized + Send + Sync + 'static,
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
    I: ?Sized + Send + Sync + 'static,
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
    Reader: Fn() -> D + Send + Sync + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
    U: Ui,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        let reader = move || Append::String(reader().to_string());
        State {
            appender: Appender::NoArgs::<()>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<Reader, Checker, U> From<(Reader, Checker)> for State<(), NoArg<Text>, U>
where
    Reader: Fn() -> Text + Send + Sync + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
    U: Ui,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        let reader = move || Append::Text(reader());
        State {
            appender: Appender::NoArgs::<()>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> From<ReadFn> for State<W, WidgetArg<String>, U>
where
    D: Display + Send + Sync,
    W: Widget<U> + Sized,
    ReadFn: Fn(&W) -> D + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &W| Append::String(reader(arg).to_string());
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
    ReadFn: Fn(&W) -> Text + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &W| Append::Text(reader(arg));
        State {
            appender: Appender::FromWidget(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, W, ReadFn, U> From<ReadFn> for State<W, FileAndWidgetArg<String>, U>
where
    D: Display + Send + Sync,
    W: Widget<U>,
    ReadFn: Fn(&File, &W) -> D + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &W| Append::String(reader(file, arg).to_string());
        State {
            appender: Appender::FromFileAndWidget(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<W, ReadFn, U> From<ReadFn> for State<W, FileAndWidgetArg<Text>, U>
where
    W: Widget<U>,
    ReadFn: Fn(&File, &W) -> Text + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, w: &W| Append::Text(reader(file, w));
        State {
            appender: Appender::FromFileAndWidget(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, ReadFn, U> From<ReadFn> for State<Cursors, CursorsArg<String>, U>
where
    D: Display + Send + Sync,
    ReadFn: Fn(&Cursors) -> D + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Cursors| Append::String(reader(arg).to_string());
        State {
            appender: Appender::FromCursors(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<ReadFn, U> From<ReadFn> for State<Cursors, CursorsArg<Text>, U>
where
    ReadFn: Fn(&Cursors) -> Text + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Cursors| Append::Text(reader(arg));
        State {
            appender: Appender::FromCursors(Box::new(reader)),
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
pub struct WidgetArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct FileAndWidgetArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct CursorsArg<T>(PhantomData<T>);

// The various types of function aliases
type RelatedFn<T> = Box<dyn FnMut(&T) -> Append + Send + Sync + 'static>;
type FileAndRelatedFn<T> = Box<dyn FnMut(&File, &T) -> Append + Send + Sync + 'static>;

type ReaderFn<U> = Box<dyn FnMut(&mut Builder, &FileReader<U>) + Send + Sync>;

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
