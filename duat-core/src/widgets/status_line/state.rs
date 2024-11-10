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
    NoArgsStr(Box<dyn FnMut() -> String + Send + Sync + 'static>),
    NoArgsText(Box<dyn FnMut() -> Text + Send + Sync + 'static>),
    FromRelatedStr(RelatedStrFn<T>),
    FromRelatedText(RelatedTextFn<T>),
    FromFileAndRelatedStr(FileAndRelatedStrFn<T>),
    FromFileAndRelatedText(FileAndRelatedTextFn<T>),
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
    pub fn fns(self) -> (ReaderFn<U>, Box<dyn Fn() -> bool>) {
        (
            match self.appender {
                Appender::NoArgsStr(mut f) => Box::new(move |builder, _| builder.push_str(f())),
                Appender::NoArgsText(mut f) => Box::new(move |builder, _| builder.push_text(f())),
                Appender::FromRelatedStr(mut f) => Box::new(move |builder, reader| {
                    if let Some(str) = reader.inspect_related(&mut f) {
                        builder.push_str(str)
                    }
                }),
                Appender::FromRelatedText(mut f) => Box::new(move |builder, reader| {
                    if let Some(text) = reader.inspect_related(&mut f) {
                        builder.push_text(text)
                    }
                }),
                Appender::FromFileAndRelatedStr(mut f) => Box::new(move |builder, reader| {
                    if let Some(str) = reader.inspect_file_and(|file, widget| f(file, widget)) {
                        builder.push_str(str)
                    }
                }),
                Appender::FromFileAndRelatedText(mut f) => Box::new(move |builder, reader| {
                    if let Some(text) = reader.inspect_file_and(|file, widget| f(file, widget)) {
                        builder.push_text(text)
                    }
                }),
                Appender::Str(str) => Box::new(move |builder, _| {
                    if !(str == " " && builder.last_was_empty()) {
                        builder.push_str(str.clone())
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
            appender: Appender::NoArgsStr::<()>({
                let value = RoData::from(&value);
                Box::new(move || value.read().to_string())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<RwData<Text>> for State<(), DataArg<Text>, U> {
    fn from(value: RwData<Text>) -> Self {
        Self {
            appender: Appender::NoArgsText::<()>({
                let value = RoData::from(&value);
                Box::new(move || value.read().clone())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            ghost: PhantomData,
        }
    }
}

impl<D: Display + Send + Sync, U: Ui> From<RoData<D>> for State<(), DataArg<String>, U> {
    fn from(value: RoData<D>) -> Self {
        Self {
            appender: Appender::NoArgsStr::<()>({
                let value = value.clone();
                Box::new(move || value.read().to_string())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> From<RoData<Text>> for State<(), DataArg<Text>, U> {
    fn from(value: RoData<Text>) -> Self {
        Self {
            appender: Appender::NoArgsText::<()>({
                let value = value.clone();
                Box::new(move || value.read().clone())
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
            appender: Appender::NoArgsStr(Box::new(move || reader().to_string())),
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
        let (reader, checker) = value.fns();
        State {
            appender: Appender::NoArgsText(reader),
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
            appender: Appender::NoArgsStr(Box::new(move || reader().to_string())),
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
        let (reader, checker) = value().fns();
        State {
            appender: Appender::NoArgsText(reader),
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
        let reader = move || reader().to_string();
        State {
            appender: Appender::NoArgsStr::<()>(Box::new(reader)),
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
        State {
            appender: Appender::NoArgsText::<()>(Box::new(reader)),
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
        let reader = move |arg: &W| reader(arg).to_string();
        State {
            appender: Appender::FromRelatedStr(Box::new(reader)),
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
        let reader = move |arg: &W| reader(arg);
        State {
            appender: Appender::FromRelatedText(Box::new(reader)),
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
        let reader = move |file: &File, arg: &W| reader(file, arg).to_string();
        State {
            appender: Appender::FromFileAndRelatedStr(Box::new(reader)),
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
        State {
            appender: Appender::FromFileAndRelatedText(Box::new(reader)),
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
        let reader = move |arg: &Cursors| reader(arg).to_string();
        State {
            appender: Appender::FromRelatedStr(Box::new(reader)),
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
        let reader = move |arg: &Cursors| reader(arg);
        State {
            appender: Appender::FromRelatedText(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<D, ReadFn, U> From<ReadFn> for State<Cursors, FileAndCursorsArg<String>, U>
where
    D: Display + Send + Sync,
    ReadFn: Fn(&File, &Cursors) -> D + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &Cursors| reader(file, arg).to_string();
        State {
            appender: Appender::FromFileAndRelatedStr(Box::new(reader)),
            checker: None,
            ghost: PhantomData,
        }
    }
}

impl<ReadFn, U> From<ReadFn> for State<Cursors, FileAndCursorsArg<Text>, U>
where
    ReadFn: Fn(&File, &Cursors) -> Text + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &Cursors| reader(file, arg);
        State {
            appender: Appender::FromFileAndRelatedText(Box::new(reader)),
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
#[doc(hidden)]
pub struct FileAndCursorsArg<T>(PhantomData<T>);

// The various types of function aliases
type RelatedStrFn<T> = Box<dyn FnMut(&T) -> String + Send + Sync + 'static>;
type RelatedTextFn<T> = Box<dyn FnMut(&T) -> Text + Send + Sync + 'static>;
type FileAndRelatedStrFn<T> = Box<dyn FnMut(&File, &T) -> String + Send + Sync + 'static>;
type FileAndRelatedTextFn<T> = Box<dyn FnMut(&File, &T) -> Text + Send + Sync + 'static>;

type ReaderFn<U> = Box<dyn FnMut(&mut Builder, &FileReader<U>) + Send + Sync>;
