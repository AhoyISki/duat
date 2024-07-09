use std::{fmt::Display, marker::PhantomData};

use duat_core::{
    data::{FileReader, RoData, RwData},
    input::InputMethod,
    text::{text, Builder, Tag, Text},
    widgets::{File, PassiveWidget},
};

use crate::Ui;

/// A struct that reads state in order to return [`Text`].
enum Appender<T> {
    NoArgsStr(Box<dyn FnMut() -> String + Send + Sync + 'static>),
    NoArgsText(Box<dyn FnMut() -> Text + Send + Sync + 'static>),
    FromInputStr(RelatedStrFn<T>),
    FromInputText(RelatedTextFn<T>),
    FromWidgetStr(RelatedStrFn<T>),
    FromWidgetText(RelatedTextFn<T>),
    FromDynInputStr(DynInputStrFn),
    FromDynInputText(DynInputTextFn),
    FromFileAndWidgetStr(FileAndRelatedStrFn<T>),
    FromFileAndWidgetText(FileAndRelatedTextFn<T>),
    FromFileAndInputStr(FileAndRelatedStrFn<T>),
    FromFileAndInputText(FileAndRelatedTextFn<T>),
    FromFileAndDynInputStr(FileAndDynInputStrFn),
    FromFileAndDynInputText(FileAndDynInputTextFn),
    Str(String),
    Text(Text),
}

/// Part of the [`StatusLine`], can either be a
/// [`&'static str`][str], or a dynamically updated [`Reader`].
pub struct State<T: 'static, Dummy> {
    appender: Appender<T>,
    checker: Option<Box<dyn Fn() -> bool + Send + Sync>>,
    _u: PhantomData<Dummy>,
}

impl<T: 'static, Dummy> State<T, Dummy> {
    pub fn fns(self) -> (ReaderFn, Box<dyn Fn() -> bool>) {
        (
            match self.appender {
                Appender::NoArgsStr(mut f) => Box::new(move |builder, _| builder.push_str(f())),
                Appender::NoArgsText(mut f) => Box::new(move |builder, _| builder.push_text(f())),

                Appender::FromInputStr(mut f) => Box::new(move |builder, reader| {
                    if let Some(str) = reader.inspect_related(&mut f) {
                        builder.push_str(str)
                    }
                }),
                Appender::FromInputText(mut f) => Box::new(move |builder, reader| {
                    if let Some(text) = reader.inspect_related(&mut f) {
                        builder.push_text(text)
                    }
                }),

                Appender::FromWidgetStr(mut f) => Box::new(move |builder, reader| {
                    if let Some(str) = reader.inspect_related(&mut f) {
                        builder.push_str(str)
                    }
                }),
                Appender::FromWidgetText(mut f) => Box::new(move |builder, reader| {
                    if let Some(text) = reader.inspect_related(&mut f) {
                        builder.push_text(text)
                    }
                }),

                Appender::FromDynInputStr(mut f) => Box::new(move |builder, reader| {
                    builder.push_str(reader.inspect(|_, _, input| f(input)))
                }),
                Appender::FromDynInputText(mut f) => Box::new(move |builder, reader| {
                    builder.push_text(reader.inspect(|_, _, input| f(input)))
                }),

                Appender::FromFileAndWidgetStr(mut f) => Box::new(move |builder, reader| {
                    if let Some(str) = reader.inspect_file_and(|file, widget| f(file, widget)) {
                        builder.push_str(str)
                    }
                }),
                Appender::FromFileAndWidgetText(mut f) => Box::new(move |builder, reader| {
                    if let Some(text) = reader.inspect_file_and(|file, widget| f(file, widget)) {
                        builder.push_text(text)
                    }
                }),

                Appender::FromFileAndInputStr(mut f) => Box::new(move |builder, reader| {
                    if let Some(str) = reader.inspect_file_and(|file, input| f(file, input)) {
                        builder.push_str(str)
                    }
                }),
                Appender::FromFileAndInputText(mut f) => Box::new(move |builder, reader| {
                    if let Some(text) = reader.inspect_file_and(|file, input| f(file, input)) {
                        builder.push_text(text)
                    }
                }),

                Appender::FromFileAndDynInputStr(mut f) => Box::new(move |builder, reader| {
                    builder.push_str(reader.inspect(|file, _, input| f(file, input)))
                }),
                Appender::FromFileAndDynInputText(mut f) => Box::new(move |builder, reader| {
                    builder.push_text(reader.inspect(|file, _, input| f(file, input)))
                }),

                Appender::Str(str) => Box::new(move |builder, _| builder.push_str(str.clone())),
                Appender::Text(text) => Box::new(move |builder, _| builder.push_text(text.clone())),
            },
            Box::new(move || self.checker.as_ref().is_some_and(|check| check())),
        )
    }
}

impl<D> From<D> for State<(), String>
where
    D: Display + Send + Sync,
{
    fn from(value: D) -> Self {
        Self {
            appender: Appender::Str::<()>(value.to_string()),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl From<Text> for State<(), Text> {
    fn from(value: Text) -> Self {
        Self {
            appender: Appender::Text::<()>(value),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl From<Tag> for State<(), Tag> {
    fn from(value: Tag) -> Self {
        Self {
            appender: Appender::Text::<()>(text!(value)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D> From<RwData<D>> for State<(), DataArg<String>>
where
    D: Display + Send + Sync,
{
    fn from(value: RwData<D>) -> Self {
        Self {
            appender: Appender::NoArgsStr::<()>({
                let value = RoData::from(&value);
                Box::new(move || value.read().to_string())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl From<RwData<Text>> for State<(), DataArg<Text>> {
    fn from(value: RwData<Text>) -> Self {
        Self {
            appender: Appender::NoArgsText::<()>({
                let value = RoData::from(&value);
                Box::new(move || value.read().clone())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<D> From<RoData<D>> for State<(), DataArg<String>>
where
    D: Display + Send + Sync,
{
    fn from(value: RoData<D>) -> Self {
        Self {
            appender: Appender::NoArgsStr::<()>({
                let value = value.clone();
                Box::new(move || value.read().to_string())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl From<RoData<Text>> for State<(), DataArg<Text>> {
    fn from(value: RoData<Text>) -> Self {
        Self {
            appender: Appender::NoArgsText::<()>({
                let value = value.clone();
                Box::new(move || value.read().clone())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<D, Reader, Checker> From<(Reader, Checker)> for State<(), NoArg<String>>
where
    D: Display + Send + Sync,
    Reader: Fn() -> D + Send + Sync + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        let reader = move || reader().to_string();
        State {
            appender: Appender::NoArgsStr::<()>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            _u: PhantomData,
        }
    }
}

impl<Reader, Checker> From<(Reader, Checker)> for State<(), NoArg<Text>>
where
    Reader: Fn() -> Text + Send + Sync + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        State {
            appender: Appender::NoArgsText::<()>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            _u: PhantomData,
        }
    }
}

impl<D, Input, ReadFn> From<ReadFn> for State<Input, InputArg<String>>
where
    D: Display + Send + Sync,
    Input: InputMethod<Ui, Widget = File> + Sized,
    ReadFn: Fn(&Input) -> D + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Input| reader(arg).to_string();
        State {
            appender: Appender::FromInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<Input, ReadFn> From<ReadFn> for State<Input, InputArg<Text>>
where
    Input: InputMethod<Ui, Widget = File> + Sized,
    ReadFn: Fn(&Input) -> Text + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Input| reader(arg);
        State {
            appender: Appender::FromInputText(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, Widget, ReadFn> From<ReadFn> for State<Widget, WidgetArg<String>>
where
    D: Display + Send + Sync,
    Widget: PassiveWidget<Ui> + Sized,
    ReadFn: Fn(&Widget) -> D + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Widget| reader(arg).to_string();
        State {
            appender: Appender::FromWidgetStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<Widget, ReadFn> From<ReadFn> for State<Widget, WidgetArg<Text>>
where
    Widget: PassiveWidget<Ui> + Sized,
    ReadFn: Fn(&Widget) -> Text + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Widget| reader(arg);
        State {
            appender: Appender::FromWidgetText(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, ReadFn> From<ReadFn> for State<(), DynInputArg<String>>
where
    D: Display + Send + Sync,
    ReadFn: Fn(&dyn InputMethod<Ui>) -> D + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &dyn InputMethod<Ui>| reader(arg).to_string();
        State {
            appender: Appender::FromDynInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<ReadFn> From<ReadFn> for State<(), DynInputArg<Text>>
where
    ReadFn: Fn(&dyn InputMethod<Ui>) -> Text + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        State {
            appender: Appender::FromDynInputText(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, Input, ReadFn> From<ReadFn> for State<Input, FileAndInputArg<String>>
where
    D: Display + Send + Sync,
    Input: InputMethod<Ui, Widget = File> + Sized,
    ReadFn: Fn(&File, &Input) -> D + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &Input| reader(file, arg).to_string();
        State {
            appender: Appender::FromFileAndInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<Input, ReadFn> From<ReadFn> for State<Input, FileAndInputArg<Text>>
where
    Input: InputMethod<Ui, Widget = File> + Sized,
    ReadFn: Fn(&File, &Input) -> Text + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        State {
            appender: Appender::FromFileAndInputText(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, Widget, ReadFn> From<ReadFn> for State<Widget, FileAndWidgetArg<String>>
where
    D: Display + Send + Sync,
    Widget: PassiveWidget<Ui>,
    ReadFn: Fn(&File, &Widget) -> D + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &Widget| reader(file, arg).to_string();
        State {
            appender: Appender::FromFileAndWidgetStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<Widget, ReadFn> From<ReadFn> for State<Widget, FileAndWidgetArg<Text>>
where
    Widget: PassiveWidget<Ui>,
    ReadFn: Fn(&File, &Widget) -> Text + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        State {
            appender: Appender::FromFileAndWidgetText(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, ReadFn> From<ReadFn> for State<(), FileAndDynInputArg<String>>
where
    D: Display + Send + Sync,
    ReadFn: Fn(&File, &dyn InputMethod<Ui>) -> D + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &dyn InputMethod<Ui>| reader(file, arg).to_string();
        State {
            appender: Appender::FromFileAndDynInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<ReadFn> From<ReadFn> for State<(), FileAndDynInputArg<Text>>
where
    ReadFn: Fn(&File, &dyn InputMethod<Ui>) -> Text + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        State {
            appender: Appender::FromFileAndDynInputText(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

// Dummy structs to prevent implementation conflicts.
#[doc(hidden)]
pub struct DataArg<T> {
    _ghost: PhantomData<T>,
}
#[doc(hidden)]
pub struct NoArg<T> {
    _ghost: PhantomData<T>,
}
#[doc(hidden)]
pub struct InputArg<T> {
    _ghost: PhantomData<T>,
}
#[doc(hidden)]
pub struct WidgetArg<T> {
    _ghost: PhantomData<T>,
}
#[doc(hidden)]
pub struct DynInputArg<T> {
    _ghost: PhantomData<T>,
}
#[doc(hidden)]
pub struct FileAndInputArg<T> {
    _ghost: PhantomData<T>,
}
#[doc(hidden)]
pub struct FileAndWidgetArg<T> {
    _ghost: PhantomData<T>,
}
#[doc(hidden)]
pub struct FileAndDynInputArg<T> {
    _ghost: PhantomData<T>,
}

// The various types of function aliases
type RelatedStrFn<T> = Box<dyn FnMut(&T) -> String + Send + Sync + 'static>;
type RelatedTextFn<T> = Box<dyn FnMut(&T) -> Text + Send + Sync + 'static>;
type FileAndRelatedStrFn<T> = Box<dyn FnMut(&File, &T) -> String + Send + Sync + 'static>;
type FileAndRelatedTextFn<T> = Box<dyn FnMut(&File, &T) -> Text + Send + Sync + 'static>;
type DynInputStrFn = Box<dyn FnMut(&dyn InputMethod<Ui>) -> String + Send + Sync + 'static>;
type DynInputTextFn = Box<dyn FnMut(&dyn InputMethod<Ui>) -> Text + Send + Sync + 'static>;
type FileAndDynInputStrFn =
    Box<dyn FnMut(&File, &dyn InputMethod<Ui>) -> String + Send + Sync + 'static>;
type FileAndDynInputTextFn =
    Box<dyn FnMut(&File, &dyn InputMethod<Ui>) -> Text + Send + Sync + 'static>;

type ReaderFn = Box<dyn FnMut(&mut Builder, &FileReader<Ui>) + Send + Sync>;
