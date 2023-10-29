use std::{fmt::Display, marker::PhantomData};

use parsec_core::{
    data::{RoData, RwData},
    input::InputMethod,
    text::{text, Builder, Tag, Text},
    widgets::{File, PassiveWidget},
};

use crate::Ui;

/// A struct that reads state in order to return [`Text`].
enum Appender<T> {
    NoArgs(Box<dyn FnMut() -> Text + Send + Sync + 'static>),
    FromInput(RelatedFn<T>),
    FromWidget(RelatedFn<T>),
    FromDynInput(DynInputFn),
    FromFileAndWidget(FileAndRelatedFn<T>),
    FromFileAndInput(FileAndRelatedFn<T>),
    FromFileAndDynInput(FileAndDynInputFn),
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
                Appender::NoArgs(mut f) => Box::new(move |builder, _, _| builder.push_text(f())),

                Appender::FromInput(mut f) => Box::new(move |builder, _, input| {
                    if let Some(text) = input.inspect_as(&mut f) {
                        builder.push_text(text)
                    }
                }),
                Appender::FromWidget(mut f) => Box::new(move |builder, file, _| {
                    if let Some(text) = file
                        .inspect_as(&mut f)
                        .or_else(|| file.read().inspect_related(&mut f))
                    {
                        builder.push_text(text)
                    }
                }),
                Appender::FromDynInput(mut f) => {
                    Box::new(move |builder, _, input| builder.push_text(f(&*input.read())))
                }

                Appender::FromFileAndInput(mut f) => Box::new(move |builder, file, input| {
                    if let Some(text) = input.inspect_as(|input| f(&file.read(), input)) {
                        builder.push_text(text)
                    }
                }),
                Appender::FromFileAndWidget(mut f) => Box::new(move |builder, file, _| {
                    let file = file.read();
                    if let Some(text) = file.inspect_related(|widget| f(&file, widget)) {
                        builder.push_text(text)
                    }
                }),
                Appender::FromFileAndDynInput(mut f) => Box::new(move |builder, file, input| {
                    let text = input.inspect(|input| f(&file.read(), input));
                    builder.push_text(text)
                }),

                Appender::Text(text) => {
                    Box::new(move |builder, _, _| builder.push_text(text.clone()))
                }
            },
            Box::new(move || self.checker.as_ref().is_some_and(|check| check())),
        )
    }
}

macro state_from($($value_type:ty),+) {
    $(
        impl From<$value_type> for State<(), ()> {
            fn from(value: $value_type) -> Self {
                Self {
                    appender: Appender::Text::<()>(Text::from(value)),
                    checker: None,
                    _u: PhantomData
                }
            }
        }
    )+
}

state_from!(
    char,
    &'_ str,
    String,
    Text,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    usize,
    isize
);

impl From<Tag> for State<(), ()> {
    fn from(value: Tag) -> Self {
        Self {
            appender: Appender::Text::<()>(text!(value)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D> From<RwData<D>> for State<(), ()>
where
    D: Display + Send + Sync,
{
    fn from(value: RwData<D>) -> Self {
        Self {
            appender: Appender::NoArgs::<()>({
                let value = value.clone();
                Box::new(move || Text::from(value.read().to_string()))
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<D> From<RoData<D>> for State<(), ()>
where
    D: Display + Send + Sync,
{
    fn from(value: RoData<D>) -> Self {
        Self {
            appender: Appender::NoArgs::<()>({
                let value = value.clone();
                Box::new(move || Text::from(value.read().to_string()))
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl From<RwData<Text>> for State<Text, ()> {
    fn from(value: RwData<Text>) -> Self {
        Self {
            appender: Appender::NoArgs::<Text>({
                let value = value.clone();
                Box::new(move || value.read().clone())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl From<RoData<Text>> for State<Text, ()> {
    fn from(value: RoData<Text>) -> Self {
        Self {
            appender: Appender::NoArgs::<Text>({
                let value = value.clone();
                Box::new(move || value.read().clone())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<T, Reader, Checker> From<(Reader, Checker)> for State<(), ()>
where
    T: Into<Text>,
    Reader: Fn() -> T + Send + Sync + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        let reader = move || reader().into();
        State {
            appender: Appender::NoArgs::<()>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            _u: PhantomData,
        }
    }
}

impl<ToText, Input, ReadFn> From<ReadFn> for State<Input, InputArg>
where
    ToText: Into<Text>,
    Input: InputMethod<Ui, Widget = File<Ui>> + Sized,
    ReadFn: Fn(&Input) -> ToText + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Input| reader(arg).into();
        State {
            appender: Appender::FromInput(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<ToText, Widget, ReadFn> From<ReadFn> for State<Widget, WidgetArg>
where
    ToText: Into<Text>,
    Widget: PassiveWidget<Ui> + Sized,
    ReadFn: Fn(&Widget) -> ToText + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Widget| reader(arg).into();
        State {
            appender: Appender::FromWidget(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<ToText, ReadFn> From<ReadFn> for State<(), DynInputArg>
where
    ToText: Into<Text>,
    ReadFn: Fn(&dyn InputMethod<Ui>) -> ToText + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &dyn InputMethod<Ui>| reader(arg).into();
        State {
            appender: Appender::FromDynInput(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<ToText, Input, ReadFn> From<ReadFn> for State<Input, FileAndInputArg>
where
    ToText: Into<Text>,
    Input: InputMethod<Ui, Widget = File<Ui>> + Sized,
    ReadFn: Fn(&File<Ui>, &Input) -> ToText + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File<Ui>, arg: &Input| reader(file, arg).into();
        State {
            appender: Appender::FromFileAndInput(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<ToText, Widget, ReadFn> From<ReadFn> for State<Widget, FileAndWidgetArg>
where
    ToText: Into<Text>,
    Widget: PassiveWidget<Ui>,
    ReadFn: Fn(&File<Ui>, &Widget) -> ToText + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File<Ui>, arg: &Widget| reader(file, arg).into();
        State {
            appender: Appender::FromFileAndWidget(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<ToText, ReadFn> From<ReadFn> for State<(), FileAndDynInputArg>
where
    ToText: Into<Text>,
    ReadFn: Fn(&File<Ui>, &dyn InputMethod<Ui>) -> ToText + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File<Ui>, arg: &dyn InputMethod<Ui>| reader(file, arg).into();
        State {
            appender: Appender::FromFileAndDynInput(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

// Dummy structs to prevent implementation conflicts.
pub struct InputArg;
pub struct WidgetArg;
pub struct DynInputArg;
pub struct FileAndInputArg;
pub struct FileAndWidgetArg;
pub struct FileAndDynInputArg;

// The various types of function aliases
type RelatedFn<T> = Box<dyn FnMut(&T) -> Text + Send + Sync + 'static>;
type FileAndRelatedFn<T> = Box<dyn FnMut(&File<Ui>, &T) -> Text + Send + Sync + 'static>;
type DynInputFn = Box<dyn FnMut(&dyn InputMethod<Ui>) -> Text + Send + Sync + 'static>;
type FileAndDynInputFn =
    Box<dyn FnMut(&File<Ui>, &dyn InputMethod<Ui>) -> Text + Send + Sync + 'static>;

type ReaderFn =
    Box<dyn FnMut(&mut Builder, &RoData<File<Ui>>, &RoData<dyn InputMethod<Ui>>) + Send + Sync>;
