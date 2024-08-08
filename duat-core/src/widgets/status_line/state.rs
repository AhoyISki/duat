use std::{fmt::Display, marker::PhantomData};

use crate::{
    data::{FileReader, RoData, RwData},
    input::InputMethod,
    text::{text, Builder, Tag, Text},
    ui::Ui,
    widgets::{File, PassiveWidget},
};

/// A struct that reads state in order to return [`Text`].
enum Appender<T, U> {
    NoArgsStr(Box<dyn FnMut() -> String + Send + Sync + 'static>),
    NoArgsText(Box<dyn FnMut() -> Text + Send + Sync + 'static>),
    FromInputStr(RelatedStrFn<T>),
    FromInputText(RelatedTextFn<T>),
    FromWidgetStr(RelatedStrFn<T>),
    FromWidgetText(RelatedTextFn<T>),
    FromDynInputStr(DynInputStrFn<U>),
    FromDynInputText(DynInputTextFn<U>),
    FromFileAndWidgetStr(FileAndRelatedStrFn<T>),
    FromFileAndWidgetText(FileAndRelatedTextFn<T>),
    FromFileAndInputStr(FileAndRelatedStrFn<T>),
    FromFileAndInputText(FileAndRelatedTextFn<T>),
    FromFileAndDynInputStr(FileAndDynInputStrFn<U>),
    FromFileAndDynInputText(FileAndDynInputTextFn<U>),
    Str(String),
    Text(Text),
}

/// Part of the [`StatusLine`], can either be a
/// [`&'static str`][str], or a dynamically updated [`Reader`].
pub struct State<T: 'static, Dummy, U> {
    appender: Appender<T, U>,
    checker: Option<Box<dyn Fn() -> bool + Send + Sync>>,
    _u: PhantomData<Dummy>,
}

impl<T: 'static, Dummy, U> State<T, Dummy, U>
where
    U: Ui,
{
    pub fn fns(self) -> (ReaderFn<U>, Box<dyn Fn() -> bool>) {
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

impl<D, U> From<D> for State<(), String, U>
where
    D: Display + Send + Sync,
    U: Ui,
{
    fn from(value: D) -> Self {
        Self {
            appender: Appender::Str::<(), U>(value.to_string()),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, U> From<Option<D>> for State<(), Option<String>, U>
where
    D: Display + Send + Sync,
    U: Ui,
{
    fn from(value: Option<D>) -> Self {
        Self {
            appender: Appender::Str::<(), U>(value.map(|d| d.to_string()).unwrap_or_default()),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<U> From<Text> for State<(), Text, U>
where
    U: Ui,
{
    fn from(value: Text) -> Self {
        Self {
            appender: Appender::Text::<(), U>(value),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<U> From<Tag> for State<(), Tag, U>
where
    U: Ui,
{
    fn from(value: Tag) -> Self {
        Self {
            appender: Appender::Text::<(), U>(text!(value)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, U> From<RwData<D>> for State<(), DataArg<String>, U>
where
    D: Display + Send + Sync,
    U: Ui,
{
    fn from(value: RwData<D>) -> Self {
        Self {
            appender: Appender::NoArgsStr::<(), U>({
                let value = RoData::from(&value);
                Box::new(move || value.read().to_string())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<D, U> From<RwData<Option<D>>> for State<(), DataArg<Option<String>>, U>
where
    D: Display + Send + Sync,
    U: Ui,
{
    fn from(value: RwData<Option<D>>) -> Self {
        Self {
            appender: Appender::NoArgsStr::<(), U>({
                let value = RoData::from(&value);
                Box::new(move || value.read().as_ref().map(D::to_string).unwrap_or_default())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<U> From<RwData<Text>> for State<(), DataArg<Text>, U>
where
    U: Ui,
{
    fn from(value: RwData<Text>) -> Self {
        Self {
            appender: Appender::NoArgsText::<(), U>({
                let value = RoData::from(&value);
                Box::new(move || value.read().clone())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<D, U> From<RoData<D>> for State<(), DataArg<String>, U>
where
    D: Display + Send + Sync,
    U: Ui,
{
    fn from(value: RoData<D>) -> Self {
        Self {
            appender: Appender::NoArgsStr::<(), U>({
                let value = value.clone();
                Box::new(move || value.read().to_string())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<D, U> From<RoData<Option<D>>> for State<(), DataArg<Option<String>>, U>
where
    D: Display + Send + Sync,
    U: Ui,
{
    fn from(value: RoData<Option<D>>) -> Self {
        Self {
            appender: Appender::NoArgsStr::<(), U>({
                let value = value.clone();
                Box::new(move || value.read().as_ref().map(D::to_string).unwrap_or_default())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<U> From<RoData<Text>> for State<(), DataArg<Text>, U>
where
    U: Ui,
{
    fn from(value: RoData<Text>) -> Self {
        Self {
            appender: Appender::NoArgsText::<(), U>({
                let value = value.clone();
                Box::new(move || value.read().clone())
            }),
            checker: Some(Box::new(move || value.has_changed())),
            _u: PhantomData,
        }
    }
}

impl<D, Reader, Checker, U> From<(Reader, Checker)> for State<(), NoArg<String>, U>
where
    D: Display + Send + Sync,
    Reader: Fn() -> D + Send + Sync + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
    U: Ui,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        let reader = move || reader().to_string();
        State {
            appender: Appender::NoArgsStr::<(), U>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            _u: PhantomData,
        }
    }
}

impl<D, Reader, Checker, U> From<(Reader, Checker)> for State<(), NoArg<Option<String>>, U>
where
    D: Display + Send + Sync,
    Reader: Fn() -> Option<D> + Send + Sync + 'static,
    Checker: Fn() -> bool + Send + Sync + 'static,
    U: Ui,
{
    fn from((reader, checker): (Reader, Checker)) -> Self {
        let reader = move || reader().map(|d| d.to_string()).unwrap_or_default();
        State {
            appender: Appender::NoArgsStr::<(), U>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            _u: PhantomData,
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
            appender: Appender::NoArgsText::<(), U>(Box::new(reader)),
            checker: Some(Box::new(checker)),
            _u: PhantomData,
        }
    }
}

impl<D, Input, ReadFn, U> From<ReadFn> for State<Input, InputArg<String>, U>
where
    D: Display + Send + Sync,
    Input: InputMethod<U, Widget = File> + Sized,
    ReadFn: Fn(&Input) -> D + Send + Sync + 'static,
    U: Ui,
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

impl<D, Input, ReadFn, U> From<ReadFn> for State<Input, InputArg<Option<String>>, U>
where
    D: Display + Send + Sync,
    Input: InputMethod<U, Widget = File> + Sized,
    ReadFn: Fn(&Input) -> Option<D> + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Input| reader(arg).map(|d| d.to_string()).unwrap_or_default();
        State {
            appender: Appender::FromInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<Input, ReadFn, U> From<ReadFn> for State<Input, InputArg<Text>, U>
where
    Input: InputMethod<U, Widget = File> + Sized,
    ReadFn: Fn(&Input) -> Text + Send + Sync + 'static,
    U: Ui,
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

impl<D, Widget, ReadFn, U> From<ReadFn> for State<Widget, WidgetArg<String>, U>
where
    D: Display + Send + Sync,
    Widget: PassiveWidget<U> + Sized,
    ReadFn: Fn(&Widget) -> D + Send + Sync + 'static,
    U: Ui,
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

impl<D, Widget, ReadFn, U> From<ReadFn> for State<Widget, WidgetArg<Option<String>>, U>
where
    D: Display + Send + Sync,
    Widget: PassiveWidget<U> + Sized,
    ReadFn: Fn(&Widget) -> Option<D> + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &Widget| reader(arg).map(|d| d.to_string()).unwrap_or_default();
        State {
            appender: Appender::FromWidgetStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<Widget, ReadFn, U> From<ReadFn> for State<Widget, WidgetArg<Text>, U>
where
    Widget: PassiveWidget<U> + Sized,
    ReadFn: Fn(&Widget) -> Text + Send + Sync + 'static,
    U: Ui,
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

impl<D, ReadFn, U> From<ReadFn> for State<(), DynInputArg<String>, U>
where
    D: Display + Send + Sync,
    ReadFn: Fn(&dyn InputMethod<U>) -> D + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |arg: &dyn InputMethod<U>| reader(arg).to_string();
        State {
            appender: Appender::FromDynInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, ReadFn, U> From<ReadFn> for State<(), DynInputArg<Option<String>>, U>
where
    D: Display + Send + Sync,
    ReadFn: Fn(&dyn InputMethod<U>) -> Option<D> + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader =
            move |arg: &dyn InputMethod<U>| reader(arg).map(|d| d.to_string()).unwrap_or_default();
        State {
            appender: Appender::FromDynInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<ReadFn, U> From<ReadFn> for State<(), DynInputArg<Text>, U>
where
    ReadFn: Fn(&dyn InputMethod<U>) -> Text + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        State {
            appender: Appender::FromDynInputText(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, Input, ReadFn, U> From<ReadFn> for State<Input, FileAndInputArg<String>, U>
where
    D: Display + Send + Sync,
    Input: InputMethod<U, Widget = File> + Sized,
    ReadFn: Fn(&File, &Input) -> D + Send + Sync + 'static,
    U: Ui,
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

impl<D, Input, ReadFn, U> From<ReadFn> for State<Input, FileAndInputArg<Option<String>>, U>
where
    D: Display + Send + Sync,
    Input: InputMethod<U, Widget = File> + Sized,
    ReadFn: Fn(&File, &Input) -> Option<D> + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &Input| {
            reader(file, arg).map(|d| d.to_string()).unwrap_or_default()
        };
        State {
            appender: Appender::FromFileAndInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<Input, ReadFn, U> From<ReadFn> for State<Input, FileAndInputArg<Text>, U>
where
    Input: InputMethod<U, Widget = File> + Sized,
    ReadFn: Fn(&File, &Input) -> Text + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        State {
            appender: Appender::FromFileAndInputText(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, Widget, ReadFn, U> From<ReadFn> for State<Widget, FileAndWidgetArg<String>, U>
where
    D: Display + Send + Sync,
    Widget: PassiveWidget<U>,
    ReadFn: Fn(&File, &Widget) -> D + Send + Sync + 'static,
    U: Ui,
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

impl<D, Widget, ReadFn, U> From<ReadFn> for State<Widget, FileAndWidgetArg<Option<String>>, U>
where
    D: Display + Send + Sync,
    Widget: PassiveWidget<U>,
    ReadFn: Fn(&File, &Widget) -> Option<D> + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &Widget| {
            reader(file, arg).map(|d| d.to_string()).unwrap_or_default()
        };
        State {
            appender: Appender::FromFileAndWidgetStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<Widget, ReadFn, U> From<ReadFn> for State<Widget, FileAndWidgetArg<Text>, U>
where
    Widget: PassiveWidget<U>,
    ReadFn: Fn(&File, &Widget) -> Text + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        State {
            appender: Appender::FromFileAndWidgetText(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, ReadFn, U> From<ReadFn> for State<(), FileAndDynInputArg<String>, U>
where
    D: Display + Send + Sync,
    ReadFn: Fn(&File, &dyn InputMethod<U>) -> D + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &dyn InputMethod<U>| reader(file, arg).to_string();
        State {
            appender: Appender::FromFileAndDynInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<D, ReadFn, U> From<ReadFn> for State<(), FileAndDynInputArg<Option<String>>, U>
where
    D: Display + Send + Sync,
    ReadFn: Fn(&File, &dyn InputMethod<U>) -> Option<D> + Send + Sync + 'static,
    U: Ui,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &File, arg: &dyn InputMethod<U>| {
            reader(file, arg).map(|d| d.to_string()).unwrap_or_default()
        };
        State {
            appender: Appender::FromFileAndDynInputStr(Box::new(reader)),
            checker: None,
            _u: PhantomData,
        }
    }
}

impl<ReadFn, U> From<ReadFn> for State<(), FileAndDynInputArg<Text>, U>
where
    ReadFn: Fn(&File, &dyn InputMethod<U>) -> Text + Send + Sync + 'static,
    U: Ui,
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
pub struct DataArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct NoArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct InputArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct WidgetArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct DynInputArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct FileAndInputArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct FileAndWidgetArg<T>(PhantomData<T>);
#[doc(hidden)]
pub struct FileAndDynInputArg<T>(PhantomData<T>);

// The various types of function aliases
type RelatedStrFn<T> = Box<dyn FnMut(&T) -> String + Send + Sync + 'static>;
type RelatedTextFn<T> = Box<dyn FnMut(&T) -> Text + Send + Sync + 'static>;
type FileAndRelatedStrFn<T> = Box<dyn FnMut(&File, &T) -> String + Send + Sync + 'static>;
type FileAndRelatedTextFn<T> = Box<dyn FnMut(&File, &T) -> Text + Send + Sync + 'static>;
type DynInputStrFn<U> = Box<dyn FnMut(&dyn InputMethod<U>) -> String + Send + Sync + 'static>;
type DynInputTextFn<U> = Box<dyn FnMut(&dyn InputMethod<U>) -> Text + Send + Sync + 'static>;
type FileAndDynInputStrFn<U> =
    Box<dyn FnMut(&File, &dyn InputMethod<U>) -> String + Send + Sync + 'static>;
type FileAndDynInputTextFn<U> =
    Box<dyn FnMut(&File, &dyn InputMethod<U>) -> Text + Send + Sync + 'static>;

type ReaderFn<U> = Box<dyn FnMut(&mut Builder, &FileReader<U>) + Send + Sync>;
