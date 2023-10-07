use std::fmt::Display;

use super::DynInput;
use crate::{
    data::{RoData, RwData},
    input::InputMethod,
    text::{text, Builder, Tag, Text},
    widgets::File,
};

/// A struct that reads state in order to return [`Text`].
enum Appender<T> {
    NoArgs(Box<dyn FnMut() -> Text + Send + Sync + 'static>),
    FromFile(Box<dyn FnMut(&T) -> Text + Send + Sync + 'static>),
    DynInput(Box<dyn FnMut(&dyn InputMethod) -> Text + Send + Sync + 'static>),
    Text(Text),
}

/// Part of the [`StatusLine`], can either be a
/// [`&'static str`][str], or a dynamically updated [`Reader`].
pub struct State<T: 'static> {
    appender: Appender<T>,
    checker: Option<Box<dyn Fn() -> bool + Send + Sync>>,
}

impl<T: 'static> State<T> {
    pub fn fns(
        self,
    ) -> (
        Box<dyn FnMut(&mut Builder, &RoData<File>, &RoData<dyn InputMethod>) + Send + Sync>,
        Box<dyn Fn() -> bool>,
    ) {
        (
            match self.appender {
                Appender::NoArgs(mut f) => Box::new(move |builder, _, _| builder.push_text(f())),
                Appender::FromFile(mut f) => Box::new(move |builder, file, input| {
                    let text = input
                        .inspect_as::<T, Text>(&mut f)
                        .or_else(|| file.inspect_as::<T, Text>(&mut f))
                        .or_else(|| file.raw_read().inspect_related::<T, Text>(&mut f));

                    if let Some(text) = text {
                        builder.push_text(text)
                    }
                }),
                Appender::DynInput(mut f) => {
                    Box::new(move |builder, _, input| builder.push_text(f(&*input.read())))
                }
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
        impl From<$value_type> for State<()> {
            fn from(value: $value_type) -> Self {
                Self {
                    appender: Appender::Text::<()>(Text::from(value)),
                    checker: None,
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

impl From<Tag> for State<()> {
    fn from(value: Tag) -> Self {
        Self {
            appender: Appender::Text::<()>(text!(value)),
            checker: None,
        }
    }
}

impl<D> From<RwData<D>> for State<()>
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
        }
    }
}

impl<D> From<RoData<D>> for State<()>
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
        }
    }
}

impl From<RwData<Text>> for State<()> {
    fn from(value: RwData<Text>) -> Self {
        Self {
            appender: Appender::NoArgs::<()>({
                let value = value.clone();
                Box::new(move || value.read().clone())
            }),
            checker: Some(Box::new(move || value.has_changed())),
        }
    }
}

impl From<RoData<Text>> for State<()> {
    fn from(value: RoData<Text>) -> Self {
        Self {
            appender: Appender::NoArgs::<()>({
                let value = value.clone();
                Box::new(move || value.read().clone())
            }),
            checker: Some(Box::new(move || value.has_changed())),
        }
    }
}

impl<T, F> From<DynInput<T, F>> for State<()>
where
    T: Into<Text>,
    F: Fn(&dyn InputMethod) -> T + Send + Sync + 'static,
{
    fn from(value: DynInput<T, F>) -> Self {
        let reader = move |input: &dyn InputMethod| (value.0)(input).into();
        Self {
            appender: Appender::DynInput::<()>(Box::new(reader)),
            checker: None,
        }
    }
}

impl<T, Reader, Checker> From<(Reader, Checker)> for State<()>
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
        }
    }
}

impl<ToText, Arg, ReadFn> From<ReadFn> for State<Arg>
where
    ToText: Into<Text>,
    ReadFn: Fn(&Arg) -> ToText + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |u: &Arg| reader(u).into();
        State {
            appender: Appender::FromFile(Box::new(reader)),
            checker: None,
        }
    }
}
