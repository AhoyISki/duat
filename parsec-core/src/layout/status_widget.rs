use crate::{
    config::{RoState, RwState},
    ui::{EndNode, NodeManager, Ui}, file::{Text, TextLine},
};

use super::{Widget};

enum StatusPrintable {
    Direct(RoState<dyn ToString>),
    Indexed(RoState<dyn ToString>, RoState<usize>),
}

pub struct StatusWidget<U>
where
    U: Ui,
{
    printables: Vec<StatusPrintable>,
    end_node: EndNode<U>,
    text: RwState<Text>
}

impl<U> StatusWidget<U>
where
    U: Ui,
{
    pub(super) fn new(end_node: EndNode<U>, node_manager: &mut NodeManager<U>) -> Self {
        StatusWidget { printables: Vec::new(), end_node, text: RwState::new(Text::default()) }
    }
}

impl<U> Widget<U> for StatusWidget<U>
where
    U: Ui,
{
    fn end_node(&self) -> &EndNode<U> {
        &self.end_node
    }

    fn end_node_mut(&mut self) -> &mut EndNode<U> {
        &mut self.end_node
    }

    fn needs_update(&self) -> bool {
        true
    }

    fn resize(&mut self, node: &EndNode<U>) {}

    fn text(&self) -> RoState<crate::file::Text> {
        (&self.text).into()
    }

    fn update(&mut self) {
        let mut text = self.text.write();
        text.lines.clear();
        text.lines.push(TextLine::new(String::from("my text here lmao")));
    }
}

unsafe impl<U> Send for StatusWidget<U> where U: Ui {}
