use crate::{
    action::Change,
    ui::{EndNode, Ui},
    widgets::file_widget::FileWidget,
};

pub trait MutTextReader<U>
where
    U: Ui,
{
    fn initial_read(&mut self, file: &mut FileWidget<U>, end_node: &EndNode<U>);

    fn read_change(&mut self, file: &mut FileWidget<U>, change: &Change, end_node: &EndNode<U>);
}
