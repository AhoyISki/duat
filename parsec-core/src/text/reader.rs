use crate::{history::Change, ui::Ui, widgets::file_widget::FileWidget};

pub trait MutTextReader<U>
where
    U: Ui,
{
    fn initial_read(&mut self, file: &mut FileWidget<U>, label: &U::Label);

    fn read_change(&mut self, file: &mut FileWidget<U>, change: &Change, label: &U::Label);
}
