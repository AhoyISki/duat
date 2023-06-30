use crate::{history::Change, ui::Ui, widgets::FileWidget};

pub trait Observer<U>
where
    U: Ui
{
    fn initial_read(&mut self, file: &mut FileWidget<U>, area: &U::Area);

    fn read_change(&mut self, file: &mut FileWidget<U>, change: &Change, area: &U::Area);
}
