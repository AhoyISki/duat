//! Data types that are meant to be shared and read across Duat.
//!
//! The data types revolve around the [`RwLock`] struct from
//! [`parking_lot`], and are adapters that may block the mutation of
//! the inner data, for the purpose of making it available for reading
//! to any extension on Duat.
//!
//! The first data type is [`RwData`], which is a read and write
//! wrapper over information. It should mostly not be shared, being
//! used instead to write information while making sure that no other
//! part of the code is still reading it. The second data type is
//! [`RoData`], or read only data. It is derived from the first
//! one, and cannot mutate the inner data.
//!
//! This type is used internally in order to keep track of state, like
//! widgets, input methods, etc.
//!
//! One of the main external uses for this macro is in creating
//! automatically update [`StatusLine`] fields.
//!
//! [`File`]: crate::widgets::File
//! [`Text`]: crate::text::Text
//! [`StatusLine`]: crate::widgets::StatusLine
pub use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};

pub use self::{
    context::{Context, CurFile, CurWidget, FileReader},
    ro::RoData,
    rw::{ReadWriteGuard, RwData},
};

mod context;
mod ro;
mod rw;

/// Private trait for the [`RwData`] and [`RoData`] structs.
pub trait Data<T>: private::InnerData<T>
where
    T: ?Sized,
{
    fn to_ro(&self) -> RoData<T>;

    fn has_changed(&self) -> bool;
}

mod private {
    use std::sync::{Arc, atomic::AtomicUsize};

    use super::RwLockReadGuard;

    pub trait InnerData<T: ?Sized> {
        /// The data, usually an [`Arc`]
        fn data(&self) -> RwLockReadGuard<'_, T>;

        /// Attempt to get the data, that is usually an [`Arc`]
        fn try_data(&self) -> Option<RwLockReadGuard<'_, T>>;

        /// The most up to date state of the data.
        fn cur_state(&self) -> &Arc<AtomicUsize>;

        /// The last read state of the data.
        fn read_state(&self) -> &AtomicUsize;
    }
}
