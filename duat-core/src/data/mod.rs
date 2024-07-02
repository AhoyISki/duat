//! Data types that are meant to be shared and read across Duat.
//!
//! The data types revolve around the [`RwLock<T>`] struct from std,
//! and are adapters that may block the mutation of the inner data,
//! for the purpose of making it available for reading to any
//! extension on Duat.
//!
//! The first data type is [`RwData<T>`], which is a read and write
//! wrapper over information. It should mostly not be shared, being
//! used instead to write information while making sure that no other
//! part of the code is still reading it. The second data type is
//! [`RoData<T>`], or read only data. It is derived from the first
//! one, and cannot mutate the inner data.
//!
//! The most common usecase for these data types is in the
//! [`FileWidget], where many readers
//! can peer into the [`Text`] or other useful
//! information, such as the printed lines, cursors, etc.
//!
//! [`FileWidget]: crate::FileWidget<U>
//! [`Text`]: crate::text::Text
pub use self::{
    ro::RoData,
    rw::{ReadWriteGuard, RwData},
    statics::{CurrentFile, CurrentWidget, FileReader},
};

mod ro;
mod rw;
mod statics;

/// Private trait for the [`RwData`] and [`RoData`] structs.
///
/// [`RwData`]: super::RwData
/// [`RoData`]: super::RoData
pub trait Data<T>: private::InnerData<T>
where
    T: ?Sized,
{
}

mod private {
    use std::sync::{atomic::AtomicUsize, Arc};

    use parking_lot::RwLockReadGuard;

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
