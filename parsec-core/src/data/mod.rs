//! Data types that are meant to be shared and read across Parsec.
//!
//! The data types revolve around the [`RwLock<T>`] struct from std,
//! and are adapters that may block the mutation of the inner data,
//! for the purpose of making it available for reading to any
//! extension on Parsec.
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

use std::marker::PhantomData;

pub use self::{
    ro::RoData,
    rw::RwData,
    statics::{CurrentFile, CurrentWidget, FileReader},
};

mod ro;
mod rw;
mod statics;

/// Errors related to reading or writing to the various data types.
pub enum Error<Holder, T, C>
where
    Holder: Data<T>,
    T: ?Sized,
    C: ?Sized,
{
    WriteBlocked(PhantomData<(Holder, Box<T>, Box<C>)>),
    ReadBlocked,
    NestedReadBlocked,
    CastingFailed,
}

impl<Holder, T, C> std::fmt::Debug for Error<Holder, T, C>
where
    Holder: Data<T>,
    T: ?Sized,
    C: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let holder = std::any::type_name::<Holder>();
        match self {
            Error::WriteBlocked(_) => {
                write!(f, "The {holder} could not be written to at this moment.")
            }
            Error::ReadBlocked => {
                write!(f, "The {holder} could not be read this moment.")
            }
            Error::NestedReadBlocked => {
                let data = std::any::type_name::<T>();
                write!(
                    f,
                    "In the RonestedData<{data}>, the initial RoData<{holder}> could not be read \
                     at the moment."
                )
            }
            Error::CastingFailed => {
                let orig = std::any::type_name::<T>();
                let cast = std::any::type_name::<C>();
                write!(f, "The given {orig} could not be casted to {cast}.")
            }
        }
    }
}

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
    #[cfg(not(feature = "deadlock-detection"))]
    use std::sync::RwLockReadGuard;
    use std::sync::{atomic::AtomicUsize, Arc, TryLockResult};

    #[cfg(feature = "deadlock-detection")]
    use no_deadlocks::RwLockReadGuard;

    pub trait InnerData<T: ?Sized> {
        /// The data, usually an [`Arc`]
        fn data(&self) -> RwLockReadGuard<'_, T>;

        /// Attempt to get the data, that is usually an [`Arc`]
        fn try_data(&self) -> TryLockResult<RwLockReadGuard<'_, T>>;

        /// The most up to date state of the data.
        fn cur_state(&self) -> &Arc<AtomicUsize>;

        /// The last read state of the data.
        fn read_state(&self) -> &AtomicUsize;
    }
}
