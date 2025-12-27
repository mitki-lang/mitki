//! `MaybeDangling` as specified in <https://github.com/rust-lang/rfcs/pull/3336>,
//! polyfilled via `MaybeUninit`.

use std::mem::MaybeUninit;
use std::ops::Deref;

#[repr(transparent)]
pub(crate) struct MaybeDangling<T> {
    value: MaybeUninit<T>,
}

impl<T> MaybeDangling<T> {
    #[inline]
    pub(crate) const fn new(value: T) -> Self {
        Self { value: MaybeUninit::new(value) }
    }

    #[inline]
    pub(crate) fn as_mut_ptr(&mut self) -> *mut T {
        self.value.as_mut_ptr()
    }
}

impl<T> Deref for MaybeDangling<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.value.assume_init_ref() }
    }
}

impl<T> Drop for MaybeDangling<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            self.value.as_mut_ptr().drop_in_place();
        }
    }
}

impl<T> MaybeDangling<Box<[MaybeUninit<T>]>> {
    pub(crate) unsafe fn assume_init(self) -> MaybeDangling<Box<[T]>> {
        // SAFETY: The layouts match, we are `#[repr(transparent)]`, `Box` has the
        // layout of a pointer, and `MaybeUninit` is `#[repr(transparent)]`.
        unsafe { std::mem::transmute::<Self, MaybeDangling<Box<[T]>>>(self) }
    }
}
