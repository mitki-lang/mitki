use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

#[derive(Debug, salsa::Update)]
pub struct Idx<T>(u32, PhantomData<T>);

impl<T> std::hash::Hash for Idx<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Idx<T> {}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Idx<T> {}

impl<T> Idx<T> {
    pub fn new(index: u32) -> Self {
        Idx(index, PhantomData)
    }

    pub fn index(self) -> u32 {
        self.0
    }
}

impl<T> From<u32> for Idx<T> {
    fn from(index: u32) -> Self {
        Idx::new(index)
    }
}

#[derive(Debug, salsa::Update)]
pub(crate) struct IdxRange<T> {
    pub start: Idx<T>,
    pub end: Idx<T>,
}

impl<T> std::hash::Hash for IdxRange<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
    }
}

impl<T> Copy for IdxRange<T> {}

impl<T> PartialEq for IdxRange<T> {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end == other.end
    }
}

impl<T> Eq for IdxRange<T> {}

impl<T> Clone for IdxRange<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> IdxRange<T> {
    pub(crate) fn new(start: Idx<T>, end: Idx<T>) -> Self {
        IdxRange { start, end }
    }

    pub(crate) fn new_inclusive(start: Idx<T>, end: Idx<T>) -> Self {
        IdxRange { start, end: Idx::new(end.index() + 1) }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct Arena<T> {
    items: Vec<T>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self { items: Default::default() }
    }
}

unsafe impl<T: salsa::Update> salsa::Update for Arena<T> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        unsafe { salsa::Update::maybe_update(&mut (*old_pointer).items, new_value.items) }
    }
}

impl<T> Arena<T> {
    pub(crate) fn new() -> Self {
        Arena { items: Vec::new() }
    }

    pub(crate) fn alloc(&mut self, value: T) -> Idx<T> {
        let idx = self.items.len() as u32;
        self.items.push(value);
        Idx::new(idx)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (Idx<T>, &T)> {
        self.items.iter().enumerate().map(|(i, item)| (Idx::new(i as u32), item))
    }

    pub(crate) fn len(&self) -> usize {
        self.items.len()
    }
}

impl<T> Index<Idx<T>> for Arena<T> {
    type Output = T;
    fn index(&self, index: Idx<T>) -> &Self::Output {
        &self.items[index.index() as usize]
    }
}

impl<T> IndexMut<Idx<T>> for Arena<T> {
    fn index_mut(&mut self, index: Idx<T>) -> &mut Self::Output {
        &mut self.items[index.index() as usize]
    }
}

impl<T> Index<IdxRange<T>> for Arena<T> {
    type Output = [T];

    fn index(&self, range: IdxRange<T>) -> &Self::Output {
        let start = range.start.index() as usize;
        let end = range.end.index() as usize;
        &self.items[start..end]
    }
}
