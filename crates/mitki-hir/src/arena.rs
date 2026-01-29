use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct Key<T>(u32, PhantomData<T>);

unsafe impl<T> salsa::Update for Key<T> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        unsafe { salsa::Update::maybe_update(&mut (*old_pointer).0, new_value.0) }
    }
}

impl<T> std::hash::Hash for Key<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> PartialEq for Key<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Key<T> {}

impl<T> Clone for Key<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Key<T> {}

impl<T> Key<T> {
    pub fn new(index: u32) -> Self {
        Self(index, PhantomData)
    }

    pub fn index(self) -> u32 {
        self.0
    }
}

impl<T> From<u32> for Key<T> {
    fn from(index: u32) -> Self {
        Self::new(index)
    }
}

#[derive(Debug, salsa::Update)]
pub struct Range<T> {
    pub start: Key<T>,
    pub end: Key<T>,
}

impl<T> std::hash::Hash for Range<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
    }
}

impl<T> Copy for Range<T> {}

impl<T> PartialEq for Range<T> {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end == other.end
    }
}

impl<T> Eq for Range<T> {}

impl<T> Clone for Range<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Range<T> {
    pub fn new(start: Key<T>, end: Key<T>) -> Self {
        Self { start, end }
    }

    pub fn new_inclusive(start: Key<T>, end: Key<T>) -> Self {
        Self { start, end: Key::new(end.index() + 1) }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Arena<T> {
    items: Vec<T>,
}

impl<T> IntoIterator for Arena<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<T> Extend<T> for Arena<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.items.extend(iter);
    }
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
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn alloc(&mut self, value: T) -> Key<T> {
        let idx = self.items.len() as u32;
        self.items.push(value);
        Key::new(idx)
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (Key<T>, &T)> {
        self.items.iter().enumerate().map(|(i, item)| (Key::new(i as u32), item))
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.items.len()
    }
}

impl<T> Index<Key<T>> for Arena<T> {
    type Output = T;
    fn index(&self, index: Key<T>) -> &Self::Output {
        &self.items[index.index() as usize]
    }
}

impl<T> IndexMut<Key<T>> for Arena<T> {
    fn index_mut(&mut self, index: Key<T>) -> &mut Self::Output {
        &mut self.items[index.index() as usize]
    }
}

impl<T> Index<Range<T>> for Arena<T> {
    type Output = [T];

    fn index(&self, range: Range<T>) -> &Self::Output {
        let start = range.start.index() as usize;
        let end = range.end.index() as usize;
        &self.items[start..end]
    }
}
