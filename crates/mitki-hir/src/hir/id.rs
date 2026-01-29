use std::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, salsa::Update)]
pub struct Id<Tag> {
    raw: u32,
    _tag: PhantomData<Tag>,
}

impl<Tag> Id<Tag> {
    pub const ZERO: Self = Self { raw: 0, _tag: PhantomData };

    pub fn is_zero(&self) -> bool {
        self.raw == 0
    }

    pub(crate) fn new(index: usize) -> Self {
        let raw: u32 = index.try_into().expect("Id overflow");
        Self { raw: raw + 1, _tag: PhantomData }
    }

    #[track_caller]
    pub(crate) fn get(self) -> usize {
        assert!(self.raw != 0, "attempted to access zero id");
        (self.raw - 1) as usize
    }

    pub(crate) fn from_raw(raw: Raw) -> Self {
        Self { raw: raw.0, _tag: PhantomData }
    }

    pub(crate) fn raw(self) -> Raw {
        Raw(self.raw)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, salsa::Update)]
pub(crate) struct Raw(pub(crate) u32);

impl Raw {
    pub(crate) const ZERO: Raw = Raw(0);
}

impl<Tag> From<Id<Tag>> for Raw {
    fn from(id: Id<Tag>) -> Self {
        Raw(id.raw)
    }
}
