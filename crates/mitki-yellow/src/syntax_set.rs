use crate::SyntaxKind;

const SIZE: usize = 1;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SyntaxSet {
    bits: [u64; SIZE],
}

impl SyntaxSet {
    pub const EMPTY: Self = Self { bits: [0; SIZE] };
    const BITS_PER_SLOT: u16 = u64::BITS as u16;

    const fn from_kind(kind: SyntaxKind) -> Self {
        let kind = kind as u16;

        let slot_index = (kind / Self::BITS_PER_SLOT) as usize;

        debug_assert!(
            slot_index < Self::EMPTY.bits.len(),
            "Index out of bounds. Increase the size of the bitset array."
        );

        let bit_index = kind % Self::BITS_PER_SLOT;
        let mask = 1 << bit_index;

        let mut bits = Self::EMPTY.bits;
        bits[slot_index] = mask;

        Self { bits }
    }

    pub const fn union(mut self, other: &Self) -> Self {
        let mut i = 0;

        while i < self.bits.len() {
            self.bits[i] |= other.bits[i];
            i += 1;
        }

        self
    }

    pub const fn new<const N: usize>(kinds: [SyntaxKind; N]) -> Self {
        let mut set = Self::EMPTY;

        let mut i = 0;
        while i < kinds.len() {
            set = set.union(&Self::from_kind(kinds[i]));
            i += 1;
        }

        set
    }

    pub const fn contains(&self, kind: SyntaxKind) -> bool {
        let kind = kind as u16;
        let slot_index = (kind / Self::BITS_PER_SLOT) as usize;
        let bit_index = kind % Self::BITS_PER_SLOT;
        let mask = 1 << bit_index;

        self.bits[slot_index] & mask != 0
    }
}
