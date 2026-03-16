use mitki_span::Symbol;
use salsa::plumbing::AsId as _;

use crate::classify::{AbiTy, BackendTy};

pub const ARC_HEADER_SIZE: u32 = 8;
pub const ARC_ALIGN: u32 = 8;
pub const ARC_IMMORTAL_REFCNT: i32 = -1;
pub const ARRAY_HEADER_SIZE: u32 = 8;
pub const ARRAY_LEN_OFFSET: u32 = 0;
pub const ARRAY_CAPACITY_OFFSET: u32 = 4;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AggregateLayout {
    pub size: u32,
    pub align: u32,
    pub kind: AggregateKind,
}

impl AggregateLayout {
    pub fn field_named(&self, name_bits: u64) -> Option<&FieldLayout> {
        let AggregateKind::Fields(fields) = &self.kind else {
            return None;
        };
        fields.iter().find(|field| field.name_bits == Some(name_bits))
    }

    pub fn fields(&self) -> Option<&[FieldLayout]> {
        let AggregateKind::Fields(fields) = &self.kind else {
            return None;
        };
        Some(fields)
    }

    pub fn variant(&self, name_bits: u64) -> Option<&VariantLayout> {
        let AggregateKind::Enum(layout) = &self.kind else {
            return None;
        };
        layout.variants.iter().find(|variant| variant.name_bits == name_bits)
    }

    pub fn contains_heap_refs(&self) -> bool {
        match &self.kind {
            AggregateKind::Fields(fields) => fields.iter().any(FieldLayout::contains_heap_refs),
            AggregateKind::Enum(layout) => layout
                .variants
                .iter()
                .any(|variant| variant.fields.iter().any(FieldLayout::contains_heap_refs)),
            AggregateKind::FunctionValue => true,
        }
    }

    pub fn is_function_value(&self) -> bool {
        matches!(self.kind, AggregateKind::FunctionValue)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ArrayRuntimeLayout {
    pub type_bits: u32,
    pub item_abi: AbiTy,
    pub item_size: u32,
    pub item_align: u32,
    pub item_stride: u32,
    pub data_offset: u32,
}

impl ArrayRuntimeLayout {
    pub fn object_align(&self) -> u32 {
        ARC_ALIGN.max(self.item_align)
    }

    pub fn total_size_for_len(&self, len: u32) -> Option<u32> {
        self.item_stride
            .checked_mul(len)?
            .checked_add(ARC_HEADER_SIZE)?
            .checked_add(self.data_offset)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AggregateKind {
    Fields(Vec<FieldLayout>),
    Enum(EnumLayout),
    FunctionValue,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldLayout {
    pub name_bits: Option<u64>,
    pub offset: u32,
    pub ty: AbiTy,
}

impl FieldLayout {
    pub fn contains_heap_refs(&self) -> bool {
        self.ty.contains_heap_refs()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumLayout {
    pub payload_offset: u32,
    pub variants: Vec<VariantLayout>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VariantLayout {
    pub name_bits: u64,
    pub tag: i32,
    pub fields: Vec<FieldLayout>,
}

#[derive(Clone, Debug)]
pub struct FieldsLayout {
    pub fields: Vec<FieldLayout>,
    pub size: u32,
    pub align: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MemoryArg {
    pub offset: u64,
    pub align: u32,
    pub memory_index: u32,
}

pub fn layout_fields(
    fields: impl IntoIterator<Item = (Option<u64>, Option<AbiTy>)>,
) -> Option<FieldsLayout> {
    let mut field_layouts = Vec::new();
    let mut size = 0;
    let mut align = 1;

    for (name_bits, field) in fields {
        let field = field?;
        let layout = abi_layout(&field)?;
        let offset = align_to(size, layout.align);
        size = offset + layout.size;
        align = align.max(layout.align);
        field_layouts.push(FieldLayout { name_bits, offset, ty: field });
    }

    Some(FieldsLayout { fields: field_layouts, size: align_to(size, align), align })
}

pub fn abi_layout(ty: &AbiTy) -> Option<AggregateLayout> {
    match ty {
        AbiTy::Scalar(BackendTy::Int | BackendTy::Bool | BackendTy::Char | BackendTy::Ref(_)) => {
            Some(AggregateLayout { size: 4, align: 4, kind: AggregateKind::Fields(Vec::new()) })
        }
        AbiTy::Scalar(BackendTy::I64) => {
            Some(AggregateLayout { size: 8, align: 8, kind: AggregateKind::Fields(Vec::new()) })
        }
        AbiTy::Scalar(BackendTy::Float) => {
            Some(AggregateLayout { size: 8, align: 8, kind: AggregateKind::Fields(Vec::new()) })
        }
        AbiTy::Scalar(BackendTy::Unit) => {
            Some(AggregateLayout { size: 0, align: 1, kind: AggregateKind::Fields(Vec::new()) })
        }
        AbiTy::Aggregate(layout) => Some((**layout).clone()),
    }
}

pub fn align_to(offset: u32, align: u32) -> u32 {
    if align <= 1 {
        offset
    } else {
        let mask = align - 1;
        (offset + mask) & !mask
    }
}

pub fn memarg(offset: u32, align: u32) -> MemoryArg {
    MemoryArg { offset: offset.into(), align, memory_index: 0 }
}

pub fn symbol_bits(symbol: Symbol<'_>) -> u64 {
    symbol.as_id().as_bits()
}
