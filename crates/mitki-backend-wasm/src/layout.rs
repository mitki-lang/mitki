#[allow(unused_imports)]
pub use mitki_codegen_core::layout::{
    ARC_ALIGN, ARC_HEADER_SIZE, ARC_IMMORTAL_REFCNT, ARRAY_CAPACITY_OFFSET, ARRAY_HEADER_SIZE,
    ARRAY_LEN_OFFSET, AggregateKind, AggregateLayout, ArrayRuntimeLayout, EnumLayout, FieldLayout,
    FieldsLayout, MemoryArg, VariantLayout, abi_layout, align_to, layout_fields, symbol_bits,
};
use wasm_encoder::MemArg;

pub fn memarg(offset: u32, align: u32) -> MemArg {
    let arg = mitki_codegen_core::layout::memarg(offset, align);
    MemArg { offset: arg.offset, align: arg.align, memory_index: arg.memory_index }
}
