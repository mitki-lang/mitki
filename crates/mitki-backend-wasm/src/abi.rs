#[allow(unused_imports)]
pub use mitki_codegen_core::classify::{
    AbiTy, ArrayShape, BackendTy, EnumShape, FieldsShape, FunctionSignature, FunctionValueShape,
    RefKind, UnionShape, ValueShape, ValueShapeFailure, ValueShapeField, ValueShapeKind,
    ValueShapeVariant, WasmBlockType, WasmValType, abi_ty, array_ty_bits, classify_ty,
    exact_int_backend_ty, function_value_abi_ty, function_value_layout, nominal_ty_bits,
    runtime_function_signature, runtime_ty_to_backend_ty, stage_intrinsic_signature,
    supported_value_shape, ty_bits, value_shape,
};
use wasm_encoder::{BlockType, ValType};

pub fn backend_ty_value_type(ty: BackendTy) -> Option<ValType> {
    match ty.value_type() {
        Some(WasmValType::I32) => Some(ValType::I32),
        Some(WasmValType::I64) => Some(ValType::I64),
        Some(WasmValType::F32) => Some(ValType::F32),
        Some(WasmValType::F64) => Some(ValType::F64),
        Some(WasmValType::V128) => Some(ValType::V128),
        Some(WasmValType::Ref) => Some(ValType::EXTERNREF),
        None => None,
    }
}

pub fn backend_ty_block_type(ty: BackendTy) -> BlockType {
    wasm_block_type(ty.block_type())
}

pub fn wasm_block_type(block_type: WasmBlockType) -> BlockType {
    match block_type {
        WasmBlockType::Empty => BlockType::Empty,
        WasmBlockType::Result(value_type) => BlockType::Result(wasm_val_type(value_type)),
    }
}

pub fn wasm_val_type(value_type: WasmValType) -> ValType {
    match value_type {
        WasmValType::I32 => ValType::I32,
        WasmValType::I64 => ValType::I64,
        WasmValType::F32 => ValType::F32,
        WasmValType::F64 => ValType::F64,
        WasmValType::V128 => ValType::V128,
        WasmValType::Ref => ValType::EXTERNREF,
    }
}
