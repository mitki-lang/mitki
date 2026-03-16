use std::collections::BTreeMap;

use mitki_abi::{
    AbiTypeKind as AbiV2TypeKind, CANONICAL_BLOB_ENCODING_VERSION,
    FunctionSignature as AbiV2FunctionSignature, SemanticTypeGraph, TypeId,
};
use mitki_abi_lower::{
    LoweringMode, MitkiAggregateKindAbi, MitkiAggregateLayoutAbi, MitkiArrayLayoutAbi,
    MitkiEnumLayoutAbi, MitkiFunctionAbi, MitkiLoweringAbi, MitkiPointeeAbi, MitkiValueAbi,
    MitkiValueKind,
};
use wasm_encoder::{Function as WasmFunction, Instruction};

use super::super::boundary::{semantic_type_is_immediate, semantic_type_kind, transport_type_id};
use super::super::wrapper_mir::{
    Operand as WrapperOperand, PlaceId, PlaceKind as WrapperPlaceKind, RValue as WrapperRValue,
    Region as WrapperRegion, Stmt as WrapperStmt, ValueId, WrapperCallTarget, WrapperHandleField,
    WrapperKind, WrapperMirFunction, WrapperMirSignature, WrapperPlaceTy, WrapperScratchKind,
};
use super::*;
use crate::backend::emit::backend_ir;
use crate::layout::memarg;
#[derive(Clone, Copy)]
enum BoundaryValueSource {
    Local(u32),
    Memory { base_local: u32, offset: u32 },
}

#[derive(Clone, Copy)]
enum BoundaryValueDest {
    Local(u32),
    Memory { base_local: u32, offset: u32 },
}

const CANONICAL_BLOB_FIXED_HEADER_LEN: u32 = 34;
const CANONICAL_BLOB_ROOT_OFFSET: u32 = CANONICAL_BLOB_FIXED_HEADER_LEN;
const CANONICAL_BLOB_NODE_TABLE_OFFSET: u32 = 39;
const CANONICAL_BLOB_TOTAL_LEN_OFFSET: u32 = 22;
const CANONICAL_BLOB_NODE_COUNT_OFFSET: u32 = 26;
const CANONICAL_BLOB_HANDLE_COUNT_OFFSET: u32 = 30;

const CANONICAL_VALUE_REF_NODE_ID_OFFSET: u32 = 1;
const CANONICAL_NODE_HEADER_LEN: u32 = 21;
const CANONICAL_NODE_KIND_OFFSET: u32 = 4;
const CANONICAL_NODE_AUX0_OFFSET: u32 = 5;
const CANONICAL_NODE_AUX1_OFFSET: u32 = 9;
const CANONICAL_NODE_CHILD_COUNT_OFFSET: u32 = 13;
const CANONICAL_NODE_PAYLOAD_LEN_OFFSET: u32 = 17;
const CANONICAL_NODE_CHILDREN_OFFSET: u32 = 21;
const CANONICAL_ARRAY_VALUES_SENTINEL: u32 = u32::MAX;
const CANONICAL_BLOB_ALIGN: u32 = 4;

const CANONICAL_INLINE_UNIT_SIZE: u32 = 2;
const CANONICAL_INLINE_BOOL_SIZE: u32 = 3;
const CANONICAL_INLINE_INT_SIZE: u32 = 13;
const CANONICAL_INLINE_FLOAT_SIZE: u32 = 12;
const CANONICAL_INLINE_CHAR_SIZE: u32 = 6;
const CANONICAL_INLINE_ENUM_TAG_SIZE: u32 = 10;
const CANONICAL_NODE_REF_SIZE: u32 = 5;
const CANONICAL_HANDLE_TABLE_LEN_SIZE: u32 = 4;
const ABI_V2_HANDLE_KIND_OFFSET: u32 = 4;
const ABI_V2_HANDLE_FIELD0_OFFSET: u32 = 0;
const ABI_V2_HANDLE_FIELD1_OFFSET: u32 = 4;
const ABI_V2_HANDLE_PAYLOAD_SIZE: u32 = 8;
const ABI_V2_HANDLE_TOTAL_SIZE: u32 = ARC_HEADER_SIZE + ABI_V2_HANDLE_PAYLOAD_SIZE;
const ABI_V2_HANDLE_KIND_OPAQUE: i32 = 0;
const ABI_V2_HANDLE_KIND_FUNCTION: i32 = 1;
#[derive(Clone, Copy)]
struct CanonicalWrapperLocals {
    node_offset: u32,
    cursor: u32,
    index: u32,
    len: u32,
    count: u32,
    bytes: u32,
    base_id: u32,
    temp_ptr: u32,
    temp_ptr_aux: u32,
    child_count: u32,
    child_bytes: u32,
    handle_count: u32,
    handle_index: u32,
    handle_cursor: u32,
    f64_temp: u32,
}

fn value_abi_is_recursive_placeholder(abi: &MitkiValueAbi) -> bool {
    matches!(abi.kind, MitkiValueKind::Array | MitkiValueKind::Struct | MitkiValueKind::Enum)
        && abi.pointee.is_none()
}

fn resolved_semantic_value_abi<'a>(
    value_abis: &'a BTreeMap<TypeId, MitkiValueAbi>,
    semantic_type: TypeId,
    fallback: &'a MitkiValueAbi,
) -> &'a MitkiValueAbi {
    if value_abi_is_recursive_placeholder(fallback) {
        value_abis.get(&semantic_type).unwrap_or(fallback)
    } else {
        fallback
    }
}

fn live_intersection_members(
    semantic_graph: &SemanticTypeGraph,
    semantic_type: TypeId,
) -> Result<(TypeId, Vec<TypeId>), Diagnostic> {
    let AbiV2TypeKind::Intersection { carrier, facet_plan, .. } =
        semantic_type_kind(semantic_graph, semantic_type)?
    else {
        return Err(Diagnostic::error(
            "internal error: expected intersection semantic type",
            mitki_errors::TextRange::default(),
        ));
    };
    let members = facet_plan
        .and_then(|plan_id| semantic_graph.facet_plans.get(plan_id.0 as usize))
        .map(|plan| {
            plan.entries
                .iter()
                .filter(|entry| entry.kind != mitki_abi::FacetPlanEntryKind::Erased)
                .map(|entry| entry.member)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    Ok((*carrier, members))
}

fn canonical_semantic_leaf_type(
    semantic_graph: &SemanticTypeGraph,
    semantic_type: TypeId,
) -> Result<TypeId, Diagnostic> {
    if matches!(
        semantic_type_kind(semantic_graph, semantic_type)?,
        AbiV2TypeKind::Intersection { .. }
    ) {
        let (carrier, live_facets) = live_intersection_members(semantic_graph, semantic_type)?;
        if live_facets.is_empty() {
            return canonical_semantic_leaf_type(semantic_graph, carrier);
        }
    }
    Ok(semantic_type)
}

fn canonical_value_uses_handle_slot(
    semantic_graph: &SemanticTypeGraph,
    semantic_type: TypeId,
) -> Result<bool, Diagnostic> {
    Ok(matches!(
        semantic_type_kind(
            semantic_graph,
            canonical_semantic_leaf_type(semantic_graph, semantic_type)?
        )?,
        AbiV2TypeKind::Function { .. } | AbiV2TypeKind::Opaque { .. }
    ))
}

fn internal_value_source(abi: &AbiTy, local: u32) -> BoundaryValueSource {
    match abi {
        AbiTy::Aggregate(_) => BoundaryValueSource::Memory { base_local: local, offset: 0 },
        AbiTy::Scalar(_) => BoundaryValueSource::Local(local),
    }
}

fn inline_scalar_ref_size(kind: &AbiV2TypeKind) -> u32 {
    match kind {
        AbiV2TypeKind::Unit => CANONICAL_INLINE_UNIT_SIZE,
        AbiV2TypeKind::Bool => CANONICAL_INLINE_BOOL_SIZE,
        AbiV2TypeKind::Int { .. } => CANONICAL_INLINE_INT_SIZE,
        AbiV2TypeKind::Float { .. } => CANONICAL_INLINE_FLOAT_SIZE,
        AbiV2TypeKind::Char => CANONICAL_INLINE_CHAR_SIZE,
        AbiV2TypeKind::Enum { .. } => CANONICAL_INLINE_ENUM_TAG_SIZE,
        _ => CANONICAL_NODE_REF_SIZE,
    }
}

fn aggregate_layout_for_mode(
    abi: &MitkiValueAbi,
    mode: LoweringMode,
) -> Result<&MitkiAggregateLayoutAbi, Diagnostic> {
    match abi.lowering(mode) {
        MitkiLoweringAbi::Aggregate(layout) => Ok(layout),
        MitkiLoweringAbi::Pointer => match abi.pointee.as_ref() {
            Some(MitkiPointeeAbi::Aggregate(layout)) => Ok(layout),
            _ => Err(Diagnostic::error(
                "internal error: aggregate-lowered ABI v2 value is missing pointee metadata",
                mitki_errors::TextRange::default(),
            )),
        },
        _ => Err(Diagnostic::error(
            "internal error: expected aggregate ABI v2 lowering",
            mitki_errors::TextRange::default(),
        )),
    }
}

fn enum_layout_for_mode(
    abi: &MitkiValueAbi,
    mode: LoweringMode,
) -> Result<&MitkiEnumLayoutAbi, Diagnostic> {
    let layout = aggregate_layout_for_mode(abi, mode)?;
    match &layout.kind {
        MitkiAggregateKindAbi::Enum(layout) => Ok(layout),
        MitkiAggregateKindAbi::Fields(_) => Err(Diagnostic::error(
            "internal error: expected enum layout during ABI v2 lowering",
            mitki_errors::TextRange::default(),
        )),
    }
}

fn array_layout_from_abi(abi: &MitkiValueAbi) -> Result<&MitkiArrayLayoutAbi, Diagnostic> {
    match abi.pointee.as_ref() {
        Some(MitkiPointeeAbi::Array(layout)) => Ok(layout),
        _ => Err(Diagnostic::error(
            "internal error: array ABI v2 lowering is missing runtime array metadata",
            mitki_errors::TextRange::default(),
        )),
    }
}

fn union_runtime_member_order<'a>(
    abi: &'a MitkiValueAbi,
    semantic_members: &'a [TypeId],
) -> &'a [TypeId] {
    abi.runtime_member_order.as_deref().unwrap_or(semantic_members)
}

fn union_runtime_variant_index(
    abi: &MitkiValueAbi,
    semantic_members: &[TypeId],
    semantic_member_ty: TypeId,
) -> Result<usize, Diagnostic> {
    union_runtime_member_order(abi, semantic_members)
        .iter()
        .position(|&runtime_member_ty| runtime_member_ty == semantic_member_ty)
        .ok_or_else(|| {
            Diagnostic::error(
                format!(
                    "internal error: union member type `{}` is missing from the runtime ABI arm \
                     order",
                    semantic_member_ty.0
                ),
                mitki_errors::TextRange::default(),
            )
        })
}

fn packed_scalar_width(abi: &MitkiValueAbi) -> Option<u32> {
    match abi.kind {
        MitkiValueKind::Bool => Some(1),
        MitkiValueKind::Int | MitkiValueKind::Char => Some(4),
        MitkiValueKind::Float => Some(8),
        _ => None,
    }
}

fn packed_scalar_kind_tag(abi: &MitkiValueAbi) -> Option<u32> {
    match abi.kind {
        MitkiValueKind::Bool => Some(0),
        MitkiValueKind::Int => Some(1),
        MitkiValueKind::Float => Some(4),
        MitkiValueKind::Char => Some(5),
        _ => None,
    }
}

fn emit_enum_variant_index_to_tag(
    function: &mut WasmFunction,
    enum_layout: &MitkiEnumLayoutAbi,
    variant_index_local: u32,
) {
    for (index, variant) in enum_layout.variants.iter().enumerate().rev() {
        function.instruction(&Instruction::LocalGet(variant_index_local));
        function.instruction(&Instruction::I32Const(index as i32));
        function.instruction(&Instruction::I32Eq);
        function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
        function.instruction(&Instruction::I32Const(variant.tag));
        function.instruction(&Instruction::Else);
    }
    function.instruction(&Instruction::I32Const(0));
    for _ in 0..enum_layout.variants.len() {
        function.instruction(&Instruction::End);
    }
}

fn emit_enum_tag_to_variant_index(
    function: &mut WasmFunction,
    enum_layout: &MitkiEnumLayoutAbi,
    tag_local: u32,
) {
    for (index, variant) in enum_layout.variants.iter().enumerate().rev() {
        function.instruction(&Instruction::LocalGet(tag_local));
        function.instruction(&Instruction::I32Const(variant.tag));
        function.instruction(&Instruction::I32Eq);
        function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
        function.instruction(&Instruction::I32Const(index as i32));
        function.instruction(&Instruction::Else);
    }
    function.instruction(&Instruction::I32Const(0));
    for _ in 0..enum_layout.variants.len() {
        function.instruction(&Instruction::End);
    }
}

fn emit_advance_value_ref_cursor(
    function: &mut WasmFunction,
    cursor_local: u32,
    tag_local: u32,
    aux_local: u32,
) {
    emit_local_get_and_load8(function, cursor_local, 0);
    function.instruction(&Instruction::LocalSet(tag_local));
    function.instruction(&Instruction::LocalGet(tag_local));
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(cursor_local));
    function.instruction(&Instruction::I32Const(CANONICAL_NODE_REF_SIZE as i32));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(cursor_local));
    function.instruction(&Instruction::Else);
    emit_local_get_and_load8(function, cursor_local, 1);
    function.instruction(&Instruction::LocalSet(aux_local));
    function.instruction(&Instruction::LocalGet(cursor_local));
    function.instruction(&Instruction::LocalGet(aux_local));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    function.instruction(&Instruction::I32Const(CANONICAL_INLINE_UNIT_SIZE as i32));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(aux_local));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    function.instruction(&Instruction::I32Const(CANONICAL_INLINE_BOOL_SIZE as i32));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(aux_local));
    function.instruction(&Instruction::I32Const(2));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    function.instruction(&Instruction::I32Const(CANONICAL_INLINE_INT_SIZE as i32));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(aux_local));
    function.instruction(&Instruction::I32Const(3));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    function.instruction(&Instruction::I32Const(CANONICAL_INLINE_FLOAT_SIZE as i32));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(aux_local));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    function.instruction(&Instruction::I32Const(CANONICAL_INLINE_CHAR_SIZE as i32));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::I32Const(CANONICAL_INLINE_ENUM_TAG_SIZE as i32));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(cursor_local));
    function.instruction(&Instruction::End);
}

fn emit_advance_node_cursor(
    function: &mut WasmFunction,
    cursor_local: u32,
    locals: CanonicalWrapperLocals,
) {
    function.instruction(&Instruction::LocalGet(cursor_local));
    function.instruction(&Instruction::I32Const(CANONICAL_NODE_CHILDREN_OFFSET as i32));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(locals.temp_ptr));

    emit_local_get_and_load32(function, cursor_local, CANONICAL_NODE_CHILD_COUNT_OFFSET);
    function.instruction(&Instruction::LocalSet(locals.len));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(locals.index));

    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(locals.index));
    function.instruction(&Instruction::LocalGet(locals.len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));
    emit_advance_value_ref_cursor(function, locals.temp_ptr, locals.count, locals.bytes);
    function.instruction(&Instruction::LocalGet(locals.index));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(locals.index));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    emit_local_get_and_load32(function, cursor_local, CANONICAL_NODE_PAYLOAD_LEN_OFFSET);
    function.instruction(&Instruction::LocalSet(locals.bytes));
    function.instruction(&Instruction::LocalGet(locals.temp_ptr));
    function.instruction(&Instruction::LocalGet(locals.bytes));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(cursor_local));
}

fn emit_find_node_offset(
    function: &mut WasmFunction,
    blob_local: u32,
    node_id_local: u32,
    offset_local: u32,
    locals: CanonicalWrapperLocals,
) {
    emit_local_plus_offset(function, blob_local, CANONICAL_BLOB_NODE_TABLE_OFFSET);
    function.instruction(&Instruction::LocalSet(offset_local));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(locals.index));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(locals.index));
    function.instruction(&Instruction::LocalGet(node_id_local));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));
    emit_advance_node_cursor(function, offset_local, locals);
    function.instruction(&Instruction::LocalGet(locals.index));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(locals.index));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
}

fn emit_find_handle_table_offset(
    function: &mut WasmFunction,
    blob_local: u32,
    offset_local: u32,
    locals: CanonicalWrapperLocals,
) {
    emit_local_plus_offset(function, blob_local, CANONICAL_BLOB_NODE_TABLE_OFFSET);
    function.instruction(&Instruction::LocalSet(offset_local));
    emit_local_get_and_load32(function, blob_local, CANONICAL_BLOB_NODE_COUNT_OFFSET);
    function.instruction(&Instruction::LocalSet(locals.len));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(locals.index));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(locals.index));
    function.instruction(&Instruction::LocalGet(locals.len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));
    emit_advance_node_cursor(function, offset_local, locals);
    function.instruction(&Instruction::LocalGet(locals.index));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(locals.index));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
}

fn emit_find_handle_slot_offset(
    function: &mut WasmFunction,
    blob_local: u32,
    slot_id_local: u32,
    offset_local: u32,
    locals: CanonicalWrapperLocals,
) {
    emit_find_handle_table_offset(function, blob_local, offset_local, locals);
    emit_local_plus_offset(function, offset_local, CANONICAL_HANDLE_TABLE_LEN_SIZE);
    function.instruction(&Instruction::LocalSet(offset_local));
    function.instruction(&Instruction::LocalGet(offset_local));
    function.instruction(&Instruction::LocalGet(slot_id_local));
    function.instruction(&Instruction::I32Const(ABI_V2_HANDLE_PAYLOAD_SIZE as i32));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(offset_local));
}

#[allow(clippy::too_many_arguments)]
fn emit_decode_immediate_value_ref(
    function: &mut WasmFunction,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    mode: LoweringMode,
    ref_local: u32,
    dest: BoundaryValueDest,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    match semantic_type_kind(semantic_graph, semantic_type)? {
        AbiV2TypeKind::Unit => Ok(()),
        AbiV2TypeKind::Bool => {
            emit_local_get_and_load8(function, ref_local, 2);
            emit_store_i32_to_dest(function, dest, locals.count);
            Ok(())
        }
        AbiV2TypeKind::Int { .. } => {
            emit_local_get_and_load32(function, ref_local, 5);
            emit_store_i32_to_dest(function, dest, locals.count);
            Ok(())
        }
        AbiV2TypeKind::Float { .. } => {
            emit_local_get_and_load_f64(function, ref_local, 4);
            emit_store_f64_to_dest(function, dest, locals.f64_temp);
            Ok(())
        }
        AbiV2TypeKind::Char => {
            emit_local_get_and_load32(function, ref_local, 2);
            emit_store_i32_to_dest(function, dest, locals.count);
            Ok(())
        }
        AbiV2TypeKind::Enum { .. } => {
            let enum_layout = enum_layout_for_mode(abi, mode)?;
            emit_local_get_and_load32(function, ref_local, 6);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            match abi.lowering(mode) {
                MitkiLoweringAbi::Aggregate(layout) => {
                    let _ = layout;
                    emit_enum_variant_index_to_tag(function, enum_layout, locals.temp_ptr);
                    emit_store_i32_to_dest(function, dest, locals.count);
                    Ok(())
                }
                MitkiLoweringAbi::Pointer => {
                    let layout = aggregate_layout_for_mode(abi, mode)?;
                    emit_alloc_nominal_payload_to_local(
                        function,
                        runtime_indices,
                        layout,
                        locals.temp_ptr_aux,
                        locals.base_id,
                    )?;
                    emit_enum_variant_index_to_tag(function, enum_layout, locals.temp_ptr);
                    emit_store32_at_local(function, locals.base_id, 0, locals.count);
                    function.instruction(&Instruction::LocalGet(locals.base_id));
                    emit_store_i32_to_dest(function, dest, locals.count);
                    Ok(())
                }
                MitkiLoweringAbi::Unit | MitkiLoweringAbi::I32 | MitkiLoweringAbi::F64 => {
                    Err(Diagnostic::error(
                        "internal error: enum immediate canonical lowering expected aggregate or \
                         pointer destination",
                        mitki_errors::TextRange::default(),
                    ))
                }
            }
        }
        other => Err(Diagnostic::error(
            format!("internal error: unexpected immediate ABI v2 semantic type `{other:?}`"),
            mitki_errors::TextRange::default(),
        )),
    }
}

fn emit_decode_string_node(
    function: &mut WasmFunction,
    node_offset_local: u32,
    dest: BoundaryValueDest,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    emit_local_get_and_load32(function, node_offset_local, CANONICAL_NODE_PAYLOAD_LEN_OFFSET);
    function.instruction(&Instruction::LocalSet(locals.len));

    function.instruction(&Instruction::LocalGet(locals.len));
    function.instruction(&Instruction::I32Const((ARC_HEADER_SIZE + 4) as i32));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(locals.bytes));
    emit_dynamic_alloc_to_local(
        function,
        runtime_indices,
        locals.bytes,
        ARC_ALIGN,
        locals.temp_ptr,
    )?;
    emit_local_plus_offset(function, locals.temp_ptr, 0);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalGet(locals.bytes));
    function.instruction(&Instruction::MemoryFill(0));
    emit_write_arc_header(function, locals.temp_ptr);

    emit_local_plus_offset(function, locals.temp_ptr, ARC_HEADER_SIZE);
    function.instruction(&Instruction::LocalSet(locals.base_id));
    emit_local_plus_offset(function, locals.base_id, 0);
    function.instruction(&Instruction::LocalGet(locals.len));
    function.instruction(&Instruction::I32Store(memarg(0, 2)));

    emit_local_plus_offset(function, locals.base_id, 4);
    emit_local_plus_offset(function, node_offset_local, CANONICAL_NODE_CHILDREN_OFFSET);
    function.instruction(&Instruction::LocalGet(locals.len));
    function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });

    function.instruction(&Instruction::LocalGet(locals.base_id));
    emit_store_i32_to_dest(function, dest, locals.count);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_decode_canonical_value_ref(
    function: &mut WasmFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    value_abis: &BTreeMap<TypeId, MitkiValueAbi>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    mode: LoweringMode,
    blob_local: u32,
    ref_local: u32,
    dest: BoundaryValueDest,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    let abi = resolved_semantic_value_abi(value_abis, semantic_type, abi);
    if semantic_type_is_immediate(semantic_graph, semantic_type)? {
        return emit_decode_immediate_value_ref(
            function,
            abi,
            semantic_type,
            semantic_graph,
            mode,
            ref_local,
            dest,
            runtime_indices,
            locals,
        );
    }

    if matches!(
        semantic_type_kind(semantic_graph, semantic_type)?,
        AbiV2TypeKind::Intersection { .. }
    ) {
        let (carrier, live_facets) = live_intersection_members(semantic_graph, semantic_type)?;
        if live_facets.is_empty() {
            return emit_decode_canonical_value_ref(
                function,
                helper_indices,
                value_abis,
                resolved_semantic_value_abi(value_abis, carrier, abi),
                carrier,
                semantic_graph,
                mode,
                blob_local,
                ref_local,
                dest,
                runtime_indices,
                locals,
            );
        }
    }

    if matches!(
        semantic_type_kind(semantic_graph, semantic_type)?,
        AbiV2TypeKind::Function { .. } | AbiV2TypeKind::Opaque { .. }
    ) {
        emit_local_get_and_load32(function, ref_local, CANONICAL_VALUE_REF_NODE_ID_OFFSET);
        function.instruction(&Instruction::LocalSet(locals.base_id));
        emit_find_handle_slot_offset(
            function,
            blob_local,
            locals.base_id,
            locals.node_offset,
            locals,
        );
        emit_local_get_and_load32(function, locals.node_offset, 4);
        function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
        return emit_unwrap_boundary_handle_slot_value(
            function,
            helper_indices,
            abi,
            locals.temp_ptr_aux,
            dest,
            locals,
        );
    }

    emit_local_get_and_load32(function, ref_local, CANONICAL_VALUE_REF_NODE_ID_OFFSET);
    function.instruction(&Instruction::LocalSet(locals.base_id));
    emit_find_node_offset(function, blob_local, locals.base_id, locals.node_offset, locals);
    emit_decode_canonical_node(
        function,
        helper_indices,
        value_abis,
        abi,
        semantic_type,
        semantic_graph,
        mode,
        blob_local,
        locals.node_offset,
        dest,
        runtime_indices,
        locals,
    )
}

#[allow(clippy::too_many_arguments)]
fn emit_decode_canonical_node(
    function: &mut WasmFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    value_abis: &BTreeMap<TypeId, MitkiValueAbi>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    mode: LoweringMode,
    blob_local: u32,
    node_offset_local: u32,
    dest: BoundaryValueDest,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    let abi = resolved_semantic_value_abi(value_abis, semantic_type, abi);
    match semantic_type_kind(semantic_graph, semantic_type)? {
        AbiV2TypeKind::String => {
            emit_decode_string_node(function, node_offset_local, dest, runtime_indices, locals)
        }
        AbiV2TypeKind::Array { elem } => {
            let layout = array_layout_from_abi(abi)?;
            let packed = semantic_type_is_immediate(semantic_graph, *elem)?
                && packed_scalar_width(&layout.item).is_some()
                && packed_scalar_kind_tag(&layout.item).is_some();
            emit_local_get_and_load32(function, node_offset_local, CANONICAL_NODE_AUX0_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.len));
            emit_alloc_runtime_array_to_local(
                function,
                runtime_indices,
                layout,
                locals.len,
                locals.temp_ptr,
                locals.temp_ptr_aux,
            )?;
            emit_local_get_and_load32(function, node_offset_local, CANONICAL_NODE_AUX1_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.count));
            emit_local_plus_offset(function, node_offset_local, CANONICAL_NODE_CHILDREN_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.cursor));
            if packed {
                function.instruction(&Instruction::LocalGet(locals.count));
                function
                    .instruction(&Instruction::I32Const(CANONICAL_ARRAY_VALUES_SENTINEL as i32));
                function.instruction(&Instruction::I32Eq);
                function.instruction(&Instruction::If(BlockType::Empty));
            }
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::Block(BlockType::Empty));
            function.instruction(&Instruction::Loop(BlockType::Empty));
            function.instruction(&Instruction::LocalGet(locals.index));
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::I32GeU);
            function.instruction(&Instruction::BrIf(1));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            emit_runtime_array_element_addr(
                function,
                locals.temp_ptr_aux,
                layout,
                locals.index,
                locals.base_id,
            );
            emit_decode_canonical_value_ref(
                function,
                helper_indices,
                value_abis,
                &layout.item,
                *elem,
                semantic_graph,
                LoweringMode::Runtime,
                blob_local,
                locals.cursor,
                BoundaryValueDest::Memory { base_local: locals.base_id, offset: 0 },
                runtime_indices,
                locals,
            )?;
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            emit_advance_value_ref_cursor(function, locals.cursor, locals.count, locals.bytes);
            function.instruction(&Instruction::LocalGet(locals.index));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::Br(0));
            function.instruction(&Instruction::End);
            function.instruction(&Instruction::End);
            if packed {
                function.instruction(&Instruction::Else);
                emit_local_plus_offset(function, node_offset_local, CANONICAL_NODE_CHILDREN_OFFSET);
                function.instruction(&Instruction::LocalSet(locals.cursor));
                match layout.item.kind {
                    MitkiValueKind::Bool => {
                        function.instruction(&Instruction::I32Const(0));
                        function.instruction(&Instruction::LocalSet(locals.index));
                        function.instruction(&Instruction::Block(BlockType::Empty));
                        function.instruction(&Instruction::Loop(BlockType::Empty));
                        function.instruction(&Instruction::LocalGet(locals.index));
                        function.instruction(&Instruction::LocalGet(locals.len));
                        function.instruction(&Instruction::I32GeU);
                        function.instruction(&Instruction::BrIf(1));
                        emit_runtime_array_element_addr(
                            function,
                            locals.temp_ptr_aux,
                            layout,
                            locals.index,
                            locals.base_id,
                        );
                        emit_local_get_and_load8(function, locals.cursor, 0);
                        emit_store32_at_local(function, locals.base_id, 0, locals.count);
                        emit_local_plus_offset(function, locals.cursor, 1);
                        function.instruction(&Instruction::LocalSet(locals.cursor));
                        function.instruction(&Instruction::LocalGet(locals.index));
                        function.instruction(&Instruction::I32Const(1));
                        function.instruction(&Instruction::I32Add);
                        function.instruction(&Instruction::LocalSet(locals.index));
                        function.instruction(&Instruction::Br(0));
                        function.instruction(&Instruction::End);
                        function.instruction(&Instruction::End);
                    }
                    MitkiValueKind::Int | MitkiValueKind::Char => {
                        emit_local_plus_offset(function, locals.temp_ptr_aux, layout.data_offset);
                        function.instruction(&Instruction::LocalGet(locals.cursor));
                        function.instruction(&Instruction::LocalGet(locals.len));
                        function.instruction(&Instruction::I32Const(4));
                        function.instruction(&Instruction::I32Mul);
                        function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
                    }
                    MitkiValueKind::Float => {
                        emit_local_plus_offset(function, locals.temp_ptr_aux, layout.data_offset);
                        function.instruction(&Instruction::LocalGet(locals.cursor));
                        function.instruction(&Instruction::LocalGet(locals.len));
                        function.instruction(&Instruction::I32Const(8));
                        function.instruction(&Instruction::I32Mul);
                        function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
                    }
                    _ => unreachable!("packed arrays only support scalar element kinds"),
                }
                function.instruction(&Instruction::End);
            }
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            emit_store_i32_to_dest(function, dest, locals.count);
            Ok(())
        }
        AbiV2TypeKind::Tuple { elems } => {
            let base_local = emit_materialize_decode_dest_base(
                function,
                abi,
                mode,
                dest,
                runtime_indices,
                locals,
            )?;
            let layout = aggregate_layout_for_mode(abi, mode)?;
            let MitkiAggregateKindAbi::Fields(fields) = &layout.kind else {
                return Err(Diagnostic::error(
                    "internal error: tuple ABI v2 lowering expected field layout",
                    mitki_errors::TextRange::default(),
                ));
            };
            emit_local_plus_offset(function, node_offset_local, CANONICAL_NODE_CHILDREN_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.cursor));
            for (field, &field_ty) in fields.iter().zip(elems.iter()) {
                function.instruction(&Instruction::LocalGet(base_local));
                emit_decode_canonical_value_ref(
                    function,
                    helper_indices,
                    value_abis,
                    &field.value,
                    field_ty,
                    semantic_graph,
                    mode,
                    blob_local,
                    locals.cursor,
                    BoundaryValueDest::Memory { base_local, offset: field.offset },
                    runtime_indices,
                    locals,
                )?;
                function.instruction(&Instruction::LocalSet(base_local));
                emit_advance_value_ref_cursor(function, locals.cursor, locals.count, locals.bytes);
            }
            if matches!(abi.lowering(mode), MitkiLoweringAbi::Pointer) {
                function.instruction(&Instruction::LocalGet(base_local));
                emit_store_i32_to_dest(function, dest, locals.count);
            }
            Ok(())
        }
        AbiV2TypeKind::Record { fields: semantic_fields }
        | AbiV2TypeKind::Struct { fields: semantic_fields, .. } => {
            let base_local = emit_materialize_decode_dest_base(
                function,
                abi,
                mode,
                dest,
                runtime_indices,
                locals,
            )?;
            let layout = aggregate_layout_for_mode(abi, mode)?;
            let MitkiAggregateKindAbi::Fields(fields) = &layout.kind else {
                return Err(Diagnostic::error(
                    "internal error: aggregate ABI v2 lowering expected field layout",
                    mitki_errors::TextRange::default(),
                ));
            };
            emit_local_plus_offset(function, node_offset_local, CANONICAL_NODE_CHILDREN_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.cursor));
            for (field, semantic_field) in fields.iter().zip(semantic_fields.iter()) {
                function.instruction(&Instruction::LocalGet(base_local));
                emit_decode_canonical_value_ref(
                    function,
                    helper_indices,
                    value_abis,
                    &field.value,
                    semantic_field.ty,
                    semantic_graph,
                    mode,
                    blob_local,
                    locals.cursor,
                    BoundaryValueDest::Memory { base_local, offset: field.offset },
                    runtime_indices,
                    locals,
                )?;
                function.instruction(&Instruction::LocalSet(base_local));
                emit_advance_value_ref_cursor(function, locals.cursor, locals.count, locals.bytes);
            }
            if matches!(abi.lowering(mode), MitkiLoweringAbi::Pointer) {
                function.instruction(&Instruction::LocalGet(base_local));
                emit_store_i32_to_dest(function, dest, locals.count);
            }
            Ok(())
        }
        AbiV2TypeKind::Enum { variants, .. } => {
            let base_local = emit_materialize_decode_dest_base(
                function,
                abi,
                mode,
                dest,
                runtime_indices,
                locals,
            )?;
            let enum_layout = enum_layout_for_mode(abi, mode)?;
            emit_local_get_and_load32(function, node_offset_local, CANONICAL_NODE_AUX0_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.len));
            emit_enum_variant_index_to_tag(function, enum_layout, locals.len);
            emit_store32_at_local(function, base_local, 0, locals.count);
            emit_local_plus_offset(function, node_offset_local, CANONICAL_NODE_CHILDREN_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.cursor));
            for (index, variant) in variants.iter().enumerate().rev() {
                function.instruction(&Instruction::LocalGet(locals.len));
                function.instruction(&Instruction::I32Const(index as i32));
                function.instruction(&Instruction::I32Eq);
                function.instruction(&Instruction::If(BlockType::Empty));
                let layout_variant = &enum_layout.variants[index];
                for (field, &field_ty) in layout_variant.fields.iter().zip(variant.fields.iter()) {
                    function.instruction(&Instruction::LocalGet(base_local));
                    emit_decode_canonical_value_ref(
                        function,
                        helper_indices,
                        value_abis,
                        &field.value,
                        field_ty,
                        semantic_graph,
                        mode,
                        blob_local,
                        locals.cursor,
                        BoundaryValueDest::Memory { base_local, offset: field.offset },
                        runtime_indices,
                        locals,
                    )?;
                    function.instruction(&Instruction::LocalSet(base_local));
                    emit_advance_value_ref_cursor(
                        function,
                        locals.cursor,
                        locals.count,
                        locals.bytes,
                    );
                }
                function.instruction(&Instruction::End);
            }
            if matches!(abi.lowering(mode), MitkiLoweringAbi::Pointer) {
                function.instruction(&Instruction::LocalGet(base_local));
                emit_store_i32_to_dest(function, dest, locals.count);
            }
            Ok(())
        }
        AbiV2TypeKind::Union { members } => {
            let base_local = emit_materialize_decode_dest_base(
                function,
                abi,
                mode,
                dest,
                runtime_indices,
                locals,
            )?;
            let union_layout = enum_layout_for_mode(abi, mode)?;
            emit_local_get_and_load32(function, node_offset_local, CANONICAL_NODE_AUX0_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.len));
            emit_local_plus_offset(function, node_offset_local, CANONICAL_NODE_CHILDREN_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.cursor));
            for (arm_index, &member_ty) in members.iter().enumerate().rev() {
                let runtime_variant_index = union_runtime_variant_index(abi, members, member_ty)?;
                let layout_variant = &union_layout.variants[runtime_variant_index];
                function.instruction(&Instruction::LocalGet(locals.len));
                function.instruction(&Instruction::I32Const(arm_index as i32));
                function.instruction(&Instruction::I32Eq);
                function.instruction(&Instruction::If(BlockType::Empty));
                function.instruction(&Instruction::I32Const(layout_variant.tag));
                emit_store32_at_local(function, base_local, 0, locals.count);
                let field = layout_variant.fields.first().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: union ABI v2 lowering expected a payload field",
                        mitki_errors::TextRange::default(),
                    )
                })?;
                emit_decode_canonical_value_ref(
                    function,
                    helper_indices,
                    value_abis,
                    &field.value,
                    member_ty,
                    semantic_graph,
                    mode,
                    blob_local,
                    locals.cursor,
                    BoundaryValueDest::Memory { base_local, offset: field.offset },
                    runtime_indices,
                    locals,
                )?;
                function.instruction(&Instruction::End);
            }
            if matches!(abi.lowering(mode), MitkiLoweringAbi::Pointer) {
                function.instruction(&Instruction::LocalGet(base_local));
                emit_store_i32_to_dest(function, dest, locals.count);
            }
            Ok(())
        }
        AbiV2TypeKind::Unit
        | AbiV2TypeKind::Bool
        | AbiV2TypeKind::Int { .. }
        | AbiV2TypeKind::Float { .. }
        | AbiV2TypeKind::Char
        | AbiV2TypeKind::Function { .. }
        | AbiV2TypeKind::Opaque { .. }
        | AbiV2TypeKind::Intersection { .. } => Err(Diagnostic::error(
            "ABI v2 baseline guest lowering does not support this canonical node kind yet",
            mitki_errors::TextRange::default(),
        )),
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_measure_canonical_value_ref(
    function: &mut WasmFunction,
    value_abis: &BTreeMap<TypeId, MitkiValueAbi>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    mode: LoweringMode,
    source: BoundaryValueSource,
    out_count_local: u32,
    out_bytes_local: u32,
    out_handle_count_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    let abi = resolved_semantic_value_abi(value_abis, semantic_type, abi);
    let kind = semantic_type_kind(semantic_graph, semantic_type)?;
    if semantic_type_is_immediate(semantic_graph, semantic_type)? {
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalSet(out_count_local));
        function.instruction(&Instruction::I32Const(inline_scalar_ref_size(kind) as i32));
        function.instruction(&Instruction::LocalSet(out_bytes_local));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalSet(out_handle_count_local));
        return Ok(());
    }

    if matches!(kind, AbiV2TypeKind::Intersection { .. }) {
        let (carrier, live_facets) = live_intersection_members(semantic_graph, semantic_type)?;
        if live_facets.is_empty() {
            return emit_measure_canonical_value_ref(
                function,
                value_abis,
                resolved_semantic_value_abi(value_abis, carrier, abi),
                carrier,
                semantic_graph,
                mode,
                source,
                out_count_local,
                out_bytes_local,
                out_handle_count_local,
                locals,
            );
        }
    }

    if matches!(kind, AbiV2TypeKind::Function { .. } | AbiV2TypeKind::Opaque { .. }) {
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalSet(out_count_local));
        function.instruction(&Instruction::I32Const(CANONICAL_NODE_REF_SIZE as i32));
        function.instruction(&Instruction::LocalSet(out_bytes_local));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::LocalSet(out_handle_count_local));
        return Ok(());
    }

    match kind {
        AbiV2TypeKind::String => {
            emit_load_pointer_from_source(function, source, locals.temp_ptr);
            emit_local_get_and_load32(function, locals.temp_ptr, 0);
            function.instruction(&Instruction::LocalSet(locals.len));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::LocalSet(out_count_local));
            function.instruction(&Instruction::I32Const(
                (CANONICAL_NODE_REF_SIZE + CANONICAL_NODE_HEADER_LEN) as i32,
            ));
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(out_bytes_local));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(out_handle_count_local));
            Ok(())
        }
        AbiV2TypeKind::Array { elem } => {
            let layout = array_layout_from_abi(abi)?;
            emit_load_pointer_from_source(function, source, locals.temp_ptr);
            emit_local_get_and_load32(function, locals.temp_ptr, ARRAY_LEN_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.len));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(out_handle_count_local));
            if semantic_type_is_immediate(semantic_graph, *elem)?
                && packed_scalar_width(&layout.item).is_some()
                && packed_scalar_kind_tag(&layout.item).is_some()
            {
                function.instruction(&Instruction::I32Const(1));
                function.instruction(&Instruction::LocalSet(out_count_local));
                function.instruction(&Instruction::I32Const(
                    (CANONICAL_NODE_REF_SIZE + CANONICAL_NODE_HEADER_LEN) as i32,
                ));
                function.instruction(&Instruction::LocalGet(locals.len));
                function.instruction(&Instruction::I32Const(
                    packed_scalar_width(&layout.item).expect("packed width") as i32,
                ));
                function.instruction(&Instruction::I32Mul);
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_bytes_local));
                return Ok(());
            }

            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::LocalSet(out_count_local));
            function.instruction(&Instruction::I32Const(
                (CANONICAL_NODE_REF_SIZE + CANONICAL_NODE_HEADER_LEN) as i32,
            ));
            function.instruction(&Instruction::LocalSet(out_bytes_local));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::Block(BlockType::Empty));
            function.instruction(&Instruction::Loop(BlockType::Empty));
            function.instruction(&Instruction::LocalGet(locals.index));
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::I32GeU);
            function.instruction(&Instruction::BrIf(1));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::LocalGet(locals.index));
            function.instruction(&Instruction::LocalGet(out_count_local));
            function.instruction(&Instruction::LocalGet(out_bytes_local));
            emit_runtime_array_element_addr(
                function,
                locals.temp_ptr,
                layout,
                locals.index,
                locals.base_id,
            );
            emit_measure_canonical_value_ref(
                function,
                value_abis,
                &layout.item,
                *elem,
                semantic_graph,
                LoweringMode::Runtime,
                BoundaryValueSource::Memory { base_local: locals.base_id, offset: 0 },
                locals.child_count,
                locals.child_bytes,
                locals.handle_index,
                locals,
            )?;
            function.instruction(&Instruction::LocalGet(locals.child_bytes));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(out_bytes_local));
            function.instruction(&Instruction::LocalGet(locals.child_count));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(out_count_local));
            function.instruction(&Instruction::LocalGet(out_handle_count_local));
            function.instruction(&Instruction::LocalGet(locals.handle_index));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(out_handle_count_local));
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::LocalSet(locals.len));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.index));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::Br(0));
            function.instruction(&Instruction::End);
            function.instruction(&Instruction::End);
            Ok(())
        }
        AbiV2TypeKind::Tuple { elems } => {
            emit_materialize_value_source_base(function, abi, mode, source, locals.temp_ptr)?;
            let layout = aggregate_layout_for_mode(abi, mode)?;
            let MitkiAggregateKindAbi::Fields(fields) = &layout.kind else {
                return Err(Diagnostic::error(
                    "internal error: tuple ABI v2 measurement expected fields layout",
                    mitki_errors::TextRange::default(),
                ));
            };
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::LocalSet(out_count_local));
            function.instruction(&Instruction::I32Const(
                (CANONICAL_NODE_REF_SIZE + CANONICAL_NODE_HEADER_LEN) as i32,
            ));
            function.instruction(&Instruction::LocalSet(out_bytes_local));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(out_handle_count_local));
            for (field, &field_ty) in fields.iter().zip(elems.iter()) {
                function.instruction(&Instruction::LocalGet(locals.temp_ptr));
                function.instruction(&Instruction::LocalGet(out_count_local));
                function.instruction(&Instruction::LocalGet(out_bytes_local));
                emit_measure_canonical_value_ref(
                    function,
                    value_abis,
                    &field.value,
                    field_ty,
                    semantic_graph,
                    mode,
                    BoundaryValueSource::Memory {
                        base_local: locals.temp_ptr,
                        offset: field.offset,
                    },
                    locals.child_count,
                    locals.child_bytes,
                    locals.handle_index,
                    locals,
                )?;
                function.instruction(&Instruction::LocalGet(locals.child_bytes));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_bytes_local));
                function.instruction(&Instruction::LocalGet(locals.child_count));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_count_local));
                function.instruction(&Instruction::LocalGet(out_handle_count_local));
                function.instruction(&Instruction::LocalGet(locals.handle_index));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_handle_count_local));
                function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            }
            Ok(())
        }
        AbiV2TypeKind::Record { fields: semantic_fields }
        | AbiV2TypeKind::Struct { fields: semantic_fields, .. } => {
            emit_materialize_value_source_base(function, abi, mode, source, locals.temp_ptr)?;
            let layout = aggregate_layout_for_mode(abi, mode)?;
            let MitkiAggregateKindAbi::Fields(fields) = &layout.kind else {
                return Err(Diagnostic::error(
                    "internal error: aggregate ABI v2 measurement expected fields layout",
                    mitki_errors::TextRange::default(),
                ));
            };
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::LocalSet(out_count_local));
            function.instruction(&Instruction::I32Const(
                (CANONICAL_NODE_REF_SIZE + CANONICAL_NODE_HEADER_LEN) as i32,
            ));
            function.instruction(&Instruction::LocalSet(out_bytes_local));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(out_handle_count_local));
            for (field, semantic_field) in fields.iter().zip(semantic_fields.iter()) {
                function.instruction(&Instruction::LocalGet(locals.temp_ptr));
                function.instruction(&Instruction::LocalGet(out_count_local));
                function.instruction(&Instruction::LocalGet(out_bytes_local));
                emit_measure_canonical_value_ref(
                    function,
                    value_abis,
                    &field.value,
                    semantic_field.ty,
                    semantic_graph,
                    mode,
                    BoundaryValueSource::Memory {
                        base_local: locals.temp_ptr,
                        offset: field.offset,
                    },
                    locals.child_count,
                    locals.child_bytes,
                    locals.handle_index,
                    locals,
                )?;
                function.instruction(&Instruction::LocalGet(locals.child_bytes));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_bytes_local));
                function.instruction(&Instruction::LocalGet(locals.child_count));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_count_local));
                function.instruction(&Instruction::LocalGet(out_handle_count_local));
                function.instruction(&Instruction::LocalGet(locals.handle_index));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_handle_count_local));
                function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            }
            Ok(())
        }
        AbiV2TypeKind::Enum { variants, .. } => {
            emit_materialize_value_source_base(function, abi, mode, source, locals.temp_ptr)?;
            let enum_layout = enum_layout_for_mode(abi, mode)?;
            emit_local_get_and_load32(function, locals.temp_ptr, 0);
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::LocalSet(out_count_local));
            function.instruction(&Instruction::I32Const(
                (CANONICAL_NODE_REF_SIZE + CANONICAL_NODE_HEADER_LEN) as i32,
            ));
            function.instruction(&Instruction::LocalSet(out_bytes_local));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(out_handle_count_local));
            for (variant_index, variant) in variants.iter().enumerate().rev() {
                let layout_variant = &enum_layout.variants[variant_index];
                function.instruction(&Instruction::LocalGet(locals.index));
                function.instruction(&Instruction::I32Const(layout_variant.tag));
                function.instruction(&Instruction::I32Eq);
                function.instruction(&Instruction::If(BlockType::Empty));
                for (field, &field_ty) in layout_variant.fields.iter().zip(variant.fields.iter()) {
                    function.instruction(&Instruction::LocalGet(locals.temp_ptr));
                    function.instruction(&Instruction::LocalGet(out_count_local));
                    function.instruction(&Instruction::LocalGet(out_bytes_local));
                    emit_measure_canonical_value_ref(
                        function,
                        value_abis,
                        &field.value,
                        field_ty,
                        semantic_graph,
                        mode,
                        BoundaryValueSource::Memory {
                            base_local: locals.temp_ptr,
                            offset: field.offset,
                        },
                        locals.child_count,
                        locals.child_bytes,
                        locals.handle_index,
                        locals,
                    )?;
                    function.instruction(&Instruction::LocalGet(locals.child_bytes));
                    function.instruction(&Instruction::I32Add);
                    function.instruction(&Instruction::LocalSet(out_bytes_local));
                    function.instruction(&Instruction::LocalGet(locals.child_count));
                    function.instruction(&Instruction::I32Add);
                    function.instruction(&Instruction::LocalSet(out_count_local));
                    function.instruction(&Instruction::LocalGet(out_handle_count_local));
                    function.instruction(&Instruction::LocalGet(locals.handle_index));
                    function.instruction(&Instruction::I32Add);
                    function.instruction(&Instruction::LocalSet(out_handle_count_local));
                    function.instruction(&Instruction::LocalSet(locals.temp_ptr));
                }
                function.instruction(&Instruction::End);
            }
            Ok(())
        }
        AbiV2TypeKind::Union { members } => {
            emit_materialize_value_source_base(function, abi, mode, source, locals.temp_ptr)?;
            let union_layout = enum_layout_for_mode(abi, mode)?;
            emit_local_get_and_load32(function, locals.temp_ptr, 0);
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::LocalSet(out_count_local));
            function.instruction(&Instruction::I32Const(
                (CANONICAL_NODE_REF_SIZE + CANONICAL_NODE_HEADER_LEN) as i32,
            ));
            function.instruction(&Instruction::LocalSet(out_bytes_local));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(out_handle_count_local));
            for &member_ty in members.iter().rev() {
                let runtime_variant_index = union_runtime_variant_index(abi, members, member_ty)?;
                let layout_variant = &union_layout.variants[runtime_variant_index];
                let field = layout_variant.fields.first().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: union ABI v2 measurement expected a payload field",
                        mitki_errors::TextRange::default(),
                    )
                })?;
                function.instruction(&Instruction::LocalGet(locals.index));
                function.instruction(&Instruction::I32Const(layout_variant.tag));
                function.instruction(&Instruction::I32Eq);
                function.instruction(&Instruction::If(BlockType::Empty));
                function.instruction(&Instruction::LocalGet(locals.temp_ptr));
                function.instruction(&Instruction::LocalGet(out_count_local));
                function.instruction(&Instruction::LocalGet(out_bytes_local));
                emit_measure_canonical_value_ref(
                    function,
                    value_abis,
                    &field.value,
                    member_ty,
                    semantic_graph,
                    mode,
                    BoundaryValueSource::Memory {
                        base_local: locals.temp_ptr,
                        offset: field.offset,
                    },
                    locals.child_count,
                    locals.child_bytes,
                    locals.handle_index,
                    locals,
                )?;
                function.instruction(&Instruction::LocalGet(locals.child_bytes));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_bytes_local));
                function.instruction(&Instruction::LocalGet(locals.child_count));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_count_local));
                function.instruction(&Instruction::LocalGet(out_handle_count_local));
                function.instruction(&Instruction::LocalGet(locals.handle_index));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(out_handle_count_local));
                function.instruction(&Instruction::LocalSet(locals.temp_ptr));
                function.instruction(&Instruction::End);
            }
            Ok(())
        }
        AbiV2TypeKind::Unit
        | AbiV2TypeKind::Bool
        | AbiV2TypeKind::Int { .. }
        | AbiV2TypeKind::Float { .. }
        | AbiV2TypeKind::Char
        | AbiV2TypeKind::Function { .. }
        | AbiV2TypeKind::Opaque { .. }
        | AbiV2TypeKind::Intersection { .. } => Err(Diagnostic::error(
            "ABI v2 baseline guest lowering does not support this canonical measurement kind yet",
            mitki_errors::TextRange::default(),
        )),
    }
}

fn emit_write_node_ref_at_cursor(
    function: &mut WasmFunction,
    cursor_local: u32,
    node_id_local: u32,
) {
    emit_local_plus_offset(function, cursor_local, 0);
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Store8(memarg(0, 0)));
    emit_local_plus_offset(function, cursor_local, CANONICAL_VALUE_REF_NODE_ID_OFFSET);
    function.instruction(&Instruction::LocalGet(node_id_local));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, cursor_local, CANONICAL_NODE_REF_SIZE);
    function.instruction(&Instruction::LocalSet(cursor_local));
}

fn emit_write_handle_ref_at_cursor(
    function: &mut WasmFunction,
    cursor_local: u32,
    handle_slot_local: u32,
) {
    emit_local_plus_offset(function, cursor_local, 0);
    function.instruction(&Instruction::I32Const(2));
    function.instruction(&Instruction::I32Store8(memarg(0, 0)));
    emit_local_plus_offset(function, cursor_local, CANONICAL_VALUE_REF_NODE_ID_OFFSET);
    function.instruction(&Instruction::LocalGet(handle_slot_local));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, cursor_local, CANONICAL_NODE_REF_SIZE);
    function.instruction(&Instruction::LocalSet(cursor_local));
}

#[allow(clippy::too_many_arguments)]
fn emit_write_non_immediate_canonical_value_ref(
    function: &mut WasmFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    value_abis: &BTreeMap<TypeId, MitkiValueAbi>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    _mode: LoweringMode,
    source: BoundaryValueSource,
    cursor_local: u32,
    child_count_local: u32,
    base_id_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    let leaf_type = canonical_semantic_leaf_type(semantic_graph, semantic_type)?;
    if canonical_value_uses_handle_slot(semantic_graph, semantic_type)? {
        let handle_abi = resolved_semantic_value_abi(value_abis, leaf_type, abi);
        return emit_write_runtime_handle_slot_ref(
            function,
            helper_indices,
            runtime_indices,
            handle_abi,
            leaf_type,
            source,
            cursor_local,
            locals,
        );
    }
    function.instruction(&Instruction::LocalGet(base_id_local));
    function.instruction(&Instruction::LocalGet(child_count_local));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(locals.count));
    emit_write_node_ref_at_cursor(function, cursor_local, locals.count);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_write_immediate_canonical_value_ref(
    function: &mut WasmFunction,
    value_abis: &BTreeMap<TypeId, MitkiValueAbi>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    mode: LoweringMode,
    source: BoundaryValueSource,
    cursor_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    let abi = resolved_semantic_value_abi(value_abis, semantic_type, abi);
    emit_local_plus_offset(function, cursor_local, 0);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Store8(memarg(0, 0)));
    match semantic_type_kind(semantic_graph, semantic_type)? {
        AbiV2TypeKind::Unit => {
            emit_local_plus_offset(function, cursor_local, 1);
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
            emit_local_plus_offset(function, cursor_local, CANONICAL_INLINE_UNIT_SIZE);
            function.instruction(&Instruction::LocalSet(cursor_local));
        }
        AbiV2TypeKind::Bool => {
            emit_local_plus_offset(function, cursor_local, 1);
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
            emit_load_i32_from_source(function, source);
            emit_store8_at_local(function, cursor_local, 2, locals.count);
            emit_local_plus_offset(function, cursor_local, CANONICAL_INLINE_BOOL_SIZE);
            function.instruction(&Instruction::LocalSet(cursor_local));
        }
        AbiV2TypeKind::Int { .. } => {
            emit_local_plus_offset(function, cursor_local, 1);
            function.instruction(&Instruction::I32Const(2));
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
            emit_local_plus_offset(function, cursor_local, 2);
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
            emit_local_plus_offset(function, cursor_local, 3);
            function.instruction(&Instruction::I32Const(32));
            function.instruction(&Instruction::I32Store16(memarg(0, 0)));
            emit_load_i32_from_source(function, source);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            emit_local_plus_offset(function, cursor_local, 5);
            function.instruction(&Instruction::LocalGet(locals.temp_ptr));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(function, cursor_local, 9);
            function.instruction(&Instruction::LocalGet(locals.temp_ptr));
            function.instruction(&Instruction::I32Const(31));
            function.instruction(&Instruction::I32ShrS);
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(function, cursor_local, CANONICAL_INLINE_INT_SIZE);
            function.instruction(&Instruction::LocalSet(cursor_local));
        }
        AbiV2TypeKind::Float { .. } => {
            emit_local_plus_offset(function, cursor_local, 1);
            function.instruction(&Instruction::I32Const(3));
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
            emit_local_plus_offset(function, cursor_local, 2);
            function.instruction(&Instruction::I32Const(64));
            function.instruction(&Instruction::I32Store16(memarg(0, 0)));
            emit_load_f64_from_source(function, source);
            emit_store_f64_at_local(function, cursor_local, 4, locals.f64_temp);
            emit_local_plus_offset(function, cursor_local, CANONICAL_INLINE_FLOAT_SIZE);
            function.instruction(&Instruction::LocalSet(cursor_local));
        }
        AbiV2TypeKind::Char => {
            emit_local_plus_offset(function, cursor_local, 1);
            function.instruction(&Instruction::I32Const(4));
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
            emit_load_i32_from_source(function, source);
            emit_store32_at_local(function, cursor_local, 2, locals.count);
            emit_local_plus_offset(function, cursor_local, CANONICAL_INLINE_CHAR_SIZE);
            function.instruction(&Instruction::LocalSet(cursor_local));
        }
        AbiV2TypeKind::Enum { .. } => {
            let enum_layout = enum_layout_for_mode(abi, mode)?;
            emit_materialize_value_source_base(function, abi, mode, source, locals.temp_ptr)?;
            emit_local_get_and_load32(function, locals.temp_ptr, 0);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            emit_local_plus_offset(function, cursor_local, 1);
            function.instruction(&Instruction::I32Const(5));
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
            emit_local_plus_offset(function, cursor_local, 2);
            function.instruction(&Instruction::I32Const(semantic_type.0 as i32));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_enum_tag_to_variant_index(function, enum_layout, locals.temp_ptr_aux);
            emit_store32_at_local(function, cursor_local, 6, locals.count);
            emit_local_plus_offset(function, cursor_local, CANONICAL_INLINE_ENUM_TAG_SIZE);
            function.instruction(&Instruction::LocalSet(cursor_local));
        }
        other => {
            return Err(Diagnostic::error(
                format!("internal error: unexpected immediate ABI v2 type `{other:?}`"),
                mitki_errors::TextRange::default(),
            ));
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_encode_canonical_nodes(
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    function: &mut WasmFunction,
    value_abis: &BTreeMap<TypeId, MitkiValueAbi>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    mode: LoweringMode,
    source: BoundaryValueSource,
    cursor_local: u32,
    base_id_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    let abi = resolved_semantic_value_abi(value_abis, semantic_type, abi);
    if matches!(
        semantic_type_kind(semantic_graph, semantic_type)?,
        AbiV2TypeKind::Intersection { .. }
    ) {
        let (carrier, live_facets) = live_intersection_members(semantic_graph, semantic_type)?;
        if live_facets.is_empty() {
            return emit_encode_canonical_nodes(
                helper_indices,
                runtime_indices,
                function,
                value_abis,
                resolved_semantic_value_abi(value_abis, carrier, abi),
                carrier,
                semantic_graph,
                mode,
                source,
                cursor_local,
                base_id_local,
                locals,
            );
        }
    }
    if canonical_value_uses_handle_slot(semantic_graph, semantic_type)? {
        return Ok(());
    }
    match semantic_type_kind(semantic_graph, semantic_type)? {
        AbiV2TypeKind::String => {
            emit_load_pointer_from_source(function, source, locals.temp_ptr);
            emit_local_get_and_load32(function, locals.temp_ptr, 0);
            function.instruction(&Instruction::LocalSet(locals.len));
            function.instruction(&Instruction::LocalGet(cursor_local));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            emit_local_plus_offset(function, locals.temp_ptr_aux, 0);
            function.instruction(&Instruction::I32Const(semantic_type.0 as i32));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_KIND_OFFSET);
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
            emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX0_OFFSET);
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX1_OFFSET);
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(
                function,
                locals.temp_ptr_aux,
                CANONICAL_NODE_CHILD_COUNT_OFFSET,
            );
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(
                function,
                locals.temp_ptr_aux,
                CANONICAL_NODE_PAYLOAD_LEN_OFFSET,
            );
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(function, cursor_local, CANONICAL_NODE_CHILDREN_OFFSET);
            function.instruction(&Instruction::LocalSet(cursor_local));
            emit_local_plus_offset(function, cursor_local, 0);
            emit_local_plus_offset(function, locals.temp_ptr, 4);
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
            function.instruction(&Instruction::LocalGet(cursor_local));
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(cursor_local));
            Ok(())
        }
        AbiV2TypeKind::Array { elem } => {
            let layout = array_layout_from_abi(abi)?;
            emit_load_pointer_from_source(function, source, locals.temp_ptr);
            emit_local_get_and_load32(function, locals.temp_ptr, ARRAY_LEN_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.len));
            let packed = semantic_type_is_immediate(semantic_graph, *elem)?
                && packed_scalar_width(&layout.item).is_some()
                && packed_scalar_kind_tag(&layout.item).is_some();
            if !packed {
                function.instruction(&Instruction::LocalGet(base_id_local));
                function.instruction(&Instruction::LocalGet(base_id_local));
                function.instruction(&Instruction::LocalSet(locals.base_id));
                function.instruction(&Instruction::I32Const(0));
                function.instruction(&Instruction::LocalSet(locals.index));
                function.instruction(&Instruction::Block(BlockType::Empty));
                function.instruction(&Instruction::Loop(BlockType::Empty));
                function.instruction(&Instruction::LocalGet(locals.index));
                function.instruction(&Instruction::LocalGet(locals.len));
                function.instruction(&Instruction::I32GeU);
                function.instruction(&Instruction::BrIf(1));
                function.instruction(&Instruction::LocalGet(locals.temp_ptr));
                function.instruction(&Instruction::LocalGet(locals.len));
                function.instruction(&Instruction::LocalGet(locals.index));
                function.instruction(&Instruction::LocalGet(locals.base_id));
                emit_runtime_array_element_addr(
                    function,
                    locals.temp_ptr,
                    layout,
                    locals.index,
                    locals.temp_ptr_aux,
                );
                emit_measure_canonical_value_ref(
                    function,
                    value_abis,
                    &layout.item,
                    *elem,
                    semantic_graph,
                    LoweringMode::Runtime,
                    BoundaryValueSource::Memory { base_local: locals.temp_ptr_aux, offset: 0 },
                    locals.child_count,
                    locals.child_bytes,
                    locals.handle_index,
                    locals,
                )?;
                function.instruction(&Instruction::LocalGet(locals.child_count));
                function.instruction(&Instruction::I32Eqz);
                function.instruction(&Instruction::If(BlockType::Empty));
                function.instruction(&Instruction::Else);
                function.instruction(&Instruction::LocalGet(locals.base_id));
                function.instruction(&Instruction::LocalSet(locals.count));
                emit_encode_canonical_nodes(
                    helper_indices,
                    runtime_indices,
                    function,
                    value_abis,
                    &layout.item,
                    *elem,
                    semantic_graph,
                    LoweringMode::Runtime,
                    BoundaryValueSource::Memory { base_local: locals.temp_ptr_aux, offset: 0 },
                    cursor_local,
                    locals.count,
                    locals,
                )?;
                function.instruction(&Instruction::End);
                function.instruction(&Instruction::LocalSet(locals.base_id));
                function.instruction(&Instruction::LocalSet(locals.index));
                function.instruction(&Instruction::LocalSet(locals.len));
                function.instruction(&Instruction::LocalSet(locals.temp_ptr));
                function.instruction(&Instruction::LocalGet(locals.child_count));
                function.instruction(&Instruction::I32Eqz);
                function.instruction(&Instruction::If(BlockType::Empty));
                function.instruction(&Instruction::Else);
                function.instruction(&Instruction::LocalGet(locals.base_id));
                function.instruction(&Instruction::LocalGet(locals.child_count));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(locals.base_id));
                function.instruction(&Instruction::End);
                function.instruction(&Instruction::LocalGet(locals.index));
                function.instruction(&Instruction::I32Const(1));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(locals.index));
                function.instruction(&Instruction::Br(0));
                function.instruction(&Instruction::End);
                function.instruction(&Instruction::End);
                function.instruction(&Instruction::LocalSet(locals.base_id));
            }

            function.instruction(&Instruction::LocalGet(cursor_local));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            emit_local_plus_offset(function, locals.temp_ptr_aux, 0);
            function.instruction(&Instruction::I32Const(semantic_type.0 as i32));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_KIND_OFFSET);
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
            emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX0_OFFSET);
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX1_OFFSET);
            function.instruction(&Instruction::I32Const(if packed {
                packed_scalar_kind_tag(&layout.item).expect("packed scalar kind") as i32
            } else {
                CANONICAL_ARRAY_VALUES_SENTINEL as i32
            }));
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(
                function,
                locals.temp_ptr_aux,
                CANONICAL_NODE_CHILD_COUNT_OFFSET,
            );
            if packed {
                function.instruction(&Instruction::I32Const(0));
            } else {
                function.instruction(&Instruction::LocalGet(locals.len));
            }
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(
                function,
                locals.temp_ptr_aux,
                CANONICAL_NODE_PAYLOAD_LEN_OFFSET,
            );
            if packed {
                function.instruction(&Instruction::LocalGet(locals.len));
                function.instruction(&Instruction::I32Const(
                    packed_scalar_width(&layout.item).expect("packed scalar width") as i32,
                ));
                function.instruction(&Instruction::I32Mul);
            } else {
                function.instruction(&Instruction::I32Const(0));
            }
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
            emit_local_plus_offset(function, cursor_local, CANONICAL_NODE_CHILDREN_OFFSET);
            function.instruction(&Instruction::LocalSet(cursor_local));

            if packed {
                match layout.item.kind {
                    MitkiValueKind::Bool => {
                        function.instruction(&Instruction::I32Const(0));
                        function.instruction(&Instruction::LocalSet(locals.index));
                        function.instruction(&Instruction::Block(BlockType::Empty));
                        function.instruction(&Instruction::Loop(BlockType::Empty));
                        function.instruction(&Instruction::LocalGet(locals.index));
                        function.instruction(&Instruction::LocalGet(locals.len));
                        function.instruction(&Instruction::I32GeU);
                        function.instruction(&Instruction::BrIf(1));
                        emit_runtime_array_element_addr(
                            function,
                            locals.temp_ptr,
                            layout,
                            locals.index,
                            locals.temp_ptr_aux,
                        );
                        emit_local_get_and_load32(function, locals.temp_ptr_aux, 0);
                        emit_store8_at_local(function, cursor_local, 0, locals.count);
                        emit_local_plus_offset(function, cursor_local, 1);
                        function.instruction(&Instruction::LocalSet(cursor_local));
                        function.instruction(&Instruction::LocalGet(locals.index));
                        function.instruction(&Instruction::I32Const(1));
                        function.instruction(&Instruction::I32Add);
                        function.instruction(&Instruction::LocalSet(locals.index));
                        function.instruction(&Instruction::Br(0));
                        function.instruction(&Instruction::End);
                        function.instruction(&Instruction::End);
                    }
                    MitkiValueKind::Int | MitkiValueKind::Char => {
                        emit_local_plus_offset(function, cursor_local, 0);
                        emit_local_plus_offset(function, locals.temp_ptr, layout.data_offset);
                        function.instruction(&Instruction::LocalGet(locals.len));
                        function.instruction(&Instruction::I32Const(4));
                        function.instruction(&Instruction::I32Mul);
                        function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
                        function.instruction(&Instruction::LocalGet(cursor_local));
                        function.instruction(&Instruction::LocalGet(locals.len));
                        function.instruction(&Instruction::I32Const(4));
                        function.instruction(&Instruction::I32Mul);
                        function.instruction(&Instruction::I32Add);
                        function.instruction(&Instruction::LocalSet(cursor_local));
                    }
                    MitkiValueKind::Float => {
                        emit_local_plus_offset(function, cursor_local, 0);
                        emit_local_plus_offset(function, locals.temp_ptr, layout.data_offset);
                        function.instruction(&Instruction::LocalGet(locals.len));
                        function.instruction(&Instruction::I32Const(8));
                        function.instruction(&Instruction::I32Mul);
                        function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
                        function.instruction(&Instruction::LocalGet(cursor_local));
                        function.instruction(&Instruction::LocalGet(locals.len));
                        function.instruction(&Instruction::I32Const(8));
                        function.instruction(&Instruction::I32Mul);
                        function.instruction(&Instruction::I32Add);
                        function.instruction(&Instruction::LocalSet(cursor_local));
                    }
                    _ => unreachable!(),
                }
                return Ok(());
            }
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::Block(BlockType::Empty));
            function.instruction(&Instruction::Loop(BlockType::Empty));
            function.instruction(&Instruction::LocalGet(locals.index));
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::I32GeU);
            function.instruction(&Instruction::BrIf(1));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.len));
            function.instruction(&Instruction::LocalGet(locals.index));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            emit_runtime_array_element_addr(
                function,
                locals.temp_ptr,
                layout,
                locals.index,
                locals.temp_ptr_aux,
            );
            if semantic_type_is_immediate(semantic_graph, *elem)? {
                emit_write_immediate_canonical_value_ref(
                    function,
                    value_abis,
                    &layout.item,
                    *elem,
                    semantic_graph,
                    LoweringMode::Runtime,
                    BoundaryValueSource::Memory { base_local: locals.temp_ptr_aux, offset: 0 },
                    cursor_local,
                    locals,
                )?;
            } else {
                emit_measure_canonical_value_ref(
                    function,
                    value_abis,
                    &layout.item,
                    *elem,
                    semantic_graph,
                    LoweringMode::Runtime,
                    BoundaryValueSource::Memory { base_local: locals.temp_ptr_aux, offset: 0 },
                    locals.child_count,
                    locals.child_bytes,
                    locals.handle_index,
                    locals,
                )?;
            }
            function.instruction(&Instruction::LocalSet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::LocalSet(locals.len));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            if !semantic_type_is_immediate(semantic_graph, *elem)? {
                emit_write_non_immediate_canonical_value_ref(
                    function,
                    helper_indices,
                    runtime_indices,
                    value_abis,
                    &layout.item,
                    *elem,
                    semantic_graph,
                    LoweringMode::Runtime,
                    BoundaryValueSource::Memory { base_local: locals.temp_ptr_aux, offset: 0 },
                    cursor_local,
                    locals.child_count,
                    locals.base_id,
                    locals,
                )?;
                function.instruction(&Instruction::LocalGet(locals.base_id));
                function.instruction(&Instruction::LocalGet(locals.child_count));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(locals.base_id));
            }
            function.instruction(&Instruction::LocalGet(locals.index));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(locals.index));
            function.instruction(&Instruction::Br(0));
            function.instruction(&Instruction::End);
            function.instruction(&Instruction::End);
            Ok(())
        }
        AbiV2TypeKind::Tuple { elems } => emit_encode_canonical_fields_node(
            helper_indices,
            runtime_indices,
            function,
            value_abis,
            abi,
            semantic_type,
            semantic_graph,
            mode,
            source,
            cursor_local,
            base_id_local,
            elems,
            2,
            locals,
        ),
        AbiV2TypeKind::Record { fields } => {
            let field_types = fields.iter().map(|field| field.ty).collect::<Vec<_>>();
            emit_encode_canonical_fields_node(
                helper_indices,
                runtime_indices,
                function,
                value_abis,
                abi,
                semantic_type,
                semantic_graph,
                mode,
                source,
                cursor_local,
                base_id_local,
                &field_types,
                3,
                locals,
            )
        }
        AbiV2TypeKind::Struct { fields, .. } => {
            let field_types = fields.iter().map(|field| field.ty).collect::<Vec<_>>();
            emit_encode_canonical_fields_node(
                helper_indices,
                runtime_indices,
                function,
                value_abis,
                abi,
                semantic_type,
                semantic_graph,
                mode,
                source,
                cursor_local,
                base_id_local,
                &field_types,
                4,
                locals,
            )
        }
        AbiV2TypeKind::Enum { variants, .. } => emit_encode_canonical_enum_node(
            helper_indices,
            runtime_indices,
            function,
            value_abis,
            abi,
            semantic_type,
            semantic_graph,
            mode,
            source,
            cursor_local,
            base_id_local,
            variants,
            locals,
        ),
        AbiV2TypeKind::Union { members } => emit_encode_canonical_union_node(
            helper_indices,
            runtime_indices,
            function,
            value_abis,
            abi,
            semantic_type,
            semantic_graph,
            mode,
            source,
            cursor_local,
            base_id_local,
            members,
            locals,
        ),
        _ => Err(Diagnostic::error(
            "ABI v2 baseline guest lowering does not support this structured node kind yet",
            mitki_errors::TextRange::default(),
        )),
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_encode_canonical_fields_node(
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    function: &mut WasmFunction,
    value_abis: &BTreeMap<TypeId, MitkiValueAbi>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    mode: LoweringMode,
    source: BoundaryValueSource,
    cursor_local: u32,
    base_id_local: u32,
    field_types: &[TypeId],
    node_kind_tag: i32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    emit_materialize_value_source_base(function, abi, mode, source, locals.temp_ptr)?;
    let layout = aggregate_layout_for_mode(abi, mode)?;
    let MitkiAggregateKindAbi::Fields(fields) = &layout.kind else {
        return Err(Diagnostic::error(
            "internal error: aggregate ABI v2 encoding expected fields layout",
            mitki_errors::TextRange::default(),
        ));
    };

    function.instruction(&Instruction::LocalGet(base_id_local));
    function.instruction(&Instruction::LocalGet(base_id_local));
    function.instruction(&Instruction::LocalSet(locals.base_id));
    for (field, &field_ty) in fields.iter().zip(field_types.iter()) {
        if semantic_type_is_immediate(semantic_graph, field_ty)? {
            continue;
        }
        function.instruction(&Instruction::LocalGet(locals.temp_ptr));
        function.instruction(&Instruction::LocalGet(locals.base_id));
        emit_measure_canonical_value_ref(
            function,
            value_abis,
            &field.value,
            field_ty,
            semantic_graph,
            mode,
            BoundaryValueSource::Memory { base_local: locals.temp_ptr, offset: field.offset },
            locals.child_count,
            locals.child_bytes,
            locals.handle_index,
            locals,
        )?;
        function.instruction(&Instruction::LocalSet(locals.base_id));
        function.instruction(&Instruction::LocalSet(locals.temp_ptr));
        function.instruction(&Instruction::LocalGet(locals.base_id));
        function.instruction(&Instruction::LocalSet(locals.count));
        function.instruction(&Instruction::LocalGet(locals.temp_ptr));
        function.instruction(&Instruction::LocalGet(locals.base_id));
        emit_encode_canonical_nodes(
            helper_indices,
            runtime_indices,
            function,
            value_abis,
            &field.value,
            field_ty,
            semantic_graph,
            mode,
            BoundaryValueSource::Memory { base_local: locals.temp_ptr, offset: field.offset },
            cursor_local,
            locals.count,
            locals,
        )?;
        function.instruction(&Instruction::LocalSet(locals.base_id));
        function.instruction(&Instruction::LocalSet(locals.temp_ptr));
        function.instruction(&Instruction::LocalGet(locals.base_id));
        function.instruction(&Instruction::LocalGet(locals.child_count));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(locals.base_id));
    }
    function.instruction(&Instruction::LocalSet(locals.base_id));

    function.instruction(&Instruction::LocalGet(cursor_local));
    function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
    emit_local_plus_offset(function, locals.temp_ptr_aux, 0);
    function.instruction(&Instruction::I32Const(semantic_type.0 as i32));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_KIND_OFFSET);
    function.instruction(&Instruction::I32Const(node_kind_tag));
    function.instruction(&Instruction::I32Store8(memarg(0, 0)));
    emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX0_OFFSET);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX1_OFFSET);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_CHILD_COUNT_OFFSET);
    function.instruction(&Instruction::I32Const(field_types.len() as i32));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_PAYLOAD_LEN_OFFSET);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, cursor_local, CANONICAL_NODE_CHILDREN_OFFSET);
    function.instruction(&Instruction::LocalSet(cursor_local));

    for (field, &field_ty) in fields.iter().zip(field_types.iter()) {
        let field_source =
            BoundaryValueSource::Memory { base_local: locals.temp_ptr, offset: field.offset };
        function.instruction(&Instruction::LocalGet(locals.temp_ptr));
        function.instruction(&Instruction::LocalGet(locals.base_id));
        if semantic_type_is_immediate(semantic_graph, field_ty)? {
            emit_write_immediate_canonical_value_ref(
                function,
                value_abis,
                &field.value,
                field_ty,
                semantic_graph,
                mode,
                field_source,
                cursor_local,
                locals,
            )?;
            function.instruction(&Instruction::LocalSet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            continue;
        }
        emit_measure_canonical_value_ref(
            function,
            value_abis,
            &field.value,
            field_ty,
            semantic_graph,
            mode,
            field_source,
            locals.child_count,
            locals.child_bytes,
            locals.handle_index,
            locals,
        )?;
        function.instruction(&Instruction::LocalSet(locals.base_id));
        function.instruction(&Instruction::LocalSet(locals.temp_ptr));
        emit_write_non_immediate_canonical_value_ref(
            function,
            helper_indices,
            runtime_indices,
            value_abis,
            &field.value,
            field_ty,
            semantic_graph,
            mode,
            field_source,
            cursor_local,
            locals.child_count,
            locals.base_id,
            locals,
        )?;
        function.instruction(&Instruction::LocalGet(locals.base_id));
        function.instruction(&Instruction::LocalGet(locals.child_count));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(locals.base_id));
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_encode_canonical_enum_node(
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    function: &mut WasmFunction,
    value_abis: &BTreeMap<TypeId, MitkiValueAbi>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    mode: LoweringMode,
    source: BoundaryValueSource,
    cursor_local: u32,
    base_id_local: u32,
    variants: &[mitki_abi::EnumVariant],
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    emit_materialize_value_source_base(function, abi, mode, source, locals.temp_ptr)?;
    let enum_layout = enum_layout_for_mode(abi, mode)?;
    emit_local_get_and_load32(function, locals.temp_ptr, 0);
    function.instruction(&Instruction::LocalSet(locals.index));

    for (variant_index, variant) in variants.iter().enumerate().rev() {
        let layout_variant = &enum_layout.variants[variant_index];
        function.instruction(&Instruction::LocalGet(locals.index));
        function.instruction(&Instruction::I32Const(layout_variant.tag));
        function.instruction(&Instruction::I32Eq);
        function.instruction(&Instruction::If(BlockType::Empty));

        function.instruction(&Instruction::LocalGet(base_id_local));
        function.instruction(&Instruction::LocalGet(base_id_local));
        function.instruction(&Instruction::LocalSet(locals.base_id));
        for (field, &field_ty) in layout_variant.fields.iter().zip(variant.fields.iter()) {
            if semantic_type_is_immediate(semantic_graph, field_ty)? {
                continue;
            }
            function.instruction(&Instruction::LocalGet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            emit_measure_canonical_value_ref(
                function,
                value_abis,
                &field.value,
                field_ty,
                semantic_graph,
                mode,
                BoundaryValueSource::Memory { base_local: locals.temp_ptr, offset: field.offset },
                locals.child_count,
                locals.child_bytes,
                locals.handle_index,
                locals,
            )?;
            function.instruction(&Instruction::LocalSet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.count));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            emit_encode_canonical_nodes(
                helper_indices,
                runtime_indices,
                function,
                value_abis,
                &field.value,
                field_ty,
                semantic_graph,
                mode,
                BoundaryValueSource::Memory { base_local: locals.temp_ptr, offset: field.offset },
                cursor_local,
                locals.count,
                locals,
            )?;
            function.instruction(&Instruction::LocalSet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            function.instruction(&Instruction::LocalGet(locals.child_count));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(locals.base_id));
        }
        function.instruction(&Instruction::LocalSet(locals.base_id));

        function.instruction(&Instruction::LocalGet(cursor_local));
        function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
        emit_local_plus_offset(function, locals.temp_ptr_aux, 0);
        function.instruction(&Instruction::I32Const(semantic_type.0 as i32));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_KIND_OFFSET);
        function.instruction(&Instruction::I32Const(5));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX0_OFFSET);
        function.instruction(&Instruction::I32Const(variant_index as i32));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX1_OFFSET);
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_CHILD_COUNT_OFFSET);
        function.instruction(&Instruction::I32Const(layout_variant.fields.len() as i32));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_PAYLOAD_LEN_OFFSET);
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, cursor_local, CANONICAL_NODE_CHILDREN_OFFSET);
        function.instruction(&Instruction::LocalSet(cursor_local));

        for (field, &field_ty) in layout_variant.fields.iter().zip(variant.fields.iter()) {
            let field_source =
                BoundaryValueSource::Memory { base_local: locals.temp_ptr, offset: field.offset };
            function.instruction(&Instruction::LocalGet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            if semantic_type_is_immediate(semantic_graph, field_ty)? {
                emit_write_immediate_canonical_value_ref(
                    function,
                    value_abis,
                    &field.value,
                    field_ty,
                    semantic_graph,
                    mode,
                    field_source,
                    cursor_local,
                    locals,
                )?;
                function.instruction(&Instruction::LocalSet(locals.base_id));
                function.instruction(&Instruction::LocalSet(locals.temp_ptr));
                continue;
            }
            emit_measure_canonical_value_ref(
                function,
                value_abis,
                &field.value,
                field_ty,
                semantic_graph,
                mode,
                field_source,
                locals.child_count,
                locals.child_bytes,
                locals.handle_index,
                locals,
            )?;
            function.instruction(&Instruction::LocalSet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            emit_write_non_immediate_canonical_value_ref(
                function,
                helper_indices,
                runtime_indices,
                value_abis,
                &field.value,
                field_ty,
                semantic_graph,
                mode,
                field_source,
                cursor_local,
                locals.child_count,
                locals.base_id,
                locals,
            )?;
            function.instruction(&Instruction::LocalGet(locals.base_id));
            function.instruction(&Instruction::LocalGet(locals.child_count));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(locals.base_id));
        }

        function.instruction(&Instruction::End);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_encode_canonical_union_node(
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    function: &mut WasmFunction,
    value_abis: &BTreeMap<TypeId, MitkiValueAbi>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    semantic_graph: &SemanticTypeGraph,
    mode: LoweringMode,
    source: BoundaryValueSource,
    cursor_local: u32,
    base_id_local: u32,
    members: &[TypeId],
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    emit_materialize_value_source_base(function, abi, mode, source, locals.temp_ptr)?;
    let union_layout = enum_layout_for_mode(abi, mode)?;
    emit_local_get_and_load32(function, locals.temp_ptr, 0);
    function.instruction(&Instruction::LocalSet(locals.index));

    for (arm_index, &member_ty) in members.iter().enumerate().rev() {
        let runtime_variant_index = union_runtime_variant_index(abi, members, member_ty)?;
        let layout_variant = &union_layout.variants[runtime_variant_index];
        let field = layout_variant.fields.first().ok_or_else(|| {
            Diagnostic::error(
                "internal error: union ABI v2 encoding expected a payload field",
                mitki_errors::TextRange::default(),
            )
        })?;
        function.instruction(&Instruction::LocalGet(locals.index));
        function.instruction(&Instruction::I32Const(layout_variant.tag));
        function.instruction(&Instruction::I32Eq);
        function.instruction(&Instruction::If(BlockType::Empty));

        if !semantic_type_is_immediate(semantic_graph, member_ty)? {
            function.instruction(&Instruction::LocalGet(base_id_local));
            function.instruction(&Instruction::LocalGet(base_id_local));
            function.instruction(&Instruction::LocalSet(locals.base_id));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            emit_measure_canonical_value_ref(
                function,
                value_abis,
                &field.value,
                member_ty,
                semantic_graph,
                mode,
                BoundaryValueSource::Memory { base_local: locals.temp_ptr, offset: field.offset },
                locals.child_count,
                locals.child_bytes,
                locals.handle_index,
                locals,
            )?;
            function.instruction(&Instruction::LocalSet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.count));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            emit_encode_canonical_nodes(
                helper_indices,
                runtime_indices,
                function,
                value_abis,
                &field.value,
                member_ty,
                semantic_graph,
                mode,
                BoundaryValueSource::Memory { base_local: locals.temp_ptr, offset: field.offset },
                cursor_local,
                locals.count,
                locals,
            )?;
            function.instruction(&Instruction::LocalSet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.temp_ptr));
            function.instruction(&Instruction::LocalGet(locals.base_id));
            function.instruction(&Instruction::LocalGet(locals.child_count));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(locals.base_id));
            function.instruction(&Instruction::LocalSet(locals.base_id));
        }

        function.instruction(&Instruction::LocalGet(cursor_local));
        function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
        emit_local_plus_offset(function, locals.temp_ptr_aux, 0);
        function.instruction(&Instruction::I32Const(semantic_type.0 as i32));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_KIND_OFFSET);
        function.instruction(&Instruction::I32Const(6));
        function.instruction(&Instruction::I32Store8(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX0_OFFSET);
        function.instruction(&Instruction::I32Const(arm_index as i32));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_AUX1_OFFSET);
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_CHILD_COUNT_OFFSET);
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, locals.temp_ptr_aux, CANONICAL_NODE_PAYLOAD_LEN_OFFSET);
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Store(memarg(0, 0)));
        emit_local_plus_offset(function, cursor_local, CANONICAL_NODE_CHILDREN_OFFSET);
        function.instruction(&Instruction::LocalSet(cursor_local));

        let field_source =
            BoundaryValueSource::Memory { base_local: locals.temp_ptr, offset: field.offset };
        if semantic_type_is_immediate(semantic_graph, member_ty)? {
            emit_write_immediate_canonical_value_ref(
                function,
                value_abis,
                &field.value,
                member_ty,
                semantic_graph,
                mode,
                field_source,
                cursor_local,
                locals,
            )?;
        } else {
            emit_write_non_immediate_canonical_value_ref(
                function,
                helper_indices,
                runtime_indices,
                value_abis,
                &field.value,
                member_ty,
                semantic_graph,
                mode,
                field_source,
                cursor_local,
                locals.child_count,
                base_id_local,
                locals,
            )?;
        }

        function.instruction(&Instruction::End);
    }
    Ok(())
}

fn emit_write_canonical_blob_header(
    function: &mut WasmFunction,
    blob_local: u32,
    transport_type: TypeId,
    total_len_local: u32,
    node_count_local: u32,
    handle_count_local: u32,
) {
    for (index, byte) in [b'M', b'T', b'K', b'C', b'V', b'2', 0, 0].into_iter().enumerate() {
        emit_local_plus_offset(function, blob_local, index as u32);
        function.instruction(&Instruction::I32Const(i32::from(byte)));
        function.instruction(&Instruction::I32Store8(memarg(0, 0)));
    }
    emit_local_plus_offset(function, blob_local, 8);
    function.instruction(&Instruction::I32Const(2));
    function.instruction(&Instruction::I32Store16(memarg(0, 0)));
    emit_local_plus_offset(function, blob_local, 10);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Store16(memarg(0, 0)));
    emit_local_plus_offset(function, blob_local, 12);
    function.instruction(&Instruction::I32Const(i32::from(CANONICAL_BLOB_ENCODING_VERSION)));
    function.instruction(&Instruction::I32Store16(memarg(0, 0)));
    emit_local_plus_offset(function, blob_local, 14);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, blob_local, 18);
    function.instruction(&Instruction::I32Const(transport_type.0 as i32));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, blob_local, CANONICAL_BLOB_TOTAL_LEN_OFFSET);
    function.instruction(&Instruction::LocalGet(total_len_local));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, blob_local, CANONICAL_BLOB_NODE_COUNT_OFFSET);
    function.instruction(&Instruction::LocalGet(node_count_local));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, blob_local, CANONICAL_BLOB_HANDLE_COUNT_OFFSET);
    function.instruction(&Instruction::LocalGet(handle_count_local));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, blob_local, CANONICAL_BLOB_ROOT_OFFSET);
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Store8(memarg(0, 0)));
    emit_local_plus_offset(function, blob_local, CANONICAL_BLOB_ROOT_OFFSET + 1);
    function.instruction(&Instruction::LocalGet(node_count_local));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
}

fn emit_alloc_runtime_array_to_local(
    function: &mut WasmFunction,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    layout: &MitkiArrayLayoutAbi,
    len_local: u32,
    base_local: u32,
    payload_local: u32,
) -> Result<(), Diagnostic> {
    function.instruction(&Instruction::LocalGet(len_local));
    function.instruction(&Instruction::I32Const(layout.item_stride as i32));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Const((ARC_HEADER_SIZE + layout.data_offset) as i32));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(payload_local));
    emit_dynamic_alloc_to_local(
        function,
        runtime_indices,
        payload_local,
        layout.object_align,
        base_local,
    )?;
    emit_local_plus_offset(function, base_local, 0);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalGet(payload_local));
    function.instruction(&Instruction::MemoryFill(0));
    emit_write_arc_header(function, base_local);
    emit_local_plus_offset(function, base_local, ARC_HEADER_SIZE);
    function.instruction(&Instruction::LocalSet(payload_local));
    emit_local_plus_offset(function, payload_local, ARRAY_LEN_OFFSET);
    function.instruction(&Instruction::LocalGet(len_local));
    function.instruction(&Instruction::I32Store(memarg(0, 2)));
    emit_local_plus_offset(function, payload_local, ARRAY_CAPACITY_OFFSET);
    function.instruction(&Instruction::LocalGet(len_local));
    function.instruction(&Instruction::I32Store(memarg(0, 2)));
    Ok(())
}

fn emit_runtime_array_element_addr(
    function: &mut WasmFunction,
    payload_local: u32,
    layout: &MitkiArrayLayoutAbi,
    index_local: u32,
    dest_local: u32,
) {
    emit_local_plus_offset(function, payload_local, layout.data_offset);
    function.instruction(&Instruction::LocalGet(index_local));
    function.instruction(&Instruction::I32Const(layout.item_stride as i32));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(dest_local));
}

fn emit_materialize_decode_dest_base(
    function: &mut WasmFunction,
    abi: &MitkiValueAbi,
    mode: LoweringMode,
    dest: BoundaryValueDest,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    locals: CanonicalWrapperLocals,
) -> Result<u32, Diagnostic> {
    match abi.lowering(mode) {
        MitkiLoweringAbi::Aggregate(_) => {
            match dest {
                BoundaryValueDest::Local(local) => {
                    function.instruction(&Instruction::LocalGet(local));
                    function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
                }
                BoundaryValueDest::Memory { base_local, offset } => {
                    emit_local_plus_offset(function, base_local, offset);
                    function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
                }
            }
            Ok(locals.temp_ptr_aux)
        }
        MitkiLoweringAbi::Pointer => {
            let layout = aggregate_layout_for_mode(abi, mode)?;
            emit_alloc_nominal_payload_to_local(
                function,
                runtime_indices,
                layout,
                locals.temp_ptr,
                locals.temp_ptr_aux,
            )?;
            Ok(locals.temp_ptr_aux)
        }
        _ => Err(Diagnostic::error(
            "internal error: expected aggregate decode destination",
            mitki_errors::TextRange::default(),
        )),
    }
}

fn emit_materialize_value_source_base(
    function: &mut WasmFunction,
    abi: &MitkiValueAbi,
    mode: LoweringMode,
    source: BoundaryValueSource,
    out_local: u32,
) -> Result<(), Diagnostic> {
    match abi.lowering(mode) {
        MitkiLoweringAbi::Aggregate(_) => {
            emit_load_aggregate_base_from_source(function, source, out_local);
            Ok(())
        }
        MitkiLoweringAbi::Pointer => {
            emit_load_pointer_from_source(function, source, out_local);
            Ok(())
        }
        _ => Err(Diagnostic::error(
            "internal error: expected aggregate or pointer source during ABI v2 lowering",
            mitki_errors::TextRange::default(),
        )),
    }
}

fn emit_local_plus_offset(function: &mut WasmFunction, local: u32, offset: u32) {
    function.instruction(&Instruction::LocalGet(local));
    if offset != 0 {
        function.instruction(&Instruction::I32Const(offset as i32));
        function.instruction(&Instruction::I32Add);
    }
}

fn emit_local_get_and_load8(function: &mut WasmFunction, local: u32, offset: u32) {
    emit_local_plus_offset(function, local, offset);
    function.instruction(&Instruction::I32Load8U(memarg(0, 0)));
}

fn emit_local_get_and_load32(function: &mut WasmFunction, local: u32, offset: u32) {
    emit_local_plus_offset(function, local, offset);
    function.instruction(&Instruction::I32Load(memarg(0, 0)));
}

fn emit_local_get_and_load_f64(function: &mut WasmFunction, local: u32, offset: u32) {
    emit_local_plus_offset(function, local, offset);
    function.instruction(&Instruction::F64Load(memarg(0, 0)));
}

fn emit_store8_at_local(function: &mut WasmFunction, local: u32, offset: u32, scratch_local: u32) {
    function.instruction(&Instruction::LocalSet(scratch_local));
    emit_local_plus_offset(function, local, offset);
    function.instruction(&Instruction::LocalGet(scratch_local));
    function.instruction(&Instruction::I32Store8(memarg(0, 0)));
}

fn emit_store32_at_local(function: &mut WasmFunction, local: u32, offset: u32, scratch_local: u32) {
    function.instruction(&Instruction::LocalSet(scratch_local));
    emit_local_plus_offset(function, local, offset);
    function.instruction(&Instruction::LocalGet(scratch_local));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
}

fn emit_store_f64_at_local(
    function: &mut WasmFunction,
    local: u32,
    offset: u32,
    scratch_local: u32,
) {
    function.instruction(&Instruction::LocalSet(scratch_local));
    emit_local_plus_offset(function, local, offset);
    function.instruction(&Instruction::LocalGet(scratch_local));
    function.instruction(&Instruction::F64Store(memarg(0, 0)));
}

fn emit_load_i32_from_source(function: &mut WasmFunction, source: BoundaryValueSource) {
    match source {
        BoundaryValueSource::Local(local) => {
            function.instruction(&Instruction::LocalGet(local));
        }
        BoundaryValueSource::Memory { base_local, offset } => {
            emit_local_get_and_load32(function, base_local, offset);
        }
    }
}

fn emit_load_f64_from_source(function: &mut WasmFunction, source: BoundaryValueSource) {
    match source {
        BoundaryValueSource::Local(local) => {
            function.instruction(&Instruction::LocalGet(local));
        }
        BoundaryValueSource::Memory { base_local, offset } => {
            emit_local_get_and_load_f64(function, base_local, offset);
        }
    }
}

fn emit_store_i32_to_dest(
    function: &mut WasmFunction,
    dest: BoundaryValueDest,
    scratch_local: u32,
) {
    match dest {
        BoundaryValueDest::Local(local) => {
            function.instruction(&Instruction::LocalSet(local));
        }
        BoundaryValueDest::Memory { base_local, offset } => {
            emit_store32_at_local(function, base_local, offset, scratch_local);
        }
    }
}

fn emit_store_f64_to_dest(
    function: &mut WasmFunction,
    dest: BoundaryValueDest,
    scratch_local: u32,
) {
    match dest {
        BoundaryValueDest::Local(local) => {
            function.instruction(&Instruction::LocalSet(local));
        }
        BoundaryValueDest::Memory { base_local, offset } => {
            emit_store_f64_at_local(function, base_local, offset, scratch_local);
        }
    }
}

fn emit_load_pointer_from_source(
    function: &mut WasmFunction,
    source: BoundaryValueSource,
    out_local: u32,
) {
    emit_load_i32_from_source(function, source);
    function.instruction(&Instruction::LocalSet(out_local));
}

fn emit_load_aggregate_base_from_source(
    function: &mut WasmFunction,
    source: BoundaryValueSource,
    out_local: u32,
) {
    match source {
        BoundaryValueSource::Local(local) => {
            function.instruction(&Instruction::LocalGet(local));
            function.instruction(&Instruction::LocalSet(out_local));
        }
        BoundaryValueSource::Memory { base_local, offset } => {
            emit_local_plus_offset(function, base_local, offset);
            function.instruction(&Instruction::LocalSet(out_local));
        }
    }
}

fn emit_zero_local_region(function: &mut WasmFunction, base_local: u32, offset: u32, size: u32) {
    if size == 0 {
        return;
    }
    emit_local_plus_offset(function, base_local, offset);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Const(size as i32));
    function.instruction(&Instruction::MemoryFill(0));
}

fn emit_alloc_temp_buffer(
    function: &mut WasmFunction,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    local: u32,
    size: u32,
    align: u32,
) -> Result<(), Diagnostic> {
    let alloc = runtime_indices.get(&RuntimeFunction::Alloc).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: missing alloc runtime import during boundary marshaling",
            mitki_errors::TextRange::default(),
        )
    })?;
    function.instruction(&Instruction::I32Const(size as i32));
    function.instruction(&Instruction::I32Const(align as i32));
    function.instruction(&Instruction::Call(alloc));
    function.instruction(&Instruction::LocalSet(local));
    Ok(())
}

fn emit_dealloc_temp_buffer(
    function: &mut WasmFunction,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    local: u32,
    size: u32,
    align: u32,
) -> Result<(), Diagnostic> {
    let dealloc = runtime_indices.get(&RuntimeFunction::Dealloc).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: missing dealloc runtime import during boundary marshaling",
            mitki_errors::TextRange::default(),
        )
    })?;
    function.instruction(&Instruction::LocalGet(local));
    function.instruction(&Instruction::I32Const(size as i32));
    function.instruction(&Instruction::I32Const(align as i32));
    function.instruction(&Instruction::Call(dealloc));
    Ok(())
}

fn emit_dealloc_canonical_blob(
    function: &mut WasmFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    closure_destroyers: &[(u32, u32)],
    blob_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    emit_release_canonical_blob_impl(
        function,
        helper_indices,
        runtime_indices,
        closure_destroyers,
        blob_local,
        locals,
    )
}

fn emit_release_canonical_blob_impl(
    function: &mut WasmFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    closure_destroyers: &[(u32, u32)],
    blob_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    let dealloc = runtime_indices.get(&RuntimeFunction::Dealloc).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: missing dealloc runtime import during ABI v2 blob cleanup",
            mitki_errors::TextRange::default(),
        )
    })?;
    emit_local_get_and_load32(function, blob_local, CANONICAL_BLOB_HANDLE_COUNT_OFFSET);
    function.instruction(&Instruction::LocalSet(locals.len));
    function.instruction(&Instruction::LocalGet(locals.len));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::Else);
    emit_find_handle_table_offset(function, blob_local, locals.cursor, locals);
    emit_local_plus_offset(function, locals.cursor, CANONICAL_HANDLE_TABLE_LEN_SIZE);
    function.instruction(&Instruction::LocalSet(locals.cursor));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(locals.index));
    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));
    function.instruction(&Instruction::LocalGet(locals.index));
    function.instruction(&Instruction::LocalGet(locals.len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));
    emit_local_get_and_load32(function, locals.cursor, ABI_V2_HANDLE_FIELD1_OFFSET);
    function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
    emit_release_boundary_handle_object_impl(
        function,
        helper_indices,
        runtime_indices,
        closure_destroyers,
        locals.temp_ptr_aux,
        locals.temp_ptr,
        locals.count,
        locals.base_id,
    )?;
    emit_local_plus_offset(function, locals.cursor, ABI_V2_HANDLE_PAYLOAD_SIZE);
    function.instruction(&Instruction::LocalSet(locals.cursor));
    function.instruction(&Instruction::LocalGet(locals.index));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(locals.index));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(blob_local));
    emit_local_get_and_load32(function, blob_local, CANONICAL_BLOB_TOTAL_LEN_OFFSET);
    function.instruction(&Instruction::I32Const(CANONICAL_BLOB_ALIGN as i32));
    function.instruction(&Instruction::Call(dealloc));
    Ok(())
}

fn emit_dynamic_alloc_to_local(
    function: &mut WasmFunction,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    size_local: u32,
    align: u32,
    dest_local: u32,
) -> Result<(), Diagnostic> {
    let alloc = runtime_indices.get(&RuntimeFunction::Alloc).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: missing alloc runtime import during ABI v2 marshaling",
            mitki_errors::TextRange::default(),
        )
    })?;
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&Instruction::I32Const(align as i32));
    function.instruction(&Instruction::Call(alloc));
    function.instruction(&Instruction::LocalSet(dest_local));
    Ok(())
}

fn emit_write_arc_header(function: &mut WasmFunction, base_local: u32) {
    emit_local_plus_offset(function, base_local, 0);
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Store(memarg(0, 2)));
    emit_local_plus_offset(function, base_local, 4);
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Store(memarg(0, 2)));
}

fn emit_alloc_nominal_payload_to_local(
    function: &mut WasmFunction,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    layout: &MitkiAggregateLayoutAbi,
    base_local: u32,
    payload_local: u32,
) -> Result<(), Diagnostic> {
    let alloc = runtime_indices.get(&RuntimeFunction::Alloc).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: missing alloc runtime import during nominal ABI v2 marshaling",
            mitki_errors::TextRange::default(),
        )
    })?;
    let total = ARC_HEADER_SIZE.checked_add(layout.size).ok_or_else(|| {
        Diagnostic::error(
            "internal error: nominal ABI v2 allocation overflowed",
            mitki_errors::TextRange::default(),
        )
    })?;
    function.instruction(&Instruction::I32Const(total as i32));
    function.instruction(&Instruction::I32Const(ARC_ALIGN as i32));
    function.instruction(&Instruction::Call(alloc));
    function.instruction(&Instruction::LocalSet(base_local));
    emit_zero_local_region(function, base_local, 0, total);
    emit_write_arc_header(function, base_local);
    emit_local_plus_offset(function, base_local, ARC_HEADER_SIZE);
    function.instruction(&Instruction::LocalSet(payload_local));
    Ok(())
}

fn adapter_layout(
    param_count: u32,
    extra_i32_locals: u32,
    extra_f64_locals: u32,
) -> FunctionLayout {
    let mut local_plan = LocalPlanBuilder::new(param_count);
    local_plan.alloc_scratch(ScratchLocalKind::ScratchI32, ValType::I32);
    local_plan.alloc_scratch(ScratchLocalKind::ScratchI32Aux, ValType::I32);
    local_plan.alloc_scratch(ScratchLocalKind::ObjectI32, ValType::I32);
    for ordinal in 0..extra_i32_locals {
        local_plan.alloc_join(LocalPurpose::AdapterTemp { ordinal }, None, ValType::I32);
    }
    if extra_f64_locals > 0 {
        local_plan.alloc_scratch(ScratchLocalKind::ScratchF64, ValType::F64);
        for ordinal in 1..extra_f64_locals {
            local_plan.alloc_join(
                LocalPurpose::AdapterTemp { ordinal: extra_i32_locals + ordinal },
                None,
                ValType::F64,
            );
        }
    }
    local_plan.alloc_scratch(ScratchLocalKind::ScratchI64, ValType::I64);
    FunctionLayout::new(
        local_plan.finish(),
        FramePlan::default(),
        FunctionLayoutLookups {
            slots: FxHashMap::default(),
            param_names: Vec::new(),
            raw_params: Vec::new(),
            temps: FxHashMap::default(),
            pattern_scalar_locals: FxHashMap::default(),
            nominal_locals: FxHashMap::default(),
            array_repeat_locals: FxHashMap::default(),
        },
    )
}

pub(super) fn emit_bool_param_normalization(
    function: &mut WasmFunction,
    body: &Function<'_>,
    signature: &FunctionSignature,
    layout: &FunctionLayout,
) {
    for (index, (&param, ty)) in body.params().iter().zip(&signature.params).enumerate() {
        if !matches!(ty, AbiTy::Scalar(BackendTy::Bool)) {
            continue;
        }

        let _ = param;
        let Some(slot) = layout.raw_params.get(index) else {
            continue;
        };
        let Some(index) = slot.local_index else {
            continue;
        };

        function.instruction(&Instruction::LocalGet(index));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Ne);
        function.instruction(&Instruction::LocalSet(index));
    }
}

struct WrapperMirLayout {
    layout: FunctionLayout,
    place_locals: FxHashMap<PlaceId, u32>,
    value_locals: FxHashMap<ValueId, u32>,
    canonical_locals: Option<CanonicalWrapperLocals>,
}

struct WrapperMirBoundaryAbi<'a, 'db> {
    semantic_graph: &'a SemanticTypeGraph,
    boundary_signature: &'a BoundarySig<'db>,
    abi_signature: &'a AbiV2FunctionSignature,
    function_abi: MitkiFunctionAbi,
}

struct WrapperMirResolvedSlot<'a> {
    transport: &'a mitki_abi::TransportRef,
    value_abi: &'a MitkiValueAbi,
    runtime_abi: &'a AbiTy,
}

struct WrapperMirEmitter<'a, 'db> {
    backend: &'a Backend<'db>,
    location: FunctionLocation<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    mir: &'a WrapperMirFunction<'db>,
    direct_function_indices: &'a FxHashMap<InstanceKey<'db>, u32>,
    raw_import_function_indices: &'a FxHashMap<InstanceKey<'db>, u32>,
    runtime_indices: &'a FxHashMap<RuntimeFunction, u32>,
    stage_indices: &'a FxHashMap<StageIntrinsic, u32>,
    helper_indices: &'a FxHashMap<HelperFunction, u32>,
    nominal_destroyers: &'a FxHashMap<u32, u32>,
    nominal_eq_helpers: &'a FxHashMap<u32, u32>,
    array_destroyers: &'a FxHashMap<u32, u32>,
    array_eq_helpers: &'a FxHashMap<u32, u32>,
    callable_type_indices: &'a FxHashMap<FunctionSignature, u32>,
    table_slots: &'a FxHashMap<FunctionValueTarget<'db>, u32>,
    closure_destroyers: &'a [(u32, u32)],
    layout: WrapperMirLayout,
    boundary_abi: Option<WrapperMirBoundaryAbi<'a, 'db>>,
}

impl WrapperMirLayout {
    fn build(mir: &WrapperMirFunction<'_>) -> Self {
        let mut local_plan = LocalPlanBuilder::new(mir.signature.wasm_params.len() as u32);
        local_plan.alloc_scratch(ScratchLocalKind::ScratchI32, ValType::I32);
        local_plan.alloc_scratch(ScratchLocalKind::ScratchI32Aux, ValType::I32);
        local_plan.alloc_scratch(ScratchLocalKind::ObjectI32, ValType::I32);

        let mut next_adapter_local = 0u32;
        let mut place_locals = FxHashMap::default();
        for place in &mir.places {
            let Some(value_type) = place.ty.value_type() else {
                continue;
            };
            let local = local_plan.alloc_join(
                LocalPurpose::AdapterTemp { ordinal: next_adapter_local },
                None,
                value_type,
            );
            next_adapter_local += 1;
            place_locals.insert(place.id, local);
        }

        let mut value_locals = FxHashMap::default();
        collect_wrapper_mir_value_locals(
            &mir.body,
            &mut local_plan,
            &mut value_locals,
            &mut next_adapter_local,
        );

        local_plan.alloc_scratch(ScratchLocalKind::ScratchF64, ValType::F64);
        local_plan.alloc_scratch(ScratchLocalKind::ScratchI64, ValType::I64);
        let layout = FunctionLayout::new(
            local_plan.finish(),
            FramePlan::default(),
            FunctionLayoutLookups {
                slots: FxHashMap::default(),
                param_names: Vec::new(),
                raw_params: Vec::new(),
                temps: FxHashMap::default(),
                pattern_scalar_locals: FxHashMap::default(),
                nominal_locals: FxHashMap::default(),
                array_repeat_locals: FxHashMap::default(),
            },
        );
        let canonical_locals = build_wrapper_mir_canonical_locals(mir, &place_locals);
        Self { layout, place_locals, value_locals, canonical_locals }
    }
}

fn collect_wrapper_mir_value_locals(
    region: &WrapperRegion<'_>,
    local_plan: &mut LocalPlanBuilder,
    value_locals: &mut FxHashMap<ValueId, u32>,
    next_adapter_local: &mut u32,
) {
    for stmt in &region.stmts {
        match stmt {
            WrapperStmt::Let { value, ty, .. } => {
                let Some(value_type) = ty.value_type() else {
                    continue;
                };
                let local = local_plan.alloc_join(
                    LocalPurpose::AdapterTemp { ordinal: *next_adapter_local },
                    None,
                    value_type,
                );
                *next_adapter_local += 1;
                value_locals.insert(*value, local);
            }
            WrapperStmt::If { then_region, else_region, .. } => {
                collect_wrapper_mir_value_locals(
                    then_region,
                    local_plan,
                    value_locals,
                    next_adapter_local,
                );
                collect_wrapper_mir_value_locals(
                    else_region,
                    local_plan,
                    value_locals,
                    next_adapter_local,
                );
            }
            WrapperStmt::Store { .. } | WrapperStmt::Eval { .. } | WrapperStmt::Return { .. } => {}
        }
    }
}

fn build_wrapper_mir_canonical_locals(
    mir: &WrapperMirFunction<'_>,
    place_locals: &FxHashMap<PlaceId, u32>,
) -> Option<CanonicalWrapperLocals> {
    let mut scratch = FxHashMap::default();
    for place in &mir.places {
        let WrapperPlaceKind::Scratch(kind) = place.kind else {
            continue;
        };
        let local = place_locals.get(&place.id).copied()?;
        scratch.insert(kind, local);
    }
    (!scratch.is_empty()).then(|| CanonicalWrapperLocals {
        node_offset: scratch[&WrapperScratchKind::NodeOffset],
        cursor: scratch[&WrapperScratchKind::Cursor],
        index: scratch[&WrapperScratchKind::Index],
        len: scratch[&WrapperScratchKind::Len],
        count: scratch[&WrapperScratchKind::Count],
        bytes: scratch[&WrapperScratchKind::Bytes],
        base_id: scratch[&WrapperScratchKind::BaseId],
        temp_ptr: scratch[&WrapperScratchKind::TempPtr],
        temp_ptr_aux: scratch[&WrapperScratchKind::TempPtrAux],
        child_count: scratch[&WrapperScratchKind::ChildCount],
        child_bytes: scratch[&WrapperScratchKind::ChildBytes],
        handle_count: scratch[&WrapperScratchKind::HandleCount],
        handle_index: scratch[&WrapperScratchKind::HandleIndex],
        handle_cursor: scratch[&WrapperScratchKind::HandleCursor],
        f64_temp: scratch[&WrapperScratchKind::F64Temp],
    })
}

fn wrapper_mir_function_result_abi(signature: &WrapperMirSignature<'_>) -> AbiTy {
    signature
        .callable
        .as_ref()
        .map(|callable| callable.result.clone())
        .or_else(|| {
            signature.internal.as_ref().and_then(|internal| internal.results.first().cloned())
        })
        .unwrap_or(AbiTy::Scalar(BackendTy::Unit))
}

fn wrapper_place_ty_for_param(
    mir: &WrapperMirFunction<'_>,
    param: u32,
) -> Result<WrapperPlaceTy, Diagnostic> {
    let value_type = mir.signature.wasm_params.get(param as usize).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: wrapper MIR referenced an out-of-bounds parameter lane",
            mitki_errors::TextRange::default(),
        )
    })?;
    Ok(match value_type {
        ValType::I32 => WrapperPlaceTy::I32,
        ValType::I64 => WrapperPlaceTy::I64,
        ValType::F64 => WrapperPlaceTy::F64,
        _ => WrapperPlaceTy::I32,
    })
}

fn wrapper_transport_is_nullary_enum(
    semantic_graph: &SemanticTypeGraph,
    transport: &mitki_abi::TransportRef,
) -> Result<bool, Diagnostic> {
    Ok(matches!(
        semantic_type_kind(semantic_graph, transport_type_id(transport))?,
        AbiV2TypeKind::Enum { variants, .. } if variants.iter().all(|variant| variant.fields.is_empty())
    ))
}

impl<'a, 'db> WrapperMirEmitter<'a, 'db> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        backend: &'a Backend<'db>,
        location: FunctionLocation<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        mir: &'a WrapperMirFunction<'db>,
        semantic_graph: Option<&'a SemanticTypeGraph>,
        direct_function_indices: &'a FxHashMap<InstanceKey<'db>, u32>,
        raw_import_function_indices: &'a FxHashMap<InstanceKey<'db>, u32>,
        runtime_indices: &'a FxHashMap<RuntimeFunction, u32>,
        stage_indices: &'a FxHashMap<StageIntrinsic, u32>,
        helper_indices: &'a FxHashMap<HelperFunction, u32>,
        nominal_destroyers: &'a FxHashMap<u32, u32>,
        nominal_eq_helpers: &'a FxHashMap<u32, u32>,
        array_destroyers: &'a FxHashMap<u32, u32>,
        array_eq_helpers: &'a FxHashMap<u32, u32>,
        callable_type_indices: &'a FxHashMap<FunctionSignature, u32>,
        table_slots: &'a FxHashMap<FunctionValueTarget<'db>, u32>,
        closure_destroyers: &'a [(u32, u32)],
    ) -> Result<Self, Diagnostic> {
        let boundary_abi = match (mir.signature.boundary.as_ref(), semantic_graph) {
            (Some(boundary_signature), Some(semantic_graph)) => {
                let signature_id = mir.signature.signature_id.ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: wrapper MIR boundary signature is missing its ABI id",
                        backend.function_range(location),
                    )
                })?;
                let abi_signature =
                    semantic_graph.signatures.get(signature_id.0 as usize).ok_or_else(|| {
                        Diagnostic::error(
                            format!(
                                "internal error: wrapper MIR referenced unknown ABI signature `{}`",
                                signature_id.0
                            ),
                            backend.function_range(location),
                        )
                    })?;
                let param_runtime_tys = boundary_signature
                    .params
                    .iter()
                    .map(|slot| slot.semantic_ty)
                    .collect::<Vec<_>>();
                let result_runtime_ty = boundary_signature
                    .results
                    .first()
                    .map_or(Ty::new(backend.db, TyKind::Tuple(Vec::new())), |slot| {
                        slot.semantic_ty
                    });
                let function_abi = MitkiFunctionAbi::from_v2_signature_with_runtime_types(
                    backend.db,
                    semantic_graph,
                    abi_signature.id,
                    &param_runtime_tys,
                    result_runtime_ty,
                )
                .map_err(|message| Diagnostic::error(message, backend.function_range(location)))?;
                Some(WrapperMirBoundaryAbi {
                    semantic_graph,
                    boundary_signature,
                    abi_signature,
                    function_abi,
                })
            }
            (Some(_), None) => {
                return Err(Diagnostic::error(
                    "internal error: boundary wrapper MIR emission requires ABI graph context",
                    backend.function_range(location),
                ));
            }
            (None, _) => None,
        };
        Ok(Self {
            backend,
            location,
            source_map,
            mir,
            direct_function_indices,
            raw_import_function_indices,
            runtime_indices,
            stage_indices,
            helper_indices,
            nominal_destroyers,
            nominal_eq_helpers,
            array_destroyers,
            array_eq_helpers,
            callable_type_indices,
            table_slots,
            closure_destroyers,
            layout: WrapperMirLayout::build(mir),
            boundary_abi,
        })
    }

    fn emit(self) -> Result<WasmFunction, Diagnostic> {
        let function_result = wrapper_mir_function_result_abi(&self.mir.signature);
        let mut backend_emitter = backend_ir::BackendEmitter {
            backend: self.backend,
            location: self.location,
            source_map: self.source_map,
            function_indices: self.direct_function_indices,
            runtime_indices: self.runtime_indices,
            stage_indices: self.stage_indices,
            helper_indices: self.helper_indices,
            nominal_destroyers: self.nominal_destroyers,
            nominal_eq_helpers: self.nominal_eq_helpers,
            array_destroyers: self.array_destroyers,
            array_eq_helpers: self.array_eq_helpers,
            callable_type_indices: self.callable_type_indices,
            table_slots: self.table_slots,
            closure_destroyers: self.closure_destroyers,
            resolved_wasm_refs: None,
            function_legalization: None,
            layout: &self.layout.layout,
            function_result: &function_result,
            control_depth: 0,
            loop_stack: Vec::new(),
            scope_stack: Vec::new(),
            return_target: None,
        };
        let mut function = WasmFunction::new(self.layout.layout.wasm_locals().iter().copied());
        self.emit_region(&mut backend_emitter, &mut function, &self.mir.body)?;
        function.instruction(&Instruction::End);
        Ok(function)
    }

    fn place_local(&self, place: PlaceId) -> Result<u32, Diagnostic> {
        self.layout.place_locals.get(&place).copied().ok_or_else(|| {
            Diagnostic::error(
                "internal error: wrapper MIR expected a concrete Wasm local for a place",
                self.backend.function_range(self.location),
            )
        })
    }

    fn place_local_opt(&self, place: PlaceId) -> Option<u32> {
        self.layout.place_locals.get(&place).copied()
    }

    fn value_local(&self, value: ValueId) -> Result<u32, Diagnostic> {
        self.layout.value_locals.get(&value).copied().ok_or_else(|| {
            Diagnostic::error(
                "internal error: wrapper MIR expected a concrete Wasm local for a value",
                self.backend.function_range(self.location),
            )
        })
    }

    fn operand_local(&self, operand: &WrapperOperand) -> Result<u32, Diagnostic> {
        match operand {
            WrapperOperand::Place(place) => self.place_local(*place),
            WrapperOperand::Value(value) => self.value_local(*value),
        }
    }

    fn canonical_locals(&self) -> Result<CanonicalWrapperLocals, Diagnostic> {
        self.layout.canonical_locals.ok_or_else(|| {
            Diagnostic::error(
                "internal error: wrapper MIR operation required canonical scratch locals",
                self.backend.function_range(self.location),
            )
        })
    }

    fn emit_region(
        &self,
        backend_emitter: &mut backend_ir::BackendEmitter<'_, 'db>,
        function: &mut WasmFunction,
        region: &WrapperRegion<'db>,
    ) -> Result<(), Diagnostic> {
        for stmt in &region.stmts {
            match stmt {
                WrapperStmt::Let { value, rhs, .. } => {
                    let local = self.value_local(*value)?;
                    let produced = self.emit_rvalue(backend_emitter, function, rhs)?;
                    if produced.is_none() {
                        return Err(Diagnostic::error(
                            "internal error: wrapper MIR let expected a value-producing rvalue",
                            self.backend.function_range(self.location),
                        ));
                    }
                    function.instruction(&Instruction::LocalSet(local));
                }
                WrapperStmt::Store { place, value } => {
                    let Some(local) = self.place_local_opt(*place) else {
                        continue;
                    };
                    self.emit_operand(function, value)?;
                    function.instruction(&Instruction::LocalSet(local));
                }
                WrapperStmt::If { cond, then_region, else_region, .. } => {
                    self.emit_operand(function, cond)?;
                    function.instruction(&Instruction::If(BlockType::Empty));
                    self.emit_region(backend_emitter, function, then_region)?;
                    function.instruction(&Instruction::Else);
                    self.emit_region(backend_emitter, function, else_region)?;
                    function.instruction(&Instruction::End);
                }
                WrapperStmt::Eval { rhs } => {
                    if self.emit_rvalue(backend_emitter, function, rhs)?.is_some() {
                        function.instruction(&Instruction::Drop);
                    }
                }
                WrapperStmt::Return { values } => {
                    for value in values {
                        self.emit_operand(function, value)?;
                    }
                    function.instruction(&Instruction::Return);
                }
            }
        }
        Ok(())
    }

    fn emit_operand(
        &self,
        function: &mut WasmFunction,
        operand: &WrapperOperand,
    ) -> Result<(), Diagnostic> {
        function.instruction(&Instruction::LocalGet(self.operand_local(operand)?));
        Ok(())
    }

    fn direct_call_target_index(&self, target: &WrapperCallTarget<'db>) -> Result<u32, Diagnostic> {
        match target {
            WrapperCallTarget::DirectFunction(instance) => {
                self.direct_function_indices.get(instance).copied().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing direct function index for wrapper MIR call",
                        self.backend.function_range(self.location),
                    )
                })
            }
            WrapperCallTarget::RawImport(instance) => {
                self.raw_import_function_indices.get(instance).copied().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing raw import function index for wrapper MIR call",
                        self.backend.function_range(self.location),
                    )
                })
            }
        }
    }

    fn resolve_slot(
        &self,
        slot: &super::super::boundary::BoundarySlot<'db>,
        rhs: &WrapperRValue<'db>,
    ) -> Result<WrapperMirResolvedSlot<'_>, Diagnostic> {
        let boundary = self.boundary_abi.as_ref().ok_or_else(|| {
            Diagnostic::error(
                "internal error: wrapper MIR transport op missing boundary ABI context",
                self.backend.function_range(self.location),
            )
        })?;
        let search_results = match (&self.mir.kind, rhs) {
            (
                WrapperKind::ImportThunk { .. },
                WrapperRValue::EncodeImmediate { .. }
                | WrapperRValue::EncodeCanonical { .. }
                | WrapperRValue::WrapHandle { .. },
            ) => false,
            (
                WrapperKind::ImportThunk { .. },
                WrapperRValue::DecodeImmediate { .. }
                | WrapperRValue::DecodeCanonical { .. }
                | WrapperRValue::UnwrapHandle { .. },
            ) => true,
            (
                WrapperKind::ExportWrapper { .. } | WrapperKind::HandleInvokeTrampoline { .. },
                WrapperRValue::DecodeImmediate { .. }
                | WrapperRValue::DecodeCanonical { .. }
                | WrapperRValue::UnwrapHandle { .. },
            ) => false,
            (
                WrapperKind::ExportWrapper { .. } | WrapperKind::HandleInvokeTrampoline { .. },
                WrapperRValue::EncodeImmediate { .. }
                | WrapperRValue::EncodeCanonical { .. }
                | WrapperRValue::WrapHandle { .. },
            ) => true,
            _ => {
                return Err(Diagnostic::error(
                    "internal error: wrapper MIR transport op appeared in an unsupported wrapper \
                     kind",
                    self.backend.function_range(self.location),
                ));
            }
        };

        let index = if search_results {
            boundary.boundary_signature.results.iter().position(|candidate| candidate == slot)
        } else {
            boundary.boundary_signature.params.iter().position(|candidate| candidate == slot)
        }
        .ok_or_else(|| {
            Diagnostic::error(
                "internal error: wrapper MIR transport op drifted from its boundary signature",
                self.backend.function_range(self.location),
            )
        })?;

        if search_results {
            Ok(WrapperMirResolvedSlot {
                transport: &boundary.abi_signature.result,
                value_abi: &boundary.function_abi.result.value,
                runtime_abi: boundary.boundary_signature.internal.results.first().ok_or_else(
                    || {
                        Diagnostic::error(
                            "internal error: wrapper MIR result transport drifted from the \
                             internal signature",
                            self.backend.function_range(self.location),
                        )
                    },
                )?,
            })
        } else {
            Ok(WrapperMirResolvedSlot {
                transport: &boundary.abi_signature.params[index],
                value_abi: &boundary.function_abi.params[index].value,
                runtime_abi: &boundary.boundary_signature.internal.params[index],
            })
        }
    }

    fn emit_rvalue(
        &self,
        backend_emitter: &mut backend_ir::BackendEmitter<'_, 'db>,
        function: &mut WasmFunction,
        rhs: &WrapperRValue<'db>,
    ) -> Result<Option<WrapperPlaceTy>, Diagnostic> {
        match rhs {
            WrapperRValue::ReadParam { param } => {
                function.instruction(&Instruction::LocalGet(*param));
                Ok(Some(wrapper_place_ty_for_param(self.mir, *param)?))
            }
            WrapperRValue::NormalizeBool { operand } => {
                self.emit_operand(function, operand)?;
                function.instruction(&Instruction::I32Const(0));
                function.instruction(&Instruction::I32Ne);
                Ok(Some(WrapperPlaceTy::I32))
            }
            WrapperRValue::LoadPlace { place } => {
                function.instruction(&Instruction::LocalGet(self.place_local(*place)?));
                Ok(Some(
                    self.mir
                        .places
                        .iter()
                        .find(|decl| decl.id == *place)
                        .map(|decl| decl.ty)
                        .ok_or_else(|| {
                            Diagnostic::error(
                                "internal error: wrapper MIR referenced an unknown place",
                                self.backend.function_range(self.location),
                            )
                        })?,
                ))
            }
            WrapperRValue::ReadHandleField { handle, field } => {
                let offset = match field {
                    WrapperHandleField::Slot => ABI_V2_HANDLE_FIELD0_OFFSET,
                    WrapperHandleField::Env => ABI_V2_HANDLE_FIELD1_OFFSET,
                };
                emit_local_get_and_load32(function, self.operand_local(handle)?, offset);
                Ok(Some(WrapperPlaceTy::I32))
            }
            WrapperRValue::CallDirect { target, args, result, wasm_results } => {
                for arg in args {
                    self.emit_operand(function, arg)?;
                }
                function.instruction(&Instruction::Call(self.direct_call_target_index(target)?));
                if let Some(place) = result {
                    function.instruction(&Instruction::LocalSet(self.place_local(*place)?));
                } else if !wasm_results.is_empty() {
                    return Err(Diagnostic::error(
                        "internal error: wrapper MIR direct call dropped a visible result",
                        self.backend.function_range(self.location),
                    ));
                }
                Ok(None)
            }
            WrapperRValue::CallIndirect {
                signature,
                env,
                table_index,
                args,
                result,
                wasm_results,
            } => {
                self.emit_operand(function, env)?;
                for arg in args {
                    self.emit_operand(function, arg)?;
                }
                self.emit_operand(function, table_index)?;
                let callable_type =
                    self.callable_type_indices.get(signature).copied().ok_or_else(|| {
                        Diagnostic::error(
                            "internal error: missing callable type index for wrapper MIR indirect \
                             call",
                            self.backend.function_range(self.location),
                        )
                    })?;
                function.instruction(&Instruction::CallIndirect {
                    type_index: callable_type,
                    table_index: 0,
                });
                if let Some(place) = result {
                    function.instruction(&Instruction::LocalSet(self.place_local(*place)?));
                } else if !wasm_results.is_empty() {
                    return Err(Diagnostic::error(
                        "internal error: wrapper MIR indirect call dropped a visible result",
                        self.backend.function_range(self.location),
                    ));
                }
                Ok(None)
            }
            WrapperRValue::EncodeImmediate { source, dest, slot } => {
                self.emit_encode_immediate(function, *source, *dest, slot)?;
                Ok(None)
            }
            WrapperRValue::DecodeImmediate { source, dest, slot } => {
                self.emit_decode_immediate(function, *source, *dest, slot)?;
                Ok(None)
            }
            WrapperRValue::EncodeCanonical { source, dest, slot } => {
                self.emit_encode_canonical(function, *source, *dest, slot)?;
                Ok(None)
            }
            WrapperRValue::DecodeCanonical { source, dest, slot } => {
                self.emit_decode_canonical(function, *source, *dest, slot)?;
                Ok(None)
            }
            WrapperRValue::WrapHandle { source, dest, slot } => {
                self.emit_wrap_handle(function, backend_emitter, *source, *dest, slot)?;
                Ok(None)
            }
            WrapperRValue::UnwrapHandle { source, dest, slot } => {
                self.emit_unwrap_handle(function, backend_emitter, *source, *dest, slot)?;
                Ok(None)
            }
            WrapperRValue::RetainNestedHandles { .. } => Ok(None),
            WrapperRValue::ReleaseValue { place, abi } => {
                backend_emitter.emit_release_value_from_local(
                    function,
                    self.place_local(*place)?,
                    abi,
                    ExprId::ZERO,
                )?;
                Ok(None)
            }
            WrapperRValue::ReleaseCanonicalBlob { place } => {
                emit_dealloc_canonical_blob(
                    function,
                    self.helper_indices,
                    self.runtime_indices,
                    self.closure_destroyers,
                    self.place_local(*place)?,
                    self.canonical_locals()?,
                )?;
                Ok(None)
            }
            WrapperRValue::ReleaseHandleObject { place } => {
                emit_release_boundary_handle_object(
                    backend_emitter,
                    function,
                    self.runtime_indices,
                    self.place_local(*place)?,
                    self.canonical_locals()?,
                )?;
                Ok(None)
            }
            WrapperRValue::AllocTempBuffer { place, layout } => {
                emit_alloc_temp_buffer(
                    function,
                    self.runtime_indices,
                    self.place_local(*place)?,
                    layout.size,
                    layout.align,
                )?;
                Ok(None)
            }
            WrapperRValue::DeallocTempBuffer { place, layout } => {
                emit_dealloc_temp_buffer(
                    function,
                    self.runtime_indices,
                    self.place_local(*place)?,
                    layout.size,
                    layout.align,
                )?;
                Ok(None)
            }
            WrapperRValue::ZeroTempBuffer { place, size } => {
                emit_zero_local_region(function, self.place_local(*place)?, 0, *size);
                Ok(None)
            }
        }
    }

    fn emit_encode_immediate(
        &self,
        function: &mut WasmFunction,
        source: PlaceId,
        dest: PlaceId,
        slot: &super::super::boundary::BoundarySlot<'db>,
    ) -> Result<(), Diagnostic> {
        let resolved = self.resolve_slot(
            slot,
            &WrapperRValue::EncodeImmediate { source, dest, slot: slot.clone() },
        )?;
        let source_local = self.place_local(source)?;
        let dest_local = self.place_local(dest)?;
        if wrapper_transport_is_nullary_enum(
            self.boundary_abi.as_ref().expect("boundary ABI").semantic_graph,
            resolved.transport,
        )? {
            let locals = self.canonical_locals()?;
            emit_materialize_value_source_base(
                function,
                resolved.value_abi,
                LoweringMode::Runtime,
                internal_value_source(resolved.runtime_abi, source_local),
                locals.temp_ptr,
            )?;
            emit_local_get_and_load32(function, locals.temp_ptr, 0);
            function.instruction(&Instruction::LocalSet(locals.count));
            let enum_layout = enum_layout_for_mode(resolved.value_abi, LoweringMode::Runtime)?;
            emit_enum_tag_to_variant_index(function, enum_layout, locals.count);
            function.instruction(&Instruction::LocalSet(dest_local));
            return Ok(());
        }
        function.instruction(&Instruction::LocalGet(source_local));
        function.instruction(&Instruction::LocalSet(dest_local));
        Ok(())
    }

    fn emit_decode_immediate(
        &self,
        function: &mut WasmFunction,
        source: PlaceId,
        dest: PlaceId,
        slot: &super::super::boundary::BoundarySlot<'db>,
    ) -> Result<(), Diagnostic> {
        let resolved = self.resolve_slot(
            slot,
            &WrapperRValue::DecodeImmediate { source, dest, slot: slot.clone() },
        )?;
        let source_local = self.place_local(source)?;
        let dest_local = self.place_local(dest)?;
        if !wrapper_transport_is_nullary_enum(
            self.boundary_abi.as_ref().expect("boundary ABI").semantic_graph,
            resolved.transport,
        )? {
            function.instruction(&Instruction::LocalGet(source_local));
            function.instruction(&Instruction::LocalSet(dest_local));
            return Ok(());
        }

        let locals = self.canonical_locals()?;
        let enum_layout = enum_layout_for_mode(resolved.value_abi, LoweringMode::Runtime)?;
        function.instruction(&Instruction::LocalGet(source_local));
        function.instruction(&Instruction::LocalSet(locals.count));
        match resolved.runtime_abi {
            AbiTy::Aggregate(layout) => {
                emit_alloc_temp_buffer(
                    function,
                    self.runtime_indices,
                    dest_local,
                    layout.size,
                    layout.align,
                )?;
                emit_zero_local_region(function, dest_local, 0, layout.size);
                emit_enum_variant_index_to_tag(function, enum_layout, locals.count);
                emit_store32_at_local(function, dest_local, 0, locals.count);
            }
            AbiTy::Scalar(BackendTy::Ref(_)) => {
                let layout = aggregate_layout_for_mode(resolved.value_abi, LoweringMode::Runtime)?;
                emit_alloc_nominal_payload_to_local(
                    function,
                    self.runtime_indices,
                    layout,
                    locals.temp_ptr_aux,
                    dest_local,
                )?;
                emit_enum_variant_index_to_tag(function, enum_layout, locals.count);
                emit_store32_at_local(function, dest_local, 0, locals.count);
            }
            _ => {
                return Err(Diagnostic::error(
                    "internal error: wrapper MIR immediate decode expected aggregate or pointer \
                     enum lowering",
                    self.backend.function_range(self.location),
                ));
            }
        }
        Ok(())
    }

    fn emit_encode_canonical(
        &self,
        function: &mut WasmFunction,
        source: PlaceId,
        dest: PlaceId,
        slot: &super::super::boundary::BoundarySlot<'db>,
    ) -> Result<(), Diagnostic> {
        let resolved = self.resolve_slot(
            slot,
            &WrapperRValue::EncodeCanonical { source, dest, slot: slot.clone() },
        )?;
        let boundary = self.boundary_abi.as_ref().expect("boundary ABI");
        let locals = self.canonical_locals()?;
        let source_value = internal_value_source(resolved.runtime_abi, self.place_local(source)?);
        let dest_local = self.place_local(dest)?;
        emit_measure_canonical_value_ref(
            function,
            &boundary.function_abi.type_values,
            resolved.value_abi,
            resolved.transport.semantic_type,
            boundary.semantic_graph,
            LoweringMode::Runtime,
            source_value,
            locals.count,
            locals.bytes,
            locals.handle_count,
            locals,
        )?;
        function.instruction(&Instruction::LocalGet(locals.bytes));
        function.instruction(&Instruction::I32Const(
            (CANONICAL_BLOB_FIXED_HEADER_LEN + CANONICAL_HANDLE_TABLE_LEN_SIZE) as i32,
        ));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalGet(locals.handle_count));
        function.instruction(&Instruction::I32Const(ABI_V2_HANDLE_PAYLOAD_SIZE as i32));
        function.instruction(&Instruction::I32Mul);
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(locals.len));
        emit_dynamic_alloc_to_local(
            function,
            self.runtime_indices,
            locals.len,
            CANONICAL_BLOB_ALIGN,
            dest_local,
        )?;
        emit_write_canonical_blob_header(
            function,
            dest_local,
            transport_type_id(resolved.transport),
            locals.len,
            locals.count,
            locals.handle_count,
        );
        emit_local_plus_offset(function, dest_local, CANONICAL_BLOB_NODE_TABLE_OFFSET);
        function.instruction(&Instruction::LocalSet(locals.cursor));
        function.instruction(&Instruction::LocalGet(dest_local));
        function.instruction(&Instruction::LocalGet(locals.bytes));
        function.instruction(&Instruction::I32Const(CANONICAL_BLOB_FIXED_HEADER_LEN as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(locals.handle_cursor));
        function.instruction(&Instruction::LocalGet(locals.handle_cursor));
        function.instruction(&Instruction::LocalGet(locals.handle_count));
        if matches!(self.mir.kind, WrapperKind::ImportThunk { .. }) {
            function.instruction(&Instruction::I32Store8(memarg(0, 0)));
        } else {
            function.instruction(&Instruction::I32Store(memarg(0, 0)));
        }
        emit_local_plus_offset(function, locals.handle_cursor, CANONICAL_HANDLE_TABLE_LEN_SIZE);
        function.instruction(&Instruction::LocalSet(locals.handle_cursor));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalSet(locals.handle_index));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalSet(locals.base_id));
        emit_encode_canonical_nodes(
            self.helper_indices,
            self.runtime_indices,
            function,
            &boundary.function_abi.type_values,
            resolved.value_abi,
            resolved.transport.semantic_type,
            boundary.semantic_graph,
            LoweringMode::Runtime,
            source_value,
            locals.cursor,
            locals.base_id,
            locals,
        )?;
        Ok(())
    }

    fn emit_decode_canonical(
        &self,
        function: &mut WasmFunction,
        source: PlaceId,
        dest: PlaceId,
        slot: &super::super::boundary::BoundarySlot<'db>,
    ) -> Result<(), Diagnostic> {
        let resolved = self.resolve_slot(
            slot,
            &WrapperRValue::DecodeCanonical { source, dest, slot: slot.clone() },
        )?;
        let boundary = self.boundary_abi.as_ref().expect("boundary ABI");
        let source_local = self.place_local(source)?;
        let dest_local = self.place_local(dest)?;
        let locals = self.canonical_locals()?;
        emit_local_plus_offset(function, source_local, CANONICAL_BLOB_ROOT_OFFSET);
        function.instruction(&Instruction::LocalSet(locals.cursor));
        match resolved.runtime_abi {
            AbiTy::Aggregate(_) => emit_decode_canonical_value_ref(
                function,
                self.helper_indices,
                &boundary.function_abi.type_values,
                resolved.value_abi,
                resolved.transport.semantic_type,
                boundary.semantic_graph,
                LoweringMode::Runtime,
                source_local,
                locals.cursor,
                BoundaryValueDest::Memory { base_local: dest_local, offset: 0 },
                self.runtime_indices,
                locals,
            )?,
            AbiTy::Scalar(BackendTy::Ref(_)) => emit_decode_canonical_value_ref(
                function,
                self.helper_indices,
                &boundary.function_abi.type_values,
                resolved.value_abi,
                resolved.transport.semantic_type,
                boundary.semantic_graph,
                LoweringMode::Runtime,
                source_local,
                locals.cursor,
                BoundaryValueDest::Local(dest_local),
                self.runtime_indices,
                locals,
            )?,
            _ => {
                return Err(Diagnostic::error(
                    "internal error: wrapper MIR canonical decode expected aggregate or pointer \
                     runtime lowering",
                    self.backend.function_range(self.location),
                ));
            }
        }
        Ok(())
    }

    fn emit_wrap_handle(
        &self,
        function: &mut WasmFunction,
        backend_emitter: &mut backend_ir::BackendEmitter<'_, 'db>,
        source: PlaceId,
        dest: PlaceId,
        slot: &super::super::boundary::BoundarySlot<'db>,
    ) -> Result<(), Diagnostic> {
        let resolved = self
            .resolve_slot(slot, &WrapperRValue::WrapHandle { source, dest, slot: slot.clone() })?;
        emit_wrap_runtime_handle_value(
            backend_emitter,
            function,
            self.runtime_indices,
            resolved.value_abi,
            internal_value_source(resolved.runtime_abi, self.place_local(source)?),
            self.place_local(dest)?,
            self.canonical_locals()?,
        )
    }

    fn emit_unwrap_handle(
        &self,
        function: &mut WasmFunction,
        backend_emitter: &mut backend_ir::BackendEmitter<'_, 'db>,
        source: PlaceId,
        dest: PlaceId,
        slot: &super::super::boundary::BoundarySlot<'db>,
    ) -> Result<(), Diagnostic> {
        let resolved = self.resolve_slot(
            slot,
            &WrapperRValue::UnwrapHandle { source, dest, slot: slot.clone() },
        )?;
        let dest_local = self.place_local(dest)?;
        let dest = match resolved.runtime_abi {
            AbiTy::Aggregate(_) => BoundaryValueDest::Memory { base_local: dest_local, offset: 0 },
            AbiTy::Scalar(BackendTy::Ref(_)) => BoundaryValueDest::Local(dest_local),
            _ => {
                return Err(Diagnostic::error(
                    "internal error: wrapper MIR handle decode expected aggregate or pointer \
                     runtime lowering",
                    self.backend.function_range(self.location),
                ));
            }
        };
        emit_unwrap_boundary_handle_value(
            backend_emitter,
            function,
            resolved.value_abi,
            self.place_local(source)?,
            dest,
            self.canonical_locals()?,
        )
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_wrapper_mir<'db>(
    backend: &Backend<'db>,
    location: FunctionLocation<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    mir: &WrapperMirFunction<'db>,
    semantic_graph: Option<&SemanticTypeGraph>,
    direct_function_indices: &FxHashMap<InstanceKey<'db>, u32>,
    raw_import_function_indices: &FxHashMap<InstanceKey<'db>, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    stage_indices: &FxHashMap<StageIntrinsic, u32>,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    nominal_destroyers: &FxHashMap<u32, u32>,
    nominal_eq_helpers: &FxHashMap<u32, u32>,
    array_destroyers: &FxHashMap<u32, u32>,
    array_eq_helpers: &FxHashMap<u32, u32>,
    callable_type_indices: &FxHashMap<FunctionSignature, u32>,
    table_slots: &FxHashMap<FunctionValueTarget<'db>, u32>,
    closure_destroyers: &[(u32, u32)],
) -> Result<WasmFunction, Diagnostic> {
    WrapperMirEmitter::new(
        backend,
        location,
        source_map,
        mir,
        semantic_graph,
        direct_function_indices,
        raw_import_function_indices,
        runtime_indices,
        stage_indices,
        helper_indices,
        nominal_destroyers,
        nominal_eq_helpers,
        array_destroyers,
        array_eq_helpers,
        callable_type_indices,
        table_slots,
        closure_destroyers,
    )?
    .emit()
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_function_value_wrapper<'db>(
    backend: &Backend<'db>,
    location: FunctionLocation<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    mir: &WrapperMirFunction<'db>,
    direct_function_indices: &FxHashMap<InstanceKey<'db>, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    stage_indices: &FxHashMap<StageIntrinsic, u32>,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    nominal_destroyers: &FxHashMap<u32, u32>,
    nominal_eq_helpers: &FxHashMap<u32, u32>,
    array_destroyers: &FxHashMap<u32, u32>,
    array_eq_helpers: &FxHashMap<u32, u32>,
    callable_type_indices: &FxHashMap<FunctionSignature, u32>,
    table_slots: &FxHashMap<FunctionValueTarget<'db>, u32>,
    closure_destroyers: &[(u32, u32)],
) -> Result<WasmFunction, Diagnostic> {
    let raw_import_function_indices = FxHashMap::default();
    emit_wrapper_mir(
        backend,
        location,
        source_map,
        mir,
        None,
        direct_function_indices,
        &raw_import_function_indices,
        runtime_indices,
        stage_indices,
        helper_indices,
        nominal_destroyers,
        nominal_eq_helpers,
        array_destroyers,
        array_eq_helpers,
        callable_type_indices,
        table_slots,
        closure_destroyers,
    )
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_import_thunk_v2<'db>(
    backend: &Backend<'db>,
    location: FunctionLocation<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    mir: &WrapperMirFunction<'db>,
    semantic_graph: &SemanticTypeGraph,
    direct_function_indices: &FxHashMap<InstanceKey<'db>, u32>,
    raw_import_function_indices: &FxHashMap<InstanceKey<'db>, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    stage_indices: &FxHashMap<StageIntrinsic, u32>,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    nominal_destroyers: &FxHashMap<u32, u32>,
    nominal_eq_helpers: &FxHashMap<u32, u32>,
    array_destroyers: &FxHashMap<u32, u32>,
    array_eq_helpers: &FxHashMap<u32, u32>,
    callable_type_indices: &FxHashMap<FunctionSignature, u32>,
    table_slots: &FxHashMap<FunctionValueTarget<'db>, u32>,
    closure_destroyers: &[(u32, u32)],
) -> Result<WasmFunction, Diagnostic> {
    emit_wrapper_mir(
        backend,
        location,
        source_map,
        mir,
        Some(semantic_graph),
        direct_function_indices,
        raw_import_function_indices,
        runtime_indices,
        stage_indices,
        helper_indices,
        nominal_destroyers,
        nominal_eq_helpers,
        array_destroyers,
        array_eq_helpers,
        callable_type_indices,
        table_slots,
        closure_destroyers,
    )
}

pub(super) fn emit_blob_release_helper(
    _helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    _closure_destroyers: &[(u32, u32)],
) -> Result<WasmFunction, Diagnostic> {
    let layout = adapter_layout(1, 0, 0);
    let mut function = WasmFunction::new(layout.wasm_locals().iter().copied());
    let dealloc = runtime_indices.get(&RuntimeFunction::Dealloc).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: missing dealloc runtime import for `mitki:abi/2/blob_release`",
            mitki_errors::TextRange::default(),
        )
    })?;
    function.instruction(&Instruction::LocalGet(0));
    emit_local_get_and_load32(&mut function, 0, CANONICAL_BLOB_TOTAL_LEN_OFFSET);
    function.instruction(&Instruction::I32Const(CANONICAL_BLOB_ALIGN as i32));
    function.instruction(&Instruction::Call(dealloc));
    function.instruction(&Instruction::End);
    Ok(function)
}

pub(super) fn emit_alloc_helper(
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
) -> Result<WasmFunction, Diagnostic> {
    let layout = adapter_layout(2, 0, 0);
    let mut function = WasmFunction::new(layout.wasm_locals().iter().copied());
    let alloc = runtime_indices.get(&RuntimeFunction::Alloc).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: missing alloc runtime import for `mitki:abi/2/alloc`",
            mitki_errors::TextRange::default(),
        )
    })?;
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::LocalGet(1));
    function.instruction(&Instruction::Call(alloc));
    function.instruction(&Instruction::End);
    Ok(function)
}

fn arc_retain_helper_index<'db>(
    emitter: &backend_ir::BackendEmitter<'_, 'db>,
) -> Result<u32, Diagnostic> {
    emitter.helper_indices.get(&HelperFunction::ArcRetain).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: missing ARC retain helper during ABI v2 handle lowering",
            emitter.backend.function_range(emitter.location),
        )
    })
}

fn alloc_runtime_index(
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    message: &'static str,
) -> Result<u32, Diagnostic> {
    runtime_indices
        .get(&RuntimeFunction::Alloc)
        .copied()
        .ok_or_else(|| Diagnostic::error(message, mitki_errors::TextRange::default()))
}

fn dealloc_runtime_index(
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    message: &'static str,
) -> Result<u32, Diagnostic> {
    runtime_indices
        .get(&RuntimeFunction::Dealloc)
        .copied()
        .ok_or_else(|| Diagnostic::error(message, mitki_errors::TextRange::default()))
}

fn arc_retain_helper_index_from_map(
    helper_indices: &FxHashMap<HelperFunction, u32>,
    message: &'static str,
) -> Result<u32, Diagnostic> {
    helper_indices
        .get(&HelperFunction::ArcRetain)
        .copied()
        .ok_or_else(|| Diagnostic::error(message, mitki_errors::TextRange::default()))
}

fn emit_alloc_handle_object_to_local(
    function: &mut WasmFunction,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    kind_tag: i32,
    field0_local: u32,
    field1_local: u32,
    base_local: u32,
    handle_local: u32,
) -> Result<(), Diagnostic> {
    let alloc = alloc_runtime_index(
        runtime_indices,
        "internal error: missing alloc runtime import during ABI v2 handle marshaling",
    )?;
    function.instruction(&Instruction::I32Const(ABI_V2_HANDLE_TOTAL_SIZE as i32));
    function.instruction(&Instruction::I32Const(ARC_ALIGN as i32));
    function.instruction(&Instruction::Call(alloc));
    function.instruction(&Instruction::LocalSet(base_local));
    emit_zero_local_region(function, base_local, 0, ABI_V2_HANDLE_TOTAL_SIZE);
    emit_write_arc_header(function, base_local);
    emit_local_plus_offset(function, base_local, ABI_V2_HANDLE_KIND_OFFSET);
    function.instruction(&Instruction::I32Const(kind_tag));
    function.instruction(&Instruction::I32Store(memarg(0, 2)));
    emit_local_plus_offset(function, base_local, ARC_HEADER_SIZE);
    function.instruction(&Instruction::LocalTee(handle_local));
    function.instruction(&Instruction::LocalGet(field0_local));
    function.instruction(&Instruction::I32Store(memarg(ABI_V2_HANDLE_FIELD0_OFFSET, 2)));
    function.instruction(&Instruction::LocalGet(handle_local));
    function.instruction(&Instruction::LocalGet(field1_local));
    function.instruction(&Instruction::I32Store(memarg(ABI_V2_HANDLE_FIELD1_OFFSET, 2)));
    Ok(())
}

fn emit_wrap_runtime_handle_slot_value(
    function: &mut WasmFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    abi: &MitkiValueAbi,
    source: BoundaryValueSource,
    handle_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    let retain = arc_retain_helper_index_from_map(
        helper_indices,
        "internal error: missing ARC retain helper during nested ABI v2 handle lowering",
    )?;
    match abi.kind {
        MitkiValueKind::Function => {
            emit_materialize_value_source_base(
                function,
                abi,
                LoweringMode::Runtime,
                source,
                locals.temp_ptr,
            )?;
            emit_local_get_and_load32(function, locals.temp_ptr, 0);
            function.instruction(&Instruction::LocalSet(locals.count));
            emit_local_get_and_load32(function, locals.temp_ptr, 4);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            function.instruction(&Instruction::Call(retain));
            emit_alloc_handle_object_to_local(
                function,
                runtime_indices,
                ABI_V2_HANDLE_KIND_FUNCTION,
                locals.count,
                locals.temp_ptr_aux,
                locals.base_id,
                handle_local,
            )
        }
        MitkiValueKind::Opaque => {
            emit_load_i32_from_source(function, source);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            function.instruction(&Instruction::Call(retain));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(locals.count));
            emit_alloc_handle_object_to_local(
                function,
                runtime_indices,
                ABI_V2_HANDLE_KIND_OPAQUE,
                locals.temp_ptr_aux,
                locals.count,
                locals.base_id,
                handle_local,
            )
        }
        _ => Err(Diagnostic::error(
            "internal error: nested ABI v2 handle lowering expected a function or opaque runtime \
             value",
            mitki_errors::TextRange::default(),
        )),
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_write_runtime_handle_slot_ref(
    function: &mut WasmFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    abi: &MitkiValueAbi,
    semantic_type: TypeId,
    source: BoundaryValueSource,
    cursor_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    emit_wrap_runtime_handle_slot_value(
        function,
        helper_indices,
        runtime_indices,
        abi,
        source,
        locals.temp_ptr_aux,
        locals,
    )?;
    emit_local_plus_offset(function, locals.handle_cursor, ABI_V2_HANDLE_FIELD0_OFFSET);
    function.instruction(&Instruction::I32Const(semantic_type.0 as i32));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_local_plus_offset(function, locals.handle_cursor, ABI_V2_HANDLE_FIELD1_OFFSET);
    function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
    function.instruction(&Instruction::I32Store(memarg(0, 0)));
    emit_write_handle_ref_at_cursor(function, cursor_local, locals.handle_index);
    emit_local_plus_offset(function, locals.handle_cursor, ABI_V2_HANDLE_PAYLOAD_SIZE);
    function.instruction(&Instruction::LocalSet(locals.handle_cursor));
    function.instruction(&Instruction::LocalGet(locals.handle_index));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(locals.handle_index));
    Ok(())
}

fn emit_unwrap_boundary_handle_slot_value(
    function: &mut WasmFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    abi: &MitkiValueAbi,
    handle_local: u32,
    dest: BoundaryValueDest,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    let retain = arc_retain_helper_index_from_map(
        helper_indices,
        "internal error: missing ARC retain helper during nested ABI v2 handle decoding",
    )?;
    match abi.kind {
        MitkiValueKind::Function => {
            let BoundaryValueDest::Memory { base_local, offset } = dest else {
                return Err(Diagnostic::error(
                    "internal error: nested ABI v2 function handles must decode into aggregate \
                     storage",
                    mitki_errors::TextRange::default(),
                ));
            };
            emit_local_get_and_load32(function, handle_local, ABI_V2_HANDLE_FIELD0_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.count));
            emit_local_get_and_load32(function, handle_local, ABI_V2_HANDLE_FIELD1_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            function.instruction(&Instruction::Call(retain));
            function.instruction(&Instruction::LocalGet(locals.count));
            emit_store32_at_local(function, base_local, offset, locals.count);
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            emit_store32_at_local(function, base_local, offset + 4, locals.temp_ptr_aux);
            Ok(())
        }
        MitkiValueKind::Opaque => {
            emit_local_get_and_load32(function, handle_local, ABI_V2_HANDLE_FIELD0_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            function.instruction(&Instruction::Call(retain));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            emit_store_i32_to_dest(function, dest, locals.count);
            Ok(())
        }
        _ => Err(Diagnostic::error(
            "internal error: nested ABI v2 handle decoding expected a function or opaque runtime \
             value",
            mitki_errors::TextRange::default(),
        )),
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_release_boundary_handle_object_impl(
    function: &mut WasmFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    closure_destroyers: &[(u32, u32)],
    handle_local: u32,
    base_local: u32,
    kind_local: u32,
    temp_local: u32,
) -> Result<(), Diagnostic> {
    let arc_release =
        helper_indices.get(&HelperFunction::ArcRelease).copied().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing ARC release helper during ABI v2 handle cleanup",
                mitki_errors::TextRange::default(),
            )
        })?;
    let dealloc = dealloc_runtime_index(
        runtime_indices,
        "internal error: missing dealloc runtime import during ABI v2 handle cleanup",
    )?;

    function.instruction(&Instruction::LocalGet(handle_local));
    function.instruction(&Instruction::Call(arc_release));
    function.instruction(&Instruction::If(BlockType::Empty));

    function.instruction(&Instruction::LocalGet(handle_local));
    function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalSet(base_local));

    emit_local_get_and_load32(function, base_local, ABI_V2_HANDLE_KIND_OFFSET);
    function.instruction(&Instruction::LocalSet(kind_local));

    function.instruction(&Instruction::LocalGet(kind_local));
    function.instruction(&Instruction::I32Const(ABI_V2_HANDLE_KIND_FUNCTION));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(BlockType::Empty));

    emit_local_get_and_load32(function, handle_local, ABI_V2_HANDLE_FIELD1_OFFSET);
    function.instruction(&Instruction::LocalSet(temp_local));
    function.instruction(&Instruction::LocalGet(temp_local));
    function.instruction(&Instruction::Call(arc_release));
    function.instruction(&Instruction::If(BlockType::Empty));

    for (slot, destroy_index) in closure_destroyers {
        emit_local_get_and_load32(function, handle_local, ABI_V2_HANDLE_FIELD0_OFFSET);
        function.instruction(&Instruction::I32Const(*slot as i32));
        function.instruction(&Instruction::I32Eq);
        function.instruction(&Instruction::If(BlockType::Empty));
        function.instruction(&Instruction::LocalGet(temp_local));
        function.instruction(&Instruction::Call(*destroy_index));
        function.instruction(&Instruction::End);
    }

    function.instruction(&Instruction::End);
    function.instruction(&Instruction::Else);

    emit_local_get_and_load32(function, handle_local, ABI_V2_HANDLE_FIELD0_OFFSET);
    function.instruction(&Instruction::LocalSet(temp_local));
    function.instruction(&Instruction::LocalGet(temp_local));
    function.instruction(&Instruction::Call(arc_release));
    function.instruction(&Instruction::Drop);

    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(base_local));
    function.instruction(&Instruction::I32Const(ABI_V2_HANDLE_TOTAL_SIZE as i32));
    function.instruction(&Instruction::I32Const(ARC_ALIGN as i32));
    function.instruction(&Instruction::Call(dealloc));

    function.instruction(&Instruction::End);
    Ok(())
}

fn emit_release_boundary_handle_object<'db>(
    emitter: &backend_ir::BackendEmitter<'_, 'db>,
    function: &mut WasmFunction,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    handle_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    emit_release_boundary_handle_object_impl(
        function,
        emitter.helper_indices,
        runtime_indices,
        emitter.closure_destroyers,
        handle_local,
        locals.temp_ptr,
        locals.count,
        locals.temp_ptr_aux,
    )
}

fn opaque_ref_abi() -> AbiTy {
    AbiTy::Scalar(BackendTy::Ref(RefKind::Opaque))
}

fn emit_wrap_runtime_handle_value<'db>(
    emitter: &mut backend_ir::BackendEmitter<'_, 'db>,
    function: &mut WasmFunction,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    abi: &MitkiValueAbi,
    source: BoundaryValueSource,
    handle_local: u32,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    match abi.kind {
        MitkiValueKind::Function => {
            emit_materialize_value_source_base(
                function,
                abi,
                LoweringMode::Runtime,
                source,
                locals.temp_ptr,
            )?;
            emit_local_get_and_load32(function, locals.temp_ptr, 0);
            function.instruction(&Instruction::LocalSet(locals.count));
            emit_local_get_and_load32(function, locals.temp_ptr, 4);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            function.instruction(&Instruction::Call(arc_retain_helper_index(emitter)?));
            emit_alloc_handle_object_to_local(
                function,
                runtime_indices,
                ABI_V2_HANDLE_KIND_FUNCTION,
                locals.count,
                locals.temp_ptr_aux,
                locals.base_id,
                handle_local,
            )
        }
        MitkiValueKind::Opaque => {
            emit_load_i32_from_source(function, source);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            function.instruction(&Instruction::Call(arc_retain_helper_index(emitter)?));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(locals.count));
            emit_alloc_handle_object_to_local(
                function,
                runtime_indices,
                ABI_V2_HANDLE_KIND_OPAQUE,
                locals.temp_ptr_aux,
                locals.count,
                locals.base_id,
                handle_local,
            )
        }
        _ => Err(Diagnostic::error(
            "internal error: ABI v2 capability-handle wrapping expected a function or opaque \
             runtime value",
            emitter.backend.function_range(emitter.location),
        )),
    }
}

fn emit_unwrap_boundary_handle_value<'db>(
    emitter: &mut backend_ir::BackendEmitter<'_, 'db>,
    function: &mut WasmFunction,
    abi: &MitkiValueAbi,
    handle_local: u32,
    dest: BoundaryValueDest,
    locals: CanonicalWrapperLocals,
) -> Result<(), Diagnostic> {
    match abi.kind {
        MitkiValueKind::Function => {
            let BoundaryValueDest::Memory { base_local, offset } = dest else {
                return Err(Diagnostic::error(
                    "internal error: ABI v2 function handles must decode into aggregate storage",
                    emitter.backend.function_range(emitter.location),
                ));
            };
            emit_local_get_and_load32(function, handle_local, ABI_V2_HANDLE_FIELD0_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.count));
            emit_local_get_and_load32(function, handle_local, ABI_V2_HANDLE_FIELD1_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            function.instruction(&Instruction::Call(arc_retain_helper_index(emitter)?));
            function.instruction(&Instruction::LocalGet(locals.count));
            emit_store32_at_local(function, base_local, offset, locals.count);
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            emit_store32_at_local(function, base_local, offset + 4, locals.temp_ptr_aux);
            Ok(())
        }
        MitkiValueKind::Opaque => {
            emit_local_get_and_load32(function, handle_local, ABI_V2_HANDLE_FIELD0_OFFSET);
            function.instruction(&Instruction::LocalSet(locals.temp_ptr_aux));
            emitter.emit_retain_value_from_local(
                function,
                locals.temp_ptr_aux,
                &opaque_ref_abi(),
                ExprId::ZERO,
            )?;
            function.instruction(&Instruction::LocalGet(locals.temp_ptr_aux));
            emit_store_i32_to_dest(function, dest, locals.count);
            Ok(())
        }
        _ => Err(Diagnostic::error(
            "internal error: ABI v2 capability-handle decoding expected a function or opaque \
             runtime value",
            emitter.backend.function_range(emitter.location),
        )),
    }
}

pub(super) fn emit_handle_retain_helper(
    helper_indices: &FxHashMap<HelperFunction, u32>,
) -> Result<WasmFunction, Diagnostic> {
    let layout = adapter_layout(1, 0, 0);
    let mut function = WasmFunction::new(layout.wasm_locals().iter().copied());
    let retain = helper_indices.get(&HelperFunction::ArcRetain).copied().ok_or_else(|| {
        Diagnostic::error(
            "internal error: missing ARC retain helper for `mitki:abi/2/handle_retain`",
            mitki_errors::TextRange::default(),
        )
    })?;
    let scratch = layout
        .scratch_i32_local()
        .expect("handle retain helper should reserve an i32 scratch local");
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::LocalTee(scratch));
    function.instruction(&Instruction::Call(retain));
    function.instruction(&Instruction::LocalGet(scratch));
    function.instruction(&Instruction::End);
    Ok(function)
}

pub(super) fn emit_handle_release_helper(
    helper_indices: &FxHashMap<HelperFunction, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    closure_destroyers: &[(u32, u32)],
) -> Result<WasmFunction, Diagnostic> {
    let layout = adapter_layout(1, 0, 0);
    let mut function = WasmFunction::new(layout.wasm_locals().iter().copied());
    let base_local =
        layout.scratch_i32_local().expect("handle release helper should reserve a scratch local");
    let kind_local = layout
        .scratch_i32_aux_local()
        .expect("handle release helper should reserve an aux scratch local");
    let temp_local = layout
        .object_i32_local()
        .expect("handle release helper should reserve an object scratch local");
    emit_release_boundary_handle_object_impl(
        &mut function,
        helper_indices,
        runtime_indices,
        closure_destroyers,
        0,
        base_local,
        kind_local,
        temp_local,
    )?;
    function.instruction(&Instruction::End);
    Ok(function)
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_handle_invoke_trampoline_v2<'db>(
    backend: &Backend<'db>,
    location: FunctionLocation<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    mir: &WrapperMirFunction<'db>,
    semantic_graph: &SemanticTypeGraph,
    direct_function_indices: &FxHashMap<InstanceKey<'db>, u32>,
    raw_import_function_indices: &FxHashMap<InstanceKey<'db>, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    stage_indices: &FxHashMap<StageIntrinsic, u32>,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    nominal_destroyers: &FxHashMap<u32, u32>,
    nominal_eq_helpers: &FxHashMap<u32, u32>,
    array_destroyers: &FxHashMap<u32, u32>,
    array_eq_helpers: &FxHashMap<u32, u32>,
    callable_type_indices: &FxHashMap<FunctionSignature, u32>,
    table_slots: &FxHashMap<FunctionValueTarget<'db>, u32>,
    closure_destroyers: &[(u32, u32)],
) -> Result<WasmFunction, Diagnostic> {
    emit_wrapper_mir(
        backend,
        location,
        source_map,
        mir,
        Some(semantic_graph),
        direct_function_indices,
        raw_import_function_indices,
        runtime_indices,
        stage_indices,
        helper_indices,
        nominal_destroyers,
        nominal_eq_helpers,
        array_destroyers,
        array_eq_helpers,
        callable_type_indices,
        table_slots,
        closure_destroyers,
    )
}

#[allow(clippy::too_many_arguments)]
pub(super) fn emit_export_wrapper_v2<'db>(
    backend: &Backend<'db>,
    location: FunctionLocation<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    mir: &WrapperMirFunction<'db>,
    semantic_graph: &SemanticTypeGraph,
    direct_function_indices: &FxHashMap<InstanceKey<'db>, u32>,
    raw_import_function_indices: &FxHashMap<InstanceKey<'db>, u32>,
    runtime_indices: &FxHashMap<RuntimeFunction, u32>,
    stage_indices: &FxHashMap<StageIntrinsic, u32>,
    helper_indices: &FxHashMap<HelperFunction, u32>,
    nominal_destroyers: &FxHashMap<u32, u32>,
    nominal_eq_helpers: &FxHashMap<u32, u32>,
    array_destroyers: &FxHashMap<u32, u32>,
    array_eq_helpers: &FxHashMap<u32, u32>,
    callable_type_indices: &FxHashMap<FunctionSignature, u32>,
    table_slots: &FxHashMap<FunctionValueTarget<'db>, u32>,
    closure_destroyers: &[(u32, u32)],
) -> Result<WasmFunction, Diagnostic> {
    emit_wrapper_mir(
        backend,
        location,
        source_map,
        mir,
        Some(semantic_graph),
        direct_function_indices,
        raw_import_function_indices,
        runtime_indices,
        stage_indices,
        helper_indices,
        nominal_destroyers,
        nominal_eq_helpers,
        array_destroyers,
        array_eq_helpers,
        callable_type_indices,
        table_slots,
        closure_destroyers,
    )
}
