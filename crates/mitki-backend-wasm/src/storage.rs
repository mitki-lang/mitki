use std::ops::Deref;

use rustc_hash::FxHashMap;
use wasm_encoder::{Function as WasmFunction, Instruction, MemArg, ValType};

use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct FrameSlotId(pub(super) u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum FrameSlotPurpose {
    Binding(NameId),
    Temp(ExprId),
    StackAlloc(ExprId),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct FrameSlot {
    pub(super) id: FrameSlotId,
    pub(super) purpose: FrameSlotPurpose,
    pub(super) size: u32,
    pub(super) align: u32,
    pub(super) offset: u32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct FramePlan {
    pub(super) slots: Vec<FrameSlot>,
    pub(super) size: u32,
}

impl FramePlan {
    pub(super) fn slot(&self, id: FrameSlotId) -> Option<&FrameSlot> {
        self.slots.get(id.0 as usize)
    }

    pub(super) fn offset(&self, id: FrameSlotId) -> Option<u32> {
        self.slot(id).map(|slot| slot.offset)
    }
}

#[derive(Clone, Debug, Default)]
pub(super) struct FramePlanBuilder {
    slots: Vec<FrameSlot>,
    size: u32,
}

impl FramePlanBuilder {
    pub(super) fn alloc_slot(
        &mut self,
        purpose: FrameSlotPurpose,
        size: u32,
        align: u32,
    ) -> FrameSlotId {
        let align = align.max(1);
        let offset = align_to(self.size, align);
        let id = FrameSlotId(self.slots.len() as u32);
        self.size = offset + size;
        self.slots.push(FrameSlot { id, purpose, size, align, offset });
        id
    }

    pub(super) fn finish(self) -> FramePlan {
        FramePlan { slots: self.slots, size: self.size }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum ScratchLocalKind {
    ScratchI32,
    ScratchI32Aux,
    ObjectI32,
    ScratchF64,
    ScratchI64,
    FrameBase,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum LocalPurpose {
    EnvPtrParam,
    ResultPtrParam,
    RawParam {
        ordinal: usize,
    },
    UserBinding {
        name: NameId,
    },
    PatternSource {
        expr: ExprId,
    },
    NominalTemp {
        expr: ExprId,
    },
    ArrayRepeatTemp {
        expr: ExprId,
    },
    AdapterTemp {
        ordinal: u32,
    },
    #[allow(dead_code)]
    ResultJoinI32,
    Scratch(ScratchLocalKind),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct PlannedLocal {
    pub(super) purpose: LocalPurpose,
    pub(super) abi: Option<AbiTy>,
    pub(super) local_index: Option<u32>,
    pub(super) value_type: Option<ValType>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct LocalPlan {
    pub(super) params: Vec<PlannedLocal>,
    pub(super) user_locals: Vec<PlannedLocal>,
    pub(super) spills: Vec<PlannedLocal>,
    pub(super) joins: Vec<PlannedLocal>,
    pub(super) scratch: Vec<PlannedLocal>,
    pub(super) allocation_order: Vec<PlannedLocal>,
    wasm_locals: Vec<(u32, ValType)>,
}

impl LocalPlan {
    pub(super) fn wasm_locals(&self) -> &[(u32, ValType)] {
        &self.wasm_locals
    }

    pub(super) fn env_ptr_local(&self) -> Option<u32> {
        self.params
            .iter()
            .find(|local| matches!(local.purpose, LocalPurpose::EnvPtrParam))
            .and_then(|local| local.local_index)
    }

    pub(super) fn result_ptr_local(&self) -> Option<u32> {
        self.params
            .iter()
            .find(|local| matches!(local.purpose, LocalPurpose::ResultPtrParam))
            .and_then(|local| local.local_index)
    }

    pub(super) fn scratch_local(&self, kind: ScratchLocalKind) -> Option<u32> {
        self.scratch
            .iter()
            .find(|local| matches!(local.purpose, LocalPurpose::Scratch(found) if found == kind))
            .and_then(|local| local.local_index)
    }

    pub(super) fn contains_local_index(&self, index: u32) -> bool {
        self.params
            .iter()
            .chain(self.user_locals.iter())
            .chain(self.spills.iter())
            .chain(self.joins.iter())
            .chain(self.scratch.iter())
            .filter_map(|local| local.local_index)
            .any(|local_index| local_index == index)
    }

    pub(super) fn next_local_index(&self) -> u32 {
        self.params
            .iter()
            .chain(self.user_locals.iter())
            .chain(self.spills.iter())
            .chain(self.joins.iter())
            .chain(self.scratch.iter())
            .filter_map(|local| local.local_index)
            .max()
            .map_or(0, |index| index + 1)
    }
}

#[derive(Clone, Debug)]
pub(super) struct LocalPlanBuilder {
    next_index: u32,
    params: Vec<PlannedLocal>,
    user_locals: Vec<PlannedLocal>,
    spills: Vec<PlannedLocal>,
    joins: Vec<PlannedLocal>,
    scratch: Vec<PlannedLocal>,
    allocation_order: Vec<PlannedLocal>,
    wasm_locals: Vec<(u32, ValType)>,
}

impl LocalPlanBuilder {
    pub(super) fn new(next_index: u32) -> Self {
        Self {
            next_index,
            params: Vec::new(),
            user_locals: Vec::new(),
            spills: Vec::new(),
            joins: Vec::new(),
            scratch: Vec::new(),
            allocation_order: Vec::new(),
            wasm_locals: Vec::new(),
        }
    }

    pub(super) fn add_param(
        &mut self,
        purpose: LocalPurpose,
        abi: Option<AbiTy>,
        value_type: Option<ValType>,
    ) -> Option<u32> {
        let local_index = value_type.map(|_| self.reserve_param_index());
        self.params.push(PlannedLocal { purpose, abi, local_index, value_type });
        local_index
    }

    pub(super) fn alloc_user_local(
        &mut self,
        purpose: LocalPurpose,
        abi: Option<AbiTy>,
        value_type: ValType,
    ) -> u32 {
        let local_index = self.alloc_local_index(value_type);
        let local = PlannedLocal {
            purpose,
            abi,
            local_index: Some(local_index),
            value_type: Some(value_type),
        };
        self.user_locals.push(local.clone());
        self.allocation_order.push(local);
        local_index
    }

    pub(super) fn alloc_spill(
        &mut self,
        purpose: LocalPurpose,
        abi: Option<AbiTy>,
        value_type: ValType,
    ) -> u32 {
        let local_index = self.alloc_local_index(value_type);
        let local = PlannedLocal {
            purpose,
            abi,
            local_index: Some(local_index),
            value_type: Some(value_type),
        };
        self.spills.push(local.clone());
        self.allocation_order.push(local);
        local_index
    }

    pub(super) fn alloc_join(
        &mut self,
        purpose: LocalPurpose,
        abi: Option<AbiTy>,
        value_type: ValType,
    ) -> u32 {
        let local_index = self.alloc_local_index(value_type);
        let local = PlannedLocal {
            purpose,
            abi,
            local_index: Some(local_index),
            value_type: Some(value_type),
        };
        self.joins.push(local.clone());
        self.allocation_order.push(local);
        local_index
    }

    pub(super) fn alloc_scratch(&mut self, kind: ScratchLocalKind, value_type: ValType) -> u32 {
        let local_index = self.alloc_local_index(value_type);
        let local = PlannedLocal {
            purpose: LocalPurpose::Scratch(kind),
            abi: None,
            local_index: Some(local_index),
            value_type: Some(value_type),
        };
        self.scratch.push(local.clone());
        self.allocation_order.push(local);
        local_index
    }

    pub(super) fn finish(self) -> LocalPlan {
        LocalPlan {
            params: self.params,
            user_locals: self.user_locals,
            spills: self.spills,
            joins: self.joins,
            scratch: self.scratch,
            allocation_order: self.allocation_order,
            wasm_locals: self.wasm_locals,
        }
    }

    fn reserve_param_index(&mut self) -> u32 {
        let index = self.next_index;
        self.next_index += 1;
        index
    }

    fn alloc_local_index(&mut self, value_type: ValType) -> u32 {
        let index = self.reserve_param_index();
        match self.wasm_locals.last_mut() {
            Some((count, existing)) if *existing == value_type => *count += 1,
            _ => self.wasm_locals.push((1, value_type)),
        }
        index
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct LocalSlot {
    pub(super) abi: AbiTy,
    pub(super) local_index: Option<u32>,
    pub(super) frame_slot: Option<FrameSlotId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct TempSlot {
    pub(super) frame_slot: FrameSlotId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum DestBase {
    PointerLocal(u32),
    FrameSlot(FrameSlotId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Dest {
    pub(super) base: DestBase,
    pub(super) offset: u32,
}

impl Dest {
    pub(super) fn pointer_local(local: u32) -> Self {
        Self { base: DestBase::PointerLocal(local), offset: 0 }
    }

    pub(super) fn frame_slot(frame_slot: FrameSlotId) -> Self {
        Self { base: DestBase::FrameSlot(frame_slot), offset: 0 }
    }

    pub(super) fn with_offset(self, offset: u32) -> Self {
        Self { base: self.base, offset: self.offset + offset }
    }
}

#[derive(Clone, Debug)]
pub(super) struct FunctionLayout {
    pub(super) local_plan: LocalPlan,
    pub(super) frame_plan: FramePlan,
    pub(super) lookups: FunctionLayoutLookups,
}

#[derive(Clone, Debug)]
pub(super) struct FunctionLayoutLookups {
    pub(super) slots: FxHashMap<NameId, LocalSlot>,
    pub(super) param_names: Vec<NameId>,
    pub(super) raw_params: Vec<LocalSlot>,
    pub(super) temps: FxHashMap<ExprId, TempSlot>,
    pub(super) pattern_scalar_locals: FxHashMap<ExprId, u32>,
    pub(super) nominal_locals: FxHashMap<ExprId, u32>,
    pub(super) array_repeat_locals: FxHashMap<ExprId, u32>,
}

impl FunctionLayout {
    pub(super) fn new(
        local_plan: LocalPlan,
        frame_plan: FramePlan,
        lookups: FunctionLayoutLookups,
    ) -> Self {
        Self { local_plan, frame_plan, lookups }
    }

    pub(super) fn wasm_locals(&self) -> &[(u32, ValType)] {
        self.local_plan.wasm_locals()
    }

    pub(super) fn env_ptr_local(&self) -> Option<u32> {
        self.local_plan.env_ptr_local()
    }

    pub(super) fn result_ptr_local(&self) -> Option<u32> {
        self.local_plan.result_ptr_local()
    }

    pub(super) fn scratch_i32_local(&self) -> Option<u32> {
        self.local_plan.scratch_local(ScratchLocalKind::ScratchI32)
    }

    pub(super) fn scratch_i32_aux_local(&self) -> Option<u32> {
        self.local_plan.scratch_local(ScratchLocalKind::ScratchI32Aux)
    }

    pub(super) fn object_i32_local(&self) -> Option<u32> {
        self.local_plan.scratch_local(ScratchLocalKind::ObjectI32)
    }

    pub(super) fn scratch_f64_local(&self) -> Option<u32> {
        self.local_plan.scratch_local(ScratchLocalKind::ScratchF64)
    }

    pub(super) fn scratch_i64_local(&self) -> Option<u32> {
        self.local_plan.scratch_local(ScratchLocalKind::ScratchI64)
    }

    pub(super) fn frame_base_local(&self) -> Option<u32> {
        self.local_plan.scratch_local(ScratchLocalKind::FrameBase)
    }

    pub(super) fn frame_size(&self) -> u32 {
        self.frame_plan.size
    }

    pub(super) fn frame_slot_offset(&self, slot: FrameSlotId) -> Option<u32> {
        self.frame_plan.offset(slot)
    }

    pub(super) fn contains_local_index(&self, index: u32) -> bool {
        self.local_plan.contains_local_index(index)
    }

    pub(super) fn next_local_index(&self) -> u32 {
        self.local_plan.next_local_index()
    }

    #[cfg(test)]
    pub(super) fn dump_storage(&self) -> String {
        use std::fmt::Write as _;

        let mut output = String::new();
        dump_locals(&mut output, "params", &self.local_plan.params);
        dump_locals(&mut output, "user_locals", &self.local_plan.user_locals);
        dump_locals(&mut output, "spills", &self.local_plan.spills);
        dump_locals(&mut output, "joins", &self.local_plan.joins);
        dump_locals(&mut output, "scratch", &self.local_plan.scratch);
        writeln!(&mut output, "frame.size: {}", self.frame_plan.size).expect("write string");
        writeln!(&mut output, "frame.slots:").expect("write string");
        for slot in &self.frame_plan.slots {
            writeln!(
                &mut output,
                "  - {:?} size={} align={} offset={}",
                slot.purpose, slot.size, slot.align, slot.offset
            )
            .expect("write string");
        }
        output
    }
}

impl Deref for FunctionLayout {
    type Target = FunctionLayoutLookups;

    fn deref(&self) -> &Self::Target {
        &self.lookups
    }
}

#[cfg(test)]
fn dump_locals(output: &mut String, label: &str, locals: &[PlannedLocal]) {
    use std::fmt::Write as _;

    writeln!(output, "{label}:").expect("write string");
    for local in locals {
        writeln!(
            output,
            "  - {:?} local={:?} type={:?} abi={:?}",
            local.purpose, local.local_index, local.value_type, local.abi
        )
        .expect("write string");
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum MemAccessKind {
    Byte,
    I32,
    I64,
    F64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct MemAccess {
    pub(super) offset: u32,
    pub(super) align_log2: u32,
    pub(super) kind: MemAccessKind,
}

impl MemAccess {
    pub(super) fn byte(offset: u32) -> Self {
        Self { offset, align_log2: 0, kind: MemAccessKind::Byte }
    }

    pub(super) fn i32(offset: u32, align_log2: u32) -> Self {
        Self { offset, align_log2, kind: MemAccessKind::I32 }
    }

    pub(super) fn i64(offset: u32) -> Self {
        Self { offset, align_log2: 3, kind: MemAccessKind::I64 }
    }

    pub(super) fn f64(offset: u32) -> Self {
        Self { offset, align_log2: 3, kind: MemAccessKind::F64 }
    }

    pub(super) fn scalar(offset: u32, ty: BackendTy) -> Option<Self> {
        match ty {
            BackendTy::Int | BackendTy::Bool | BackendTy::Char | BackendTy::Ref(_) => {
                Some(Self::i32(offset, 2))
            }
            BackendTy::I64 => Some(Self::i64(offset)),
            BackendTy::Float => Some(Self::f64(offset)),
            BackendTy::Unit => None,
        }
    }

    pub(super) fn array_len() -> Self {
        Self::i32(ARRAY_LEN_OFFSET, 2)
    }

    pub(super) fn array_capacity() -> Self {
        Self::i32(ARRAY_CAPACITY_OFFSET, 2)
    }

    pub(super) fn arc_ref_count() -> Self {
        Self::i32(0, 2)
    }

    pub(super) fn arc_type_bits() -> Self {
        Self::i32(4, 2)
    }

    pub(super) fn enum_tag(offset: u32) -> Self {
        Self::i32(offset, 2)
    }

    pub(super) fn function_word(offset: u32) -> Self {
        Self::i32(offset, 2)
    }

    pub(super) fn memarg(self) -> MemArg {
        MemArg { offset: self.offset.into(), align: self.align_log2, memory_index: 0 }
    }

    pub(super) fn emit_load(self, function: &mut WasmFunction) {
        match self.kind {
            MemAccessKind::Byte => {
                function.instruction(&Instruction::I32Load8U(self.memarg()));
            }
            MemAccessKind::I32 => {
                function.instruction(&Instruction::I32Load(self.memarg()));
            }
            MemAccessKind::I64 => {
                function.instruction(&Instruction::I64Load(self.memarg()));
            }
            MemAccessKind::F64 => {
                function.instruction(&Instruction::F64Load(self.memarg()));
            }
        }
    }

    pub(super) fn emit_store(self, function: &mut WasmFunction) {
        match self.kind {
            MemAccessKind::Byte => {
                function.instruction(&Instruction::I32Store8(self.memarg()));
            }
            MemAccessKind::I32 => {
                function.instruction(&Instruction::I32Store(self.memarg()));
            }
            MemAccessKind::I64 => {
                function.instruction(&Instruction::I64Store(self.memarg()));
            }
            MemAccessKind::F64 => {
                function.instruction(&Instruction::F64Store(self.memarg()));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_plan_builder_tracks_categories_and_stable_order() {
        let mut builder = LocalPlanBuilder::new(2);
        builder.add_param(
            LocalPurpose::RawParam { ordinal: 0 },
            Some(AbiTy::Scalar(BackendTy::Int)),
            Some(ValType::I32),
        );
        builder.alloc_user_local(
            LocalPurpose::UserBinding { name: NameId::ZERO },
            Some(AbiTy::Scalar(BackendTy::Bool)),
            ValType::I32,
        );
        builder.alloc_spill(
            LocalPurpose::PatternSource { expr: ExprId::ZERO },
            Some(AbiTy::Scalar(BackendTy::Int)),
            ValType::I32,
        );
        builder.alloc_join(LocalPurpose::ResultJoinI32, None, ValType::I32);
        builder.alloc_scratch(ScratchLocalKind::ScratchI32, ValType::I32);

        let plan = builder.finish();
        assert_eq!(plan.params.len(), 1);
        assert_eq!(plan.user_locals.len(), 1);
        assert_eq!(plan.spills.len(), 1);
        assert_eq!(plan.joins.len(), 1);
        assert_eq!(plan.scratch.len(), 1);
        assert_eq!(
            plan.wasm_locals(),
            &[(4, ValType::I32)],
            "non-param locals should preserve stable grouped ordering"
        );
    }

    #[test]
    fn frame_plan_builder_assigns_aligned_non_overlapping_slots() {
        let mut builder = FramePlanBuilder::default();
        let first = builder.alloc_slot(FrameSlotPurpose::Temp(ExprId::ZERO), 4, 4);
        let second = builder.alloc_slot(FrameSlotPurpose::StackAlloc(ExprId::ZERO), 8, 8);
        let third = builder.alloc_slot(FrameSlotPurpose::Binding(NameId::ZERO), 4, 4);
        let plan = builder.finish();

        assert_eq!(plan.offset(first), Some(0));
        assert_eq!(plan.offset(second), Some(8));
        assert_eq!(plan.offset(third), Some(16));
        assert_eq!(plan.size, 20);
    }

    #[test]
    fn mem_access_derives_scalar_and_runtime_layout_accesses() {
        assert_eq!(
            MemAccess::scalar(12, BackendTy::Int),
            Some(MemAccess { offset: 12, align_log2: 2, kind: MemAccessKind::I32 })
        );
        assert_eq!(
            MemAccess::scalar(24, BackendTy::Float),
            Some(MemAccess { offset: 24, align_log2: 3, kind: MemAccessKind::F64 })
        );
        assert_eq!(MemAccess::array_len(), MemAccess::i32(ARRAY_LEN_OFFSET, 2));
        assert_eq!(MemAccess::array_capacity(), MemAccess::i32(ARRAY_CAPACITY_OFFSET, 2));
        assert_eq!(MemAccess::arc_ref_count(), MemAccess::i32(0, 2));
        assert_eq!(MemAccess::function_word(4), MemAccess::i32(4, 2));
        assert_eq!(MemAccess::byte(0).kind, MemAccessKind::Byte);
    }
}
