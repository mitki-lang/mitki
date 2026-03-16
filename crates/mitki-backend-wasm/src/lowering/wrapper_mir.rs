#[cfg(test)]
use std::fmt::Write as _;
#[cfg(test)]
use std::sync::Arc;

use mitki_abi::{FunctionSignature as AbiV2FunctionSignature, SigId, TransportClass};
use mitki_errors::Diagnostic;
use mitki_hir::ty::{Ty, TyKind};
use rustc_hash::FxHashMap;
use wasm_encoder::ValType;

use super::super::boundary::{BoundarySig, BoundarySlot, InternalSig, transport_has_wasm_lane};
use super::plan::ModulePlan;
use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::backend) struct WrapperMirId(pub(in crate::backend) u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::backend) struct PlaceId(pub(in crate::backend) u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::backend) struct ValueId(pub(in crate::backend) u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct WrapperMirBundle<'db> {
    pub(in crate::backend) imports: Vec<WrapperMirFunction<'db>>,
    pub(in crate::backend) callable_adapters: Vec<WrapperMirFunction<'db>>,
    pub(in crate::backend) trampolines: Vec<WrapperMirFunction<'db>>,
    pub(in crate::backend) exports: Vec<WrapperMirFunction<'db>>,
    pub(in crate::backend) import_indices: FxHashMap<InstanceKey<'db>, usize>,
    pub(in crate::backend) callable_adapter_indices: FxHashMap<InstanceKey<'db>, usize>,
    pub(in crate::backend) trampoline_indices: FxHashMap<SigId, usize>,
    pub(in crate::backend) export_indices: FxHashMap<InstanceKey<'db>, usize>,
}

impl<'db> WrapperMirBundle<'db> {
    pub(in crate::backend) fn import(
        &self,
        instance: &InstanceKey<'db>,
    ) -> Option<&WrapperMirFunction<'db>> {
        self.import_indices.get(instance).and_then(|&index| self.imports.get(index))
    }

    pub(in crate::backend) fn callable_adapter(
        &self,
        instance: &InstanceKey<'db>,
    ) -> Option<&WrapperMirFunction<'db>> {
        self.callable_adapter_indices
            .get(instance)
            .and_then(|&index| self.callable_adapters.get(index))
    }

    pub(in crate::backend) fn trampoline(
        &self,
        signature_id: SigId,
    ) -> Option<&WrapperMirFunction<'db>> {
        self.trampoline_indices.get(&signature_id).and_then(|&index| self.trampolines.get(index))
    }

    pub(in crate::backend) fn export(
        &self,
        instance: &InstanceKey<'db>,
    ) -> Option<&WrapperMirFunction<'db>> {
        self.export_indices.get(instance).and_then(|&index| self.exports.get(index))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct WrapperMirFunction<'db> {
    pub(in crate::backend) id: WrapperMirId,
    pub(in crate::backend) kind: WrapperKind<'db>,
    pub(in crate::backend) debug_name: String,
    pub(in crate::backend) signature: WrapperMirSignature<'db>,
    pub(in crate::backend) places: Vec<PlaceDecl>,
    pub(in crate::backend) body: Region<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) enum WrapperKind<'db> {
    ImportThunk {
        instance: InstanceKey<'db>,
        metadata_index: usize,
    },
    ExportWrapper {
        instance: InstanceKey<'db>,
        metadata_index: usize,
    },
    HandleInvokeTrampoline {
        instance: InstanceKey<'db>,
        metadata_index: usize,
        signature_id: SigId,
    },
    FunctionValueWrapper {
        instance: InstanceKey<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct WrapperMirSignature<'db> {
    pub(in crate::backend) wasm_params: Vec<ValType>,
    pub(in crate::backend) wasm_results: Vec<ValType>,
    pub(in crate::backend) internal: Option<InternalSig>,
    pub(in crate::backend) boundary: Option<BoundarySig<'db>>,
    pub(in crate::backend) callable: Option<FunctionSignature>,
    pub(in crate::backend) metadata_index: Option<usize>,
    pub(in crate::backend) signature_id: Option<SigId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::backend) enum WrapperPlaceTy {
    I32,
    I64,
    F64,
    Unit,
}

impl WrapperPlaceTy {
    pub(in crate::backend) fn value_type(self) -> Option<ValType> {
        match self {
            Self::I32 => Some(ValType::I32),
            Self::I64 => Some(ValType::I64),
            Self::F64 => Some(ValType::F64),
            Self::Unit => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) enum WrapperScratchKind {
    NodeOffset,
    Cursor,
    Index,
    Len,
    Count,
    Bytes,
    BaseId,
    TempPtr,
    TempPtrAux,
    ChildCount,
    ChildBytes,
    HandleCount,
    HandleIndex,
    HandleCursor,
    F64Temp,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) enum PlaceKind {
    Local,
    Scratch(WrapperScratchKind),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct PlaceDecl {
    pub(in crate::backend) id: PlaceId,
    pub(in crate::backend) label: String,
    pub(in crate::backend) ty: WrapperPlaceTy,
    pub(in crate::backend) kind: PlaceKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct Region<'db> {
    pub(in crate::backend) stmts: Vec<Stmt<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) enum Stmt<'db> {
    Let {
        value: ValueId,
        ty: WrapperPlaceTy,
        rhs: RValue<'db>,
    },
    Store {
        place: PlaceId,
        value: Operand,
    },
    #[allow(dead_code)]
    If {
        cond: Operand,
        then_region: Region<'db>,
        else_region: Region<'db>,
        result_places: Vec<PlaceId>,
    },
    Eval {
        rhs: RValue<'db>,
    },
    Return {
        values: Vec<Operand>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::backend) enum WrapperHandleField {
    Slot,
    Env,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) enum Operand {
    Value(ValueId),
    Place(PlaceId),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) enum WrapperCallTarget<'db> {
    DirectFunction(InstanceKey<'db>),
    RawImport(InstanceKey<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) enum RValue<'db> {
    ReadParam {
        param: u32,
    },
    NormalizeBool {
        operand: Operand,
    },
    #[allow(dead_code)]
    LoadPlace {
        place: PlaceId,
    },
    CallDirect {
        target: WrapperCallTarget<'db>,
        wasm_results: Vec<WrapperPlaceTy>,
        args: Vec<Operand>,
        result: Option<PlaceId>,
    },
    CallIndirect {
        signature: FunctionSignature,
        wasm_results: Vec<WrapperPlaceTy>,
        env: Operand,
        table_index: Operand,
        args: Vec<Operand>,
        result: Option<PlaceId>,
    },
    EncodeImmediate {
        source: PlaceId,
        dest: PlaceId,
        slot: BoundarySlot<'db>,
    },
    DecodeImmediate {
        source: PlaceId,
        dest: PlaceId,
        slot: BoundarySlot<'db>,
    },
    EncodeCanonical {
        source: PlaceId,
        dest: PlaceId,
        slot: BoundarySlot<'db>,
    },
    DecodeCanonical {
        source: PlaceId,
        dest: PlaceId,
        slot: BoundarySlot<'db>,
    },
    WrapHandle {
        source: PlaceId,
        dest: PlaceId,
        slot: BoundarySlot<'db>,
    },
    UnwrapHandle {
        source: PlaceId,
        dest: PlaceId,
        slot: BoundarySlot<'db>,
    },
    RetainNestedHandles {
        place: PlaceId,
        ty: Ty<'db>,
    },
    ReleaseValue {
        place: PlaceId,
        abi: AbiTy,
    },
    ReleaseCanonicalBlob {
        place: PlaceId,
    },
    ReleaseHandleObject {
        place: PlaceId,
    },
    AllocTempBuffer {
        place: PlaceId,
        layout: AggregateLayout,
    },
    DeallocTempBuffer {
        place: PlaceId,
        layout: AggregateLayout,
    },
    ZeroTempBuffer {
        place: PlaceId,
        size: u32,
    },
    ReadHandleField {
        handle: Operand,
        field: WrapperHandleField,
    },
}

pub(in crate::backend) struct WrapperMirBuilder;
pub(in crate::backend) struct WrapperMirValidator;

pub(in crate::backend) fn abi_v2_wasm_signature(
    graph: &mitki_abi::SemanticTypeGraph,
    signature: &AbiV2FunctionSignature,
) -> Result<(Vec<ValType>, Vec<ValType>), Diagnostic> {
    let shape = mitki_abi::typed_signature_wasm_shape(graph, signature).map_err(|error| {
        Diagnostic::error(error.to_string(), mitki_errors::TextRange::default())
    })?;
    Ok((
        shape.params.into_iter().map(contract_val_type_to_wasm).collect(),
        shape.results.into_iter().map(contract_val_type_to_wasm).collect(),
    ))
}

pub(in crate::backend) fn abi_v2_transport_lane(
    graph: &mitki_abi::SemanticTypeGraph,
    transport: &mitki_abi::TransportRef,
) -> Result<Option<ValType>, Diagnostic> {
    mitki_abi::transport_wasm_lane(graph, transport)
        .map(|lane| lane.map(contract_val_type_to_wasm))
        .map_err(|error| Diagnostic::error(error.to_string(), mitki_errors::TextRange::default()))
}

fn contract_val_type_to_wasm(value_type: mitki_abi::ContractValType) -> ValType {
    match value_type {
        mitki_abi::ContractValType::I32 => ValType::I32,
        mitki_abi::ContractValType::I64 => ValType::I64,
        mitki_abi::ContractValType::F32 => ValType::F32,
        mitki_abi::ContractValType::F64 => ValType::F64,
        mitki_abi::ContractValType::V128 => ValType::V128,
        mitki_abi::ContractValType::Ref => ValType::I32,
    }
}

struct WrapperFunctionBuilder<'db> {
    id: WrapperMirId,
    kind: WrapperKind<'db>,
    debug_name: String,
    signature: WrapperMirSignature<'db>,
    places: Vec<PlaceDecl>,
    place_by_label: FxHashMap<String, PlaceId>,
    stmts: Vec<Stmt<'db>>,
    next_place: u32,
    next_value: u32,
}

impl<'db> WrapperFunctionBuilder<'db> {
    fn new(
        id: WrapperMirId,
        kind: WrapperKind<'db>,
        debug_name: String,
        signature: WrapperMirSignature<'db>,
    ) -> Self {
        Self {
            id,
            kind,
            debug_name,
            signature,
            places: Vec::new(),
            place_by_label: FxHashMap::default(),
            stmts: Vec::new(),
            next_place: 0,
            next_value: 0,
        }
    }

    fn place(&mut self, label: impl Into<String>, ty: WrapperPlaceTy, kind: PlaceKind) -> PlaceId {
        let label = label.into();
        let id = PlaceId(self.next_place);
        self.next_place += 1;
        self.place_by_label.insert(label.clone(), id);
        self.places.push(PlaceDecl { id, label, ty, kind });
        id
    }

    fn find_place(&self, label: &str) -> PlaceId {
        self.place_by_label[label]
    }

    fn let_value(&mut self, ty: WrapperPlaceTy, rhs: RValue<'db>) -> ValueId {
        let value = ValueId(self.next_value);
        self.next_value += 1;
        self.stmts.push(Stmt::Let { value, ty, rhs });
        value
    }

    fn read_param_into(&mut self, place: PlaceId, param: u32, ty: WrapperPlaceTy) {
        let value = self.let_value(ty, RValue::ReadParam { param });
        self.stmts.push(Stmt::Store { place, value: Operand::Value(value) });
    }

    fn normalize_place(&mut self, place: PlaceId) {
        let value = self.let_value(
            WrapperPlaceTy::I32,
            RValue::NormalizeBool { operand: Operand::Place(place) },
        );
        self.stmts.push(Stmt::Store { place, value: Operand::Value(value) });
    }

    fn eval(&mut self, rhs: RValue<'db>) {
        self.stmts.push(Stmt::Eval { rhs });
    }

    fn ret(&mut self, values: Vec<Operand>) {
        self.stmts.push(Stmt::Return { values });
    }

    fn finish(self) -> WrapperMirFunction<'db> {
        WrapperMirFunction {
            id: self.id,
            kind: self.kind,
            debug_name: self.debug_name,
            signature: self.signature,
            places: self.places,
            body: Region { stmts: self.stmts },
        }
    }
}

fn place_ty_for_abi(abi: &AbiTy) -> WrapperPlaceTy {
    match abi {
        AbiTy::Scalar(BackendTy::I64) => WrapperPlaceTy::I64,
        AbiTy::Scalar(BackendTy::Float) => WrapperPlaceTy::F64,
        AbiTy::Scalar(BackendTy::Unit) => WrapperPlaceTy::Unit,
        AbiTy::Scalar(BackendTy::Int | BackendTy::Bool | BackendTy::Char | BackendTy::Ref(_))
        | AbiTy::Aggregate(_) => WrapperPlaceTy::I32,
    }
}

fn place_ty_for_val_type(value_type: ValType) -> WrapperPlaceTy {
    match value_type {
        ValType::I32 => WrapperPlaceTy::I32,
        ValType::I64 => WrapperPlaceTy::I64,
        ValType::F64 => WrapperPlaceTy::F64,
        _ => WrapperPlaceTy::I32,
    }
}

fn place_ty_for_transport(
    graph: &mitki_abi::SemanticTypeGraph,
    transport: &mitki_abi::TransportRef,
) -> Result<WrapperPlaceTy, Diagnostic> {
    Ok(match abi_v2_transport_lane(graph, transport)? {
        None => WrapperPlaceTy::Unit,
        Some(ValType::I32) => WrapperPlaceTy::I32,
        Some(ValType::I64) => WrapperPlaceTy::I64,
        Some(ValType::F64) => WrapperPlaceTy::F64,
        Some(_) => WrapperPlaceTy::I32,
    })
}

fn bind_wrapper_signature_params<'db>(
    builder: &mut WrapperFunctionBuilder<'db>,
    graph: &mitki_abi::SemanticTypeGraph,
    signature: &AbiV2FunctionSignature,
) -> Result<Vec<PlaceId>, Diagnostic> {
    bind_wrapper_signature_params_with_offset(builder, graph, signature, 0)
}

fn bind_wrapper_signature_params_with_offset<'db>(
    builder: &mut WrapperFunctionBuilder<'db>,
    graph: &mitki_abi::SemanticTypeGraph,
    signature: &AbiV2FunctionSignature,
    offset: u32,
) -> Result<Vec<PlaceId>, Diagnostic> {
    let mut params = Vec::with_capacity(signature.params.len());
    let mut wasm_index = offset;
    for (ordinal, transport) in signature.params.iter().enumerate() {
        let ty = place_ty_for_transport(graph, transport)?;
        let place = builder.place(format!("param{ordinal}.lane"), ty, PlaceKind::Local);
        if ty != WrapperPlaceTy::Unit {
            builder.read_param_into(place, wasm_index, ty);
            wasm_index += 1;
        }
        params.push(place);
    }
    Ok(params)
}

fn add_canonical_scratch_places(builder: &mut WrapperFunctionBuilder<'_>) {
    let scratches = [
        ("scratch.node_offset", WrapperScratchKind::NodeOffset, WrapperPlaceTy::I32),
        ("scratch.cursor", WrapperScratchKind::Cursor, WrapperPlaceTy::I32),
        ("scratch.index", WrapperScratchKind::Index, WrapperPlaceTy::I32),
        ("scratch.len", WrapperScratchKind::Len, WrapperPlaceTy::I32),
        ("scratch.count", WrapperScratchKind::Count, WrapperPlaceTy::I32),
        ("scratch.bytes", WrapperScratchKind::Bytes, WrapperPlaceTy::I32),
        ("scratch.base_id", WrapperScratchKind::BaseId, WrapperPlaceTy::I32),
        ("scratch.temp_ptr", WrapperScratchKind::TempPtr, WrapperPlaceTy::I32),
        ("scratch.temp_ptr_aux", WrapperScratchKind::TempPtrAux, WrapperPlaceTy::I32),
        ("scratch.child_count", WrapperScratchKind::ChildCount, WrapperPlaceTy::I32),
        ("scratch.child_bytes", WrapperScratchKind::ChildBytes, WrapperPlaceTy::I32),
        ("scratch.handle_count", WrapperScratchKind::HandleCount, WrapperPlaceTy::I32),
        ("scratch.handle_index", WrapperScratchKind::HandleIndex, WrapperPlaceTy::I32),
        ("scratch.handle_cursor", WrapperScratchKind::HandleCursor, WrapperPlaceTy::I32),
        ("scratch.f64_temp", WrapperScratchKind::F64Temp, WrapperPlaceTy::F64),
    ];
    for (label, kind, ty) in scratches {
        builder.place(label, ty, PlaceKind::Scratch(kind));
    }
}

fn maybe_prepare_runtime_place(
    builder: &mut WrapperFunctionBuilder<'_>,
    place: PlaceId,
    abi: &AbiTy,
) {
    if let AbiTy::Aggregate(layout) = abi {
        builder.eval(RValue::AllocTempBuffer { place, layout: (**layout).clone() });
        builder.eval(RValue::ZeroTempBuffer { place, size: layout.size });
    }
}

fn release_runtime_places(
    builder: &mut WrapperFunctionBuilder<'_>,
    abis: &[AbiTy],
    places: &[PlaceId],
) {
    for (abi, place) in abis.iter().zip(places.iter().copied()) {
        release_runtime_place(builder, abi, place);
    }
}

fn release_runtime_place(builder: &mut WrapperFunctionBuilder<'_>, abi: &AbiTy, place: PlaceId) {
    builder.eval(RValue::ReleaseValue { place, abi: abi.clone() });
    if let AbiTy::Aggregate(layout) = abi {
        builder.eval(RValue::DeallocTempBuffer { place, layout: (**layout).clone() });
    }
}

fn call_args_for_internal_signature(
    signature: &FunctionSignature,
    result_ptr_place: Option<PlaceId>,
    arg_places: &[PlaceId],
) -> Vec<Operand> {
    let mut args = Vec::new();
    if signature.result.is_aggregate()
        && let Some(result_ptr_place) = result_ptr_place
    {
        args.push(Operand::Place(result_ptr_place));
    }
    args.extend(arg_places.iter().copied().map(Operand::Place));
    args
}

impl WrapperMirBuilder {
    pub(in crate::backend) fn build<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
    ) -> Result<WrapperMirBundle<'db>, Diagnostic> {
        let mut bundle = WrapperMirBundle {
            imports: Vec::new(),
            callable_adapters: Vec::new(),
            trampolines: Vec::new(),
            exports: Vec::new(),
            import_indices: FxHashMap::default(),
            callable_adapter_indices: FxHashMap::default(),
            trampoline_indices: FxHashMap::default(),
            export_indices: FxHashMap::default(),
        };
        let mut next_id = 0u32;

        for import in &plan.boundary.imports {
            let function = build_import_wrapper_mir(backend, plan, import, WrapperMirId(next_id))?;
            bundle.import_indices.insert(import.instance.clone(), bundle.imports.len());
            bundle.imports.push(function);
            next_id += 1;
        }
        for instance in &plan.reachability.functions {
            let function =
                build_function_value_wrapper_mir(backend, plan, instance, WrapperMirId(next_id))?;
            bundle
                .callable_adapter_indices
                .insert(instance.clone(), bundle.callable_adapters.len());
            bundle.callable_adapters.push(function);
            next_id += 1;
        }
        for trampoline in &plan.callables.invoke_trampolines {
            let function =
                build_handle_trampoline_mir(backend, plan, trampoline, WrapperMirId(next_id))?;
            bundle.trampoline_indices.insert(trampoline.signature_id, bundle.trampolines.len());
            bundle.trampolines.push(function);
            next_id += 1;
        }
        for export in &plan.boundary.exports {
            let function = build_export_wrapper_mir(backend, plan, export, WrapperMirId(next_id))?;
            bundle.export_indices.insert(export.instance.clone(), bundle.exports.len());
            bundle.exports.push(function);
            next_id += 1;
        }

        Ok(bundle)
    }
}

impl WrapperMirValidator {
    pub(in crate::backend) fn validate<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        bundle: &WrapperMirBundle<'db>,
    ) -> Result<(), Diagnostic> {
        for function in bundle
            .imports
            .iter()
            .chain(bundle.callable_adapters.iter())
            .chain(bundle.trampolines.iter())
            .chain(bundle.exports.iter())
        {
            validate_wrapper_mir_function(backend, plan, function)?;
        }
        Ok(())
    }
}

fn build_function_value_wrapper_mir<'db>(
    backend: &Backend<'db>,
    _plan: &ModulePlan<'db>,
    instance: &InstanceKey<'db>,
    id: WrapperMirId,
) -> Result<WrapperMirFunction<'db>, Diagnostic> {
    let hir_function = instance.location.hir_function(backend.db);
    let function = hir_function.function(backend.db);
    let inference = instance.location.infer(backend.db);
    let signature = backend.function_signature(instance, function, inference)?;
    let debug_name = instance
        .location
        .source(backend.db)
        .name()
        .map_or("<anonymous>".to_owned(), |name| name.as_str().to_owned());
    let signature_strategy = backend.signature_strategy();
    let callable_lowering = backend.callable_lowering_strategy();
    let memory_model = backend.memory_model_strategy();
    let callable_signature =
        callable_lowering.callable_signature(signature_strategy, memory_model, &signature);
    let mut builder = WrapperFunctionBuilder::new(
        id,
        WrapperKind::FunctionValueWrapper { instance: instance.clone() },
        format!("callable {debug_name}"),
        WrapperMirSignature {
            wasm_params: callable_signature.params.clone(),
            wasm_results: callable_signature.results.clone(),
            internal: Some(InternalSig::from_function_signature(signature.clone())),
            boundary: None,
            callable: Some(signature.clone()),
            metadata_index: None,
            signature_id: None,
        },
    );

    let mut next_param = 0u32;
    if let Some(env_lane) = callable_lowering.environment_lane(memory_model) {
        let env_ty = place_ty_for_val_type(env_lane);
        let env_place = builder.place("env", env_ty, PlaceKind::Local);
        builder.read_param_into(env_place, next_param, env_ty);
        next_param += 1;
    }
    let result_ptr_place = if signature.result.is_aggregate() {
        let ptr_ty = place_ty_for_val_type(signature_strategy.result_pointer_lane());
        let place = builder.place("result_ptr", ptr_ty, PlaceKind::Local);
        builder.read_param_into(place, next_param, ptr_ty);
        next_param += 1;
        Some(place)
    } else {
        None
    };

    let mut arg_places = Vec::with_capacity(signature.params.len());
    for (index, abi) in signature.params.iter().enumerate() {
        let ty = place_ty_for_abi(abi);
        let place = builder.place(format!("param{index}"), ty, PlaceKind::Local);
        if ty != WrapperPlaceTy::Unit {
            builder.read_param_into(place, next_param, ty);
            next_param += 1;
            if matches!(abi, AbiTy::Scalar(BackendTy::Bool)) {
                builder.normalize_place(place);
            }
        }
        arg_places.push(place);
    }

    let call_result_place = if signature.result.is_aggregate()
        || matches!(signature.result, AbiTy::Scalar(BackendTy::Unit))
    {
        None
    } else {
        Some(builder.place("result", place_ty_for_abi(&signature.result), PlaceKind::Local))
    };
    builder.eval(RValue::CallDirect {
        target: WrapperCallTarget::DirectFunction(instance.clone()),
        wasm_results: signature_strategy
            .direct_signature(&signature)
            .results
            .into_iter()
            .map(place_ty_for_val_type)
            .collect(),
        args: call_args_for_internal_signature(&signature, result_ptr_place, &arg_places),
        result: if signature.result.is_aggregate() { None } else { call_result_place },
    });

    let mut return_values = Vec::new();
    if let Some(result_place) = call_result_place {
        if matches!(signature.result, AbiTy::Scalar(BackendTy::Bool)) {
            builder.normalize_place(result_place);
        }
        return_values.push(Operand::Place(result_place));
    }
    builder.ret(return_values);
    Ok(builder.finish())
}

fn build_import_wrapper_mir<'db>(
    backend: &Backend<'db>,
    plan: &ModulePlan<'db>,
    import: &boundary::ImportBoundaryPlan<'db>,
    id: WrapperMirId,
) -> Result<WrapperMirFunction<'db>, Diagnostic> {
    let entry = &plan.boundary.instances[import.metadata_index];
    let built = &plan.abi_preview.functions[import.metadata_index];
    let abi_signature = &plan.abi_preview.graph.signatures[built.signature_id.0 as usize];
    let signature_strategy = backend.signature_strategy();
    let lowered_internal =
        signature_strategy.direct_signature(&entry.signature.internal.as_function_signature());
    let mut builder = WrapperFunctionBuilder::new(
        id,
        WrapperKind::ImportThunk {
            instance: import.instance.clone(),
            metadata_index: import.metadata_index,
        },
        format!("import {}", import.logical_name),
        WrapperMirSignature {
            wasm_params: lowered_internal.params.clone(),
            wasm_results: lowered_internal.results.clone(),
            internal: Some(entry.signature.internal.clone()),
            boundary: Some(import.wrapper.signature.clone()),
            callable: None,
            metadata_index: Some(import.metadata_index),
            signature_id: Some(built.signature_id),
        },
    );

    let internal_signature = entry.signature.internal.as_function_signature();
    let mut wasm_param = 0u32;
    let result_ptr_place = if internal_signature.result.is_aggregate() {
        let ptr_ty = place_ty_for_val_type(signature_strategy.result_pointer_lane());
        let place = builder.place("result_ptr", ptr_ty, PlaceKind::Local);
        builder.read_param_into(place, wasm_param, ptr_ty);
        wasm_param += 1;
        Some(place)
    } else {
        None
    };
    let mut internal_param_places = Vec::with_capacity(internal_signature.params.len());
    for (index, abi) in internal_signature.params.iter().enumerate() {
        let ty = place_ty_for_abi(abi);
        let place = builder.place(format!("param{index}"), ty, PlaceKind::Local);
        if ty != WrapperPlaceTy::Unit {
            builder.read_param_into(place, wasm_param, ty);
            wasm_param += 1;
        }
        internal_param_places.push(place);
    }
    add_canonical_scratch_places(&mut builder);

    let mut call_args = Vec::new();
    for (index, slot) in import.wrapper.signature.params.iter().enumerate() {
        let source_place = internal_param_places[index];
        match slot.transport.transport_class {
            TransportClass::Immediate => {
                if matches!(slot.semantic_ty.kind(backend.db), TyKind::Bool) {
                    builder.normalize_place(source_place);
                }
                call_args.push(Operand::Place(source_place));
            }
            TransportClass::CanonicalValue => {
                let blob = builder.place(
                    format!("param{index}.blob"),
                    WrapperPlaceTy::I32,
                    PlaceKind::Local,
                );
                builder.eval(RValue::EncodeCanonical {
                    source: source_place,
                    dest: blob,
                    slot: slot.clone(),
                });
                builder.eval(RValue::RetainNestedHandles { place: blob, ty: slot.semantic_ty });
                call_args.push(Operand::Place(blob));
            }
            TransportClass::CapabilityHandle => {
                let handle = builder.place(
                    format!("param{index}.handle"),
                    WrapperPlaceTy::I32,
                    PlaceKind::Local,
                );
                builder.eval(RValue::WrapHandle {
                    source: source_place,
                    dest: handle,
                    slot: slot.clone(),
                });
                call_args.push(Operand::Place(handle));
            }
        }
    }

    let call_result_place = import.wrapper.signature.results.first().map(|slot| {
        let ty = place_ty_for_transport(&plan.abi_preview.graph, &abi_signature.result)
            .unwrap_or_else(|diagnostic| {
                panic!("transport lane should lower: {}", diagnostic.message())
            });
        builder.place(
            match slot.transport.transport_class {
                TransportClass::Immediate => "result.lane",
                TransportClass::CanonicalValue => "result.blob",
                TransportClass::CapabilityHandle => "result.handle",
            },
            ty,
            PlaceKind::Local,
        )
    });
    builder.eval(RValue::CallDirect {
        target: WrapperCallTarget::RawImport(import.instance.clone()),
        wasm_results: abi_v2_transport_lane(&plan.abi_preview.graph, &abi_signature.result)?
            .into_iter()
            .map(place_ty_for_val_type)
            .collect(),
        args: call_args,
        result: call_result_place,
    });

    let mut return_values = Vec::new();
    if let Some(slot) = import.wrapper.signature.results.first()
        && let Some(result_place) = call_result_place
    {
        match slot.transport.transport_class {
            TransportClass::Immediate => {
                if matches!(slot.semantic_ty.kind(backend.db), TyKind::Bool) {
                    builder.normalize_place(result_place);
                }
                if result_ptr_place.is_none() {
                    return_values.push(Operand::Place(result_place));
                }
            }
            TransportClass::CanonicalValue => {
                if let Some(result_ptr_place) = result_ptr_place {
                    builder.eval(RValue::DecodeCanonical {
                        source: result_place,
                        dest: result_ptr_place,
                        slot: slot.clone(),
                    });
                } else if let Some(result_abi) = entry.signature.internal.results.first() {
                    let runtime_result = builder.place(
                        "result.runtime",
                        place_ty_for_abi(result_abi),
                        PlaceKind::Local,
                    );
                    builder.eval(RValue::DecodeCanonical {
                        source: result_place,
                        dest: runtime_result,
                        slot: slot.clone(),
                    });
                    return_values.push(Operand::Place(runtime_result));
                }
            }
            TransportClass::CapabilityHandle => {
                if let Some(result_ptr_place) = result_ptr_place {
                    builder.eval(RValue::UnwrapHandle {
                        source: result_place,
                        dest: result_ptr_place,
                        slot: slot.clone(),
                    });
                } else if let Some(result_abi) = entry.signature.internal.results.first() {
                    let runtime_result = builder.place(
                        "result.runtime",
                        place_ty_for_abi(result_abi),
                        PlaceKind::Local,
                    );
                    builder.eval(RValue::UnwrapHandle {
                        source: result_place,
                        dest: runtime_result,
                        slot: slot.clone(),
                    });
                    return_values.push(Operand::Place(runtime_result));
                }
            }
        }
    }
    for (index, abi) in internal_signature.params.iter().enumerate() {
        release_runtime_place(&mut builder, abi, internal_param_places[index]);
        match import.wrapper.signature.params[index].transport.transport_class {
            TransportClass::CanonicalValue => {
                builder.eval(RValue::ReleaseCanonicalBlob {
                    place: builder.find_place(&format!("param{index}.blob")),
                });
            }
            TransportClass::CapabilityHandle => {
                builder.eval(RValue::ReleaseHandleObject {
                    place: builder.find_place(&format!("param{index}.handle")),
                });
            }
            TransportClass::Immediate => {}
        }
    }
    builder.ret(return_values);
    Ok(builder.finish())
}

fn build_export_wrapper_mir<'db>(
    backend: &Backend<'db>,
    plan: &ModulePlan<'db>,
    export: &boundary::ExportBoundaryPlan<'db>,
    id: WrapperMirId,
) -> Result<WrapperMirFunction<'db>, Diagnostic> {
    let entry = &plan.boundary.instances[export.metadata_index];
    let built = &plan.abi_preview.functions[export.metadata_index];
    let abi_signature = &plan.abi_preview.graph.signatures[built.signature_id.0 as usize];
    let (wasm_params, wasm_results) =
        abi_v2_wasm_signature(&plan.abi_preview.graph, abi_signature)?;
    let mut builder = WrapperFunctionBuilder::new(
        id,
        WrapperKind::ExportWrapper {
            instance: export.instance.clone(),
            metadata_index: export.metadata_index,
        },
        format!("export {}", export.logical_name),
        WrapperMirSignature {
            wasm_params,
            wasm_results,
            internal: Some(entry.signature.internal.clone()),
            boundary: Some(export.wrapper.signature.clone()),
            callable: None,
            metadata_index: Some(export.metadata_index),
            signature_id: Some(built.signature_id),
        },
    );

    let param_places =
        bind_wrapper_signature_params(&mut builder, &plan.abi_preview.graph, abi_signature)?;
    add_canonical_scratch_places(&mut builder);

    let mut runtime_param_places = Vec::with_capacity(entry.signature.params.len());
    for (index, slot) in entry.signature.params.iter().enumerate() {
        let runtime_place = builder.place(
            format!("runtime.param{index}"),
            place_ty_for_abi(&entry.signature.internal.params[index]),
            PlaceKind::Local,
        );
        match slot.transport.transport_class {
            TransportClass::Immediate => {
                builder.eval(RValue::DecodeImmediate {
                    source: param_places[index],
                    dest: runtime_place,
                    slot: slot.clone(),
                });
                if matches!(slot.semantic_ty.kind(backend.db), TyKind::Bool) {
                    builder.normalize_place(runtime_place);
                }
            }
            TransportClass::CanonicalValue => {
                maybe_prepare_runtime_place(
                    &mut builder,
                    runtime_place,
                    &entry.signature.internal.params[index],
                );
                builder.eval(RValue::DecodeCanonical {
                    source: param_places[index],
                    dest: runtime_place,
                    slot: slot.clone(),
                });
            }
            TransportClass::CapabilityHandle => {
                maybe_prepare_runtime_place(
                    &mut builder,
                    runtime_place,
                    &entry.signature.internal.params[index],
                );
                builder.eval(RValue::UnwrapHandle {
                    source: param_places[index],
                    dest: runtime_place,
                    slot: slot.clone(),
                });
            }
        }
        runtime_param_places.push(runtime_place);
    }

    let call_result_place = entry.signature.internal.results.first().map(|abi| {
        let place = builder.place("runtime.result", place_ty_for_abi(abi), PlaceKind::Local);
        maybe_prepare_runtime_place(&mut builder, place, abi);
        place
    });
    let internal_signature = entry.signature.internal.as_function_signature();
    let lowered_internal = backend.signature_strategy().direct_signature(&internal_signature);
    builder.eval(RValue::CallDirect {
        target: WrapperCallTarget::DirectFunction(export.instance.clone()),
        wasm_results: lowered_internal.results.into_iter().map(place_ty_for_val_type).collect(),
        args: call_args_for_internal_signature(
            &internal_signature,
            call_result_place,
            &runtime_param_places,
        ),
        result: if internal_signature.result.is_aggregate() { None } else { call_result_place },
    });

    let mut return_values = Vec::new();
    if let Some(slot) = entry.signature.results.first()
        && let Some(result_place) = call_result_place
    {
        let outward_place = builder.place(
            "result.out",
            place_ty_for_transport(&plan.abi_preview.graph, &abi_signature.result)?,
            PlaceKind::Local,
        );
        match slot.transport.transport_class {
            TransportClass::Immediate => {
                builder.eval(RValue::EncodeImmediate {
                    source: result_place,
                    dest: outward_place,
                    slot: slot.clone(),
                });
                if matches!(slot.semantic_ty.kind(backend.db), TyKind::Bool) {
                    builder.normalize_place(outward_place);
                }
            }
            TransportClass::CanonicalValue => {
                builder.eval(RValue::EncodeCanonical {
                    source: result_place,
                    dest: outward_place,
                    slot: slot.clone(),
                });
                builder.eval(RValue::RetainNestedHandles {
                    place: outward_place,
                    ty: slot.semantic_ty,
                });
            }
            TransportClass::CapabilityHandle => {
                builder.eval(RValue::WrapHandle {
                    source: result_place,
                    dest: outward_place,
                    slot: slot.clone(),
                });
            }
        }
        return_values.push(Operand::Place(outward_place));
    }

    release_runtime_places(&mut builder, &internal_signature.params, &runtime_param_places);
    if let Some(result_place) = call_result_place {
        release_runtime_place(&mut builder, &internal_signature.result, result_place);
    }
    builder.ret(return_values);
    Ok(builder.finish())
}

fn build_handle_trampoline_mir<'db>(
    backend: &Backend<'db>,
    plan: &ModulePlan<'db>,
    trampoline: &registry::CallableTrampolinePlan<'db>,
    id: WrapperMirId,
) -> Result<WrapperMirFunction<'db>, Diagnostic> {
    let entry = &plan.boundary.instances[trampoline.metadata_index];
    let built = &plan.abi_preview.functions[trampoline.metadata_index];
    let abi_signature = &plan.abi_preview.graph.signatures[built.signature_id.0 as usize];
    let (wasm_params, wasm_results) =
        abi_v2_wasm_signature(&plan.abi_preview.graph, abi_signature)?;
    let callable_lowering = backend.callable_lowering_strategy();
    let memory_model = backend.memory_model_strategy();
    if !callable_lowering.uses_table_slots() {
        return Err(Diagnostic::error(
            format!(
                "the target profile `{}` does not support handle-and-table boundary trampolines \
                 for callable exports yet",
                backend.target_profile().canonical_name()
            ),
            backend.function_range(trampoline.instance.location),
        ));
    }
    let lowered_boundary = callable_lowering.boundary_invoke_signature(
        memory_model,
        PhysicalWasmSignature::new(wasm_params, wasm_results),
    );
    let mut builder = WrapperFunctionBuilder::new(
        id,
        WrapperKind::HandleInvokeTrampoline {
            instance: trampoline.instance.clone(),
            metadata_index: trampoline.metadata_index,
            signature_id: trampoline.signature_id,
        },
        format!("trampoline {}", entry.logical_name),
        WrapperMirSignature {
            wasm_params: lowered_boundary.params.clone(),
            wasm_results: lowered_boundary.results.clone(),
            internal: Some(entry.signature.internal.clone()),
            boundary: Some(entry.signature.clone()),
            callable: Some(entry.signature.internal.as_function_signature()),
            metadata_index: Some(trampoline.metadata_index),
            signature_id: Some(trampoline.signature_id),
        },
    );

    let handle_ty = place_ty_for_val_type(callable_lowering.handle_lane(memory_model));
    let handle_place = builder.place("handle", handle_ty, PlaceKind::Local);
    builder.read_param_into(handle_place, 0, handle_ty);
    let boundary_param_offset = u32::from(callable_lowering.uses_table_slots());
    let param_places = bind_wrapper_signature_params_with_offset(
        &mut builder,
        &plan.abi_preview.graph,
        abi_signature,
        boundary_param_offset,
    )?;
    add_canonical_scratch_places(&mut builder);

    let mut runtime_param_places = Vec::with_capacity(entry.signature.params.len());
    for (index, slot) in entry.signature.params.iter().enumerate() {
        let runtime_place = builder.place(
            format!("runtime.param{index}"),
            place_ty_for_abi(&entry.signature.internal.params[index]),
            PlaceKind::Local,
        );
        match slot.transport.transport_class {
            TransportClass::Immediate => {
                builder.eval(RValue::DecodeImmediate {
                    source: param_places[index],
                    dest: runtime_place,
                    slot: slot.clone(),
                });
                if matches!(slot.semantic_ty.kind(backend.db), TyKind::Bool) {
                    builder.normalize_place(runtime_place);
                }
            }
            TransportClass::CanonicalValue => {
                maybe_prepare_runtime_place(
                    &mut builder,
                    runtime_place,
                    &entry.signature.internal.params[index],
                );
                builder.eval(RValue::DecodeCanonical {
                    source: param_places[index],
                    dest: runtime_place,
                    slot: slot.clone(),
                });
            }
            TransportClass::CapabilityHandle => {
                maybe_prepare_runtime_place(
                    &mut builder,
                    runtime_place,
                    &entry.signature.internal.params[index],
                );
                builder.eval(RValue::UnwrapHandle {
                    source: param_places[index],
                    dest: runtime_place,
                    slot: slot.clone(),
                });
            }
        }
        runtime_param_places.push(runtime_place);
    }

    let env = builder.let_value(
        place_ty_for_val_type(callable_lowering.handle_lane(memory_model)),
        RValue::ReadHandleField {
            handle: Operand::Place(handle_place),
            field: WrapperHandleField::Env,
        },
    );
    let table_slot = builder.let_value(
        place_ty_for_val_type(callable_lowering.table_index_lane(memory_model)),
        RValue::ReadHandleField {
            handle: Operand::Place(handle_place),
            field: WrapperHandleField::Slot,
        },
    );
    let internal_signature = entry.signature.internal.as_function_signature();
    let lowered_internal = backend.signature_strategy().direct_signature(&internal_signature);
    let call_result_place = entry.signature.internal.results.first().map(|abi| {
        let place = builder.place("runtime.result", place_ty_for_abi(abi), PlaceKind::Local);
        maybe_prepare_runtime_place(&mut builder, place, abi);
        place
    });
    builder.eval(RValue::CallIndirect {
        signature: internal_signature.clone(),
        wasm_results: lowered_internal.results.into_iter().map(place_ty_for_val_type).collect(),
        env: Operand::Value(env),
        table_index: Operand::Value(table_slot),
        args: call_args_for_internal_signature(
            &internal_signature,
            call_result_place,
            &runtime_param_places,
        ),
        result: if internal_signature.result.is_aggregate() { None } else { call_result_place },
    });

    let mut return_values = Vec::new();
    if let Some(slot) = entry.signature.results.first()
        && let Some(result_place) = call_result_place
    {
        let outward_place = builder.place(
            "result.out",
            place_ty_for_transport(&plan.abi_preview.graph, &abi_signature.result)?,
            PlaceKind::Local,
        );
        match slot.transport.transport_class {
            TransportClass::Immediate => {
                builder.eval(RValue::EncodeImmediate {
                    source: result_place,
                    dest: outward_place,
                    slot: slot.clone(),
                });
                if matches!(slot.semantic_ty.kind(backend.db), TyKind::Bool) {
                    builder.normalize_place(outward_place);
                }
            }
            TransportClass::CanonicalValue => {
                builder.eval(RValue::EncodeCanonical {
                    source: result_place,
                    dest: outward_place,
                    slot: slot.clone(),
                });
                builder.eval(RValue::RetainNestedHandles {
                    place: outward_place,
                    ty: slot.semantic_ty,
                });
            }
            TransportClass::CapabilityHandle => {
                builder.eval(RValue::WrapHandle {
                    source: result_place,
                    dest: outward_place,
                    slot: slot.clone(),
                });
            }
        }
        return_values.push(Operand::Place(outward_place));
    }

    release_runtime_places(&mut builder, &internal_signature.params, &runtime_param_places);
    if let Some(result_place) = call_result_place {
        release_runtime_place(&mut builder, &internal_signature.result, result_place);
    }
    builder.ret(return_values);
    Ok(builder.finish())
}

fn validate_wrapper_mir_function<'db>(
    backend: &Backend<'db>,
    plan: &ModulePlan<'db>,
    function: &WrapperMirFunction<'db>,
) -> Result<(), Diagnostic> {
    match &function.kind {
        WrapperKind::ImportThunk { instance, metadata_index } => {
            let entry = plan.boundary.instances.get(*metadata_index).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: wrapper MIR import metadata index drifted",
                    backend.file_range(),
                )
            })?;
            let expected = entry.signature.internal.as_function_signature();
            let lowered = backend.signature_strategy().direct_signature(&expected);
            if &entry.instance != instance
                || entry.linkage != mitki_abi::LinkageKind::WasmImport
                || function.signature.wasm_params != lowered.params
                || function.signature.wasm_results != lowered.results
            {
                return Err(Diagnostic::error(
                    "internal error: import thunk MIR signature drifted from its source plan",
                    backend.function_range(instance.location),
                ));
            }
        }
        WrapperKind::ExportWrapper { instance, metadata_index } => {
            let entry = plan.boundary.instances.get(*metadata_index).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: wrapper MIR export metadata index drifted",
                    backend.file_range(),
                )
            })?;
            let built = &plan.abi_preview.functions[*metadata_index];
            let abi_signature = &plan.abi_preview.graph.signatures[built.signature_id.0 as usize];
            let (params, results) = abi_v2_wasm_signature(&plan.abi_preview.graph, abi_signature)?;
            let expected_param_lanes = abi_signature
                .params
                .iter()
                .map(|transport| transport_has_wasm_lane(&plan.abi_preview.graph, transport))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .filter(|has_lane| *has_lane)
                .count();
            let expected_result_lanes = usize::from(transport_has_wasm_lane(
                &plan.abi_preview.graph,
                &abi_signature.result,
            )?);
            if &entry.instance != instance
                || entry.linkage != mitki_abi::LinkageKind::WasmExport
                || params.len() != expected_param_lanes
                || results.len() != expected_result_lanes
                || function.signature.wasm_params != params
                || function.signature.wasm_results != results
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: export wrapper MIR signature drifted from ABI metadata \
                         in `{}` (actual params={:?}, actual results={:?}, expected params={:?}, \
                         expected results={:?})",
                        function.debug_name,
                        function.signature.wasm_params,
                        function.signature.wasm_results,
                        params,
                        results,
                    ),
                    backend.function_range(instance.location),
                ));
            }
        }
        WrapperKind::HandleInvokeTrampoline { instance, metadata_index, signature_id } => {
            let entry = plan.boundary.instances.get(*metadata_index).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: wrapper MIR trampoline metadata index drifted",
                    backend.file_range(),
                )
            })?;
            let built = &plan.abi_preview.functions[*metadata_index];
            let abi_signature = &plan.abi_preview.graph.signatures[built.signature_id.0 as usize];
            let (params, results) = abi_v2_wasm_signature(&plan.abi_preview.graph, abi_signature)?;
            let lowered = backend.callable_lowering_strategy().boundary_invoke_signature(
                backend.memory_model_strategy(),
                PhysicalWasmSignature::new(params, results),
            );
            let expected_param_lanes = abi_signature
                .params
                .iter()
                .map(|transport| transport_has_wasm_lane(&plan.abi_preview.graph, transport))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .filter(|has_lane| *has_lane)
                .count();
            let expected_result_lanes = usize::from(transport_has_wasm_lane(
                &plan.abi_preview.graph,
                &abi_signature.result,
            )?);
            if &entry.instance != instance
                || built.signature_id != *signature_id
                || lowered.params.len()
                    != expected_param_lanes
                        + usize::from(backend.callable_lowering_strategy().uses_table_slots())
                || lowered.results.len() != expected_result_lanes
                || function.signature.wasm_params != lowered.params
                || function.signature.wasm_results != lowered.results
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: handle trampoline MIR signature drifted from the \
                         callable plan in `{}`",
                        function.debug_name
                    ),
                    backend.function_range(instance.location),
                ));
            }
        }
        WrapperKind::FunctionValueWrapper { instance } => {
            let hir_function = instance.location.hir_function(backend.db);
            let function_sig = backend.function_signature(
                instance,
                hir_function.function(backend.db),
                instance.location.infer(backend.db),
            )?;
            let lowered = backend.callable_lowering_strategy().callable_signature(
                backend.signature_strategy(),
                backend.memory_model_strategy(),
                &function_sig,
            );
            if function.signature.wasm_params != lowered.params
                || function.signature.wasm_results != lowered.results
            {
                return Err(Diagnostic::error(
                    "internal error: callable wrapper MIR signature drifted from the callable ABI",
                    backend.function_range(instance.location),
                ));
            }
        }
    }

    let mut seen_places = FxHashMap::default();
    for place in &function.places {
        if seen_places.insert(place.id, place.label.as_str()).is_some() {
            return Err(Diagnostic::error(
                "internal error: wrapper MIR declared the same place id twice",
                backend.file_range(),
            ));
        }
    }
    if function.body.stmts.is_empty()
        || !matches!(function.body.stmts.last(), Some(Stmt::Return { .. }))
    {
        return Err(Diagnostic::error(
            "internal error: wrapper MIR region must end with a return",
            backend.file_range(),
        ));
    }
    validate_region(backend, function, &function.body, &mut FxHashMap::default())?;
    Ok(())
}

fn validate_region<'db>(
    backend: &Backend<'db>,
    function: &WrapperMirFunction<'db>,
    region: &Region<'db>,
    values: &mut FxHashMap<ValueId, WrapperPlaceTy>,
) -> Result<(), Diagnostic> {
    let mut initialized = Vec::new();
    for stmt in &region.stmts {
        match stmt {
            Stmt::Let { value, ty, rhs } => {
                validate_rvalue_uses(backend, function, &initialized, values, rhs)?;
                values.insert(*value, *ty);
            }
            Stmt::Store { place, value } => {
                validate_operand(backend, function, &initialized, values, value)?;
                if !initialized.contains(place) {
                    initialized.push(*place);
                }
            }
            Stmt::If { cond, then_region, else_region, .. } => {
                validate_operand(backend, function, &initialized, values, cond)?;
                validate_region(backend, function, then_region, &mut values.clone())?;
                validate_region(backend, function, else_region, &mut values.clone())?;
            }
            Stmt::Eval { rhs } => {
                validate_rvalue_uses(backend, function, &initialized, values, rhs)?;
                if let Some(place) = rvalue_writes_place(rhs)
                    && !initialized.contains(&place)
                {
                    initialized.push(place);
                }
            }
            Stmt::Return { values: return_values } => {
                if return_values.len() != function.signature.wasm_results.len() {
                    return Err(Diagnostic::error(
                        format!(
                            "internal error: wrapper MIR return arity drifted from its signature \
                             in `{}`",
                            function.debug_name
                        ),
                        backend.file_range(),
                    ));
                }
                for value in return_values {
                    validate_operand(backend, function, &initialized, values, value)?;
                }
            }
        }
    }
    Ok(())
}

fn validate_rvalue_uses<'db>(
    backend: &Backend<'db>,
    function: &WrapperMirFunction<'db>,
    initialized: &[PlaceId],
    values: &FxHashMap<ValueId, WrapperPlaceTy>,
    rhs: &RValue<'db>,
) -> Result<(), Diagnostic> {
    match rhs {
        RValue::ReadParam { .. } => {}
        RValue::NormalizeBool { operand } => {
            validate_operand(backend, function, initialized, values, operand)?
        }
        RValue::LoadPlace { place }
        | RValue::ReleaseValue { place, .. }
        | RValue::ReleaseCanonicalBlob { place }
        | RValue::ReleaseHandleObject { place }
        | RValue::DeallocTempBuffer { place, .. }
        | RValue::ZeroTempBuffer { place, .. } => {
            validate_place_initialized(backend, function, initialized, *place)?
        }
        RValue::CallDirect { args, .. } => {
            for arg in args {
                validate_operand(backend, function, initialized, values, arg)?;
            }
        }
        RValue::CallIndirect { env, table_index, args, .. } => {
            validate_operand(backend, function, initialized, values, env)?;
            validate_operand(backend, function, initialized, values, table_index)?;
            for arg in args {
                validate_operand(backend, function, initialized, values, arg)?;
            }
        }
        RValue::EncodeImmediate { source, .. }
        | RValue::DecodeImmediate { source, .. }
        | RValue::EncodeCanonical { source, .. }
        | RValue::DecodeCanonical { source, .. }
        | RValue::WrapHandle { source, .. }
        | RValue::UnwrapHandle { source, .. }
        | RValue::RetainNestedHandles { place: source, .. } => {
            validate_place_initialized(backend, function, initialized, *source)?
        }
        RValue::AllocTempBuffer { .. } => {}
        RValue::ReadHandleField { handle, .. } => {
            validate_operand(backend, function, initialized, values, handle)?
        }
    }
    Ok(())
}

fn validate_operand<'db>(
    backend: &Backend<'db>,
    function: &WrapperMirFunction<'db>,
    initialized: &[PlaceId],
    values: &FxHashMap<ValueId, WrapperPlaceTy>,
    operand: &Operand,
) -> Result<(), Diagnostic> {
    match operand {
        Operand::Value(value) => {
            if !values.contains_key(value) {
                return Err(Diagnostic::error(
                    "internal error: wrapper MIR referenced an unknown value",
                    backend.file_range(),
                ));
            }
        }
        Operand::Place(place) => {
            validate_place_initialized(backend, function, initialized, *place)?
        }
    }
    Ok(())
}

fn validate_place_initialized<'db>(
    backend: &Backend<'db>,
    function: &WrapperMirFunction<'db>,
    initialized: &[PlaceId],
    place: PlaceId,
) -> Result<(), Diagnostic> {
    if !function.places.iter().any(|decl| decl.id == place) {
        return Err(Diagnostic::error(
            "internal error: wrapper MIR referenced an unknown place",
            backend.file_range(),
        ));
    }
    if !initialized.contains(&place) {
        return Err(Diagnostic::error(
            "internal error: wrapper MIR used a place before initialization",
            backend.file_range(),
        ));
    }
    Ok(())
}

fn rvalue_writes_place(rhs: &RValue<'_>) -> Option<PlaceId> {
    match rhs {
        RValue::EncodeImmediate { dest, .. }
        | RValue::DecodeImmediate { dest, .. }
        | RValue::EncodeCanonical { dest, .. }
        | RValue::DecodeCanonical { dest, .. }
        | RValue::WrapHandle { dest, .. }
        | RValue::UnwrapHandle { dest, .. }
        | RValue::AllocTempBuffer { place: dest, .. } => Some(*dest),
        RValue::CallDirect { result, .. } | RValue::CallIndirect { result, .. } => *result,
        _ => None,
    }
}

#[cfg(test)]
impl<'db> WrapperMirBundle<'db> {
    pub(crate) fn dump(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        dump_group(&mut output, "wrapper_mir.imports", db, &self.imports);
        dump_group(&mut output, "wrapper_mir.callable_adapters", db, &self.callable_adapters);
        dump_group(&mut output, "wrapper_mir.trampolines", db, &self.trampolines);
        dump_group(&mut output, "wrapper_mir.exports", db, &self.exports);
        output
    }
}

#[cfg(test)]
fn dump_group(
    output: &mut String,
    label: &str,
    db: &dyn salsa::Database,
    group: &[WrapperMirFunction<'_>],
) {
    writeln!(output, "{label}:").expect("write to string");
    for function in group {
        writeln!(output, "  - [w{}] {}", function.id.0, function.debug_name).expect("write");
        writeln!(
            output,
            "    sig: ({}) -> ({})",
            function
                .signature
                .wasm_params
                .iter()
                .map(format_val_type)
                .collect::<Vec<_>>()
                .join(", "),
            function
                .signature
                .wasm_results
                .iter()
                .map(format_val_type)
                .collect::<Vec<_>>()
                .join(", ")
        )
        .expect("write");
        writeln!(output, "    places:").expect("write");
        for place in &function.places {
            writeln!(
                output,
                "      - [p{}] {}: {}{}",
                place.id.0,
                place.label,
                format_place_ty(place.ty),
                match place.kind {
                    PlaceKind::Local => "",
                    PlaceKind::Scratch(_) => " scratch",
                }
            )
            .expect("write");
        }
        writeln!(output, "    body:").expect("write");
        for stmt in &function.body.stmts {
            writeln!(output, "      - {}", format_stmt(db, stmt)).expect("write");
        }
    }
}

#[cfg(test)]
fn format_stmt(db: &dyn salsa::Database, stmt: &Stmt<'_>) -> String {
    match stmt {
        Stmt::Let { value, rhs, .. } => format!("v{} = {}", value.0, format_rvalue(db, rhs)),
        Stmt::Store { place, value } => format!("store p{} <- {}", place.0, format_operand(value)),
        Stmt::If { cond, result_places, .. } => format!(
            "if {} -> ({})",
            format_operand(cond),
            result_places
                .iter()
                .map(|place| format!("p{}", place.0))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Stmt::Eval { rhs } => format_rvalue(db, rhs),
        Stmt::Return { values } => {
            format!("return ({})", values.iter().map(format_operand).collect::<Vec<_>>().join(", "))
        }
    }
}

#[cfg(test)]
fn format_rvalue(db: &dyn salsa::Database, rhs: &RValue<'_>) -> String {
    match rhs {
        RValue::ReadParam { param } => format!("read_param {param}"),
        RValue::NormalizeBool { operand } => format!("normalize_bool {}", format_operand(operand)),
        RValue::LoadPlace { place } => format!("load p{}", place.0),
        RValue::CallDirect { target, args, result, .. } => format!(
            "call_direct {} ({}){}",
            match target {
                WrapperCallTarget::DirectFunction(instance)
                | WrapperCallTarget::RawImport(instance) => {
                    instance.location.source(db).name().map_or("<anonymous>", |name| name.as_str())
                }
            },
            args.iter().map(format_operand).collect::<Vec<_>>().join(", "),
            result.map_or_else(String::new, |place| format!(" -> p{}", place.0))
        ),
        RValue::CallIndirect { args, result, .. } => format!(
            "call_indirect ({}){}",
            args.iter().map(format_operand).collect::<Vec<_>>().join(", "),
            result.map_or_else(String::new, |place| format!(" -> p{}", place.0))
        ),
        RValue::EncodeImmediate { source, dest, slot } => {
            format!(
                "encode_immediate p{} -> p{} ({})",
                source.0,
                dest.0,
                slot.semantic_ty.display(db)
            )
        }
        RValue::DecodeImmediate { source, dest, slot } => {
            format!(
                "decode_immediate p{} -> p{} ({})",
                source.0,
                dest.0,
                slot.semantic_ty.display(db)
            )
        }
        RValue::EncodeCanonical { source, dest, slot } => {
            format!(
                "encode_canonical p{} -> p{} ({})",
                source.0,
                dest.0,
                slot.semantic_ty.display(db)
            )
        }
        RValue::DecodeCanonical { source, dest, slot } => {
            format!(
                "decode_canonical p{} -> p{} ({})",
                source.0,
                dest.0,
                slot.semantic_ty.display(db)
            )
        }
        RValue::WrapHandle { source, dest, slot } => {
            format!("wrap_handle p{} -> p{} ({})", source.0, dest.0, slot.semantic_ty.display(db))
        }
        RValue::UnwrapHandle { source, dest, slot } => {
            format!("unwrap_handle p{} -> p{} ({})", source.0, dest.0, slot.semantic_ty.display(db))
        }
        RValue::RetainNestedHandles { place, ty } => {
            format!("retain_nested_handles p{} ({})", place.0, ty.display(db))
        }
        RValue::ReleaseValue { place, abi } => {
            format!("release_value p{} ({})", place.0, format_abi_ty(abi))
        }
        RValue::ReleaseCanonicalBlob { place } => format!("release_blob p{}", place.0),
        RValue::ReleaseHandleObject { place } => format!("release_handle p{}", place.0),
        RValue::AllocTempBuffer { place, layout } => {
            format!("alloc_temp p{} size={} align={}", place.0, layout.size, layout.align)
        }
        RValue::DeallocTempBuffer { place, layout } => {
            format!("dealloc_temp p{} size={} align={}", place.0, layout.size, layout.align)
        }
        RValue::ZeroTempBuffer { place, size } => format!("zero_temp p{} size={size}", place.0),
        RValue::ReadHandleField { handle, field } => format!(
            "read_handle_field {}.{}",
            format_operand(handle),
            match field {
                WrapperHandleField::Slot => "slot",
                WrapperHandleField::Env => "env",
            }
        ),
    }
}

#[cfg(test)]
fn format_operand(operand: &Operand) -> String {
    match operand {
        Operand::Value(value) => format!("v{}", value.0),
        Operand::Place(place) => format!("p{}", place.0),
    }
}

#[cfg(test)]
fn format_place_ty(ty: WrapperPlaceTy) -> &'static str {
    match ty {
        WrapperPlaceTy::I32 => "i32",
        WrapperPlaceTy::I64 => "i64",
        WrapperPlaceTy::F64 => "f64",
        WrapperPlaceTy::Unit => "unit",
    }
}

#[cfg(test)]
fn format_abi_ty(abi: &AbiTy) -> String {
    match abi {
        AbiTy::Scalar(ty) => format!("{ty:?}"),
        AbiTy::Aggregate(layout) => {
            format!("aggregate(size={}, align={})", layout.size, layout.align)
        }
    }
}

#[cfg(test)]
fn format_val_type(ty: &ValType) -> &'static str {
    match ty {
        ValType::I32 => "i32",
        ValType::I64 => "i64",
        ValType::F64 => "f64",
        ValType::F32 => "f32",
        ValType::V128 => "v128",
        ValType::Ref(_) => "ref",
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use mitki_db::RootDatabase;
    use mitki_inputs::File;

    use super::*;

    fn compiler_for_fixture(fixture: &str) -> Backend<'_> {
        let db = Box::leak(Box::new(RootDatabase::default()));
        let file = File::new(db, "wrapper_mir_fixture.mitki".into(), fixture.to_owned());
        let diagnostics = mitki_analysis::check_file(db, file);
        assert!(
            diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        let runtime_diagnostics = mitki_analysis::check_runtime_file(db, file);
        assert!(
            runtime_diagnostics.is_empty(),
            "unexpected runtime diagnostics: {:?}",
            runtime_diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        let mut backend = Backend::new_file_with_options(
            db,
            file,
            crate::CompileOptions,
            Arc::new(crate::NoopComptimeEvaluator),
        );
        backend.collect_reachable_program();
        assert!(
            backend.diagnostics.is_empty(),
            "unexpected Backend diagnostics: {:?}",
            backend.diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        backend
    }

    fn assert_wrapper_mir_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        expected.assert_eq(&plan.wrapper_mir.dump(backend.db));
    }

    #[test]
    fn wrapper_mir_dump_tracks_bool_wrappers() {
        assert_wrapper_mir_dump(
            r#"
import "env" fun round_trip(flag: bool): bool;

export fun echo(flag: bool): bool {
    round_trip(flag)
}
"#,
            &expect![[r#"
                wrapper_mir.imports:
                  - [w0] import round_trip
                    sig: (i32) -> (i32)
                    places:
                      - [p0] param0: i32
                      - [p1] scratch.node_offset: i32 scratch
                      - [p2] scratch.cursor: i32 scratch
                      - [p3] scratch.index: i32 scratch
                      - [p4] scratch.len: i32 scratch
                      - [p5] scratch.count: i32 scratch
                      - [p6] scratch.bytes: i32 scratch
                      - [p7] scratch.base_id: i32 scratch
                      - [p8] scratch.temp_ptr: i32 scratch
                      - [p9] scratch.temp_ptr_aux: i32 scratch
                      - [p10] scratch.child_count: i32 scratch
                      - [p11] scratch.child_bytes: i32 scratch
                      - [p12] scratch.handle_count: i32 scratch
                      - [p13] scratch.handle_index: i32 scratch
                      - [p14] scratch.handle_cursor: i32 scratch
                      - [p15] scratch.f64_temp: f64 scratch
                      - [p16] result.lane: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = normalize_bool p0
                      - store p0 <- v1
                      - call_direct round_trip (p0) -> p16
                      - v2 = normalize_bool p16
                      - store p16 <- v2
                      - release_value p0 (Bool)
                      - return (p16)
                wrapper_mir.callable_adapters:
                  - [w1] callable echo
                    sig: (i32, i32) -> (i32)
                    places:
                      - [p0] env: i32
                      - [p1] param0: i32
                      - [p2] result: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - v2 = normalize_bool p1
                      - store p1 <- v2
                      - call_direct echo (p1) -> p2
                      - v3 = normalize_bool p2
                      - store p2 <- v3
                      - return (p2)
                  - [w2] callable round_trip
                    sig: (i32, i32) -> (i32)
                    places:
                      - [p0] env: i32
                      - [p1] param0: i32
                      - [p2] result: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - v2 = normalize_bool p1
                      - store p1 <- v2
                      - call_direct round_trip (p1) -> p2
                      - v3 = normalize_bool p2
                      - store p2 <- v3
                      - return (p2)
                wrapper_mir.trampolines:
                wrapper_mir.exports:
                  - [w3] export echo
                    sig: (i32) -> (i32)
                    places:
                      - [p0] param0.lane: i32
                      - [p1] scratch.node_offset: i32 scratch
                      - [p2] scratch.cursor: i32 scratch
                      - [p3] scratch.index: i32 scratch
                      - [p4] scratch.len: i32 scratch
                      - [p5] scratch.count: i32 scratch
                      - [p6] scratch.bytes: i32 scratch
                      - [p7] scratch.base_id: i32 scratch
                      - [p8] scratch.temp_ptr: i32 scratch
                      - [p9] scratch.temp_ptr_aux: i32 scratch
                      - [p10] scratch.child_count: i32 scratch
                      - [p11] scratch.child_bytes: i32 scratch
                      - [p12] scratch.handle_count: i32 scratch
                      - [p13] scratch.handle_index: i32 scratch
                      - [p14] scratch.handle_cursor: i32 scratch
                      - [p15] scratch.f64_temp: f64 scratch
                      - [p16] runtime.param0: i32
                      - [p17] runtime.result: i32
                      - [p18] result.out: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - decode_immediate p0 -> p16 (bool)
                      - v1 = normalize_bool p16
                      - store p16 <- v1
                      - call_direct echo (p16) -> p17
                      - encode_immediate p17 -> p18 (bool)
                      - v2 = normalize_bool p18
                      - store p18 <- v2
                      - release_value p16 (Bool)
                      - release_value p17 (Bool)
                      - return (p18)
            "#]],
        );
    }

    #[test]
    fn wrapper_mir_dump_tracks_canonical_wrappers() {
        assert_wrapper_mir_dump(
            r#"
import "env" fun mirror(xs: [str]): [str];

export fun words(): [str] {
    mirror(["a", "b"])
}
"#,
            &expect![[r#"
                wrapper_mir.imports:
                  - [w0] import mirror
                    sig: (i32) -> (i32)
                    places:
                      - [p0] param0: i32
                      - [p1] scratch.node_offset: i32 scratch
                      - [p2] scratch.cursor: i32 scratch
                      - [p3] scratch.index: i32 scratch
                      - [p4] scratch.len: i32 scratch
                      - [p5] scratch.count: i32 scratch
                      - [p6] scratch.bytes: i32 scratch
                      - [p7] scratch.base_id: i32 scratch
                      - [p8] scratch.temp_ptr: i32 scratch
                      - [p9] scratch.temp_ptr_aux: i32 scratch
                      - [p10] scratch.child_count: i32 scratch
                      - [p11] scratch.child_bytes: i32 scratch
                      - [p12] scratch.handle_count: i32 scratch
                      - [p13] scratch.handle_index: i32 scratch
                      - [p14] scratch.handle_cursor: i32 scratch
                      - [p15] scratch.f64_temp: f64 scratch
                      - [p16] param0.blob: i32
                      - [p17] result.blob: i32
                      - [p18] result.runtime: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - encode_canonical p0 -> p16 ([str])
                      - retain_nested_handles p16 ([str])
                      - call_direct mirror (p16) -> p17
                      - decode_canonical p17 -> p18 ([str])
                      - release_value p0 (Ref(Array(10254)))
                      - release_blob p16
                      - return (p18)
                wrapper_mir.callable_adapters:
                  - [w1] callable words
                    sig: (i32) -> (i32)
                    places:
                      - [p0] env: i32
                      - [p1] result: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - call_direct words () -> p1
                      - return (p1)
                  - [w2] callable mirror
                    sig: (i32, i32) -> (i32)
                    places:
                      - [p0] env: i32
                      - [p1] param0: i32
                      - [p2] result: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - call_direct mirror (p1) -> p2
                      - return (p2)
                wrapper_mir.trampolines:
                wrapper_mir.exports:
                  - [w3] export words
                    sig: () -> (i32)
                    places:
                      - [p0] scratch.node_offset: i32 scratch
                      - [p1] scratch.cursor: i32 scratch
                      - [p2] scratch.index: i32 scratch
                      - [p3] scratch.len: i32 scratch
                      - [p4] scratch.count: i32 scratch
                      - [p5] scratch.bytes: i32 scratch
                      - [p6] scratch.base_id: i32 scratch
                      - [p7] scratch.temp_ptr: i32 scratch
                      - [p8] scratch.temp_ptr_aux: i32 scratch
                      - [p9] scratch.child_count: i32 scratch
                      - [p10] scratch.child_bytes: i32 scratch
                      - [p11] scratch.handle_count: i32 scratch
                      - [p12] scratch.handle_index: i32 scratch
                      - [p13] scratch.handle_cursor: i32 scratch
                      - [p14] scratch.f64_temp: f64 scratch
                      - [p15] runtime.result: i32
                      - [p16] result.out: i32
                    body:
                      - call_direct words () -> p15
                      - encode_canonical p15 -> p16 ([str])
                      - retain_nested_handles p16 ([str])
                      - release_value p15 (Ref(Array(10254)))
                      - return (p16)
            "#]],
        );
    }

    #[test]
    fn wrapper_mir_dump_tracks_handle_wrappers_and_trampolines() {
        assert_wrapper_mir_dump(
            r#"
import "env" fun bounce(f: fun(int) -> int): fun(int) -> int;

fun add_one(x: int): int {
    x + 1
}

export fun id(f: fun(int) -> int): fun(int) -> int {
    bounce(f)
}

export fun make_adder(): fun(int) -> int {
    add_one
}
"#,
            &expect![[r#"
                wrapper_mir.imports:
                  - [w0] import bounce
                    sig: (i32, i32) -> ()
                    places:
                      - [p0] result_ptr: i32
                      - [p1] param0: i32
                      - [p2] scratch.node_offset: i32 scratch
                      - [p3] scratch.cursor: i32 scratch
                      - [p4] scratch.index: i32 scratch
                      - [p5] scratch.len: i32 scratch
                      - [p6] scratch.count: i32 scratch
                      - [p7] scratch.bytes: i32 scratch
                      - [p8] scratch.base_id: i32 scratch
                      - [p9] scratch.temp_ptr: i32 scratch
                      - [p10] scratch.temp_ptr_aux: i32 scratch
                      - [p11] scratch.child_count: i32 scratch
                      - [p12] scratch.child_bytes: i32 scratch
                      - [p13] scratch.handle_count: i32 scratch
                      - [p14] scratch.handle_index: i32 scratch
                      - [p15] scratch.handle_cursor: i32 scratch
                      - [p16] scratch.f64_temp: f64 scratch
                      - [p17] param0.handle: i32
                      - [p18] result.handle: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - wrap_handle p1 -> p17 (fun(int) -> int)
                      - call_direct bounce (p17) -> p18
                      - unwrap_handle p18 -> p0 (fun(int) -> int)
                      - release_value p1 (aggregate(size=8, align=4))
                      - dealloc_temp p1 size=8 align=4
                      - release_handle p17
                      - return ()
                wrapper_mir.callable_adapters:
                  - [w1] callable id
                    sig: (i32, i32, i32) -> ()
                    places:
                      - [p0] env: i32
                      - [p1] result_ptr: i32
                      - [p2] param0: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - v2 = read_param 2
                      - store p2 <- v2
                      - call_direct id (p1, p2)
                      - return ()
                  - [w2] callable make_adder
                    sig: (i32, i32) -> ()
                    places:
                      - [p0] env: i32
                      - [p1] result_ptr: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - call_direct make_adder (p1)
                      - return ()
                  - [w3] callable bounce
                    sig: (i32, i32, i32) -> ()
                    places:
                      - [p0] env: i32
                      - [p1] result_ptr: i32
                      - [p2] param0: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - v2 = read_param 2
                      - store p2 <- v2
                      - call_direct bounce (p1, p2)
                      - return ()
                  - [w4] callable add_one
                    sig: (i32, i32) -> (i32)
                    places:
                      - [p0] env: i32
                      - [p1] param0: i32
                      - [p2] result: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - call_direct add_one (p1) -> p2
                      - return (p2)
                wrapper_mir.trampolines:
                  - [w5] trampoline id
                    sig: (i32, i32) -> (i32)
                    places:
                      - [p0] handle: i32
                      - [p1] param0.lane: i32
                      - [p2] scratch.node_offset: i32 scratch
                      - [p3] scratch.cursor: i32 scratch
                      - [p4] scratch.index: i32 scratch
                      - [p5] scratch.len: i32 scratch
                      - [p6] scratch.count: i32 scratch
                      - [p7] scratch.bytes: i32 scratch
                      - [p8] scratch.base_id: i32 scratch
                      - [p9] scratch.temp_ptr: i32 scratch
                      - [p10] scratch.temp_ptr_aux: i32 scratch
                      - [p11] scratch.child_count: i32 scratch
                      - [p12] scratch.child_bytes: i32 scratch
                      - [p13] scratch.handle_count: i32 scratch
                      - [p14] scratch.handle_index: i32 scratch
                      - [p15] scratch.handle_cursor: i32 scratch
                      - [p16] scratch.f64_temp: f64 scratch
                      - [p17] runtime.param0: i32
                      - [p18] runtime.result: i32
                      - [p19] result.out: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - alloc_temp p17 size=8 align=4
                      - zero_temp p17 size=8
                      - unwrap_handle p1 -> p17 (fun(int) -> int)
                      - v2 = read_handle_field p0.env
                      - v3 = read_handle_field p0.slot
                      - alloc_temp p18 size=8 align=4
                      - zero_temp p18 size=8
                      - call_indirect (p18, p17)
                      - wrap_handle p18 -> p19 (fun(int) -> int)
                      - release_value p17 (aggregate(size=8, align=4))
                      - dealloc_temp p17 size=8 align=4
                      - release_value p18 (aggregate(size=8, align=4))
                      - dealloc_temp p18 size=8 align=4
                      - return (p19)
                  - [w6] trampoline make_adder
                    sig: (i32) -> (i32)
                    places:
                      - [p0] handle: i32
                      - [p1] scratch.node_offset: i32 scratch
                      - [p2] scratch.cursor: i32 scratch
                      - [p3] scratch.index: i32 scratch
                      - [p4] scratch.len: i32 scratch
                      - [p5] scratch.count: i32 scratch
                      - [p6] scratch.bytes: i32 scratch
                      - [p7] scratch.base_id: i32 scratch
                      - [p8] scratch.temp_ptr: i32 scratch
                      - [p9] scratch.temp_ptr_aux: i32 scratch
                      - [p10] scratch.child_count: i32 scratch
                      - [p11] scratch.child_bytes: i32 scratch
                      - [p12] scratch.handle_count: i32 scratch
                      - [p13] scratch.handle_index: i32 scratch
                      - [p14] scratch.handle_cursor: i32 scratch
                      - [p15] scratch.f64_temp: f64 scratch
                      - [p16] runtime.result: i32
                      - [p17] result.out: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_handle_field p0.env
                      - v2 = read_handle_field p0.slot
                      - alloc_temp p16 size=8 align=4
                      - zero_temp p16 size=8
                      - call_indirect (p16)
                      - wrap_handle p16 -> p17 (fun(int) -> int)
                      - release_value p16 (aggregate(size=8, align=4))
                      - dealloc_temp p16 size=8 align=4
                      - return (p17)
                  - [w7] trampoline bounce
                    sig: (i32, i32) -> (i32)
                    places:
                      - [p0] handle: i32
                      - [p1] param0.lane: i32
                      - [p2] scratch.node_offset: i32 scratch
                      - [p3] scratch.cursor: i32 scratch
                      - [p4] scratch.index: i32 scratch
                      - [p5] scratch.len: i32 scratch
                      - [p6] scratch.count: i32 scratch
                      - [p7] scratch.bytes: i32 scratch
                      - [p8] scratch.base_id: i32 scratch
                      - [p9] scratch.temp_ptr: i32 scratch
                      - [p10] scratch.temp_ptr_aux: i32 scratch
                      - [p11] scratch.child_count: i32 scratch
                      - [p12] scratch.child_bytes: i32 scratch
                      - [p13] scratch.handle_count: i32 scratch
                      - [p14] scratch.handle_index: i32 scratch
                      - [p15] scratch.handle_cursor: i32 scratch
                      - [p16] scratch.f64_temp: f64 scratch
                      - [p17] runtime.param0: i32
                      - [p18] runtime.result: i32
                      - [p19] result.out: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - alloc_temp p17 size=8 align=4
                      - zero_temp p17 size=8
                      - unwrap_handle p1 -> p17 (fun(int) -> int)
                      - v2 = read_handle_field p0.env
                      - v3 = read_handle_field p0.slot
                      - alloc_temp p18 size=8 align=4
                      - zero_temp p18 size=8
                      - call_indirect (p18, p17)
                      - wrap_handle p18 -> p19 (fun(int) -> int)
                      - release_value p17 (aggregate(size=8, align=4))
                      - dealloc_temp p17 size=8 align=4
                      - release_value p18 (aggregate(size=8, align=4))
                      - dealloc_temp p18 size=8 align=4
                      - return (p19)
                wrapper_mir.exports:
                  - [w8] export id
                    sig: (i32) -> (i32)
                    places:
                      - [p0] param0.lane: i32
                      - [p1] scratch.node_offset: i32 scratch
                      - [p2] scratch.cursor: i32 scratch
                      - [p3] scratch.index: i32 scratch
                      - [p4] scratch.len: i32 scratch
                      - [p5] scratch.count: i32 scratch
                      - [p6] scratch.bytes: i32 scratch
                      - [p7] scratch.base_id: i32 scratch
                      - [p8] scratch.temp_ptr: i32 scratch
                      - [p9] scratch.temp_ptr_aux: i32 scratch
                      - [p10] scratch.child_count: i32 scratch
                      - [p11] scratch.child_bytes: i32 scratch
                      - [p12] scratch.handle_count: i32 scratch
                      - [p13] scratch.handle_index: i32 scratch
                      - [p14] scratch.handle_cursor: i32 scratch
                      - [p15] scratch.f64_temp: f64 scratch
                      - [p16] runtime.param0: i32
                      - [p17] runtime.result: i32
                      - [p18] result.out: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - alloc_temp p16 size=8 align=4
                      - zero_temp p16 size=8
                      - unwrap_handle p0 -> p16 (fun(int) -> int)
                      - alloc_temp p17 size=8 align=4
                      - zero_temp p17 size=8
                      - call_direct id (p17, p16)
                      - wrap_handle p17 -> p18 (fun(int) -> int)
                      - release_value p16 (aggregate(size=8, align=4))
                      - dealloc_temp p16 size=8 align=4
                      - release_value p17 (aggregate(size=8, align=4))
                      - dealloc_temp p17 size=8 align=4
                      - return (p18)
                  - [w9] export make_adder
                    sig: () -> (i32)
                    places:
                      - [p0] scratch.node_offset: i32 scratch
                      - [p1] scratch.cursor: i32 scratch
                      - [p2] scratch.index: i32 scratch
                      - [p3] scratch.len: i32 scratch
                      - [p4] scratch.count: i32 scratch
                      - [p5] scratch.bytes: i32 scratch
                      - [p6] scratch.base_id: i32 scratch
                      - [p7] scratch.temp_ptr: i32 scratch
                      - [p8] scratch.temp_ptr_aux: i32 scratch
                      - [p9] scratch.child_count: i32 scratch
                      - [p10] scratch.child_bytes: i32 scratch
                      - [p11] scratch.handle_count: i32 scratch
                      - [p12] scratch.handle_index: i32 scratch
                      - [p13] scratch.handle_cursor: i32 scratch
                      - [p14] scratch.f64_temp: f64 scratch
                      - [p15] runtime.result: i32
                      - [p16] result.out: i32
                    body:
                      - alloc_temp p15 size=8 align=4
                      - zero_temp p15 size=8
                      - call_direct make_adder (p15)
                      - wrap_handle p15 -> p16 (fun(int) -> int)
                      - release_value p15 (aggregate(size=8, align=4))
                      - dealloc_temp p15 size=8 align=4
                      - return (p16)
            "#]],
        );
    }

    #[test]
    fn wrapper_mir_dump_tracks_generic_export_instances() {
        assert_wrapper_mir_dump(
            r#"
fun id[T](value: T): T {
    value
}

export instance id[int];
export instance id[bool];
"#,
            &expect![[r#"
                wrapper_mir.imports:
                wrapper_mir.callable_adapters:
                  - [w0] callable id
                    sig: (i32, i32) -> (i32)
                    places:
                      - [p0] env: i32
                      - [p1] param0: i32
                      - [p2] result: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - call_direct id (p1) -> p2
                      - return (p2)
                  - [w1] callable id
                    sig: (i32, i32) -> (i32)
                    places:
                      - [p0] env: i32
                      - [p1] param0: i32
                      - [p2] result: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - v1 = read_param 1
                      - store p1 <- v1
                      - v2 = normalize_bool p1
                      - store p1 <- v2
                      - call_direct id (p1) -> p2
                      - v3 = normalize_bool p2
                      - store p2 <- v3
                      - return (p2)
                wrapper_mir.trampolines:
                wrapper_mir.exports:
                  - [w2] export id
                    sig: (i32) -> (i32)
                    places:
                      - [p0] param0.lane: i32
                      - [p1] scratch.node_offset: i32 scratch
                      - [p2] scratch.cursor: i32 scratch
                      - [p3] scratch.index: i32 scratch
                      - [p4] scratch.len: i32 scratch
                      - [p5] scratch.count: i32 scratch
                      - [p6] scratch.bytes: i32 scratch
                      - [p7] scratch.base_id: i32 scratch
                      - [p8] scratch.temp_ptr: i32 scratch
                      - [p9] scratch.temp_ptr_aux: i32 scratch
                      - [p10] scratch.child_count: i32 scratch
                      - [p11] scratch.child_bytes: i32 scratch
                      - [p12] scratch.handle_count: i32 scratch
                      - [p13] scratch.handle_index: i32 scratch
                      - [p14] scratch.handle_cursor: i32 scratch
                      - [p15] scratch.f64_temp: f64 scratch
                      - [p16] runtime.param0: i32
                      - [p17] runtime.result: i32
                      - [p18] result.out: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - decode_immediate p0 -> p16 (int)
                      - call_direct id (p16) -> p17
                      - encode_immediate p17 -> p18 (int)
                      - release_value p16 (Int)
                      - release_value p17 (Int)
                      - return (p18)
                  - [w3] export id
                    sig: (i32) -> (i32)
                    places:
                      - [p0] param0.lane: i32
                      - [p1] scratch.node_offset: i32 scratch
                      - [p2] scratch.cursor: i32 scratch
                      - [p3] scratch.index: i32 scratch
                      - [p4] scratch.len: i32 scratch
                      - [p5] scratch.count: i32 scratch
                      - [p6] scratch.bytes: i32 scratch
                      - [p7] scratch.base_id: i32 scratch
                      - [p8] scratch.temp_ptr: i32 scratch
                      - [p9] scratch.temp_ptr_aux: i32 scratch
                      - [p10] scratch.child_count: i32 scratch
                      - [p11] scratch.child_bytes: i32 scratch
                      - [p12] scratch.handle_count: i32 scratch
                      - [p13] scratch.handle_index: i32 scratch
                      - [p14] scratch.handle_cursor: i32 scratch
                      - [p15] scratch.f64_temp: f64 scratch
                      - [p16] runtime.param0: i32
                      - [p17] runtime.result: i32
                      - [p18] result.out: i32
                    body:
                      - v0 = read_param 0
                      - store p0 <- v0
                      - decode_immediate p0 -> p16 (bool)
                      - v1 = normalize_bool p16
                      - store p16 <- v1
                      - call_direct id (p16) -> p17
                      - encode_immediate p17 -> p18 (bool)
                      - v2 = normalize_bool p18
                      - store p18 <- v2
                      - release_value p16 (Bool)
                      - release_value p17 (Bool)
                      - return (p18)
            "#]],
        );
    }

    #[test]
    fn wrapper_mir_dump_is_stable_across_repeated_planning() {
        let backend = compiler_for_fixture(
            r#"
import "env" fun round_trip(xs: [int]): [int];

export fun main(): [int] {
    round_trip([20, 22])
}
"#,
        );
        let first = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .wrapper_mir
            .dump(backend.db);
        let second = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .wrapper_mir
            .dump(backend.db);
        assert_eq!(first, second);
    }

    #[test]
    fn wrapper_mir_validator_rejects_return_arity_mismatches() {
        let backend = compiler_for_fixture(
            r#"
export fun answer(): int {
    42
}
"#,
        );
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        let mut broken = plan
            .wrapper_mir
            .exports
            .first()
            .expect("expected an export wrapper MIR function")
            .clone();
        let Some(Stmt::Return { values }) = broken.body.stmts.last_mut() else {
            panic!("expected wrapper MIR to end in a return");
        };
        values.clear();
        let error = validate_wrapper_mir_function(&backend, &plan, &broken)
            .expect_err("broken wrapper MIR should fail validation");
        assert!(
            error.message().contains("return arity drifted"),
            "unexpected diagnostic: {}",
            error.message()
        );
    }
}
