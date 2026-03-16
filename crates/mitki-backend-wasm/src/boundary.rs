use std::collections::BTreeSet;

use mitki_abi::{
    AbiTypeKind as AbiV2TypeKind, ExecutionDomain, LinkageKind as AbiV2LinkageKind,
    SemanticTypeGraph, TransportClass, TypeId,
};
use mitki_abi_lower::{BoundaryFunctionMetadata, BuiltAbiV2, build_module_abi_v2};
use mitki_errors::Diagnostic;
use mitki_hir::ty::{Ty, TyKind};
use rustc_hash::FxHashMap;

use super::plan::FunctionInstanceId;
use super::{BoundaryTransportPlan, *};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct InternalSig {
    pub(in crate::backend) params: Vec<AbiTy>,
    pub(in crate::backend) results: Vec<AbiTy>,
}

impl InternalSig {
    pub(in crate::backend) fn from_function_signature(signature: FunctionSignature) -> Self {
        let FunctionSignature { params, result } = signature;
        Self { params, results: unit_result_to_vec(result) }
    }

    pub(in crate::backend) fn as_function_signature(&self) -> FunctionSignature {
        assert!(
            self.results.len() <= 1,
            "shadow internal signatures only support 0 or 1 result in Step 1",
        );
        FunctionSignature {
            params: self.params.clone(),
            result: self.results.first().cloned().unwrap_or(AbiTy::Scalar(BackendTy::Unit)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct BoundarySlot<'db> {
    pub(in crate::backend) semantic_ty: Ty<'db>,
    pub(in crate::backend) runtime_abi: AbiTy,
    pub(in crate::backend) transport: BoundaryTransportPlan,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct BoundarySig<'db> {
    pub(in crate::backend) internal: InternalSig,
    pub(in crate::backend) params: Vec<BoundarySlot<'db>>,
    pub(in crate::backend) results: Vec<BoundarySlot<'db>>,
}

impl<'db> BoundarySig<'db> {
    pub(in crate::backend) fn from_boundary_signatures(
        signatures: &BoundaryFunctionSignatures<'db>,
    ) -> Self {
        let params = signatures
            .param_tys
            .iter()
            .copied()
            .zip(signatures.param_transport_plans.iter().cloned())
            .zip(signatures.param_runtime_abis.iter().cloned())
            .map(|((semantic_ty, transport), runtime_abi)| BoundarySlot {
                semantic_ty,
                runtime_abi,
                transport,
            })
            .collect::<Vec<_>>();
        let results = unit_boundary_result_to_vec(
            signatures.result_ty,
            signatures.result_runtime_abi.clone(),
            signatures.result_transport_plan,
        );
        Self {
            internal: InternalSig::from_function_signature(signatures.internal_signature()),
            params,
            results,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::backend) enum WrapperDirection {
    ImportThunk,
    ExportWrapper,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) enum TransportOp<'db> {
    ReadLane { lane: u32, ty: Ty<'db> },
    NormalizeBool { lane: u32 },
    DecodeCanonical { lane: u32, ty: Ty<'db> },
    EncodeCanonical { lane: u32, ty: Ty<'db> },
    HandleToFunction { lane: u32, ty: Ty<'db> },
    FunctionToHandle { lane: u32, ty: Ty<'db> },
    RetainNestedHandles { ty: Ty<'db> },
    ReleaseCanonicalTemp { lane: u32 },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct BoundaryWrapperPlan<'db> {
    pub(in crate::backend) direction: WrapperDirection,
    pub(in crate::backend) signature: BoundarySig<'db>,
    pub(in crate::backend) param_ops: Vec<TransportOp<'db>>,
    pub(in crate::backend) result_ops: Vec<TransportOp<'db>>,
}

impl<'db> BoundaryWrapperPlan<'db> {
    fn import(signature: BoundarySig<'db>, db: &'db dyn salsa::Database) -> Self {
        let (param_ops, result_ops) =
            transport_ops_for_direction(db, WrapperDirection::ImportThunk, &signature);
        Self { direction: WrapperDirection::ImportThunk, signature, param_ops, result_ops }
    }

    fn export(signature: BoundarySig<'db>, db: &'db dyn salsa::Database) -> Self {
        let (param_ops, result_ops) =
            transport_ops_for_direction(db, WrapperDirection::ExportWrapper, &signature);
        Self { direction: WrapperDirection::ExportWrapper, signature, param_ops, result_ops }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct ImportBoundaryPlan<'db> {
    pub(in crate::backend) function_id: FunctionInstanceId,
    pub(in crate::backend) instance: InstanceKey<'db>,
    pub(in crate::backend) metadata_index: usize,
    pub(in crate::backend) logical_name: String,
    pub(in crate::backend) module_name: String,
    pub(in crate::backend) field_name: String,
    pub(in crate::backend) wrapper: BoundaryWrapperPlan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct ExportBoundaryPlan<'db> {
    pub(in crate::backend) function_id: FunctionInstanceId,
    pub(in crate::backend) instance: InstanceKey<'db>,
    pub(in crate::backend) metadata_index: usize,
    pub(in crate::backend) logical_name: String,
    pub(in crate::backend) export_name: String,
    pub(in crate::backend) wrapper: BoundaryWrapperPlan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct BoundaryAliasPlan<'db> {
    pub(in crate::backend) alias: String,
    pub(in crate::backend) instance: InstanceKey<'db>,
    pub(in crate::backend) function_id: FunctionInstanceId,
    pub(in crate::backend) metadata_index: usize,
    pub(in crate::backend) logical_name: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct BoundaryInstancePlan<'db> {
    pub(in crate::backend) function_id: FunctionInstanceId,
    pub(in crate::backend) instance: InstanceKey<'db>,
    pub(in crate::backend) metadata_index: usize,
    pub(in crate::backend) logical_name: String,
    pub(in crate::backend) generic_origin_name: Option<String>,
    pub(in crate::backend) wasm_module_name: Option<String>,
    pub(in crate::backend) wasm_field_name: String,
    pub(in crate::backend) domain: ExecutionDomain,
    pub(in crate::backend) linkage: AbiV2LinkageKind,
    pub(in crate::backend) signature: BoundarySig<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct AbiBoundaryMetadataPlan<'db> {
    pub(in crate::backend) functions: Vec<BoundaryFunctionMetadata<'db>>,
}

impl<'db> AbiBoundaryMetadataPlan<'db> {
    pub(in crate::backend) fn build_preview(
        &self,
        db: &'db dyn salsa::Database,
    ) -> Result<BuiltAbiV2, String> {
        build_module_abi_v2(db, &self.functions)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoundaryPlan<'db> {
    pub(in crate::backend) imports: Vec<ImportBoundaryPlan<'db>>,
    pub(in crate::backend) exports: Vec<ExportBoundaryPlan<'db>>,
    pub(in crate::backend) instances: Vec<BoundaryInstancePlan<'db>>,
    pub(in crate::backend) aliases: Vec<BoundaryAliasPlan<'db>>,
    pub(in crate::backend) metadata: AbiBoundaryMetadataPlan<'db>,
    pub(in crate::backend) instance_indices: FxHashMap<InstanceKey<'db>, usize>,
    pub(in crate::backend) import_indices: FxHashMap<InstanceKey<'db>, usize>,
    pub(in crate::backend) export_indices: FxHashMap<InstanceKey<'db>, usize>,
}

impl<'db> BoundaryPlan<'db> {}

pub(in crate::backend) struct BoundaryPlanner;

impl BoundaryPlanner {
    pub(in crate::backend) fn build<'db>(
        backend: &Backend<'db>,
        reachability: &plan::ReachabilityGraph<'db>,
        function_ids: &FxHashMap<InstanceKey<'db>, FunctionInstanceId>,
    ) -> Result<BoundaryPlan<'db>, Diagnostic> {
        let reachable_functions = &reachability.functions;
        let mut imports = Vec::new();
        let mut exports = Vec::new();
        let mut instances = Vec::new();
        let mut aliases = Vec::with_capacity(reachability.exports.len());
        let mut metadata_functions = Vec::new();
        let export_names = reachability
            .roots
            .iter()
            .map(|root| (root.instance.clone(), root.logical_name.clone()))
            .collect::<FxHashMap<_, _>>();

        for instance in reachable_functions {
            let function = instance.location.hir_function(backend.db).function(backend.db);
            if !matches!(
                function.linkage(),
                WasmLinkage::Import { .. } | WasmLinkage::RawImport { .. }
            ) {
                continue;
            }
            let inference = instance.location.infer(backend.db);
            let source = instance.location.source(backend.db);
            let name = source.name().expect("reachable import should have a name");
            let WasmLinkage::Import { module } = function.linkage() else {
                continue;
            };
            let signatures = backend.boundary_function_signatures(instance, function, inference)?;
            let signature = BoundarySig::from_boundary_signatures(&signatures);
            let metadata_index = metadata_functions.len();
            let wasm_field_name = mitki_abi::typed_boundary_wasm_name(metadata_index);
            let generic_origin_name =
                (!instance.type_args.is_empty()).then(|| name.as_str().to_owned());
            let function_id = *function_ids
                .get(instance)
                .expect("reachable boundary import should have a function id");

            metadata_functions.push(BoundaryFunctionMetadata {
                logical_name: name.as_str().to_owned(),
                generic_origin_name: generic_origin_name.clone(),
                wasm_module_name: Some(module.text(backend.db).to_owned()),
                wasm_field_name: wasm_field_name.clone(),
                param_tys: signature.params.iter().map(|slot| slot.semantic_ty).collect(),
                result_ty: signatures.result_ty,
                type_args: instance.type_args.clone(),
                domain: ExecutionDomain::Runtime,
                linkage: AbiV2LinkageKind::WasmImport,
            });

            instances.push(BoundaryInstancePlan {
                function_id,
                instance: instance.clone(),
                metadata_index,
                logical_name: name.as_str().to_owned(),
                generic_origin_name,
                wasm_module_name: Some(module.text(backend.db).to_owned()),
                wasm_field_name: wasm_field_name.clone(),
                domain: ExecutionDomain::Runtime,
                linkage: AbiV2LinkageKind::WasmImport,
                signature: signature.clone(),
            });

            imports.push(ImportBoundaryPlan {
                function_id,
                instance: instance.clone(),
                metadata_index,
                logical_name: name.as_str().to_owned(),
                module_name: module.text(backend.db).to_owned(),
                field_name: wasm_field_name,
                wrapper: BoundaryWrapperPlan::import(signature, backend.db),
            });
        }

        for instance in &reachability.exports {
            let logical_name = export_names.get(instance).cloned().ok_or_else(|| {
                Diagnostic::error(
                    "internal error: reachable export was missing its logical-name root",
                    backend.function_range(instance.location),
                )
            })?;
            let hir_function = instance.location.hir_function(backend.db);
            let function = hir_function.function(backend.db);
            let inference = instance.location.infer(backend.db);
            let signatures = backend.boundary_function_signatures(instance, function, inference)?;
            let signature = BoundarySig::from_boundary_signatures(&signatures);
            let emit_start_alias = matches!(function.linkage(), WasmLinkage::ImplicitMainExport)
                && signature.params.is_empty()
                && signature.results.is_empty();
            let metadata_index = metadata_functions.len();
            let wasm_field_name = mitki_abi::typed_boundary_wasm_name(metadata_index);
            let generic_origin_name =
                (!instance.type_args.is_empty()).then_some(logical_name.clone());
            let function_id = *function_ids
                .get(instance)
                .expect("reachable boundary export should have a function id");
            let domain = if backend.is_stage_mode() {
                ExecutionDomain::Stage
            } else {
                ExecutionDomain::Runtime
            };

            metadata_functions.push(BoundaryFunctionMetadata {
                logical_name: logical_name.clone(),
                generic_origin_name: generic_origin_name.clone(),
                wasm_module_name: None,
                wasm_field_name: wasm_field_name.clone(),
                param_tys: signature.params.iter().map(|slot| slot.semantic_ty).collect(),
                result_ty: signatures.result_ty,
                type_args: instance.type_args.clone(),
                domain,
                linkage: AbiV2LinkageKind::WasmExport,
            });

            instances.push(BoundaryInstancePlan {
                function_id,
                instance: instance.clone(),
                metadata_index,
                logical_name: logical_name.clone(),
                generic_origin_name,
                wasm_module_name: None,
                wasm_field_name: wasm_field_name.clone(),
                domain,
                linkage: AbiV2LinkageKind::WasmExport,
                signature: signature.clone(),
            });

            exports.push(ExportBoundaryPlan {
                function_id,
                instance: instance.clone(),
                metadata_index,
                logical_name: logical_name.clone(),
                export_name: wasm_field_name.clone(),
                wrapper: BoundaryWrapperPlan::export(signature, backend.db),
            });

            aliases.push(BoundaryAliasPlan {
                alias: wasm_field_name,
                instance: instance.clone(),
                function_id,
                metadata_index,
                logical_name,
            });
            if emit_start_alias {
                aliases.push(BoundaryAliasPlan {
                    alias: "_start".to_owned(),
                    instance: instance.clone(),
                    function_id,
                    metadata_index,
                    logical_name: "main".to_owned(),
                });
            }
        }

        let instance_indices = instances
            .iter()
            .enumerate()
            .map(|(index, entry)| (entry.instance.clone(), index))
            .collect::<FxHashMap<_, _>>();
        let import_indices = imports
            .iter()
            .map(|plan| (plan.instance.clone(), plan.metadata_index))
            .collect::<FxHashMap<_, _>>();
        let export_indices = exports
            .iter()
            .map(|plan| (plan.instance.clone(), plan.metadata_index))
            .collect::<FxHashMap<_, _>>();

        Ok(BoundaryPlan {
            imports,
            exports,
            instances,
            aliases,
            metadata: AbiBoundaryMetadataPlan { functions: metadata_functions },
            instance_indices,
            import_indices,
            export_indices,
        })
    }
}

impl<'db> Backend<'db> {
    pub(super) fn boundary_function_signatures(
        &self,
        instance: &InstanceKey<'db>,
        function: &'db Function<'db>,
        inference: &'db mitki_typeck::infer::Inference<'db>,
    ) -> Result<BoundaryFunctionSignatures<'db>, Diagnostic> {
        let (param_tys, result_ty) =
            self.function_signature_types(instance, function, inference)?;
        let transport_profile = self.boundary_transport_profile();
        let signature_context = transport_profile.boundary_signature_context();
        let param_transport_plans = param_tys
            .iter()
            .copied()
            .map(|ty| {
                transport_profile.plan_or_message(self.db, ty, &signature_context).map_err(
                    |message| Diagnostic::error(message, self.function_range(instance.location)),
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        let param_runtime_abis = param_tys
            .iter()
            .copied()
            .map(|ty| {
                crate::capability::supported_value_abi_or_message(self.db, ty, &signature_context)
                    .map_err(|message| {
                        Diagnostic::error(message, self.function_range(instance.location))
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let result_transport_plan =
            transport_profile.plan_or_message(self.db, result_ty, &signature_context).map_err(
                |message| Diagnostic::error(message, self.function_range(instance.location)),
            )?;
        let result_runtime_abi = crate::capability::supported_value_abi_or_message(
            self.db,
            result_ty,
            &signature_context,
        )
        .map_err(|message| Diagnostic::error(message, self.function_range(instance.location)))?;
        Ok(BoundaryFunctionSignatures {
            param_tys,
            result_ty,
            param_transport_plans,
            result_transport_plan,
            param_runtime_abis,
            result_runtime_abi,
        })
    }
}

pub(in crate::backend) fn semantic_type_kind(
    graph: &SemanticTypeGraph,
    ty: TypeId,
) -> Result<&AbiV2TypeKind, Diagnostic> {
    graph.types.get(ty.0 as usize).map(|node| &node.kind).ok_or_else(|| {
        Diagnostic::error(
            format!("internal error: unknown ABI v2 type `{}`", ty.0),
            mitki_errors::TextRange::default(),
        )
    })
}

fn semantic_type_edges(kind: &AbiV2TypeKind) -> Vec<TypeId> {
    match kind {
        AbiV2TypeKind::Array { elem } => vec![*elem],
        AbiV2TypeKind::Tuple { elems } => elems.clone(),
        AbiV2TypeKind::Record { fields } | AbiV2TypeKind::Struct { fields, .. } => {
            fields.iter().map(|field| field.ty).collect()
        }
        AbiV2TypeKind::Enum { variants, .. } => {
            variants.iter().flat_map(|variant| variant.fields.iter().copied()).collect()
        }
        AbiV2TypeKind::Union { members } => members.clone(),
        AbiV2TypeKind::Intersection { members, carrier, .. } => {
            let mut edges = members.clone();
            edges.push(*carrier);
            edges
        }
        AbiV2TypeKind::Function { params, result, .. } => {
            let mut edges = params.clone();
            edges.push(*result);
            edges
        }
        AbiV2TypeKind::Unit
        | AbiV2TypeKind::Bool
        | AbiV2TypeKind::Int { .. }
        | AbiV2TypeKind::Float { .. }
        | AbiV2TypeKind::Char
        | AbiV2TypeKind::String
        | AbiV2TypeKind::Opaque { .. } => Vec::new(),
    }
}

pub(in crate::backend) fn semantic_type_uses_recursive_group(
    graph: &SemanticTypeGraph,
    root: TypeId,
) -> Result<bool, Diagnostic> {
    let mut stack = vec![root];
    let mut seen = BTreeSet::new();
    while let Some(ty) = stack.pop() {
        if !seen.insert(ty) {
            continue;
        }
        let node = graph.types.get(ty.0 as usize).ok_or_else(|| {
            Diagnostic::error(
                format!("internal error: unknown ABI v2 type `{}`", ty.0),
                mitki_errors::TextRange::default(),
            )
        })?;
        if node.recursive_group.is_some() {
            return Ok(true);
        }
        stack.extend(semantic_type_edges(&node.kind));
    }
    Ok(false)
}

pub(in crate::backend) fn semantic_type_is_immediate(
    graph: &SemanticTypeGraph,
    ty: TypeId,
) -> Result<bool, Diagnostic> {
    Ok(match semantic_type_kind(graph, ty)? {
        AbiV2TypeKind::Unit
        | AbiV2TypeKind::Bool
        | AbiV2TypeKind::Int { .. }
        | AbiV2TypeKind::Float { .. }
        | AbiV2TypeKind::Char => true,
        AbiV2TypeKind::Enum { variants, .. } => {
            variants.iter().all(|variant| variant.fields.is_empty())
        }
        AbiV2TypeKind::Function { .. } | AbiV2TypeKind::Opaque { .. } => false,
        AbiV2TypeKind::String
        | AbiV2TypeKind::Array { .. }
        | AbiV2TypeKind::Tuple { .. }
        | AbiV2TypeKind::Record { .. }
        | AbiV2TypeKind::Struct { .. }
        | AbiV2TypeKind::Union { .. }
        | AbiV2TypeKind::Intersection { .. } => false,
    })
}

pub(in crate::backend) fn transport_has_wasm_lane(
    graph: &SemanticTypeGraph,
    transport: &mitki_abi::TransportRef,
) -> Result<bool, Diagnostic> {
    Ok(
        match (transport.transport_class, semantic_type_kind(graph, transport_type_id(transport))?)
        {
            (TransportClass::Immediate, AbiV2TypeKind::Unit) => false,
            (TransportClass::Immediate, AbiV2TypeKind::Tuple { elems }) if elems.is_empty() => {
                false
            }
            _ => true,
        },
    )
}

pub(in crate::backend) fn transport_type_id(transport: &mitki_abi::TransportRef) -> TypeId {
    transport.transport_type.unwrap_or(transport.semantic_type)
}

fn unit_result_to_vec(result: AbiTy) -> Vec<AbiTy> {
    match result {
        AbiTy::Scalar(BackendTy::Unit) => Vec::new(),
        other => vec![other],
    }
}

fn unit_boundary_result_to_vec<'db>(
    semantic_ty: Ty<'db>,
    runtime_abi: AbiTy,
    transport: BoundaryTransportPlan,
) -> Vec<BoundarySlot<'db>> {
    if matches!(runtime_abi, AbiTy::Scalar(BackendTy::Unit)) {
        Vec::new()
    } else {
        vec![BoundarySlot { semantic_ty, runtime_abi, transport }]
    }
}

fn boundary_slot_lane_count(slot: &BoundarySlot<'_>) -> u32 {
    if matches!(slot.runtime_abi, AbiTy::Scalar(BackendTy::Unit)) { 0 } else { 1 }
}

fn transport_ops_for_direction<'db>(
    db: &'db dyn salsa::Database,
    direction: WrapperDirection,
    signature: &BoundarySig<'db>,
) -> (Vec<TransportOp<'db>>, Vec<TransportOp<'db>>) {
    let mut param_lane = 0u32;
    let param_ops = signature
        .params
        .iter()
        .flat_map(|slot| {
            let lane = param_lane;
            param_lane += boundary_slot_lane_count(slot);
            slot_transport_ops(db, direction, true, lane, slot)
        })
        .collect::<Vec<_>>();

    let mut result_lane = 0u32;
    let result_ops = signature
        .results
        .iter()
        .flat_map(|slot| {
            let lane = result_lane;
            result_lane += boundary_slot_lane_count(slot);
            slot_transport_ops(db, direction, false, lane, slot)
        })
        .collect::<Vec<_>>();

    (param_ops, result_ops)
}

fn slot_transport_ops<'db>(
    db: &'db dyn salsa::Database,
    direction: WrapperDirection,
    is_param: bool,
    lane: u32,
    slot: &BoundarySlot<'db>,
) -> Vec<TransportOp<'db>> {
    let mut ops = Vec::new();
    match slot.transport.transport_class {
        TransportClass::Immediate => {
            if boundary_slot_lane_count(slot) != 0 {
                ops.push(TransportOp::ReadLane { lane, ty: slot.semantic_ty });
                if matches!(slot.semantic_ty.kind(db), TyKind::Bool) {
                    ops.push(TransportOp::NormalizeBool { lane });
                }
            }
        }
        TransportClass::CanonicalValue => {
            let encode = match (direction, is_param) {
                (WrapperDirection::ImportThunk, true) => true,
                (WrapperDirection::ImportThunk, false) => false,
                (WrapperDirection::ExportWrapper, true) => false,
                (WrapperDirection::ExportWrapper, false) => true,
            };
            if encode {
                ops.push(TransportOp::EncodeCanonical { lane, ty: slot.semantic_ty });
                ops.push(TransportOp::RetainNestedHandles { ty: slot.semantic_ty });
            } else {
                ops.push(TransportOp::DecodeCanonical { lane, ty: slot.semantic_ty });
            }
            ops.push(TransportOp::ReleaseCanonicalTemp { lane });
        }
        TransportClass::CapabilityHandle => {
            let to_handle = match (direction, is_param) {
                (WrapperDirection::ImportThunk, true) => true,
                (WrapperDirection::ImportThunk, false) => false,
                (WrapperDirection::ExportWrapper, true) => false,
                (WrapperDirection::ExportWrapper, false) => true,
            };
            if to_handle {
                ops.push(TransportOp::FunctionToHandle { lane, ty: slot.semantic_ty });
            } else {
                ops.push(TransportOp::HandleToFunction { lane, ty: slot.semantic_ty });
            }
        }
    }
    ops
}
