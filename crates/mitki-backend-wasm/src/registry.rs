use std::collections::BTreeSet;

use mitki_abi::SigId;
use mitki_abi_lower::BuiltAbiV2;
use mitki_errors::Diagnostic;
use rustc_hash::FxHashMap;

use super::boundary::BoundaryPlan;
use super::plan::{
    CallableAdapterNeed, EmissionObligations, FunctionInstanceId, FunctionInstancePlan, HelperNeed,
    NamePlan, ReachabilityGraph,
};
use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::backend) struct HelperId(pub(in crate::backend) u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct HelperRegistryPlan {
    pub(in crate::backend) needs: Vec<HelperNeed>,
    pub(in crate::backend) builtin_helpers: Vec<HelperFunction>,
    pub(in crate::backend) exported_helpers: Vec<HelperNeed>,
    pub(in crate::backend) helper_ids: FxHashMap<HelperNeed, HelperId>,
}

impl HelperRegistryPlan {
    pub(in crate::backend) fn helper_export_names(&self) -> Vec<String> {
        self.exported_helpers.iter().filter_map(|helper| helper.export_name()).collect()
    }

    pub(in crate::backend) fn contains(&self, need: HelperNeed) -> bool {
        self.helper_ids.contains_key(&need)
    }
}

pub(in crate::backend) struct HelperRegistry;

impl HelperRegistry {
    pub(in crate::backend) fn build<'db>(
        obligations: &EmissionObligations<'db>,
        boundary: &BoundaryPlan<'db>,
        abi_preview: &BuiltAbiV2,
    ) -> HelperRegistryPlan {
        let mut helpers = BTreeSet::new();
        for helper in &obligations.helpers {
            helpers.insert(HelperNeed::from_helper_function(*helper));
        }
        for ty in &obligations.reachable_nominals {
            let bits = nominal_ty_bits(*ty);
            helpers.insert(HelperNeed::NominalDestroy(bits));
            helpers.insert(HelperNeed::NominalEq(bits));
        }
        for ty in &obligations.reachable_arrays {
            let bits = array_ty_bits(*ty);
            helpers.insert(HelperNeed::ArrayDestroy(bits));
            helpers.insert(HelperNeed::ArrayEq(bits));
        }
        if obligations.needs_blob_helpers() {
            helpers.insert(HelperNeed::AbiAlloc);
            helpers.insert(HelperNeed::AbiBlobRelease);
        }
        if obligations.needs_handle_helpers() {
            helpers.insert(HelperNeed::AbiHandleRetain);
            helpers.insert(HelperNeed::AbiHandleRelease);
        }
        for adapter in &obligations.callable_adapters {
            let CallableAdapterNeed::BoundaryInvoke(instance) = adapter else {
                continue;
            };
            let Some(&metadata_index) = boundary
                .import_indices
                .get(instance)
                .or_else(|| boundary.export_indices.get(instance))
            else {
                continue;
            };
            if let Some(function) = abi_preview.functions.get(metadata_index) {
                helpers.insert(HelperNeed::HandleInvoke(function.signature_id));
            }
        }

        let needs = helpers.into_iter().collect::<Vec<_>>();
        let helper_ids = needs
            .iter()
            .enumerate()
            .map(|(index, helper)| {
                (
                    *helper,
                    HelperId(u32::try_from(index).expect("helper registry ids should fit in u32")),
                )
            })
            .collect();
        let exported_helpers =
            needs.iter().copied().filter(|helper| helper.export_name().is_some()).collect();

        HelperRegistryPlan {
            needs,
            builtin_helpers: HelperFunction::all().into_iter().collect(),
            exported_helpers,
            helper_ids,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::backend) struct CallableSigId(pub(in crate::backend) u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct CallableSignaturePlan {
    pub(in crate::backend) id: CallableSigId,
    pub(in crate::backend) signature: FunctionSignature,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct CallableTrampolinePlan<'db> {
    pub(in crate::backend) signature_id: SigId,
    pub(in crate::backend) function_id: FunctionInstanceId,
    pub(in crate::backend) instance: InstanceKey<'db>,
    pub(in crate::backend) metadata_index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct CallableRegistryPlan<'db> {
    pub(in crate::backend) signatures: Vec<CallableSignaturePlan>,
    pub(in crate::backend) signature_ids: FxHashMap<FunctionSignature, CallableSigId>,
    pub(in crate::backend) invoke_trampolines: Vec<CallableTrampolinePlan<'db>>,
}

pub(in crate::backend) struct CallableRegistry;

impl CallableRegistry {
    pub(in crate::backend) fn build<'db>(
        backend: &Backend<'db>,
        reachability: &ReachabilityGraph<'db>,
        function_instances: &[FunctionInstancePlan<'db>],
        boundary: &BoundaryPlan<'db>,
        obligations: &EmissionObligations<'db>,
        abi_preview: &BuiltAbiV2,
    ) -> Result<CallableRegistryPlan<'db>, Diagnostic> {
        let function_ids = function_instances
            .iter()
            .map(|function| (function.instance.clone(), function.id))
            .collect::<FxHashMap<_, _>>();
        let mut signatures = Vec::new();
        let mut signature_ids = FxHashMap::default();

        for instance in &reachability.functions {
            let hir_function = instance.location.hir_function(backend.db);
            let function = hir_function.function(backend.db);
            let inference = instance.location.infer(backend.db);
            let signature = backend.function_signature(instance, function, inference)?;
            if signature_ids.contains_key(&signature) {
                continue;
            }
            let id = CallableSigId(
                u32::try_from(signatures.len()).expect("callable registry ids should fit in u32"),
            );
            signature_ids.insert(signature.clone(), id);
            signatures.push(CallableSignaturePlan { id, signature });
        }

        for closure in &reachability.closures {
            let hir_function = closure.owner.hir_function(backend.db);
            let function = hir_function.function(backend.db);
            let inference = closure.owner.infer(backend.db);
            let info = backend.closure_info(closure, function, inference)?;
            if signature_ids.contains_key(&info.signature) {
                continue;
            }
            let id = CallableSigId(
                u32::try_from(signatures.len()).expect("callable registry ids should fit in u32"),
            );
            signature_ids.insert(info.signature.clone(), id);
            signatures.push(CallableSignaturePlan { id, signature: info.signature });
        }

        let mut invoke_trampolines = Vec::new();
        for adapter in &obligations.callable_adapters {
            let CallableAdapterNeed::BoundaryInvoke(instance) = adapter else {
                continue;
            };
            let Some(&metadata_index) = boundary
                .import_indices
                .get(instance)
                .or_else(|| boundary.export_indices.get(instance))
            else {
                continue;
            };
            invoke_trampolines.push(CallableTrampolinePlan {
                signature_id: abi_preview.functions[metadata_index].signature_id,
                function_id: *function_ids
                    .get(instance)
                    .expect("reachable boundary function should have an assigned id"),
                instance: instance.clone(),
                metadata_index,
            });
        }

        Ok(CallableRegistryPlan { signatures, signature_ids, invoke_trampolines })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) enum SectionExportTarget {
    Func(u32),
    Memory(u32),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct SectionExportPlan {
    pub(in crate::backend) name: String,
    pub(in crate::backend) target: SectionExportTarget,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct SectionPlan<'db> {
    pub(in crate::backend) runtime_imports: Vec<RuntimeFunction>,
    pub(in crate::backend) stage_intrinsics: Vec<StageIntrinsic>,
    pub(in crate::backend) raw_imports: Vec<InstanceKey<'db>>,
    pub(in crate::backend) builtin_helpers: Vec<HelperFunction>,
    pub(in crate::backend) direct_functions: Vec<InstanceKey<'db>>,
    pub(in crate::backend) function_wrappers: Vec<InstanceKey<'db>>,
    pub(in crate::backend) closure_functions: Vec<ClosureInstanceKey<'db>>,
    pub(in crate::backend) closure_destroyers: Vec<ClosureInstanceKey<'db>>,
    pub(in crate::backend) export_wrappers: Vec<InstanceKey<'db>>,
    pub(in crate::backend) runtime_type_indices: FxHashMap<RuntimeFunction, u32>,
    pub(in crate::backend) stage_type_indices: FxHashMap<StageIntrinsic, u32>,
    pub(in crate::backend) helper_type_indices: FxHashMap<HelperFunction, u32>,
    pub(in crate::backend) direct_type_indices: FxHashMap<InstanceKey<'db>, u32>,
    pub(in crate::backend) external_type_indices: FxHashMap<InstanceKey<'db>, u32>,
    pub(in crate::backend) callable_type_indices: FxHashMap<FunctionSignature, u32>,
    pub(in crate::backend) nominal_destroy_type_index: Option<u32>,
    pub(in crate::backend) nominal_eq_type_index: Option<u32>,
    pub(in crate::backend) array_destroy_type_index: Option<u32>,
    pub(in crate::backend) array_eq_type_index: Option<u32>,
    pub(in crate::backend) closure_destroy_type_index: u32,
    pub(in crate::backend) abi_alloc_type_index: Option<u32>,
    pub(in crate::backend) abi_blob_release_type_index: Option<u32>,
    pub(in crate::backend) abi_handle_retain_type_index: Option<u32>,
    pub(in crate::backend) abi_handle_release_type_index: Option<u32>,
    pub(in crate::backend) invoke_trampoline_type_indices: FxHashMap<SigId, u32>,
    pub(in crate::backend) export_wrapper_type_indices: FxHashMap<InstanceKey<'db>, u32>,
    pub(in crate::backend) runtime_function_indices: FxHashMap<RuntimeFunction, u32>,
    pub(in crate::backend) stage_function_indices: FxHashMap<StageIntrinsic, u32>,
    pub(in crate::backend) raw_import_function_indices: FxHashMap<InstanceKey<'db>, u32>,
    pub(in crate::backend) helper_function_indices: FxHashMap<HelperFunction, u32>,
    pub(in crate::backend) nominal_destroyer_indices: FxHashMap<u32, u32>,
    pub(in crate::backend) nominal_eq_indices: FxHashMap<u32, u32>,
    pub(in crate::backend) array_destroyer_indices: FxHashMap<u32, u32>,
    pub(in crate::backend) array_eq_indices: FxHashMap<u32, u32>,
    pub(in crate::backend) direct_function_indices: FxHashMap<InstanceKey<'db>, u32>,
    pub(in crate::backend) wrapper_function_indices: FxHashMap<InstanceKey<'db>, u32>,
    pub(in crate::backend) closure_function_indices: FxHashMap<ClosureInstanceKey<'db>, u32>,
    pub(in crate::backend) closure_destroy_indices: FxHashMap<ClosureInstanceKey<'db>, u32>,
    pub(in crate::backend) abi_alloc_function_index: Option<u32>,
    pub(in crate::backend) abi_blob_release_function_index: Option<u32>,
    pub(in crate::backend) abi_handle_retain_function_index: Option<u32>,
    pub(in crate::backend) abi_handle_release_function_index: Option<u32>,
    pub(in crate::backend) invoke_trampoline_indices: Vec<(SigId, u32)>,
    pub(in crate::backend) export_wrapper_indices: FxHashMap<InstanceKey<'db>, u32>,
    pub(in crate::backend) table_slots: FxHashMap<FunctionValueTarget<'db>, u32>,
    pub(in crate::backend) table_elements: Vec<u32>,
    pub(in crate::backend) closure_destroyer_pairs: Vec<(u32, u32)>,
    pub(in crate::backend) exports: Vec<SectionExportPlan>,
    pub(in crate::backend) memory_index: u32,
    pub(in crate::backend) stack_global_index: u32,
}

pub(in crate::backend) struct SectionAssigner;

impl SectionAssigner {
    pub(in crate::backend) fn build<'db>(
        backend: &Backend<'db>,
        reachability: &ReachabilityGraph<'db>,
        obligations: &EmissionObligations<'db>,
        boundary: &BoundaryPlan<'db>,
        abi_preview: &BuiltAbiV2,
        helpers: &HelperRegistryPlan,
        callables: &CallableRegistryPlan<'db>,
    ) -> Result<SectionPlan<'db>, Diagnostic> {
        let runtime_imports = obligations.runtime_imports.clone();
        let stage_intrinsics = StageIntrinsic::all()
            .into_iter()
            .filter(|intrinsic| obligations.stage_intrinsics.contains(intrinsic))
            .collect::<Vec<_>>();
        let raw_imports = reachability
            .functions
            .iter()
            .filter(|instance| {
                let function = instance.location.hir_function(backend.db).function(backend.db);
                matches!(
                    function.linkage(),
                    WasmLinkage::Import { .. } | WasmLinkage::RawImport { .. }
                )
            })
            .cloned()
            .collect::<Vec<_>>();
        let builtin_helpers = helpers.builtin_helpers.clone();
        let direct_functions = reachability.functions.clone();
        let function_wrappers = reachability.functions.clone();
        let closure_functions = reachability.closures.clone();
        let closure_infos = reachability
            .closures
            .iter()
            .map(|closure| {
                let hir_function = closure.owner.hir_function(backend.db);
                let function = hir_function.function(backend.db);
                let inference = closure.owner.infer(backend.db);
                backend
                    .closure_info(closure, function, inference)
                    .map(|info| ReachableClosureInfo { closure: closure.clone(), info })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let closure_destroyers = closure_infos
            .iter()
            .filter(|closure| closure.info.env_layout.size != 0)
            .map(|closure| closure.closure.clone())
            .collect::<Vec<_>>();
        let export_wrappers =
            boundary.exports.iter().map(|export| export.instance.clone()).collect::<Vec<_>>();
        let callable_lowering = backend.callable_lowering_strategy();

        let mut runtime_type_indices = FxHashMap::default();
        let mut stage_type_indices = FxHashMap::default();
        let mut helper_type_indices = FxHashMap::default();
        let mut direct_type_indices = FxHashMap::default();
        let mut external_type_indices = FxHashMap::default();
        let mut callable_type_indices = FxHashMap::default();
        let mut invoke_trampoline_type_indices = FxHashMap::default();
        let mut export_wrapper_type_indices = FxHashMap::default();
        let mut next_type_index = 0u32;

        for runtime in &runtime_imports {
            runtime_type_indices.insert(*runtime, next_type_index);
            next_type_index += 1;
        }
        for intrinsic in &stage_intrinsics {
            stage_type_indices.insert(*intrinsic, next_type_index);
            next_type_index += 1;
        }
        for helper in &builtin_helpers {
            helper_type_indices.insert(*helper, next_type_index);
            next_type_index += 1;
        }
        let nominal_destroy_type_index =
            (!obligations.reachable_nominals.is_empty()).then_some(next_type_index);
        if nominal_destroy_type_index.is_some() {
            next_type_index += 1;
        }
        let nominal_eq_type_index =
            (!obligations.reachable_nominals.is_empty()).then_some(next_type_index);
        if nominal_eq_type_index.is_some() {
            next_type_index += 1;
        }
        let array_destroy_type_index =
            (!obligations.reachable_arrays.is_empty()).then_some(next_type_index);
        if array_destroy_type_index.is_some() {
            next_type_index += 1;
        }
        let array_eq_type_index =
            (!obligations.reachable_arrays.is_empty()).then_some(next_type_index);
        if array_eq_type_index.is_some() {
            next_type_index += 1;
        }
        for instance in &direct_functions {
            direct_type_indices.insert(instance.clone(), next_type_index);
            next_type_index += 1;
        }
        for instance in &direct_functions {
            external_type_indices.insert(instance.clone(), next_type_index);
            next_type_index += 1;
        }
        for callable in &callables.signatures {
            callable_type_indices.insert(callable.signature.clone(), next_type_index);
            next_type_index += 1;
        }
        let closure_destroy_type_index = next_type_index;
        next_type_index += 1;

        let abi_alloc_type_index =
            helpers.contains(HelperNeed::AbiAlloc).then_some(next_type_index);
        if abi_alloc_type_index.is_some() {
            next_type_index += 1;
        }
        let abi_blob_release_type_index =
            helpers.contains(HelperNeed::AbiBlobRelease).then_some(next_type_index);
        if abi_blob_release_type_index.is_some() {
            next_type_index += 1;
        }
        let abi_handle_retain_type_index =
            helpers.contains(HelperNeed::AbiHandleRetain).then_some(next_type_index);
        if abi_handle_retain_type_index.is_some() {
            next_type_index += 1;
        }
        let abi_handle_release_type_index =
            helpers.contains(HelperNeed::AbiHandleRelease).then_some(next_type_index);
        if abi_handle_release_type_index.is_some() {
            next_type_index += 1;
        }
        for trampoline in &callables.invoke_trampolines {
            invoke_trampoline_type_indices.insert(trampoline.signature_id, next_type_index);
            next_type_index += 1;
        }
        for instance in &export_wrappers {
            export_wrapper_type_indices.insert(instance.clone(), next_type_index);
            next_type_index += 1;
        }

        let mut runtime_function_indices = FxHashMap::default();
        let mut stage_function_indices = FxHashMap::default();
        let mut raw_import_function_indices = FxHashMap::default();
        let mut helper_function_indices = FxHashMap::default();
        let mut nominal_destroyer_indices = FxHashMap::default();
        let mut nominal_eq_indices = FxHashMap::default();
        let mut array_destroyer_indices = FxHashMap::default();
        let mut array_eq_indices = FxHashMap::default();
        let mut direct_function_indices = FxHashMap::default();
        let mut wrapper_function_indices = FxHashMap::default();
        let mut closure_function_indices = FxHashMap::default();
        let mut closure_destroy_indices = FxHashMap::default();
        let mut export_wrapper_indices = FxHashMap::default();
        let mut next_function_index = 0u32;

        for runtime in &runtime_imports {
            runtime_function_indices.insert(*runtime, next_function_index);
            next_function_index += 1;
        }
        for intrinsic in &stage_intrinsics {
            stage_function_indices.insert(*intrinsic, next_function_index);
            next_function_index += 1;
        }
        for instance in &raw_imports {
            raw_import_function_indices.insert(instance.clone(), next_function_index);
            next_function_index += 1;
        }
        for helper in &builtin_helpers {
            helper_function_indices.insert(*helper, next_function_index);
            next_function_index += 1;
        }
        for ty in &obligations.reachable_nominals {
            nominal_destroyer_indices.insert(nominal_ty_bits(*ty), next_function_index);
            next_function_index += 1;
        }
        for ty in &obligations.reachable_arrays {
            array_destroyer_indices.insert(array_ty_bits(*ty), next_function_index);
            next_function_index += 1;
        }
        for ty in &obligations.reachable_nominals {
            nominal_eq_indices.insert(nominal_ty_bits(*ty), next_function_index);
            next_function_index += 1;
        }
        for ty in &obligations.reachable_arrays {
            array_eq_indices.insert(array_ty_bits(*ty), next_function_index);
            next_function_index += 1;
        }
        for instance in &direct_functions {
            direct_function_indices.insert(instance.clone(), next_function_index);
            next_function_index += 1;
        }
        for instance in &function_wrappers {
            wrapper_function_indices.insert(instance.clone(), next_function_index);
            next_function_index += 1;
        }
        for closure in &closure_functions {
            closure_function_indices.insert(closure.clone(), next_function_index);
            next_function_index += 1;
        }
        for closure in &closure_destroyers {
            closure_destroy_indices.insert(closure.clone(), next_function_index);
            next_function_index += 1;
        }

        let mut table_slots = FxHashMap::default();
        let mut table_elements = Vec::new();
        let closure_destroyer_pairs = if callable_lowering.uses_table_slots() {
            table_elements.reserve(direct_functions.len() + closure_functions.len());
            for instance in &function_wrappers {
                table_slots.insert(
                    FunctionValueTarget::Function(instance.clone()),
                    table_elements.len() as u32,
                );
                table_elements.push(wrapper_function_indices[instance]);
            }
            for closure in &closure_functions {
                table_slots.insert(
                    FunctionValueTarget::Closure(closure.clone()),
                    table_elements.len() as u32,
                );
                table_elements.push(closure_function_indices[closure]);
            }
            closure_infos
                .iter()
                .filter_map(|closure| {
                    closure_destroy_indices.get(&closure.closure).copied().map(|destroy_index| {
                        (
                            table_slots[&FunctionValueTarget::Closure(closure.closure.clone())],
                            destroy_index,
                        )
                    })
                })
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };

        let abi_alloc_function_index =
            helpers.contains(HelperNeed::AbiAlloc).then_some(next_function_index);
        if abi_alloc_function_index.is_some() {
            next_function_index += 1;
        }
        let abi_blob_release_function_index =
            helpers.contains(HelperNeed::AbiBlobRelease).then_some(next_function_index);
        if abi_blob_release_function_index.is_some() {
            next_function_index += 1;
        }
        let abi_handle_retain_function_index =
            helpers.contains(HelperNeed::AbiHandleRetain).then_some(next_function_index);
        if abi_handle_retain_function_index.is_some() {
            next_function_index += 1;
        }
        let abi_handle_release_function_index =
            helpers.contains(HelperNeed::AbiHandleRelease).then_some(next_function_index);
        if abi_handle_release_function_index.is_some() {
            next_function_index += 1;
        }
        let mut invoke_trampoline_indices = Vec::with_capacity(callables.invoke_trampolines.len());
        for trampoline in &callables.invoke_trampolines {
            invoke_trampoline_indices.push((trampoline.signature_id, next_function_index));
            next_function_index += 1;
        }
        for instance in &export_wrappers {
            export_wrapper_indices.insert(instance.clone(), next_function_index);
            next_function_index += 1;
        }

        let mut exports = boundary
            .aliases
            .iter()
            .map(|alias| SectionExportPlan {
                name: alias.alias.clone(),
                target: SectionExportTarget::Func(export_wrapper_indices[&alias.instance]),
            })
            .collect::<Vec<_>>();
        if let Some(index) = abi_alloc_function_index {
            exports.push(SectionExportPlan {
                name: mitki_abi::ABI_V2_ALLOC_EXPORT.to_owned(),
                target: SectionExportTarget::Func(index),
            });
        }
        if let Some(index) = abi_blob_release_function_index {
            exports.push(SectionExportPlan {
                name: mitki_abi::ABI_V2_BLOB_RELEASE_EXPORT.to_owned(),
                target: SectionExportTarget::Func(index),
            });
        }
        if let Some(index) = abi_handle_retain_function_index {
            exports.push(SectionExportPlan {
                name: mitki_abi::ABI_V2_HANDLE_RETAIN_EXPORT.to_owned(),
                target: SectionExportTarget::Func(index),
            });
        }
        if let Some(index) = abi_handle_release_function_index {
            exports.push(SectionExportPlan {
                name: mitki_abi::ABI_V2_HANDLE_RELEASE_EXPORT.to_owned(),
                target: SectionExportTarget::Func(index),
            });
        }
        for (signature_id, function_index) in &invoke_trampoline_indices {
            exports.push(SectionExportPlan {
                name: mitki_abi::handle_invoke_export_name(*signature_id),
                target: SectionExportTarget::Func(*function_index),
            });
        }
        exports.push(SectionExportPlan {
            name: "memory".to_owned(),
            target: SectionExportTarget::Memory(0),
        });

        let _ = abi_preview;

        Ok(SectionPlan {
            runtime_imports,
            stage_intrinsics,
            raw_imports,
            builtin_helpers,
            direct_functions,
            function_wrappers,
            closure_functions,
            closure_destroyers,
            export_wrappers,
            runtime_type_indices,
            stage_type_indices,
            helper_type_indices,
            direct_type_indices,
            external_type_indices,
            callable_type_indices,
            nominal_destroy_type_index,
            nominal_eq_type_index,
            array_destroy_type_index,
            array_eq_type_index,
            closure_destroy_type_index,
            abi_alloc_type_index,
            abi_blob_release_type_index,
            abi_handle_retain_type_index,
            abi_handle_release_type_index,
            invoke_trampoline_type_indices,
            export_wrapper_type_indices,
            runtime_function_indices,
            stage_function_indices,
            raw_import_function_indices,
            helper_function_indices,
            nominal_destroyer_indices,
            nominal_eq_indices,
            array_destroyer_indices,
            array_eq_indices,
            direct_function_indices,
            wrapper_function_indices,
            closure_function_indices,
            closure_destroy_indices,
            abi_alloc_function_index,
            abi_blob_release_function_index,
            abi_handle_retain_function_index,
            abi_handle_release_function_index,
            invoke_trampoline_indices,
            export_wrapper_indices,
            table_slots,
            table_elements,
            closure_destroyer_pairs,
            exports,
            memory_index: 0,
            stack_global_index: 0,
        })
    }
}

pub(in crate::backend) struct NameAssigner;

impl NameAssigner {
    pub(in crate::backend) fn build<'db>(
        function_instances: &[FunctionInstancePlan<'db>],
        boundary: &BoundaryPlan<'db>,
        helpers: &HelperRegistryPlan,
    ) -> NamePlan {
        NamePlan {
            logical_functions: function_instances
                .iter()
                .map(|function| (function.id, function.logical_name.clone()))
                .collect(),
            typed_exports: boundary
                .aliases
                .iter()
                .map(|alias| (alias.alias.clone(), alias.function_id))
                .collect(),
            helper_exports: helpers.helper_export_names(),
        }
    }
}
