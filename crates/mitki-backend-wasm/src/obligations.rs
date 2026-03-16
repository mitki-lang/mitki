use mitki_abi::TransportClass;
use rustc_hash::FxHashSet;

use super::plan::{
    BoundaryWrapperNeed, CallableAdapterNeed, CanonicalSupportNeed, EmissionObligations,
    LayoutNeed, ReachabilityGraph, ReachabilityInstance, RuntimeImportNeed,
};
use super::reachability::collect_instance_emission_obligations;
use super::*;

pub(in crate::backend) struct ObligationBuild<'db> {
    pub(in crate::backend) shadow_obligations: EmissionObligations<'db>,
}

pub(in crate::backend) struct ObligationCollector;

impl ObligationCollector {
    pub(in crate::backend) fn build<'db>(
        backend: &Backend<'db>,
        graph: &ReachabilityGraph<'db>,
    ) -> ObligationBuild<'db> {
        let mut scratch = EmissionObligationScratch::default();
        let mut callable_adapters = FxHashSet::default();
        let mut boundary_wrappers = FxHashSet::default();
        let mut canonical_types = FxHashSet::default();
        let mut needs_blob_helpers = false;
        let mut needs_handle_helpers = false;

        for instance in &graph.instances {
            let reachable = match instance {
                ReachabilityInstance::Function(instance) => {
                    ReachableInstance::Function(instance.clone())
                }
                ReachabilityInstance::Closure(instance) => {
                    ReachableInstance::Closure(instance.clone())
                }
            };
            collect_instance_emission_obligations(backend, &reachable, &mut scratch);

            if let ReachableInstance::Closure(closure) = &reachable {
                let hir_function = closure.owner.hir_function(backend.db);
                let function = hir_function.function(backend.db);
                let inference = closure.owner.infer(backend.db);
                if let Ok(info) = backend.closure_info(closure, function, inference)
                    && info.env_layout.size > 0
                {
                    callable_adapters.insert(CallableAdapterNeed::ClosureEnv(closure.clone()));
                }
            }
        }

        for instance in &graph.imports {
            let function = instance.location.hir_function(backend.db).function(backend.db);
            let inference = instance.location.infer(backend.db);
            match function.linkage() {
                WasmLinkage::Import { .. } => {
                    boundary_wrappers.insert(BoundaryWrapperNeed::Import(instance.clone()));
                    let needs = collect_boundary_support_for_function(
                        backend,
                        instance,
                        function,
                        inference,
                        BoundaryExposure::Import,
                        &mut scratch,
                        &mut canonical_types,
                    );
                    needs_blob_helpers |= needs.blob_helpers;
                    needs_handle_helpers |= needs.handle_helpers;
                }
                WasmLinkage::RawImport { .. } => {
                    collect_boundary_support_for_function(
                        backend,
                        instance,
                        function,
                        inference,
                        BoundaryExposure::RawImport,
                        &mut scratch,
                        &mut canonical_types,
                    );
                }
                WasmLinkage::Internal | WasmLinkage::ImplicitMainExport | WasmLinkage::Export => {}
            }
        }

        for instance in &graph.exports {
            let function = instance.location.hir_function(backend.db).function(backend.db);
            let inference = instance.location.infer(backend.db);
            boundary_wrappers.insert(BoundaryWrapperNeed::Export(instance.clone()));
            let needs = collect_boundary_support_for_function(
                backend,
                instance,
                function,
                inference,
                BoundaryExposure::Export,
                &mut scratch,
                &mut canonical_types,
            );
            needs_blob_helpers |= needs.blob_helpers;
            needs_handle_helpers |= needs.handle_helpers;
        }

        if needs_handle_helpers {
            for instance in &graph.imports {
                let function = instance.location.hir_function(backend.db).function(backend.db);
                if matches!(function.linkage(), WasmLinkage::Import { .. }) {
                    callable_adapters.insert(CallableAdapterNeed::BoundaryInvoke(instance.clone()));
                }
            }
            for instance in &graph.exports {
                callable_adapters.insert(CallableAdapterNeed::BoundaryInvoke(instance.clone()));
            }
        }

        let mut shadow = scratch.into_plan(backend);
        shadow.runtime_import_needs =
            shadow.runtime_imports.iter().copied().map(RuntimeImportNeed::Runtime).collect();
        shadow.callable_adapters = graph
            .closures
            .iter()
            .filter(|closure| {
                callable_adapters.contains(&CallableAdapterNeed::ClosureEnv((*closure).clone()))
            })
            .cloned()
            .map(CallableAdapterNeed::ClosureEnv)
            .chain(
                graph
                    .functions
                    .iter()
                    .filter(|instance| {
                        callable_adapters
                            .contains(&CallableAdapterNeed::BoundaryInvoke((*instance).clone()))
                    })
                    .cloned()
                    .map(CallableAdapterNeed::BoundaryInvoke),
            )
            .collect();
        shadow.boundary_wrappers = graph
            .imports
            .iter()
            .filter(|instance| {
                boundary_wrappers.contains(&BoundaryWrapperNeed::Import((*instance).clone()))
            })
            .cloned()
            .map(BoundaryWrapperNeed::Import)
            .chain(
                graph
                    .exports
                    .iter()
                    .filter(|instance| {
                        boundary_wrappers
                            .contains(&BoundaryWrapperNeed::Export((*instance).clone()))
                    })
                    .cloned()
                    .map(BoundaryWrapperNeed::Export),
            )
            .collect();
        shadow.canonical_support = graph
            .types
            .iter()
            .copied()
            .filter(|ty| canonical_types.contains(ty))
            .map(CanonicalSupportNeed::Type)
            .collect();
        if needs_blob_helpers {
            shadow.canonical_support.push(CanonicalSupportNeed::BlobHelpers);
        }
        if needs_handle_helpers {
            shadow.canonical_support.push(CanonicalSupportNeed::HandleHelpers);
        }
        shadow.layout_needs = shadow
            .reachable_arrays
            .iter()
            .copied()
            .map(LayoutNeed::Array)
            .chain(shadow.reachable_nominals.iter().copied().map(LayoutNeed::Nominal))
            .collect();

        ObligationBuild { shadow_obligations: shadow }
    }
}

#[derive(Clone, Copy)]
enum BoundaryExposure {
    Import,
    RawImport,
    Export,
}

#[derive(Default)]
struct BoundarySupportSummary {
    blob_helpers: bool,
    handle_helpers: bool,
}

fn collect_boundary_support_for_function<'db>(
    backend: &Backend<'db>,
    instance: &InstanceKey<'db>,
    function: &'db Function<'db>,
    inference: &'db mitki_typeck::infer::Inference<'db>,
    exposure: BoundaryExposure,
    scratch: &mut EmissionObligationScratch<'db>,
    canonical_types: &mut FxHashSet<Ty<'db>>,
) -> BoundarySupportSummary {
    let Ok((params, result_ty)) = backend.function_signature_types(instance, function, inference)
    else {
        return BoundarySupportSummary::default();
    };

    let mut summary = BoundarySupportSummary::default();
    let typed_boundary = matches!(exposure, BoundaryExposure::Import | BoundaryExposure::Export);
    let is_export = matches!(exposure, BoundaryExposure::Export);
    let mut boundary_params_need_runtime_allocs = false;
    let transport_profile = backend.boundary_transport_profile();

    for ty in params {
        scratch.register_nominals_in_ty(backend.db, ty);
        if typed_boundary
            && let Ok(plan) = transport_profile.plan_or_message(
                backend.db,
                ty,
                "Wasm backend does not support this function signature type",
            )
        {
            if matches!(plan.transport_class, TransportClass::CanonicalValue) {
                canonical_types.insert(ty);
                if is_export {
                    summary.blob_helpers = true;
                }
            }
            if matches!(plan.transport_class, TransportClass::CapabilityHandle) {
                summary.handle_helpers = true;
            }
            boundary_params_need_runtime_allocs |=
                crate::capability::supported_value_abi(backend.db, ty).is_some_and(|runtime_abi| {
                    transport_profile.requires_runtime_allocs(backend.db, ty, &runtime_abi)
                });
        }
    }

    scratch.register_nominals_in_ty(backend.db, result_ty);
    let result_needs_runtime_allocs = if matches!(
        exposure,
        BoundaryExposure::Import | BoundaryExposure::RawImport | BoundaryExposure::Export
    ) {
        transport_profile
            .plan_or_message(
                backend.db,
                result_ty,
                "Wasm backend does not support this function signature type",
            )
            .is_ok_and(|plan| {
                if typed_boundary && matches!(plan.transport_class, TransportClass::CanonicalValue)
                {
                    canonical_types.insert(result_ty);
                    if is_export {
                        summary.blob_helpers = true;
                    }
                }
                if typed_boundary
                    && matches!(plan.transport_class, TransportClass::CapabilityHandle)
                {
                    summary.handle_helpers = true;
                }
                crate::capability::supported_value_abi(backend.db, result_ty).is_some_and(
                    |runtime_abi| {
                        transport_profile.requires_runtime_allocs(
                            backend.db,
                            result_ty,
                            &runtime_abi,
                        )
                    },
                )
            })
    } else {
        false
    };
    if boundary_params_need_runtime_allocs || result_needs_runtime_allocs {
        scratch.used_runtime_functions.insert(RuntimeFunction::Alloc);
        scratch.used_runtime_functions.insert(RuntimeFunction::Dealloc);
    }

    summary
}
