use mitki_abi_lower::boundary_transport_class;
use mitki_errors::Diagnostic;
use rustc_hash::FxHashSet;

use super::super::boundary::BoundaryPlan;
use super::super::plan::{CallableAdapterNeed, FunctionInstanceId, HelperNeed, ModulePlan};
use super::super::registry::{CallableSigId, HelperId, SectionExportTarget};
use super::super::*;

pub(in crate::backend) struct BoundaryPlanValidator;
pub(in crate::backend) struct ModulePlanValidator;
pub(in crate::backend) struct StoragePlanValidator;

impl BoundaryPlanValidator {
    pub(in crate::backend) fn validate<'db>(
        backend: &Backend<'db>,
        plan: &BoundaryPlan<'db>,
    ) -> Result<(), Diagnostic> {
        if plan.instances.len() != plan.metadata.functions.len() {
            return Err(Diagnostic::error(
                format!(
                    "internal error: boundary plan has {} entries but {} metadata records",
                    plan.instances.len(),
                    plan.metadata.functions.len()
                ),
                backend.file_range(),
            ));
        }
        if plan.imports.len() != plan.import_indices.len()
            || plan.exports.len() != plan.export_indices.len()
            || plan.instances.len() != plan.instance_indices.len()
        {
            return Err(Diagnostic::error(
                "internal error: boundary lookup maps drifted from the planned vectors",
                backend.file_range(),
            ));
        }

        for (expected_index, entry) in plan.instances.iter().enumerate() {
            if entry.metadata_index != expected_index {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: boundary plan entry `{}` was assigned metadata index {}, \
                         expected {}",
                        entry.logical_name, entry.metadata_index, expected_index
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
            let metadata = &plan.metadata.functions[expected_index];
            if metadata.logical_name != entry.logical_name
                || metadata.generic_origin_name != entry.generic_origin_name
                || metadata.wasm_module_name != entry.wasm_module_name
                || metadata.wasm_field_name != entry.wasm_field_name
                || metadata.type_args != entry.instance.type_args
                || metadata.domain != entry.domain
                || metadata.linkage != entry.linkage
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: boundary metadata drifted from the shadow plan for `{}`",
                        entry.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
            if metadata.param_tys.len() != entry.signature.params.len()
                || metadata
                    .param_tys
                    .iter()
                    .zip(entry.signature.params.iter())
                    .any(|(semantic_ty, slot)| semantic_ty != &slot.semantic_ty)
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: boundary metadata params drifted from the boundary \
                         signature for `{}`",
                        entry.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
        }

        for (instance, metadata_index) in &plan.import_indices {
            let Some(entry) = plan.instances.get(*metadata_index) else {
                return Err(Diagnostic::error(
                    "internal error: boundary import index pointed past the plan entries",
                    backend.file_range(),
                ));
            };
            if entry.instance != *instance || entry.linkage != mitki_abi::LinkageKind::WasmImport {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: boundary import index mismatch for `{}`",
                        entry.logical_name
                    ),
                    backend.function_range(instance.location),
                ));
            }
        }

        for (instance, metadata_index) in &plan.export_indices {
            let Some(entry) = plan.instances.get(*metadata_index) else {
                return Err(Diagnostic::error(
                    "internal error: boundary export index pointed past the plan entries",
                    backend.file_range(),
                ));
            };
            if entry.instance != *instance || entry.linkage != mitki_abi::LinkageKind::WasmExport {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: boundary export index mismatch for `{}`",
                        entry.logical_name
                    ),
                    backend.function_range(instance.location),
                ));
            }
        }

        let mut raw_imports = FxHashSet::default();
        for import in &plan.imports {
            let Some(entry) = plan.instances.get(import.metadata_index) else {
                return Err(Diagnostic::error(
                    "internal error: import wrapper pointed past the boundary entries",
                    backend.file_range(),
                ));
            };
            if entry.function_id != import.function_id
                || entry.instance != import.instance
                || entry.linkage != mitki_abi::LinkageKind::WasmImport
                || entry.signature != import.wrapper.signature
                || entry.wasm_module_name.as_deref() != Some(import.module_name.as_str())
                || entry.wasm_field_name != import.field_name
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: import wrapper plan drifted from boundary entry `{}`",
                        import.logical_name
                    ),
                    backend.function_range(import.instance.location),
                ));
            }
            if !raw_imports.insert((import.module_name.clone(), import.field_name.clone())) {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: duplicate boundary raw import mapping `{}::{}`",
                        import.module_name, import.field_name
                    ),
                    backend.function_range(import.instance.location),
                ));
            }
        }

        for export in &plan.exports {
            let Some(entry) = plan.instances.get(export.metadata_index) else {
                return Err(Diagnostic::error(
                    "internal error: export wrapper pointed past the boundary entries",
                    backend.file_range(),
                ));
            };
            if entry.function_id != export.function_id
                || entry.instance != export.instance
                || entry.linkage != mitki_abi::LinkageKind::WasmExport
                || entry.signature != export.wrapper.signature
                || entry.wasm_field_name != export.export_name
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: export wrapper plan drifted from boundary entry `{}`",
                        export.logical_name
                    ),
                    backend.function_range(export.instance.location),
                ));
            }
        }

        let mut aliases = FxHashSet::default();
        for alias in &plan.aliases {
            if !aliases.insert(alias.alias.clone()) {
                return Err(Diagnostic::error(
                    format!("internal error: duplicate boundary export alias `{}`", alias.alias),
                    backend.function_range(alias.instance.location),
                ));
            }
            let Some(&metadata_index) = plan.export_indices.get(&alias.instance) else {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: export alias `{}` had no matching boundary export",
                        alias.alias
                    ),
                    backend.function_range(alias.instance.location),
                ));
            };
            let entry = &plan.instances[metadata_index];
            if entry.metadata_index != alias.metadata_index || entry.instance != alias.instance {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: export alias `{}` did not match the boundary plan",
                        alias.alias
                    ),
                    backend.function_range(alias.instance.location),
                ));
            }
        }

        let preview = plan
            .metadata
            .build_preview(backend.db)
            .map_err(|message| Diagnostic::error(message, backend.file_range()))?;
        if preview.functions.len() != plan.instances.len() {
            return Err(Diagnostic::error(
                "internal error: planned ABI preview drifted from boundary entries",
                backend.file_range(),
            ));
        }
        for entry in &plan.instances {
            let built = &preview.functions[entry.metadata_index];
            let signature = &preview.graph.signatures[built.signature_id.0 as usize];
            if built.wasm_field_name != entry.wasm_field_name || built.linkage != entry.linkage {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: metadata-facing function instance drifted from boundary \
                         plan for `{}`",
                        entry.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
            if signature.params.len() != entry.signature.params.len()
                || signature.params.iter().zip(entry.signature.params.iter()).any(
                    |(transport, slot)| transport.transport_class != slot.transport.transport_class,
                )
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: metadata signature drifted from boundary params for `{}`",
                        entry.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
            let expected_result_class = boundary_transport_class(
                backend.db,
                plan.metadata.functions[entry.metadata_index].result_ty,
            );
            if signature.result.transport_class != expected_result_class {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: metadata signature drifted from boundary result for `{}`",
                        entry.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
            backend.boundary_transport_profile().ensure_supported(
                &preview.graph,
                signature,
                backend.function_range(entry.instance.location),
            )?;
        }

        Ok(())
    }
}

impl StoragePlanValidator {
    pub(in crate::backend) fn validate<'db>(
        backend: &Backend<'db>,
        layout: &FunctionLayout,
        range: mitki_errors::TextRange,
    ) -> Result<(), Diagnostic> {
        let mut frame_end = 0u32;
        for (expected_index, slot) in layout.frame_plan.slots.iter().enumerate() {
            if slot.id.0 as usize != expected_index {
                return Err(Diagnostic::error(
                    "internal error: frame slot ids drifted from frame slot ordering",
                    range,
                ));
            }
            if slot.align == 0 || slot.offset % slot.align != 0 {
                return Err(Diagnostic::error(
                    "internal error: frame slot alignment was not applied correctly",
                    range,
                ));
            }
            if slot.offset < frame_end {
                return Err(Diagnostic::error(
                    "internal error: frame slots overlap in the storage plan",
                    range,
                ));
            }
            frame_end = slot.offset.saturating_add(slot.size);
        }
        if layout.frame_plan.size < frame_end {
            return Err(Diagnostic::error(
                "internal error: frame plan size truncated planned frame slots",
                range,
            ));
        }
        if layout.frame_plan.size > 0 && layout.frame_base_local().is_none() {
            return Err(Diagnostic::error(
                "internal error: non-empty frame plan is missing its frame-base scratch local",
                range,
            ));
        }
        if layout.frame_plan.size == 0 && layout.frame_base_local().is_some() {
            return Err(Diagnostic::error(
                "internal error: empty frame plan unexpectedly reserved a frame-base scratch local",
                range,
            ));
        }

        for slot in &layout.raw_params {
            Self::validate_storage_slot(backend, layout, slot, range)?;
            if slot.frame_slot.is_some() {
                return Err(Diagnostic::error(
                    "internal error: raw parameter storage unexpectedly used a frame slot",
                    range,
                ));
            }
        }
        for slot in layout.slots.values() {
            Self::validate_storage_slot(backend, layout, slot, range)?;
        }
        for temp in layout.temps.values() {
            if layout.frame_plan.slot(temp.frame_slot).is_none() {
                return Err(Diagnostic::error(
                    "internal error: aggregate temp referenced a missing frame slot",
                    range,
                ));
            }
        }
        for &local in layout.pattern_scalar_locals.values() {
            if !layout.contains_local_index(local) {
                return Err(Diagnostic::error(
                    "internal error: pattern spill local drifted from the explicit local plan",
                    range,
                ));
            }
        }
        for &local in layout.nominal_locals.values() {
            if !layout.contains_local_index(local) {
                return Err(Diagnostic::error(
                    "internal error: nominal spill local drifted from the explicit local plan",
                    range,
                ));
            }
        }
        for &local in layout.array_repeat_locals.values() {
            if !layout.contains_local_index(local) {
                return Err(Diagnostic::error(
                    "internal error: array-repeat spill local drifted from the explicit local plan",
                    range,
                ));
            }
        }
        for &name in &layout.param_names {
            if !layout.slots.contains_key(&name) {
                return Err(Diagnostic::error(
                    "internal error: parameter binding order referenced a missing storage slot",
                    range,
                ));
            }
        }

        let expected_non_param_start = layout
            .local_plan
            .params
            .iter()
            .filter_map(|local| local.local_index)
            .max()
            .map_or(0, |index| index + 1);
        let mut value_types = Vec::new();
        for (expected_non_param_index, local) in
            (expected_non_param_start..).zip(layout.local_plan.allocation_order.iter())
        {
            let Some(local_index) = local.local_index else {
                return Err(Diagnostic::error(
                    "internal error: non-parameter local was missing its Wasm local index",
                    range,
                ));
            };
            if local_index != expected_non_param_index {
                return Err(Diagnostic::error(
                    "internal error: local-plan ordering drifted from Wasm local assignment",
                    range,
                ));
            }
            let Some(value_type) = local.value_type else {
                return Err(Diagnostic::error(
                    "internal error: non-parameter local was missing its Wasm value type",
                    range,
                ));
            };
            value_types.push(value_type);
        }
        let grouped_value_types = layout
            .wasm_locals()
            .iter()
            .flat_map(|(count, value_type)| std::iter::repeat_n(*value_type, *count as usize))
            .collect::<Vec<_>>();
        if grouped_value_types != value_types {
            return Err(Diagnostic::error(
                "internal error: grouped Wasm locals drifted from the explicit local plan",
                range,
            ));
        }

        let required_scratch = [
            ScratchLocalKind::ScratchI32,
            ScratchLocalKind::ScratchI32Aux,
            ScratchLocalKind::ObjectI32,
            ScratchLocalKind::ScratchI64,
        ];
        for kind in required_scratch {
            let exists = layout.local_plan.scratch.iter().any(
                |local| matches!(local.purpose, LocalPurpose::Scratch(found) if found == kind),
            );
            if !exists {
                return Err(Diagnostic::error(
                    "internal error: storage plan is missing a required scratch local",
                    backend.file_range(),
                ));
            }
        }

        Ok(())
    }

    fn validate_storage_slot<'db>(
        _compiler: &Backend<'db>,
        layout: &FunctionLayout,
        slot: &LocalSlot,
        range: mitki_errors::TextRange,
    ) -> Result<(), Diagnostic> {
        match (slot.local_index, slot.frame_slot) {
            (Some(local_index), None) => {
                if !layout.contains_local_index(local_index) {
                    return Err(Diagnostic::error(
                        "internal error: storage slot pointed at a missing Wasm local",
                        range,
                    ));
                }
            }
            (None, Some(frame_slot)) => {
                if layout.frame_plan.slot(frame_slot).is_none() {
                    return Err(Diagnostic::error(
                        "internal error: storage slot pointed at a missing frame slot",
                        range,
                    ));
                }
            }
            (None, None) if matches!(slot.abi, AbiTy::Scalar(BackendTy::Unit)) => {}
            _ => {
                return Err(Diagnostic::error(
                    "internal error: storage slot encoded an invalid local/frame assignment",
                    range,
                ));
            }
        }
        Ok(())
    }
}

impl ModulePlanValidator {
    pub(in crate::backend) fn validate<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
    ) -> Result<(), Diagnostic> {
        BoundaryPlanValidator::validate(backend, &plan.boundary)?;

        if plan.mode != plan.reachability.mode {
            return Err(Diagnostic::error(
                "internal error: module plan mode disagreed with reachability mode",
                backend.file_range(),
            ));
        }
        if plan.target_profile != backend.target_profile() {
            return Err(Diagnostic::error(
                "internal error: module plan target profile drifted from Backend target policy",
                backend.file_range(),
            ));
        }
        if plan.target_decisions != backend.target_decision_snapshot() {
            return Err(Diagnostic::error(
                "internal error: module plan target decisions drifted from Backend target policy",
                backend.file_range(),
            ));
        }

        let unique_runtime_imports =
            plan.obligations.runtime_imports.iter().collect::<FxHashSet<_>>();
        if unique_runtime_imports.len() != plan.obligations.runtime_imports.len() {
            return Err(Diagnostic::error(
                "internal error: emission obligations duplicated a runtime import",
                backend.file_range(),
            ));
        }
        let unique_stage_intrinsics =
            plan.obligations.stage_intrinsics.iter().collect::<FxHashSet<_>>();
        if unique_stage_intrinsics.len() != plan.obligations.stage_intrinsics.len() {
            return Err(Diagnostic::error(
                "internal error: emission obligations duplicated a stage intrinsic",
                backend.file_range(),
            ));
        }
        let unique_helpers = plan.obligations.helpers.iter().collect::<FxHashSet<_>>();
        if unique_helpers.len() != plan.obligations.helpers.len() {
            return Err(Diagnostic::error(
                "internal error: emission obligations duplicated a helper requirement",
                backend.file_range(),
            ));
        }
        let unique_runtime_import_needs =
            plan.obligations.runtime_import_needs.iter().collect::<FxHashSet<_>>();
        if unique_runtime_import_needs.len() != plan.obligations.runtime_import_needs.len() {
            return Err(Diagnostic::error(
                "internal error: emission obligations duplicated a runtime import need",
                backend.file_range(),
            ));
        }
        let unique_callable_adapters =
            plan.obligations.callable_adapters.iter().collect::<FxHashSet<_>>();
        if unique_callable_adapters.len() != plan.obligations.callable_adapters.len() {
            return Err(Diagnostic::error(
                "internal error: emission obligations duplicated a callable adapter need",
                backend.file_range(),
            ));
        }
        let unique_boundary_wrappers =
            plan.obligations.boundary_wrappers.iter().collect::<FxHashSet<_>>();
        if unique_boundary_wrappers.len() != plan.obligations.boundary_wrappers.len() {
            return Err(Diagnostic::error(
                "internal error: emission obligations duplicated a boundary wrapper need",
                backend.file_range(),
            ));
        }
        let unique_canonical_support =
            plan.obligations.canonical_support.iter().collect::<FxHashSet<_>>();
        if unique_canonical_support.len() != plan.obligations.canonical_support.len() {
            return Err(Diagnostic::error(
                "internal error: emission obligations duplicated a canonical support need",
                backend.file_range(),
            ));
        }
        let unique_layout_needs = plan.obligations.layout_needs.iter().collect::<FxHashSet<_>>();
        if unique_layout_needs.len() != plan.obligations.layout_needs.len() {
            return Err(Diagnostic::error(
                "internal error: emission obligations duplicated a layout support need",
                backend.file_range(),
            ));
        }
        let unique_helper_needs = plan.helpers.needs.iter().collect::<FxHashSet<_>>();
        if unique_helper_needs.len() != plan.helpers.needs.len() {
            return Err(Diagnostic::error(
                "internal error: module plan duplicated a helper need",
                backend.file_range(),
            ));
        }

        if plan.function_instances.len() != plan.reachability.functions.len() {
            return Err(Diagnostic::error(
                "internal error: function instance planning drifted from reachable functions",
                backend.file_range(),
            ));
        }
        for (expected_index, function) in plan.function_instances.iter().enumerate() {
            let expected_id = FunctionInstanceId(expected_index as u32);
            if function.id != expected_id {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: function instance `{}` had id f{}, expected f{}",
                        function.logical_name, function.id.0, expected_id.0
                    ),
                    backend.function_range(function.instance.location),
                ));
            }
            if plan.reachability.functions.get(expected_index) != Some(&function.instance) {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: function instance `f{}` drifted from reachability order",
                        function.id.0
                    ),
                    backend.function_range(function.instance.location),
                ));
            }
            if function.internal_signature.results.len() > 1 {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: function instance `{}` recorded more than one internal \
                         result in Step 1",
                        function.logical_name
                    ),
                    backend.function_range(function.instance.location),
                ));
            }
        }

        let reachable_functions = plan.reachability.functions.iter().collect::<FxHashSet<_>>();
        for entry in &plan.boundary.instances {
            if !reachable_functions.contains(&entry.instance) {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: boundary entry `{}` was not present in the reachability \
                         graph",
                        entry.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
            let Some(function) = plan.function_instances.get(entry.function_id.0 as usize) else {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: boundary entry `{}` referenced missing function id f{}",
                        entry.logical_name, entry.function_id.0
                    ),
                    backend.function_range(entry.instance.location),
                ));
            };
            if function.instance != entry.instance {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: boundary entry `{}` drifted from function id f{}",
                        entry.logical_name, entry.function_id.0
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
            if function.metadata_index != Some(entry.metadata_index)
                || function.boundary_signature.as_ref() != Some(&entry.signature)
                || function.internal_signature != entry.signature.internal
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: function instance shadow state drifted from boundary \
                         entry `{}`",
                        entry.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
        }

        if plan.abi_preview.functions.len() != plan.boundary.instances.len() {
            return Err(Diagnostic::error(
                "internal error: ABI v2 preview disagreed with the boundary plan entry count",
                backend.file_range(),
            ));
        }
        for entry in &plan.boundary.instances {
            let built = &plan.abi_preview.functions[entry.metadata_index];
            if built.wasm_field_name != entry.wasm_field_name || built.linkage != entry.linkage {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: ABI v2 preview drifted from the boundary plan for `{}`",
                        entry.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
        }
        let rebuilt = plan
            .boundary
            .metadata
            .build_preview(backend.db)
            .map_err(|message| Diagnostic::error(message, backend.file_range()))?;
        if rebuilt != plan.abi_preview {
            return Err(Diagnostic::error(
                "internal error: stored ABI v2 preview drifted from the boundary plan",
                backend.file_range(),
            ));
        }

        for import in &plan.imports {
            let Some(entry) = plan.boundary.instances.get(import.metadata_index) else {
                return Err(Diagnostic::error(
                    "internal error: import plan pointed past the boundary entries",
                    backend.file_range(),
                ));
            };
            if entry.function_id != import.function_id
                || entry.linkage != mitki_abi::LinkageKind::WasmImport
                || entry.logical_name != import.logical_name
                || entry.wasm_field_name != import.field_name
                || entry.wasm_module_name.as_deref() != Some(import.module_name.as_str())
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: import plan drifted from boundary entry `{}`",
                        import.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
        }
        if plan.imports.len() != plan.boundary.import_indices.len() {
            return Err(Diagnostic::error(
                "internal error: import plan count drifted from boundary imports",
                backend.file_range(),
            ));
        }

        for export in &plan.exports {
            let Some(entry) = plan.boundary.instances.get(export.metadata_index) else {
                return Err(Diagnostic::error(
                    "internal error: export plan pointed past the boundary entries",
                    backend.file_range(),
                ));
            };
            if entry.function_id != export.function_id
                || entry.linkage != mitki_abi::LinkageKind::WasmExport
                || entry.logical_name != export.logical_name
                || entry.wasm_field_name != export.export_name
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: export plan drifted from boundary entry `{}`",
                        export.logical_name
                    ),
                    backend.function_range(entry.instance.location),
                ));
            }
        }
        if plan.boundary.aliases.len() < plan.exports.len() {
            return Err(Diagnostic::error(
                "internal error: export plan count drifted from boundary export aliases",
                backend.file_range(),
            ));
        }

        let mut expected_helpers = Vec::new();
        expected_helpers
            .extend(plan.obligations.helpers.iter().copied().map(HelperNeed::from_helper_function));
        expected_helpers.extend(plan.obligations.reachable_nominals.iter().flat_map(|ty| {
            let bits = nominal_ty_bits(*ty);
            [HelperNeed::NominalDestroy(bits), HelperNeed::NominalEq(bits)]
        }));
        expected_helpers.extend(plan.obligations.reachable_arrays.iter().flat_map(|ty| {
            let bits = array_ty_bits(*ty);
            [HelperNeed::ArrayDestroy(bits), HelperNeed::ArrayEq(bits)]
        }));
        if plan.obligations.needs_blob_helpers() {
            expected_helpers.push(HelperNeed::AbiAlloc);
            expected_helpers.push(HelperNeed::AbiBlobRelease);
        }
        if plan.obligations.needs_handle_helpers() {
            expected_helpers.push(HelperNeed::AbiHandleRetain);
            expected_helpers.push(HelperNeed::AbiHandleRelease);
        }
        expected_helpers.extend(plan.obligations.callable_adapters.iter().filter_map(|adapter| {
            let CallableAdapterNeed::BoundaryInvoke(instance) = adapter else {
                return None;
            };
            let metadata_index = plan
                .boundary
                .import_indices
                .get(instance)
                .or_else(|| plan.boundary.export_indices.get(instance))
                .copied()?;
            Some(HelperNeed::HandleInvoke(plan.abi_preview.functions[metadata_index].signature_id))
        }));
        if plan.obligations.layout_needs.len()
            != plan.obligations.reachable_arrays.len() + plan.obligations.reachable_nominals.len()
        {
            return Err(Diagnostic::error(
                "internal error: layout support obligations drifted from reachable array/nominal \
                 obligations",
                backend.file_range(),
            ));
        }
        expected_helpers.sort();
        expected_helpers.dedup();
        if expected_helpers != plan.helpers.needs {
            return Err(Diagnostic::error(
                "internal error: helper needs drifted from emission obligations",
                backend.file_range(),
            ));
        }
        let expected_builtin_helpers = HelperFunction::all().into_iter().collect::<Vec<_>>();
        if plan.helpers.builtin_helpers != expected_builtin_helpers {
            return Err(Diagnostic::error(
                "internal error: helper registry builtin helper order drifted from the backend \
                 helper surface",
                backend.file_range(),
            ));
        }
        if plan.helpers.helper_ids.len() != plan.helpers.needs.len() {
            return Err(Diagnostic::error(
                "internal error: helper registry id map drifted from helper needs",
                backend.file_range(),
            ));
        }
        for (index, helper) in plan.helpers.needs.iter().enumerate() {
            let expected_id = HelperId(index as u32);
            if plan.helpers.helper_ids.get(helper).copied() != Some(expected_id) {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: helper registry assigned `{}` an unexpected id",
                        helper.dump_name()
                    ),
                    backend.file_range(),
                ));
            }
        }

        let mut expected_callable_signatures = Vec::new();
        let mut seen_callable_signatures = FxHashSet::default();
        for instance in &plan.reachability.functions {
            let hir_function = instance.location.hir_function(backend.db);
            let function = hir_function.function(backend.db);
            let inference = instance.location.infer(backend.db);
            let signature = backend.function_signature(instance, function, inference)?;
            if seen_callable_signatures.insert(signature.clone()) {
                expected_callable_signatures.push(signature);
            }
        }
        for closure in &plan.reachability.closures {
            let hir_function = closure.owner.hir_function(backend.db);
            let function = hir_function.function(backend.db);
            let inference = closure.owner.infer(backend.db);
            let info = backend.closure_info(closure, function, inference)?;
            if seen_callable_signatures.insert(info.signature.clone()) {
                expected_callable_signatures.push(info.signature);
            }
        }
        let planned_callable_signatures = plan
            .callables
            .signatures
            .iter()
            .map(|callable| callable.signature.clone())
            .collect::<Vec<_>>();
        if expected_callable_signatures != planned_callable_signatures {
            return Err(Diagnostic::error(
                "internal error: callable registry signature order drifted from reachability",
                backend.file_range(),
            ));
        }
        if plan.callables.signature_ids.len() != plan.callables.signatures.len() {
            return Err(Diagnostic::error(
                "internal error: callable registry signature lookup drifted from callable plans",
                backend.file_range(),
            ));
        }
        for (index, callable) in plan.callables.signatures.iter().enumerate() {
            let expected_id = CallableSigId(index as u32);
            if callable.id != expected_id
                || plan.callables.signature_ids.get(&callable.signature).copied()
                    != Some(expected_id)
            {
                return Err(Diagnostic::error(
                    "internal error: callable registry ids drifted from signature order",
                    backend.file_range(),
                ));
            }
        }

        let expected_trampolines = plan
            .obligations
            .callable_adapters
            .iter()
            .filter_map(|adapter| {
                let CallableAdapterNeed::BoundaryInvoke(instance) = adapter else {
                    return None;
                };
                let metadata_index = plan
                    .boundary
                    .import_indices
                    .get(instance)
                    .or_else(|| plan.boundary.export_indices.get(instance))
                    .copied()?;
                Some((
                    plan.abi_preview.functions[metadata_index].signature_id,
                    plan.function_instances
                        .iter()
                        .find(|function| function.instance == *instance)
                        .expect("reachable function instance should exist")
                        .id,
                    instance.clone(),
                    metadata_index,
                ))
            })
            .collect::<Vec<_>>();
        let planned_trampolines = plan
            .callables
            .invoke_trampolines
            .iter()
            .map(|trampoline| {
                (
                    trampoline.signature_id,
                    trampoline.function_id,
                    trampoline.instance.clone(),
                    trampoline.metadata_index,
                )
            })
            .collect::<Vec<_>>();
        if expected_trampolines != planned_trampolines {
            return Err(Diagnostic::error(
                "internal error: handle trampoline registration drifted from boundary-visible \
                 callable adapter obligations",
                backend.file_range(),
            ));
        }

        let expected_runtime_imports = plan.obligations.runtime_imports.clone();
        if plan.sections.runtime_imports != expected_runtime_imports {
            return Err(Diagnostic::error(
                "internal error: section planner runtime import order drifted from obligations",
                backend.file_range(),
            ));
        }
        let expected_stage_intrinsics = StageIntrinsic::all()
            .into_iter()
            .filter(|intrinsic| plan.obligations.stage_intrinsics.contains(intrinsic))
            .collect::<Vec<_>>();
        if plan.sections.stage_intrinsics != expected_stage_intrinsics {
            return Err(Diagnostic::error(
                "internal error: section planner stage intrinsic order drifted from obligations",
                backend.file_range(),
            ));
        }
        let expected_raw_imports = plan
            .reachability
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
        if plan.sections.raw_imports != expected_raw_imports
            || plan.sections.direct_functions != plan.reachability.functions
            || plan.sections.function_wrappers != plan.reachability.functions
            || plan.sections.closure_functions != plan.reachability.closures
            || plan.sections.export_wrappers
                != plan
                    .boundary
                    .exports
                    .iter()
                    .map(|export| export.instance.clone())
                    .collect::<Vec<_>>()
            || plan.sections.builtin_helpers != expected_builtin_helpers
        {
            return Err(Diagnostic::error(
                "internal error: section planner function membership drifted from the module plan",
                backend.file_range(),
            ));
        }

        let expected_closure_destroyers = plan
            .reachability
            .closures
            .iter()
            .filter_map(|closure| {
                let hir_function = closure.owner.hir_function(backend.db);
                let function = hir_function.function(backend.db);
                let inference = closure.owner.infer(backend.db);
                backend
                    .closure_info(closure, function, inference)
                    .ok()
                    .filter(|info| info.env_layout.size != 0)
                    .map(|_| closure.clone())
            })
            .collect::<Vec<_>>();
        if plan.sections.closure_destroyers != expected_closure_destroyers {
            return Err(Diagnostic::error(
                "internal error: section planner closure destroyer order drifted from reachable \
                 closures",
                backend.file_range(),
            ));
        }

        let mut next_type_index = 0u32;
        for runtime in &plan.sections.runtime_imports {
            if plan.sections.runtime_type_indices.get(runtime).copied() != Some(next_type_index) {
                return Err(Diagnostic::error(
                    "internal error: runtime type indices drifted from section ordering",
                    backend.file_range(),
                ));
            }
            next_type_index += 1;
        }
        for intrinsic in &plan.sections.stage_intrinsics {
            if plan.sections.stage_type_indices.get(intrinsic).copied() != Some(next_type_index) {
                return Err(Diagnostic::error(
                    "internal error: stage intrinsic type indices drifted from section ordering",
                    backend.file_range(),
                ));
            }
            next_type_index += 1;
        }
        for helper in &plan.sections.builtin_helpers {
            if plan.sections.helper_type_indices.get(helper).copied() != Some(next_type_index) {
                return Err(Diagnostic::error(
                    "internal error: helper type indices drifted from section ordering",
                    backend.file_range(),
                ));
            }
            next_type_index += 1;
        }
        if plan.sections.nominal_destroy_type_index
            != (!plan.obligations.reachable_nominals.is_empty()).then_some(next_type_index)
        {
            return Err(Diagnostic::error(
                "internal error: nominal destroy helper type placement drifted",
                backend.file_range(),
            ));
        }
        if plan.sections.nominal_destroy_type_index.is_some() {
            next_type_index += 1;
        }
        if plan.sections.nominal_eq_type_index
            != (!plan.obligations.reachable_nominals.is_empty()).then_some(next_type_index)
        {
            return Err(Diagnostic::error(
                "internal error: nominal equality helper type placement drifted",
                backend.file_range(),
            ));
        }
        if plan.sections.nominal_eq_type_index.is_some() {
            next_type_index += 1;
        }
        if plan.sections.array_destroy_type_index
            != (!plan.obligations.reachable_arrays.is_empty()).then_some(next_type_index)
        {
            return Err(Diagnostic::error(
                "internal error: array destroy helper type placement drifted",
                backend.file_range(),
            ));
        }
        if plan.sections.array_destroy_type_index.is_some() {
            next_type_index += 1;
        }
        if plan.sections.array_eq_type_index
            != (!plan.obligations.reachable_arrays.is_empty()).then_some(next_type_index)
        {
            return Err(Diagnostic::error(
                "internal error: array equality helper type placement drifted",
                backend.file_range(),
            ));
        }
        if plan.sections.array_eq_type_index.is_some() {
            next_type_index += 1;
        }
        for instance in &plan.sections.direct_functions {
            if plan.sections.direct_type_indices.get(instance).copied() != Some(next_type_index) {
                return Err(Diagnostic::error(
                    "internal error: direct function type indices drifted from reachability",
                    backend.file_range(),
                ));
            }
            next_type_index += 1;
        }
        for instance in &plan.sections.direct_functions {
            if plan.sections.external_type_indices.get(instance).copied() != Some(next_type_index) {
                return Err(Diagnostic::error(
                    "internal error: external function type indices drifted from reachability",
                    backend.file_range(),
                ));
            }
            next_type_index += 1;
        }
        for callable in &plan.callables.signatures {
            if plan.sections.callable_type_indices.get(&callable.signature).copied()
                != Some(next_type_index)
            {
                return Err(Diagnostic::error(
                    "internal error: callable type indices drifted from callable registry order",
                    backend.file_range(),
                ));
            }
            next_type_index += 1;
        }
        if plan.sections.closure_destroy_type_index != next_type_index {
            return Err(Diagnostic::error(
                "internal error: closure destroy type index drifted from callable type section",
                backend.file_range(),
            ));
        }
        next_type_index += 1;
        for optional in [
            plan.sections
                .abi_alloc_type_index
                .filter(|_| plan.helpers.contains(HelperNeed::AbiAlloc)),
            plan.sections
                .abi_blob_release_type_index
                .filter(|_| plan.helpers.contains(HelperNeed::AbiBlobRelease)),
            plan.sections
                .abi_handle_retain_type_index
                .filter(|_| plan.helpers.contains(HelperNeed::AbiHandleRetain)),
            plan.sections
                .abi_handle_release_type_index
                .filter(|_| plan.helpers.contains(HelperNeed::AbiHandleRelease)),
        ] {
            if optional == Some(next_type_index) {
                next_type_index += 1;
            } else if optional.is_some() {
                return Err(Diagnostic::error(
                    "internal error: late helper type placement drifted from the planned order",
                    backend.file_range(),
                ));
            }
        }
        for trampoline in &plan.callables.invoke_trampolines {
            if plan.sections.invoke_trampoline_type_indices.get(&trampoline.signature_id).copied()
                != Some(next_type_index)
            {
                return Err(Diagnostic::error(
                    "internal error: invoke trampoline type indices drifted from callable \
                     registry order",
                    backend.file_range(),
                ));
            }
            next_type_index += 1;
        }
        for export in &plan.boundary.exports {
            if plan.sections.export_wrapper_type_indices.get(&export.instance).copied()
                != Some(next_type_index)
            {
                return Err(Diagnostic::error(
                    "internal error: export wrapper type indices drifted from boundary export \
                     order",
                    backend.file_range(),
                ));
            }
            next_type_index += 1;
        }

        let mut next_function_index = 0u32;
        for runtime in &plan.sections.runtime_imports {
            if plan.sections.runtime_function_indices.get(runtime).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: runtime import indices drifted from section ordering",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for intrinsic in &plan.sections.stage_intrinsics {
            if plan.sections.stage_function_indices.get(intrinsic).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: stage intrinsic indices drifted from section ordering",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for instance in &plan.sections.raw_imports {
            if plan.sections.raw_import_function_indices.get(instance).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: raw import indices drifted from reachability order",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for helper in &plan.sections.builtin_helpers {
            if plan.sections.helper_function_indices.get(helper).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: helper indices drifted from helper registry order",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for ty in &plan.obligations.reachable_nominals {
            if plan.sections.nominal_destroyer_indices.get(&nominal_ty_bits(*ty)).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: nominal destroy helper indices drifted from obligations",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for ty in &plan.obligations.reachable_arrays {
            if plan.sections.array_destroyer_indices.get(&array_ty_bits(*ty)).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: array destroy helper indices drifted from obligations",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for ty in &plan.obligations.reachable_nominals {
            if plan.sections.nominal_eq_indices.get(&nominal_ty_bits(*ty)).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: nominal equality helper indices drifted from obligations",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for ty in &plan.obligations.reachable_arrays {
            if plan.sections.array_eq_indices.get(&array_ty_bits(*ty)).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: array equality helper indices drifted from obligations",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for instance in &plan.reachability.functions {
            if plan.sections.direct_function_indices.get(instance).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: direct function indices drifted from reachability",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for instance in &plan.reachability.functions {
            if plan.sections.wrapper_function_indices.get(instance).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: function wrapper indices drifted from reachability",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for closure in &plan.reachability.closures {
            if plan.sections.closure_function_indices.get(closure).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: closure function indices drifted from reachability",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for closure in &plan.sections.closure_destroyers {
            if plan.sections.closure_destroy_indices.get(closure).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: closure destroyer indices drifted from closure ordering",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for optional in [
            plan.sections
                .abi_alloc_function_index
                .filter(|_| plan.helpers.contains(HelperNeed::AbiAlloc)),
            plan.sections
                .abi_blob_release_function_index
                .filter(|_| plan.helpers.contains(HelperNeed::AbiBlobRelease)),
            plan.sections
                .abi_handle_retain_function_index
                .filter(|_| plan.helpers.contains(HelperNeed::AbiHandleRetain)),
            plan.sections
                .abi_handle_release_function_index
                .filter(|_| plan.helpers.contains(HelperNeed::AbiHandleRelease)),
        ] {
            if optional == Some(next_function_index) {
                next_function_index += 1;
            } else if optional.is_some() {
                return Err(Diagnostic::error(
                    "internal error: late helper function placement drifted from the planned order",
                    backend.file_range(),
                ));
            }
        }
        for (signature_id, function_index) in &plan.sections.invoke_trampoline_indices {
            let expected_signature_id = plan
                .callables
                .invoke_trampolines
                .get(
                    (next_function_index
                        - plan
                            .sections
                            .invoke_trampoline_indices
                            .first()
                            .map_or(next_function_index, |(_, first)| *first))
                        as usize,
                )
                .map(|trampoline| trampoline.signature_id);
            if Some(*signature_id) != expected_signature_id
                || *function_index != next_function_index
            {
                return Err(Diagnostic::error(
                    "internal error: invoke trampoline indices drifted from callable registry \
                     order",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }
        for export in &plan.boundary.exports {
            if plan.sections.export_wrapper_indices.get(&export.instance).copied()
                != Some(next_function_index)
            {
                return Err(Diagnostic::error(
                    "internal error: export wrapper indices drifted from boundary export order",
                    backend.file_range(),
                ));
            }
            next_function_index += 1;
        }

        let expected_table_elements = if backend.callable_lowering_strategy().uses_table_slots() {
            plan.reachability
                .functions
                .iter()
                .map(|instance| plan.sections.wrapper_function_indices[instance])
                .chain(
                    plan.reachability
                        .closures
                        .iter()
                        .map(|closure| plan.sections.closure_function_indices[closure]),
                )
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };
        if plan.sections.table_elements != expected_table_elements {
            return Err(Diagnostic::error(
                "internal error: table element ordering drifted from wrapper and closure ordering",
                backend.file_range(),
            ));
        }
        let expected_table_slots = if backend.callable_lowering_strategy().uses_table_slots() {
            plan.reachability
                .functions
                .iter()
                .enumerate()
                .map(|(index, instance)| {
                    (
                        FunctionValueTarget::Function(instance.clone()),
                        u32::try_from(index).expect("table slot should fit u32"),
                    )
                })
                .chain(plan.reachability.closures.iter().enumerate().map(|(index, closure)| {
                    (
                        FunctionValueTarget::Closure(closure.clone()),
                        u32::try_from(plan.reachability.functions.len() + index)
                            .expect("table slot should fit u32"),
                    )
                }))
                .collect::<FxHashMap<_, _>>()
        } else {
            FxHashMap::default()
        };
        if plan.sections.table_slots != expected_table_slots {
            return Err(Diagnostic::error(
                "internal error: table slot assignment drifted from reachable wrapper order",
                backend.file_range(),
            ));
        }
        let expected_closure_destroyer_pairs =
            if backend.callable_lowering_strategy().uses_table_slots() {
                plan.sections
                    .closure_destroyers
                    .iter()
                    .map(|closure| {
                        (
                            plan.sections.table_slots
                                [&FunctionValueTarget::Closure(closure.clone())],
                            plan.sections.closure_destroy_indices[closure],
                        )
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
        if plan.sections.closure_destroyer_pairs != expected_closure_destroyer_pairs {
            return Err(Diagnostic::error(
                "internal error: closure destroyer table pairs drifted from closure destroyer \
                 indices",
                backend.file_range(),
            ));
        }

        let expected_exports = plan
            .boundary
            .aliases
            .iter()
            .map(|alias| {
                (
                    alias.alias.clone(),
                    SectionExportTarget::Func(
                        plan.sections.export_wrapper_indices[&alias.instance],
                    ),
                )
            })
            .chain(
                plan.sections.abi_alloc_function_index.into_iter().map(|index| {
                    ("mitki:abi/2/alloc".to_owned(), SectionExportTarget::Func(index))
                }),
            )
            .chain(plan.sections.abi_blob_release_function_index.into_iter().map(|index| {
                ("mitki:abi/2/blob_release".to_owned(), SectionExportTarget::Func(index))
            }))
            .chain(plan.sections.abi_handle_retain_function_index.into_iter().map(|index| {
                ("mitki:abi/2/handle_retain".to_owned(), SectionExportTarget::Func(index))
            }))
            .chain(plan.sections.abi_handle_release_function_index.into_iter().map(|index| {
                ("mitki:abi/2/handle_release".to_owned(), SectionExportTarget::Func(index))
            }))
            .chain(plan.sections.invoke_trampoline_indices.iter().map(|(signature_id, index)| {
                (
                    format!("mitki:abi/2/invoke${}", signature_id.0),
                    SectionExportTarget::Func(*index),
                )
            }))
            .chain(std::iter::once(("memory".to_owned(), SectionExportTarget::Memory(0))))
            .collect::<Vec<_>>();
        let planned_exports = plan
            .sections
            .exports
            .iter()
            .map(|export| (export.name.clone(), export.target.clone()))
            .collect::<Vec<_>>();
        if expected_exports != planned_exports
            || plan.sections.memory_index != 0
            || plan.sections.stack_global_index != 0
        {
            return Err(Diagnostic::error(
                "internal error: export/global/memory section planning drifted from the backend \
                 skeleton",
                backend.file_range(),
            ));
        }

        if plan.memories
            != [plan::MemoryPlan { memory_index: 0, export_name: Some("memory".to_owned()) }]
        {
            return Err(Diagnostic::error(
                "internal error: memory plan drifted from the known module memory skeleton",
                backend.file_range(),
            ));
        }
        if !plan.tables.is_empty() || !plan.globals.is_empty() || !plan.data_segments.is_empty() {
            return Err(Diagnostic::error(
                "internal error: Step 1 shadow module skeleton should not eagerly populate \
                 tables/globals/data segments",
                backend.file_range(),
            ));
        }

        let expected_logical_names = plan
            .function_instances
            .iter()
            .map(|function| (function.id, function.logical_name.clone()))
            .collect::<Vec<_>>();
        if expected_logical_names != plan.names.logical_functions {
            return Err(Diagnostic::error(
                "internal error: name plan logical functions drifted from function instances",
                backend.file_range(),
            ));
        }
        let expected_typed_exports = plan
            .boundary
            .aliases
            .iter()
            .map(|alias| (alias.alias.clone(), alias.function_id))
            .collect::<Vec<_>>();
        if expected_typed_exports != plan.names.typed_exports {
            return Err(Diagnostic::error(
                "internal error: name plan typed exports drifted from export plan",
                backend.file_range(),
            ));
        }
        let expected_helper_exports = plan.helpers.helper_export_names();
        if expected_helper_exports != plan.names.helper_exports {
            return Err(Diagnostic::error(
                "internal error: name plan helper exports drifted from helper needs",
                backend.file_range(),
            ));
        }

        Ok(())
    }
}
