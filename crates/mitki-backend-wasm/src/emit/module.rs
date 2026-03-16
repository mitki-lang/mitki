use std::borrow::Cow;
#[cfg(test)]
use std::sync::Arc;

use mitki_abi::{
    AbiScalar, AbiValue, ArrayElements, CanonicalGraph, CanonicalNode,
    LinkageKind as AbiV2LinkageKind, PackedScalarKind, RawExportRecord, RawImportRecord, SigId,
    ValueRef, validate_wasm_module_contract,
};
use mitki_abi_lower::{MITKI_ABI_V2_CUSTOM_SECTION, encode_module_abi_v2};
use mitki_analysis::ownership;
use mitki_hir::hir::ParamId;
use mitki_lower::item::scope::enum_variants;
use mitki_resolve::{BindingId, resolve_method_for_receiver};
use mitki_span::IntoSymbol as _;
use rustc_hash::FxHashMap;
use wasm_encoder::{
    CodeSection, CustomSection, DataSection, ElementSection, Elements, EntityType, ExportKind,
    ExportSection, Function as WasmFunction, FunctionSection, GlobalSection, GlobalType,
    ImportSection, Instruction, MemorySection, MemoryType, Module, TableSection, TableType,
    TypeSection,
};

use super::super::*;
use super::backend_ir;
use super::boundary::{
    emit_alloc_helper, emit_blob_release_helper, emit_bool_param_normalization,
    emit_export_wrapper_v2, emit_function_value_wrapper, emit_handle_invoke_trampoline_v2,
    emit_handle_release_helper, emit_handle_retain_helper, emit_import_thunk_v2,
};
use super::function_kernel::{
    FunctionKernelBindingInit as KernelBindingInit, FunctionKernelBindingSource,
    FunctionKernelClosureEnvInit as KernelClosureEnvInit,
    FunctionKernelFieldValue as KernelFieldValue, FunctionKernelLowered,
    FunctionKernelMatchArm as KernelMatchArm, FunctionKernelStmt as KernelStmt,
    FunctionKernelValidator, FunctionKernelValue as KernelExpr,
    FunctionKernelValueKind as KernelExprKind,
};
use super::function_legalize::CallConvValidator;
use super::function_ownership::OwnershipValidator;
use super::function_wasm_ir::StructuredWasmValidator;
use super::plan::HelperNeed;
use super::registry::SectionExportTarget;
use super::validate::{BoundaryPlanValidator, ModulePlanValidator, StoragePlanValidator};
use super::wrapper_mir::{WrapperMirValidator, abi_v2_wasm_signature};
use crate::abi::backend_ty_value_type;
use crate::layout::VariantLayout;

struct BoundaryInvokeTrampoline<'db> {
    signature_id: SigId,
    location: FunctionLocation<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
}

#[derive(Clone, Copy)]
struct ResolvedMethodCall<'db> {
    receiver: ExprId,
    function: FunctionLocation<'db>,
}

impl<'db> Backend<'db> {
    pub fn emit_module(&self) -> Result<Vec<u8>, Diagnostic> {
        let module_plan = self.build_validated_module_plan()?;
        let static_data = self.build_static_data_for(&module_plan.reachability)?;
        self.serialize_module_from_plan(&module_plan, &static_data)
    }

    pub fn emit_module_from_plan(
        &self,
        module_plan: &plan::ModulePlan<'db>,
    ) -> Result<Vec<u8>, Diagnostic> {
        BoundaryPlanValidator::validate(self, &module_plan.boundary)?;
        ModulePlanValidator::validate(self, module_plan)?;
        WrapperMirValidator::validate(self, module_plan, &module_plan.wrapper_mir)?;
        let function_kernel = module_plan.function_kernel.as_ref().ok_or_else(|| {
            Diagnostic::error(
                "internal error: module plan is missing function kernel for ordinary functions",
                self.file_range(),
            )
        })?;
        FunctionKernelValidator::validate(self, module_plan, function_kernel)?;
        OwnershipValidator::validate(self, module_plan, function_kernel)?;
        CallConvValidator::validate(self, module_plan, function_kernel)?;
        let function_wasm_ir = module_plan.function_wasm_ir.as_ref().ok_or_else(|| {
            Diagnostic::error(
                "internal error: module plan is missing structured Wasm IR for ordinary functions",
                self.file_range(),
            )
        })?;
        StructuredWasmValidator::validate(self, module_plan, function_wasm_ir)?;
        let static_data = self.build_static_data_for(&module_plan.reachability)?;
        self.serialize_module_from_plan(module_plan, &static_data)
    }

    fn build_validated_module_plan(&self) -> Result<plan::ModulePlan<'db>, Diagnostic> {
        let module_plan = self.build_module_plan()?;
        BoundaryPlanValidator::validate(self, &module_plan.boundary)?;
        ModulePlanValidator::validate(self, &module_plan)?;
        WrapperMirValidator::validate(self, &module_plan, &module_plan.wrapper_mir)?;
        let function_kernel = module_plan.function_kernel.as_ref().ok_or_else(|| {
            Diagnostic::error(
                "internal error: module plan is missing function kernel for ordinary functions",
                self.file_range(),
            )
        })?;
        FunctionKernelValidator::validate(self, &module_plan, function_kernel)?;
        OwnershipValidator::validate(self, &module_plan, function_kernel)?;
        CallConvValidator::validate(self, &module_plan, function_kernel)?;
        let function_wasm_ir = module_plan.function_wasm_ir.as_ref().ok_or_else(|| {
            Diagnostic::error(
                "internal error: module plan is missing structured Wasm IR for ordinary functions",
                self.file_range(),
            )
        })?;
        StructuredWasmValidator::validate(self, &module_plan, function_wasm_ir)?;
        Ok(module_plan)
    }

    fn serialize_module_from_plan(
        &self,
        module_plan: &plan::ModulePlan<'db>,
        static_data: &StaticData<'db>,
    ) -> Result<Vec<u8>, Diagnostic> {
        let reachable_functions = module_plan.reachability.functions.clone();
        let reachable_closures = module_plan.reachability.closures.clone();
        let reachable_closure_infos = reachable_closures
            .iter()
            .map(|closure| {
                let hir_function = closure.owner.hir_function(self.db);
                let function = hir_function.function(self.db);
                let inference = closure.owner.infer(self.db);
                self.closure_info(closure, function, inference)
                    .map(|info| ReachableClosureInfo { closure: closure.clone(), info })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let reachable_arrays = module_plan.obligations.reachable_arrays.clone();
        let reachable_nominals = module_plan.obligations.reachable_nominals.clone();
        let boundary_plan = &module_plan.boundary;
        let mut abi_v2 = module_plan.abi_preview.clone();
        let mut handle_invoke_trampolines = Vec::new();
        for trampoline in &module_plan.callables.invoke_trampolines {
            let instance = &trampoline.instance;
            let hir_function = instance.location.hir_function(self.db);
            handle_invoke_trampolines.push(BoundaryInvokeTrampoline {
                signature_id: trampoline.signature_id,
                location: instance.location,
                source_map: hir_function.source_map(self.db),
            });
        }
        let stack_base = align_to(static_data.bytes.len() as u32, 4);
        let needs_abi_v2_blob_helpers = module_plan.helpers.contains(HelperNeed::AbiAlloc);
        let needs_abi_v2_handle_helpers = module_plan.helpers.contains(HelperNeed::AbiHandleRetain);
        let section_plan = &module_plan.sections;
        let callable_registry = &module_plan.callables;
        let import_provider = self.import_provider();
        let callable_lowering = self.callable_lowering_strategy();
        let memory_model = self.memory_model_strategy();
        let signature_strategy = self.signature_strategy();
        let word_type = memory_model.word_type();
        let runtime_imports = section_plan.runtime_imports.clone();
        let stage_imports = section_plan.stage_intrinsics.clone();
        let helper_functions = section_plan.builtin_helpers.clone();
        let mut type_section = TypeSection::new();
        let mut import_section = ImportSection::new();
        let mut function_section = FunctionSection::new();
        let mut table_section = TableSection::new();
        let mut memory_section = MemorySection::new();
        let mut global_section = GlobalSection::new();
        let mut export_section = ExportSection::new();
        let mut element_section = ElementSection::new();
        let mut code_section = CodeSection::new();
        let mut data_section = DataSection::new();
        let runtime_type_indices = &section_plan.runtime_type_indices;
        let stage_type_indices = &section_plan.stage_type_indices;
        let helper_type_indices = &section_plan.helper_type_indices;
        let direct_type_indices = &section_plan.direct_type_indices;
        let external_type_indices = &section_plan.external_type_indices;
        let callable_type_indices = &section_plan.callable_type_indices;
        let runtime_indices = &section_plan.runtime_function_indices;
        let stage_indices = &section_plan.stage_function_indices;
        let helper_indices = &section_plan.helper_function_indices;
        let raw_import_function_indices = &section_plan.raw_import_function_indices;
        let direct_function_indices = &section_plan.direct_function_indices;
        let closure_destroy_indices = &section_plan.closure_destroy_indices;
        let array_destroyer_indices = &section_plan.array_destroyer_indices;
        let array_eq_indices = &section_plan.array_eq_indices;
        let nominal_destroyer_indices = &section_plan.nominal_destroyer_indices;
        let nominal_eq_indices = &section_plan.nominal_eq_indices;
        let table_slots = &section_plan.table_slots;
        let table_elements = &section_plan.table_elements;
        let nominal_destroy_type_index = section_plan.nominal_destroy_type_index;
        let nominal_eq_type_index = section_plan.nominal_eq_type_index;
        let array_destroy_type_index = section_plan.array_destroy_type_index;
        let array_eq_type_index = section_plan.array_eq_type_index;
        let closure_destroy_type_index = section_plan.closure_destroy_type_index;
        let closure_destroyers = &section_plan.closure_destroyer_pairs;

        for runtime in &runtime_imports {
            let signature = runtime_function_signature(*runtime);
            let lowered = signature_strategy.direct_signature(&signature);
            type_section.ty().function(lowered.params, lowered.results);
        }
        for intrinsic in &stage_imports {
            let signature = stage_intrinsic_signature(*intrinsic);
            let lowered = signature_strategy.direct_signature(&signature);
            type_section.ty().function(lowered.params, lowered.results);
        }

        for helper in &helper_functions {
            let signature = helper_function_signature(*helper);
            let lowered = signature_strategy.direct_signature(&signature);
            type_section.ty().function(lowered.params, lowered.results);
        }
        if nominal_destroy_type_index.is_some() {
            type_section.ty().function([word_type], []);
        }
        if nominal_eq_type_index.is_some() {
            type_section.ty().function([word_type, word_type], [ValType::I32]);
        }
        if array_destroy_type_index.is_some() {
            type_section.ty().function([word_type], []);
        }
        if array_eq_type_index.is_some() {
            type_section.ty().function([word_type, word_type], [ValType::I32]);
        }

        for instance in &reachable_functions {
            let hir_function = instance.location.hir_function(self.db);
            let function = hir_function.function(self.db);
            let inference = instance.location.infer(self.db);
            let signature = self.function_signature(instance, function, inference)?;
            let lowered = signature_strategy.direct_signature(&signature);
            type_section.ty().function(lowered.params, lowered.results);
        }
        for instance in &reachable_functions {
            let hir_function = instance.location.hir_function(self.db);
            let function = hir_function.function(self.db);
            let inference = instance.location.infer(self.db);
            if let Some(&metadata_index) = boundary_plan
                .import_indices
                .get(instance)
                .or_else(|| boundary_plan.export_indices.get(instance))
            {
                let built = &abi_v2.functions[metadata_index];
                let signature = &abi_v2.graph.signatures[built.signature_id.0 as usize];
                let (params, results) = abi_v2_wasm_signature(&abi_v2.graph, signature)?;
                type_section.ty().function(params, results);
            } else {
                let signature = self.function_signature(instance, function, inference)?;
                let lowered = signature_strategy.direct_signature(&signature);
                type_section.ty().function(lowered.params, lowered.results);
            }
        }

        for callable in &callable_registry.signatures {
            let lowered = callable_lowering.callable_signature(
                signature_strategy,
                memory_model,
                &callable.signature,
            );
            type_section.ty().function(lowered.params, lowered.results);
        }
        type_section.ty().function([word_type], []);
        for runtime in &runtime_imports {
            let (module, field) = import_provider.runtime_import(*runtime);
            import_section.import(
                module,
                field,
                EntityType::Function(runtime_type_indices[runtime]),
            );
        }
        for intrinsic in &stage_imports {
            let (module, field) = import_provider.stage_intrinsic_import(*intrinsic);
            import_section.import(
                module,
                field,
                EntityType::Function(stage_type_indices[intrinsic]),
            );
        }
        for instance in &section_plan.raw_imports {
            let function = instance.location.hir_function(self.db).function(self.db);
            let (module, import_name) = match function.linkage() {
                WasmLinkage::Import { module } => (
                    module.text(self.db),
                    boundary_plan.import_indices.get(instance).map_or_else(
                        || {
                            instance
                                .location
                                .source(self.db)
                                .name()
                                .expect("reachable import should have a name")
                                .as_str()
                        },
                        |&metadata_index| abi_v2.functions[metadata_index].wasm_field_name.as_str(),
                    ),
                ),
                WasmLinkage::RawImport { module } => (
                    module.text(self.db),
                    instance
                        .location
                        .source(self.db)
                        .name()
                        .expect("reachable import should have a name")
                        .as_str(),
                ),
                _ => unreachable!(),
            };
            import_section.import(
                module,
                import_name,
                EntityType::Function(external_type_indices[instance]),
            );
        }

        for helper in &helper_functions {
            function_section.function(helper_type_indices[helper]);
            let wasm_function = emit_helper_function(*helper, helper_indices);
            code_section.function(&wasm_function);
        }
        if let Some(helper_location) = reachable_functions.first().map(|instance| instance.location)
        {
            let helper_source_map = helper_location.hir_function(self.db).source_map(self.db);
            let destroy_layout = scratch_only_layout(1, word_type);
            let eq_layout = scratch_only_layout(2, word_type);
            let destroy_result = AbiTy::Scalar(BackendTy::Unit);
            let eq_result = AbiTy::Scalar(BackendTy::Bool);

            for ty in &reachable_nominals {
                let payload_layout =
                    crate::capability::supported_internal_nominal_payload_layout_or_message(
                        self.db,
                        *ty,
                        "Wasm backend does not support this reachable value type",
                    )
                    .map_err(|message| {
                        Diagnostic::error(message, self.function_range(helper_location))
                    })?;
                let mut emitter = backend_ir::BackendEmitter {
                    backend: self,
                    location: helper_location,
                    source_map: helper_source_map,
                    function_indices: direct_function_indices,
                    runtime_indices,
                    stage_indices,
                    helper_indices,
                    nominal_destroyers: nominal_destroyer_indices,
                    nominal_eq_helpers: nominal_eq_indices,
                    array_destroyers: array_destroyer_indices,
                    array_eq_helpers: array_eq_indices,
                    callable_type_indices,
                    table_slots,
                    closure_destroyers,
                    resolved_wasm_refs: None,
                    function_legalization: None,
                    layout: &destroy_layout,
                    function_result: &destroy_result,
                    control_depth: 0,
                    loop_stack: Vec::new(),
                    scope_stack: Vec::new(),
                    return_target: None,
                };
                function_section.function(
                    nominal_destroy_type_index.expect("destroy helper type should exist"),
                );
                let mut wasm_function =
                    WasmFunction::new(destroy_layout.wasm_locals().iter().copied());
                emitter.emit_nominal_destructor(
                    &mut wasm_function,
                    &payload_layout,
                    0,
                    ExprId::ZERO,
                )?;
                wasm_function.instruction(&Instruction::End);
                code_section.function(&wasm_function);
            }

            for ty in &reachable_arrays {
                let layout = crate::capability::supported_array_runtime_layout_or_message(
                    self.db,
                    *ty,
                    "Wasm backend does not support this reachable value type",
                )
                .map_err(|message| {
                    Diagnostic::error(message, self.function_range(helper_location))
                })?;
                let mut emitter = backend_ir::BackendEmitter {
                    backend: self,
                    location: helper_location,
                    source_map: helper_source_map,
                    function_indices: direct_function_indices,
                    runtime_indices,
                    stage_indices,
                    helper_indices,
                    nominal_destroyers: nominal_destroyer_indices,
                    nominal_eq_helpers: nominal_eq_indices,
                    array_destroyers: array_destroyer_indices,
                    array_eq_helpers: array_eq_indices,
                    callable_type_indices,
                    table_slots,
                    closure_destroyers,
                    resolved_wasm_refs: None,
                    function_legalization: None,
                    layout: &destroy_layout,
                    function_result: &destroy_result,
                    control_depth: 0,
                    loop_stack: Vec::new(),
                    scope_stack: Vec::new(),
                    return_target: None,
                };
                function_section
                    .function(array_destroy_type_index.expect("array destroy helper type"));
                let mut wasm_function =
                    WasmFunction::new(destroy_layout.wasm_locals().iter().copied());
                emitter.emit_array_destructor(&mut wasm_function, &layout, ExprId::ZERO)?;
                wasm_function.instruction(&Instruction::End);
                code_section.function(&wasm_function);
            }

            for ty in &reachable_nominals {
                let payload_layout =
                    crate::capability::supported_internal_nominal_payload_layout_or_message(
                        self.db,
                        *ty,
                        "Wasm backend does not support this reachable value type",
                    )
                    .map_err(|message| {
                        Diagnostic::error(message, self.function_range(helper_location))
                    })?;
                let mut emitter = backend_ir::BackendEmitter {
                    backend: self,
                    location: helper_location,
                    source_map: helper_source_map,
                    function_indices: direct_function_indices,
                    runtime_indices,
                    stage_indices,
                    helper_indices,
                    nominal_destroyers: nominal_destroyer_indices,
                    nominal_eq_helpers: nominal_eq_indices,
                    array_destroyers: array_destroyer_indices,
                    array_eq_helpers: array_eq_indices,
                    callable_type_indices,
                    table_slots,
                    closure_destroyers,
                    resolved_wasm_refs: None,
                    function_legalization: None,
                    layout: &eq_layout,
                    function_result: &eq_result,
                    control_depth: 0,
                    loop_stack: Vec::new(),
                    scope_stack: Vec::new(),
                    return_target: None,
                };
                function_section
                    .function(nominal_eq_type_index.expect("eq helper type should exist"));
                let mut wasm_function = WasmFunction::new(eq_layout.wasm_locals().iter().copied());
                emitter.emit_nominal_equality(
                    &mut wasm_function,
                    &payload_layout,
                    0,
                    1,
                    ExprId::ZERO,
                )?;
                wasm_function.instruction(&Instruction::End);
                code_section.function(&wasm_function);
            }

            for ty in &reachable_arrays {
                let layout = crate::capability::supported_array_runtime_layout_or_message(
                    self.db,
                    *ty,
                    "Wasm backend does not support this reachable value type",
                )
                .map_err(|message| {
                    Diagnostic::error(message, self.function_range(helper_location))
                })?;
                let mut emitter = backend_ir::BackendEmitter {
                    backend: self,
                    location: helper_location,
                    source_map: helper_source_map,
                    function_indices: direct_function_indices,
                    runtime_indices,
                    stage_indices,
                    helper_indices,
                    nominal_destroyers: nominal_destroyer_indices,
                    nominal_eq_helpers: nominal_eq_indices,
                    array_destroyers: array_destroyer_indices,
                    array_eq_helpers: array_eq_indices,
                    callable_type_indices,
                    table_slots,
                    closure_destroyers,
                    resolved_wasm_refs: None,
                    function_legalization: None,
                    layout: &eq_layout,
                    function_result: &eq_result,
                    control_depth: 0,
                    loop_stack: Vec::new(),
                    scope_stack: Vec::new(),
                    return_target: None,
                };
                function_section.function(array_eq_type_index.expect("array eq helper type"));
                let mut wasm_function = WasmFunction::new(eq_layout.wasm_locals().iter().copied());
                emitter.emit_array_equality(&mut wasm_function, &layout, ExprId::ZERO)?;
                wasm_function.instruction(&Instruction::End);
                code_section.function(&wasm_function);
            }
        }

        for instance in &reachable_functions {
            let hir_function = instance.location.hir_function(self.db);
            let function = hir_function.function(self.db);
            let inference = instance.location.infer(self.db);
            function_section.function(direct_type_indices[instance]);
            if matches!(function.linkage(), WasmLinkage::Import { .. }) {
                let mir = module_plan
                    .wrapper_mir
                    .import(instance)
                    .expect("planned import wrapper MIR should exist");
                let wasm_function = emit_import_thunk_v2(
                    self,
                    instance.location,
                    hir_function.source_map(self.db),
                    mir,
                    &abi_v2.graph,
                    direct_function_indices,
                    raw_import_function_indices,
                    runtime_indices,
                    stage_indices,
                    helper_indices,
                    nominal_destroyer_indices,
                    nominal_eq_indices,
                    array_destroyer_indices,
                    array_eq_indices,
                    callable_type_indices,
                    table_slots,
                    closure_destroyers,
                )?;
                code_section.function(&wasm_function);
                continue;
            }
            if matches!(function.linkage(), WasmLinkage::RawImport { .. }) {
                let signature = self.function_signature(instance, function, inference)?;
                let wasm_function = emit_raw_import_forwarder(
                    self.function_range(instance.location),
                    raw_import_function_indices[instance],
                    &signature,
                )?;
                code_section.function(&wasm_function);
                continue;
            }

            let signature = self.function_signature(instance, function, inference)?;
            let emit_function = module_plan
                .function_wasm_ir
                .as_ref()
                .and_then(|bundle| bundle.direct(instance))
                .ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing planned structured Wasm IR for a reachable \
                         function",
                        self.function_range(instance.location),
                    )
                })?;
            let layout = emit_function.layout.clone();
            let mut emitter = backend_ir::BackendEmitter {
                backend: self,
                location: instance.location,
                source_map: hir_function.source_map(self.db),
                function_indices: direct_function_indices,
                runtime_indices,
                stage_indices,
                helper_indices,
                nominal_destroyers: nominal_destroyer_indices,
                nominal_eq_helpers: nominal_eq_indices,
                array_destroyers: array_destroyer_indices,
                array_eq_helpers: array_eq_indices,
                callable_type_indices,
                table_slots,
                closure_destroyers,
                resolved_wasm_refs: Some(emit_function.resolved()),
                function_legalization: emit_function.legalization.as_ref(),
                layout: &layout,
                function_result: &signature.result,
                control_depth: 0,
                loop_stack: Vec::new(),
                scope_stack: Vec::new(),
                return_target: None,
            };
            let wasm_locals = emit_function.wasm_locals();
            let mut wasm_function = WasmFunction::new(wasm_locals.iter().copied());
            emit_bool_param_normalization(&mut wasm_function, function, &signature, &layout);
            emitter.emit_prologue(&mut wasm_function);
            emitter.structured_wasm_body(&mut wasm_function, emit_function)?;
            emitter.emit_epilogue(&mut wasm_function);
            wasm_function.instruction(&Instruction::End);
            code_section.function(&wasm_function);
        }

        for instance in &reachable_functions {
            let hir_function = instance.location.hir_function(self.db);
            let function = hir_function.function(self.db);
            let inference = instance.location.infer(self.db);
            let signature = self.function_signature(instance, function, inference)?;
            function_section.function(callable_type_indices[&signature]);
            let mir = module_plan
                .wrapper_mir
                .callable_adapter(instance)
                .expect("planned callable wrapper MIR should exist");
            let wasm_function = emit_function_value_wrapper(
                self,
                instance.location,
                hir_function.source_map(self.db),
                mir,
                direct_function_indices,
                runtime_indices,
                stage_indices,
                helper_indices,
                nominal_destroyer_indices,
                nominal_eq_indices,
                array_destroyer_indices,
                array_eq_indices,
                callable_type_indices,
                table_slots,
                closure_destroyers,
            )?;
            code_section.function(&wasm_function);
        }

        for closure in &reachable_closure_infos {
            let hir_function = closure.closure.owner.hir_function(self.db);
            let source_map = hir_function.source_map(self.db);
            let emit_function = module_plan
                .function_wasm_ir
                .as_ref()
                .and_then(|bundle| bundle.closure(&closure.closure))
                .ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing planned structured Wasm IR for a reachable \
                         closure",
                        self.function_range(closure.closure.owner),
                    )
                })?;
            let layout = emit_function.layout.clone();
            let mut emitter = backend_ir::BackendEmitter {
                backend: self,
                location: closure.closure.owner,
                source_map,
                function_indices: direct_function_indices,
                runtime_indices,
                stage_indices,
                helper_indices,
                nominal_destroyers: nominal_destroyer_indices,
                nominal_eq_helpers: nominal_eq_indices,
                array_destroyers: array_destroyer_indices,
                array_eq_helpers: array_eq_indices,
                callable_type_indices,
                table_slots,
                closure_destroyers,
                resolved_wasm_refs: Some(emit_function.resolved()),
                function_legalization: emit_function.legalization.as_ref(),
                layout: &layout,
                function_result: &closure.info.signature.result,
                control_depth: 0,
                loop_stack: Vec::new(),
                scope_stack: Vec::new(),
                return_target: None,
            };
            function_section.function(callable_type_indices[&closure.info.signature]);
            let wasm_locals = emit_function.wasm_locals();
            let mut wasm_function = WasmFunction::new(wasm_locals.iter().copied());
            emitter.emit_prologue(&mut wasm_function);
            emitter.structured_wasm_body(&mut wasm_function, emit_function)?;
            emitter.emit_epilogue(&mut wasm_function);
            wasm_function.instruction(&Instruction::End);
            code_section.function(&wasm_function);
        }
        let closure_destroy_layout = closure_destroy_layout(word_type);
        let closure_destroy_result = AbiTy::Scalar(BackendTy::Unit);
        for closure in &reachable_closure_infos {
            if !closure_destroy_indices.contains_key(&closure.closure) {
                continue;
            }

            let hir_function = closure.closure.owner.hir_function(self.db);
            let source_map = hir_function.source_map(self.db);
            let mut emitter = backend_ir::BackendEmitter {
                backend: self,
                location: closure.closure.owner,
                source_map,
                function_indices: direct_function_indices,
                runtime_indices,
                stage_indices,
                helper_indices,
                nominal_destroyers: nominal_destroyer_indices,
                nominal_eq_helpers: nominal_eq_indices,
                array_destroyers: array_destroyer_indices,
                array_eq_helpers: array_eq_indices,
                callable_type_indices,
                table_slots,
                closure_destroyers,
                resolved_wasm_refs: None,
                function_legalization: None,
                layout: &closure_destroy_layout,
                function_result: &closure_destroy_result,
                control_depth: 0,
                loop_stack: Vec::new(),
                scope_stack: Vec::new(),
                return_target: None,
            };
            function_section.function(closure_destroy_type_index);
            let mut wasm_function =
                WasmFunction::new(closure_destroy_layout.wasm_locals().iter().copied());
            emitter.emit_closure_env_destructor(
                &mut wasm_function,
                &closure.info.env_layout,
                closure.closure.closure,
            )?;
            wasm_function.instruction(&Instruction::End);
            code_section.function(&wasm_function);
        }
        if !table_elements.is_empty() {
            table_section.table(TableType {
                element_type: callable_lowering.table_element_type(),
                table64: callable_lowering.table64(),
                minimum: table_elements.len() as u64,
                maximum: None,
                shared: false,
            });
            element_section.active(
                None,
                &memory_model.const_expr_from_u32(0),
                Elements::Functions(Cow::Owned(table_elements.clone())),
            );
        }

        memory_section.memory(MemoryType {
            minimum: memory_min_pages(stack_base as usize + 65_536),
            maximum: None,
            memory64: memory_model.memory64(),
            shared: false,
            page_size_log2: None,
        });
        global_section.global(
            GlobalType { val_type: word_type, mutable: true, shared: false },
            &memory_model.const_expr_from_u32(stack_base),
        );
        if !static_data.bytes.is_empty() {
            data_section.active(
                0,
                &memory_model.const_expr_from_u32(0),
                static_data.bytes.iter().copied(),
            );
        }

        for runtime in &runtime_imports {
            let (module_name, field_name) = import_provider.runtime_import(*runtime);
            let module = abi_v2.graph.insert_string(module_name);
            let field = abi_v2.graph.insert_string(field_name);
            abi_v2.graph.raw_imports.push(RawImportRecord {
                module,
                field,
                symbol: None,
                signature: None,
            });
        }
        for intrinsic in &stage_imports {
            let (module_name, field_name) = import_provider.stage_intrinsic_import(*intrinsic);
            let module = abi_v2.graph.insert_string(module_name);
            let field = abi_v2.graph.insert_string(field_name);
            abi_v2.graph.raw_imports.push(RawImportRecord {
                module,
                field,
                symbol: None,
                signature: None,
            });
        }
        for (metadata_index, built) in abi_v2.functions.iter().enumerate() {
            match built.linkage {
                AbiV2LinkageKind::WasmImport => {
                    let instance = &abi_v2.graph.function_instances[metadata_index];
                    let module = instance
                        .wasm_module_name
                        .expect("v2 import instance should record its module name");
                    let field = instance
                        .wasm_field_name
                        .expect("v2 import instance should record its field name");
                    abi_v2.graph.raw_imports.push(RawImportRecord {
                        module,
                        field,
                        symbol: Some(built.symbol_id),
                        signature: Some(built.signature_id),
                    });
                }
                AbiV2LinkageKind::WasmExport => {
                    let typed_name = abi_v2.graph.insert_string(&built.wasm_field_name);
                    abi_v2.graph.raw_exports.push(RawExportRecord {
                        name: typed_name,
                        symbol: Some(built.symbol_id),
                        signature: Some(built.signature_id),
                    });
                }
                AbiV2LinkageKind::StageEntry | AbiV2LinkageKind::RawImport => {}
            }
        }
        if needs_abi_v2_blob_helpers {
            let alloc_export_name = abi_v2.graph.insert_string(mitki_abi::ABI_V2_ALLOC_EXPORT);
            abi_v2.graph.raw_exports.push(RawExportRecord {
                name: alloc_export_name,
                symbol: None,
                signature: None,
            });
            let blob_release_export_name =
                abi_v2.graph.insert_string(mitki_abi::ABI_V2_BLOB_RELEASE_EXPORT);
            abi_v2.graph.raw_exports.push(RawExportRecord {
                name: blob_release_export_name,
                symbol: None,
                signature: None,
            });
        }
        if needs_abi_v2_handle_helpers {
            for helper_name in
                [mitki_abi::ABI_V2_HANDLE_RETAIN_EXPORT, mitki_abi::ABI_V2_HANDLE_RELEASE_EXPORT]
            {
                let helper_export_name = abi_v2.graph.insert_string(helper_name);
                abi_v2.graph.raw_exports.push(RawExportRecord {
                    name: helper_export_name,
                    symbol: None,
                    signature: None,
                });
            }
            for trampoline in &handle_invoke_trampolines {
                let invoke_name = abi_v2
                    .graph
                    .insert_string(mitki_abi::handle_invoke_export_name(trampoline.signature_id));
                abi_v2.graph.raw_exports.push(RawExportRecord {
                    name: invoke_name,
                    symbol: None,
                    signature: Some(trampoline.signature_id),
                });
            }
        }
        let memory_name = abi_v2.graph.insert_string("memory");
        abi_v2.graph.raw_exports.push(RawExportRecord {
            name: memory_name,
            symbol: None,
            signature: None,
        });

        if let Some(alloc_helper_type_index) = section_plan.abi_alloc_type_index {
            let alloc_signature = memory_model.alloc_helper_signature();
            type_section.ty().function(alloc_signature.params, alloc_signature.results);
            function_section.function(alloc_helper_type_index);
            let alloc_helper = emit_alloc_helper(runtime_indices)?;
            code_section.function(&alloc_helper);
        }
        if let Some(blob_release_type_index) = section_plan.abi_blob_release_type_index {
            type_section.ty().function([word_type], []);
            function_section.function(blob_release_type_index);
            let blob_release =
                emit_blob_release_helper(helper_indices, runtime_indices, closure_destroyers)?;
            code_section.function(&blob_release);
        }
        if needs_abi_v2_handle_helpers {
            if let Some(handle_retain_type_index) = section_plan.abi_handle_retain_type_index {
                let lowered = callable_lowering.handle_retain_signature(memory_model);
                type_section.ty().function(lowered.params, lowered.results);
                function_section.function(handle_retain_type_index);
                let handle_retain = emit_handle_retain_helper(helper_indices)?;
                code_section.function(&handle_retain);
            }

            if let Some(handle_release_type_index) = section_plan.abi_handle_release_type_index {
                let lowered = callable_lowering.handle_release_signature(memory_model);
                type_section.ty().function(lowered.params, lowered.results);
                function_section.function(handle_release_type_index);
                let handle_release = emit_handle_release_helper(
                    helper_indices,
                    runtime_indices,
                    closure_destroyers,
                )?;
                code_section.function(&handle_release);
            }

            for (trampoline, (_, function_index)) in
                handle_invoke_trampolines.iter().zip(section_plan.invoke_trampoline_indices.iter())
            {
                let signature = &abi_v2.graph.signatures[trampoline.signature_id.0 as usize];
                let (params, results) = abi_v2_wasm_signature(&abi_v2.graph, signature)?;
                let lowered = callable_lowering.boundary_invoke_signature(
                    memory_model,
                    PhysicalWasmSignature::new(params, results),
                );
                let type_index =
                    section_plan.invoke_trampoline_type_indices[&trampoline.signature_id];
                type_section.ty().function(lowered.params, lowered.results);
                function_section.function(type_index);
                debug_assert_eq!(
                    *function_index,
                    section_plan
                        .invoke_trampoline_indices
                        .iter()
                        .find_map(|(signature_id, index)| {
                            (*signature_id == trampoline.signature_id).then_some(*index)
                        })
                        .expect("planned handle trampoline should have a function index")
                );
                let mir = module_plan
                    .wrapper_mir
                    .trampoline(trampoline.signature_id)
                    .expect("planned trampoline wrapper MIR should exist");
                let trampoline_function = emit_handle_invoke_trampoline_v2(
                    self,
                    trampoline.location,
                    trampoline.source_map,
                    mir,
                    &abi_v2.graph,
                    direct_function_indices,
                    raw_import_function_indices,
                    runtime_indices,
                    stage_indices,
                    helper_indices,
                    nominal_destroyer_indices,
                    nominal_eq_indices,
                    array_destroyer_indices,
                    array_eq_indices,
                    callable_type_indices,
                    table_slots,
                    closure_destroyers,
                )?;
                code_section.function(&trampoline_function);
            }
        }

        for export in &boundary_plan.exports {
            let metadata_index = export.metadata_index;
            let built = &abi_v2.functions[metadata_index];
            let signature = &abi_v2.graph.signatures[built.signature_id.0 as usize];
            let (params, results) = abi_v2_wasm_signature(&abi_v2.graph, signature)?;
            let type_index = section_plan.export_wrapper_type_indices[&export.instance];
            type_section.ty().function(params, results);
            let hir_function = export.instance.location.hir_function(self.db);
            let source_map = hir_function.source_map(self.db);
            let mir = module_plan
                .wrapper_mir
                .export(&export.instance)
                .expect("planned export wrapper MIR should exist");
            let wrapper = emit_export_wrapper_v2(
                self,
                export.instance.location,
                source_map,
                mir,
                &abi_v2.graph,
                direct_function_indices,
                raw_import_function_indices,
                runtime_indices,
                stage_indices,
                helper_indices,
                nominal_destroyer_indices,
                nominal_eq_indices,
                array_destroyer_indices,
                array_eq_indices,
                callable_type_indices,
                table_slots,
                closure_destroyers,
            )?;
            function_section.function(type_index);
            code_section.function(&wrapper);
        }

        for export in &section_plan.exports {
            match export.target {
                SectionExportTarget::Func(index) => {
                    export_section.export(&export.name, ExportKind::Func, index);
                }
                SectionExportTarget::Memory(index) => {
                    export_section.export(&export.name, ExportKind::Memory, index);
                }
            }
        }

        let mut module = Module::new();
        module.section(&type_section);
        if !import_section.is_empty() {
            module.section(&import_section);
        }
        module.section(&function_section);
        if !table_section.is_empty() {
            module.section(&table_section);
        }
        module.section(&memory_section);
        module.section(&global_section);
        module.section(&export_section);
        if !element_section.is_empty() {
            module.section(&element_section);
        }
        module.section(&code_section);
        if !data_section.is_empty() {
            module.section(&data_section);
        }
        let encoded_v2 = encode_module_abi_v2(&abi_v2.graph).map_err(|error| {
            Diagnostic::error(
                format!("internal error: failed to encode Wasm ABI v2 metadata: {error}"),
                self.file_range(),
            )
        })?;
        module.section(&CustomSection {
            name: MITKI_ABI_V2_CUSTOM_SECTION.into(),
            data: Cow::Owned(encoded_v2),
        });
        let bytes = module.finish();
        validate_wasm_module_contract(&bytes, &abi_v2.graph).map_err(|error| {
            Diagnostic::error(
                format!("internal ABI contract validation failed: {error}"),
                self.file_range(),
            )
        })?;
        Ok(bytes)
    }

    pub(in crate::backend) fn build_layout(
        &self,
        instance: &ReachableInstance<'db>,
        function: &'db Function<'db>,
        inference: &'db mitki_typeck::infer::Inference<'db>,
        signature: &FunctionSignature,
    ) -> Result<FunctionLayout, Diagnostic> {
        let (params, body, has_env_param) = match instance {
            ReachableInstance::Function(_) => (function.params().to_vec(), function.body(), false),
            ReachableInstance::Closure(closure) => {
                let info = self.closure_info(closure, function, inference)?;
                (info.params, info.body, true)
            }
        };
        let owner_instance = instance.owner_instance();
        let mut builder = LayoutBuilder::new(
            self,
            &owner_instance,
            function,
            inference,
            signature,
            params,
            has_env_param,
        );
        builder.add_params()?;
        if body != ExprId::ZERO {
            builder.visit(body.into())?;
        }
        let layout = builder.finish();
        StoragePlanValidator::validate(
            self,
            &layout,
            self.function_range(owner_instance.location),
        )?;
        Ok(layout)
    }

    pub(in crate::backend) fn build_lowered_kernel_function(
        &self,
        instance: &ReachableInstance<'db>,
        function: &'db Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        inference: &'db mitki_typeck::infer::Inference<'db>,
        layout: &FunctionLayout,
        static_data: &StaticData<'db>,
    ) -> Result<FunctionKernelLowered<'db>, Diagnostic> {
        let mut lowerer = BackendLowerer::new(
            self,
            instance,
            function,
            source_map,
            inference,
            layout,
            static_data,
        )?;
        lowerer.lower()
    }

    pub(in crate::backend) fn build_static_data_for(
        &self,
        reachability: &plan::ReachabilityGraph<'db>,
    ) -> Result<StaticData<'db>, Diagnostic> {
        let mut builder = StaticDataBuilder::default();
        for instance in &reachability.functions {
            let hir_function = instance.location.hir_function(self.db);
            let function = hir_function.function(self.db);
            let source_map = hir_function.source_map(self.db);
            if function.body() != ExprId::ZERO {
                builder.expr(self, instance.location, function, source_map, function.body())?;
            }
        }
        Ok(builder.finish())
    }
}

struct LayoutBuilder<'a, 'db> {
    backend: &'a Backend<'db>,
    instance: &'a InstanceKey<'db>,
    location: FunctionLocation<'db>,
    function: &'db Function<'db>,
    resolver: Resolver<'db>,
    inference: &'db mitki_typeck::infer::Inference<'db>,
    signature: &'a FunctionSignature,
    params: Vec<ParamId>,
    local_plan: LocalPlanBuilder,
    frame_plan: FramePlanBuilder,
    slots: FxHashMap<NameId, LocalSlot>,
    param_names: Vec<NameId>,
    raw_params: Vec<LocalSlot>,
    temps: FxHashMap<ExprId, TempSlot>,
    pattern_scalar_locals: FxHashMap<ExprId, u32>,
    nominal_locals: FxHashMap<ExprId, u32>,
    array_repeat_locals: FxHashMap<ExprId, u32>,
}

impl<'a, 'db> LayoutBuilder<'a, 'db> {
    fn new(
        backend: &'a Backend<'db>,
        instance: &'a InstanceKey<'db>,
        function: &'db Function<'db>,
        inference: &'db mitki_typeck::infer::Inference<'db>,
        signature: &'a FunctionSignature,
        params: Vec<ParamId>,
        has_env_param: bool,
    ) -> Self {
        let mut local_plan = LocalPlanBuilder::new(0);
        let word_type = backend.memory_model_strategy().word_type();
        if has_env_param {
            local_plan.add_param(LocalPurpose::EnvPtrParam, None, Some(word_type));
        }
        if signature.result.is_aggregate() {
            local_plan.add_param(LocalPurpose::ResultPtrParam, None, Some(word_type));
        }
        Self {
            backend,
            instance,
            location: instance.location,
            function,
            resolver: Resolver::new(backend.db, instance.location),
            inference,
            signature,
            params,
            local_plan,
            frame_plan: FramePlanBuilder::default(),
            slots: FxHashMap::default(),
            param_names: Vec::new(),
            raw_params: Vec::new(),
            temps: FxHashMap::default(),
            pattern_scalar_locals: FxHashMap::default(),
            nominal_locals: FxHashMap::default(),
            array_repeat_locals: FxHashMap::default(),
        }
    }

    fn add_params(&mut self) -> Result<(), Diagnostic> {
        let nodes = self.function.node_store();
        let params = self.params.clone();
        let signature_params = self.signature.params.clone();
        for (ordinal, (param, abi)) in params.into_iter().zip(signature_params).enumerate() {
            let (pattern, _) = nodes.param(param);
            let word_type = self.backend.memory_model_strategy().word_type();
            let value_type = match &abi {
                AbiTy::Scalar(ty) => backend_ty_value_type(*ty),
                AbiTy::Aggregate(_) => Some(word_type),
            };
            let raw_slot = LocalSlot {
                abi: abi.clone(),
                local_index: self.local_plan.add_param(
                    LocalPurpose::RawParam { ordinal },
                    Some(abi.clone()),
                    value_type,
                ),
                frame_slot: None,
            };
            self.raw_params.push(raw_slot.clone());

            if let Some(name) = self.simple_binding_name(pattern) {
                self.param_names.push(name);
                let slot = if self.param_binding_needs_stable_slot(name)? {
                    LocalSlot {
                        abi: abi.clone(),
                        local_index: Some(self.local_plan.alloc_user_local(
                            LocalPurpose::UserBinding { name },
                            Some(abi.clone()),
                            value_type.expect("mutable aggregate param should lower to a pointer"),
                        )),
                        frame_slot: None,
                    }
                } else {
                    raw_slot
                };
                self.slots.insert(name, slot);
                continue;
            }

            for name in nodes.pattern_binding_names(pattern) {
                self.param_names.push(name);
                self.ensure_binding_slot(name)?;
            }
        }
        Ok(())
    }

    fn param_binding_needs_stable_slot(&self, name: NameId) -> Result<bool, Diagnostic> {
        let source_map = self.location.hir_function(self.backend.db).source_map(self.backend.db);
        if !source_map.is_mutable_binding(name) {
            return Ok(false);
        }

        Ok(self.node_abi(name.into())?.is_aggregate())
    }

    fn finish(mut self) -> FunctionLayout {
        let word_type = self.backend.memory_model_strategy().word_type();
        self.local_plan.alloc_scratch(ScratchLocalKind::ScratchI32, word_type);
        self.local_plan.alloc_scratch(ScratchLocalKind::ScratchI32Aux, word_type);
        self.local_plan.alloc_scratch(ScratchLocalKind::ObjectI32, word_type);
        self.local_plan.alloc_scratch(ScratchLocalKind::ScratchF64, ValType::F64);
        self.local_plan.alloc_scratch(ScratchLocalKind::ScratchI64, ValType::I64);

        let frame_plan = self.frame_plan.finish();
        if frame_plan.size > 0 {
            self.local_plan.alloc_scratch(ScratchLocalKind::FrameBase, word_type);
        }

        FunctionLayout::new(
            self.local_plan.finish(),
            frame_plan,
            FunctionLayoutLookups {
                slots: self.slots,
                param_names: self.param_names,
                raw_params: self.raw_params,
                temps: self.temps,
                pattern_scalar_locals: self.pattern_scalar_locals,
                nominal_locals: self.nominal_locals,
                array_repeat_locals: self.array_repeat_locals,
            },
        )
    }

    fn visit(&mut self, node: StmtId) -> Result<(), Diagnostic> {
        let nodes = self.function.node_store();
        if nodes.node_kind(node) == NodeKind::ReturnStmt {
            let (value, _) =
                nodes.return_stmt(nodes.as_return_stmt(node).expect("ReturnStmt mismatch"));
            if value != ExprId::ZERO {
                self.maybe_alloc_temp(value)?;
            }
        } else if let Some(expr) = stmt_as_expr(nodes, node) {
            self.maybe_alloc_temp(expr)?;
        }

        match nodes.node_kind(node) {
            NodeKind::LocalVar => {
                let var =
                    nodes.local_var(nodes.as_local_var(node).expect("LocalVar node mismatch"));
                for name in nodes.pattern_binding_names(var.pattern) {
                    self.ensure_binding_slot(name)?;
                }
                if !self.is_simple_binding_pattern(var.pattern) && var.initializer != ExprId::ZERO {
                    self.ensure_pattern_source_storage(var.initializer)?;
                }

                if var.initializer != ExprId::ZERO {
                    self.visit(var.initializer.into())?;
                }
            }
            NodeKind::AssignStmt => {
                let (target, value) =
                    nodes.assign_stmt(nodes.as_assign_stmt(node).expect("AssignStmt mismatch"));
                self.visit(target.into())?;
                self.visit(value.into())?;
            }
            NodeKind::ReturnStmt => {
                let (value, _) =
                    nodes.return_stmt(nodes.as_return_stmt(node).expect("ReturnStmt mismatch"));
                if value != ExprId::ZERO {
                    self.visit(value.into())?;
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(node).expect("Block node mismatch"));
                for stmt in stmts.iter() {
                    self.visit(stmt)?;
                }
                if tail != ExprId::ZERO {
                    self.visit(tail.into())?;
                }
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(node).expect("Call node mismatch"));
                self.visit(callee.into())?;
                for arg in args.iter() {
                    self.visit(arg.into())?;
                }
            }
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(node).expect("Tuple node mismatch"));
                for item in tuple.iter() {
                    self.visit(item.into())?;
                }
            }
            NodeKind::Field => {
                let (base, _) = nodes.field(nodes.as_field(node).expect("Field node mismatch"));
                if base != ExprId::ZERO {
                    self.visit(base.into())?;
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(node).expect("Binary node mismatch"));
                self.visit(binary.lhs.into())?;
                self.visit(binary.rhs.into())?;
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(node).expect("Prefix node mismatch"));
                self.visit(prefix.expr.into())?;
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(node).expect("If node mismatch"));
                self.visit(if_expr.cond.into())?;
                if if_expr.then_branch != ExprId::ZERO {
                    self.visit(if_expr.then_branch.into())?;
                }
                if if_expr.else_branch != ExprId::ZERO {
                    self.visit(if_expr.else_branch.into())?;
                }
            }
            NodeKind::Match => {
                let (scrutinee, arms) =
                    nodes.match_expr(nodes.as_match(node).expect("Match node mismatch"));
                self.ensure_pattern_source_storage(scrutinee)?;
                self.visit(scrutinee.into())?;
                for arm in arms.iter() {
                    let (pattern, expr) =
                        nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm mismatch"));
                    for name in nodes.pattern_binding_names(pattern) {
                        self.ensure_binding_slot(name)?;
                    }
                    self.visit(expr.into())?;
                }
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(node).expect("LoopExpr node mismatch"));
                if body != ExprId::ZERO {
                    self.visit(body.into())?;
                }
            }
            NodeKind::UnsafeBlock => {
                let (body, _) =
                    nodes.unsafe_block(nodes.as_unsafe_block(node).expect("UnsafeBlock mismatch"));
                if body != ExprId::ZERO {
                    self.visit(body.into())?;
                }
            }
            NodeKind::StructExpr => {
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(node).expect("StructExpr mismatch"));
                let has_struct_name = items.len() % 2 == 1;
                let mut index = if has_struct_name { 2 } else { 1 };
                while index < items.len() {
                    self.visit(items.get(index).unwrap().into())?;
                    index += 2;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn resolve_name(&self, expr: ExprId) -> Result<BindingId<'db>, Diagnostic> {
        let nodes = self.function.node_store();
        let Some(name) = nodes.as_name(expr) else {
            return Err(Diagnostic::error(
                "internal error: expected name expression during layout planning",
                self.backend.function_range(self.location),
            ));
        };
        let symbol = nodes.name(name);
        let mut resolver = self.resolver.clone();
        let guard = resolver.scopes_for_node(expr);
        let resolution = resolver.resolve_value_binding(symbol);
        resolver.reset(guard);
        resolution.ok_or_else(|| {
            Diagnostic::error(
                "internal error: unresolved name during layout planning",
                self.backend.function_range(self.location),
            )
        })
    }

    fn stack_alloc_count(&self, expr: ExprId) -> Result<Option<u32>, Diagnostic> {
        let nodes = self.function.node_store();
        if nodes.node_kind(expr) != NodeKind::Call {
            return Ok(None);
        }
        let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
        if args.len() != 1 {
            return Ok(None);
        }
        if !matches!(
            self.resolve_name(callee),
            Ok(BindingId::CompilerIntrinsic(CompilerIntrinsic::StackAlloc))
        ) {
            return Ok(None);
        }
        let count_expr = args.iter().next().expect("single argument");
        if nodes.node_kind(count_expr) != NodeKind::Int {
            return Err(Diagnostic::error(
                "`stack_alloc` currently requires a constant integer count",
                self.backend.function_range(self.location),
            ));
        }
        let count = parse_int_literal(
            nodes.int(nodes.as_int(count_expr).expect("Int node mismatch")),
            self.backend.db,
        )
        .map_err(|message| {
            Diagnostic::error(message, self.backend.function_range(self.location))
        })?;
        u32::try_from(count).map(Some).map_err(|_overflow| {
            Diagnostic::error(
                "`stack_alloc` count must fit in `u32`",
                self.backend.function_range(self.location),
            )
        })
    }

    fn maybe_alloc_temp(&mut self, expr: ExprId) -> Result<(), Diagnostic> {
        if !self.temps.contains_key(&expr)
            && let Some(count) = self.stack_alloc_count(expr)?
        {
            let ty = self
                .inference
                .type_of_node(expr)
                .map(|ty| self.backend.specialize_ty(self.instance, ty))
                .ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing inferred `stack_alloc` type",
                        self.backend.function_range(self.location),
                    )
                })?;
            let TyKind::Pointer { pointee, .. } = ty.kind(self.backend.db) else {
                return Err(Diagnostic::error(
                    "internal error: `stack_alloc` did not infer a pointer type",
                    self.backend.function_range(self.location),
                ));
            };
            let pointee_abi = crate::capability::supported_value_abi_or_message(
                self.backend.db,
                *pointee,
                "Wasm backend does not support this `stack_alloc` pointee type",
            )
            .map_err(|message| {
                Diagnostic::error(message, self.backend.function_range(self.location))
            })?;
            let pointee_layout = crate::layout::abi_layout(&pointee_abi).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: missing `stack_alloc` pointee layout",
                    self.backend.function_range(self.location),
                )
            })?;
            let size = pointee_layout.size.checked_mul(count).ok_or_else(|| {
                Diagnostic::error(
                    "`stack_alloc` size overflowed the current frame allocator",
                    self.backend.function_range(self.location),
                )
            })?;
            let frame_slot = self.frame_plan.alloc_slot(
                FrameSlotPurpose::StackAlloc(expr),
                size,
                pointee_layout.align.max(1),
            );
            self.temps.insert(expr, TempSlot { frame_slot });
            return Ok(());
        }

        let Some(abi) = self
            .inference
            .type_of_node(expr)
            .map(|ty| self.backend.specialize_ty(self.instance, ty))
            .and_then(|ty| crate::capability::supported_value_abi(self.backend.db, ty))
        else {
            return Ok(());
        };
        let AbiTy::Aggregate(layout) = abi else {
            if matches!(abi, AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(_) | RefKind::Array(_))))
                && !self.nominal_locals.contains_key(&expr)
            {
                let local = self.local_plan.alloc_spill(
                    LocalPurpose::NominalTemp { expr },
                    Some(abi.clone()),
                    ValType::I32,
                );
                self.nominal_locals.insert(expr, local);
            }
            if self.function.node_store().node_kind(expr) == NodeKind::ArrayRepeat
                && !self.array_repeat_locals.contains_key(&expr)
            {
                let local = self.local_plan.alloc_spill(
                    LocalPurpose::ArrayRepeatTemp { expr },
                    Some(abi.clone()),
                    ValType::I32,
                );
                self.array_repeat_locals.insert(expr, local);
            }
            return Ok(());
        };

        if !self.temps.contains_key(&expr) {
            let frame_slot =
                self.frame_plan.alloc_slot(FrameSlotPurpose::Temp(expr), layout.size, layout.align);
            self.temps.insert(expr, TempSlot { frame_slot });
        }
        Ok(())
    }

    fn node_abi(&self, expr: ExprId) -> Result<AbiTy, Diagnostic> {
        let ty = self.inference.type_of_node(expr).ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing inferred expression type",
                self.backend.function_range(self.location),
            )
        })?;
        let ty = self.backend.specialize_ty(self.instance, ty);
        crate::capability::supported_value_abi_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support this value type",
        )
        .map_err(|message| Diagnostic::error(message, self.backend.function_range(self.location)))
    }

    fn simple_binding_name(&self, pattern: PatId) -> Option<NameId> {
        let nodes = self.function.node_store();
        let binding = nodes.as_pat_binding(pattern)?;
        let (name, _) = nodes.pat_binding(binding);
        Some(name)
    }

    fn is_simple_binding_pattern(&self, pattern: PatId) -> bool {
        self.simple_binding_name(pattern).is_some()
    }

    fn ensure_binding_slot(&mut self, name: NameId) -> Result<(), Diagnostic> {
        if self.slots.contains_key(&name) {
            return Ok(());
        }
        let abi = self.node_abi(name.into())?;
        let (local_index, frame_slot) = match &abi {
            AbiTy::Scalar(ty) => {
                let local_index = backend_ty_value_type(*ty).map(|value_type| {
                    self.local_plan.alloc_user_local(
                        LocalPurpose::UserBinding { name },
                        Some(abi.clone()),
                        value_type,
                    )
                });
                (local_index, None)
            }
            AbiTy::Aggregate(layout) => (
                None,
                Some(self.frame_plan.alloc_slot(
                    FrameSlotPurpose::Binding(name),
                    layout.size,
                    layout.align,
                )),
            ),
        };
        self.slots.insert(name, LocalSlot { abi, local_index, frame_slot });
        Ok(())
    }

    fn ensure_pattern_source_storage(&mut self, expr: ExprId) -> Result<(), Diagnostic> {
        match self.node_abi(expr)? {
            AbiTy::Scalar(BackendTy::Unit) => Ok(()),
            AbiTy::Scalar(ty) => {
                if !self.pattern_scalar_locals.contains_key(&expr) {
                    let local = self.local_plan.alloc_spill(
                        LocalPurpose::PatternSource { expr },
                        Some(AbiTy::Scalar(ty)),
                        backend_ty_value_type(ty)
                            .expect("non-unit scalar should have a value type"),
                    );
                    self.pattern_scalar_locals.insert(expr, local);
                }
                Ok(())
            }
            AbiTy::Aggregate(_) => self.maybe_alloc_temp(expr),
        }
    }
}

struct BackendLowerer<'a, 'db> {
    backend: &'a Backend<'db>,
    owner_instance: InstanceKey<'db>,
    location: FunctionLocation<'db>,
    function: &'db Function<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    inference: &'db mitki_typeck::infer::Inference<'db>,
    resolver: Resolver<'db>,
    layout: &'a FunctionLayout,
    static_data: &'a StaticData<'db>,
    params: Vec<ParamId>,
    param_tys: Vec<Ty<'db>>,
    result_ty: Ty<'db>,
    body: ExprId,
    capture_fields: FxHashMap<NameId, FieldLayout>,
}

impl<'a, 'db> BackendLowerer<'a, 'db> {
    fn new(
        backend: &'a Backend<'db>,
        instance: &ReachableInstance<'db>,
        function: &'db Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        inference: &'db mitki_typeck::infer::Inference<'db>,
        layout: &'a FunctionLayout,
        static_data: &'a StaticData<'db>,
    ) -> Result<Self, Diagnostic> {
        let owner_instance = instance.owner_instance();
        let (location, params, param_tys, result_ty, body, capture_fields) = match &instance {
            ReachableInstance::Function(instance) => {
                let (param_tys, result_ty) =
                    backend.function_signature_types(instance, function, inference)?;
                (
                    instance.location,
                    function.params().to_vec(),
                    param_tys,
                    result_ty,
                    function.body(),
                    FxHashMap::default(),
                )
            }
            ReachableInstance::Closure(closure) => {
                let info = backend.closure_info(closure, function, inference)?;
                let closure_ty = backend
                    .specialized_expr_ty(&owner_instance, inference, closure.closure)
                    .ok_or_else(|| {
                        Diagnostic::error(
                            "internal error: missing inferred closure type during backend lowering",
                            backend.function_range(closure.owner),
                        )
                    })?;
                let TyKind::Function { inputs, output } = closure_ty.kind(backend.db) else {
                    return Err(Diagnostic::error(
                        "internal error: closure value did not lower to a function type",
                        backend.function_range(closure.owner),
                    ));
                };
                let capture_fields = info
                    .captures
                    .into_iter()
                    .map(|capture| (capture.binding, capture.field))
                    .collect();
                (closure.owner, info.params, inputs.clone(), *output, info.body, capture_fields)
            }
        };
        Ok(Self {
            backend,
            owner_instance,
            location,
            function,
            source_map,
            inference,
            resolver: Resolver::new(backend.db, location),
            layout,
            static_data,
            params,
            param_tys,
            result_ty,
            body,
            capture_fields,
        })
    }

    fn lower(&mut self) -> Result<FunctionKernelLowered<'db>, Diagnostic> {
        let mut param_inits = Vec::new();
        for (index, (&param, ty)) in
            self.params.iter().zip(self.param_tys.iter().copied()).enumerate()
        {
            let (pattern, _) = self.function.node_store().param(param);
            if self.function.node_store().as_pat_binding(pattern).is_some() {
                let name = self
                    .function
                    .node_store()
                    .pat_binding(
                        self.function.node_store().as_pat_binding(pattern).expect("PatBinding"),
                    )
                    .0;
                let slot = self.layout.slots.get(&name).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing parameter binding slot during backend lowering",
                        self.node_range(name.into()),
                    )
                })?;
                let raw_abi =
                    self.layout.raw_params.get(index).map(|slot| slot.abi.clone()).ok_or_else(
                        || {
                            Diagnostic::error(
                                "internal error: missing raw parameter slot during backend \
                                 lowering",
                                self.backend.function_range(self.location),
                            )
                        },
                    )?;
                if slot.abi == raw_abi
                    && slot.local_index == self.layout.raw_params[index].local_index
                    && slot.frame_slot == self.layout.raw_params[index].frame_slot
                {
                    continue;
                }
                param_inits.push(KernelBindingInit {
                    pattern: self.lower_pattern(pattern, ty, &slot.abi, ExprId::ZERO)?,
                    source: FunctionKernelBindingSource::Param {
                        index,
                        abi: raw_abi,
                        source: ExprId::ZERO,
                    },
                });
                continue;
            }
            let abi = self.layout.raw_params.get(index).map(|slot| slot.abi.clone()).ok_or_else(
                || {
                    Diagnostic::error(
                        "internal error: missing raw parameter slot during backend lowering",
                        self.backend.function_range(self.location),
                    )
                },
            )?;
            param_inits.push(KernelBindingInit {
                pattern: self.lower_pattern(pattern, ty, &abi, ExprId::ZERO)?,
                source: FunctionKernelBindingSource::Param { index, abi, source: ExprId::ZERO },
            });
        }
        let body = (self.body != ExprId::ZERO)
            .then(|| self.lower_expr_as(self.body, Some(self.result_ty)))
            .transpose()?;
        Ok(FunctionKernelLowered { param_inits, body })
    }

    fn lower_stmt(&mut self, stmt: StmtId) -> Result<KernelStmt<'db>, Diagnostic> {
        let nodes = self.function.node_store();
        if nodes.node_kind(stmt) == NodeKind::LocalVar {
            let var = nodes.local_var(nodes.as_local_var(stmt).expect("LocalVar node mismatch"));
            if let Some(binding) = nodes.as_pat_binding(var.pattern) {
                let (name, _) = nodes.pat_binding(binding);
                let abi =
                    self.layout.slots.get(&name).map(|slot| slot.abi.clone()).ok_or_else(|| {
                        Diagnostic::error(
                            "internal error: missing local slot during backend lowering",
                            self.node_range(name.into()),
                        )
                    })?;
                let binding_ty = self
                    .backend
                    .specialized_expr_ty(&self.owner_instance, self.inference, name.into())
                    .unwrap_or_else(|| Ty::new(self.backend.db, TyKind::Unknown));
                let initializer = (var.initializer != ExprId::ZERO)
                    .then(|| self.lower_expr_as(var.initializer, Some(binding_ty)))
                    .transpose()?;
                return Ok(KernelStmt::Local { name, abi, initializer });
            }
            let initializer = (var.initializer != ExprId::ZERO)
                .then(|| self.lower_expr(var.initializer))
                .transpose()?;
            let source_ty = self
                .backend
                .specialized_expr_ty(&self.owner_instance, self.inference, var.initializer)
                .unwrap_or_else(|| Ty::new(self.backend.db, TyKind::Unknown));
            let source_abi = self.expr_abi(var.initializer)?;
            let Some(initializer) = initializer else {
                return Err(Diagnostic::error(
                    "internal error: non-binding local pattern is missing an initializer",
                    self.node_range(stmt.node_id()),
                ));
            };
            return Ok(KernelStmt::Pattern(KernelBindingInit {
                pattern: self.lower_pattern(
                    var.pattern,
                    source_ty,
                    &source_abi,
                    var.initializer,
                )?,
                source: FunctionKernelBindingSource::Value(initializer),
            }));
        }
        if nodes.node_kind(stmt) == NodeKind::ReturnStmt {
            let (value, _) =
                nodes.return_stmt(nodes.as_return_stmt(stmt).expect("ReturnStmt mismatch"));
            let value = (value != ExprId::ZERO)
                .then(|| self.lower_expr_as(value, Some(self.result_ty)))
                .transpose()?;
            return Ok(KernelStmt::Return { source: stmt.node_id(), value });
        }

        if nodes.node_kind(stmt) == NodeKind::AssignStmt {
            let (target, value) =
                nodes.assign_stmt(nodes.as_assign_stmt(stmt).expect("AssignStmt mismatch"));
            let target_ty = self.lowerable_expr_ty(target)?;
            let target_abi = crate::capability::supported_value_abi_or_message(
                self.backend.db,
                target_ty,
                "Wasm backend does not support this assignment target type",
            )
            .map_err(|message| Diagnostic::error(message, self.node_range(target)))?;
            let value = self.lower_expr_as(value, Some(target_ty))?;

            if nodes.node_kind(target) == NodeKind::Name {
                return match self.resolve_name(target)? {
                    BindingId::Local(name) => {
                        Ok(KernelStmt::Assign { name, abi: target_abi, value })
                    }
                    BindingId::Param(_) => {
                        let addr = self.lower_place_address(target)?;
                        Ok(KernelStmt::Expr(KernelExpr {
                            source: stmt.node_id(),
                            abi: AbiTy::Scalar(BackendTy::Unit),
                            ownership: ValueOwnership::None,
                            kind: KernelExprKind::MemoryWrite {
                                addr: Box::new(addr),
                                value: Box::new(value),
                            },
                        }))
                    }
                    _ => Err(Diagnostic::error(
                        "internal error: invalid assignment target reached backend lowering",
                        self.node_range(target),
                    )),
                };
            }

            let addr = self.lower_place_address(target)?;
            return Ok(KernelStmt::Expr(KernelExpr {
                source: stmt.node_id(),
                abi: AbiTy::Scalar(BackendTy::Unit),
                ownership: ValueOwnership::None,
                kind: KernelExprKind::MemoryWrite { addr: Box::new(addr), value: Box::new(value) },
            }));
        }

        let expr = stmt_as_expr(nodes, stmt).ok_or_else(|| {
            Diagnostic::error(
                "internal error: unsupported statement reached backend lowering",
                self.backend.function_range(self.location),
            )
        })?;
        Ok(KernelStmt::Expr(self.lower_expr(expr)?))
    }

    fn lower_expr(&mut self, expr: ExprId) -> Result<KernelExpr<'db>, Diagnostic> {
        self.lower_expr_as(expr, None)
    }

    fn lower_expr_as(
        &mut self,
        expr: ExprId,
        expected_ty: Option<Ty<'db>>,
    ) -> Result<KernelExpr<'db>, Diagnostic> {
        let inferred_ty = self.lowerable_expr_ty(expr)?;
        let actual_ty = match (inferred_ty.kind(self.backend.db), expected_ty) {
            (TyKind::Unknown, Some(expected)) => expected,
            (TyKind::Tuple(items), Some(expected))
                if items.is_empty()
                    && !matches!(expected.kind(self.backend.db), TyKind::Tuple(expected_items) if expected_items.is_empty()) =>
            {
                expected
            }
            _ => inferred_ty,
        };
        let abi = crate::capability::supported_value_abi_or_message(
            self.backend.db,
            actual_ty,
            "Wasm backend does not support this value type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(expr)))?;
        let nodes = self.function.node_store();
        let kind = match nodes.node_kind(expr) {
            NodeKind::Name => self.lower_name_expr(expr)?,
            NodeKind::True => KernelExprKind::Bool(true),
            NodeKind::False => KernelExprKind::Bool(false),
            NodeKind::Int => {
                let literal = nodes.int(nodes.as_int(expr).expect("Int node mismatch"));
                KernelExprKind::Int(
                    parse_int_literal(literal, self.backend.db)
                        .map_err(|message| Diagnostic::error(message, self.node_range(expr)))?,
                )
            }
            NodeKind::Float => {
                let literal = nodes.float(nodes.as_float(expr).expect("Float node mismatch"));
                KernelExprKind::Float(
                    parse_float_literal(literal, self.backend.db)
                        .map_err(|message| Diagnostic::error(message, self.node_range(expr)))?,
                )
            }
            NodeKind::String => {
                let literal = nodes.string(nodes.as_string(expr).expect("String node mismatch"));
                let offset = self.static_data.offset(literal).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing static string data during backend lowering",
                        self.node_range(expr),
                    )
                })?;
                KernelExprKind::String(offset)
            }
            NodeKind::Char => {
                let literal = nodes.char(nodes.as_char(expr).expect("Char node mismatch"));
                KernelExprKind::Char(
                    decode_char_literal(literal, self.backend.db)
                        .map_err(|message| Diagnostic::error(message, self.node_range(expr)))?,
                )
            }
            NodeKind::Tuple if matches!(abi, AbiTy::Scalar(BackendTy::Unit)) => {
                KernelExprKind::Unit
            }
            NodeKind::Tuple => {
                let layout = abi.aggregate().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: tuple expression is missing aggregate layout",
                        self.node_range(expr),
                    )
                })?;
                let fields = layout.fields().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: tuple layout is missing field metadata",
                        self.node_range(expr),
                    )
                })?;
                let items = nodes
                    .tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"))
                    .iter()
                    .collect::<Vec<_>>();
                let item_tys = match actual_ty.kind(self.backend.db) {
                    TyKind::Tuple(items) => items.clone(),
                    _ => vec![Ty::new(self.backend.db, TyKind::Unknown); items.len()],
                };
                let fields = fields
                    .iter()
                    .zip(items.iter().zip(item_tys.iter()))
                    .map(|(field, (&item, &item_ty))| {
                        self.lower_expr_as(item, Some(item_ty))
                            .map(|value| KernelFieldValue { field: field.clone(), value })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                KernelExprKind::Tuple { fields }
            }
            NodeKind::Array => {
                let layout = self.array_layout_for_expr(expr)?;
                let item_ty = match actual_ty.kind(self.backend.db) {
                    TyKind::Array(item_ty) => Some(*item_ty),
                    _ => None,
                };
                let items = nodes
                    .array(nodes.as_array(expr).expect("Array node mismatch"))
                    .iter()
                    .map(|item| self.lower_expr_as(item, item_ty))
                    .collect::<Result<Vec<_>, _>>()?;
                KernelExprKind::Array { layout, items }
            }
            NodeKind::ArrayRepeat => {
                let layout = self.array_layout_for_expr(expr)?;
                let (value, len) =
                    nodes.array_repeat(nodes.as_array_repeat(expr).expect("ArrayRepeat mismatch"));
                let item_ty = match actual_ty.kind(self.backend.db) {
                    TyKind::Array(item_ty) => Some(*item_ty),
                    _ => None,
                };
                KernelExprKind::ArrayRepeat {
                    layout,
                    value: Box::new(self.lower_expr_as(value, item_ty)?),
                    len: Box::new(self.lower_expr(len)?),
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(expr).expect("Block node mismatch"));
                let mut stmt_ids = stmts.iter().collect::<Vec<_>>();
                let expected_value = expected_ty.filter(|expected| {
                    !matches!(expected.kind(self.backend.db), TyKind::Tuple(items) if items.is_empty())
                });
                let tail_expr = if tail != ExprId::ZERO {
                    Some(tail)
                } else if expected_value.is_some() {
                    let candidate = stmt_ids.last().and_then(|stmt| stmt_as_expr(nodes, *stmt));
                    if candidate.is_some() {
                        stmt_ids.pop();
                    }
                    candidate
                } else {
                    None
                };
                let stmts = stmt_ids
                    .iter()
                    .map(|stmt| self.lower_stmt(*stmt))
                    .collect::<Result<Vec<_>, _>>()?;
                let tail = tail_expr
                    .map(|tail| self.lower_expr_as(tail, Some(actual_ty)))
                    .transpose()?
                    .map(Self::clone_if_borrowed)
                    .map(Box::new);
                KernelExprKind::Block { stmts, tail }
            }
            NodeKind::UnsafeBlock => {
                let (body, _) =
                    nodes.unsafe_block(nodes.as_unsafe_block(expr).expect("UnsafeBlock mismatch"));
                if body == ExprId::ZERO {
                    KernelExprKind::Unit
                } else {
                    return self.lower_expr_as(body, expected_ty);
                }
            }
            NodeKind::Call => {
                if let Some(intrinsic_kind) = self.lower_compiler_intrinsic_call(expr, &abi)? {
                    intrinsic_kind
                } else if self.is_variant_constructor_call(expr)? {
                    let (callee, args) =
                        nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                    let variant = self.enum_variant_layout_for_call(expr, callee)?;
                    let args = args
                        .iter()
                        .map(|arg| self.lower_expr(arg))
                        .collect::<Result<Vec<_>, _>>()?;
                    KernelExprKind::VariantCall { variant, args }
                } else {
                    let (callee, args) =
                        nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                    let param_tys = self.call_param_tys(callee)?;
                    if self.is_direct_call(callee)? {
                        let target = self.lower_call_target(callee, expr)?;
                        let target_function = self.direct_call_target_function(callee)?;
                        let call_args = if let Some(method) = self.resolve_method_call(callee)? {
                            let mut call_args = Vec::with_capacity(args.len() + 1);
                            call_args.push(method.receiver);
                            call_args.extend(args.iter());
                            call_args
                        } else {
                            args.iter().collect::<Vec<_>>()
                        };
                        let args = self.lower_call_args(
                            call_args.as_slice(),
                            &param_tys,
                            target_function,
                        )?;
                        KernelExprKind::Call { target, args }
                    } else {
                        let signature = self.call_signature(callee)?;
                        let args = self.lower_call_args(
                            args.iter().collect::<Vec<_>>().as_slice(),
                            &param_tys,
                            None,
                        )?;
                        KernelExprKind::IndirectCall {
                            callee: Box::new(self.lower_expr(callee)?),
                            signature,
                            args,
                        }
                    }
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(expr).expect("Binary node mismatch"));
                KernelExprKind::Binary {
                    op: self.lower_binary_op(binary.op)?,
                    lhs: Box::new(self.lower_expr(binary.lhs)?),
                    rhs: Box::new(self.lower_expr(binary.rhs)?),
                }
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(expr).expect("Prefix node mismatch"));
                KernelExprKind::Prefix {
                    op: self.lower_prefix_op(prefix.op)?,
                    expr: Box::new(self.lower_expr(prefix.expr)?),
                }
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(expr).expect("LoopExpr node mismatch"));
                let body = (body != ExprId::ZERO)
                    .then(|| self.lower_expr(body))
                    .transpose()?
                    .map(Box::new);
                KernelExprKind::Loop { body }
            }
            NodeKind::BreakExpr => KernelExprKind::Break,
            NodeKind::ContinueExpr => KernelExprKind::Continue,
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(expr).expect("If node mismatch"));
                KernelExprKind::If {
                    cond: Box::new(self.lower_expr(if_expr.cond)?),
                    then_branch: (if_expr.then_branch != ExprId::ZERO)
                        .then(|| self.lower_expr_as(if_expr.then_branch, Some(actual_ty)))
                        .transpose()?
                        .map(Self::clone_if_borrowed)
                        .map(Box::new),
                    else_branch: (if_expr.else_branch != ExprId::ZERO)
                        .then(|| self.lower_expr_as(if_expr.else_branch, Some(actual_ty)))
                        .transpose()?
                        .map(Self::clone_if_borrowed)
                        .map(Box::new),
                }
            }
            NodeKind::Match => {
                let (scrutinee, arms) =
                    nodes.match_expr(nodes.as_match(expr).expect("Match node mismatch"));
                let scrutinee_ty = self
                    .backend
                    .specialized_expr_ty(&self.owner_instance, self.inference, scrutinee)
                    .unwrap_or_else(|| Ty::new(self.backend.db, TyKind::Unknown));
                let scrutinee_abi = self.expr_abi(scrutinee)?;
                let arms = arms
                    .iter()
                    .map(|arm| {
                        let (pattern, body) =
                            nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm mismatch"));
                        Ok(KernelMatchArm {
                            pattern: self.lower_pattern(
                                pattern,
                                scrutinee_ty,
                                &scrutinee_abi,
                                expr,
                            )?,
                            body: Self::clone_if_borrowed(
                                self.lower_expr_as(body, Some(actual_ty))?,
                            ),
                        })
                    })
                    .collect::<Result<Vec<_>, Diagnostic>>()?;
                KernelExprKind::Match { scrutinee: Box::new(self.lower_expr(scrutinee)?), arms }
            }
            NodeKind::Field if self.is_enum_variant_ref(expr)? => {
                KernelExprKind::VariantValue { variant: self.enum_variant_layout(expr)? }
            }
            NodeKind::Field => {
                let (base, field) = self.project_field(expr)?;
                if self.should_lower_field_via_stable_address(base)? {
                    let ownership = if field.ty.contains_heap_refs() {
                        ValueOwnership::Borrowed
                    } else {
                        ValueOwnership::None
                    };
                    let (addr, access) = if field.ty.is_aggregate() {
                        (self.lower_place_address(expr)?, None)
                    } else {
                        (self.lower_place_address(base)?, abi_mem_access(&field.ty, field.offset))
                    };
                    return Ok(KernelExpr {
                        source: expr,
                        abi: field.ty.clone(),
                        ownership,
                        kind: KernelExprKind::MemoryRead { addr: Box::new(addr), access },
                    });
                }
                let base = self.lower_expr(base)?;
                let ownership = if field.ty.contains_heap_refs() {
                    if base.ownership.is_owned() {
                        ValueOwnership::Owned
                    } else {
                        ValueOwnership::Borrowed
                    }
                } else {
                    ValueOwnership::None
                };
                return Ok(KernelExpr {
                    source: expr,
                    abi: field.ty.clone(),
                    ownership,
                    kind: KernelExprKind::Field { base: Box::new(base), field },
                });
            }
            NodeKind::StructExpr => {
                let layout = if let Some(layout) = abi.aggregate() {
                    layout.clone()
                } else {
                    self.nominal_payload_layout(expr)?
                };
                let fields = layout.fields().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: struct or record layout is missing field metadata",
                        self.node_range(expr),
                    )
                })?;
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(expr).expect("StructExpr mismatch"));
                let has_struct_name = items.len() % 2 == 1;
                let mut values = FxHashMap::default();
                let mut index = if has_struct_name { 1 } else { 0 };
                while index + 1 < items.len() {
                    let field_name = items.get(index).unwrap();
                    let value_expr = items.get(index + 1).unwrap();
                    index += 2;

                    let name = nodes.name(nodes.as_name(field_name).expect("field name"));
                    values.insert(symbol_bits(name), value_expr);
                }

                let fields = fields
                    .iter()
                    .filter_map(|field| {
                        Some((field.clone(), values.get(&field.name_bits?)?.to_owned()))
                    })
                    .map(|(field, value_expr)| {
                        self.lower_expr(value_expr).map(|value| KernelFieldValue { field, value })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                KernelExprKind::Struct { fields }
            }
            NodeKind::Closure => self.lower_closure_expr(expr)?,
            kind => {
                return Err(Diagnostic::error(
                    format!("internal error: unsupported node in backend lowering: {kind:?}"),
                    self.node_range(expr),
                ));
            }
        };

        let ownership = Self::expr_ownership(&kind, &abi);
        let lowered = KernelExpr { source: expr, abi, ownership, kind };
        let lowered = self.coerce_expr_to_expected(lowered, actual_ty, expected_ty)?;
        self.require_copyable_value_use(expr, actual_ty, &lowered)?;
        Ok(lowered)
    }

    fn array_layout_for_expr(&self, expr: ExprId) -> Result<ArrayRuntimeLayout, Diagnostic> {
        let ty = self
            .backend
            .specialized_expr_ty(&self.owner_instance, self.inference, expr)
            .ok_or_else(|| {
                Diagnostic::error(
                    "internal error: missing inferred array type during backend lowering",
                    self.node_range(expr),
                )
            })?;
        crate::capability::supported_array_runtime_layout_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support this array value type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(expr)))
    }

    fn clone_if_borrowed(expr: KernelExpr<'db>) -> KernelExpr<'db> {
        if !expr.abi.contains_heap_refs() || !expr.ownership.is_borrowed() {
            return expr;
        }

        KernelExpr {
            source: expr.source,
            abi: expr.abi.clone(),
            ownership: ValueOwnership::Owned,
            kind: KernelExprKind::Clone { value: Box::new(expr) },
        }
    }

    fn coerce_expr_to_expected(
        &self,
        expr: KernelExpr<'db>,
        actual_ty: Ty<'db>,
        expected_ty: Option<Ty<'db>>,
    ) -> Result<KernelExpr<'db>, Diagnostic> {
        let Some(expected_ty) = expected_ty else {
            return Ok(expr);
        };
        if expected_ty == actual_ty {
            return Ok(expr);
        }

        let Some((union_abi, variant)) =
            self.union_coercion_target(actual_ty, expected_ty, expr.source)?
        else {
            return Ok(expr);
        };

        match expr.kind {
            KernelExprKind::Block { stmts, tail } => {
                let tail = match tail {
                    Some(tail) => {
                        Some(Box::new(self.coerce_lowered_expr_to_expected(*tail, expected_ty)?))
                    }
                    None => Some(Box::new(Self::unit_union_expr(
                        expr.source,
                        union_abi.clone(),
                        variant.clone(),
                    ))),
                };
                let kind = KernelExprKind::Block { stmts, tail };
                let ownership = Self::expr_ownership(&kind, &union_abi);
                Ok(KernelExpr { source: expr.source, abi: union_abi, ownership, kind })
            }
            KernelExprKind::If { cond, then_branch, else_branch } => {
                let then_branch = then_branch
                    .map(|branch| self.coerce_lowered_expr_to_expected(*branch, expected_ty))
                    .transpose()?
                    .map(Box::new);
                let else_branch = else_branch
                    .map(|branch| self.coerce_lowered_expr_to_expected(*branch, expected_ty))
                    .transpose()?
                    .map(Box::new);
                let kind = KernelExprKind::If { cond, then_branch, else_branch };
                let ownership = Self::expr_ownership(&kind, &union_abi);
                Ok(KernelExpr { source: expr.source, abi: union_abi, ownership, kind })
            }
            KernelExprKind::Match { scrutinee, arms } => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        Ok(KernelMatchArm {
                            pattern: arm.pattern,
                            body: self.coerce_lowered_expr_to_expected(arm.body, expected_ty)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Diagnostic>>()?;
                let kind = KernelExprKind::Match { scrutinee, arms };
                let ownership = Self::expr_ownership(&kind, &union_abi);
                Ok(KernelExpr { source: expr.source, abi: union_abi, ownership, kind })
            }
            _ => {
                let source = expr.source;
                let value = if matches!(expr.abi, AbiTy::Scalar(BackendTy::Unit)) {
                    None
                } else {
                    Some(Box::new(expr))
                };
                let kind = KernelExprKind::Union { variant, value };
                let ownership = Self::expr_ownership(&kind, &union_abi);
                Ok(KernelExpr { source, abi: union_abi, ownership, kind })
            }
        }
    }

    fn coerce_lowered_expr_to_expected(
        &self,
        expr: KernelExpr<'db>,
        expected_ty: Ty<'db>,
    ) -> Result<KernelExpr<'db>, Diagnostic> {
        let actual_ty = self
            .backend
            .specialized_expr_ty(&self.owner_instance, self.inference, expr.source)
            .ok_or_else(|| {
                Diagnostic::error(
                    "internal error: missing inferred type during union coercion",
                    self.node_range(expr.source),
                )
            })?;
        self.coerce_expr_to_expected(expr, actual_ty, Some(expected_ty))
    }

    fn require_copyable_value_use(
        &self,
        expr: ExprId,
        actual_ty: Ty<'db>,
        lowered: &KernelExpr<'db>,
    ) -> Result<(), Diagnostic> {
        if !lowered.ownership.is_borrowed() || ownership::is_copyable(self.backend.db, actual_ty) {
            return Ok(());
        }

        Err(Diagnostic::error(
            format!(
                "non-copy value `{}` cannot be used by value yet",
                actual_ty.display(self.backend.db)
            ),
            self.node_range(expr),
        ))
    }

    fn union_coercion_target(
        &self,
        actual_ty: Ty<'db>,
        expected_ty: Ty<'db>,
        source: ExprId,
    ) -> Result<Option<(AbiTy, VariantLayout)>, Diagnostic> {
        let TyKind::Union(members) = expected_ty.kind(self.backend.db) else {
            return Ok(None);
        };
        let Some(arm_index) = members.iter().position(|member| *member == actual_ty) else {
            return Ok(None);
        };
        let union_abi = crate::capability::supported_value_abi_or_message(
            self.backend.db,
            expected_ty,
            "Wasm backend does not support this union value type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(source)))?;
        let layout = union_abi.aggregate().ok_or_else(|| {
            Diagnostic::error(
                "internal error: union value is missing aggregate layout metadata",
                self.node_range(source),
            )
        })?;
        let AggregateKind::Enum(enum_layout) = &layout.kind else {
            return Err(Diagnostic::error(
                "internal error: union value is missing enum-style aggregate metadata",
                self.node_range(source),
            ));
        };
        let variant = enum_layout.variants.get(arm_index).cloned().ok_or_else(|| {
            Diagnostic::error(
                "internal error: union arm index was out of range during backend lowering",
                self.node_range(source),
            )
        })?;
        Ok(Some((union_abi, variant)))
    }

    fn unit_union_expr(source: ExprId, abi: AbiTy, variant: VariantLayout) -> KernelExpr<'db> {
        let kind = KernelExprKind::Union { variant, value: None };
        let ownership = Self::expr_ownership(&kind, &abi);
        KernelExpr { source, abi, ownership, kind }
    }

    fn expr_ownership(kind: &KernelExprKind<'db>, abi: &AbiTy) -> ValueOwnership {
        if !abi.contains_heap_refs() {
            return ValueOwnership::None;
        }

        match kind {
            KernelExprKind::Local(_)
            | KernelExprKind::Capture(_)
            | KernelExprKind::Field { .. } => ValueOwnership::Borrowed,
            KernelExprKind::Block { tail, .. } => {
                tail.as_ref().map_or(ValueOwnership::None, |tail| tail.ownership)
            }
            KernelExprKind::Clone { .. }
            | KernelExprKind::FunctionValue { .. }
            | KernelExprKind::ClosureValue { .. }
            | KernelExprKind::String(_)
            | KernelExprKind::StringFromBytes { .. }
            | KernelExprKind::MemoryRead { .. }
            | KernelExprKind::Array { .. }
            | KernelExprKind::ArrayRepeat { .. }
            | KernelExprKind::Call { .. }
            | KernelExprKind::IndirectCall { .. }
            | KernelExprKind::If { .. }
            | KernelExprKind::Match { .. }
            | KernelExprKind::Tuple { .. }
            | KernelExprKind::Struct { .. }
            | KernelExprKind::Union { .. }
            | KernelExprKind::VariantValue { .. }
            | KernelExprKind::VariantCall { .. } => ValueOwnership::Owned,
            KernelExprKind::Bool(_)
            | KernelExprKind::Int(_)
            | KernelExprKind::Float(_)
            | KernelExprKind::Char(_)
            | KernelExprKind::Unit
            | KernelExprKind::StackAddr { .. }
            | KernelExprKind::AddrOffset { .. }
            | KernelExprKind::MemoryWrite { .. }
            | KernelExprKind::PointerAdd { .. }
            | KernelExprKind::Binary { .. }
            | KernelExprKind::Prefix { .. }
            | KernelExprKind::Loop { .. }
            | KernelExprKind::Break
            | KernelExprKind::Continue => ValueOwnership::None,
        }
    }

    fn lower_pattern(
        &self,
        pattern: PatId,
        ty: Ty<'db>,
        abi: &AbiTy,
        anchor: ExprId,
    ) -> Result<BackendPattern, Diagnostic> {
        if let Some(pattern) = self.lower_selected_union_pattern(pattern, ty, abi, anchor)? {
            return Ok(pattern);
        }

        self.lower_pattern_inner(pattern, ty, abi, anchor)
    }

    fn lower_selected_union_pattern(
        &self,
        pattern: PatId,
        ty: Ty<'db>,
        abi: &AbiTy,
        anchor: ExprId,
    ) -> Result<Option<BackendPattern>, Diagnostic> {
        let TyKind::Union(members) = ty.kind(self.backend.db) else {
            return Ok(None);
        };
        let Some(selected_member) = self.inference.selected_union_member(pattern) else {
            return Ok(None);
        };

        let selected_member = self.backend.specialize_ty(&self.owner_instance, selected_member);
        let layout = self.pattern_aggregate_layout(ty, abi, anchor)?;
        let AggregateKind::Enum(enum_layout) = &layout.kind else {
            return Err(Diagnostic::error(
                "internal error: union pattern is missing enum-style layout metadata",
                self.node_range(anchor),
            ));
        };
        let arm_index =
            members.iter().position(|member| *member == selected_member).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: selected union member was not present in the scrutinee type",
                    self.node_range(anchor),
                )
            })?;
        let variant = enum_layout.variants.get(arm_index).cloned().ok_or_else(|| {
            Diagnostic::error(
                "internal error: selected union arm was out of range during pattern lowering",
                self.node_range(anchor),
            )
        })?;
        let payload_abi =
            variant.fields.first().map_or(AbiTy::Scalar(BackendTy::Unit), |field| field.ty.clone());
        let nested = self.lower_pattern_inner(pattern, selected_member, &payload_abi, anchor)?;
        let fields = variant
            .fields
            .first()
            .cloned()
            .map(|field| vec![BackendPatternField { field, pattern: nested }])
            .unwrap_or_default();
        Ok(Some(BackendPattern::Variant { variant, fields }))
    }

    fn lower_pattern_inner(
        &self,
        pattern: PatId,
        ty: Ty<'db>,
        abi: &AbiTy,
        anchor: ExprId,
    ) -> Result<BackendPattern, Diagnostic> {
        let nodes = self.function.node_store();
        if pattern == PatId::ZERO {
            return Ok(BackendPattern::Wildcard);
        }

        match nodes.node_kind(pattern) {
            NodeKind::PatBinding => {
                let (name, _) =
                    nodes.pat_binding(nodes.as_pat_binding(pattern).expect("PatBinding mismatch"));
                Ok(BackendPattern::Binding(name))
            }
            NodeKind::PatWildcard => Ok(BackendPattern::Wildcard),
            NodeKind::PatTyped => {
                let (inner, _) = nodes.pat_typed(nodes.as_pat_typed(pattern).expect("PatTyped"));
                self.lower_pattern(inner, ty, abi, anchor)
            }
            NodeKind::PatTrue => Ok(BackendPattern::Literal(BackendPatternLiteral::Bool(true))),
            NodeKind::PatFalse => Ok(BackendPattern::Literal(BackendPatternLiteral::Bool(false))),
            NodeKind::PatInt => {
                let literal = nodes.pat_int(nodes.as_pat_int(pattern).expect("PatInt mismatch"));
                Ok(BackendPattern::Literal(BackendPatternLiteral::Int(
                    parse_int_literal(literal, self.backend.db)
                        .map_err(|message| Diagnostic::error(message, self.node_range(anchor)))?,
                )))
            }
            NodeKind::PatString => {
                let literal =
                    nodes.pat_string(nodes.as_pat_string(pattern).expect("PatString mismatch"));
                let offset = self.static_data.offset(literal).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing static string data during pattern lowering",
                        self.node_range(anchor),
                    )
                })?;
                Ok(BackendPattern::Literal(BackendPatternLiteral::String(offset)))
            }
            NodeKind::PatChar => {
                let literal = nodes.pat_char(nodes.as_pat_char(pattern).expect("PatChar mismatch"));
                Ok(BackendPattern::Literal(BackendPatternLiteral::Char(
                    decode_char_literal(literal, self.backend.db)
                        .map_err(|message| Diagnostic::error(message, self.node_range(anchor)))?,
                )))
            }
            NodeKind::PatFloat => Err(Diagnostic::error(
                "Wasm backend does not support float patterns",
                self.node_range(anchor),
            )),
            NodeKind::PatParen => {
                let (inner, _) = nodes.pat_paren(nodes.as_pat_paren(pattern).expect("PatParen"));
                self.lower_pattern(inner, ty, abi, anchor)
            }
            NodeKind::PatTuple => {
                let layout = self.pattern_aggregate_layout(ty, abi, anchor)?;
                let fields = layout.fields().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: tuple pattern is missing field layout metadata",
                        self.node_range(anchor),
                    )
                })?;
                let item_ids = nodes
                    .pat_tuple(nodes.as_pat_tuple(pattern).expect("PatTuple mismatch"))
                    .iter()
                    .collect::<Vec<_>>();
                let item_tys = match ty.kind(self.backend.db) {
                    TyKind::Tuple(items) => items.clone(),
                    _ => vec![Ty::new(self.backend.db, TyKind::Unknown); item_ids.len()],
                };
                let items = fields
                    .iter()
                    .zip(item_ids.iter())
                    .enumerate()
                    .map(|(index, (field, &item))| {
                        let item_ty = item_tys
                            .get(index)
                            .copied()
                            .unwrap_or_else(|| Ty::new(self.backend.db, TyKind::Unknown));
                        Ok(BackendPatternField {
                            field: field.clone(),
                            pattern: self.lower_pattern(item, item_ty, &field.ty, anchor)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Diagnostic>>()?;
                Ok(BackendPattern::Tuple(items))
            }
            NodeKind::PatStruct => {
                let layout = self.pattern_aggregate_layout(ty, abi, anchor)?;
                let field_tys = match ty.kind(self.backend.db) {
                    TyKind::Struct(struct_ty) => struct_fields(self.backend.db, *struct_ty)
                        .iter()
                        .copied()
                        .collect::<FxHashMap<_, _>>(),
                    TyKind::Record(fields) => fields.iter().copied().collect::<FxHashMap<_, _>>(),
                    _ => FxHashMap::default(),
                };
                let (_, fields) =
                    nodes.pat_struct(nodes.as_pat_struct(pattern).expect("PatStruct mismatch"));
                let items = fields
                    .iter()
                    .map(|field| {
                        let (name, pat) = nodes.pat_struct_field(
                            nodes.as_pat_struct_field(field).expect("PatStructField mismatch"),
                        );
                        let field_sym = nodes.name(name);
                        let field_layout = layout
                            .field_named(symbol_bits(field_sym))
                            .cloned()
                            .ok_or_else(|| {
                                Diagnostic::error(
                                    "internal error: missing field layout during pattern lowering",
                                    self.node_range(name.into()),
                                )
                            })?;
                        let field_ty = field_tys
                            .get(&field_sym)
                            .copied()
                            .unwrap_or_else(|| Ty::new(self.backend.db, TyKind::Unknown));
                        let nested = if pat != PatId::ZERO {
                            self.lower_pattern(pat, field_ty, &field_layout.ty, name.into())?
                        } else {
                            BackendPattern::Binding(name)
                        };
                        Ok(BackendPatternField { field: field_layout, pattern: nested })
                    })
                    .collect::<Result<Vec<_>, Diagnostic>>()?;
                Ok(BackendPattern::Struct(items))
            }
            NodeKind::PatVariant => {
                let layout = self.pattern_aggregate_layout(ty, abi, anchor)?;
                let AggregateKind::Enum(enum_layout) = &layout.kind else {
                    return Err(Diagnostic::error(
                        "internal error: enum pattern is missing enum layout metadata",
                        self.node_range(anchor),
                    ));
                };
                let (path, args) =
                    nodes.pat_variant(nodes.as_pat_variant(pattern).expect("PatVariant mismatch"));
                let field_id = nodes.as_field(path).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: variant pattern is missing its path",
                        self.node_range(anchor),
                    )
                })?;
                let (_, variant_name_expr) = nodes.field(field_id);
                let variant_name =
                    nodes.as_name(variant_name_expr).expect("variant name should lower to Name");
                let variant_sym = nodes.name(variant_name);
                let variant = enum_layout
                    .variants
                    .iter()
                    .find(|variant| variant.name_bits == symbol_bits(variant_sym))
                    .cloned()
                    .ok_or_else(|| {
                        Diagnostic::error(
                            "internal error: missing enum variant layout during pattern lowering",
                            self.node_range(variant_name_expr),
                        )
                    })?;
                let payload_tys = match ty.kind(self.backend.db) {
                    TyKind::Enum(enum_ty) => enum_variants(self.backend.db, *enum_ty)
                        .iter()
                        .find(|(name, _)| *name == variant_sym)
                        .map(|(_, payload)| payload.clone())
                        .unwrap_or_default(),
                    _ => Vec::new(),
                };
                let arg_ids = args.iter().collect::<Vec<_>>();
                let fields = variant
                    .fields
                    .iter()
                    .zip(arg_ids.iter())
                    .enumerate()
                    .map(|(index, (field, &arg))| {
                        let field_ty = payload_tys
                            .get(index)
                            .copied()
                            .unwrap_or_else(|| Ty::new(self.backend.db, TyKind::Unknown));
                        Ok(BackendPatternField {
                            field: field.clone(),
                            pattern: self.lower_pattern(
                                arg,
                                field_ty,
                                &field.ty,
                                variant_name_expr,
                            )?,
                        })
                    })
                    .collect::<Result<Vec<_>, Diagnostic>>()?;
                Ok(BackendPattern::Variant { variant, fields })
            }
            kind => Err(Diagnostic::error(
                format!("internal error: unsupported pattern `{kind:?}` reached backend lowering"),
                self.node_range(anchor),
            )),
        }
    }

    fn pattern_aggregate_layout(
        &self,
        ty: Ty<'db>,
        abi: &AbiTy,
        source: ExprId,
    ) -> Result<AggregateLayout, Diagnostic> {
        if let Some(layout) = abi.aggregate() {
            return Ok(layout.clone());
        }
        crate::capability::supported_internal_nominal_payload_layout_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support this nominal value type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(source)))
    }

    fn lower_name_expr(&self, expr: ExprId) -> Result<KernelExprKind<'db>, Diagnostic> {
        match self.resolve_name(expr)? {
            BindingId::Local(name) | BindingId::Param(name) => Ok(self
                .capture_fields
                .get(&name)
                .cloned()
                .map_or(KernelExprKind::Local(name), KernelExprKind::Capture)),
            BindingId::Function(target) => {
                let instance = self.backend.instance_for_function_value(
                    &self.owner_instance,
                    self.inference,
                    target,
                    expr,
                    self.node_range(expr),
                )?;
                Ok(KernelExprKind::FunctionValue {
                    target: FunctionValueTarget::Function(instance),
                })
            }
            BindingId::RuntimeFunction(_) => Err(Diagnostic::error(
                "internal error: runtime functions are not supported as first-class values",
                self.node_range(expr),
            )),
            BindingId::CompilerIntrinsic(intrinsic) => Err(Diagnostic::error(
                format!(
                    "internal error: Backend intrinsic `{}` is not supported as a first-class \
                     value",
                    intrinsic.source_name()
                ),
                self.node_range(expr),
            )),
            BindingId::Struct(_)
            | BindingId::Enum(_)
            | BindingId::EnumVariant(_)
            | BindingId::BuiltinType(_) => Err(Diagnostic::error(
                "internal error: type values are not supported in backend lowering",
                self.node_range(expr),
            )),
        }
    }

    fn lower_compiler_intrinsic_call(
        &mut self,
        expr: ExprId,
        abi: &AbiTy,
    ) -> Result<Option<KernelExprKind<'db>>, Diagnostic> {
        let nodes = self.function.node_store();
        if nodes.node_kind(expr) != NodeKind::Call {
            return Ok(None);
        }
        let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
        let Some(name_id) = nodes.as_name(callee) else {
            return Ok(None);
        };
        let symbol = nodes.name(name_id);
        let Some(BindingId::CompilerIntrinsic(intrinsic)) = self.resolve_name(callee).ok() else {
            return Ok(None);
        };

        if intrinsic == CompilerIntrinsic::Comptime {
            self.lower_comptime_call(expr, abi).map(Some)
        } else if intrinsic.is_reflection() {
            if !self.backend.is_stage_mode() {
                return Err(Diagnostic::error(
                    format!(
                        "`{}` is only supported during `comptime` execution",
                        symbol.text(self.backend.db)
                    ),
                    self.node_range(expr),
                ));
            }
            self.lower_reflection_call(expr, intrinsic, args.iter().collect::<Vec<_>>().as_slice())
                .map(Some)
        } else {
            if self.backend.is_stage_mode() {
                return Err(Diagnostic::error(
                    format!(
                        "`{}` is not supported during `comptime` execution",
                        symbol.text(self.backend.db)
                    ),
                    self.node_range(expr),
                ));
            }
            self.lower_unsafe_intrinsic_call(
                expr,
                abi,
                intrinsic,
                args.iter().collect::<Vec<_>>().as_slice(),
            )
            .map(Some)
        }
    }

    fn lower_comptime_call(
        &mut self,
        expr: ExprId,
        _abi: &AbiTy,
    ) -> Result<KernelExprKind<'db>, Diagnostic> {
        let target = resolve_comptime_target(
            self.backend,
            self.location,
            self.function,
            expr,
            self.node_range(expr),
        )?
        .ok_or_else(|| {
            Diagnostic::error(
                "internal error: malformed `comptime` call reached backend lowering",
                self.node_range(expr),
            )
        })?;
        let value = self
            .backend
            .comptime_evaluator
            .eval_comptime_function(self.backend.db, target)
            .map_err(|message| Diagnostic::error(message, self.node_range(expr)))?;
        let ty = self.expr_ty(expr)?;
        let static_value = self.static_data.comptime_value(self.location, expr);
        let value = comptime_value_from_abi(self.backend.db, ty, value, self.node_range(expr))?;
        Ok(self.lower_comptime_value_expr(expr, ty, value, static_value)?.kind)
    }

    fn lower_comptime_value_expr(
        &mut self,
        expr: ExprId,
        ty: Ty<'db>,
        value: ComptimeValue,
        static_value: Option<&ComptimeStaticValue>,
    ) -> Result<KernelExpr<'db>, Diagnostic> {
        let abi = crate::capability::supported_value_abi_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support this comptime result type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(expr)))?;
        let kind = match (ty.kind(self.backend.db), value) {
            (TyKind::Tuple(items), ComptimeValue::Unit) if items.is_empty() => KernelExprKind::Unit,
            (TyKind::Int, ComptimeValue::I32(value)) => KernelExprKind::Int(i64::from(value)),
            (TyKind::ExactInt(_), ComptimeValue::I32(value)) => {
                KernelExprKind::Int(i64::from(value))
            }
            (TyKind::Bool, ComptimeValue::Bool(value)) => KernelExprKind::Bool(value),
            (TyKind::Float, ComptimeValue::F64(value)) => KernelExprKind::Float(value),
            (TyKind::Char, ComptimeValue::Char(value)) => KernelExprKind::Char(value),
            (TyKind::String, ComptimeValue::String(_value)) => {
                let Some(ComptimeStaticValue::String(offset)) = static_value else {
                    return Err(Diagnostic::error(
                        "internal error: missing static data for comptime string result",
                        self.node_range(expr),
                    ));
                };
                KernelExprKind::String(*offset)
            }
            (TyKind::Array(item_ty), ComptimeValue::Array(items)) => {
                let layout = crate::capability::supported_array_runtime_layout_or_message(
                    self.backend.db,
                    ty,
                    "Wasm backend does not support this comptime result type",
                )
                .map_err(|message| Diagnostic::error(message, self.node_range(expr)))?;
                let Some(ComptimeStaticValue::Array(static_items)) = static_value else {
                    return Err(Diagnostic::error(
                        "internal error: missing static data for comptime array result",
                        self.node_range(expr),
                    ));
                };
                if static_items.len() != items.len() {
                    return Err(Diagnostic::error(
                        "internal error: comptime array static data length did not match",
                        self.node_range(expr),
                    ));
                }
                let items = items
                    .into_iter()
                    .zip(static_items.iter())
                    .map(|(item, static_item)| {
                        self.lower_comptime_value_expr(expr, *item_ty, item, Some(static_item))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                KernelExprKind::Array { layout, items }
            }
            (TyKind::Tuple(item_tys), ComptimeValue::Tuple(items)) => {
                let layout = abi.aggregate().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing tuple layout for comptime result",
                        self.node_range(expr),
                    )
                })?;
                let fields = layout.fields().unwrap_or(&[]);
                let Some(ComptimeStaticValue::Tuple(static_items)) = static_value else {
                    return Err(Diagnostic::error(
                        "internal error: missing static data for comptime tuple result",
                        self.node_range(expr),
                    ));
                };
                if static_items.len() != items.len() {
                    return Err(Diagnostic::error(
                        "internal error: comptime tuple static data length did not match",
                        self.node_range(expr),
                    ));
                }
                let fields = fields
                    .iter()
                    .zip(item_tys.iter().zip(items).zip(static_items.iter()))
                    .map(|(field, ((&item_ty, value), static_item))| {
                        self.lower_comptime_value_expr(expr, item_ty, value, Some(static_item))
                            .map(|value| KernelFieldValue { field: field.clone(), value })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                KernelExprKind::Tuple { fields }
            }
            (TyKind::Record(field_tys), ComptimeValue::Record(values)) => {
                let layout = abi.aggregate().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing record layout for comptime result",
                        self.node_range(expr),
                    )
                })?;
                let fields = layout.fields().unwrap_or(&[]);
                let mut field_tys = field_tys.clone();
                field_tys.sort_by_key(|(name, _)| name.text(self.backend.db).to_owned());
                let Some(ComptimeStaticValue::Record(static_values)) = static_value else {
                    return Err(Diagnostic::error(
                        "internal error: missing static data for comptime record result",
                        self.node_range(expr),
                    ));
                };
                if static_values.len() != values.len() {
                    return Err(Diagnostic::error(
                        "internal error: comptime record static data length did not match",
                        self.node_range(expr),
                    ));
                }
                let fields = fields
                    .iter()
                    .zip(field_tys.iter().zip(values).zip(static_values.iter()))
                    .map(|(field, (((_, field_ty), value), static_value))| {
                        self.lower_comptime_value_expr(expr, *field_ty, value, Some(static_value))
                            .map(|value| KernelFieldValue { field: field.clone(), value })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                KernelExprKind::Struct { fields }
            }
            (TyKind::Struct(struct_ty), ComptimeValue::Struct(values)) => {
                let layout =
                    crate::capability::supported_internal_nominal_payload_layout_or_message(
                        self.backend.db,
                        ty,
                        "Wasm backend does not support this comptime result type",
                    )
                    .map_err(|message| Diagnostic::error(message, self.node_range(expr)))?;
                let fields = layout.fields().unwrap_or(&[]);
                let field_tys = struct_fields(self.backend.db, *struct_ty);
                let Some(ComptimeStaticValue::Struct(static_values)) = static_value else {
                    return Err(Diagnostic::error(
                        "internal error: missing static data for comptime struct result",
                        self.node_range(expr),
                    ));
                };
                if static_values.len() != values.len() {
                    return Err(Diagnostic::error(
                        "internal error: comptime struct static data length did not match",
                        self.node_range(expr),
                    ));
                }
                let fields = fields
                    .iter()
                    .zip(field_tys.iter().zip(values).zip(static_values.iter()))
                    .map(|(field, (((_, field_ty), value), static_value))| {
                        self.lower_comptime_value_expr(expr, *field_ty, value, Some(static_value))
                            .map(|value| KernelFieldValue { field: field.clone(), value })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                KernelExprKind::Struct { fields }
            }
            (TyKind::Enum(enum_ty), ComptimeValue::Enum { variant_index, fields }) => {
                let layout =
                    crate::capability::supported_internal_nominal_payload_layout_or_message(
                        self.backend.db,
                        ty,
                        "Wasm backend does not support this comptime result type",
                    )
                    .map_err(|message| Diagnostic::error(message, self.node_range(expr)))?;
                let AggregateKind::Enum(enum_layout) = layout.kind else {
                    return Err(Diagnostic::error(
                        "internal error: expected enum layout for comptime result",
                        self.node_range(expr),
                    ));
                };
                let variant =
                    enum_layout.variants.get(variant_index).cloned().ok_or_else(|| {
                        Diagnostic::error(
                            "internal error: comptime enum variant index was out of range",
                            self.node_range(expr),
                        )
                    })?;
                let (_, field_tys) = enum_variants(self.backend.db, *enum_ty)
                    .get(variant_index)
                    .ok_or_else(|| {
                        Diagnostic::error(
                            "internal error: comptime enum variant metadata was missing",
                            self.node_range(expr),
                        )
                    })?;
                if fields.is_empty() {
                    KernelExprKind::VariantValue { variant }
                } else {
                    let Some(ComptimeStaticValue::Enum {
                        variant_index: static_variant_index,
                        fields: static_fields,
                    }) = static_value
                    else {
                        return Err(Diagnostic::error(
                            "internal error: missing static data for comptime enum result",
                            self.node_range(expr),
                        ));
                    };
                    if *static_variant_index != variant_index {
                        return Err(Diagnostic::error(
                            "internal error: comptime enum static data did not match the variant",
                            self.node_range(expr),
                        ));
                    }
                    if static_fields.len() != fields.len() {
                        return Err(Diagnostic::error(
                            "internal error: comptime enum static data length did not match",
                            self.node_range(expr),
                        ));
                    }
                    let args = field_tys
                        .iter()
                        .copied()
                        .zip(fields.into_iter().zip(static_fields.iter()))
                        .map(|(field_ty, (value, static_value))| {
                            self.lower_comptime_value_expr(
                                expr,
                                field_ty,
                                value,
                                Some(static_value),
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    KernelExprKind::VariantCall { variant, args }
                }
            }
            _ => {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: comptime result did not match `{}`",
                        ty.display(self.backend.db)
                    ),
                    self.node_range(expr),
                ));
            }
        };
        let ownership = Self::expr_ownership(&kind, &abi);
        Ok(KernelExpr { source: expr, abi, ownership, kind })
    }

    fn lower_reflection_call(
        &mut self,
        expr: ExprId,
        intrinsic: CompilerIntrinsic,
        args: &[ExprId],
    ) -> Result<KernelExprKind<'db>, Diagnostic> {
        let stage_intrinsic =
            StageIntrinsic::from_compiler_intrinsic(intrinsic).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: non-reflection intrinsic reached stage lowering",
                    self.node_range(expr),
                )
            })?;
        let mut lowered_args = Vec::with_capacity(args.len());
        match stage_intrinsic {
            StageIntrinsic::TypeName
            | StageIntrinsic::FieldCount
            | StageIntrinsic::FieldName
            | StageIntrinsic::VariantCount
            | StageIntrinsic::VariantName => {
                lowered_args.push(self.encoded_type_arg(args[0])?);
            }
            StageIntrinsic::FunctionParamCount
            | StageIntrinsic::FunctionParamTypeName
            | StageIntrinsic::FunctionReturnTypeName => {
                lowered_args.push(self.encoded_function_arg(args[0])?);
            }
        }
        if args.len() > 1 {
            lowered_args.push(self.lower_expr(args[1])?);
        }
        Ok(KernelExprKind::Call {
            target: BackendCallTarget {
                callable: BackendCallable::StageIntrinsic(stage_intrinsic),
                signature: stage_intrinsic_signature(stage_intrinsic),
            },
            args: lowered_args,
        })
    }

    fn lower_unsafe_intrinsic_call(
        &mut self,
        expr: ExprId,
        abi: &AbiTy,
        intrinsic: CompilerIntrinsic,
        args: &[ExprId],
    ) -> Result<KernelExprKind<'db>, Diagnostic> {
        match intrinsic {
            CompilerIntrinsic::StackAlloc => {
                let temp = self.layout.temps.get(&expr).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: `stack_alloc` is missing planned frame storage",
                        self.node_range(expr),
                    )
                })?;
                Ok(KernelExprKind::StackAddr { frame_slot: temp.frame_slot })
            }
            CompilerIntrinsic::PtrRead => {
                let addr = self.lower_expr(args[0])?;
                let ty = self.expr_ty(args[0])?;
                let TyKind::Pointer { pointee, .. } = ty.kind(self.backend.db) else {
                    return Err(Diagnostic::error(
                        "internal error: `ptr_read` did not infer a pointer type",
                        self.node_range(expr),
                    ));
                };
                Ok(KernelExprKind::MemoryRead {
                    addr: Box::new(addr),
                    access: pointee_mem_access(self.backend.db, *pointee, abi, 0),
                })
            }
            CompilerIntrinsic::PtrWrite => {
                let addr = self.lower_expr(args[0])?;
                let value = self.lower_expr(args[1])?;
                Ok(KernelExprKind::MemoryWrite { addr: Box::new(addr), value: Box::new(value) })
            }
            CompilerIntrinsic::PtrAdd => {
                let ptr = self.lower_expr(args[0])?;
                let count = self.lower_expr(args[1])?;
                let ty = self.expr_ty(args[0])?;
                let TyKind::Pointer { pointee, .. } = ty.kind(self.backend.db) else {
                    return Err(Diagnostic::error(
                        "internal error: `ptr_add` did not infer a pointer type",
                        self.node_range(expr),
                    ));
                };
                let stride = pointee_stride(self.backend.db, *pointee).ok_or_else(|| {
                    Diagnostic::error(
                        "Wasm backend does not support this `ptr_add` pointee type",
                        self.node_range(expr),
                    )
                })?;
                Ok(KernelExprKind::PointerAdd {
                    ptr: Box::new(ptr),
                    count: Box::new(count),
                    stride,
                })
            }
            CompilerIntrinsic::StrBytes => {
                let value = self.lower_expr(args[0])?;
                self.lower_byte_view(expr, abi, &value, false)
            }
            CompilerIntrinsic::StrFromUtf8Unchecked => {
                let ptr = self.lower_expr(args[0])?;
                let len = self.lower_expr(args[1])?;
                Ok(KernelExprKind::StringFromBytes { ptr: Box::new(ptr), len: Box::new(len) })
            }
            CompilerIntrinsic::ArrayMutBytes => {
                let value = self.lower_expr(args[0])?;
                self.lower_byte_view(expr, abi, &value, true)
            }
            CompilerIntrinsic::Comptime
            | CompilerIntrinsic::TypeName
            | CompilerIntrinsic::FieldCount
            | CompilerIntrinsic::FieldName
            | CompilerIntrinsic::VariantCount
            | CompilerIntrinsic::VariantName
            | CompilerIntrinsic::FunctionParamCount
            | CompilerIntrinsic::FunctionParamTypeName
            | CompilerIntrinsic::FunctionReturnTypeName => unreachable!("handled above"),
        }
    }

    fn lower_byte_view(
        &mut self,
        expr: ExprId,
        abi: &AbiTy,
        value: &KernelExpr<'db>,
        mutable: bool,
    ) -> Result<KernelExprKind<'db>, Diagnostic> {
        let layout = abi.aggregate().ok_or_else(|| {
            Diagnostic::error(
                "internal error: byte view result is missing aggregate layout",
                self.node_range(expr),
            )
        })?;
        let fields = layout.fields().ok_or_else(|| {
            Diagnostic::error(
                "internal error: byte view result is missing field metadata",
                self.node_range(expr),
            )
        })?;
        let ptr_bits = symbol_bits("ptr".into_symbol(self.backend.db));
        let len_bits = symbol_bits("len".into_symbol(self.backend.db));
        let (ptr_offset, len_offset) = match value.abi {
            AbiTy::Scalar(BackendTy::Ref(RefKind::String)) => (4, 0),
            AbiTy::Scalar(BackendTy::Ref(RefKind::Array(_))) if mutable => {
                let layout = self.array_layout_for_expr(value.source)?;
                (layout.data_offset, ARRAY_LEN_OFFSET)
            }
            _ => {
                return Err(Diagnostic::error(
                    "internal error: unexpected byte-view base type",
                    self.node_range(expr),
                ));
            }
        };
        let values = fields
            .iter()
            .map(|field| {
                let value = match field.name_bits {
                    Some(bits) if bits == ptr_bits => KernelExpr {
                        source: expr,
                        abi: AbiTy::Scalar(BackendTy::Int),
                        ownership: ValueOwnership::None,
                        kind: KernelExprKind::AddrOffset {
                            base: Box::new(value.clone()),
                            offset: ptr_offset,
                        },
                    },
                    Some(bits) if bits == len_bits => KernelExpr {
                        source: expr,
                        abi: AbiTy::Scalar(BackendTy::Int),
                        ownership: ValueOwnership::None,
                        kind: KernelExprKind::MemoryRead {
                            addr: Box::new(value.clone()),
                            access: Some(MemAccess::i32(len_offset, 2)),
                        },
                    },
                    _ => {
                        return Err(Diagnostic::error(
                            "internal error: unexpected byte-view field layout",
                            self.node_range(expr),
                        ));
                    }
                };
                Ok(KernelFieldValue { field: field.clone(), value })
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(KernelExprKind::Struct { fields: values })
    }

    fn encoded_type_arg(&self, expr: ExprId) -> Result<KernelExpr<'db>, Diagnostic> {
        let ty = self.resolve_name_ty(expr).map_err(|_diagnostic| {
            Diagnostic::error(
                "internal error: reflection type argument did not resolve to a type",
                self.node_range(expr),
            )
        })?;
        let bits = u32::try_from(ty.as_id().as_bits()).map_err(|_error| {
            Diagnostic::error(
                "internal error: reflection type id exceeded the supported range",
                self.node_range(expr),
            )
        })?;
        Ok(KernelExpr {
            source: expr,
            abi: AbiTy::Scalar(BackendTy::Int),
            ownership: ValueOwnership::None,
            kind: KernelExprKind::Int(i64::from(bits)),
        })
    }

    fn encoded_function_arg(&self, expr: ExprId) -> Result<KernelExpr<'db>, Diagnostic> {
        let BindingId::Function(function) = self.resolve_name(expr)? else {
            return Err(Diagnostic::error(
                "internal error: reflection function argument did not resolve to a function",
                self.node_range(expr),
            ));
        };
        let bits = u32::try_from(function.as_id().as_bits()).map_err(|_error| {
            Diagnostic::error(
                "internal error: reflection function id exceeded the supported range",
                self.node_range(expr),
            )
        })?;
        Ok(KernelExpr {
            source: expr,
            abi: AbiTy::Scalar(BackendTy::Int),
            ownership: ValueOwnership::None,
            kind: KernelExprKind::Int(i64::from(bits)),
        })
    }

    fn lower_closure_expr(&mut self, expr: ExprId) -> Result<KernelExprKind<'db>, Diagnostic> {
        let closure = ClosureInstanceKey {
            owner: self.location,
            type_args: self.owner_instance.type_args.clone(),
            closure: expr,
        };
        let info = self.backend.closure_info(&closure, self.function, self.inference)?;
        let fields = info
            .captures
            .into_iter()
            .map(|capture| {
                self.value_expr_for_binding(capture.binding, expr)
                    .map(|value| KernelFieldValue { field: capture.field, value })
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(KernelExprKind::ClosureValue {
            target: closure,
            env: KernelClosureEnvInit { layout: info.env_layout, fields },
        })
    }

    fn value_expr_for_binding(
        &self,
        binding: NameId,
        source: ExprId,
    ) -> Result<KernelExpr<'db>, Diagnostic> {
        let ty = self
            .inference
            .type_of_node(binding.into())
            .ok_or_else(|| {
                Diagnostic::error(
                    "internal error: missing inferred binding type during closure lowering",
                    self.node_range(source),
                )
            })
            .map(|ty| self.backend.specialize_ty(&self.owner_instance, ty))?;
        let abi = crate::capability::supported_value_abi_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support capturing this value type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(source)))?;
        let kind = self
            .capture_fields
            .get(&binding)
            .cloned()
            .map_or(KernelExprKind::Local(binding), KernelExprKind::Capture);
        let ownership = Self::expr_ownership(&kind, &abi);
        Ok(KernelExpr { source, abi, ownership, kind })
    }

    fn is_direct_call(&self, callee: ExprId) -> Result<bool, Diagnostic> {
        if self.function.node_store().node_kind(callee) == NodeKind::Name {
            return Ok(matches!(
                self.resolve_name(callee),
                Ok(BindingId::RuntimeFunction(_) | BindingId::Function(_))
            ));
        }

        Ok(self.resolve_method_call(callee)?.is_some())
    }

    fn call_signature(&self, callee: ExprId) -> Result<FunctionSignature, Diagnostic> {
        let Some(ty) =
            self.backend.specialized_expr_ty(&self.owner_instance, self.inference, callee)
        else {
            return Err(Diagnostic::error(
                "internal error: missing inferred callee type during indirect call lowering",
                self.node_range(callee),
            ));
        };
        crate::capability::supported_function_signature_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support this function value type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(callee)))
    }

    fn lower_call_args(
        &mut self,
        args: &[ExprId],
        param_tys: &[Ty<'db>],
        target_function: Option<FunctionLocation<'db>>,
    ) -> Result<Vec<KernelExpr<'db>>, Diagnostic> {
        args.iter()
            .enumerate()
            .map(|(index, &arg)| {
                if target_function.is_some_and(|function| {
                    self.function_param_is_mutable(function, index)
                        && self
                            .param_ty_needs_indirect_mutable_passing(param_tys.get(index).copied())
                            .unwrap_or(false)
                }) {
                    self.lower_place_address(arg)
                } else {
                    self.lower_expr_as(arg, param_tys.get(index).copied())
                }
            })
            .collect()
    }

    fn param_ty_needs_indirect_mutable_passing(
        &self,
        ty: Option<Ty<'db>>,
    ) -> Result<bool, Diagnostic> {
        let Some(ty) = ty else {
            return Ok(false);
        };
        Ok(crate::capability::supported_value_abi_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support this value type",
        )
        .map_err(|message| Diagnostic::error(message, self.backend.function_range(self.location)))?
        .is_aggregate())
    }

    fn call_param_tys(&self, callee: ExprId) -> Result<Vec<Ty<'db>>, Diagnostic> {
        let Some(ty) =
            self.backend.specialized_expr_ty(&self.owner_instance, self.inference, callee)
        else {
            return Err(Diagnostic::error(
                "internal error: missing inferred callee type during call lowering",
                self.node_range(callee),
            ));
        };

        let TyKind::Function { inputs, .. } = ty.kind(self.backend.db) else {
            return Err(Diagnostic::error(
                "internal error: non-function callee reached call argument lowering",
                self.node_range(callee),
            ));
        };

        Ok(inputs.clone())
    }

    fn lower_call_target(
        &self,
        callee: ExprId,
        call_expr: ExprId,
    ) -> Result<BackendCallTarget<'db>, Diagnostic> {
        if let Some(method) = self.resolve_method_call(callee)? {
            let nodes = self.function.node_store();
            let (_, args) = nodes.call(nodes.as_call(call_expr).expect("Call node mismatch"));
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(method.receiver);
            call_args.extend(args.iter());
            let instance = self.backend.instance_for_call(
                &self.owner_instance,
                self.inference,
                method.function,
                call_expr,
                call_args.as_slice(),
                self.node_range(call_expr),
            )?;
            let hir_function = method.function.hir_function(self.backend.db);
            let function = hir_function.function(self.backend.db);
            let inference = method.function.infer(self.backend.db);
            return Ok(BackendCallTarget {
                callable: BackendCallable::Function(instance.clone()),
                signature: self.backend.function_signature(&instance, function, inference)?,
            });
        }

        match self.resolve_name(callee)? {
            BindingId::RuntimeFunction(function) => Ok(BackendCallTarget {
                callable: BackendCallable::Runtime(function),
                signature: runtime_function_signature(function),
            }),
            BindingId::Function(target) => {
                let nodes = self.function.node_store();
                let (_, args) = nodes.call(nodes.as_call(call_expr).expect("Call node mismatch"));
                let args = args.iter().collect::<Vec<_>>();
                let instance = self.backend.instance_for_call(
                    &self.owner_instance,
                    self.inference,
                    target,
                    call_expr,
                    args.as_slice(),
                    self.node_range(call_expr),
                )?;
                let hir_function = target.hir_function(self.backend.db);
                let function = hir_function.function(self.backend.db);
                let inference = target.infer(self.backend.db);
                Ok(BackendCallTarget {
                    callable: BackendCallable::Function(instance.clone()),
                    signature: self.backend.function_signature(&instance, function, inference)?,
                })
            }
            _ => Err(Diagnostic::error(
                "internal error: non-direct call reached backend lowering",
                self.node_range(call_expr),
            )),
        }
    }

    fn direct_call_target_function(
        &self,
        callee: ExprId,
    ) -> Result<Option<FunctionLocation<'db>>, Diagnostic> {
        if let Some(method) = self.resolve_method_call(callee)? {
            return Ok(Some(method.function));
        }
        match self.resolve_name(callee)? {
            BindingId::Function(target) => Ok(Some(target)),
            BindingId::RuntimeFunction(_) => Ok(None),
            _ => Err(Diagnostic::error(
                "internal error: non-direct call reached target-function lookup",
                self.node_range(callee),
            )),
        }
    }

    fn resolve_method_call(
        &self,
        callee: ExprId,
    ) -> Result<Option<ResolvedMethodCall<'db>>, Diagnostic> {
        let nodes = self.function.node_store();
        let Some(field_id) = nodes.as_field(callee) else {
            return Ok(None);
        };
        let (receiver, field_name_expr) = nodes.field(field_id);
        if receiver == ExprId::ZERO {
            return Ok(None);
        }

        let Some(field_name_id) = nodes.as_name(field_name_expr) else {
            return Ok(None);
        };
        let Some(receiver_ty) =
            self.backend.specialized_expr_ty(&self.owner_instance, self.inference, receiver)
        else {
            return Err(Diagnostic::error(
                "internal error: missing inferred receiver type during method call lowering",
                self.node_range(callee),
            ));
        };

        Ok(resolve_method_for_receiver(self.backend.db, receiver_ty, nodes.name(field_name_id))
            .map(|method| ResolvedMethodCall { receiver, function: method.function }))
    }

    fn function_param_is_mutable(&self, function: FunctionLocation<'db>, index: usize) -> bool {
        let hir_function = function.hir_function(self.backend.db);
        function_param_binding_name(hir_function.function(self.backend.db), index)
            .is_some_and(|name| hir_function.source_map(self.backend.db).is_mutable_binding(name))
    }

    fn should_lower_field_via_stable_address(&self, expr: ExprId) -> Result<bool, Diagnostic> {
        let nodes = self.function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::Name => match self.resolve_name(expr)? {
                BindingId::Local(name) | BindingId::Param(name) => {
                    Ok(self.source_map.is_mutable_binding(name)
                        && self.expr_abi(expr)?.is_aggregate())
                }
                _ => Ok(false),
            },
            NodeKind::Field => {
                let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
                if base == ExprId::ZERO {
                    return Ok(false);
                }
                self.should_lower_field_via_stable_address(base)
            }
            _ => Ok(false),
        }
    }

    fn lower_place_address(&mut self, expr: ExprId) -> Result<KernelExpr<'db>, Diagnostic> {
        let abi = AbiTy::Scalar(match self.backend.memory_model_strategy().guest_word() {
            target::GuestWord::I32 => BackendTy::Int,
            target::GuestWord::I64 => BackendTy::I64,
        });
        let kind = match self.function.node_store().node_kind(expr) {
            NodeKind::Name => match self.resolve_name(expr)? {
                BindingId::Local(name) | BindingId::Param(name) => {
                    let slot = self.layout.slots.get(&name).ok_or_else(|| {
                        Diagnostic::error(
                            "internal error: missing slot for addressable place",
                            self.node_range(expr),
                        )
                    })?;
                    if slot.local_index.is_some() {
                        KernelExprKind::Local(name)
                    } else if let Some(frame_slot) = slot.frame_slot {
                        KernelExprKind::StackAddr { frame_slot }
                    } else {
                        return Err(Diagnostic::error(
                            "internal error: addressable place is missing storage",
                            self.node_range(expr),
                        ));
                    }
                }
                _ => {
                    return Err(Diagnostic::error(
                        "internal error: invalid place reached address lowering",
                        self.node_range(expr),
                    ));
                }
            },
            NodeKind::Field => {
                let (base, field) = self.project_field(expr)?;
                let base = self.lower_place_address(base)?;
                KernelExprKind::AddrOffset { base: Box::new(base), offset: field.offset }
            }
            _ => {
                return Err(Diagnostic::error(
                    "internal error: non-place reached address lowering",
                    self.node_range(expr),
                ));
            }
        };
        Ok(KernelExpr { source: expr, abi, ownership: ValueOwnership::None, kind })
    }

    fn lower_binary_op(&self, op: ExprId) -> Result<BackendBinaryOp, Diagnostic> {
        let nodes = self.function.node_store();
        let symbol = nodes.name(nodes.as_name(op).expect("binary op should be Name"));
        match symbol.text(self.backend.db) {
            "+" => Ok(BackendBinaryOp::Add),
            "-" => Ok(BackendBinaryOp::Sub),
            "*" => Ok(BackendBinaryOp::Mul),
            "/" => Ok(BackendBinaryOp::Div),
            "%" => Ok(BackendBinaryOp::Rem),
            "<" => Ok(BackendBinaryOp::Lt),
            ">" => Ok(BackendBinaryOp::Gt),
            "<=" => Ok(BackendBinaryOp::Le),
            ">=" => Ok(BackendBinaryOp::Ge),
            "==" => Ok(BackendBinaryOp::Eq),
            "!=" => Ok(BackendBinaryOp::Ne),
            "&&" => Ok(BackendBinaryOp::And),
            "||" => Ok(BackendBinaryOp::Or),
            other => Err(Diagnostic::error(
                format!("internal error: unsupported binary operator `{other}`"),
                self.node_range(op),
            )),
        }
    }

    fn lower_prefix_op(&self, op: ExprId) -> Result<BackendPrefixOp, Diagnostic> {
        let nodes = self.function.node_store();
        let symbol = nodes.name(nodes.as_name(op).expect("prefix op should be Name"));
        match symbol.text(self.backend.db) {
            "!" => Ok(BackendPrefixOp::Not),
            "-" => Ok(BackendPrefixOp::Neg),
            other => Err(Diagnostic::error(
                format!("internal error: unsupported prefix operator `{other}`"),
                self.node_range(op),
            )),
        }
    }

    fn resolve_name(&self, expr: ExprId) -> Result<BindingId<'db>, Diagnostic> {
        let nodes = self.function.node_store();
        let Some(name) = nodes.as_name(expr) else {
            return Err(Diagnostic::error(
                "internal error: expected name expression during backend lowering",
                self.node_range(expr),
            ));
        };
        let symbol = nodes.name(name);
        let mut resolver = self.resolver.clone();
        let guard = resolver.scopes_for_node(expr);
        let resolution = resolver.resolve_value_binding(symbol);
        resolver.reset(guard);
        resolution.ok_or_else(|| {
            self.backend.diagnostic_at_function(
                self.location,
                "internal error: unresolved name during backend lowering",
                self.node_range(expr),
            )
        })
    }

    fn resolve_name_ty(&self, expr: ExprId) -> Result<Ty<'db>, Diagnostic> {
        let nodes = self.function.node_store();
        let Some(name) = nodes.as_name(expr) else {
            return Err(Diagnostic::error(
                "internal error: expected name expression during backend lowering",
                self.node_range(expr),
            ));
        };
        let symbol = nodes.name(name);
        let mut resolver = self.resolver.clone();
        let guard = resolver.scopes_for_node(expr);
        let ty = resolver
            .resolve_type_binding(symbol)
            .and_then(|binding| resolver.ty_for_binding(binding));
        resolver.reset(guard);
        ty.ok_or_else(|| {
            Diagnostic::error(
                "internal error: unresolved type name during backend lowering",
                self.node_range(expr),
            )
        })
    }

    fn expr_abi(&self, expr: ExprId) -> Result<AbiTy, Diagnostic> {
        let ty = self.lowerable_expr_ty(expr)?;
        crate::capability::supported_value_abi_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support this value type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(expr)))
    }

    fn expr_ty(&self, expr: ExprId) -> Result<Ty<'db>, Diagnostic> {
        self.backend.specialized_expr_ty(&self.owner_instance, self.inference, expr).ok_or_else(
            || {
                Diagnostic::error(
                    "internal error: missing inferred expression type during backend lowering",
                    self.node_range(expr),
                )
            },
        )
    }

    fn lowerable_expr_ty(&self, expr: ExprId) -> Result<Ty<'db>, Diagnostic> {
        let ty = self.expr_ty(expr)?;
        Ok(self.concrete_expr_member_ty(expr, ty).unwrap_or(ty))
    }

    fn concrete_expr_member_ty(&self, expr: ExprId, ty: Ty<'db>) -> Option<Ty<'db>> {
        let TyKind::Union(members) = ty.kind(self.backend.db) else {
            return None;
        };

        let compatible = members
            .iter()
            .copied()
            .filter(|member| self.expr_matches_ty(expr, *member))
            .collect::<Vec<_>>();
        match compatible.as_slice() {
            [selected] => Some(*selected),
            _ => None,
        }
    }

    fn expr_matches_ty(&self, expr: ExprId, ty: Ty<'db>) -> bool {
        let nodes = self.function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::True | NodeKind::False => matches!(ty.kind(self.backend.db), TyKind::Bool),
            NodeKind::Int => matches!(ty.kind(self.backend.db), TyKind::Int | TyKind::ExactInt(_)),
            NodeKind::Float => matches!(ty.kind(self.backend.db), TyKind::Float),
            NodeKind::String => matches!(ty.kind(self.backend.db), TyKind::String),
            NodeKind::Char => matches!(ty.kind(self.backend.db), TyKind::Char),
            NodeKind::Tuple => {
                let items = nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"));
                match ty.kind(self.backend.db) {
                    TyKind::Tuple(member_items) if member_items.len() == items.len() => items
                        .iter()
                        .zip(member_items.iter())
                        .all(|(item, member_item)| self.expr_matches_ty(item, *member_item)),
                    _ => false,
                }
            }
            NodeKind::StructExpr => {
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(expr).expect("StructExpr mismatch"));
                let struct_name = items.iter().next().unwrap_or(ExprId::ZERO);
                if struct_name == ExprId::ZERO {
                    return matches!(ty.kind(self.backend.db), TyKind::Record(_));
                }

                matches!(self.resolve_name_ty(struct_name), Ok(struct_ty) if struct_ty == ty)
            }
            NodeKind::Field => self.expr_is_enum_variant(expr, ty),
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                self.expr_is_enum_variant(callee, ty)
                    && matches!(ty.kind(self.backend.db), TyKind::Enum(enum_ty)
                        if enum_variants(self.backend.db, *enum_ty)
                            .iter()
                            .find(|(name, payload)| {
                                let (_, variant_name_expr) =
                                    nodes.field(nodes.as_field(callee).expect("variant call"));
                                let variant_name = nodes
                                    .as_name(variant_name_expr)
                                    .expect("variant name should lower to Name");
                                *name == nodes.name(variant_name) && payload.len() == args.len()
                            })
                            .is_some())
            }
            _ => false,
        }
    }

    fn expr_is_enum_variant(&self, expr: ExprId, ty: Ty<'db>) -> bool {
        let nodes = self.function.node_store();
        let TyKind::Enum(enum_ty) = ty.kind(self.backend.db) else {
            return false;
        };
        if nodes.node_kind(expr) != NodeKind::Field {
            return false;
        }

        let (base, field_name_expr) =
            nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
        let variant_name =
            nodes.as_name(field_name_expr).expect("variant name should lower to Name");
        let variant_sym = nodes.name(variant_name);
        if !enum_variants(self.backend.db, *enum_ty).iter().any(|(name, _)| *name == variant_sym) {
            return false;
        }
        if base == ExprId::ZERO {
            return true;
        }

        matches!(self.resolve_name_ty(base), Ok(base_ty) if base_ty == ty)
    }

    fn project_field(&self, expr: ExprId) -> Result<(ExprId, FieldLayout), Diagnostic> {
        let nodes = self.function.node_store();
        let (base, field_name_expr) =
            nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
        if base == ExprId::ZERO {
            return Err(Diagnostic::error(
                "internal error: bare enum variant reached field lowering",
                self.node_range(expr),
            ));
        }

        let name = nodes.name(nodes.as_name(field_name_expr).expect("field name should be Name"));
        let base_abi = self.expr_abi(base)?;
        let layout = if let Some(layout) = base_abi.aggregate() {
            layout.clone()
        } else {
            self.nominal_payload_layout(base)?
        };
        let field = layout.field_named(symbol_bits(name)).ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing field layout metadata during backend lowering",
                self.node_range(expr),
            )
        })?;
        Ok((base, field.clone()))
    }

    fn enum_variant_layout(&self, expr: ExprId) -> Result<VariantLayout, Diagnostic> {
        let nodes = self.function.node_store();
        let (_, field_name_expr) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
        let name = nodes.name(nodes.as_name(field_name_expr).expect("field name should be Name"));
        let abi = self.expr_abi(expr)?;
        let layout = if let Some(layout) = abi.aggregate() {
            layout.clone()
        } else {
            self.nominal_payload_layout(expr)?
        };
        layout.variant(symbol_bits(name)).cloned().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing enum variant layout metadata during backend lowering",
                self.node_range(expr),
            )
        })
    }

    fn enum_variant_layout_for_call(
        &self,
        call_expr: ExprId,
        callee: ExprId,
    ) -> Result<VariantLayout, Diagnostic> {
        let nodes = self.function.node_store();
        let (_, field_name_expr) =
            nodes.field(nodes.as_field(callee).expect("variant callee should be Field"));
        let name = nodes.name(nodes.as_name(field_name_expr).expect("field name should be Name"));
        let abi = self.expr_abi(call_expr)?;
        let layout = if let Some(layout) = abi.aggregate() {
            layout.clone()
        } else {
            self.nominal_payload_layout(call_expr)?
        };
        layout.variant(symbol_bits(name)).cloned().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing enum variant layout metadata during backend lowering",
                self.node_range(call_expr),
            )
        })
    }

    fn is_variant_constructor_call(&self, expr: ExprId) -> Result<bool, Diagnostic> {
        let nodes = self.function.node_store();
        if nodes.node_kind(expr) != NodeKind::Call {
            return Ok(false);
        }
        let (callee, _) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
        if nodes.node_kind(callee) != NodeKind::Field {
            return Ok(false);
        }

        let ty = self.lowerable_expr_ty(expr)?;
        if !matches!(ty.kind(self.backend.db), TyKind::Enum(_)) {
            return Ok(false);
        }

        let (base, _) = nodes.field(nodes.as_field(callee).expect("Field node mismatch"));
        if base == ExprId::ZERO {
            return Ok(true);
        }

        Ok(self.resolve_name_ty(base).is_ok())
    }

    fn nominal_payload_layout(&self, expr: ExprId) -> Result<AggregateLayout, Diagnostic> {
        let ty = self.lowerable_expr_ty(expr)?;
        crate::capability::supported_internal_nominal_payload_layout_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support this nominal value type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(expr)))
    }

    fn is_enum_variant_ref(&self, expr: ExprId) -> Result<bool, Diagnostic> {
        let nodes = self.function.node_store();
        if nodes.node_kind(expr) != NodeKind::Field {
            return Ok(false);
        }

        let ty = self.lowerable_expr_ty(expr)?;
        if !matches!(ty.kind(self.backend.db), TyKind::Enum(_)) {
            return Ok(false);
        }

        let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
        if base == ExprId::ZERO {
            return Ok(true);
        }

        Ok(self.resolve_name_ty(base).is_ok())
    }

    fn node_range(&self, expr: ExprId) -> mitki_errors::TextRange {
        self.source_map
            .try_node_syntax(expr)
            .map_or_else(|| self.backend.function_range(self.location), |ptr| ptr.range)
    }
}

fn resolve_comptime_target<'db>(
    backend: &Backend<'db>,
    location: FunctionLocation<'db>,
    function: &'db Function<'db>,
    expr: ExprId,
    range: mitki_errors::TextRange,
) -> Result<Option<FunctionLocation<'db>>, Diagnostic> {
    let nodes = function.node_store();
    if nodes.node_kind(expr) != NodeKind::Call {
        return Ok(None);
    }

    let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
    let Some(name_id) = nodes.as_name(callee) else {
        return Ok(None);
    };
    let symbol = nodes.name(name_id);
    let mut resolver = Resolver::new(backend.db, location);
    let guard = resolver.scopes_for_node(callee);
    let resolution = resolver.resolve_value_binding(symbol);
    resolver.reset(guard);
    if !matches!(resolution, Some(BindingId::CompilerIntrinsic(CompilerIntrinsic::Comptime))) {
        return Ok(None);
    }

    if args.len() != 1 {
        return Err(Diagnostic::error(
            "internal error: malformed `comptime` call reached backend lowering",
            range,
        ));
    }
    let target_call = args.get(0).expect("single comptime arg");
    let Some(target_call_id) = nodes.as_call(target_call) else {
        return Err(Diagnostic::error(
            "internal error: comptime target is not a direct call",
            range,
        ));
    };
    let (target_expr, _) = nodes.call(target_call_id);
    let Some(target_name_id) = nodes.as_name(target_expr) else {
        return Err(Diagnostic::error(
            "internal error: comptime target is not a named function",
            range,
        ));
    };
    let symbol = nodes.name(target_name_id);
    let mut resolver = Resolver::new(backend.db, location);
    let guard = resolver.scopes_for_node(target_expr);
    let resolution = resolver.resolve_value_binding(symbol);
    resolver.reset(guard);
    let Some(BindingId::Function(target)) = resolution else {
        return Err(Diagnostic::error(
            format!(
                "internal error: comptime target `{}` did not resolve to a function",
                symbol.text(backend.db)
            ),
            range,
        ));
    };

    Ok(Some(target))
}

#[derive(Clone, Debug)]
enum ComptimeValue {
    Unit,
    I32(i32),
    Bool(bool),
    F64(f64),
    Char(char),
    String(String),
    Array(Vec<ComptimeValue>),
    Tuple(Vec<ComptimeValue>),
    Record(Vec<ComptimeValue>),
    Struct(Vec<ComptimeValue>),
    Enum { variant_index: usize, fields: Vec<ComptimeValue> },
}

#[derive(Clone, Debug)]
enum ComptimeStaticValue {
    Unit,
    I32,
    Bool,
    F64,
    Char,
    String(u32),
    Array(Vec<ComptimeStaticValue>),
    Tuple(Vec<ComptimeStaticValue>),
    Record(Vec<ComptimeStaticValue>),
    Struct(Vec<ComptimeStaticValue>),
    Enum { variant_index: usize, fields: Vec<ComptimeStaticValue> },
}

fn comptime_value_from_abi<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
    value: AbiValue,
    range: mitki_errors::TextRange,
) -> Result<ComptimeValue, Diagnostic> {
    match (ty.kind(db), value) {
        (TyKind::Tuple(items), AbiValue::Immediate(AbiScalar::Unit)) if items.is_empty() => {
            Ok(ComptimeValue::Unit)
        }
        (TyKind::Int, AbiValue::Immediate(AbiScalar::Int { value, .. })) => {
            Ok(ComptimeValue::I32(i32::try_from(value).map_err(|_error| {
                Diagnostic::error(
                    "internal error: comptime integer result exceeded i32 range",
                    range,
                )
            })?))
        }
        (TyKind::Bool, AbiValue::Immediate(AbiScalar::Bool(value))) => {
            Ok(ComptimeValue::Bool(value))
        }
        (TyKind::Float, AbiValue::Immediate(AbiScalar::Float { raw_bits, .. })) => {
            Ok(ComptimeValue::F64(f64::from_bits(raw_bits)))
        }
        (TyKind::Char, AbiValue::Immediate(AbiScalar::Char { unicode_scalar })) => {
            Ok(ComptimeValue::Char(char::from_u32(unicode_scalar).ok_or_else(|| {
                Diagnostic::error("internal error: comptime char result was invalid", range)
            })?))
        }
        (TyKind::Enum(enum_ty), AbiValue::Immediate(AbiScalar::EnumTag { variant_index, .. })) => {
            let variants = enum_variants(db, *enum_ty);
            let variant_index = usize::try_from(variant_index).map_err(|_error| {
                Diagnostic::error(
                    "internal error: comptime enum variant index exceeded usize",
                    range,
                )
            })?;
            let (_, fields) = variants.get(variant_index).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: comptime enum variant index was out of range",
                    range,
                )
            })?;
            if !fields.is_empty() {
                return Err(Diagnostic::error(
                    "internal error: payload enum crossed the stage boundary as an immediate tag",
                    range,
                ));
            }
            Ok(ComptimeValue::Enum { variant_index, fields: Vec::new() })
        }
        (TyKind::String, AbiValue::Canonical { graph, .. }) => {
            let CanonicalNode::String { value, .. } =
                comptime_node(&graph, &graph.root, "string", range)?
            else {
                return Err(Diagnostic::error(
                    "internal error: comptime string result was not encoded as a string node",
                    range,
                ));
            };
            Ok(ComptimeValue::String(value.clone()))
        }
        (TyKind::Array(item_ty), AbiValue::Canonical { graph, .. }) => {
            let CanonicalNode::Array { elements, .. } =
                comptime_node(&graph, &graph.root, "array", range)?
            else {
                return Err(Diagnostic::error(
                    "internal error: comptime array result was not encoded as an array node",
                    range,
                ));
            };
            Ok(ComptimeValue::Array(comptime_array_items(db, *item_ty, &graph, elements, range)?))
        }
        (TyKind::Tuple(item_tys), AbiValue::Canonical { graph, .. }) => {
            let CanonicalNode::Tuple { fields, .. } =
                comptime_node(&graph, &graph.root, "tuple", range)?
            else {
                return Err(Diagnostic::error(
                    "internal error: comptime tuple result was not encoded as a tuple node",
                    range,
                ));
            };
            Ok(ComptimeValue::Tuple(comptime_child_values(db, &graph, item_tys, fields, range)?))
        }
        (TyKind::Record(field_tys), AbiValue::Canonical { graph, .. }) => {
            let CanonicalNode::Record { fields, .. } =
                comptime_node(&graph, &graph.root, "record", range)?
            else {
                return Err(Diagnostic::error(
                    "internal error: comptime record result was not encoded as a record node",
                    range,
                ));
            };
            let mut ordered = field_tys.clone();
            ordered.sort_by_key(|(name, _)| name.text(db).to_owned());
            let tys = ordered.iter().map(|(_, ty)| *ty).collect::<Vec<_>>();
            Ok(ComptimeValue::Record(comptime_child_values(db, &graph, &tys, fields, range)?))
        }
        (TyKind::Struct(struct_ty), AbiValue::Canonical { graph, .. }) => {
            let CanonicalNode::Struct { fields, .. } =
                comptime_node(&graph, &graph.root, "struct", range)?
            else {
                return Err(Diagnostic::error(
                    "internal error: comptime struct result was not encoded as a struct node",
                    range,
                ));
            };
            let tys = struct_fields(db, *struct_ty).iter().map(|(_, ty)| *ty).collect::<Vec<_>>();
            Ok(ComptimeValue::Struct(comptime_child_values(db, &graph, &tys, fields, range)?))
        }
        (TyKind::Enum(enum_ty), AbiValue::Canonical { graph, .. }) => {
            let CanonicalNode::Enum { variant_index, fields, .. } =
                comptime_node(&graph, &graph.root, "enum", range)?
            else {
                return Err(Diagnostic::error(
                    "internal error: comptime enum result was not encoded as an enum node",
                    range,
                ));
            };
            let variant_index = usize::try_from(*variant_index).map_err(|_error| {
                Diagnostic::error(
                    "internal error: comptime enum variant index exceeded usize",
                    range,
                )
            })?;
            let (_, field_tys) =
                enum_variants(db, *enum_ty).get(variant_index).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: comptime enum variant index was out of range",
                        range,
                    )
                })?;
            Ok(ComptimeValue::Enum {
                variant_index,
                fields: comptime_child_values(db, &graph, field_tys, fields, range)?,
            })
        }
        (_, AbiValue::Handle { .. }) => Err(Diagnostic::error(
            "internal error: comptime evaluation produced a capability handle",
            range,
        )),
        _ => Err(Diagnostic::error(
            format!("internal error: comptime result did not match `{}`", ty.display(db)),
            range,
        )),
    }
}

fn comptime_node<'a>(
    graph: &'a CanonicalGraph,
    value_ref: &ValueRef,
    kind: &str,
    range: mitki_errors::TextRange,
) -> Result<&'a CanonicalNode, Diagnostic> {
    let ValueRef::NodeRef(node_id) = value_ref else {
        return Err(Diagnostic::error(
            format!("internal error: comptime {kind} result was not encoded as a canonical node"),
            range,
        ));
    };
    graph.nodes.get(node_id.0 as usize).ok_or_else(|| {
        Diagnostic::error(
            format!("internal error: comptime {kind} node id was out of range"),
            range,
        )
    })
}

fn comptime_child_values<'db>(
    db: &'db dyn salsa::Database,
    graph: &CanonicalGraph,
    tys: &[Ty<'db>],
    refs: &[ValueRef],
    range: mitki_errors::TextRange,
) -> Result<Vec<ComptimeValue>, Diagnostic> {
    if tys.len() != refs.len() {
        return Err(Diagnostic::error(
            "internal error: canonical child count did not match the semantic type",
            range,
        ));
    }
    tys.iter()
        .copied()
        .zip(refs.iter())
        .map(|(ty, value_ref)| comptime_value_from_ref(db, ty, graph, value_ref, range))
        .collect()
}

fn comptime_value_from_ref<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
    graph: &CanonicalGraph,
    value_ref: &ValueRef,
    range: mitki_errors::TextRange,
) -> Result<ComptimeValue, Diagnostic> {
    match value_ref {
        ValueRef::InlineScalar(scalar) => {
            comptime_value_from_abi(db, ty, AbiValue::Immediate(scalar.clone()), range)
        }
        ValueRef::NodeRef(node_id) => {
            let node = graph.nodes.get(node_id.0 as usize).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: canonical node reference was out of range",
                    range,
                )
            })?;
            comptime_value_from_abi(
                db,
                ty,
                AbiValue::Canonical {
                    transport_type: node.transport_type(),
                    graph: CanonicalGraph {
                        root: ValueRef::NodeRef(*node_id),
                        nodes: graph.nodes.clone(),
                        handles: graph.handles.clone(),
                    },
                },
                range,
            )
        }
        ValueRef::HandleRef(_) => Err(Diagnostic::error(
            "internal error: comptime evaluation produced a nested capability handle",
            range,
        )),
    }
}

fn comptime_array_items<'db>(
    db: &'db dyn salsa::Database,
    item_ty: Ty<'db>,
    graph: &CanonicalGraph,
    elements: &ArrayElements,
    range: mitki_errors::TextRange,
) -> Result<Vec<ComptimeValue>, Diagnostic> {
    match elements {
        ArrayElements::Values(values) => values
            .iter()
            .map(|value_ref| comptime_value_from_ref(db, item_ty, graph, value_ref, range))
            .collect(),
        ArrayElements::PackedScalars { kind, len, bytes } => {
            comptime_packed_scalar_array(db, item_ty, *kind, *len, bytes, range)
        }
    }
}

fn comptime_packed_scalar_array<'db>(
    db: &'db dyn salsa::Database,
    item_ty: Ty<'db>,
    kind: PackedScalarKind,
    len: u32,
    bytes: &[u8],
    range: mitki_errors::TextRange,
) -> Result<Vec<ComptimeValue>, Diagnostic> {
    let len = usize::try_from(len).map_err(|_error| {
        Diagnostic::error("internal error: packed scalar array length exceeded usize", range)
    })?;
    match (item_ty.kind(db), kind) {
        (TyKind::Bool, PackedScalarKind::Bool) => {
            if bytes.len() != len {
                return Err(Diagnostic::error(
                    "internal error: packed bool array payload length was invalid",
                    range,
                ));
            }
            Ok(bytes.iter().map(|byte| ComptimeValue::Bool(*byte != 0)).collect())
        }
        (TyKind::Int, PackedScalarKind::I32) => {
            if bytes.len() != len * 4 {
                return Err(Diagnostic::error(
                    "internal error: packed int array payload length was invalid",
                    range,
                ));
            }
            Ok(bytes
                .chunks_exact(4)
                .map(|chunk| {
                    ComptimeValue::I32(i32::from_le_bytes(
                        chunk.try_into().expect("i32 chunk should be 4 bytes"),
                    ))
                })
                .collect())
        }
        (TyKind::Float, PackedScalarKind::F64) => {
            if bytes.len() != len * 8 {
                return Err(Diagnostic::error(
                    "internal error: packed float array payload length was invalid",
                    range,
                ));
            }
            Ok(bytes
                .chunks_exact(8)
                .map(|chunk| {
                    ComptimeValue::F64(f64::from_bits(u64::from_le_bytes(
                        chunk.try_into().expect("f64 chunk should be 8 bytes"),
                    )))
                })
                .collect())
        }
        (TyKind::Char, PackedScalarKind::Char) => {
            if bytes.len() != len * 4 {
                return Err(Diagnostic::error(
                    "internal error: packed char array payload length was invalid",
                    range,
                ));
            }
            bytes
                .chunks_exact(4)
                .map(|chunk| {
                    let scalar =
                        u32::from_le_bytes(chunk.try_into().expect("char chunk should be 4 bytes"));
                    char::from_u32(scalar).map(ComptimeValue::Char).ok_or_else(|| {
                        Diagnostic::error(
                            "internal error: packed char array contained an invalid scalar",
                            range,
                        )
                    })
                })
                .collect()
        }
        _ => Err(Diagnostic::error(
            "internal error: packed scalar array did not match the semantic element type",
            range,
        )),
    }
}

#[derive(Default)]
struct StaticDataBuilder<'db> {
    bytes: Vec<u8>,
    pooled_offsets: FxHashMap<Vec<u8>, u32>,
    literal_offsets: FxHashMap<Symbol<'db>, u32>,
    comptime_values: FxHashMap<ComptimeValueKey<'db>, ComptimeStaticValue>,
}

impl<'db> StaticDataBuilder<'db> {
    fn finish(self) -> StaticData<'db> {
        StaticData {
            bytes: self.bytes,
            literal_offsets: self.literal_offsets,
            comptime_values: self.comptime_values,
        }
    }

    fn intern_string_bytes(
        &mut self,
        bytes: Vec<u8>,
        range: mitki_errors::TextRange,
    ) -> Result<u32, Diagnostic> {
        if let Some(offset) = self.pooled_offsets.get(&bytes).copied() {
            return Ok(offset);
        }

        let base = align_to(self.bytes.len() as u32, ARC_ALIGN);
        self.bytes.resize(base as usize, 0);
        let len = u32::try_from(bytes.len()).map_err(|_error| {
            Diagnostic::error("Wasm backend string literal exceeds the supported size", range)
        })?;
        self.bytes.extend_from_slice(&ARC_IMMORTAL_REFCNT.to_le_bytes());
        self.bytes.extend_from_slice(&0u32.to_le_bytes());
        self.bytes.extend_from_slice(&len.to_le_bytes());
        self.bytes.extend_from_slice(&bytes);
        let offset = base + ARC_HEADER_SIZE;
        self.pooled_offsets.insert(bytes, offset);
        Ok(offset)
    }

    fn intern_comptime_static_value(
        &mut self,
        value: ComptimeValue,
        range: mitki_errors::TextRange,
    ) -> Result<ComptimeStaticValue, Diagnostic> {
        match value {
            ComptimeValue::Unit => Ok(ComptimeStaticValue::Unit),
            ComptimeValue::I32(_) => Ok(ComptimeStaticValue::I32),
            ComptimeValue::Bool(_) => Ok(ComptimeStaticValue::Bool),
            ComptimeValue::F64(_) => Ok(ComptimeStaticValue::F64),
            ComptimeValue::Char(_) => Ok(ComptimeStaticValue::Char),
            ComptimeValue::String(value) => {
                let offset = self.intern_string_bytes(value.into_bytes(), range)?;
                Ok(ComptimeStaticValue::String(offset))
            }
            ComptimeValue::Array(items) => Ok(ComptimeStaticValue::Array(
                items
                    .into_iter()
                    .map(|item| self.intern_comptime_static_value(item, range))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            ComptimeValue::Tuple(items) => Ok(ComptimeStaticValue::Tuple(
                items
                    .into_iter()
                    .map(|item| self.intern_comptime_static_value(item, range))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            ComptimeValue::Record(values) => Ok(ComptimeStaticValue::Record(
                values
                    .into_iter()
                    .map(|value| self.intern_comptime_static_value(value, range))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            ComptimeValue::Struct(values) => Ok(ComptimeStaticValue::Struct(
                values
                    .into_iter()
                    .map(|value| self.intern_comptime_static_value(value, range))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            ComptimeValue::Enum { variant_index, fields } => Ok(ComptimeStaticValue::Enum {
                variant_index,
                fields: fields
                    .into_iter()
                    .map(|field| self.intern_comptime_static_value(field, range))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
        }
    }

    fn maybe_store_comptime_value(
        &mut self,
        backend: &Backend<'db>,
        location: FunctionLocation<'db>,
        function: &'db Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        expr: ExprId,
    ) -> Result<bool, Diagnostic> {
        let range = node_range(backend, location, source_map, expr);
        let Some(target) = resolve_comptime_target(backend, location, function, expr, range)?
        else {
            return Ok(false);
        };

        let value = backend
            .comptime_evaluator
            .eval_comptime_function(backend.db, target)
            .map_err(|message| Diagnostic::error(message, range))?;
        let result_ty = comptime::classify_comptime_result(backend.db, target)
            .map_err(|message| Diagnostic::error(message, range))?;
        let value = comptime_value_from_abi(backend.db, result_ty, value, range)?;
        let static_value = self.intern_comptime_static_value(value, range)?;
        self.comptime_values.insert(ComptimeValueKey { location, expr }, static_value);
        Ok(true)
    }

    fn expr(
        &mut self,
        backend: &Backend<'db>,
        location: FunctionLocation<'db>,
        function: &'db Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        expr: ExprId,
    ) -> Result<(), Diagnostic> {
        let nodes = function.node_store();
        if self.maybe_store_comptime_value(backend, location, function, source_map, expr)? {
            return Ok(());
        }
        match nodes.node_kind(expr) {
            NodeKind::String => {
                let literal = nodes.string(nodes.as_string(expr).expect("String node mismatch"));
                let bytes = decode_string_literal(literal, backend.db).map_err(|message| {
                    Diagnostic::error(message, node_range(backend, location, source_map, expr))
                })?;
                let offset = self
                    .intern_string_bytes(bytes, node_range(backend, location, source_map, expr))?;
                if let Some(literal) = literal {
                    self.literal_offsets.insert(literal, offset);
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(expr).expect("Block node mismatch"));
                for stmt in stmts.iter() {
                    self.stmt(backend, location, function, source_map, stmt)?;
                }
                if tail != ExprId::ZERO {
                    self.expr(backend, location, function, source_map, tail)?;
                }
            }
            NodeKind::UnsafeBlock => {
                let (body, _) =
                    nodes.unsafe_block(nodes.as_unsafe_block(expr).expect("UnsafeBlock mismatch"));
                if body != ExprId::ZERO {
                    self.expr(backend, location, function, source_map, body)?;
                }
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                self.expr(backend, location, function, source_map, callee)?;
                for arg in args.iter() {
                    self.expr(backend, location, function, source_map, arg)?;
                }
            }
            NodeKind::Field => {
                let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
                if base != ExprId::ZERO {
                    self.expr(backend, location, function, source_map, base)?;
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(expr).expect("Binary node mismatch"));
                self.expr(backend, location, function, source_map, binary.lhs)?;
                self.expr(backend, location, function, source_map, binary.rhs)?;
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(expr).expect("Prefix node mismatch"));
                self.expr(backend, location, function, source_map, prefix.expr)?;
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(expr).expect("If node mismatch"));
                self.expr(backend, location, function, source_map, if_expr.cond)?;
                if if_expr.then_branch != ExprId::ZERO {
                    self.expr(backend, location, function, source_map, if_expr.then_branch)?;
                }
                if if_expr.else_branch != ExprId::ZERO {
                    self.expr(backend, location, function, source_map, if_expr.else_branch)?;
                }
            }
            NodeKind::Match => {
                let (scrutinee, arms) =
                    nodes.match_expr(nodes.as_match(expr).expect("Match node mismatch"));
                self.expr(backend, location, function, source_map, scrutinee)?;
                for arm in arms.iter() {
                    let (pattern, body) =
                        nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm mismatch"));
                    self.pattern(backend, location, function, source_map, pattern)?;
                    self.expr(backend, location, function, source_map, body)?;
                }
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(expr).expect("LoopExpr node mismatch"));
                if body != ExprId::ZERO {
                    self.expr(backend, location, function, source_map, body)?;
                }
            }
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"));
                for item in tuple.iter() {
                    self.expr(backend, location, function, source_map, item)?;
                }
            }
            NodeKind::Array => {
                let array = nodes.array(nodes.as_array(expr).expect("Array node mismatch"));
                for item in array.iter() {
                    self.expr(backend, location, function, source_map, item)?;
                }
            }
            NodeKind::ArrayRepeat => {
                let (value, len) =
                    nodes.array_repeat(nodes.as_array_repeat(expr).expect("ArrayRepeat mismatch"));
                self.expr(backend, location, function, source_map, value)?;
                self.expr(backend, location, function, source_map, len)?;
            }
            NodeKind::StructExpr => {
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(expr).expect("StructExpr mismatch"));
                let has_struct_name = items.len() % 2 == 1;
                let mut index = if has_struct_name { 2 } else { 1 };
                while index < items.len() {
                    self.expr(backend, location, function, source_map, items.get(index).unwrap())?;
                    index += 2;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn stmt(
        &mut self,
        backend: &Backend<'db>,
        location: FunctionLocation<'db>,
        function: &'db Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        stmt: StmtId,
    ) -> Result<(), Diagnostic> {
        let nodes = function.node_store();
        if nodes.node_kind(stmt) == NodeKind::LocalVar {
            let var = nodes.local_var(nodes.as_local_var(stmt).expect("LocalVar node mismatch"));
            self.pattern(backend, location, function, source_map, var.pattern)?;
            if var.initializer != ExprId::ZERO {
                self.expr(backend, location, function, source_map, var.initializer)?;
            }
        } else if nodes.node_kind(stmt) == NodeKind::AssignStmt {
            let (target, value) =
                nodes.assign_stmt(nodes.as_assign_stmt(stmt).expect("AssignStmt mismatch"));
            self.expr(backend, location, function, source_map, target)?;
            self.expr(backend, location, function, source_map, value)?;
        } else if nodes.node_kind(stmt) == NodeKind::ReturnStmt {
            let (value, _) =
                nodes.return_stmt(nodes.as_return_stmt(stmt).expect("ReturnStmt mismatch"));
            if value != ExprId::ZERO {
                self.expr(backend, location, function, source_map, value)?;
            }
        } else if let Some(expr) = stmt_as_expr(nodes, stmt) {
            self.expr(backend, location, function, source_map, expr)?;
        }
        Ok(())
    }

    fn pattern(
        &mut self,
        backend: &Backend<'db>,
        location: FunctionLocation<'db>,
        function: &'db Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        pattern: PatId,
    ) -> Result<(), Diagnostic> {
        if pattern == PatId::ZERO {
            return Ok(());
        }
        let nodes = function.node_store();
        match nodes.node_kind(pattern) {
            NodeKind::PatString => {
                let literal =
                    nodes.pat_string(nodes.as_pat_string(pattern).expect("PatString mismatch"));
                let bytes = decode_string_literal(literal, backend.db).map_err(|message| {
                    let range = source_map
                        .try_pat_syntax(pattern)
                        .map_or_else(|| backend.function_range(location), |ptr| ptr.range);
                    Diagnostic::error(message, range)
                })?;
                let range = source_map
                    .try_pat_syntax(pattern)
                    .map_or_else(|| backend.function_range(location), |ptr| ptr.range);
                let offset = self.intern_string_bytes(bytes, range)?;
                if let Some(literal) = literal {
                    self.literal_offsets.insert(literal, offset);
                }
            }
            NodeKind::PatTyped => {
                let (inner, _) = nodes.pat_typed(nodes.as_pat_typed(pattern).expect("PatTyped"));
                self.pattern(backend, location, function, source_map, inner)?;
            }
            NodeKind::PatParen => {
                let (inner, _) = nodes.pat_paren(nodes.as_pat_paren(pattern).expect("PatParen"));
                self.pattern(backend, location, function, source_map, inner)?;
            }
            NodeKind::PatTuple => {
                for item in nodes.pat_tuple(nodes.as_pat_tuple(pattern).expect("PatTuple")).iter() {
                    self.pattern(backend, location, function, source_map, item)?;
                }
            }
            NodeKind::PatVariant => {
                let (_, args) =
                    nodes.pat_variant(nodes.as_pat_variant(pattern).expect("PatVariant"));
                for arg in args.iter() {
                    self.pattern(backend, location, function, source_map, arg)?;
                }
            }
            NodeKind::PatStruct => {
                let (_, fields) =
                    nodes.pat_struct(nodes.as_pat_struct(pattern).expect("PatStruct"));
                for field in fields.iter() {
                    let (_, pat) =
                        nodes.pat_struct_field(nodes.as_pat_struct_field(field).expect("field"));
                    self.pattern(backend, location, function, source_map, pat)?;
                }
            }
            _ => {}
        }
        Ok(())
    }
}

pub(in crate::backend) struct StaticData<'db> {
    bytes: Vec<u8>,
    literal_offsets: FxHashMap<Symbol<'db>, u32>,
    comptime_values: FxHashMap<ComptimeValueKey<'db>, ComptimeStaticValue>,
}

impl<'db> StaticData<'db> {
    fn offset(&self, literal: Option<Symbol<'db>>) -> Option<u32> {
        literal.and_then(|literal| self.literal_offsets.get(&literal).copied())
    }

    fn comptime_value(
        &self,
        location: FunctionLocation<'db>,
        expr: ExprId,
    ) -> Option<&ComptimeStaticValue> {
        self.comptime_values.get(&ComptimeValueKey { location, expr })
    }
}

fn function_param_binding_name<'db>(function: &Function<'db>, index: usize) -> Option<NameId> {
    let &param = function.params().get(index)?;
    let (pattern, _) = function.node_store().param(param);
    let binding = function.node_store().as_pat_binding(pattern)?;
    let (name, _) = function.node_store().pat_binding(binding);
    Some(name)
}

fn helper_function_signature(helper: HelperFunction) -> FunctionSignature {
    match helper {
        HelperFunction::MemoryEq => FunctionSignature {
            params: vec![
                AbiTy::Scalar(BackendTy::Int),
                AbiTy::Scalar(BackendTy::Int),
                AbiTy::Scalar(BackendTy::Int),
            ],
            result: AbiTy::Scalar(BackendTy::Bool),
        },
        HelperFunction::StringEq => FunctionSignature {
            params: vec![
                AbiTy::Scalar(BackendTy::Ref(RefKind::String)),
                AbiTy::Scalar(BackendTy::Ref(RefKind::String)),
            ],
            result: AbiTy::Scalar(BackendTy::Bool),
        },
        HelperFunction::ArcRetain => FunctionSignature {
            params: vec![AbiTy::Scalar(BackendTy::Ref(RefKind::Opaque))],
            result: AbiTy::Scalar(BackendTy::Unit),
        },
        HelperFunction::ArcRelease => FunctionSignature {
            params: vec![AbiTy::Scalar(BackendTy::Ref(RefKind::Opaque))],
            result: AbiTy::Scalar(BackendTy::Bool),
        },
    }
}

fn emit_helper_function(
    helper: HelperFunction,
    helper_indices: &FxHashMap<HelperFunction, u32>,
) -> WasmFunction {
    let mut function = match helper {
        HelperFunction::MemoryEq | HelperFunction::StringEq => {
            WasmFunction::new(vec![(1, ValType::I32)])
        }
        HelperFunction::ArcRetain | HelperFunction::ArcRelease => {
            WasmFunction::new(vec![(2, ValType::I32)])
        }
    };

    match helper {
        HelperFunction::MemoryEq => emit_memory_eq_helper(&mut function),
        HelperFunction::StringEq => {
            let memory_eq = helper_indices
                .get(&HelperFunction::MemoryEq)
                .copied()
                .expect("string_eq helper depends on memory_eq");
            emit_string_eq_helper(&mut function, memory_eq);
        }
        HelperFunction::ArcRetain => emit_arc_retain_helper(&mut function),
        HelperFunction::ArcRelease => emit_arc_release_helper(&mut function),
    }

    function.instruction(&Instruction::End);
    function
}

fn emit_memory_eq_helper(function: &mut WasmFunction) {
    const LHS_PTR: u32 = 0;
    const RHS_PTR: u32 = 1;
    const LEN: u32 = 2;
    const INDEX: u32 = 3;

    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(INDEX));

    function.instruction(&Instruction::Block(BlockType::Empty));
    function.instruction(&Instruction::Loop(BlockType::Empty));

    function.instruction(&Instruction::LocalGet(INDEX));
    function.instruction(&Instruction::LocalGet(LEN));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(LHS_PTR));
    function.instruction(&Instruction::LocalGet(INDEX));
    function.instruction(&Instruction::I32Add);
    MemAccess::byte(0).emit_load(function);

    function.instruction(&Instruction::LocalGet(RHS_PTR));
    function.instruction(&Instruction::LocalGet(INDEX));
    function.instruction(&Instruction::I32Add);
    MemAccess::byte(0).emit_load(function);

    function.instruction(&Instruction::I32Ne);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(INDEX));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(INDEX));
    function.instruction(&Instruction::Br(0));

    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::I32Const(1));
}

fn emit_string_eq_helper(function: &mut WasmFunction, memory_eq_index: u32) {
    const LHS_PTR: u32 = 0;
    const RHS_PTR: u32 = 1;
    const LEN: u32 = 2;

    function.instruction(&Instruction::LocalGet(LHS_PTR));
    function.instruction(&Instruction::LocalGet(RHS_PTR));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(LHS_PTR));
    MemAccess::i32(0, 2).emit_load(function);
    function.instruction(&Instruction::LocalTee(LEN));

    function.instruction(&Instruction::LocalGet(RHS_PTR));
    MemAccess::i32(0, 2).emit_load(function);
    function.instruction(&Instruction::I32Ne);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(LHS_PTR));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(RHS_PTR));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(LEN));
    function.instruction(&Instruction::Call(memory_eq_index));
}

fn emit_arc_retain_helper(function: &mut WasmFunction) {
    const PTR: u32 = 0;
    const BASE: u32 = 1;
    const COUNT: u32 = 2;

    function.instruction(&Instruction::LocalGet(PTR));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(PTR));
    function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalTee(BASE));
    MemAccess::arc_ref_count().emit_load(function);
    function.instruction(&Instruction::LocalTee(COUNT));
    function.instruction(&Instruction::I32Const(ARC_IMMORTAL_REFCNT));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(BASE));
    function.instruction(&Instruction::LocalGet(COUNT));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    MemAccess::arc_ref_count().emit_store(function);
}

fn emit_arc_release_helper(function: &mut WasmFunction) {
    const PTR: u32 = 0;
    const BASE: u32 = 1;
    const COUNT: u32 = 2;

    function.instruction(&Instruction::LocalGet(PTR));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(PTR));
    function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalTee(BASE));
    MemAccess::arc_ref_count().emit_load(function);
    function.instruction(&Instruction::LocalTee(COUNT));
    function.instruction(&Instruction::I32Const(ARC_IMMORTAL_REFCNT));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(BlockType::Empty));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(COUNT));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalSet(COUNT));
    function.instruction(&Instruction::LocalGet(BASE));
    function.instruction(&Instruction::LocalGet(COUNT));
    MemAccess::arc_ref_count().emit_store(function);
    function.instruction(&Instruction::LocalGet(COUNT));
    function.instruction(&Instruction::I32Eqz);
}

fn closure_destroy_layout(word_type: ValType) -> FunctionLayout {
    let mut local_plan = LocalPlanBuilder::new(0);
    local_plan.add_param(LocalPurpose::EnvPtrParam, None, Some(word_type));
    local_plan.alloc_scratch(ScratchLocalKind::ScratchI32, word_type);
    local_plan.alloc_scratch(ScratchLocalKind::ScratchI32Aux, word_type);
    local_plan.alloc_scratch(ScratchLocalKind::ObjectI32, word_type);
    local_plan.alloc_scratch(ScratchLocalKind::ScratchF64, ValType::F64);
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

fn scratch_only_layout(param_count: u32, word_type: ValType) -> FunctionLayout {
    let mut local_plan = LocalPlanBuilder::new(param_count);
    local_plan.alloc_scratch(ScratchLocalKind::ScratchI32, word_type);
    local_plan.alloc_scratch(ScratchLocalKind::ScratchI32Aux, word_type);
    local_plan.alloc_scratch(ScratchLocalKind::ObjectI32, word_type);
    local_plan.alloc_scratch(ScratchLocalKind::ScratchF64, ValType::F64);
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

fn memory_min_pages(static_data_len: usize) -> u64 {
    let bytes = static_data_len.max(1);
    let bytes = u64::try_from(bytes).expect("static data length should fit into u64");
    bytes.div_ceil(65_536)
}

fn abi_mem_access(abi: &AbiTy, offset: u32) -> Option<MemAccess> {
    match abi {
        AbiTy::Scalar(ty) => MemAccess::scalar(offset, *ty),
        AbiTy::Aggregate(_) => None,
    }
}

fn pointee_mem_access(
    db: &dyn salsa::Database,
    pointee: Ty<'_>,
    fallback_abi: &AbiTy,
    offset: u32,
) -> Option<MemAccess> {
    match pointee.kind(db) {
        TyKind::ExactInt(mitki_hir::ty::ExactInt::U8) => Some(MemAccess::byte(offset)),
        _ => abi_mem_access(fallback_abi, offset),
    }
}

fn pointee_stride(db: &dyn salsa::Database, pointee: Ty<'_>) -> Option<u32> {
    if let TyKind::ExactInt(int_ty) = pointee.kind(db) {
        Some(u32::from(int_ty.bits().div_ceil(8)).max(1))
    } else {
        let abi = crate::capability::supported_value_abi(db, pointee)?;
        crate::layout::abi_layout(&abi).map(|layout| layout.size.max(1))
    }
}

pub(super) fn emit_scalar_load(function: &mut WasmFunction, ty: BackendTy, offset: u32) {
    if let Some(access) = MemAccess::scalar(offset, ty) {
        access.emit_load(function);
    }
}

pub(super) fn emit_scalar_store(function: &mut WasmFunction, ty: BackendTy, offset: u32) {
    if let Some(access) = MemAccess::scalar(offset, ty) {
        access.emit_store(function);
    }
}

fn emit_raw_import_forwarder(
    range: mitki_errors::TextRange,
    import_index: u32,
    signature: &FunctionSignature,
) -> Result<WasmFunction, Diagnostic> {
    if signature.result.is_aggregate() {
        return Err(Diagnostic::error("raw Wasm imports cannot return aggregate values", range));
    }
    if signature.params.iter().any(AbiTy::is_aggregate) {
        return Err(Diagnostic::error("raw Wasm imports cannot take aggregate parameters", range));
    }

    let mut function = WasmFunction::new(Vec::new());
    let mut next_param = 0u32;
    for param in &signature.params {
        let AbiTy::Scalar(backend_ty) = param else {
            unreachable!();
        };
        if backend_ty_value_type(*backend_ty).is_some() {
            function.instruction(&Instruction::LocalGet(next_param));
            next_param += 1;
        }
    }
    function.instruction(&Instruction::Call(import_index));
    function.instruction(&Instruction::End);
    Ok(function)
}

#[cfg(test)]
mod tests {
    use mitki_db::RootDatabase;
    use mitki_inputs::File;

    use super::*;

    fn compiler_for_fixture(fixture: &str) -> Backend<'_> {
        let db = Box::leak(Box::new(RootDatabase::default()));
        let file = File::new(db, "storage_plan_fixture.mitki".into(), fixture.to_owned());
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

    fn function_layout_for_fixture(fixture: &str, expected_name: &str) -> FunctionLayout {
        let backend = compiler_for_fixture(fixture);
        let instance = backend
            .build_reachability_graph()
            .functions
            .iter()
            .find_map(|instance| {
                instance
                    .location
                    .source(backend.db)
                    .name()
                    .is_some_and(|name| name.as_str() == expected_name)
                    .then_some(instance.clone())
            })
            .expect("expected reachable function");
        let hir_function = instance.location.hir_function(backend.db);
        let function = hir_function.function(backend.db);
        let inference = instance.location.infer(backend.db);
        let signature = backend
            .function_signature(&instance, function, inference)
            .unwrap_or_else(|diagnostic| panic!("function signature: {}", diagnostic.message()));
        backend
            .build_layout(&ReachableInstance::Function(instance), function, inference, &signature)
            .unwrap_or_else(|diagnostic| panic!("storage layout: {}", diagnostic.message()))
    }

    fn storage_fixture() -> &'static str {
        r#"
extern struct ByteSlice {
    ptr: *const u8,
    len: u32,
}

unsafe fun local_layout(flag: bool): u32 {
    val count: u32 = 1
    val xs: [int] = [1, 2, 3]
    val bytes: ByteSlice = str_bytes("ok")
    val out: *mut u32 = stack_alloc(1)
    ptr_write(out, if flag { bytes.len } else { count })
    ptr_read(out)
}

export fun main(): int {
    unsafe {
        if local_layout(true) == 2 {
            1
        } else {
            0
        }
    }
}
"#
    }

    #[test]
    fn storage_layout_exposes_explicit_local_categories() {
        let layout = function_layout_for_fixture(storage_fixture(), "local_layout");

        assert_eq!(layout.local_plan.params.len(), 1, "expected one runtime parameter");
        assert!(
            layout.local_plan.user_locals.len() >= 2,
            "expected explicit user locals for scalar/ref bindings"
        );
        assert!(
            !layout.local_plan.spills.is_empty(),
            "expected explicit spill locals for temporary object storage"
        );
        assert_eq!(
            layout.local_plan.joins.len(),
            0,
            "current function emitter should keep joins explicit but unused in Step 6"
        );
        assert!(
            layout.local_plan.scratch.len() >= 4,
            "expected scratch locals to be planned explicitly"
        );
    }

    #[test]
    fn storage_layout_assigns_frame_slots_for_bindings_temps_and_stack_alloc() {
        let layout = function_layout_for_fixture(storage_fixture(), "local_layout");

        assert!(
            layout
                .frame_plan
                .slots
                .iter()
                .any(|slot| matches!(slot.purpose, FrameSlotPurpose::Binding(_))),
            "expected aggregate bindings to live in explicit frame slots"
        );
        assert!(
            layout
                .frame_plan
                .slots
                .iter()
                .any(|slot| matches!(slot.purpose, FrameSlotPurpose::Temp(_))),
            "expected aggregate expression temps to live in explicit frame slots"
        );
        assert!(
            layout
                .frame_plan
                .slots
                .iter()
                .any(|slot| matches!(slot.purpose, FrameSlotPurpose::StackAlloc(_))),
            "expected stack_alloc to reserve an explicit frame slot"
        );
    }

    #[test]
    fn storage_layout_dump_is_deterministic_across_repeated_planning() {
        let first = function_layout_for_fixture(storage_fixture(), "local_layout").dump_storage();
        let second = function_layout_for_fixture(storage_fixture(), "local_layout").dump_storage();
        assert_eq!(first, second);
    }
}
