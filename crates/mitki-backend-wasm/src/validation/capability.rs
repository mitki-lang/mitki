#[cfg(test)]
use std::sync::Arc;

use mitki_abi::TransportClass;
use mitki_abi_lower::BuiltAbiV2;
use mitki_errors::Diagnostic;
use rustc_hash::FxHashMap;

use super::super::Backend;
use super::super::boundary::BoundaryPlanner;
use super::super::plan::{FunctionInstanceId, ReachabilityEdgeKind, ReachabilityGraph};

pub struct CapabilityValidator;

impl CapabilityValidator {
    pub fn check<'db>(backend: &Backend<'db>) -> Result<(), Diagnostic> {
        backend.target_policies().validate_backend_support(backend.file_range())?;
        let graph = backend.shadow_reachability.as_ref().ok_or_else(|| {
            Diagnostic::error(
                "internal error: capability validation requires collected reachability state",
                backend.file_range(),
            )
        })?;
        if backend.capability_matrix().guest_word() != backend.memory_model_strategy().guest_word()
        {
            return Err(Diagnostic::error(
                "internal error: target capability matrix drifted from the memory model strategy",
                backend.file_range(),
            ));
        }
        if backend.capability_matrix().callable_representation()
            != backend.callable_lowering_strategy().representation()
        {
            return Err(Diagnostic::error(
                "internal error: target capability matrix drifted from the callable strategy",
                backend.file_range(),
            ));
        }
        if backend.capability_matrix().reference_representation()
            != backend.reference_representation_strategy().representation()
        {
            return Err(Diagnostic::error(
                "internal error: target capability matrix drifted from the reference \
                 representation strategy",
                backend.file_range(),
            ));
        }
        if backend.capability_matrix().failure_lowering()
            != backend.control_flow_strategy().failure_lowering()
        {
            return Err(Diagnostic::error(
                "internal error: target capability matrix drifted from the control-flow strategy",
                backend.file_range(),
            ));
        }

        let function_ids = graph
            .functions
            .iter()
            .enumerate()
            .map(|(index, instance)| {
                (
                    instance.clone(),
                    FunctionInstanceId(
                        u32::try_from(index).expect("function count should fit u32"),
                    ),
                )
            })
            .collect::<FxHashMap<_, _>>();
        let boundary = BoundaryPlanner::build(backend, graph, &function_ids)?;
        let preview = boundary
            .metadata
            .build_preview(backend.db)
            .map_err(|message| Diagnostic::error(message, backend.file_range()))?;

        if !backend.capability_matrix().uses_reference_types()
            && backend.callable_lowering_strategy().requires_reference_types()
            && (Self::requires_table_backed_callables(graph)
                || Self::boundary_uses_capability_handles(&preview))
        {
            return Err(Diagnostic::error(
                format!(
                    "the target profile `{}` does not enable reference types, but the current \
                     callable lowering strategy requires them for function values and boundary \
                     capability handles",
                    backend.target_profile().canonical_name()
                ),
                backend.file_range(),
            ));
        }

        for entry in &boundary.instances {
            let built = &preview.functions[entry.metadata_index];
            let signature = &preview.graph.signatures[built.signature_id.0 as usize];
            backend.boundary_transport_profile().ensure_supported(
                &preview.graph,
                signature,
                backend.function_range(entry.instance.location),
            )?;
        }

        Ok(())
    }

    fn requires_table_backed_callables<'db>(graph: &ReachabilityGraph<'db>) -> bool {
        !graph.closures.is_empty()
            || graph.edges.iter().any(|edge| edge.kind == ReachabilityEdgeKind::FunctionValue)
    }

    fn boundary_uses_capability_handles(preview: &BuiltAbiV2) -> bool {
        preview.functions.iter().any(|function| {
            let signature = &preview.graph.signatures[function.signature_id.0 as usize];
            signature
                .params
                .iter()
                .chain(std::iter::once(&signature.result))
                .any(|transport| transport.transport_class == TransportClass::CapabilityHandle)
        })
    }
}

#[cfg(test)]
mod tests {
    use mitki_comptime_wasm::compile_file_to_wasm;
    use mitki_db::RootDatabase;
    use mitki_inputs::File;

    use super::*;
    use crate::CompileOptions;
    use crate::backend::BoundaryLegalityValidator;
    use crate::backend::target::PointerWidth;

    fn build_compiler<'db>(
        db: &'db RootDatabase,
        fixture: &str,
        profile: crate::backend::TargetProfile,
    ) -> Backend<'db> {
        let file = File::new(db, "capability_target_profile.mitki".into(), fixture.to_owned());
        assert!(mitki_analysis::check_file(db, file).is_empty(), "fixture should parse cleanly");
        assert!(
            mitki_analysis::check_runtime_file(db, file).is_empty(),
            "fixture should pass runtime checks"
        );
        let mut backend = Backend::new_file_with_profile(
            db,
            file,
            profile,
            CompileOptions,
            Arc::new(crate::NoopComptimeEvaluator),
        );
        backend.collect_reachable_program();
        let legality = BoundaryLegalityValidator::check(&backend);
        assert!(legality.is_empty(), "fixture should be boundary-legal: {legality:?}");
        backend
    }

    #[test]
    fn explicit_default_profile_matches_public_default_compilation() {
        let fixture = r#"
export fun main(): int {
    42
}
"#;
        let public_db = RootDatabase::default();
        let public_file = File::new(
            &public_db,
            "capability_target_profile_public.mitki".into(),
            fixture.to_owned(),
        );
        let public_bytes =
            compile_file_to_wasm(&public_db, public_file).expect("public compile should work");

        let explicit_db = RootDatabase::default();
        let backend = build_compiler(
            &explicit_db,
            fixture,
            crate::backend::TargetProfile::wasm_core_v2_m32(),
        );
        CapabilityValidator::check(&backend).expect("default target profile should validate");
        let explicit_bytes = backend.emit_module().expect("explicit profile compile should work");

        assert_eq!(public_bytes, explicit_bytes);
    }

    #[test]
    fn unsupported_future_profile_reports_capability_limit() {
        let fixture = r#"
export fun main(): int {
    42
}
"#;
        let db = RootDatabase::default();
        let profile = crate::backend::TargetProfile::wasm_core_v2_m32()
            .with_pointer_width(PointerWidth::M64)
            .with_memory64(true);
        let backend = build_compiler(&db, fixture, profile);
        let diagnostic =
            CapabilityValidator::check(&backend).expect_err("memory64 profile should be rejected");
        assert!(diagnostic.message().contains("target profile `wasm-core-v2/m64`"));
        assert!(diagnostic.message().contains("memory64"));
    }

    #[test]
    fn multi_value_profile_is_rejected_before_lowering() {
        let db = RootDatabase::default();
        let backend = build_compiler(
            &db,
            "export fun main(): int { 42 }",
            crate::backend::TargetProfile::wasm_core_v2_m32().with_multi_value(true),
        );
        let diagnostic =
            CapabilityValidator::check(&backend).expect_err("multi-value should be rejected");
        assert!(diagnostic.message().contains("result lowering mode `multi_value`"));
    }

    #[test]
    fn typed_funcref_profile_is_rejected_before_callable_planning() {
        let db = RootDatabase::default();
        let backend = build_compiler(
            &db,
            "export fun main(): int { 42 }",
            crate::backend::TargetProfile::wasm_core_v2_m32().with_typed_funcref(true),
        );
        let diagnostic =
            CapabilityValidator::check(&backend).expect_err("typed funcref should be rejected");
        assert!(diagnostic.message().contains("callable representation `typed_funcref`"));
    }

    #[test]
    fn wasm_gc_profile_is_rejected_before_layout_selection() {
        let db = RootDatabase::default();
        let backend = build_compiler(
            &db,
            "export fun main(): int { 42 }",
            crate::backend::TargetProfile::wasm_core_v2_m32().with_wasm_gc(true),
        );
        let diagnostic =
            CapabilityValidator::check(&backend).expect_err("wasm gc should be rejected");
        assert!(diagnostic.message().contains("reference representation `gc_ref`"));
    }

    #[test]
    fn exceptions_profile_is_rejected_before_control_flow_lowering() {
        let db = RootDatabase::default();
        let backend = build_compiler(
            &db,
            "export fun main(): int { 42 }",
            crate::backend::TargetProfile::wasm_core_v2_m32().with_exceptions(true),
        );
        let diagnostic =
            CapabilityValidator::check(&backend).expect_err("exceptions should be rejected");
        assert!(diagnostic.message().contains("failure lowering `exceptions`"));
    }

    #[test]
    fn reference_types_must_remain_enabled_for_handle_based_callables() {
        let db = RootDatabase::default();
        let backend = build_compiler(
            &db,
            r#"
import "env" fun round_trip(f: fun(int) -> int): fun(int) -> int;

export fun main(f: fun(int) -> int): fun(int) -> int {
    round_trip(f)
}
"#,
            crate::backend::TargetProfile::wasm_core_v2_m32().with_reference_types(false),
        );
        let diagnostic = CapabilityValidator::check(&backend)
            .expect_err("reference types should be required for table-backed callables");
        assert!(diagnostic.message().contains("does not enable reference types"));
    }
}
