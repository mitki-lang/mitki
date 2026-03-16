use mitki_errors::Diagnostic;
use mitki_hir::hir::WasmLinkage;
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::BoundaryInstanceKind;
use mitki_typeck::infer::Inferable as _;
use mitki_yellow::ast::HasName as _;

use super::super::{Backend, InstanceKey};

pub struct BoundaryLegalityValidator;

impl BoundaryLegalityValidator {
    pub fn check<'db>(backend: &Backend<'db>) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        let Some(graph) = backend.shadow_reachability.as_ref() else {
            return vec![Diagnostic::error(
                "internal error: boundary legality validation requires collected reachability \
                 state",
                backend.file_range(),
            )];
        };

        for instance in &graph.imports {
            let function = instance.location.hir_function(backend.db).function(backend.db);
            if !matches!(function.linkage(), WasmLinkage::Import { .. }) {
                continue;
            }
            if !instance.type_args.is_empty()
                && !backend.has_declared_boundary_instance(BoundaryInstanceKind::Import, instance)
            {
                diagnostics.push(Diagnostic::error(
                    format!(
                        "`import instance` declaration is required for `{}` with the reachable \
                         concrete type arguments",
                        instance
                            .location
                            .source(backend.db)
                            .name()
                            .map_or("", |name| name.as_str())
                    ),
                    backend.function_range(instance.location),
                ));
                continue;
            }
            diagnostics.extend(Self::check_signature_legality(backend, instance));
        }

        for instance in &graph.exports {
            diagnostics.extend(Self::check_signature_legality(backend, instance));
        }

        diagnostics
    }

    fn check_signature_legality<'db>(
        backend: &Backend<'db>,
        instance: &InstanceKey<'db>,
    ) -> Vec<Diagnostic> {
        let hir_function = instance.location.hir_function(backend.db);
        let function = hir_function.function(backend.db);
        let inference = instance.location.infer(backend.db);
        let source_map = hir_function.source_map(backend.db);
        let nodes = function.node_store();
        let mut diagnostics = Vec::new();
        let (param_tys, result_ty) =
            match backend.function_signature_types(instance, function, inference) {
                Ok(types) => types,
                Err(diagnostic) => return vec![diagnostic],
            };

        for (&param, ty) in function.params().iter().zip(param_tys) {
            let (pattern, ty_id) = nodes.param(param);
            let range = if ty_id != mitki_hir::hir::TyId::ZERO {
                source_map
                    .try_type_syntax(ty_id)
                    .map_or_else(|| backend.function_range(instance.location), |ptr| ptr.range)
            } else {
                source_map
                    .try_pat_syntax(pattern)
                    .map_or_else(|| backend.function_range(instance.location), |ptr| ptr.range)
            };
            if let Some(failure) =
                mitki_analysis::wasm_boundary_legality::typed_wasm_boundary_failure(backend.db, ty)
            {
                diagnostics.push(Diagnostic::error(
                    mitki_analysis::wasm_boundary_legality::typed_wasm_boundary_message(
                        backend.db, failure,
                    ),
                    range,
                ));
            }
        }

        if let Some(failure) = mitki_analysis::wasm_boundary_legality::typed_wasm_boundary_failure(
            backend.db, result_ty,
        ) {
            diagnostics.push(Diagnostic::error(
                mitki_analysis::wasm_boundary_legality::typed_wasm_boundary_message(
                    backend.db, failure,
                ),
                backend.return_range(instance.location, function, source_map),
            ));
        }

        diagnostics
    }
}
