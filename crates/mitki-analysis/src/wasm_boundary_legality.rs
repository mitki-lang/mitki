use mitki_errors::Diagnostic;
use mitki_hir::hir::{Function, WasmLinkage};
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{
    BoundaryInstanceKind, BoundaryInstanceLocation, FunctionLocation, Signature, enum_variants,
    struct_fields,
};
use mitki_resolve::SignatureTypeResolver;
use mitki_typeck::infer::Inferable as _;
use mitki_yellow::SyntaxNodePtr;
use mitki_yellow::ast::{HasName as _, Node as _};
use rustc_hash::FxHashSet;

use crate::ownership;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BoundaryLegalityFailure<'db> {
    ExactInt(Ty<'db>),
    Pointer(Ty<'db>),
    ExternStruct(Ty<'db>),
    NonCopy(Ty<'db>),
}

pub fn typed_wasm_boundary_failure<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
) -> Option<BoundaryLegalityFailure<'db>> {
    let mut seen = FxHashSet::default();
    typed_wasm_boundary_failure_inner(db, ty, &mut seen)
}

pub fn typed_wasm_boundary_message(
    db: &dyn salsa::Database,
    failure: BoundaryLegalityFailure<'_>,
) -> String {
    match failure {
        BoundaryLegalityFailure::ExactInt(ty) => format!(
            "typed Wasm imports/exports do not allow exact-width integer types like `{}`; raw \
             unsafe Wasm imports remain available for exact-width integers",
            ty.display(db)
        ),
        BoundaryLegalityFailure::Pointer(ty) => format!(
            "typed Wasm imports/exports do not allow pointer types like `{}`; raw unsafe Wasm \
             imports remain available for pointers",
            ty.display(db)
        ),
        BoundaryLegalityFailure::ExternStruct(ty) => format!(
            "typed Wasm imports/exports do not allow extern structs like `{}`; raw unsafe Wasm \
             imports remain available for extern structs",
            ty.display(db)
        ),
        BoundaryLegalityFailure::NonCopy(ty) => format!(
            "typed Wasm imports/exports do not allow non-copy types like `{}` because destructor \
             ownership is not supported across the Wasm boundary yet",
            ty.display(db)
        ),
    }
}

pub(crate) fn check_function_boundary_legality(
    db: &dyn salsa::Database,
    func: FunctionLocation<'_>,
) -> Vec<Diagnostic> {
    let source = func.source(db);
    let source_map = func.hir_function(db).source_map(db);
    let function = func.hir_function(db).function(db);
    let nodes = function.node_store();
    let fallback_range = SyntaxNodePtr::new(source.syntax()).range;
    let mut diagnostics = Vec::new();

    match function.linkage() {
        WasmLinkage::Import { .. } | WasmLinkage::RawImport { .. } => {
            if source.body().is_some() {
                diagnostics.push(Diagnostic::error(
                    "Imported Wasm functions cannot have a body",
                    fallback_range,
                ));
            }
        }
        WasmLinkage::Internal | WasmLinkage::ImplicitMainExport | WasmLinkage::Export => {
            if source.body().is_none() {
                diagnostics.push(Diagnostic::error("Function body is required", fallback_range));
            }
        }
    }

    if function.is_comptime() && !matches!(function.linkage(), WasmLinkage::Internal) {
        diagnostics.push(Diagnostic::error(
            "`comptime fun` cannot be combined with Wasm import/export modifiers",
            fallback_range,
        ));
    }

    if matches!(function.linkage(), WasmLinkage::ImplicitMainExport | WasmLinkage::Export)
        && !function.type_params().is_empty()
    {
        diagnostics.push(Diagnostic::error(
            "Wasm imports and exports do not support generic functions",
            fallback_range,
        ));
    }

    if !function.type_params().is_empty() || !is_direct_typed_wasm_boundary(function) {
        return diagnostics;
    }

    let Some((param_tys, result_ty)) = function_signature_types(db, func, function, &[]) else {
        return diagnostics;
    };

    for (&param, ty) in function.params().iter().zip(param_tys) {
        let (pattern, ty_id) = nodes.param(param);
        let range = if ty_id != mitki_hir::hir::TyId::ZERO {
            source_map.try_type_syntax(ty_id).map_or_else(|| fallback_range, |ptr| ptr.range)
        } else {
            source_map.try_pat_syntax(pattern).map_or_else(|| fallback_range, |ptr| ptr.range)
        };
        if let Some(failure) = typed_wasm_boundary_failure(db, ty) {
            diagnostics.push(Diagnostic::error(typed_wasm_boundary_message(db, failure), range));
        }
    }

    if let Some(failure) = typed_wasm_boundary_failure(db, result_ty) {
        diagnostics.push(Diagnostic::error(
            typed_wasm_boundary_message(db, failure),
            return_range(db, func, function, fallback_range),
        ));
    }

    diagnostics
}

pub(crate) fn check_boundary_instance_legality(
    db: &dyn salsa::Database,
    instance: BoundaryInstanceLocation<'_>,
) -> Vec<Diagnostic> {
    let source = instance.source(db);
    let range = SyntaxNodePtr::new(source.syntax()).range;
    let mut diagnostics = Vec::new();
    let name = source.name().map_or("", |name| name.as_str());

    let Some(origin) = instance.origin(db) else {
        diagnostics
            .push(Diagnostic::error(format!("unknown boundary instance target `{name}`"), range));
        return diagnostics;
    };

    let function = origin.hir_function(db).function(db);
    let type_param_count = function.type_params().len();
    if type_param_count == 0 {
        diagnostics.push(Diagnostic::error(
            format!("boundary instance target `{name}` must be a generic function"),
            range,
        ));
        return diagnostics;
    }

    let type_args = instance.type_args(db);
    if type_args.len() != type_param_count {
        diagnostics.push(Diagnostic::error(
            format!(
                "boundary instance target `{name}` expects {type_param_count} type argument(s), \
                 found {}",
                type_args.len()
            ),
            range,
        ));
    }

    match instance.kind(db) {
        BoundaryInstanceKind::Import => {
            if !matches!(function.linkage(), WasmLinkage::Import { .. }) {
                diagnostics.push(Diagnostic::error(
                    format!("import instance target `{name}` must be an imported generic function"),
                    range,
                ));
            }
        }
        BoundaryInstanceKind::Export => {
            if !matches!(function.linkage(), WasmLinkage::Internal) {
                diagnostics.push(Diagnostic::error(
                    format!(
                        "export instance target `{name}` must be a non-import generic function"
                    ),
                    range,
                ));
            }
        }
    }

    if !diagnostics.is_empty() {
        return diagnostics;
    }

    let Some((param_tys, result_ty)) = function_signature_types(db, origin, function, type_args)
    else {
        return diagnostics;
    };

    for ty in param_tys.into_iter().chain(std::iter::once(result_ty)) {
        if let Some(failure) = typed_wasm_boundary_failure(db, ty) {
            diagnostics.push(Diagnostic::error(typed_wasm_boundary_message(db, failure), range));
        }
    }

    diagnostics
}

fn typed_wasm_boundary_failure_inner<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
    seen: &mut FxHashSet<Ty<'db>>,
) -> Option<BoundaryLegalityFailure<'db>> {
    if !seen.insert(ty) {
        return None;
    }

    match ty.kind(db) {
        _ if !ownership::is_copyable(db, ty) => Some(BoundaryLegalityFailure::NonCopy(ty)),
        TyKind::ExactInt(_) => Some(BoundaryLegalityFailure::ExactInt(ty)),
        TyKind::Pointer { .. } => Some(BoundaryLegalityFailure::Pointer(ty)),
        TyKind::ExternStruct(_) => Some(BoundaryLegalityFailure::ExternStruct(ty)),
        TyKind::Array(item) => typed_wasm_boundary_failure_inner(db, *item, seen),
        TyKind::Tuple(items) | TyKind::Union(items) | TyKind::Inter(items) => {
            items.iter().find_map(|&item| typed_wasm_boundary_failure_inner(db, item, seen))
        }
        TyKind::Record(fields) => fields
            .iter()
            .find_map(|(_, field_ty)| typed_wasm_boundary_failure_inner(db, *field_ty, seen)),
        TyKind::Function { inputs, output } => inputs
            .iter()
            .find_map(|&input| typed_wasm_boundary_failure_inner(db, input, seen))
            .or_else(|| typed_wasm_boundary_failure_inner(db, *output, seen)),
        TyKind::Rec(_, body) => typed_wasm_boundary_failure_inner(db, *body, seen),
        TyKind::Struct(struct_ty) => struct_fields(db, *struct_ty)
            .iter()
            .find_map(|(_, field_ty)| typed_wasm_boundary_failure_inner(db, *field_ty, seen)),
        TyKind::Enum(enum_ty) => enum_variants(db, *enum_ty).iter().find_map(|(_, fields)| {
            fields
                .iter()
                .find_map(|&field_ty| typed_wasm_boundary_failure_inner(db, field_ty, seen))
        }),
        TyKind::Bool
        | TyKind::Float
        | TyKind::Int
        | TyKind::String
        | TyKind::Char
        | TyKind::Unknown
        | TyKind::Var(_) => None,
    }
}

fn is_direct_typed_wasm_boundary(function: &Function<'_>) -> bool {
    matches!(
        function.linkage(),
        WasmLinkage::Import { .. } | WasmLinkage::ImplicitMainExport | WasmLinkage::Export
    )
}

fn function_signature_types<'db>(
    db: &'db dyn salsa::Database,
    location: FunctionLocation<'db>,
    function: &'db Function<'db>,
    type_args: &[Ty<'db>],
) -> Option<(Vec<Ty<'db>>, Ty<'db>)> {
    let signature = location.signature(db);
    let params = resolve_param_types(db, location, signature)?
        .into_iter()
        .map(|ty| specialize_ty(db, ty, type_args))
        .collect::<Vec<_>>();
    let result = location
        .infer(db)
        .type_of_node(function.body())
        .unwrap_or_else(|| Ty::new(db, TyKind::Tuple(Vec::new())));
    Some((params, specialize_ty(db, result, type_args)))
}

fn resolve_param_types<'db>(
    db: &'db dyn salsa::Database,
    location: FunctionLocation<'db>,
    signature: &'db Signature<'db>,
) -> Option<Vec<Ty<'db>>> {
    let resolver = SignatureTypeResolver::new(db, location, signature);
    let nodes = signature.nodes(db);
    signature
        .params(db)
        .iter()
        .map(|&param| {
            let (_, ty) = nodes.param(param);
            resolver.resolve(ty).ok()
        })
        .collect()
}

fn return_range(
    db: &dyn salsa::Database,
    location: FunctionLocation<'_>,
    function: &Function<'_>,
    fallback_range: mitki_errors::TextRange,
) -> mitki_errors::TextRange {
    let source_map = location.hir_function(db).source_map(db);
    if function.ret_type().is_zero() {
        fallback_range
    } else {
        source_map.try_type_syntax(function.ret_type()).map_or(fallback_range, |ptr| ptr.range)
    }
}

fn specialize_ty<'db>(db: &'db dyn salsa::Database, ty: Ty<'db>, type_args: &[Ty<'db>]) -> Ty<'db> {
    match ty.kind(db) {
        TyKind::Var(id) => type_args.get(*id as usize).copied().unwrap_or(ty),
        TyKind::Array(item) => {
            let item = specialize_ty(db, *item, type_args);
            Ty::new(db, TyKind::Array(item))
        }
        TyKind::Tuple(items) => {
            let items = items.iter().map(|&item| specialize_ty(db, item, type_args)).collect();
            Ty::new(db, TyKind::Tuple(items))
        }
        TyKind::Record(fields) => {
            let fields = fields
                .iter()
                .map(|(name, field_ty)| (*name, specialize_ty(db, *field_ty, type_args)))
                .collect();
            Ty::new(db, TyKind::Record(fields))
        }
        TyKind::Function { inputs, output } => {
            let inputs = inputs.iter().map(|&item| specialize_ty(db, item, type_args)).collect();
            let output = specialize_ty(db, *output, type_args);
            Ty::new(db, TyKind::Function { inputs, output })
        }
        TyKind::Union(items) => {
            let items = items.iter().map(|&item| specialize_ty(db, item, type_args)).collect();
            Ty::new(db, TyKind::Union(items))
        }
        TyKind::Inter(items) => {
            let items = items.iter().map(|&item| specialize_ty(db, item, type_args)).collect();
            Ty::new(db, TyKind::Inter(items))
        }
        TyKind::Pointer { mutable, pointee } => {
            let pointee = specialize_ty(db, *pointee, type_args);
            Ty::new(db, TyKind::Pointer { mutable: *mutable, pointee })
        }
        TyKind::Rec(id, body) => {
            let body = specialize_ty(db, *body, type_args);
            Ty::new(db, TyKind::Rec(*id, body))
        }
        _ => ty,
    }
}
