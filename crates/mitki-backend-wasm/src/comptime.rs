use mitki_hir::hir::ExprId;
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::FunctionLocation;
use mitki_typeck::infer::Inferable as _;

pub(crate) fn classify_comptime_result<'db>(
    db: &'db dyn salsa::Database,
    function: FunctionLocation<'db>,
) -> Result<Ty<'db>, String> {
    let hir_function = function.hir_function(db).function(db);
    let inference = function.infer(db);
    let return_ty = inference
        .type_of_node(hir_function.body())
        .unwrap_or_else(|| Ty::new(db, TyKind::Tuple(Vec::new())));
    if crate::capability::supports_boundary(db, return_ty) {
        Ok(return_ty)
    } else {
        Err(format!(
            "comptime requires the target function to return a runtime-lowerable value, found `{}`",
            return_ty.display(db)
        ))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct ComptimeValueKey<'db> {
    pub(crate) location: FunctionLocation<'db>,
    pub(crate) expr: ExprId,
}
