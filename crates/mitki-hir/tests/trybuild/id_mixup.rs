use mitki_hir::hir::{ExprId, StmtId, TyId};

fn takes_expr(_expr: ExprId) {}
fn takes_exprs(_exprs: Vec<ExprId>) {}
fn takes_ty(_ty: TyId) {}

fn main() {
    let expr = ExprId::ZERO;
    let stmt = StmtId::ZERO;
    let ty = TyId::ZERO;

    // StmtId should not be accepted where ExprId is required.
    takes_expr(stmt);

    // TyId should not be accepted where ExprId is required.
    takes_exprs(vec![ty]);

    // ExprId should not be accepted where TyId is required.
    takes_ty(expr);
}
