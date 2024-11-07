use la_arena::ArenaMap;
use salsa::Database;

use crate::hir::{Block, Expr, ExprData, Function, HasFunction as _, Stmt};
use crate::item::scope::FunctionLocation;
use crate::resolver::Resolver;
use crate::ty::{Ty, TyKind};

pub(crate) trait Inferable<'db> {
    fn infer(self, db: &'db dyn Database) -> &'db Inference<'db>;
}

#[salsa::tracked]
impl<'db> Inferable<'db> for FunctionLocation<'db> {
    #[salsa::tracked(return_ref, no_eq)]
    fn infer(self, db: &'db dyn Database) -> Inference<'db> {
        let builder = InferenceBuilder {
            db,
            resolver: Resolver::new(db, self),
            inference: Inference::default(),
            function: self.hir_function(db),
        };
        builder.build()
    }
}

#[derive(Debug, Default)]
pub(crate) struct Inference<'db> {
    type_of_expr: ArenaMap<Expr<'db>, Ty<'db>>,
}

pub(crate) struct InferenceBuilder<'db> {
    db: &'db dyn Database,
    resolver: Resolver<'db>,
    inference: Inference<'db>,
    function: &'db Function<'db>,
}

impl<'db> InferenceBuilder<'db> {
    fn infer_block<'block>(&mut self, block: &'block Block<'db>) -> Ty<'db> {
        for &stmt in &block.stmts {
            self.infer_stmt(stmt);
        }

        if let Some(tail) = block.tail {
            self.infer_expr(tail)
        } else {
            Ty::new(self.db, TyKind::Tuple(Vec::new()))
        }
    }

    fn infer_stmt(&mut self, stmt: Stmt<'db>) {
        match stmt {
            Stmt::Val { name: _, ty, initializer } => {
                _ = ty;
                self.infer_expr(initializer);
            }
            Stmt::Expr { expr, has_semi: _ } => {
                self.infer_expr(expr);
            }
        }
    }

    fn infer_expr(&mut self, expr: Expr<'db>) -> Ty<'db> {
        let ty = match &self.function.exprs()[expr] {
            &ExprData::Path(path) => {
                let guard = self.resolver.scopes_for_expr(expr);
                let ty = match self.resolver.resolve_path(path) {
                    Some(_) => Ty::new(self.db, TyKind::Unknown),
                    None => Ty::new(self.db, TyKind::Unknown),
                };
                self.resolver.reset(guard);
                ty
            }
            ExprData::Int(_symbol) => Ty::new(self.db, TyKind::Unknown),
            ExprData::Float(_symbol) => Ty::new(self.db, TyKind::Unknown),
            ExprData::If { condition, then_branch, else_branch } => {
                let _ = (condition, then_branch, else_branch);

                Ty::new(self.db, TyKind::Unknown)
            }
            ExprData::Missing => Ty::new(self.db, TyKind::Unknown),
        };

        self.inference.type_of_expr.insert(expr, ty);
        ty
    }

    fn build(mut self) -> Inference<'db> {
        self.infer_block(self.function.body());
        self.inference
    }
}
