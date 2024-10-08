mod body;
mod hir;

pub use body::{Body, BodySourceMap};
pub use hir::{Expr, ExprData};
use mitki_span::Symbol;
use mitki_yellow::ast;
use salsa::Database;

pub struct Lowering<'db> {
    db: &'db dyn Database,
    body: Body<'db>,
    source_map: BodySourceMap,
}

impl<'db> Lowering<'db> {
    pub fn new(db: &'db dyn Database) -> Self {
        Self { db, body: Body::default(), source_map: BodySourceMap::default() }
    }

    pub fn lower_module(&mut self, module: ast::Module<'db>) {
        for expr in module.exprs(self.db) {
            self.lower_expr(expr.into());
        }
    }

    fn lower_expr(&mut self, expr: Option<ast::Expr<'db>>) -> Expr<'db> {
        let Some(expr) = expr else {
            return self.body.exprs.alloc(ExprData::Error);
        };

        macro_rules! mk_literal {
            ($value:expr, $constructor:expr) => {{
                let value = $value.green(self.db).text_trimmed(self.db);
                let value = Symbol::new(self.db, value.into());
                $constructor(value)
            }};
        }

        let data = match expr {
            ast::Expr::Literal(literal) => match literal.kind(self.db) {
                ast::LiteralKind::Int(value) => mk_literal!(value, ExprData::Int),
                ast::LiteralKind::Float(value) => mk_literal!(value, ExprData::Float),
            },
            ast::Expr::Binary(binary) => {
                let lhs = self.lower_expr(binary.lhs(self.db));
                let op = Symbol::new(self.db, binary.op(self.db).unwrap_or_default().into());
                let rhs = self.lower_expr(binary.rhs(self.db));
                ExprData::Binary(lhs, op, rhs)
            }
            ast::Expr::Postfix(postfix) => {
                let expr = self.lower_expr(postfix.expr(self.db));
                let op = Symbol::new(self.db, postfix.op(self.db).unwrap_or_default().into());
                ExprData::Postfix(expr, op)
            }
            ast::Expr::Prefix(prefix) => {
                let op = Symbol::new(self.db, prefix.op(self.db).unwrap_or_default().into());
                let expr = self.lower_expr(prefix.expr(self.db));
                ExprData::Prefix(op, expr)
            }
        };

        self.body.exprs.alloc(data)
    }

    pub fn finish(self) -> (Body<'db>, BodySourceMap) {
        (self.body, self.source_map)
    }
}
