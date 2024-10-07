use mitki_span::Symbol;

pub type Expr<'db> = la_arena::Idx<ExprData<'db>>;

#[derive(Debug, Clone)]
pub enum ExprData<'db> {
    Int(u64),
    Binary(Expr<'db>, Symbol<'db>, Expr<'db>),
    Postfix(Expr<'db>, Symbol<'db>),
    Prefix(Symbol<'db>, Expr<'db>),
    Error,
}
