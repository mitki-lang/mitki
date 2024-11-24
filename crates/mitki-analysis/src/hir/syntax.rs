use la_arena::Idx;
use mitki_span::Symbol;

pub(crate) type Expr<'db> = Idx<ExprData<'db>>;
pub(crate) type Binding<'db> = Idx<Symbol<'db>>;

#[derive(Debug, Default, Clone)]
pub(crate) struct Block<'db> {
    pub stmts: Vec<Stmt<'db>>,
    pub tail: Option<Expr<'db>>,
}

#[derive(Debug, Clone, Copy)]
#[expect(dead_code)]
pub(crate) enum Stmt<'db> {
    Val { name: Binding<'db>, ty: Option<Ty<'db>>, initializer: Expr<'db> },
    Expr { expr: Expr<'db>, has_semi: bool },
}

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) enum ExprData<'db> {
    Path(Symbol<'db>),
    Bool(bool),
    Int(Symbol<'db>),
    Float(Symbol<'db>),
    If { condition: Expr<'db>, then_branch: Block<'db>, else_branch: Option<Block<'db>> },
    Closure { params: Vec<Binding<'db>>, body: Block<'db> },
    Missing,
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub(crate) enum Ty<'db> {
    Path(Symbol<'db>),
}
