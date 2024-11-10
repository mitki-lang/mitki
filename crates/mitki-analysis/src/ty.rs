#[salsa::interned]
pub(crate) struct Ty<'db> {
    kind: TyKind<'db>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
#[expect(dead_code)]
pub(crate) enum TyKind<'db> {
    Bool,
    Float,
    Int,
    Tuple(Vec<Ty<'db>>),
    Unknown,
}
