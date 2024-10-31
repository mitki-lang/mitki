#[salsa::interned]
pub(crate) struct Ty<'db> {
    kind: TyKind<'db>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum TyKind<'db> {
    Tuple(Vec<Ty<'db>>),
    Unknown,
}
