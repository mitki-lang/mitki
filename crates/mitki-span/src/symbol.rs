#[salsa::interned(debug)]
pub struct Symbol<'db> {
    #[returns(deref)]
    pub text: Box<str>,
}

pub trait IntoSymbol<'db> {
    fn into_symbol(self, db: &'db dyn salsa::Database) -> Symbol<'db>;
}

impl<'db, T> IntoSymbol<'db> for T
where
    T: salsa::plumbing::interned::Lookup<Box<str>> + std::hash::Hash,
    Box<str>: salsa::plumbing::interned::HashEqLike<T>,
{
    fn into_symbol(self, db: &'db dyn salsa::Database) -> Symbol<'db> {
        Symbol::new(db, self)
    }
}
