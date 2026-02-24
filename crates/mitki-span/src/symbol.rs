use std::marker::PhantomData;
use std::sync::Arc;

#[picante::interned]
pub struct SymbolData {
    pub text: Box<str>,
}

pub trait SymbolDatabase: picante::HasRuntime + HasSymbolDataIngredient {}

impl<T> SymbolDatabase for T where T: picante::HasRuntime + HasSymbolDataIngredient {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, facet::Facet)]
pub struct Symbol<'db> {
    id: picante::InternId,
    _marker: PhantomData<&'db ()>,
}

impl<'db> Symbol<'db> {
    pub fn new<DB>(db: &DB, text: impl Into<Box<str>>) -> Self
    where
        DB: SymbolDatabase,
    {
        let data = SymbolData::new(db, text.into()).expect("failed to intern symbol");
        Self { id: data.0, _marker: PhantomData }
    }

    pub fn text<DB>(self, db: &DB) -> Arc<str>
    where
        DB: SymbolDatabase,
    {
        let data = SymbolData(self.id).value(db).expect("failed to load symbol");
        Arc::from(data.text.as_ref())
    }

    pub fn as_bits(self) -> u64 {
        (self.id.0 as u64) + 1
    }

    pub fn from_bits(bits: u64) -> Self {
        let id = bits.checked_sub(1).expect("invalid symbol bits");
        let id = u32::try_from(id).expect("symbol bits overflow");
        Self { id: picante::InternId(id), _marker: PhantomData }
    }
}

pub trait IntoSymbol<'db> {
    fn into_symbol<DB>(self, db: &DB) -> Symbol<'db>
    where
        DB: SymbolDatabase;
}

impl<'db, T> IntoSymbol<'db> for T
where
    T: Into<Box<str>>,
{
    fn into_symbol<DB>(self, db: &DB) -> Symbol<'db>
    where
        DB: SymbolDatabase,
    {
        Symbol::new(db, self)
    }
}
