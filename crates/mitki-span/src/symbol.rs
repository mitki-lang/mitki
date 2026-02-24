use std::sync::Arc;

#[picante::interned]
pub struct Symbol {
    pub raw: Box<str>,
}

pub trait SymbolDatabase: picante::HasRuntime + HasSymbolIngredient {}

impl<T> SymbolDatabase for T where T: picante::HasRuntime + HasSymbolIngredient {}

impl Symbol {
    pub fn intern<DB>(db: &DB, text: impl Into<Box<str>>) -> Self
    where
        DB: SymbolDatabase,
    {
        Symbol::new(db, text.into()).expect("failed to intern symbol")
    }

    pub fn text<DB>(self, db: &DB) -> Arc<str>
    where
        DB: SymbolDatabase,
    {
        let raw = self.raw(db).expect("failed to load symbol");
        Arc::from(raw.as_ref())
    }

    pub fn as_bits(self) -> u64 {
        (self.0.0 as u64) + 1
    }

    pub fn from_bits(bits: u64) -> Self {
        let id = bits.checked_sub(1).expect("invalid symbol bits");
        let id = u32::try_from(id).expect("symbol bits overflow");
        Self(picante::InternId(id))
    }
}

pub trait IntoSymbol {
    fn into_symbol<DB>(self, db: &DB) -> Symbol
    where
        DB: SymbolDatabase;
}

impl<T> IntoSymbol for T
where
    T: Into<Box<str>>,
{
    fn into_symbol<DB>(self, db: &DB) -> Symbol
    where
        DB: SymbolDatabase,
    {
        Symbol::intern(db, self)
    }
}
