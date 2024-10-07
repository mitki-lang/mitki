#[salsa::interned]
pub struct Symbol<'db> {
    pub data: Box<str>,
}
