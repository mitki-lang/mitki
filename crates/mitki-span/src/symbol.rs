#[salsa::interned(debug)]
pub struct Symbol<'db> {
    pub data: Box<str>,
}
