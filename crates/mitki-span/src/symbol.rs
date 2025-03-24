#[salsa::interned(debug)]
pub struct Symbol<'db> {
    #[return_ref]
    pub text: Box<str>,
}
