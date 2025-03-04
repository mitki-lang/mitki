#[salsa::interned]
pub struct Symbol<'db> {
    #[return_ref]
    pub text: Box<str>,
}
