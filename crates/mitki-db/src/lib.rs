use camino::Utf8PathBuf;
pub use mitki_errors::Diagnostic;
use salsa::Database;

#[salsa::input]
pub struct File {
    #[return_ref]
    pub path: Utf8PathBuf,
    #[return_ref]
    pub text: String,
}

#[salsa::tracked]
pub fn check_file(db: &dyn Database, file: File) {
    let _module = mitki_parse::module(db, file.text(db));
}
