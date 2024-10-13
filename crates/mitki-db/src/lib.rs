use camino::Utf8PathBuf;
pub use mitki_errors::Diagnostic;
use mitki_yellow::ast;
use salsa::Database;

#[salsa::input]
pub struct File {
    #[return_ref]
    pub path: Utf8PathBuf,
    #[return_ref]
    pub text: String,
}

#[salsa::tracked]
impl File {
    #[salsa::tracked]
    pub fn parse(self, db: &dyn Database) -> ast::Module<'_> {
        mitki_parse::module(db, self.text(db))
    }
}

#[salsa::tracked]
pub fn check_file(db: &dyn Database, file: File) {
    _ = file.parse(db);
}
