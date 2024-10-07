use camino::Utf8PathBuf;
pub use mitki_errors::Diagnostic;
use mitki_hir::Body;
use mitki_yellow::GreenNode;
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
    fn parse(self, db: &dyn Database) -> GreenNode<'_> {
        mitki_parse::module(db, self.text(db))
    }

    #[salsa::tracked(no_eq)]
    fn hir(self, db: &dyn Database) -> Body {
        let _module = self.parse(db);
        Body::default()
    }
}

#[salsa::tracked]
pub fn check_file(db: &dyn Database, file: File) {
    _ = file.hir(db);
}
