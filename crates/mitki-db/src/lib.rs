use camino::Utf8PathBuf;
pub use mitki_errors::Diagnostic;
use mitki_hir::{Body, BodySourceMap, Lowering};
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

    pub fn body(self, db: &dyn Database) -> &Body<'_> {
        &self.body_with_source_map(db).0
    }

    #[salsa::tracked(no_eq, return_ref)]
    pub fn body_with_source_map(self, db: &dyn Database) -> (Body<'_>, BodySourceMap) {
        let mut lowering = Lowering::new(db);
        lowering.lower_module(self.parse(db));
        lowering.finish()
    }
}

#[salsa::tracked]
pub fn check_file(db: &dyn Database, file: File) {
    _ = file.body(db);
}
