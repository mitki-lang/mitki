use camino::Utf8PathBuf;
pub use mitki_errors::Diagnostic;
use mitki_yellow::GreenNode;
use salsa::{Database, Event};

#[salsa::db]
#[derive(Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl Database for RootDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> Event) {}
}

#[salsa::input]
pub struct File {
    #[return_ref]
    pub path: Utf8PathBuf,
    #[return_ref]
    pub text: String,
}

#[salsa::tracked]
impl File {
    #[salsa::tracked(return_ref)]
    pub fn line_index(self, db: &dyn Database) -> line_index::LineIndex {
        line_index::LineIndex::new(self.text(db))
    }

    #[salsa::tracked]
    pub fn parse(self, db: &dyn Database) -> GreenNode<'_> {
        mitki_parse::module(db, self.text(db))
    }
}

#[salsa::tracked]
pub fn check_file(db: &dyn Database, file: File) {
    let _module = file.parse(db);
}
