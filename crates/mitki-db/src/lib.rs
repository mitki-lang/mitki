pub use mitki_errors::Diagnostic;
use mitki_inputs::File;
use mitki_parse::FileParse as _;
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

#[salsa::tracked]
pub fn check_file(db: &dyn Database, file: File) {
    let _module = file.parse(db);
}
