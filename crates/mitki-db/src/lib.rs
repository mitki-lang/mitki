pub use mitki_analysis::check_file;
pub use mitki_errors::{Diagnostic, Level};
use salsa::{Database, Event};

#[salsa::db]
#[derive(Default, Clone)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl Database for RootDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> Event) {}
}
