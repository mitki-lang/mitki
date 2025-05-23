pub use mitki_analysis::check_file;
pub use mitki_errors::{Diagnostic, Level};
use salsa::Database;

#[salsa::db]
#[derive(Default, Clone)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl Database for RootDatabase {}
