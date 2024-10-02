use camino::Utf8PathBuf;
use mitki_errors::{Diagnostic, TextRange};
use salsa::{Accumulator, Database};

#[salsa::input]
pub struct File {
    #[return_ref]
    pub path: Utf8PathBuf,
    #[return_ref]
    pub text: String,
}

#[salsa::tracked]
pub fn check_file(db: &dyn Database, _file: File) {
    Diagnostic {
        message: "parsing functionality not yet implemented".to_string(),
        range: TextRange::empty(0.into()),
    }
    .accumulate(db);
}
