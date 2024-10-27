#[salsa::input]
pub struct File {
    #[return_ref]
    pub path: camino::Utf8PathBuf,
    #[return_ref]
    pub text: String,
}

#[salsa::tracked]
impl File {
    #[salsa::tracked(return_ref, no_eq)]
    pub fn line_index(self, db: &dyn salsa::Database) -> line_index::LineIndex {
        line_index::LineIndex::new(self.text(db))
    }
}